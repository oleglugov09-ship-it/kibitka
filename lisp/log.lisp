;;;; Game log: what happens on the players' side, to work out later where they got stuck, what the frame
;;;; rate was, which errors occurred. (log-event :event :key value ...) puts a line into the queue; every
;;;; 30 seconds the queue goes to the room server (relay/, POST /log). Once a minute there is a 'pulse':
;;;; path coordinate, tank, money, frame rate. The name the player typed in the lobby is recorded; nothing
;;;; else personal. With no server the log stays silent.

(in-package #:kiosk-trip)

(defvar *log-queue* '() "The JSON lines waiting to be sent, newest first.")
(defvar *log-http* nil "HTTPRequest.")
(defvar *log-sid* nil "The identifier of this game launch.")
(defvar *log-timer* 5.0)
(defvar *pulse-timer* 20.0)
(defvar *log-errors* '() "The texts of the errors already written to the log.")
(defparameter *log-every* 30.0)
(defparameter *pulse-every* 60.0)

(defun json-value (v)
  (cond ((stringp v) (format nil "\"~a\"" (remove-if (lambda (c) (or (find c "\"\\") (< (char-code c) 32))) v)))
        ((eq v t) "true")
        ((null v) "null")
        ((keywordp v) (format nil "\"~(~a~)\"" v))
        ((integerp v) (format nil "~d" v))
        ((realp v) (format nil "~,2f" v))
        (t (json-value (princ-to-string v)))))

(defun log-event (event &rest plist)
  "Record the event EVENT with the fields in PLIST (:key value ...)."
  (when (and (< (length *log-queue*) 200) (not (ext:getenv "KIOSK_NOLOG")))     ; test runs (tools/smoke.py) must not clutter the log
    (push (format nil "{\"e\":~a,\"at\":~d~{,\"~(~a~)\":~a~}}"
                  (json-value event) (round *time*)
                  (loop for (k v) on plist by #'cddr collect k collect (json-value v)))
          *log-queue*)))

(defun note-error (condition where)
  "Report the Lisp error CONDITION raised in WHERE and log it.
Each distinct error text goes to the log only once."
  (let ((text (princ-to-string condition)))
    (format t "~&Ошибка (~a): ~a~%" where text)
    (unless (or (member text *log-errors* :test #'string=) (> (length *log-errors*) 40))
      (push text *log-errors*)
      (log-event :error :where where :text (subseq text 0 (min 300 (length text)))))))

(defun log-url ()
  (let ((base (relay-url)))
    (when base
      (format nil "~a/log?sid=~a&v=~a"
              (cond ((eql 0 (search "wss://" base)) (format nil "https://~a" (subseq base 6)))
                    ((eql 0 (search "ws://" base)) (format nil "http://~a" (subseq base 5)))
                    (t base))
              *log-sid* *net-version*))))

(defun make-log (root)
  (setf *log-sid* (string-downcase (format nil "~36r" (+ 60466176 (random 2116316160))))
        *log-http* (gdext:make-object "HTTPRequest")
        *log-queue* '() *log-errors* '())
  (object-set *log-http* "process_mode" 3)                 ; keep sending while paused
  (object-set *log-http* "timeout" 20.0)
  (node-add-child root *log-http*)
  ;; along the way: two numbers of the world layout; desktop and browser must agree on them (own generator, random.lisp)
  (log-event :start :web (and (os-has-feature "web") t)
                    :town3 (round (town-stop-d (third *towns*))) :town12 (round (town-stop-d (car (last *towns*))))))

(defun pulse ()
  (log-event :pulse :dist (round *dist*) :speed *speed* :fuel *fuel* :money *money* :planks *planks*
               :parcels (reduce #'+ (mapcar #'cdr (parcels-left))) :items (length *items*)
               :fps (round (object-call (gdext:singleton "Engine") "get_frames_per_second"))
               :view *view* :role *net-role* :others (length *net-others*) :season *season* :weather *weather*))

(defun step-log (delta)
  (when (<= (decf *pulse-timer* delta) 0) (setf *pulse-timer* *pulse-every*) (pulse))
  (when (and (<= (decf *log-timer* delta) 0) *log-queue* *log-http*)
    (setf *log-timer* *log-every*)
    (let ((url (log-url)))
      (cond ((null url) (setf *log-queue* '()))
            ((zerop (object-call *log-http* "get_http_client_status"))        ; the previous request has finished
             ;; the server takes at most 80 lines at a time, so send the 60 oldest and leave the rest for the next request
             (let* ((all (reverse *log-queue*)) (now (subseq all 0 (min 60 (length all)))))
               (when (eql 0 (object-call *log-http* "request" url (vector "Content-Type: text/plain") 2
                                         (format nil "~{~a~^~%~}" now)))
                 (setf *log-queue* (reverse (nthcdr (length now) all))))))))))
