;;;; For development: the debug screen (F3) and the 'dev polygon', all booths in a row at the start of the
;;;; first town. The polygon is switched on at startup: KIOSK_DEV=1 for the desktop game, ?dev in the address
;;;; for the browser. It changes the booth layout, so playing together works only when everyone has it on.

(in-package #:kiosk-trip)

(defvar *debug-on* nil "True while the debug screen (F3) is shown.")
(defvar *debug-label* nil)
(defvar *poi* 0 "The index of the place last jumped to (PageUp/PageDown while the debug screen is open).")

(defun init-dev ()
  (setf *dev* (and (or (let ((e (ext:getenv "KIOSK_DEV"))) (and e (plusp (length e))))
                       (and (object-call (gdext:singleton "OS") "has_feature" "web")
                            (let ((q (ignore-errors (object-call (gdext:singleton "JavaScriptBridge") "eval" "location.search"))))
                              (and (stringp q) (search "dev" q)))))
                   t)))

(defun places-of-interest ()
  "Return everything worth checking by hand, in order along the road, as (path-coordinate . name) pairs."
  (flet ((en (text) (or (gethash text *en*) text)))        ; the debug screen is always in English
    (sort (append (loop for (d i) in *gimmicks* collect (cons d (en (third (find i *gimmick-plan* :key #'first)))))
                  (loop for tw in *towns* for i from 0 collect (cons (town-stop-d tw) (cl:format nil "market: ~a" (en (town-name i)))))
                  (loop for d in *stations* collect (cons d "gas station"))
                  (when *observatory* (list (cons *observatory* "observatory")))
                  (when *depot-d* (list (cons *depot-d* "scrap depot")))
                  (when *airmail-d* (list (cons *airmail-d* "airmail"))))
          #'< :key #'car)))

(defun jump-place (step)
  "Jump to the next (STEP 1) or the previous (STEP -1) place in the list.
Only the driver moves the wagon."
  (let ((list (places-of-interest)))
    (cond ((net-guest-p) (say "Переносит водитель"))                 ; shown to the player, so it goes through the translation
          (list (setf *poi* (mod (+ *poi* step) (length list)))
                (let ((p (nth *poi* list)))
                  (setf *auto* nil *speed* 0.0)
                  (relocate (car p))
                  (say (cl:format nil "~d/~d · ~a · ~d m" (1+ *poi*) (length list) (cdr p) (round (car p)))))))))

(defun debug-text ()
  (let* ((tw (town-at *dist*)) (place (ignore-errors (walk-place)))
         (near (first (sort (copy-list (places-of-interest)) #'< :key (lambda (p) (abs (- (car p) *dist*)))))))
    (cl:format nil "F3 — debug~:[~; · DEV POLYGON~]   PageUp/PageDown — jump between places
fps ~d · lang ~a · view ~a~@[ · mini-game ~a~]
path ~,1f m · ~a · speed ~,1f · dir ~a · lane ~,1f~@[ · autopilot → ~a~] · turns ahead ~d
town: ~a · nearest: ~a
cat: x ~,2f z ~,2f · eye ~,2f · carrying ~a · standing on ~a
day-t ~,0f (light ~,2f) · ~a · ~a · fuel ~,2f · money ~d
net: ~a~@[ · room ~a~] · peers ~d
scenes built ~d of ~d · land chunks ~d · solid cells ~d"
               *dev* (round (object-call (gdext:singleton "Engine") "get_frames_per_second")) *lang* *view*
               (and *play* (getf *play* :kind))
               *dist* (if (ring-d-p *dist*) "ring" "spoke through the Crossroads") *speed* *dir* *lane-now* (cdr *auto*) (length *way*)
               (if tw (let ((name (town-name (position tw *towns*)))) (or (gethash name *en*) name)) "—") (if near (cl:format nil "~a (~d m)" (cdr near) (round (- (car near) *dist*))) "—")
               (vector3-x *pos*) (vector3-z *pos*) *eye-now* (or (car *carry*) "—") (or (first place) "—")
               *day-t* (daylight *day-t*) *season* *weather* *fuel* *money*
               (or *net-role* "solo") *net-room* (length *net-peers*)
               (count-if #'scene-node *scenes*) (length *scenes*) (length *chunks*) (hash-table-count *solid-grid*))))

(defun toggle-debug ()
  (setf *debug-on* (not *debug-on*))
  (unless *debug-label*
    (let ((layer (gdext:make-object "CanvasLayer")) (bg (gdext:make-object "ColorRect")))
      (node-add-child *root* layer) (node-add-child layer bg)
      (godot:object-set layer "layer" 20)
      (godot:object-set bg "position" (vector2 4 4)) (godot:object-set bg "size" (vector2 400 96))
      (godot:object-set bg "color" (color 0 0 0 .62))
      (setf *debug-label* (hud-label bg "" 5 3 7 '(.75 1.0 .75)))
      (setf (getf (symbol-plist '*debug-label*) :bg) bg)))
  (godot:object-set (getf (symbol-plist '*debug-label*) :bg) "visible" *debug-on*))

(defun debug-key (k)
  "Handle the debug key K.
Return T when the key was ours."
  (cond ((= k (1+ +key-f2+)) (toggle-debug) t)                       ; F3
        ((and *debug-on* (= k 4194324)) (jump-place 1) t)            ; PageDown
        ((and *debug-on* (= k 4194323)) (jump-place -1) t)))         ; PageUp

(defun step-debug ()
  (when (and *debug-on* *debug-label*)
    (godot:object-set *debug-label* "text" (or (ignore-errors (debug-text)) "debug: could not build the text"))))
