;;;; Getting back to Godot's main thread.
;;;; gdecl is not thread-safe: code on another thread (the Slynk REPL, a world
;;;; reload) does not call Godot itself but puts a closure on a queue; the scene root runs
;;;; the queue after its own process, between frames.

(in-package #:kiosk-trip)

(defvar *main-thread* nil "The thread Godot calls the extension on.")
(defvar *queue-lock* (mp:make-lock :name "main-thread queue"))
(defvar *queue* '() "Closures to run on the main thread, newest first.")
(defparameter *on-main-sync-timeout* 2 "Seconds ON-MAIN-SYNC waits for the main thread.")

(defun main-thread-p ()
  (eq mp:*current-process* *main-thread*))

(defun on-main (function)
  "Run FUNCTION, of no arguments, on the main thread at the next frame."
  (mp:with-lock (*queue-lock*)
    (push function *queue*))
  nil)

(defun on-main-sync (function &key (timeout *on-main-sync-timeout*))
  "Run FUNCTION on the main thread and return its values. An error in it is
signalled again here with its message. If the main thread doesn't get to it
within TIMEOUT seconds (paused in a debugger, frozen), signal an error, and
FUNCTION won't run. On the main thread, just call FUNCTION."
  (when (main-thread-p)
    (return-from on-main-sync (funcall function)))
  (let ((lock (mp:make-lock :name "on-main-sync"))
        (done (mp:make-condition-variable))
        (state :queued)                 ; :queued, :running, :done, :cancelled
        values error)
    (on-main (lambda ()
               (when (mp:with-lock (lock)
                       (when (eq state :queued)
                         (setf state :running)))
                 (handler-case (setf values (multiple-value-list (funcall function)))
                   (error (condition)
                     (setf error (princ-to-string condition))))
                 (mp:with-lock (lock)
                   (setf state :done)
                   (mp:condition-variable-broadcast done)))))
    (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
      (mp:with-lock (lock)
        (loop until (eq state :done)
              do (let ((left (/ (- deadline (get-internal-real-time)) internal-time-units-per-second)))
                   (when (and (<= left 0) (eq state :queued))
                     (setf state :cancelled)
                     (error "The main thread didn't run the form within ~a s" timeout))
                   (mp:condition-variable-timedwait done lock (max left 0.05))))))
    (if error
        (error "On the main thread: ~a" error)
        (values-list values))))

(defun run-main-thread-queue ()
  "Run the queued closures, oldest first; an error in one is printed."
  (let ((functions (mp:with-lock (*queue-lock*)
                     (prog1 (reverse *queue*) (setf *queue* '())))))
    (dolist (function functions)
      (handler-case (funcall function)
        (error (condition)
          (godot-utility:push-error (format nil "main-thread queue: ~a" condition)))))))

(defmethod process :after ((root trip-root) delta)
  (declare (ignore delta))
  (run-main-thread-queue))

(gdext:add-initialize-hook (lambda () (setf *main-thread* mp:*current-process*)))
