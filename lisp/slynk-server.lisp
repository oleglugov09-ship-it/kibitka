;;;; Slynk in the game, for the REPL, Autolith and tools/godot-ask: only in a debug run
;;;; (`godot --path project`, F5; not in the editor and not in an export), on
;;;; 127.0.0.1:4009, in its own thread. The port is written to
;;;; project/.slynk-port. Code coming from Slynk changes the game through LIVE, on the main thread.

(in-package #:kiosk-trip)

(defparameter *slynk-port* (or (ignore-errors (parse-integer (ext:getenv "KIOSK_SLYNK"))) 4009)
  "The port of the Slynk server.
A second game on the same machine (for network testing) needs its own port: KIOSK_SLYNK=4010.")
(defvar *slynk-port-file* nil)

(defun start-slynk ()
  (when (and (os-is-debug-build) (not (engine-is-editor-hint)))
    (let ((port-file (project-settings-globalize-path (if (= *slynk-port* 4009) "res://.slynk-port" (format nil "res://.slynk-port-~d" *slynk-port*)))))
      (setf *slynk-port-file* port-file)
      (mp:process-run-function
       "slynk starter"
       (lambda ()
         (handler-case
             (let ((port (slynk:create-server :port *slynk-port* :interface "127.0.0.1" :dont-close t)))
               (with-open-file (out port-file :direction :output :if-exists :supersede)
                 (format out "~a~%" port))
               (format t "~&Slynk on 127.0.0.1:~a~%" port))
           (error (condition)
             (format t "~&Slynk didn't start: ~a~%" condition))))))))

(defun stop-slynk ()
  (when *slynk-port-file*
    (ignore-errors (slynk:stop-server *slynk-port*))
    (ignore-errors (delete-file *slynk-port-file*))
    (setf *slynk-port-file* nil)))

(gdext:add-initialize-hook 'start-slynk)
(gdext:add-deinitialize-hook 'stop-slynk)
