;;;; A minimal profiler: (TIMED :name ...) accumulates the time of a piece of code, PROFILE-REPORT
;;;; returns the average and the worst per piece, the worst frame and the Godot counters
;;;; (FPS, GPU frame time, objects, draw calls). To look at it, call
;;;; (live:profile): a report for the time since the previous call; the counters are reset.

(in-package #:kiosk-trip)

(defvar *root* nil "The scene root (TripRoot).")
(defvar *timings* (make-hash-table) "The table mapping a name to #(calls total-ms worst-ms).")
(defvar *frames* 0)
(defvar *worst-frame* 0.0 "The longest frame in milliseconds (the delta from Godot).")

(defun note-time (name ms)
  (let ((v (or (gethash name *timings*) (setf (gethash name *timings*) (vector 0 0.0 0.0)))))
    (incf (aref v 0)) (incf (aref v 1) ms) (setf (aref v 2) (max (aref v 2) ms))))

(defmacro timed (name &body body)
  (let ((t0 (gensym)))
    `(let ((,t0 (get-internal-real-time)))
       (multiple-value-prog1 (progn ,@body)
         (note-time ,name (/ (* 1000.0 (- (get-internal-real-time) ,t0)) internal-time-units-per-second))))))

(defun note-frame (delta)
  (incf *frames*)
  (setf *worst-frame* (max *worst-frame* (* 1000.0 delta))))

(defun monitor (id) (object-call (gdext:singleton "Performance") "get_monitor" id))

(defun gpu-ms ()
  "Return the GPU time of the last frame in milliseconds.
The measurement is switched on by the first call."
  (let ((rs (gdext:singleton "RenderingServer"))
        (vp (object-call (node-get-viewport *root*) "get_viewport_rid")))
    (object-call rs "viewport_set_measure_render_time" vp t)
    (object-call rs "viewport_get_measured_render_time_gpu" vp)))

(defun profile-report ()
  "Return the report for the time since the previous call and reset the accumulators."
  (prog1
      (list :frames *frames*
            :fps (round (monitor 0))
            :worst-frame-ms (round *worst-frame*) :gpu-ms (/ (round (* 10 (gpu-ms))) 10.0)
            :draw-calls (round (monitor 13)) :objects-drawn (round (monitor 11))
            :primitives (round (monitor 12)) :nodes (round (monitor 9))
            :video-mb (round (monitor 14) (* 1024 1024))
            :lisp-ms (sort (loop for k being the hash-keys of *timings* using (hash-value v)
                                 collect (list k :avg (/ (round (* 100 (/ (aref v 1) (max 1 (aref v 0))))) 100.0)
                                               :max (/ (round (* 100 (aref v 2))) 100.0)
                                               :per-frame (/ (round (* 100 (/ (aref v 1) (max 1 *frames*)))) 100.0)))
                           #'> :key (lambda (r) (getf (cdr r) :per-frame))))
    (clrhash *timings*)
    (setf *frames* 0 *worst-frame* 0.0)))
