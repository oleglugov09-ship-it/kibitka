;;;; The view from the window, the frame step: bring together the far land (far.lisp), the places
;;;; (places.lisp) and the traffic (traffic.lisp). The place plan and buildings are in places.lisp, marks.lisp, towns.lisp.

(in-package #:kiosk-trip)

;;; ------------------------------------------------------------------ the whole thing

(defun reset-vista ()
  "Forget the scene nodes for a new world: they are gone together with the previous World."
  (dolist (s *scenes*) (setf (scene-node s) nil (scene-moves s) nil))
  (reset-life)
  (setf *trains* '() *cars* '() *flocks* '() *walkers* '()
        *next-car* (+ *time* 8.0) *next-flock* (+ *time* 12.0)))

(defun step-vista (delta)
  (step-seasonal-scenes)
  (step-scenes)
  (dolist (s *scenes*) (when (scene-moves s) (step-moves (scene-moves s))))
  (step-rail delta)
  (step-tunnel)
  (step-cars delta)
  (step-birds delta)
  (step-walkers)
  (step-life delta))
