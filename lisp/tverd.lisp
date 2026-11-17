;;;; Solids: what cannot be walked through on foot. Every building hands in its outlines here: rectangles
;;;; on the map (a wall, a post, a trunk, a boulder). They sit in a grid, and a player outside bumps into them.
;;;; No engine here: only geometry on the plane. Where the outlines come from: shapes.lisp (WITH-SOLIDS).

(in-package #:kiosk-trip)

(defparameter *solid-cell* 8.0 "The side of a grid cell, in metres.")
(defstruct (solid (:constructor make-solid (x z hw hd cos sin)))
  "A rectangle on the map: centre, half-width and half-depth, rotation (cosine and sine)."
  x z hw hd cos sin)

(defvar *solid-grid* (make-hash-table) "The table from a cell to the outlines that touch it.")
(defvar *solid-owners* (make-hash-table :test 'eq) "The table from an owner (a building node) to ((cell . outline) ...), so they can be dropped at once.")

(defun solid-cell (x z) (+ (* 100003 (floor x *solid-cell*)) (floor z *solid-cell*)))

(defun add-solid (owner x z hw hd yaw)
  "Record for OWNER a rectangle centred at (X Z) with half-sizes HW and HD, rotated by YAW.
YAW is the same angle as the rotation of a Godot node. Return the new solid."
  (let ((s (make-solid x z hw hd (cos yaw) (sin yaw))) (r (sqrt (+ (* hw hw) (* hd hd)))))
    (loop for cx from (floor (- x r) *solid-cell*) to (floor (+ x r) *solid-cell*)
          do (loop for cz from (floor (- z r) *solid-cell*) to (floor (+ z r) *solid-cell*)
                   do (let ((cell (+ (* 100003 cx) cz)))
                        (push s (gethash cell *solid-grid*))
                        (push (cons cell s) (gethash owner *solid-owners*)))))
    s))

(defun drop-solids (owner)
  "Drop the outlines of OWNER, a building that has been removed."
  (let ((mine (gethash owner *solid-owners*)))
    (when mine
      (loop for (cell . s) in mine
            do (let ((rest (delete s (gethash cell *solid-grid*) :count 1)))
                 (if rest (setf (gethash cell *solid-grid*) rest) (remhash cell *solid-grid*))))
      (remhash owner *solid-owners*))))

(defun clear-solids () (clrhash *solid-grid*) (clrhash *solid-owners*))

(defun solid-at-p (x z &optional (r .28))
  "Return true if point (X Z), padded by margin R, is inside something solid."
  (dolist (s (gethash (solid-cell x z) *solid-grid*) nil)
    (let* ((dx (- x (solid-x s))) (dz (- z (solid-z s)))
           (lx (- (* dx (solid-cos s)) (* dz (solid-sin s)))) (lz (+ (* dx (solid-sin s)) (* dz (solid-cos s)))))
      (when (and (< (abs lx) (+ (solid-hw s) r)) (< (abs lz) (+ (solid-hd s) r))) (return t)))))
