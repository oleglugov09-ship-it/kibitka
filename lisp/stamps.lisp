;;;; Postage stamps. Each town has its own stamp: a picture, no letters (tools/make-stamps.py draws them).
;;;; The stamp stands for the town on the map and on parcels instead of its name, so matching a parcel to
;;;; its town is done by eye.

(in-package #:kiosk-trip)

(defvar *stamp-textures* (make-hash-table) "Town number -> the texture of its stamp.")

(defun stamp-texture (town)
  "Return the texture of the stamp of TOWN, or NIL if there is no picture for it."
  (or (gethash town *stamp-textures*)
      (let ((path (cl:format nil "res://textures/stamps/~d.png" town)))
        (let ((tx (picture-texture path)))
          (when tx (gdext:setf-reference (gethash town *stamp-textures*) tx) tx)))))

(defun stamp (parent town x y z height &key (yaw 0.0) (pitch 0.0))
  "Stick the stamp of TOWN, HEIGHT metres tall, at (X Y Z) under PARENT, turned by YAW and PITCH like SIGN.
Return the node, or NIL if the town has no stamp."
  (let ((tx (stamp-texture town)))
    (when tx
      (let ((s (gdext:make-object "Sprite3D")))
        (object-set s "texture" tx)
        (object-set s "pixel_size" (/ height 48.0))
        (object-set s "texture_filter" 0)                       ; pixel for pixel: a stamp is small and must stay crisp
        (object-set s "alpha_cut" 1)
        (node-add-child parent s)
        (node-3d-set-position s (v3 x y z))
        (node-3d-set-rotation s (v3 pitch yaw 0))
        s))))
