;;;; Models drawn in Blender. A thing may be drawn in Blender instead of being assembled from shapes in code:
;;;; models/NAME.blend is the drawing, tools/make-models.sh makes project/models/NAME.glb of it, and MODEL puts
;;;; that file into the scene. The game reads the file itself, as bytes, like its pictures and sounds, so nothing
;;;; has to be imported in the editor first. Axes and what goes into a model: see tools/make-models.sh.
;;;; What moves, glows on a switch or is clicked stays a node made in code; a model is the still part of a thing.

(in-package #:kiosk-trip)

(defun settle-model (node shadow)
  "Make the meshes under NODE look like the rest of the game: non-metals get almost no specular, as in MATERIAL."
  (when (object-call node "is_class" "MeshInstance3D")
    (unless shadow (object-set node "cast_shadow" 0))
    (let ((mesh (object-get node "mesh")))
      (when mesh
        (dotimes (i (object-call mesh "get_surface_count"))
          (let ((m (object-call mesh "surface_get_material" i)))
            (when (and m (<= (object-get m "metallic") 0)) (object-set m "metallic_specular" .15)))))))
  (dotimes (i (object-call node "get_child_count"))
    (settle-model (object-call node "get_child" i) shadow)))

(defun model (parent name &optional (x 0.0) (y 0.0) (z 0.0) &key rot scale (shadow t))
  "Put the model NAME (project/models/NAME.glb) under PARENT at the point (X Y Z) and return its root Node3D.
ROT is (x y z) in radians and SCALE is (x y z), as for SHAPE. A missing or broken file is logged as an error, and NIL is returned."
  (let* ((path (format nil "res://models/~a.glb" name))
         (root (and (file-here-p path)
                    (let ((doc (gdext:make-object "GLTFDocument")) (state (gdext:make-object "GLTFState")))
                      (and (eql 0 (object-call doc "append_from_buffer" (file-bytes path) "" state))
                           (object-call doc "generate_scene" state))))))
    (cond ((null root) (note-error (format nil "модель ~a не прочитана" path) "model") nil)
          (t (settle-model root shadow)
             (node-add-child parent root)
             (node-3d-set-position root (v3 x y z))
             (when rot (node-3d-set-rotation root (v3 (first rot) (second rot) (third rot))))
             (when scale (node-3d-set-scale root (v3 (first scale) (second scale) (third scale))))
             root))))
