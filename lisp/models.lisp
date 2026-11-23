;;;; Models drawn in Blender. A thing may be drawn in Blender instead of being assembled from shapes in code:
;;;; models/NAME.blend is the drawing, tools/make-models.sh makes project/models/NAME.glb of it, and MODEL puts
;;;; that file into the scene. The game reads the file itself, as bytes, like its pictures and sounds, so nothing
;;;; has to be imported in the editor first. Axes and what goes into a model: see tools/make-models.sh.
;;;; What moves, glows on a switch or is clicked stays a node made in code; a model is the still part of a thing.

(in-package #:kiosk-trip)

(defun settle-model (node shadow)
  "Make the meshes under NODE look like the rest of the game: non-metals get almost no specular, as in MATERIAL.
A mesh whose name in Blender ends with _noshadow casts no shadow."
  (when (object-call node "is_class" "MeshInstance3D")
    (when (or (not shadow) (search "_noshadow" (princ-to-string (object-get node "name")))) (object-set node "cast_shadow" 0))
    (let ((mesh (object-get node "mesh")))
      (when mesh
        (dotimes (i (object-call mesh "get_surface_count"))
          (let ((m (object-call mesh "surface_get_material" i)))
            (when (and m (<= (object-get m "metallic") 0)) (object-set m "metallic_specular" .15)))))))
  (dotimes (i (object-call node "get_child_count"))
    (settle-model (object-call node "get_child" i) shadow)))

(defvar *models* (make-hash-table :test 'equal) "A table from the name of a model to its scene as read from the file: a node
that is never put into the tree; every MODEL puts a copy of it, and the copies share the meshes.")

(defun model-scene (name)
  "Return the scene of the model NAME, read once; NIL (and an error in the log) when the file is missing or broken."
  (or (gethash name *models*)
      (let* ((path (format nil "res://models/~a.glb" name))
             (root (and (file-here-p path)
                        (let ((doc (gdext:make-object "GLTFDocument")) (state (gdext:make-object "GLTFState")))
                          (and (eql 0 (object-call doc "append_from_buffer" (file-bytes path) "" state))
                               (object-call doc "generate_scene" state))))))
        (cond ((null root) (note-error (format nil "модель ~a не прочитана" path) "model") nil)
              (t (settle-model root t)
                 (setf (gethash name *models*) root))))))

(defun model (parent name &optional (x 0.0) (y 0.0) (z 0.0) &key rot scale (shadow t))
  "Put the model NAME (project/models/NAME.glb) under PARENT at the point (X Y Z) and return its root Node3D.
ROT is (x y z) in radians and SCALE is (x y z), as for SHAPE. A missing or broken file is logged as an error, and NIL is returned."
  (let* ((scene (model-scene name)) (root (and scene (object-call scene "duplicate"))))
    (when root
      (unless shadow (settle-model root nil))
      (node-add-child parent root)
      (node-3d-set-position root (v3 x y z))
      (when rot (node-3d-set-rotation root (v3 (first rot) (second rot) (third rot))))
      (when scale (node-3d-set-scale root (v3 (first scale) (second scale) (third scale))))
      root)))

;;; Moving a piece from code to Blender. Wrap the shapes of the piece - only those that never move, hide or get
;;; clicked on their own - in (drawn (parent "NAME") ...) and run tools/to-blender.sh: the game runs once with
;;; KIOSK_DRAW set to a folder and, instead of building those shapes, writes them to FOLDER/NAME.json, each with its
;;; sizes, colour and place under PARENT; tools/draw-model.py makes models/NAME.blend of that file, the same to the
;;; centimetre. Several forms may share a NAME and a PARENT: they make one model. PARENT may be a group that moves
;;; or hides: the model moves and hides with it. Then the first form is replaced by (model parent "NAME") and the
;;; others are deleted, and from there on the piece is edited in Blender. Without KIOSK_DRAW the forms are built as ever.

(defvar *drawn* (make-hash-table :test 'equal) "With *DRAW-DIR*: a table from the name of a piece to ((form . shapes) ...).")

(defun drawing-chain (parent)
  "Return the places of PARENT and of the groups above it up to the root of the drawing, the nearest first."
  (if (eq parent (first *drawing*))
      '()
      (let ((cell (gethash parent *drawn-groups*)))
        (unless cell (error "DRAWN: a shape is put under a node that is not under the parent of the piece"))
        (cons (rest cell) (drawing-chain (first cell))))))

(defun json-of (v)
  (cond ((null v) "null")
        ((eq v t) "true")
        ((keywordp v) (format nil "\"~(~a~)\"" v))
        ((stringp v) (format nil "\"~a\"" v))
        ((realp v) (format nil "~,5f" (float v 1d0)))
        (t (format nil "[~{~a~^,~}]" (mapcar #'json-of v)))))

(defun draw-out (root name form build)
  "Run BUILD with the shapes it puts under ROOT written down, and write the piece NAME to NAME.json in *DRAW-DIR*.
FORM names the place in the code: built again, a form replaces what it wrote before."
  (let ((*drawing* (list root '())) (*batch* nil) (*tracking* nil))
    (funcall build)
    (let ((lines (loop for s in (reverse (second *drawing*))
                       collect (destructuring-bind (parent kind dims rgb x y z &key emit energy rough metal rot scale alpha shadow unshaded) s
                                 (format nil "{\"kind\":~a,\"dims\":~a,\"rgb\":~a,\"emit\":~a,\"energy\":~a,\"rough\":~a,\"metal\":~a,\"alpha\":~a,\"shadow\":~a,\"unshaded\":~a,\"chain\":~a}"
                                         (json-of kind) (json-of dims) (json-of rgb) (json-of emit) (json-of energy) (json-of rough) (json-of metal)
                                         (json-of alpha) (json-of (and shadow t)) (json-of (and unshaded t))
                                         (json-of (cons (list (list x y z) (or rot '(0 0 0)) (or scale '(1 1 1))) (drawing-chain parent))))))))
      (setf (gethash name *drawn*) (cons (cons form lines) (remove form (gethash name *drawn*) :key #'car)))
      (let ((all (loop for (nil . ls) in (gethash name *drawn*) append ls)))
        (with-open-file (out (format nil "~a/~a.json" *draw-dir* name) :direction :output :if-exists :supersede)
          (format out "[~%~{~a~^,~%~}~%]~%" all))
        (format t "~&DRAWN ~a: ~d~%" name (length all))))))

(defmacro drawn ((parent name) &body body)
  "Build BODY, static shapes put under PARENT; with KIOSK_DRAW set, write them down for Blender instead (see above)."
  `(if *draw-dir*
       (draw-out ,parent ,name ',(gensym "FORM") (lambda () ,@body))
       (progn ,@body)))
