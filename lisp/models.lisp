;;;; Models drawn in Blender. A thing may be drawn in Blender instead of being assembled from shapes in code:
;;;; models/NAME.blend is the drawing, tools/make-models.sh makes project/models/NAME.glb of it, and MODEL puts
;;;; that file into the scene. The game reads the file itself, as bytes, like its pictures and sounds, so nothing
;;;; has to be imported in the editor first. Axes and what goes into a model: see tools/make-models.sh.
;;;; What moves, glows on a switch or is clicked stays a node made in code; a model is the still part of a thing.

(in-package #:kiosk-trip)

(defun name-range (name)
  "Return how far a mesh named NAME in Blender is drawn: its name ends with _r and the metres (_r110), or NIL."
  (let ((at (search "_r" name :from-end t)))
    (and at (< (+ at 2) (length name)) (every #'digit-char-p (subseq name (+ at 2)))
         (float (parse-integer name :start (+ at 2))))))

(defun words (text)
  "Return the words of TEXT, which are separated by single spaces."
  (loop for start = 0 then (1+ end) for end = (position #\Space text :start start)
        collect (subseq text start end) while end))

(defun game-material (name)
  "Return the game's own material for a Blender material named like \"#8a6440\" or \"#ffe9b0 emit #ffd890 2.5 metal 0.5
rough 0.3 alpha 0.2 unshaded\": the colour and what MATERIAL takes. Such a material looks in the game as that colour
does on a shape built in code - with the texture of the current style, if the colour has one. NIL for any other name:
that material is shown as it was drawn in Blender."
  (when (and (> (length name) 6) (char= (char name 0) #\#))
    (let ((ws (words name)) (args '()) (*read-eval* nil) (*read-default-float-format* 'single-float))
      (flet ((number (w) (let ((v (ignore-errors (read-from-string w)))) (if (realp v) (float v 1.0) 0.0))))
        (loop for (w . more) on (rest ws)
              do (cond ((string= w "emit") (setf args (list* :emit (first more) :energy (number (second more)) args)))
                       ((string= w "metal") (setf args (list* :metal (number (first more)) args)))
                       ((string= w "rough") (setf args (list* :rough (number (first more)) args)))
                       ((string= w "alpha") (setf args (list* :alpha (number (first more)) args)))
                       ((string= w "unshaded") (setf args (list* :unshaded t args))))))
      (and (= (length (first ws)) 7) (ignore-errors (apply #'material (first ws) args))))))

(defun settle-model (node shadow)
  "Make the meshes under NODE look like the rest of the game. A material named after a colour becomes the game's own
material of that colour (GAME-MATERIAL); in any other, non-metals get almost no specular, as in MATERIAL.
A mesh whose name in Blender has _noshadow in it casts no shadow; one that ends with _r110 is drawn up to 110 m away."
  (when (object-call node "is_class" "MeshInstance3D")
    (let ((name (princ-to-string (object-get node "name"))))
      (when (or (not shadow) (search "_noshadow" name)) (object-set node "cast_shadow" 0))
      (let ((range (name-range name))) (when range (object-set node "visibility_range_end" range))))
    (let ((mesh (object-get node "mesh")))
      (when mesh
        (dotimes (i (object-call mesh "get_surface_count"))
          (let* ((m (object-call mesh "surface_get_material" i))
                 (own (and m (game-material (princ-to-string (object-get m "resource_name"))))))
            (cond (own (object-call mesh "surface_set_material" i own))
                  ((and m (<= (object-get m "metallic") 0)) (object-set m "metallic_specular" .15))))))))
  (dotimes (i (object-call node "get_child_count"))
    (settle-model (object-call node "get_child" i) shadow)))

(defvar *models* (make-hash-table :test 'equal) "A table from the name of a model to (scene solids points surfaces), see TAKE-PLACES. SCENE is the model as read
from the file: a node that is never put into the tree; every MODEL puts a copy of it, and the copies share the meshes.
SOLIDS are its obstacles: the objects named solid... in Blender, each a cube stretched over what cannot be walked through,
as (x y z yaw width height depth) - they are not drawn.")

(defun take-places (root)
  "Take out of the model ROOT what is there for the rules, not for the eye, and return it as three lists.
Objects named solid...: obstacles, each as (x y z yaw w h d). Empties named at_NAME: points, each as (name x y z).
Cubes named surface_NAME: the top face is a place to stand or put things, each as (name x top z width depth)."
  (let ((solids '()) (points '()) (surfaces '()))
    (loop for child across (node-get-children root)
          do (let ((name (princ-to-string (object-get child "name"))))
               (flet ((starts (prefix) (eql 0 (search prefix name))))
                 (when (or (starts "solid") (starts "at_") (starts "surface_"))
                   (let ((at (object-get child "position")) (rot (object-get child "rotation")) (size (object-get child "scale")))
                     (let ((x (vector3-x at)) (y (vector3-y at)) (z (vector3-z at))
                           (w (abs (vector3-x size))) (h (abs (vector3-y size))) (d (abs (vector3-z size))))
                       (cond ((starts "solid") (push (list x y z (vector3-y rot) w h d) solids))
                             ((starts "at_") (push (list (subseq name 3) x y z) points))
                             (t (push (list (subseq name 8) x (+ y (/ h 2)) z w d) surfaces)))))
                   (object-call root "remove_child" child)
                   (godot:node-queue-free child)))))
    (list solids points surfaces)))

(defun model-entry (name)
  "Return (scene solids points surfaces) of the model NAME, read once; NIL (and an error in the log) when the file is missing or broken."
  (or (gethash name *models*)
      (let* ((path (format nil "res://models/~a.glb" name))
             (root (and (file-here-p path)
                        (let ((doc (gdext:make-object "GLTFDocument")) (state (gdext:make-object "GLTFState")))
                          (and (eql 0 (object-call doc "append_from_buffer" (file-bytes path) "" state))
                               (object-call doc "generate_scene" state))))))
        (cond ((null root) (note-error (format nil "модель ~a не прочитана" path) "model") nil)
              (t (let ((places (take-places root)))
                   (settle-model root t)
                   (setf (gethash name *models*) (cons root places))))))))

(defun model-point (model name)
  "Return the point NAME of MODEL (an empty named at_NAME in its Blender file) as (x y z), or NIL."
  (rest (assoc name (third (model-entry model)) :test #'string=)))

(defun model-colours (model)
  "Return the colours of the materials of MODEL as a list of (material-name . \"#rrggbb\")."
  (let ((out '()))
    (labels ((walk (node)
               (when (object-call node "is_class" "MeshInstance3D")
                 (let ((mesh (object-get node "mesh")))
                   (when mesh
                     (dotimes (i (object-call mesh "get_surface_count"))
                       (let ((m (object-call mesh "surface_get_material" i)))
                         (when m
                           (let ((c (object-get m "albedo_color")))
                             (push (cons (princ-to-string (object-get m "resource_name"))
                                         (format nil "#~(~2,'0x~2,'0x~2,'0x~)" (round (* 255 (color-r c))) (round (* 255 (color-g c))) (round (* 255 (color-b c)))))
                                   out))))))))
               (dotimes (i (object-call node "get_child_count")) (walk (object-call node "get_child" i)))))
      (let ((entry (model-entry model))) (when entry (walk (first entry)))))
    out))

(defun model-points (model prefix)
  "Return the points of MODEL whose names begin with PREFIX, each as (x y z)."
  (loop for (name . at) in (third (model-entry model)) when (eql 0 (search prefix name)) collect at))

(defun model-surface (model name)
  "Return the surface NAME of MODEL (a cube named surface_NAME in its Blender file) as (x top z width depth), or NIL."
  (rest (assoc name (fourth (model-entry model)) :test #'string=)))

(defun model-surfaces (model prefix)
  "Return the surfaces of MODEL whose names begin with PREFIX, each as (x top z width depth), in the order of their names."
  (mapcar #'rest (sort (remove-if-not (lambda (s) (eql 0 (search prefix (first s)))) (copy-list (fourth (model-entry model))))
                       #'string< :key #'first)))

(defun paint-model (node paint)
  "Give the meshes under NODE whose material is named paint0, paint1, ... in Blender the colours of the list PAINT."
  (when (object-call node "is_class" "MeshInstance3D")
    (dotimes (surface (let ((mesh (object-get node "mesh"))) (if mesh (object-call mesh "get_surface_count") 0)))
     (let* ((m (object-call (object-get node "mesh") "surface_get_material" surface))
           (name (if m (princ-to-string (object-get m "resource_name")) "")))
      (when (and (> (length name) 5) (eql 0 (search "paint" name)) (digit-char-p (char name 5)))
        (let* ((rgb (nth (digit-char-p (char name 5)) paint)) (ws (words name)) (glow (member "emit" ws :test #'string=)))
          (when rgb                                                 ; "paint1 emit own 0.5": it glows with its own colour
            (object-call node "set_surface_override_material" surface
                         (if glow
                             (material rgb :emit rgb :energy (let ((v (ignore-errors (let ((*read-eval* nil)) (read-from-string (third glow))))))
                                                               (if (realp v) (float v 1.0) 1.0)))
                             (material rgb :rough (/ (round (* 100 (object-get m "roughness"))) 100.0)
                                           :metal (/ (round (* 100 (object-get m "metallic"))) 100.0))))))))))
  (dotimes (i (object-call node "get_child_count"))
    (paint-model (object-call node "get_child" i) paint)))

(defun model (parent name &optional (x 0.0) (y 0.0) (z 0.0) &key rot scale (shadow t) paint)
  "Put the model NAME (project/models/NAME.glb) under PARENT at the point (X Y Z) and return its root Node3D.
ROT is (x y z) in radians and SCALE is (x y z), as for SHAPE. PAINT is a list of colours for the materials named
paint0, paint1, ... in Blender: one model, many liveries. Where the world is being built (WITH-SOLIDS), the model's
obstacles go to the solids grid like shapes do; a model that has any is put unturned and unscaled, its parent turns.
A missing or broken file is logged as an error, and NIL is returned."
  (let* ((entry (model-entry name)) (root (and entry (object-call (car entry) "duplicate"))))
    (when root
      (unless shadow (settle-model root nil))
      (when paint (paint-model root paint))
      (node-add-child parent root)
      (godot:node-3d-set-position root (v3 x y z))
      (when rot (godot:node-3d-set-rotation root (v3 (first rot) (second rot) (third rot))))
      (when scale (godot:node-3d-set-scale root (v3 (first scale) (second scale) (third scale))))
      (when *tracking*
        (loop for (sx sy sz yaw w h d) in (second entry)
              do (push (list parent :box (list w h d) (+ x sx) (+ y sy) (+ z sz) (list 0.0 yaw 0.0) nil) (cdr *tracking*))))
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

(defvar *paint-show* nil "With *DRAW-DIR*: the colours to show in Blender for the paints of the piece being written down.")

(defun draw-out (root name form paint build)
  "Run BUILD with the shapes it puts under ROOT written down, and write the piece NAME to NAME.json in *DRAW-DIR*.
FORM names the place in the code: built again, a form replaces what it wrote before. PAINT is the list of colours
that are paints: shapes of these colours get the materials paint0, paint1, ..."
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
          (format out "{\"paint\":~a,\"show\":~a,\"shapes\":[~%~{~a~^,~%~}~%]}~%" (if paint (json-of paint) "[]") (if *paint-show* (json-of *paint-show*) "[]") all))
        (format t "~&DRAWN ~a: ~d~%" name (length all))))))

(defmacro drawn ((parent name &key paint) &body body)
  "Build BODY, static shapes put under PARENT; with KIOSK_DRAW set, write them down for Blender instead (see above)."
  `(if *draw-dir*
       (draw-out ,parent ,name ',(gensym "FORM") ,paint (lambda () ,@body))
       (progn ,@body)))
