;;;; Building blocks of the scene: a color from "#rrggbb", materials (cached by look) and shapes -
;;;; box, cylinder, cone, ball, a rough 'rock'. Each shape is a MeshInstance3D
;;;; under a parent, with its own mesh; identical ones share a material. Everything that builds
;;;; the world and the wagon (world.lisp, cart.lisp) comes from here.

(in-package #:kiosk-trip)

;;; Node text is written in the game's language. The Russian source stays on the node (meta ru_<property>)
;;; so that labels already in place can be rewritten when the language changes (RETRANSLATE).

(defun object-set (object property value)
  (when (and (stringp value) (nth-value 1 (gethash value *en*))
             (member property '("text" "placeholder_text" "title") :test #'string=))
    (object-call object "set_meta" (concatenate 'string "ru_" property) value)
    (setf value (tr value)))
  (when (and (eq *lang* :en) (stringp value) (string= property "text") (find-if (lambda (c) (char<= #\а c #\я)) value))
    (pushnew value *untranslated* :test #'string=))
  (godot:object-set object property value))

(defun retranslate (node)
  "Rewrite every label under NODE that has a translation into the current language."
  (dolist (property '("text" "placeholder_text" "title"))
    (let ((meta (concatenate 'string "ru_" property)))
      (when (object-call node "has_meta" meta)
        (godot:object-set node property (tr (object-call node "get_meta" meta))))))
  (dotimes (i (object-call node "get_child_count"))
    (retranslate (object-call node "get_child" i))))

(defvar *colors* (make-hash-table :test 'equal) "A table from (\"#rrggbb\" . alpha) to the color.")

(defun hex (text &optional (alpha 1.0))
  "Return the color for TEXT, a string like '#rrggbb', with opacity ALPHA.
Each color is parsed only once."
  (let ((key (cons text alpha)))
    (or (gethash key *colors*)
        (setf (gethash key *colors*)
              (flet ((part (i) (/ (parse-integer text :start i :end (+ i 2) :radix 16) 255.0)))
                (color (part 1) (part 3) (part 5) alpha))))))

(defun mix-color (a b tt)
  (color (+ (color-r a) (* tt (- (color-r b) (color-r a)))) (+ (color-g a) (* tt (- (color-g b) (color-g a))))
         (+ (color-b a) (* tt (- (color-b b) (color-b a)))) 1.0))

(defun v3 (x y z) (vector3 x y z))

;;; Materials: one per combination (color, emission, metal, vertex colors, transparency)

(defvar *materials* (make-hash-table :test 'equal)
  "A table from a list of material parameters to a StandardMaterial3D holding its own reference.")

(defun material (rgb &key emit (energy 1.5) (rough .9) (metal 0.0) vertex alpha unshaded double)
  (let ((key (list rgb emit energy rough metal vertex alpha unshaded double)))
    (or (gethash key *materials*)
        (let ((m (gdext:make-object "StandardMaterial3D")))
          (object-set m "albedo_color" (hex rgb (or alpha 1.0)))
          (object-set m "roughness" rough)
          (object-set m "metallic" metal)
          (object-set m "metallic_specular" (if (> metal 0) .5 .15))   ; non-metals get almost no specular - no sparkling far away
          ;; vertex colors are given in sRGB, like "#rrggbb"
          (when vertex (object-set m "vertex_color_use_as_albedo" t) (object-set m "vertex_color_is_srgb" t))
          (when alpha (object-set m "transparency" 1))           ; TRANSPARENCY_ALPHA
          (when unshaded (object-set m "shading_mode" 0))        ; SHADING_MODE_UNSHADED
          (when double (object-set m "cull_mode" 2))             ; visible from both sides
          (when emit
            (object-set m "emission_enabled" t)
            (object-set m "emission" (hex emit))
            (object-set m "emission_energy_multiplier" energy))
          (gdext:setf-reference (gethash key *materials*) m)
          m))))

;;; Shapes

(defvar *meshes* (make-hash-table :test 'equal) "A table from (kind . dimensions) to the mesh; identical shapes share a mesh.")

(defun mesh-of (kind dims)
  "Return the shared mesh of kind KIND with dimensions DIMS.
KIND is :box (w h d), :cyl (r-top r-bottom h segments), :cone (r h segments),
:ball (r segments rings) or :rock (r), a rough polyhedron. Dimensions are rounded to the
centimeter, so random rocks share meshes too."
  (let ((key (cons kind (mapcar (lambda (v) (/ (round (* v 100)) 100.0)) dims))))
    (or (gethash key *meshes*)
        (let ((m (make-mesh kind (rest key))))
          (gdext:setf-reference (gethash key *meshes*) m)
          m))))

(defun make-mesh (kind dims)
  (ecase kind
    (:box (let ((m (gdext:make-object "BoxMesh")))
            (object-set m "size" (v3 (first dims) (second dims) (third dims))) m))
    (:cyl (let ((m (gdext:make-object "CylinderMesh")))
            (destructuring-bind (rt rb h &optional (seg 8)) dims
              (object-set m "top_radius" rt) (object-set m "bottom_radius" rb)
              (object-set m "height" h) (object-set m "radial_segments" seg) (object-set m "rings" 1))
            m))
    (:cone (make-mesh :cyl (list 0.0 (first dims) (second dims) (or (third dims) 7))))
    (:ball (let ((m (gdext:make-object "SphereMesh")))
             (destructuring-bind (r &optional (seg 10) (rings 6)) dims
               (object-set m "radius" r) (object-set m "height" (* 2 r))
               (object-set m "radial_segments" seg) (object-set m "rings" rings))
             m))
    (:rock (make-mesh :ball (list (first dims) 5 3)))))

;;; Solids (tverd.lisp). While WITH-SOLIDS runs, we record where groups and shapes are placed, and at the end
;;; we work out where they ended up on the map and hand the grid the outlines of everything that stands on the
;;; ground and reaches knee height: walls, posts, trunks, boulders. Roofs, eaves, slabs underfoot and benches are
;;; not solid. The record lives only while one thing is being built; the outlines belong to its root node (under
;;; World): when the node is removed (NODE-QUEUE-FREE), the outlines go too.

(defvar *tracking* nil "The build record while one is being taken, (groups . shapes), else NIL.
A group is (node parent x y z yaw sx sz).")

(defvar *batch* nil "See Batches below. While a batch is gathered: (root offsets lots). OFFSETS is a table from a group that only shifts its shapes to
(parent x y z); LOTS a table from (mesh material shadow range) to the pending shapes of that lot.")

(defvar *draw-dir* (ext:getenv "KIOSK_DRAW") "The folder pieces are written down into for Blender (models.lisp, DRAWN), or NIL: the usual game.")
(defvar *drawing* nil "While a piece is written down for Blender: (root shapes), SHAPES the shapes noted so far; else NIL.")
(defvar *drawn-groups* (make-hash-table :test 'eq) "With *DRAW-DIR*: a table from every group made to (parent position rotation scale).")

(defun drawing-set (node slot v)
  "With *DRAW-DIR*: note the new position (SLOT 1), rotation (2) or scale (3) of a group."
  (let ((cell (and *draw-dir* (gethash node *drawn-groups*))))
    (when cell (setf (nth slot cell) (list (vector3-x v) (vector3-y v) (vector3-z v))))))

(defmacro with-solids (&body body)
  "Build BODY and hand the outlines of what was built to the solids grid."
  `(if *tracking*
       (progn ,@body)
       (let ((*tracking* (cons nil nil))) (multiple-value-prog1 (progn ,@body) (commit-solids)))))

(defmacro without-solids (&body body)
  "Build BODY without making it solid: for things that walk and drive."
  `(let ((*tracking* nil)) ,@body))

(defun tracked (node) (assoc node (car *tracking*) :test #'eq))

(defun node-3d-set-position (node v)
  (drawing-set node 1 v)
  (when *batch*
    (let ((cell (gethash node (second *batch*)))) (when cell (setf (rest cell) (list (vector3-x v) (vector3-y v) (vector3-z v))))))
  (let ((e (and *tracking* (tracked node))))
    (when e (setf (third e) (vector3-x v) (fourth e) (vector3-y v) (fifth e) (vector3-z v))))
  (godot:node-3d-set-position node v))

(defun node-3d-set-rotation (node v)
  (drawing-set node 2 v)
  (unbatch node)
  (let ((e (and *tracking* (tracked node))))
    (when e (setf (sixth e) (vector3-y v))))
  (godot:node-3d-set-rotation node v))

(defun node-3d-set-scale (node v)
  (drawing-set node 3 v)
  (unbatch node)
  (let ((e (and *tracking* (tracked node))))
    (when e (setf (seventh e) (vector3-x v) (eighth e) (vector3-z v))))
  (godot:node-3d-set-scale node v))

(defun node-queue-free (node)
  (drop-solids node)
  (godot:node-queue-free node))

(defun commit-solids ()
  "Turn the build record into outlines in the solids grid.
Each shape is lifted through its chain of groups up to the root, which is under World."
  (loop for (parent kind dims x y z rot scale) in (cdr *tracking*)
        do (let ((yaw (if rot (second rot) 0.0)) (sx (if scale (first scale) 1.0)) (sy (if scale (second scale) 1.0))
                 (sz (if scale (third scale) 1.0)) (root nil))
             (loop for e = (tracked parent)
                   while e
                   do (destructuring-bind (node up px py pz pyaw psx psz) e
                        (let ((ox (* x psx)) (oz (* z psz)))
                          (setf x (+ px (* ox (cos pyaw)) (* oz (sin pyaw)))
                                z (+ pz (* oz (cos pyaw)) (- (* ox (sin pyaw))))
                                yaw (+ yaw pyaw) sx (* sx psx) sz (* sz psz) sy (* sy psx)))
                        (if (eq up *world*)
                            (progn (setf root node) (return))
                            (setf y (+ (* y psx) py) parent up))))            ; height is measured from the root: it stands on the ground
             (when root
               (multiple-value-bind (hw hd hh)
                   (ecase kind
                     (:box (values (* .5 sx (first dims)) (* .5 sz (third dims)) (* .5 sy (second dims))))
                     (:cyl (let ((r (max (first dims) (second dims)))) (values (* sx r) (* sz r) (* .5 sy (third dims)))))
                     (:cone (values (* sx (first dims)) (* sz (first dims)) (* .5 sy (second dims))))
                     (:rock (values (* sx (first dims)) (* sz (first dims)) (* sy (first dims)))))
                 (when (and (< (- y hh) 1.1) (> (+ y hh) .5) (>= hh .17) (>= (min hw hd) .03))
                   (add-solid root x z hw hd yaw)))))))

(defparameter *small-size* .7 "The largest side, in meters, below which a shape is a trifle, visible up to *SMALL-RANGE*.")
(defparameter *small-range* 110.0)
(defparameter *medium-size* 2.6 "The largest side, in meters, below which a shape is a medium detail, visible up to *MEDIUM-RANGE*.")
(defparameter *medium-range* 320.0)

(defun shape-size (kind dims)
  "Return the largest side, in meters, of a shape of kind KIND with dimensions DIMS."
  (ecase kind
    (:box (max (first dims) (second dims) (third dims)))
    (:cyl (max (* 2 (first dims)) (* 2 (second dims)) (third dims)))
    (:cone (max (* 2 (first dims)) (second dims)))
    ((:ball :rock) (* 2 (first dims)))))

;;; Batches. A thing built of many shapes costs the renderer one draw for every shape. While WITH-BATCH runs,
;;; shapes put straight under its root - or under groups that only shift them - are not made into nodes but
;;; noted down, and at the end all that share a mesh and a material become one MultiMesh: one draw for the lot.
;;; A shape that has to be a node of its own after all (it will move) is made one by SOLO; a group that is turned,
;;; scaled or set moving takes its shapes out of the batch.

(defstruct (pending (:constructor pending (key args)))
  "A shape noted down in a batch: KEY is what it shares with the others of its lot, ARGS the arguments of SHAPE."
  key args)

(defun batch-offset (parent)
  "Return the shift of PARENT against the root of the batch as a list (x y z), or NIL if its shapes cannot be batched."
  (cond ((eq parent (first *batch*)) '(0.0 0.0 0.0))
        (t (let ((cell (gethash parent (second *batch*))))
             (when cell
               (let ((up (batch-offset (first cell))))
                 (and up (mapcar #'+ up (rest cell)))))))))

(defun solo (thing)
  "Make the pending shape THING a node of its own and return the node; anything else is returned as it is."
  (if (pending-p thing)
      (let ((lot (gethash (pending-key thing) (third *batch*))))
        (setf (gethash (pending-key thing) (third *batch*)) (remove thing lot))
        (let ((*batch* nil)) (apply #'shape (pending-args thing))))
      thing))

(defun unbatch (group)
  "Take GROUP out of the batch: it is going to turn, grow or move, so its shapes become nodes of their own."
  (when (and *batch* (gethash group (second *batch*)))
    (let ((mine '()))
      (maphash (lambda (key lot) (declare (ignore key)) (dolist (p lot) (when (eq (first (pending-args p)) group) (push p mine)))) (third *batch*))
      (mapc #'solo mine))
    (remhash group (second *batch*))
    (loop for child being the hash-keys of (second *batch*) using (hash-value cell)        ; and the groups under it with it
          when (eq (first cell) group) collect child into children
          finally (mapc #'unbatch children))))

(defun place-transform (x y z rot scale)
  "Return the Transform3D of a shape at (X Y Z) turned by ROT, (x y z) in radians as Godot turns a node, and scaled by SCALE."
  (destructuring-bind (&optional (a 0.0) (b 0.0) (c 0.0)) rot
    (destructuring-bind (&optional (sx 1.0) (sy 1.0) (sz 1.0)) scale
      (let ((ca (cos a)) (sa (sin a)) (cb (cos b)) (sb (sin b)) (cc (cos c)) (sc (sin c)))
        (transform3d (basis (vector3 (* sx (+ (* cb cc) (* sb sa sc))) (* sx ca sc) (* sx (- (* cb sa sc) (* sb cc))))
                            (vector3 (* sy (- (* sb sa cc) (* cb sc))) (* sy ca cc) (* sy (+ (* sb sc) (* cb sa cc))))
                            (vector3 (* sz sb ca) (* sz (- sa)) (* sz cb ca)))
                     (vector3 x y z))))))

(defun commit-batch ()
  "Turn what the batch has gathered into nodes under its root: a MultiMesh for every lot, a plain node for a lot of one."
  (destructuring-bind (root offsets lots) *batch*
    (declare (ignore offsets))
    (let ((batch *batch*) (*batch* nil))
      (maphash (lambda (key lot)
                 (cond ((null lot))
                       ((null (rest lot)) (apply #'shape (pending-args (first lot))))
                       (t (destructuring-bind (mesh mat shadow range) key
                            (let ((mm (gdext:make-object "MultiMesh")) (mmi (gdext:make-object "MultiMeshInstance3D")))
                              (object-set mm "transform_format" 1)                  ; TRANSFORM_3D
                              (object-set mm "mesh" mesh)
                              (object-set mm "instance_count" (length lot))
                              (loop for p in lot for n from 0
                                    do (destructuring-bind (parent kind dims rgb x y z &key rot scale &allow-other-keys) (pending-args p)
                                         (declare (ignore kind dims rgb))
                                         (destructuring-bind (ox oy oz) (let ((*batch* batch)) (batch-offset parent))
                                           (object-call mm "set_instance_transform" n (place-transform (+ ox x) (+ oy y) (+ oz z) rot scale)))))
                              (object-set mmi "multimesh" mm)
                              (object-set mmi "material_override" mat)
                              (unless shadow (object-set mmi "cast_shadow" 0))
                              ;; a lot lies all over its cell: judged from the cell's middle
                              (when range (object-set mmi "visibility_range_end" (+ range 60.0)))
                              (node-add-child root mmi))))))
               lots))))

(defmacro with-batch ((root) &body body)
  "Build BODY, gathering the shapes put under ROOT into as few draws as there are different ones among them."
  `(if (or *batch* *tracking*)
       (progn ,@body)
       (let ((*batch* (list ,root (make-hash-table :test 'eq) (make-hash-table :test 'equal))))
         (multiple-value-prog1 (progn ,@body) (commit-batch)))))

(defun shape-range (kind dims scale emit)
  "Return how far a shape of this size is drawn, in metres, or NIL for as far as the eye sees: trifles are not drawn
far away, where they are a dot in the haze yet cost the same as a house. Glowing things are seen from afar."
  (unless emit
    (let ((size (* (shape-size kind dims) (if scale (max (first scale) (second scale) (third scale)) 1.0))))
      (cond ((< size *small-size*) *small-range*)
            ((< size *medium-size*) *medium-range*)))))

(defun shape (parent kind dims rgb x y z &key emit (energy 1.5) (rough .9) (metal 0.0)
                                              rot scale alpha (shadow t) unshaded)
  "Add a shape of kind KIND and dimensions DIMS under PARENT at the point (X Y Z).
RGB is its color, ROT is (x y z) in radians and SCALE is (x y z). Return the MeshInstance3D."
  (when *drawing*                                                         ; written down for Blender, not built
    (push (list parent kind dims rgb x y z :emit emit :energy energy :rough rough :metal metal
                :rot rot :scale scale :alpha alpha :shadow shadow :unshaded unshaded) (second *drawing*))
    (return-from shape nil))
  (when (and *batch* (not *tracking*) (batch-offset parent))             ; noted down for the batch, not made a node
    (let* ((key (list (mesh-of kind dims) (material rgb :emit emit :energy energy :rough rough :metal metal :alpha alpha :unshaded unshaded)
                      (and shadow t) (shape-range kind dims scale emit)))
           (p (pending key (list parent kind dims rgb x y z :emit emit :energy energy :rough rough :metal metal
                                 :rot rot :scale scale :alpha alpha :shadow shadow :unshaded unshaded))))
      (push p (gethash key (third *batch*)))
      (return-from shape p)))
  (let ((mi (gdext:make-object "MeshInstance3D")))
    (object-set mi "mesh" (mesh-of kind dims))
    (object-set mi "material_override" (material rgb :emit emit :energy energy :rough rough
                                                     :metal metal :alpha alpha :unshaded unshaded))
    (unless shadow (object-set mi "cast_shadow" 0))
    (let ((range (shape-range kind dims scale emit))) (when range (object-set mi "visibility_range_end" range)))
    (node-add-child parent mi)
    (node-3d-set-position mi (v3 x y z))
    (when rot (node-3d-set-rotation mi (v3 (first rot) (second rot) (third rot))))
    (when scale (node-3d-set-scale mi (v3 (first scale) (second scale) (third scale))))
    (when (and *tracking* (member kind '(:box :cyl :cone :rock)) (not alpha)
               (or (null rot) (and (< (abs (first rot)) .2) (< (abs (third rot)) .2))))        ; lying and tilted things are not a wall
      (push (list parent kind dims x y z rot scale) (cdr *tracking*)))
    mi))

(defun plain-text (node text)
  "Set the text of NODE to TEXT as it is, past the translation: for text that is already in the game language."
  (godot:object-set node "text" text))

(defun group (parent &optional (x 0.0) (y 0.0) (z 0.0))
  "Add an empty Node3D under PARENT at (X Y Z) and return it.
It lets a thing be assembled from shapes and moved as a whole."
  (let ((g (gdext:make-object "Node3D")))
    (node-add-child parent g)
    (node-3d-set-position g (v3 x y z))
    (when (and *tracking* (or (eq parent *world*) (tracked parent)))
      (push (list g parent x y z 0.0 1.0 1.0) (car *tracking*)))
    (when *draw-dir* (setf (gethash g *drawn-groups*) (list parent (list x y z) (list 0 0 0) (list 1 1 1))))
    (when (and *batch* (not *tracking*) (batch-offset parent))
      (setf (gethash g (second *batch*)) (list parent (float x 1.0) (float y 1.0) (float z 1.0))))
    g))

;;; Lamps. A lamp is a bulb that glows and, often, the light it gives; the two are put together here, so that every
;;; lamp of one tone is the same colour, and so that there is one place to count the lights: each point light
;;; costs the renderer another pass over everything it reaches.

(defparameter *lamp-tones*
  '(:warm   (:rgb "#ffe2a8" :glow "#ffcf8a" :light "#ffcf8a")             ; a bulb under a tin shade
    :soft   (:rgb "#fff0c8" :glow "#ffe2a8" :light "#ffd9a0")             ; a globe, a chandelier
    :amber  (:rgb "#ffcf8a" :glow "#ffb060" :light "#ffb866")             ; a sodium lamp
    :window (:rgb "#ffcf7a" :glow "#ffb866" :light "#ffb866")             ; a window with somebody at home
    :cold   (:rgb "#f4f6ff" :glow "#e8f0ff" :light "#dfe8ff")             ; a floodlight
    :alarm  (:rgb "#ff3020" :glow "#ff2010" :light "#ff5a40"))            ; a red lamp
  "The tones of lamp: the colour of the bulb, of its glow, and of the light it gives.")

(defun lamp (parent tone kind dims x y z &key reach (power 2.0) at (shine 3) unshaded rot scale)
  "Put a lamp of the tone TONE (a key of *LAMP-TONES*) under PARENT: a glowing bulb, the shape KIND of DIMS at (X Y Z), and,
with REACH, the light it gives - a point light of that range and of POWER, at the bulb or, where the bulb is set in a
wall and the light has to stand off it, at AT, a list (x y z). SHINE is how bright the bulb itself glows. Return the bulb."
  (let* ((spec (or (getf *lamp-tones* tone) (error "No lamp of the tone ~s" tone)))
         (bulb (shape parent kind dims (getf spec :rgb) x y z :emit (getf spec :glow) :energy shine
                      :unshaded unshaded :rot rot :scale scale :shadow nil)))
    (when reach
      (destructuring-bind (lx ly lz) (or at (list x y z))
        (light parent (getf spec :light) power reach lx ly lz)))
    bulb))

(defun light (parent rgb energy range x y z)
  "Add a point light (OmniLight3D) of color RGB, ENERGY and RANGE under PARENT at (X Y Z).
Return the light."
  (let ((l (gdext:make-object "OmniLight3D")))
    (object-set l "light_color" (hex rgb))
    (object-set l "light_energy" energy)
    (object-set l "omni_range" range)
    (node-add-child parent l)
    (node-3d-set-position l (v3 x y z))
    l))

