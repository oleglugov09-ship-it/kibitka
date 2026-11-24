;;;; Baking the map. The base world is drawn from files (zemlya.lisp); this is how the files are made of the
;;;; formulas of the land: tools/bake-map.sh runs the game once without a window with KIOSK_BAKE set, and the game,
;;;; instead of starting, walks every region within *BAKE-REACH* of a road, asks the formulas for the height, the
;;;; colour and the kind of ground of each of its points, and writes the regions (project/terrain/data), the water
;;;; (project/terrain/water) and the roads (project/terrain/roads.bin).
;;;; KIOSK_BAKE_ONLY="x0 z0 x1 z1" bakes only the regions from (x0 z0) to (x1 z1): a quick look at a piece.

(in-package #:kiosk-trip)

(defparameter *bake-reach* 2000.0 "How far from a road, in metres, the land is baked; beyond it lies Terrain3D's flat background.")

(defun put-f32 (bytes at x)
  "Write X into BYTES at index AT as a little-endian IEEE single float."
  (let ((bits (if (zerop x) 0
                  (multiple-value-bind (m e s) (integer-decode-float (float x 1f0))
                    (logior (if (minusp s) #x80000000 0) (ash (+ e 150) 23) (logand m #x7fffff))))))
    (setf (aref bytes at) (ldb (byte 8 0) bits) (aref bytes (+ at 1)) (ldb (byte 8 8) bits)
          (aref bytes (+ at 2)) (ldb (byte 8 16) bits) (aref bytes (+ at 3)) (ldb (byte 8 24) bits))))

(defun put-u32 (bytes at bits)
  (setf (aref bytes at) (ldb (byte 8 0) bits) (aref bytes (+ at 1)) (ldb (byte 8 8) bits)
        (aref bytes (+ at 2)) (ldb (byte 8 16) bits) (aref bytes (+ at 3)) (ldb (byte 8 24) bits)))

;;; The colour of a place: the grass of its country, rock where it is high, silt where it is low, dirt by the road

(defparameter *grass*
  '(:pine "#2c3a30" :meadow "#4a5238" :autumn "#5a4e34" :birch "#444c38" :rocky "#484846"
    :steppe "#5e583e" :marsh "#363c2e")
  "The colour of the grass by the vegetation of the country.")
(defparameter *rock-rgb* "#58545e")
(defparameter *snow-rgb* "#dfe4ee")
(defparameter *silt-rgb* "#8a7a5a")
(defparameter *road-rgb* "#6a5640")
(defparameter *wild-rgb* "#3e5a32" "The grass far from all roads.")

(defun veg-color (d)
  "Return the grass colour at D in summer: a blend of the vegetation kinds by their weights."
  (let ((r 0.0) (g 0.0) (b 0.0))
    (loop for (k wt) on (layer-weights :veg d) by #'cddr
          do (let ((c (hex (getf *grass* k))))
               (incf r (* wt (color-r c))) (incf g (* wt (color-g c))) (incf b (* wt (color-b c)))))
    (color r g b 1.0)))

(defun ground-look (lat h y ny meadow dry)
  "Return the look of a point of land as two values: the mixes that take it away from plain grass, as a plist of
fractions (:rock :snow :silt :road, by which the colour is mixed), and the weights of the seven textures of
*TERRAIN-TEXTURES*. LAT is the sideways offset from the road (999 far from roads), H the height above the road,
Y above the world level, NY the y of the normal; MEADOW and DRY are how much of each grows here."
  (let* ((road-c (if (and (< -6.4 lat 2.8) (< -.3 h 3)) (min (sstep 2.8 1.8 lat) (sstep -6.4 -5.4 lat)) 0.0))
         (rock-c (max (sstep 4 10 h) (sstep 14 26 y)))
         (snow-c (if (> y 32) (sstep 32 40 y) 0.0))
         (silt-c (if (< h -.2) (sstep -.2 -1 h) 0.0))
         (w (list 1.0 0.0 0.0 0.0 0.0 0.0 0.0)))
    (flet ((mix (k a) (when (> a 0) (setf w (mapcar (lambda (v) (* v (- 1 a))) w)) (incf (nth k w) a))))
      (mix 1 meadow) (mix 2 dry) (mix 3 silt-c) (mix 4 road-c)
      (mix 5 (* (max (sstep .3 .6 (- 1 ny)) (sstep 4 10 h)) (- 1 road-c)))
      (mix 6 snow-c))
    (values (list :rock rock-c :snow snow-c :silt silt-c :road road-c) w)))

(defun bake-region (data rx rz)
  "Bake the region (RX RZ) into DATA, and its water into a file. Return the number of cells of water."
  (let* ((n *terrain-points*) (cell *terrain-step*) (m (1+ n)) (x0 (* rx (region-side))) (z0 (* rz (region-side)))
         (ys (make-array (list m m) :element-type 'single-float)) (wls (make-array (list m m) :initial-element nil))
         (info (make-array (list m m) :initial-element nil))          ; (lat h meadow dry d) by a road, NIL in the wild
         (heights (make-array (* 4 n n) :element-type '(unsigned-byte 8)))
         (control (make-array (* 4 n n) :element-type '(unsigned-byte 8)))
         (colour (make-array (* 4 n n) :element-type '(unsigned-byte 8)))
         (water 0))
    (dotimes (j m)
      (dotimes (i m)
        (let ((x (+ x0 (* i cell))) (z (+ z0 (* j cell))))
          (multiple-value-bind (y near) (formula-land-height x z)
            (when near
              (destructuring-bind (d lat &rest more) near
                (declare (ignore more))
                (let* ((row (row-near d)) (ry (road-y d)) (h (- y ry)) (wet (max (row-lake row) (row-river row)))
                       (wl (and (> wet .01) (< (abs lat) *road-near*) (+ ry *water-y*))))
                  ;; far away the terrain is drawn with cells wider than the road, and a face between tall neighbours
                  ;; would cover it: by the roadbed the land is kept a hand below the road
                  (unless (deck-at d)
                    (let ((k (- 1 (sstep 10 18 (abs (+ lat 2.25))))))
                      (when (and (> k 0) (> y (- ry .25))) (setf y (- y (* k (- y (- ry .25))))))))
                  ;; the bed under water is sunk by almost a metre: in the shallows it lay level with the water
                  (when (and wl (< y (+ wl .15)))
                    (setf y (- y (* 1.5 (min 1.0 (/ (- (+ wl .15) y) .25))))))
                  (setf (aref wls i j) wl
                        (aref info i j) (list lat h (row-meadow row) (row-dry row) (row-d row))))))
            (setf (aref ys i j) (float y 1f0))))))
    (dotimes (j n)
      (dotimes (i n)
        (let* ((at (* 4 (+ i (* j n)))) (y (aref ys i j))
               (dx (/ (- (aref ys (1+ i) j) y) cell)) (dz (/ (- (aref ys i (1+ j)) y) cell))
               (ny (/ 1.0 (sqrt (+ 1.0 (* dx dx) (* dz dz))))) (near (aref info i j)))
          (put-f32 heights at y)
          (multiple-value-bind (mixes w)
              (if near
                  (destructuring-bind (lat h meadow dry d) near (declare (ignore d)) (ground-look lat h y ny meadow dry))
                  (ground-look 999.0 0.0 (* .45 y) ny 0.0 0.0))
            (let* ((grass (if near (veg-color (fifth near)) (hex *wild-rgb*))) (c grass))
              (flet ((toward (rgb key) (let ((a (getf mixes key))) (when (> a 0) (setf c (mix-color c (hex rgb) a))))))
                (toward *rock-rgb* :rock) (toward *snow-rgb* :snow) (toward *silt-rgb* :silt) (toward *road-rgb* :road))
              (flet ((byte-of (v) (max 0 (min 255 (round (* 255 v))))))
                (setf (aref colour at) (byte-of (color-r c)) (aref colour (+ at 1)) (byte-of (color-g c)) (aref colour (+ at 2)) (byte-of (color-b c))
                      (aref colour (+ at 3)) (byte-of (* (- 1 (getf mixes :rock)) (- 1 (getf mixes :snow)) (- 1 (getf mixes :silt)) (- 1 (getf mixes :road)))))))
            ;; the two heaviest textures and how they blend
            (let* ((order (sort (loop for v in w for k from 0 collect (cons v k)) #'> :key #'car))
                   (a (first order)) (b (second order))
                   (blend (if (> (car b) .004) (round (* 255 (/ (car b) (+ (car a) (car b))))) 0)))
              (put-u32 control at (logior (ash (cdr a) 27) (ash (cdr b) 22) (ash blend 14))))))))
    ;; the water: a level for every vertex, shared by the cells around it, so that they meet without gaps
    (let ((wst (gdext:make-object "SurfaceTool")))
      (object-call wst "begin" 3)
      (flet ((level (i j)
               (or (aref wls i j)
                   (let ((low nil))
                     (loop for di from -1 to 1
                           do (loop for dj from -1 to 1
                                    do (let ((a (+ i di)) (b (+ j dj)))
                                         (when (and (<= 0 a n) (<= 0 b n) (aref wls a b) (or (null low) (< (aref wls a b) low)))
                                           (setf low (aref wls a b))))))
                     low))))
        (dotimes (j n)
          (dotimes (i n)
            (when (or (aref wls i j) (aref wls (1+ i) j) (aref wls i (1+ j)) (aref wls (1+ i) (1+ j)))
              (let ((l00 (level i j)) (l10 (level (1+ i) j)) (l11 (level (1+ i) (1+ j))) (l01 (level i (1+ j))))
                (when (and l00 l10 l11 l01
                           (< (min (aref ys i j) (aref ys (1+ i) j) (aref ys i (1+ j)) (aref ys (1+ i) (1+ j))) (max l00 l10 l11 l01)))
                  (incf water)
                  (object-call wst "add_triangle_fan"
                               (vector (v3 (* i cell) l00 (* j cell)) (v3 (* (1+ i) cell) l10 (* j cell))
                                       (v3 (* (1+ i) cell) l11 (* (1+ j) cell)) (v3 (* i cell) l01 (* (1+ j) cell))))))))))
      (when (plusp water)
        (object-call wst "generate_normals")
        (object-call (gdext:singleton "ResourceSaver") "save" (object-call wst "commit") (water-path rx rz))))
    (let ((region (new-object "Terrain3DRegion")))
      (flet ((image (format bytes) (let ((img (gdext:make-object "Image"))) (object-call img "set_data" n n nil format bytes) img)))
        (object-call region "set_region_size" n)
        (object-call region "set_vertex_spacing" *terrain-step*)
        (object-call region "set_location" (vector2i rx rz))
        (object-call region "set_height_map" (image 8 heights))           ; FORMAT_RF
        (object-call region "set_control_map" (image 8 control))
        (object-call region "set_color_map" (image 5 colour)))             ; FORMAT_RGBA8
      (object-call data "add_region" region nil))
    water))

(defun bake-roads ()
  "Write the roads of the map to *ROADS-PATH*: for every road its ends, where it leads, and its points two metres
apart - x, z, heading and height. Numbers are 32-bit integers: millimetres, and millionths of a radian."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t)))
    (flet ((put (v) (let ((n (ldb (byte 32 0) (round v)))) (dotimes (k 4) (vector-push-extend (ldb (byte 8 (* 8 k)) n) out)))))
      (put 1) (put (length *edges*))
      (put (* 1000 (first *hub*))) (put (* 1000 (second *hub*)))
      (dolist (e *edges*)
        (let ((n (length (edge-xs e))))
          (put (cond ((eq e (ring)) 0) ((eq (edge-next e) :hub) 1) (t 2)))        ; the ring, an exit to the Crossroads, an out from it
          (put (* 1000 (edge-d0 e))) (put (* 1000 (edge-d1 e)))
          (put (* 1000 (if (eq (edge-next e) :hub) (car (rassoc e *exits*)) (edge-next e))))   ; an exit: where it leaves the ring
          (put n)
          (dotimes (i n)
            (put (* 1000 (aref (edge-xs e) i))) (put (* 1000 (aref (edge-zs e) i))) (put (* 1000000 (aref (edge-ths e) i)))
            (put (* 1000 (formula-road-y (+ (edge-d0 e) (* i *map-step*)))))))))
    (let ((path (object-call (gdext:singleton "ProjectSettings") "globalize_path" *roads-path*)))
      (with-open-file (file path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
        (write-sequence out file))
      (format t "~&BAKE roads: ~d roads, ~d bytes, seam of the ring ~,2f m~%" (length *edges*) (length out)
              (- (formula-road-y (edge-d1 (ring))) (formula-road-y (edge-d0 (ring))))))))

(defun ribbon-mesh (e a b)
  "Return the mesh of the ribbon of road E from path coordinate A to B: the strips across and, in a town, a curb and
sidewalks, in rows of four metres. Its points are counted from the road point at A, where the game puts it; it lies
at road height, the land under it is slightly lower; over the chasm it runs alone as the bridge deck. The mesh has
two surfaces: the roadway, and its soft edges - a strip on either side that fades into the roadside, or the road
looks cut out of card."
  (declare (ignore e))
  (let* ((n (max 1 (ceiling (- b a) 4.0))) (step (/ (- b a) n)) (base (road-pos a))
         (st (gdext:make-object "SurfaceTool")) (soft (gdext:make-object "SurfaceTool")) (uv (vector2 0 0))
         (points (make-hash-table :test 'equal)) (tints (make-hash-table :test 'equal)))
    (flet ((pt (s j) (or (gethash (cons s j) points)
                         (setf (gethash (cons s j) points)
                               (destructuring-bind (x y z) (road-pos (+ a (* j step)) s *road-lift*)
                                 (v3 (- x (first base)) (- y (second base)) (- z (third base)))))))
           (tint (rgb &optional (alpha 1.0))
             (or (gethash (cons rgb alpha) tints) (setf (gethash (cons rgb alpha) tints) (hex rgb alpha)))))
      (object-call st "begin" 3) (object-call soft "begin" 3)
      (dotimes (j n)
        (let ((town (town-at (+ a (* (+ j .5) step)))))
          (when town                                                            ; in a town - curb and sidewalks
            (loop for (s0 s1 rgb) in '((-9.4 -6.6 "#67655f") (-6.6 -6.3 "#9a988f") (2.7 3.0 "#9a988f") (3.0 5.8 "#67655f"))
                  do (let ((c (tint rgb)))
                       (object-call st "add_triangle_fan" (vector (pt s0 (1+ j)) (pt s1 (1+ j)) (pt s1 j) (pt s0 j))
                                    (vector uv uv uv uv) (vector c c c c)))))
          (loop for (s0 s1 rgb) in *road-strips*
                do (let ((c (tint (if (eq rgb :line) (if (evenp (round (+ a (* j step)) 4.0)) "#b9b6ac" "#55524f") rgb))))
                     (object-call st "add_triangle_fan" (vector (pt s0 (1+ j)) (pt s1 (1+ j)) (pt s1 j) (pt s0 j))
                                  (vector uv uv uv uv) (vector c c c c))))
          ;; the soft edges: from clear outside to the colour of the outermost strip
          (loop for (s0 s1 rgb) in (if town
                                       '((-10.9 -9.4 "#67655f") (7.3 5.8 "#67655f"))
                                       '((-7.8 -6.3 "#3a393b") (4.2 2.7 "#3a393b")))
                do (let ((c0 (tint rgb 0.0)) (c1 (tint rgb 1.0)) (lo (min s0 s1)) (hi (max s0 s1)))
                     (let ((clo (if (< s0 s1) c0 c1)) (chi (if (< s0 s1) c1 c0)))
                       (object-call soft "add_triangle_fan" (vector (pt lo (1+ j)) (pt hi (1+ j)) (pt hi j) (pt lo j))
                                    (vector uv uv uv uv) (vector clo chi chi clo)))))))
      (object-call st "generate_normals") (object-call soft "generate_normals")
      (let ((mesh (object-call st "commit")))
        (object-call soft "commit" mesh)
        mesh))))

(defun bake-ribbons ()
  "Bake the ribbons of all the roads into *RIBBONS-DIR*: a mesh for every *ROAD-PIECE* metres (doroga.lisp)."
  (let ((saver (gdext:singleton "ResourceSaver")) (count 0) (t0 (get-internal-real-time)))
    (dolist (e *edges*)
      (loop for a from (edge-d0 e) below (- (edge-d1 e) .5) by *road-piece*
            do (let ((mesh (ribbon-mesh e a (min (edge-d1 e) (+ a *road-piece*)))))
                 (object-call saver "save" mesh (ribbon-path a) 32)              ; FLAG_COMPRESS
                 (incf count))))
    (format t "~&BAKE ribbons: ~d of ~d m on ~d roads, ~d s~%" count (round *road-piece*) (length *edges*)
            (round (- (get-internal-real-time) t0) internal-time-units-per-second))))


;;; Things: what stands still on the map, built once here and saved as a scene (things.lisp tells how the game
;;; loads it).

(defun bake-weld (root kept)
  "Draw the like meshes under ROOT in one go each: a MultiMesh for every mesh that stands there more than once in the
same paint, wherever under ROOT and however turned - their places are taken as they stand on the map. Only what is
seen from any distance is taken (a trifle that is hidden far away is judged from its own place, and must keep it),
and nothing that is among KEPT - the nodes that move - or under one of them. Return the number of nodes melted."
  (let ((lots '()) (gone 0))
    (labels ((walk (node)
               (loop for c across (node-get-children node)
                     do (cond ((member c kept :test #'eq))
                              ((and (string= (princ-to-string (object-call c "get_class")) "MeshInstance3D")
                                    (zerop (object-call c "get_child_count")) (object-call c "is_visible_in_tree")
                                    (present (object-get c "mesh")) (zerop (object-get c "visibility_range_end")))
                               (let* ((mesh (object-get c "mesh")) (over (present (object-get c "material_override")))
                                      (overrides (unless (object-call mesh "is_class" "PrimitiveMesh")
                                                   (loop for i below (object-call mesh "get_surface_count")
                                                         collect (present (object-call c "get_surface_override_material" i)))))
                                      (shadow (object-get c "cast_shadow"))
                                      (lot (find-if (lambda (l) (and (eq (first l) mesh) (eq (second l) over) (eql (fourth l) shadow)
                                                                     (= (length (third l)) (length overrides)) (every #'eq (third l) overrides)))
                                                    lots)))
                                 (unless lot (setf lot (list mesh over overrides shadow)) (push lot lots))
                                 (push (cons c (object-get c "global_transform")) (cdr (last lot 1)))))
                              (t (walk c))))))
      (walk root)
      ;; the MultiMeshes hang on a holder that stands as World does, so the places are the places on the map
      (let ((holder (gdext:make-object "Node3D")))
        (node-add-child root holder)
        (object-set holder "global_transform" (object-get *world* "global_transform"))
        (dolist (lot lots)
          (destructuring-bind (mesh over overrides shadow &rest places) lot
            (when (and (rest places) (notany #'identity overrides))
              (let ((mm (gdext:make-object "MultiMesh")) (mmi (gdext:make-object "MultiMeshInstance3D")))
                (object-set mm "transform_format" 1)
                (object-set mm "mesh" mesh)
                (object-set mm "instance_count" (length places))
                ;; the places go in as numbers, twelve for each: baking runs without a video card, and a place set
                ;; one by one is kept only there - the saved MultiMesh came out empty
                (let ((buffer (make-array (* 12 (length places)) :element-type 'single-float)) (k -1))
                  (loop for (nil . at) in places
                        do (let* ((b (godot:transform3d-basis at)) (o (godot:transform3d-origin at))
                                  (bx (godot:basis-x b)) (by (godot:basis-y b)) (bz (godot:basis-z b)))
                             (dolist (v (list (vector3-x bx) (vector3-x by) (vector3-x bz) (vector3-x o)
                                              (vector3-y bx) (vector3-y by) (vector3-y bz) (vector3-y o)
                                              (vector3-z bx) (vector3-z by) (vector3-z bz) (vector3-z o)))
                               (setf (aref buffer (incf k)) (float v 1.0)))))
                  (object-set mm "buffer" buffer)
                  (object-call mmi "set_meta" "buffer" buffer))                 ; and beside it, for LOAD-THING to hand over again
                (object-set mmi "multimesh" mm)
                (when over (object-set mmi "material_override" over))
                (object-set mmi "cast_shadow" shadow)
                (node-add-child holder mmi)
                (loop for (c) in places
                      do (object-call (object-call c "get_parent") "remove_child" c) (godot:node-queue-free c) (incf gone)))))))
      gone)))

(defvar *material-keys* nil "While things are baked: a table from a material of the game to the key MATERIAL files it under.")
(defvar *mesh-notes* nil "While things are baked: a table from a mesh to the keys of the materials its surfaces wore, taken off it once.")

(defun material-key (m)
  (unless *material-keys*
    (setf *material-keys* (make-hash-table :test 'eq))
    (maphash (lambda (key mat) (setf (gethash mat *material-keys*) key)) *materials*))
  (and m (or (gethash m *material-keys*)
             ;; a material made since the table was: look once more
             (progn (maphash (lambda (key mat) (setf (gethash mat *material-keys*) key)) *materials*)
                    (gethash m *material-keys*)))))

(defun strip-materials (root)
  "Take the game's materials off everything under ROOT and return the note of what went where, each entry as
(path override surfaces overrides) with the keys of the materials. The nodes get plain names first, so that the
paths hold in the saved scene."
  (let ((notes '()) (n 0))
    (unless *mesh-notes* (setf *mesh-notes* (make-hash-table :test 'eq)))
    (labels ((walk (node path)
               (loop for c across (node-get-children node)
                     do (let* ((name (format nil "n~d" (incf n))) (here (if path (format nil "~a/~a" path name) name)))
                          (object-set c "name" name)
                          (object-call c "set_meta" "path" here)
                          (let* ((mmi (object-call c "is_class" "MultiMeshInstance3D"))
                                 (mesh (cond (mmi (present (object-get (object-get c "multimesh") "mesh")))
                                             ((object-call c "is_class" "MeshInstance3D") (present (object-get c "mesh"))))))
                            (when mesh
                              (let* ((over (material-key (present (object-get c "material_override"))))
                                     (primitive (object-call mesh "is_class" "PrimitiveMesh"))
                                     (count (object-call mesh "get_surface_count"))
                                     (surfaces (unless primitive
                                                 (multiple-value-bind (known found) (gethash mesh *mesh-notes*)
                                                   (if found
                                                       known
                                                       (setf (gethash mesh *mesh-notes*)
                                                             (let ((keys (loop for i below count
                                                                               collect (material-key (present (object-call mesh "surface_get_material" i))))))
                                                               (loop for k in keys for i from 0
                                                                     do (when k (object-set mesh (format nil "surface_~d/material" i) nil)))   ; a method will not take NIL for a material
                                                               keys))))))
                                     (overrides (unless (or mmi primitive)
                                                  (loop for i below count
                                                        collect (let ((k (material-key (present (object-call c "get_surface_override_material" i)))))
                                                                  (when k (object-set c (format nil "surface_material_override/~d" i) nil))
                                                                  k)))))
                                (when over (object-set c "material_override" nil))
                                (when (or mmi over (some #'identity surfaces) (some #'identity overrides))
                                  (push (list here over (and (some #'identity surfaces) surfaces) (and (some #'identity overrides) overrides))
                                        notes)))))
                          (walk c here)))))
      (walk root nil))
    (nreverse notes)))

(defun bake-thing (key builder)
  "Build a thing with BUILDER - a function that puts it under World and returns its root node - and save it as the
thing KEY (things.lisp). Return the size of the file in nodes, or NIL when BUILDER built nothing."
  (unless (and (< (abs (vector3-x (object-get *world* "position"))) 1e-3) (< (abs (vector3-z (object-get *world* "position"))) 1e-3))
    (error "Things are baked with the origin of the scene at the origin of the map"))
  (let* ((*solid-log* (list nil)) (*moves* '())
         (root (let ((*weld* nil)) (with-solids (funcall builder)))))    ; BAKE-WELD does all the melting, with places that get saved
    (when root
      (let* ((moves *moves*) (melted (bake-weld root (mapcar #'first moves)))
             (mats (strip-materials root)) (count 0)
             (scene (gdext:make-object "PackedScene")))
        (labels ((own (node) (loop for c across (node-get-children node) do (incf count) (object-set c "owner" root) (own c))))
          (own root))
        (with-standard-io-syntax
          (object-call root "set_meta" "mats" (prin1-to-string mats))
          (object-call root "set_meta" "solids" (prin1-to-string (cdr *solid-log*)))
          (object-call root "set_meta" "moves"
                       (prin1-to-string (loop for (node . what) in moves
                                              collect (cons (princ-to-string (object-call node "get_meta" "path")) what)))))
        (unless (and (eql 0 (object-call scene "pack" root))
                     (eql 0 (object-call (gdext:singleton "ResourceSaver") "save" scene (thing-path key) 32)))
          (error "The thing ~a could not be saved" key))
        (drop-solids root)
        (object-call (object-call root "get_parent") "remove_child" root)
        (godot:node-queue-free root)
        (values count melted (length moves))))))

(defun bake-things ()
  "Bake everything that stands still on the map into *THINGS-DIR*: for now the scenes by the road (places.lisp)."
  (let ((seen (make-hash-table :test 'equal)) (t0 (get-internal-real-time)) (n 0) (nodes 0) (melted 0))
    (set-season :summer)
    (dolist (s *scenes*)
      (let ((key (scene-key s)))
        (when (gethash key seen) (error "Two scenes would be baked as ~a" key))
        (setf (gethash key seen) t)
        (multiple-value-bind (count gone moves) (bake-thing key (scene-build s))
          (when count
            (incf n) (incf nodes count) (incf melted gone)
            (format t "~&BAKE thing ~a: ~d nodes (~d melted into batches), ~d moving~%" key count gone moves)))))
    (format t "~&BAKE things: ~d scenes, ~d nodes, ~d melted, ~d s~%" n nodes melted
            (round (- (get-internal-real-time) t0) internal-time-units-per-second))))

(defun bake-regions ()
  "Return the regions to bake, each as (rx rz): those within *BAKE-REACH* of a road, or the ones KIOSK_BAKE_ONLY names."
  (let ((only (ext:getenv "KIOSK_BAKE_ONLY")) (side (region-side)) (seen (make-hash-table)) (out '()))
    (if only
        (destructuring-bind (a b c d) (let ((*read-eval* nil)) (read-from-string (format nil "(~a)" only)))
          (loop for rx from a to c do (loop for rz from b to d do (push (list rx rz) out))))
        (dolist (e *edges*)
          (loop for i from 0 below (length (edge-xs e)) by 16
                do (let ((x (aref (edge-xs e) i)) (z (aref (edge-zs e) i)))
                     (loop for rx from (floor (- x *bake-reach*) side) to (floor (+ x *bake-reach*) side)
                           do (loop for rz from (floor (- z *bake-reach*) side) to (floor (+ z *bake-reach*) side)
                                    do (unless (gethash (region-key rx rz) seen)
                                         (setf (gethash (region-key rx rz) seen) t)
                                         (push (list rx rz) out))))))))
    (sort out (lambda (a b) (or (< (first a) (first b)) (and (= (first a) (first b)) (< (second a) (second b))))))))

(defun save-editor-resources ()
  "Save what the Godot editor needs to show the map as the game does and to paint on it: the terrain's textures
(assets.res) and its material (material.res). The scene that uses them, terrain.tscn, is written by tools/bake-map.sh."
  (let ((saver (gdext:singleton "ResourceSaver")))
    (terrain-param "world_shift" (v3 0.0 0.0 0.0)) (terrain-param "season_amount" 0.0)
    (object-call saver "save" (object-get *terrain* "assets") "res://terrain/assets.res")
    (object-call saver "save" (object-get *terrain* "material") "res://terrain/material.res")
    (format t "~&BAKE editor resources: assets.res with ~d textures, material.res~%"
            (object-call (object-get *terrain* "assets") "get_texture_count"))))

(defun dump-terrain-shader (path)
  "Write Terrain3D's own shader, as it makes it for a terrain like ours, to the file PATH: tools/terrain-shader.py
makes the game's shader of it."
  (let* ((terrain (new-object "Terrain3D")) (material (progn (node-add-child *root* terrain) (object-get terrain "material"))))   ; it has a material once it is in the tree
    (object-call material "set_auto_shader_enabled" nil)
    (object-call material "set_world_background" 1)
    (object-call material "set_macro_variation_enabled" t)
    (object-call material "set_shader_override_enabled" t)
    (let* ((shader (object-call material "get_shader_override")) (code (and shader (object-get shader "code"))))
      (when (and code (plusp (length code)))
        (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8) (write-string code out)))
      (format t "~&BAKE shader: ~a characters~%" (and code (length code))))
    (godot:node-queue-free terrain)))

(defun bake-map (root)
  "Bake the whole map (see the head of this file). ROOT is the scene's root: the terrain needs a place in the tree."
  (let* ((*season* :summer) (regions (bake-regions)) (t0 (get-internal-real-time)) (wet 0))
    (let ((path (ext:getenv "KIOSK_BAKE_SHADER"))) (when path (dump-terrain-shader path)))
    (make-terrain root nil)
    (when (ext:getenv "KIOSK_BAKE_EDITOR")                              ; only what the editor needs, the map is left as it is
      (save-editor-resources)
      (return-from bake-map nil))
    (format t "~&BAKE: ~d regions~%" (length regions))
    (loop for (rx rz) in regions for k from 1
          do (let ((water (bake-region *terrain-data* rx rz)))
               (when (plusp water) (incf wet))
               (format t "~&BAKE ~d/~d: region ~d ~d, water cells ~d, ~d s~%" k (length regions) rx rz water
                       (round (- (get-internal-real-time) t0) internal-time-units-per-second))
               (clrhash *rows*) (finish-output)))
    (object-call *terrain* "set_save_16_bit" t)                          ; heights to the millimetre in half the bytes
    (object-call *terrain-data* "save_directory" "res://terrain/data")
    (bake-roads)
    (bake-ribbons)
    (save-editor-resources)
    (format t "~&BAKE done: ~d regions, ~d with water, ~d s~%" (length regions) wet
            (round (- (get-internal-real-time) t0) internal-time-units-per-second))))
