;;;; The land of the base world is a Terrain3D terrain (project/addons/terrain_3d). The map is baked into files:
;;;; project/terrain/data holds its regions - heights, which texture lies where, the colour of each place - and
;;;; project/terrain/water a mesh of water for every region that has any. It is drawn, sculpted and painted in the
;;;; Godot editor (project/terrain/terrain.tscn) and baked anew from the formulas by tools/bake-map.sh.
;;;; A region is 256 points 4 m apart, that is 1024 m.
;;;; Terrain3D stands where the map is and cannot be moved, while the game keeps the scene's origin near the wagon
;;;; (world.lisp). So the terrain's shader (tools/terrain-shader.py) draws the map shifted: it is told where the
;;;; origin is (world_shift), and the origin is kept at multiples of 1024 m, on the terrain's own grid.

(in-package #:kiosk-trip)

(defparameter *terrain-step* 4.0 "The spacing of the terrain's points, in metres.")
(defparameter *terrain-points* 256 "The number of points along the side of a region.")
(defparameter *terrain-ground* 14.0 "The height of the flat land beyond the baked regions.")
(defparameter *terrain-textures* '(:grass :meadow :dry :sand :road :rock :snow)
  "The textures of the land in the order of their numbers in the terrain's control map.")
(defvar *terrain* nil "The Terrain3D node.")
(defvar *terrain-data* nil "Its Terrain3DData: the heights are asked of it.")
(defvar *terrain-regions* (make-hash-table) "The regions that are baked: a table keyed by REGION-KEY.")
(defvar *terrain-keep* (make-hash-table :test 'equal) "What the terrain is made of and must not be freed: textures, the shader.")

(defun region-side () (* *terrain-step* *terrain-points*))
(defun region-key (rx rz) (+ (* 4096 (+ rx 2048)) (+ rz 2048)))

(defun new-object (class) "Make an object of CLASS, which may belong to another extension." (object-call (gdext:singleton "ClassDB") "instantiate" class))

(defun terrain-texture (name)
  "Return a Terrain3D texture asset made of the land picture NAME of the current style, or NIL without the picture.
The pictures are nearly grey, the colour comes from the map; the shader doubles them (texture_gain)."
  (let ((path (texture-path (or *style* :pixel) name)))
    (when (picture-here-p path)
      (let ((img (gdext:make-object "Image")) (flat (gdext:make-object "Image"))
            (albedo (gdext:make-object "ImageTexture")) (normal (gdext:make-object "ImageTexture"))
            (asset (new-object "Terrain3DTextureAsset")))
        (read-picture img path)
        (object-call img "convert" 5)                                    ; FORMAT_RGBA8
        (object-call img "generate_mipmaps")
        (object-call flat "set_data" 4 4 nil 5                           ; a flat normal, fully rough
                     (let ((b (make-array 64 :element-type '(unsigned-byte 8))))
                       (dotimes (i 16 b) (setf (aref b (* 4 i)) 128 (aref b (+ 1 (* 4 i))) 128 (aref b (+ 2 (* 4 i))) 255 (aref b (+ 3 (* 4 i))) 255))))
        (object-call flat "resize" (object-call img "get_width") (object-call img "get_height") 0)
        (object-call flat "generate_mipmaps")
        (object-call albedo "set_image" img) (object-call normal "set_image" flat)
        (object-call asset "set_asset_name" (string-downcase (symbol-name name)))
        (object-call asset "set_albedo_texture" albedo) (object-call asset "set_normal_texture" normal)
        (object-call asset "set_uv_scale" (/ 1.0 *ground-tile*))
        (gdext:setf-reference (gethash (list :albedo name) *terrain-keep*) albedo)
        (gdext:setf-reference (gethash (list :normal name) *terrain-keep*) normal)
        (gdext:setf-reference (gethash (list :asset name) *terrain-keep*) asset)
        asset))))

(defun make-terrain (root &optional (directory "res://terrain/data"))
  "Put the terrain under ROOT, with its textures and its shader, and load the baked map from DIRECTORY (NIL: none)."
  (when *terrain* (godot:node-queue-free *terrain*))
  (clrhash *terrain-regions*)
  (let ((terrain (new-object "Terrain3D")))
    (setf *terrain* terrain)
    (object-call terrain "set_collision_mode" 0)                         ; nobody collides with it: the game asks for heights itself
    (when *baking* (object-call terrain "set_free_editor_textures" nil)) ; the textures are saved for the editor: keep them
    (node-add-child root terrain)
    (object-call terrain "change_region_size" *terrain-points*)
    (object-call terrain "set_vertex_spacing" *terrain-step*)
    (object-call terrain "set_mesh_lods" 7)                              ; 4 m near, 256 m far: the origin's 1024 m suits them all
    (let ((material (object-get terrain "material")) (assets (object-get terrain "assets"))
          (shader (and (object-call (gdext:singleton "ResourceLoader") "exists" "res://terrain/terrain.gdshader")
                       (object-call (gdext:singleton "ResourceLoader") "load" "res://terrain/terrain.gdshader"))))
      (loop for name in *terrain-textures* for id from 0
            do (let ((asset (terrain-texture name))) (when asset (object-call assets "set_texture_asset" id asset))))
      (object-call material "set_world_background" 1)                    ; FLAT beyond the regions
      (object-call material "set_macro_variation_enabled" t)             ; large patches a little lighter and darker: the tiles do not show
      (object-call material "set_texture_filtering" (if (eq *style* :pixel) 2 0))   ; NEAREST_ANISOTROPIC for pixel pictures: square pixels near, no shimmer far
      (when shader
        (gdext:setf-reference (gethash :shader *terrain-keep*) shader)
        (object-call material "set_shader_override" shader)
        (object-call material "set_shader_override_enabled" t))
      (object-call material "set_shader_param" "ground_level" *terrain-ground*)
      (object-call material "set_shader_param" "macro_variation1" (color .80 .82 .78 1.0))
      (object-call material "set_shader_param" "macro_variation2" (color .90 .88 .86 1.0)))
    (setf *terrain-data* (object-get terrain "data"))
    ;; the regions are loaded one by one: Terrain3D's own loading of a whole directory divides by zero in the browser build
    (when directory
      (loop for file across (dir-access-get-files-at directory)
            when (and (> (length file) 4) (string= ".res" file :start2 (- (length file) 4)))
              do (let ((region (object-call (gdext:singleton "ResourceLoader") "load" (format nil "~a/~a" directory file))))
                   (when region (object-call *terrain-data* "add_region" region nil))))
      (object-call *terrain-data* "update_maps"))
    (loop for at across (object-call *terrain-data* "get_region_locations")
          do (setf (gethash (region-key (vector2i-x at) (vector2i-y at)) *terrain-regions*) t))
    (terrain-season) (terrain-shift)
    terrain))

(defvar *terrain-shown* t "False while the terrain is hidden: behind a door the base world is not there.")

(defun terrain-show (on)
  "Show the terrain or hide it. Neither its VISIBLE nor its render layers hide what Terrain3D draws, so a hidden
terrain is drawn a hundred kilometres below: out of everybody's sight."
  (unless (eq (and on t) *terrain-shown*)
    (setf *terrain-shown* (and on t))
    (terrain-shift)))

(defun terrain-param (name value)
  (when *terrain* (object-call (object-get *terrain* "material") "set_shader_param" name value)))

(defun terrain-shift ()
  "Tell the terrain where the scene's origin is on the map."
  (destructuring-bind (ox oy oz) *origin* (terrain-param "world_shift" (v3 ox (if *terrain-shown* oy (+ oy 100000.0)) oz))))

(defun terrain-season ()
  "Tint the grass of the terrain for the season: as SEASON-GRASS does a colour."
  (destructuring-bind (rgb amount) (case *season* (:spring '("#6aa03a" .25)) (:autumn '("#9a6a2a" .45)) (:winter '("#e8eef4" .85)) (t '("#ffffff" 0.0)))
    (terrain-param "season_color" (hex rgb)) (terrain-param "season_amount" amount)))

(defun terrain-height (x z)
  "Return the height of the baked land at the map point (X, Z), or NIL where no region is baked."
  (let ((side (region-side)))
    (when (and *terrain-data* (gethash (region-key (floor x side) (floor z side)) *terrain-regions*))
      (float (object-call *terrain-data* "get_height" (v3 x 0.0 z)) 1.0))))

;;; Water: a mesh for every region that has any, baked with the land (bake.lisp)

(defun water-path (rx rz) (format nil "res://terrain/water/water_~d_~d.res" rx rz))

(defun load-water (parent)
  "Put under PARENT, which lies by the map, the water of every baked region."
  (let ((loader (gdext:singleton "ResourceLoader")) (side (region-side)))
    (loop for at across (object-call *terrain-data* "get_region_locations")
          do (let* ((rx (vector2i-x at)) (rz (vector2i-y at)) (path (water-path rx rz)))
               (when (object-call loader "exists" path)
                 (let ((mesh (object-call loader "load" path)) (w (gdext:make-object "MeshInstance3D")))
                   (object-set w "mesh" mesh)
                   (object-set w "material_override" (water-material))
                   (object-set w "cast_shadow" 0)
                   (node-add-child parent w)
                   (godot:node-3d-set-position w (v3 (* rx side) 0.0 (* rz side)))))))))

;;; The roads: their lines and heights are data too, baked with the land (bake.lisp, BAKE-ROADS)

(defparameter *roads-path* "res://terrain/roads.bin" "The file of the baked roads.")

(defun load-roads ()
  "Read the roads of the map from *ROADS-PATH* into *EDGES*, *EXITS*, *OUTS* and *HUB*. Return NIL without the file."
  (when (file-here-p *roads-path*)
    (let ((bytes (file-bytes *roads-path*)) (at 0))
      (flet ((int () (let ((n (logior (aref bytes at) (ash (aref bytes (+ at 1)) 8) (ash (aref bytes (+ at 2)) 16) (ash (aref bytes (+ at 3)) 24))))
                       (incf at 4)
                       (if (logbitp 31 n) (- n 4294967296) n))))
        (let ((version (int)) (count (int)))
          (unless (= version 1) (error "~a is of version ~d, this game reads version 1" *roads-path* version))
          (setf *hub* (list (/ (int) 1000d0) (/ (int) 1000d0)) *exits* '() *outs* '() *edges* '())
          (dotimes (k count)
            (let* ((kind (int)) (d0 (/ (int) 1000.0)) (d1 (/ (int) 1000.0)) (where (/ (int) 1000.0)) (n (int))
                   (xs (make-array n :element-type 'double-float)) (zs (make-array n :element-type 'double-float))
                   (ths (make-array n :element-type 'double-float)) (ys (make-array n :element-type 'single-float)))
              (dotimes (i n)
                (setf (aref xs i) (/ (int) 1000d0) (aref zs i) (/ (int) 1000d0) (aref ths i) (/ (int) 1000000d0)
                      (aref ys i) (/ (int) 1000.0)))
              (let ((e (make-edge :d0 d0 :d1 d1 :xs xs :zs zs :ths ths :ys ys :next (if (= kind 1) :hub where))))
                (push e *edges*)
                (case kind (1 (push (cons where e) *exits*)) (2 (push e *outs*))))))
          (setf *edges* (nreverse *edges*) *exits* (sort *exits* #'< :key #'car) *outs* (nreverse *outs*))
          t)))))

(defun baked-road-y (d)
  "Return the height of the baked road at path coordinate D."
  (let* ((e (edge-of d)) (ys (edge-ys e)) (n (length ys))
         (d (if (eq e (ring)) (wrap-d d) d))
         (u (/ (- d (edge-d0 e)) *map-step*)) (i (max 0 (min (- n 2) (floor u)))) (f (max 0.0 (min 1.0 (- u i)))))
    (+ (aref ys i) (* f (- (aref ys (1+ i)) (aref ys i))))))
