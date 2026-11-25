;;;; The world outside the window. Everything scenic is under the World node: a thing at path coordinate D
;;;; stands in its coordinates at the point (ROAD-X D + sideways offset, ROAD-Y D + height, −D). The world
;;;; stands still and the Rig moves — the wagon with the camera: it stands on a road point, turns to the
;;;; course and tilts with the slope. This way the shadows do not shimmer: Godot snaps the shadow map to a
;;;; grid relative to the camera, and if the world floated under a fixed camera, every shadow edge
;;;; would shift by fractions of a texel each frame. To keep the numbers from growing, every
;;;; *REBASE* m the origin is moved towards the wagon (World is shifted). The land is the baked map, drawn by
;;;; Terrain3D (zemlya.lisp), and the water lies in meshes baked with it. The roadside things (trees, lanterns,
;;;; rocks…) and the bridges are built in a window around the wagon: one is left behind — we build a new one ahead.

(in-package #:kiosk-trip)

(defvar *speed* 7.0 "The truck speed in m/s. It is set through HALT, SLOW-TO and SET-SPEED only.")

(defun set-speed (v) "Make the truck go at V m/s: the autopilot's word, or the driver's speed told to a guest." (setf *speed* v))
(defun halt () "Stop the truck at once." (setf *speed* 0.0))
(defun slow-to (limit) "Do not let the truck go faster than LIMIT m/s right now." (setf *speed* (min *speed* limit)))
(defparameter *smooth-ground* t "True for smooth land lighting: flat faces far away are smaller than a pixel and shimmer.")
(defparameter *prop-step* 2.4 "The spacing of the roadside things, in metres.")
(defparameter *ahead* 170.0 "The distance ahead up to which we build.")
(defparameter *behind* 60.0 "The distance behind up to which we keep things.")

;; no fairy-tale mushrooms and pumpkins; fewer things by the road - poles and bus stops replace them
(defparameter *veg-props*
  '(:pine (:pine :pine :pine :rock :snag)
        :meadow (:bush :hay :fence :round :flowers)
        :autumn (:autumn :autumn :snag :hay :bush)
        :birch (:birch :birch :birch :round :rock)
        :rocky (:rock :rock :cairn :pine)
        :steppe (:bush :bush :hay :rock :fence)
        :marsh (:reeds :reeds :snag :snag :bush)))
(defparameter *small-far* 70.0 "The distance in metres beyond which small things fade out.
Flowers, mushrooms, reeds and lanterns are smaller than a pixel far away and flicker.")
(defparameter *small-fade* 1 "The fade mode of small things: 1 to fade smoothly (VISIBILITY_RANGE_FADE_SELF), 0 to vanish at once.")

(defvar *world* nil "The Node3D holding everything that floats.")
(defvar *turn* nil "The Node3D above World and the sun: the turn to the road course.")
(defvar *rig* nil "The Node3D of the wagon and the camera: the tilt with the road slope and the sway.")
(defvar *slope* 0.0 "The slope of the road under the wagon, in radians.")
(defvar *course* 0.0 "The course of the road under the wagon, in radians.")
(defvar *origin* '(0.0 0.0 0.0) "The scene origin in World coordinates (x y z).")
(defvar *rig-at* (list 0.0 0.0 0.0) "The position of the Rig in the scene (x y z).")
(defparameter *rebase* 700.0 "The number of metres from the origin after which it is moved to the nearest point of
the terrain's 1024 m grid. Far from the origin the engine runs out of precision, and surfaces lying a millimetre
apart flicker: so the scene never reaches beyond a kilometre from it.")
(defvar *dist* 0.0 "The distance travelled, in metres.")
(defvar *props* '() "The list of (node . d), in ascending order of d.")
(defvar *bridges* '())

;;; The land is Terrain3D's (zemlya.lisp): the map is baked into files and drawn by the terrain around the camera.
;;; Here is only what the rest of the world asks of it.

(defvar *land* nil "The node of what lies by the map besides the terrain - the roads, the water: it shifts together with the origin.")
(defvar *chunks* '() "A leftover from the former strip land: empty.")

(defun settle-land ()
  "The wagon has been put somewhere else: tell the terrain where the scene's origin is, and lay the roads anew."
  (terrain-shift)
  (setf *roads-at* nil))

(defun clear-land () "The land is baked: there is nothing to clear." nil)

(defun pick-weighted (plist)
  (let ((x (random 1.0)))
    (loop for (k w) on plist by #'cddr do (decf x w) (when (<= x 0) (return-from pick-weighted k)))
    (first plist)))

(defun crown (g dims rgb x y z &rest keys)
  "Build the crown of a deciduous tree in G for the current season; in winter, bare branches."
  (let ((c (season-crown rgb)))
    (if c
        (apply #'shape g :rock dims c x y z keys)
        (dolist (a '(0 2.1 4.2))
          (shape g :box (list .06 (* .9 (first dims)) .06) "#4a3424" (* .3 (cos a)) (- y (* .2 (first dims))) (* .3 (sin a))
                 :rot (list (* .5 (sin a)) 0 (* .5 (cos a))))))))

(defun tree (g file rgb)
  "Stand in G the tree drawn in FILE, its needles or leaves in RGB: each turned its own way, a little taller or
shorter than the drawing. The trees are of their true height: a pine is twenty metres, not five."
  (let ((s (+ .85 (random .35))))
    (model g file 0.0 0.0 0.0 :paint (list rgb) :rot (list 0 (random 6.28) 0) :scale (list s s s))))

(defun leafy (g name rgb)
  "Stand in G a leafy tree of the kind NAME (\"birch\", \"lime\") with leaves of the season; in winter, bare."
  (let ((c (if (and (eq *season* :spring) (string= name "birch")) "#7ac050" (season-crown rgb))))   ; a birch does not blossom
    (tree g (format nil "tree-~a-~d~:[-bare~;~]" name (1+ (random 2)) c) (or c "#4a3424"))))

(defun build-prop (g kind side)
  "Build a thing of kind KIND in the group G (already in place).
SIDE is ±1, the side of the road. The colours of foliage and flowers follow the season."
  (case kind
    (:pine (if (zerop (random 7))                                   ; a bor is pines; now and then a spruce under them
               (tree g (format nil "tree-spruce-~d" (1+ (random 2))) (season-conifer "#1e3a24"))
               (tree g (format nil "tree-pine-~d" (1+ (random 3))) (season-conifer (elt '("#24482c" "#2a5232" "#2c4a2a") (random 3))))))
    (:round (leafy g "lime" "#3a6a2a"))
    (:autumn (leafy g (if (zerop (random 3)) "birch" "lime") (elt '("#d86a20" "#c84020" "#e0a030") (random 3))))
    (:birch (leafy g "birch" "#8ab040"))
    (:bush (crown g (list (+ .6 (random .4))) "#4a6a2a" 0 .35 0 :scale '(1.4 .7 1.2)))
    (:snag (model g "prop-snag"))
    (:lantern (shape g :box '(.12 2.2 .12) "#4a3626" 0 1.1 0) (shape g :box '(.6 .06 .06) "#4a3626" (* side -.25) 2.15 0)
     (shape g :box '(.2 .26 .2) "#ffd080" (* side -.5) 1.95 0 :emit "#ffb050" :energy 1.2))
    (:mush (model g "prop-mush"))
    (:rock (shape g :rock (list (+ .5 (random .6))) "#4a4648" 0 .3 0) (shape g :rock '(.3) "#3a5a3a" .5 .2 .3))
    (:cairn (model g "prop-cairn"))
    (:cliff (shape g :rock (list (+ 4 (random 3.0))) "#46424e" 0 2.5 0 :scale '(1 1.6 1)))
    (:flowers (loop repeat (if (eq *season* :autumn) 5 12)
                    for c = (nth (random 5) '("#ff70a0" "#ffe060" "#a080ff" "#ffffff" "#ff9040"))
                    for x = (- (random 3.0) 1.5) for z = (- (random 3.0) 1.5)
                    do (shape g :box '(.03 .3 .03) "#4a8a3a" x .15 z :shadow nil)
                       (shape g :ball '(.08 5 3) c x .32 z :shadow nil)))
    (:drift (shape g :rock (list (+ .5 (random .5))) "#f0f4f8" 0 0 0 :scale '(1.6 .35 1.2)))
    (:hay (model g "prop-hay"))
    (:fence (model g "prop-fence"))
    (:pumpkin (model g "prop-pumpkin"))
    (:campfire (model g "prop-campfire"))
    (:reeds (loop repeat 9 do (shape g :box (list .04 (+ .9 (random .5)) .04) (if (eq *season* :winter) "#8a7a5a" "#6a7a3a")
                                     (- (random 1.6) .8) .5 (- (random 1.6) .8) :shadow nil))))
  (when (member kind '(:flowers :mush :reeds :pumpkin :lantern :campfire :rock :cairn :fence))
    (labels ((far (node)                                              ; a shape, or a model with its meshes under it
               (when (object-call node "is_class" "GeometryInstance3D")
                 (object-set node "visibility_range_end" *small-far*)
                 (object-set node "visibility_range_end_margin" 15.0)
                 (object-set node "visibility_range_fade_mode" *small-fade*))
               (loop for child across (node-get-children node) do (far child))))
      (loop for child across (node-get-children g) do (far child)))))

(defparameter *woods* '(:pine 2 :birch 1 :autumn 1)
  "How many trees stand behind every tree by the road, by the vegetation: a bor is a wall of trunks, not a row.")

(defun place-prop (d side kind x)
  "Stand a thing of kind KIND at path coordinate D, X metres from the road (signed), unless another road runs there."
  (let ((h (min (height-at x d) (height-at (- x 1.2) d) (height-at (+ x 1.2) d)))   ; on a slope — by the lower edge, so it does not hang in the air
        (wet (> (max (weight :water :lake d) (weight :water :river d)) .01)))
    (when (and wet (< h (- *water-y* .1))) (setf kind :reeds))     ; only reeds grow in water
    (destructuring-bind (wx wy wz) (road-pos d x)                 ; at a fork this spot may turn out to be another road
      (declare (ignore wy))
      (when (on-road-p wx wz) (return-from place-prop nil)))
    (let ((g (apply #'group *world* (road-pos d x (if wet (max *water-y* h) (- h .3))))))
      (node-3d-set-rotation g (v3 0 (- (road-frame d)) 0))
      (build-prop g kind side)
      (setf *props* (nconc *props* (list (cons g d)))))))

(defun add-prop (d side)
  (when (bare-at d) (return-from add-prop nil))                  ; a bridge, a tunnel
  (when (town-at d) (return-from add-prop (town-prop d side)))   ; a town has its own roadside (doroga.lisp)
  (let* ((veg (pick-weighted (layer-weights :veg d)))
         (kinds (getf *veg-props* veg))
         (kind (season-prop (elt kinds (random (length kinds))))))
    (place-prop d side kind (* side (if (eq kind :cliff) (+ 16 (random 8.0)) (+ (if (< side 0) 7.8 4) (random 12.0)))))   ; on the left — beyond the oncoming lane
    (loop repeat (getf *woods* veg 0)                              ; the wood behind the roadside
          do (place-prop (+ d (random *prop-step*)) side veg (* side (+ 18.0 (random 34.0)))))))

;;; Grass. A tuft is twelve triangles (tools/draw/make_grass.py); a patch of the roadside is one MultiMesh of some
;;; dozens of tufts, one draw. The shader sways the blades and lets them sink into the ground as they near *GRASS-FAR*
;;; from the camera, beyond which a patch is not drawn at all: the cost is what stands within fifty metres.

(defparameter *grass* t "Whether grass grows by the road: NIL for a machine that cannot bear it.")
(defparameter *grass-step* 6.0 "The length of a patch of grass along the road, in metres: patches of the two sides take turns.")
(defparameter *grass-far* 55.0 "The distance from the camera at which the grass has sunk into the ground, in metres.")
(defparameter *grass-thick* '(:meadow 150 :steppe 100 :birch 100 :autumn 90 :marsh 120 :pine 45 :rocky 30)
  "How many tufts stand in a patch, by the vegetation of the place.")
(defparameter *grass-code* "shader_type spatial;
render_mode cull_disabled;
uniform vec3 tint : source_color;
uniform float far = 55.0;
varying float up;
void vertex() {
  vec3 w = (MODEL_MATRIX * vec4(VERTEX, 1.0)).xyz;
  float near = clamp((far - distance(w, CAMERA_POSITION_WORLD)) / 14.0, 0.0, 1.0);
  up = clamp(VERTEX.y * 2.0, 0.0, 1.0);
  VERTEX.y *= near;
  VERTEX.x += sin(TIME * 1.6 + w.x * 0.6 + w.z * 0.45) * 0.07 * up * near;
  NORMAL = vec3(0.0, 1.0, 0.0);
}
void fragment() {
  ALBEDO = tint * COLOR.rgb * mix(0.5, 0.95, up);
  ROUGHNESS = 1.0;
  SPECULAR = 0.0;
}" "The shader of the grass.")
(defvar *grass-mats* '() "The materials of the grass by season: a list of (season . ShaderMaterial).")

(defun grass-material ()
  "Return the material of the grass for the current season: green, yellowed in autumn, dry stalks in winter."
  (or (cdr (assoc *season* *grass-mats*))
      (let ((sh (gdext:make-object "Shader")) (mat (gdext:make-object "ShaderMaterial")))
        (object-set sh "code" *grass-code*)
        (object-set mat "shader" sh)
        (object-call mat "set_shader_parameter" "tint"
                     (hex (case *season* (:spring "#5a8a3a") (:autumn "#8a7a3c") (:winter "#a09468") (t "#456e30"))))
        (object-call mat "set_shader_parameter" "far" *grass-far*)
        (push (cons *season* mat) *grass-mats*)
        mat)))

(defun grass-mesh ()
  "Return the mesh of a tuft: the one mesh of the model grass-tuft."
  (labels ((find-mesh (node)
             (if (object-call node "is_class" "MeshInstance3D")
                 (object-get node "mesh")
                 (loop for child across (node-get-children node) thereis (find-mesh child)))))
    (let ((entry (model-entry "grass-tuft"))) (and entry (find-mesh (first entry))))))

(defun add-grass (d side)
  "Grow a patch of grass at path coordinate D on side SIDE (±1): tufts strewn from the verge to thirty metres out,
thicker by the road, each on the land where it stands. None in a town, on a bridge, on a road or under water; in
winter one tuft in five shows through the snow."
  (when (or (not *grass*) (bare-at d) (town-at d)) (return-from add-grass nil))
  (let* ((n (floor (getf *grass-thick* (dominant :veg d) 60) (if (eq *season* :winter) 5 1)))
         (mesh (grass-mesh)) (origin (road-pos d (* side 6.0) (height-at (* side 6.0) d))) (places '()))
    (unless mesh (return-from add-grass nil))
    (destructuring-bind (ox oy oz) origin
      (loop repeat n
            do (let* ((r (random 1.0)) (x (* side (+ (if (< side 0) 7.6 3.8) (* 28.0 r r)))) (dd (+ d (random (* 2 *grass-step*))))
                      (h (height-at x dd)))
                 (destructuring-bind (wx wy wz) (road-pos dd x (- h .02))
                   (unless (or (< h (+ *water-y* .05)) (on-road-p wx wz))
                     (let ((s (+ .6 (random .7))))
                       (push (cons (place-transform (- wx ox) (- wy oy) (- wz oz) (list 0 (random 6.28) 0) (list s (* s (+ .8 (random .6))) s))
                                   (+ .75 (random .5)))
                             places)))))))
    (when places
      (let ((g (apply #'group *world* origin))                        ; unturned: the tufts are placed by the map
            (mm (gdext:make-object "MultiMesh")) (mmi (gdext:make-object "MultiMeshInstance3D")))
        (object-set mm "transform_format" 1)                          ; TRANSFORM_3D
        (object-set mm "use_colors" t)
        (object-set mm "mesh" mesh)
        (object-set mm "instance_count" (length places))
        (loop for (at . shade) in places for i from 0
              do (object-call mm "set_instance_transform" i at)
                 (object-call mm "set_instance_color" i (color shade shade (* shade .9) 1.0)))
        (object-set mmi "multimesh" mm)
        (object-set mmi "material_override" (grass-material))
        (object-set mmi "cast_shadow" 0)
        (object-set mmi "visibility_range_end" (+ *grass-far* 40.0))
        (node-add-child g mmi)
        (setf *props* (nconc *props* (list (cons g d))))))))

;;; The window around the wagon. Everything by the roadside is built not 'ahead in ascending path coordinate'
;;; but in a window around the wagon: far along its heading (*AHEAD*), a little behind (*BEHIND*). Where it is
;;; heading is *DIR*: +1 along the path, −1 against it. COVER builds up the things standing on a grid (poles,
;;; lanterns, trees), LISTED the things standing by a list (bus stops, gas stations, steles); whatever has
;;; left the window is removed by the SWEEPs. A jump to another place on the map is to them just 'the window moved'.

(defparameter *lane-x* -4.5 "The middle of the second lane (sideways from the path axis).
It carries the traffic oncoming to those driving forward.")

(defun lane () (if (> *dir* 0) 0.0 *lane-x*))
(defun ahead-of (d) "Return the distance to path coordinate D along the wagon's heading (negative when behind)." (* (- d *dist*) *dir*))
(defun side-x (off) "Convert OFF, to the right of the wagon, to the offset sideways from the path axis." (+ (lane) (* off *dir*)))
(defun turning-p () (> (abs (- *yaw-now* (if (> *dir* 0) 0.0 pi))) .03))

(defparameter *turn-legs* '((1 80) (-1 20) (1 80))
  "The U-turn of a lorry on a road too narrow to turn in one go: forward with the wheel to the left, back
with it to the right, forward to the left again. Each leg is (direction degrees): the heading turns the
same way on every leg, by so many degrees, along an arc.")
(defparameter *turn-speed* 1.8 "The speed of the truck during a U-turn, m/s.")
(defparameter *turn-pause* .5 "The pause between the legs of a U-turn, s: the driver changes gear.")
(defvar *u-turn* nil "The U-turn under way: (radius sign leg turned pause), or NIL.")
(define-fields u-turn radius sign leg turned pause)

(defun turn-shift ()
  "Return the sideways shift a whole U-turn of radius 1 makes, along the legs of *TURN-LEGS*."
  (let ((th 0.0) (shift 0.0))
    (loop for (dir deg) in *turn-legs*
          do (let ((a (* deg (/ pi 180))))
               (incf shift (* dir (- (cos th) (cos (+ th a)))))
               (incf th a)))
    shift))

(defun step-turn (delta)
  "Perform the U-turn: the truck drives along arcs, as a lorry turns on a narrow road (*TURN-LEGS*): the
heading turns only while it moves, and it moves from its lane to the other one. Only the driver moves
the truck along the road; guests are told where it stands."
  (let ((want (if (> *dir* 0) 0.0 pi)))
    (cond ((not (turning-p)) (setf *u-turn* nil *yaw-now* want)
           (let ((dl (- (lane) *lane-now*)) (ls (* 1.8 delta)))       ; the last few centimetres
             (setf *lane-now* (if (< (abs dl) ls) (lane) (+ *lane-now* (* ls (signum dl)))))))
          (t (unless *u-turn*
               ;; the heading turns the way that takes the first leg forward towards the other lane
               (let* ((shift (- (lane) *lane-now*)) (sign (* (if (minusp shift) -1 1) (if (minusp (cos *yaw-now*)) -1 1))))
                 (setf *u-turn* (list (max 2.0 (/ (abs shift) (turn-shift))) sign 0 0.0 0.0))))
             (destructuring-bind (r sign leg turned pause) *u-turn*
               (let ((spec (nth leg *turn-legs*)))
                 (cond ((plusp pause) (setf (u-turn-pause *u-turn*) (max 0.0 (- pause delta))))
                       ((null spec) (setf *yaw-now* want *lane-now* (lane) *u-turn* nil))
                       (t (destructuring-bind (dir deg) spec
                            (let* ((left (- (* deg (/ pi 180)) turned))
                                   (da (min left (/ (* *turn-speed* delta) r))) (ds (* r da)) (th *yaw-now*))
                              ;; heading TH from the road: forward it moves along the road by cos TH and towards *LANE-X* by sin TH
                              (setf *yaw-now* (+ th (* sign da)))
                              (incf *lane-now* (* dir ds (sin th)))
                              (unless (net-guest-p) (incf *dist* (* dir ds (cos th))))
                              (if (>= da left)
                                  (setf (u-turn-leg *u-turn*) (1+ leg) (u-turn-turned *u-turn*) 0.0 (u-turn-pause *u-turn*) *turn-pause*)
                                  (setf (u-turn-turned *u-turn*) (+ turned da)))))))))))))

(defvar *spans* (make-hash-table :test 'equal) "The table of what is already built on the grid: key → (first . last) grid node number.")
(defvar *listed* (make-hash-table :test 'equal) "The table of what is already built by the list: key → (path-coordinate . reach).")

(defun win-lo (&optional (reach 0.0)) (- *dist* (if (> *dir* 0) *behind* *ahead*) reach))
(defun win-hi (&optional (reach 0.0)) (+ *dist* (if (> *dir* 0) *ahead* *behind*) reach))

;;; The path coordinate is the address of a thing on the road, not space. There may be other roads near the
;;; wagon: at a fork, at a crossroads. Their pieces inside the visibility circle are a window too — *SIDE-ROADS*;
;;; then whatever stands on a neighbouring road is seen in advance, and moving onto it swaps nothing out.

(defvar *side-roads* '() "The pieces of other roads inside the visibility circle: (road-start from to) in path coordinates.")
(defvar *side-tick* 0)

(defun note-side-roads (&optional now)
  "Recompute *SIDE-ROADS* once every few frames, or right away if NOW: the wagon has been moved."
  (when (or now (zerop (mod (incf *side-tick*) 8)))
    (multiple-value-bind (x z) (road-point *dist*)
      (let ((own (edge-of *dist*)) (r (float *ahead* 1.0)) (found '()))
        (loop for (d nil dist e) in (roads-near x z)
              do (unless (or (eq e own) (>= dist r))
                   (let* ((half (sqrt (- (* r r) (* dist dist))))
                          (lo (max (edge-d0 e) (- d half))) (hi (min (edge-d1 e) (+ d half)))
                          (old (assoc (edge-d0 e) found)))
                     (if old
                         (setf (second old) (min lo (second old)) (third old) (max hi (third old)))
                         (push (list (edge-d0 e) lo hi) found)))))
        (setf *side-roads* found)))))

(defun in-window-p (d &optional (reach 0.0))
  "Return true if path coordinate D is in the window (with the margin REACH).
That is, on our own road near the wagon or on a piece of a neighbouring one."
  (or (< (win-lo reach) d (win-hi reach))
      (some (lambda (s) (< (- (second s) reach) d (+ (third s) reach))) *side-roads*)))

(defun span-in-window-p (lo hi &optional (reach 0.0))
  "Return true if the path segment from LO to HI touches the window (with the margin REACH)."
  (or (and (< lo (win-hi reach)) (> hi (win-lo reach)))
      (some (lambda (s) (and (< lo (+ (third s) reach)) (> hi (- (second s) reach)))) *side-roads*)))

(defun out-of-window-p (d &optional (reach 0.0)) (not (in-window-p d (+ reach 6.0))))

(defparameter *cover-budget* 4 "How many grid nodes COVER builds in one call. When a side road came into the window at a fork,
everything along it - hundreds of metres of poles, trees and trifles - was built in one frame: a freeze of up to a
second in the browser. Now the roadside grows a few things a frame, from the wagon outwards.")

(defun cover (key step fn &optional (reach 0.0))
  "Call FN (path-coordinate number) for the grid nodes with spacing STEP in the window that are not yet built, at most
*COVER-BUDGET* of them in one call: the rest wait for the next frames. What is built is tracked per road: *SPANS*
keeps under KEY a list of (road-start first-node . last-node)."
  (let ((book (gethash key *spans*)) (fresh '()) (left *cover-budget*))
    (flet ((span (edge lo hi)
             (let* ((i0 (ceiling lo step)) (i1 (floor hi step)) (old (cdr (assoc edge book))) a b)
               (when (<= i0 i1)
                 (if (or (null old) (> i0 (1+ (cdr old))) (< i1 (1- (car old))))
                     ;; the window moved - anew, from the node nearest to the wagon: an empty range just before it
                     (let ((near (max i0 (min i1 (round *dist* step)))))
                       (setf a near b (1- near)))
                     (setf a (max i0 (car old)) b (min i1 (cdr old))))
                 (loop while (and (plusp left) (or (< b i1) (> a i0)))
                       do (when (and (plusp left) (< b i1))
                            (incf b) (decf left) (with-solids (funcall fn (* b step) b)))
                          (when (and (plusp left) (> a i0))
                            (decf a) (decf left) (with-solids (funcall fn (* a step) a))))
                 (push (cons edge (cons a b)) fresh)))))
      (span (edge-d0 (edge-of *dist*)) (win-lo reach) (win-hi reach))
      (loop for (edge lo hi) in *side-roads* do (span edge (- lo reach) (+ hi reach))))
    (setf (gethash key *spans*) fresh)))

(defvar *keep-reach* 0.0 "The margin beyond the window within which to keep what is being built now (a gas station is seen from afar).")

(defmacro listed ((key d &optional (reach 0.0)) &body body)
  "Build the thing KEY standing at path coordinate D while it is in the window (with the margin REACH).
It is built once, until it is removed. This is called every frame for everything on the map, so the cheap
window check comes first, and the key and the building itself are evaluated only for what is nearby."
  (let ((dv (gensym "D")) (rv (gensym "REACH")) (kv (gensym "KEY")))
    `(let ((,dv ,d) (,rv ,reach))
       (when (in-window-p ,dv ,rv)
         (let ((,kv ,key))
           (unless (gethash ,kv *listed*)
             (setf (gethash ,kv *listed*) (cons ,dv ,rv))
             (let ((*keep-reach* ,rv)) (with-solids ,@body))))))))

(defun sweep-listed ()
  (maphash (lambda (key v) (when (out-of-window-p (car v) (cdr v)) (remhash key *listed*))) *listed*))

(defun step-props ()
  (cover :props *prop-step* (lambda (d i) (add-prop d (if (evenp i) 1 -1))))
  (cover :grass *grass-step* (lambda (d i) (add-grass d (if (evenp i) 1 -1))))
  (setf *props* (remove-if (lambda (p) (when (out-of-window-p (cdr p)) (node-queue-free (car p)) t)) *props*)))

;;; Bridges — where the river ahead crosses the road

(defun add-bridge (d)
  (let ((g (apply #'group *world* (road-pos d))))
    (multiple-value-bind (course slope) (road-frame d)
      (node-3d-set-rotation g (v3 slope (- course) 0)))
    (model g "wood-bridge")
    (push (cons g d) *bridges*)))

(defun step-bridges ()
  "Build a little bridge where the stream passes from one side of the road to the other."
  (cover :bridges 20.0
         (lambda (d i)
           (declare (ignore i))
           ;; the stream must be real: water right by the roadbed on at least one side. In a town and on
           ;; level grounds the riverbed is filled in — there a bridge would lie in the middle of a dry street
           ;; (that is how it was: of sixteen bridges on the ring road only one saw water)
           (when (and (/= (signum (river-x d)) (signum (river-x (- d 20.0)))) (> (weight :water :river d) .05)
                      (not (town-at d)) (not (bare-at d))
                      (some (lambda (x) (< (height-at x d) *water-y*)) '(-20.0 -12.0 9.0 16.0)))
             (add-bridge d))))
  (setf *bridges* (remove-if (lambda (b) (when (out-of-window-p (cdr b)) (node-queue-free (car b)) t)) *bridges*)))

;;; As a whole

(defun make-pivots (root)
  "Create the two pivots under ROOT.
Turn turns the world and the sun to the course; Rig tilts the wagon with the camera."
  (setf *turn* (group root) *rig* (group root)))

(defun make-world (parent)
  "Create World under PARENT (Turn): the things at the start of the path.
The land (Land) is a separate node; it outlives a rebuild of the world."
  (when *world*
    ;; whatever still points at a node under the old World is forgotten here, next to where the nodes go. The
    ;; stages of START-TRIP forget most of it earlier, but each stage runs even if the one before it failed, and
    ;; a reference that outlived its node is an error in every frame from then on.
    (reset-vista)
    (forget-find-nodes) (forget-gnome-nodes)
    (node-queue-free *world*))
  (unless *land* (setf *land* (group parent)))
  (setf *world* (group parent) *dist* 0.0 *props* '() *bridges* '()
        *origin* (list 0.0 0.0 0.0))
  (node-3d-set-position *land* (v3 0 0 0))
  (terrain-shift)
  (clrhash *spans*) (clrhash *listed*) (clear-solids)                       ; a new world — nothing has been built yet
  (step-props)
  (place-world))

(defun place-world ()
  "Put the wagon on the road point under it and return the course and the slope.
The Rig itself is placed by step-cart together with the sway. Far from the origin, move the origin: the
world of things and the land are shifted."
  (multiple-value-bind (x z course) (road-point *dist*)
    (let ((y (road-y *dist*)) (slope (* (cos *yaw-now*) (nth-value 1 (road-frame *dist*)))))
      (setf x (+ x (* *lane-now* (cos course))) z (+ z (* *lane-now* (sin course)))        ; its own lane
            course (+ course *yaw-now*))
      (destructuring-bind (ox oy oz) *origin*
        (declare (ignore oy))
        (when (> (max (abs (- x ox)) (abs (- z oz))) *rebase*)
          ;; the origin sits on the terrain's grid: at a multiple of 1024 m in x and z (zemlya.lisp)
          (setf *origin* (list (* 1024.0 (round x 1024.0)) (float (round y) 1.0) (* 1024.0 (round z 1024.0))))
          (destructuring-bind (ox oy oz) *origin*
            (node-3d-set-position *world* (v3 (- ox) (- oy) (- oz)))
            (node-3d-set-position *land* (v3 (- ox) (- oy) (- oz))))
          (terrain-shift)))
      (destructuring-bind (ox oy oz) *origin*
        (setf *rig-at* (list (- x ox) (- y oy) (- z oz)) *course* (float course 1.0) *slope* slope))
      (values course slope))))

(defun step-world (delta)
  (step-turn delta)
  (note-side-roads)
  (if (and (off-ring-p) (or (> *dir* 0) (plusp *mega-s*)))                ; behind a door we stand still on the ring:
      (incf *mega-s* (* *dir* *speed* delta))                              ; the way driven there is its own count (place.lisp)
      (incf *dist* (* *dir* *speed* delta (cos (nth-value 1 (road-frame *dist*))))))   ; along the road itself — at the same speed
  (when (ring) (step-way))                                            ; an exit, a crossroads, an entry, a new lap
  (place-world)
  (step-roads)
  (timed :props (step-props))
  (step-bridges))
