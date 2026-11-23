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

(defparameter *speed* 7.0 "The truck speed in m/s.")
(defparameter *smooth-ground* t "True for smooth land lighting: flat faces far away are smaller than a pixel and shimmer.")
(defparameter *prop-step* 1.4 "The spacing of the roadside things, in metres.")
(defparameter *ahead* 170.0 "The distance ahead up to which we build.")
(defparameter *behind* 60.0 "The distance behind up to which we keep things.")

(defparameter *veg-props*
  '(:pine (:pine :pine :pine :lantern :mush :campfire :rock)
    :meadow (:flowers :flowers :flowers :hay :fence :round :lantern)
    :autumn (:autumn :autumn :autumn :mush :pumpkin :lantern :hay)
    :birch (:birch :birch :birch :round :lantern :rock :mush)
    :rocky (:rock :rock :cairn :cairn :pine)
    :steppe (:bush :bush :hay :rock :cairn :fence :lantern)
    :marsh (:reeds :reeds :snag :snag :bush :mush :lantern)))
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
(defvar *river-side* 0)

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

(defun build-prop (g kind side)
  "Build a thing of kind KIND in the group G (already in place).
SIDE is ±1, the side of the road. The colours of foliage and flowers follow the season."
  (case kind
    (:pine (shape g :cyl '(.15 .25 3 5) "#3a2a1e" 0 1.5 0)
     (loop for j below 3 do (shape g :cone (list (- 1.4 (* j .35)) 1.8) (season-conifer (nth j '("#1e3a24" "#24482c" "#2a5232"))) 0 (+ 2.4 j) 0)))
    (:round (shape g :cyl '(.18 .25 2.4 5) "#4a3424" 0 1.2 0) (crown g '(1.4) "#3a6a2a" 0 3 0))
    (:autumn (shape g :cyl '(.16 .24 2.6 5) "#4a3424" 0 1.3 0)
     (crown g (list (+ 1.3 (random .5))) (nth (random 3) '("#d86a20" "#c84020" "#e0a030")) 0 3.1 0))
    (:birch (shape g :cyl '(.1 .14 4 6) "#e0dcd0" 0 2 0) (crown g '(1.0) "#8ab040" 0 4 0))
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

(defun add-prop (d side)
  (when (bare-at d) (return-from add-prop nil))                  ; a bridge, a tunnel
  (when (town-at d) (return-from add-prop (town-prop d side)))   ; a town has its own roadside (doroga.lisp)
  (let* ((veg (pick-weighted (layer-weights :veg d)))
         (kinds (getf *veg-props* veg))
         (kind (season-prop (nth (random (length kinds)) kinds)))
         (x (* side (if (eq kind :cliff) (+ 16 (random 8.0)) (+ (if (< side 0) 7.8 4) (random 12.0)))))   ; on the left — beyond the oncoming lane
         (h (min (height-at x d) (height-at (- x 1.2) d) (height-at (+ x 1.2) d)))   ; on a slope — by the lower edge, so it does not hang in the air
         (wet (> (max (weight :water :lake d) (weight :water :river d)) .01)))
    (when (and wet (< h (- *water-y* .1))) (setf kind :reeds))     ; only reeds grow in water
    (destructuring-bind (wx wy wz) (road-pos d x)                 ; at a fork this spot may turn out to be another road
      (declare (ignore wy))
      (when (on-road-p wx wz) (return-from add-prop nil)))
    (let ((g (apply #'group *world* (road-pos d x (if wet (max *water-y* h) (- h .3))))))
      (node-3d-set-rotation g (v3 0 (- (road-frame d)) 0))
      (build-prop g kind side)
      (setf *props* (nconc *props* (list (cons g d)))))))

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
                 (cond ((plusp pause) (setf (fifth *u-turn*) (max 0.0 (- pause delta))))
                       ((null spec) (setf *yaw-now* want *lane-now* (lane) *u-turn* nil))
                       (t (destructuring-bind (dir deg) spec
                            (let* ((left (- (* deg (/ pi 180)) turned))
                                   (da (min left (/ (* *turn-speed* delta) r))) (ds (* r da)) (th *yaw-now*))
                              ;; heading TH from the road: forward it moves along the road by cos TH and towards *LANE-X* by sin TH
                              (setf *yaw-now* (+ th (* sign da)))
                              (incf *lane-now* (* dir ds (sin th)))
                              (unless (net-guest-p) (incf *dist* (* dir ds (cos th))))
                              (if (>= da left)
                                  (setf (third *u-turn*) (1+ leg) (fourth *u-turn*) 0.0 (fifth *u-turn*) *turn-pause*)
                                  (setf (fourth *u-turn*) (+ turned da)))))))))))))

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

(defun cover (key step fn &optional (reach 0.0))
  "Call FN (path-coordinate number) for every grid node with spacing STEP in the window and not yet built.
What is built is tracked per road: *SPANS* keeps under KEY a list of (road-start first-node . last-node)."
  (let ((book (gethash key *spans*)) (fresh '()))
    (flet ((span (edge lo hi)
             (let* ((i0 (ceiling lo step)) (i1 (floor hi step)) (old (cdr (assoc edge book))))
               (if (or (null old) (> i0 (1+ (cdr old))) (< i1 (1- (car old))))
                   (loop for i from i0 to i1 do (with-solids (funcall fn (* i step) i)))          ; the window moved — everything anew
                   (progn (loop for i from (1+ (cdr old)) to i1 do (with-solids (funcall fn (* i step) i)))
                          (loop for i from i0 below (car old) do (with-solids (funcall fn (* i step) i)))))
               (push (cons edge (cons i0 i1)) fresh))))
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
  (when *world* (node-queue-free *world*))
  (unless *land* (setf *land* (group parent)))
  (setf *world* (group parent) *dist* 0.0 *props* '() *bridges* '() *river-side* 0
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
