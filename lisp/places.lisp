;;;; The plan of places. The world is permanent, so everything special stands by plan, not 'as the dice fall':
;;;; here are the tables (landmarks, diners, rails, the route sheet of the relief, bridge, tunnel, town
;;;; themes) and PLAN-PLACES, which uses them to give the land its pads and level road sections and to set up
;;;; the 'scenes' - what gets built when it is *FAR-RANGE* away and removed once behind. The buildings are in
;;;; marks.lisp and towns.lisp, the far land in far.lisp, everything that moves in traffic.lisp, the frame step in vista.lisp.

(in-package #:kiosk-trip)

;;; ------------------------------------------------------------------ the plan

(defvar *landmark-plan* '()
  "The landmarks, each as (stretch before town number, fraction of the stretch, side +-1, what).
The right side is the side of the window.")

(defparameter *landmark-kinds*
  '(:windmill (38 13 2.5 30) :plane (22 8 .3 14) :church (64 17 9.0 50) :pumpjacks (40 22 0.0 22)
    :mast (72 10 1.0 25) :elevator (50 26 0.0 25) :wheel (62 24 0.0 30) :pylons nil
    :watchtower (30 8 3.0 22) :oldbus (17 7 -.25 10) :radar (78 19 13.0 60) :farm (46 28 0.0 25)
    :rocket (26 9 .3 14) :camp (26 22 0.0 25))
  "A plist from a landmark kind to the numbers of its pad.
They are (offset sideways from the road, pad radius, pad height above the road, transition margin).")

(defvar *diner-plan* '()
  "The roadside diners on the right, each as (stretch, fraction, sign text, kind, color).")

(defparameter *rail-plan* '((10 .08 .92)) "The railways on the right, each as (stretch, from fraction, to fraction).")
(defparameter *rail-x* 52.0 "The distance in meters between the rails and the highway.")
(defparameter *far-range* 1400.0 "The distance ahead at which landmarks and towns are visible.")

;;; Special places of the road. A stretch is the number of the town it lies before; fractions run from its start.
(defvar *route-plan* '()
  "The route sheet, each entry as (stretch from-fraction to-fraction layer kind).")
(defparameter *chasm-at* '(4 .58) "The stretch and fraction of the bridge over the chasm.")
(defparameter *chasm-half* 42.0 "Half the width of the chasm along the road, in meters.")
(defparameter *tunnel-at* '(11 .62 250.0) "The stretch, start fraction and length of the tunnel.")
(defparameter *swamp-gap* 8 "The stretch with the swamp, which has its own gas station and mosquitoes.")

(defvar *chasm* nil "The path coordinate of the middle of the bridge.")
(defvar *tunnel* nil "The (from to) path coordinates of the tunnel.")
(defvar *mega* nil "The (from to) path coordinates of the tunnel through which the megastructure is entered, or NIL if there is
none. From outside it is a plain tunnel; the megastructure is another place, shown only to those inside (marks.lisp).")
(defvar *mega-node* nil "The megastructure as built right now, a node, or NIL while nobody of us is inside.")
(defvar *swamp* nil "The (from to) path coordinates of the swamp.")
(defun in-swamp-p (d) (and *swamp* (< (first *swamp*) d (second *swamp*))))

(defparameter *town-themes* '(:river :military :factory :university :village :port :admin :outpost :mine :resort :rail :sawmill)
  "The theme that sets each town apart from the others, by town number.")
(defun town-theme (tw) (nth (town-index tw) *town-themes*))
(defparameter *theme-gaps* '(:military (.22 .62) :factory (.36 .58) :university (.38 .62) :port (.2 .74) :mine (.33 .53) :rail (.31 .5)
    :river (.12 .40) :admin (.38 .62) :outpost (.3 .6) :resort (.3 .6) :sawmill (.3 .55) :village (.47 .53))
  "A plist from a town theme to where the first row of houses on the right parts.
The two numbers are fractions of the town length.
Without the gap the main sight of a themed town could not be seen from the window.")
(defun city-gap (tw d)
  "Return the path coordinate of the end of the gap in town TW at path coordinate D.
Return NIL unless such a gap starts or runs on the right at D."
  (let ((g (getf *theme-gaps* (town-theme tw))))
    (when g
      (let* ((len (- (town-end tw) (town-start tw)))
             (g0 (+ (town-start tw) (* (first g) len))) (g1 (+ (town-start tw) (* (second g) len))))
        (when (< (- g0 70) d g1) g1)))))

(defstruct scene
  "A place seen from afar, built when it is *FAR-RANGE* away and removed once left behind."
  d0 d1 build node moves
  at)                          ; (x z) on the map, if the scene is not tied to a single road (the Crossroads)

(defvar *scenes* '() "All the scenes of the world, as SCENE structures.")
(defvar *tracks* '() "The railways, each as (from to).")
(defvar *moves* '() "The things that spin and swing, each as (node kind . numbers), collected while a scene is built.")

(defun gap-d (i f)
  "Return the path coordinate at fraction F of the stretch before town I."
  (let ((tw (nth i *towns*)))
    (+ (town-from tw) (* f (- (town-start tw) (town-from tw))))))

(defun track-x (track d)
  "Return how far sideways from the highway the rails of TRACK are at path coordinate D.
At the ends they veer away, in the middle they run alongside."
  (destructuring-bind (d0 d1) track
    (+ *rail-x* (* 420 (- 1 (* (sstep d0 (+ d0 320) d) (- 1 (sstep (- d1 320) d1 d))))))))

(defun plan-places ()
  "Collect the pads and scenes of the world from the town layout.
It is called before the land is built."
  (reset-segments)
  (build-map)                                               ; road curves on the map - from the town layout
  (reset-vista)
  (setf *pads* '() *levels* '() *scenes* '() *tracks* '() *decks* '() *bare* '() *basins* '()
        *route* (loop for (i f0 f1 layer kind) in *route-plan* collect (list (gap-d i f0) (gap-d i f1) layer kind))
        *swamp* (list (gap-d *swamp-gap* .04) (gap-d *swamp-gap* .96))
        *extra-stations* (list (gap-d *swamp-gap* .5))
        *chasm* (straightest-d (gap-d (first *chasm-at*) (- (second *chasm-at*) .1))      ; the bridge goes on the straightest piece:
                               (gap-d (first *chasm-at*) (+ (second *chasm-at*) .1))      ; on a bend of the road it came out crooked
                               (+ *chasm-half* 40.0))
        *tunnel* (let ((d (gap-d (first *tunnel-at*) (second *tunnel-at*)))) (list d (+ d (third *tunnel-at*))))
        *mega* nil *mega-node* nil)
  (when (boundp '*mega-in*) (setf (symbol-value '*mega-in*) nil))
  (reset-segments)
  (flet ((pad (&rest keys) (push (apply #'make-pad keys) *pads*))
         (level (d0 d1 &optional (ramp 90.0))                 ; the road along a pad is level, at the height of its middle
           (push (list d0 d1 (raw-road-y (/ (+ d0 d1) 2)) ramp) *levels*)))
    ;; towns: the street is level, on both sides a terrace for buildings and the second rows of houses
    (dolist (tw *towns*)
      (let ((d0 (- (town-start tw) 30)) (d1 (+ (town-end tw) 30)))
        (level d0 d1 260.0)
        (cond ((eq (town-theme tw) :port)                    ; port: beyond the second line of houses - the sea
               (pad :d0 d0 :d1 d1 :ramp 60.0 :x0 4.0 :x1 128.0 :m0 4.0 :m1 14.0 :y -.37)
               (pad :d0 d0 :d1 d1 :ramp 80.0 :x0 150.0 :x1 1100.0 :m0 8.0 :m1 200.0 :y -5.0)
               (let ((c (road-pos (/ (+ d0 d1) 2) 900.0)))     ; and farther out, where the land is already shared by the map, sea too
                 (push (list (first c) (third c) 1300.0 (- (road-y (/ (+ d0 d1) 2)) 6.0)) *basins*)))
              ((eq (town-theme tw) :river)                   ; river town: beyond the embankment - a river along the whole town
               (pad :d0 d0 :d1 d1 :ramp 60.0 :x0 4.0 :x1 58.0 :m0 4.0 :m1 8.0 :y -.37)
               (pad :d0 d0 :d1 d1 :ramp 70.0 :x0 74.0 :x1 150.0 :m0 8.0 :m1 40.0 :y -3.4))
              (t (pad :d0 d0 :d1 d1 :ramp 60.0 :x0 4.0 :x1 125.0 :m0 4.0 :m1 110.0 :y -.37)))
        (when (eq (town-theme tw) :resort)                    ; resort: the mountain the cable car goes up
          (let ((len (- (town-end tw) (town-start tw))))
            (pad :d0 (+ (town-start tw) (* .34 len)) :d1 (+ (town-start tw) (* .56 len)) :ramp 90.0
                 :x0 240.0 :x1 310.0 :m0 80.0 :m1 80.0 :y 46.0)))
        (pad :d0 d0 :d1 d1 :ramp 60.0 :x0 -125.0 :x1 -4.0 :m0 110.0 :m1 4.0 :y 0.0)     ; on the left the oncoming lane - flush with the road
        (let ((tw tw)) (push (make-scene :d0 (town-start tw) :d1 (town-end tw) :build (lambda () (build-skyline tw))) *scenes*))
        (dolist (s (town-stations tw))                               ; gas stations outside the town
          (unless (town-at s) (level (- s 20) (+ s 20)) (pad :d0 (- s 20) :d1 (+ s 20) :ramp 14.0 :x0 4.0 :x1 27.0 :m0 4.0 :m1 14.0 :y -.37)))))
    (level -14.0 46.0)
    (pad :d0 -14.0 :d1 46.0 :ramp 16.0 :x0 4.0 :x1 34.0 :m0 4.0 :m1 16.0 :y -.37)   ; the warehouse we set out from
    ;; bus stops between towns
    (loop for d = 150.0 then (stop-after d) while (< d 100500.0)     ; bus stops - both on the ring road and on the spokes
          do (unless (or (town-at d) (off-road-p d) (< 25300.0 d *ring-top*)) (pad :d0 (- d 4) :d1 (+ d 4) :ramp 5.0 :x0 4.0 :x1 10.5 :m0 4.0 :m1 5.0 :y -.2)))
    ;; railway: the embankment
    (loop for (i f0 f1) in *rail-plan*
          do (let ((track (list (gap-d i f0) (gap-d i f1))))
               (push track *tracks*)
               (pad :d0 (first track) :d1 (second track) :ramp 20.0 :x0 -3.4 :x1 3.4 :m0 8.0 :m1 8.0 :y .7
                    :shift (lambda (d) (track-x track d)))))
    ;; the bridge over the chasm: the land drops away across the full width, the road runs on a deck
    (let ((c *chasm*) (h *chasm-half*))
      (level (- c h 30) (+ c h 30) 120.0)
      (pad :d0 (- c h) :d1 (+ c h) :ramp 24.0 :x0 -700.0 :x1 700.0 :m0 260.0 :m1 260.0 :y -52.0)
      (push (list (- c h 30) (+ c h 30)) *decks*) (push (list (- c h 34) (+ c h 34)) *bare*)
      (push (make-scene :d0 (- c h 30) :d1 (+ c h 30) :build (lambda () (build-chasm-bridge c h))) *scenes*))
    ;; the tunnel: mountain on both sides of the road, the road itself in a cut under the vault
    (destructuring-bind (d0 d1) *tunnel*
      (setf *mega* (list d0 d1))                                  ; the megastructure is entered inside it (marks.lisp)
      (level (- d0 30) (+ d1 30) 140.0)
      (pad :d0 d0 :d1 d1 :ramp 34.0 :x0 13.0 :x1 300.0 :m0 3.9 :m1 190.0 :y 40.0)     ; the land is now a grid over the map: the cut is wider than the vault
      (pad :d0 d0 :d1 d1 :ramp 34.0 :x0 -300.0 :x1 -17.0 :m0 190.0 :m1 3.9 :y 40.0)
      (push (list (- d0 44) (+ d1 44)) *bare*))
    ;; the observatory in the mountains before the bridge and the scrap depot in the factory town: here one gets out of the wagon
    (let ((d (gap-d 4 .34)))
      (setf *observatory* d)
      (level (- d 22) (+ d 22))
      (pad :d0 (- d 22) :d1 (+ d 22) :ramp 16.0 :x0 4.0 :x1 30.0 :m0 4.0 :m1 14.0 :y -.37)
      (push (make-scene :d0 (- d 22) :d1 (+ d 22) :build (lambda () (build-observatory d))) *scenes*))
    (let* ((tw (find :factory *towns* :key #'town-theme)) (d (and tw (+ (town-start tw) (* .4 (- (town-end tw) (town-start tw)))))))
      (setf *depot-d* d)
      (when d (push (make-scene :d0 (- d 14) :d1 (+ d 14) :build (lambda () (build-scrap-depot d))) *scenes*)))
    (let* ((tw (nth *dalny* *towns*)) (d (and tw (+ (town-start tw) (* .44 (- (town-end tw) (town-start tw)))))))   ; airmail in Dalny, away from the market
      (setf *airmail-d* d *plane* nil)
      (when d (push (make-scene :d0 (- d 12) :d1 (+ d 12) :build (lambda () (build-airmail d))) *scenes*)))
    (when *hub* (push (make-scene :d0 (- (edge-d1 (cdr (first *exits*))) 60) :d1 (+ (edge-d1 (cdr (first *exits*))) 60)
                                  :build #'build-hub :at (list (first *hub*) (second *hub*))) *scenes*))
    (plan-gimmicks)                                         ; booths with the towns' gimmicks
    (setf *find-nodes* '() *gnome-nodes* '() *gnome-told* nil)                                 ; the old finds went away with the old world
    (setf *stop-ds* (append (mapcar #'town-stop-d *towns*)
                            (loop for d = 150.0 then (stop-after d) while (< d 100500.0)
                                  unless (or (town-at d) (bare-at d) (< 25300.0 d *ring-top*)) collect d)))
    ;; the swamp: patches of mist and fireflies
    (loop for d from (+ (first *swamp*) 60) below (second *swamp*) by 140
          do (let ((d d)) (push (make-scene :d0 (- d 70) :d1 (+ d 70) :build (lambda () (build-mist d))) *scenes*)))
    ;; diners: a pad with a parking lot
    (loop for (i f name kind rgb) in *diner-plan*
          do (let ((d (gap-d i f)) (name name) (kind kind) (rgb rgb))
               (level (- d 19) (+ d 19))
               (pad :d0 (- d 19) :d1 (+ d 19) :ramp 14.0 :x0 4.0 :x1 30.0 :m0 4.0 :m1 12.0 :y -.37)
               (push (make-scene :d0 (- d 20) :d1 (+ d 20) :build (lambda () (build-diner d name kind rgb))) *scenes*)))
    ;; landmarks
    (loop for (i f side kind) in *landmark-plan*
          do (let ((d (gap-d i f)) (spec (getf *landmark-kinds* kind)) (side side) (kind kind))
               (when (eq kind :camp) (push (list (- d 23) (+ d 23)) *bare*))     ; nothing grows in front of the camp's gates
               (when spec
                 (destructuring-bind (x r y m) spec
                   (level (- d r) (+ d r))
                   (pad :d0 (- d r) :d1 (+ d r) :ramp m :x0 (- (* side x) r) :x1 (+ (* side x) r) :m0 m :m1 m :y y)))
               (push (make-scene :d0 (- d 30) :d1 (+ d 30) :build (lambda () (build-landmark kind d side))) *scenes*)))
    ;; exits, entries and the Crossroads go last: they take the height of the ring road, and every level section above changes it
    (plan-junctions))
  (clrhash *rows*)
  (setf *row* nil))

;;; ------------------------------------------------------------------ helpers

(defun vr (a &optional (b 1.0))
  "Return a random number in 0..1 derived from the pair A and B.
Decorations thus leave the game's shared generator alone."
  (hash2 (* a 1.37) (+ b (* 17.0 *trip-seed*))))

(defun moving (node kind &rest numbers)
  "Record that NODE moves in the manner KIND with parameters NUMBERS, and return NODE.
The forms are :spin axis speed | :swing amplitude speed phase [axis] | :path ... |
:blink speed | :night, which is visible only in the dark."
  (push (list* node kind numbers) *moves*)
  node)

(defun place-at (d x &optional (up 0.0) (side 1))
  "Return a group under World at path coordinate D, X meters sideways from the road, UP above it.
Its x+ points away from the road on side SIDE and its z runs along the road."
  (destructuring-bind (px py pz) (side-point x d up)
    (let ((g (group *world* px py pz)))
      (node-3d-set-rotation g (v3 0 (- (if (< side 0) pi 0) (road-frame d)) 0))
      g)))

(defun straightest-d (d0 d1 half)
  "Return the path coordinate between D0 and D1 around which the road turns the least.
The turning is measured HALF meters in both directions."
  (let ((best d0) (least nil))
    (loop for d from d0 to d1 by 20.0
          do (let ((turn (loop for s from (- d half) below (+ d half) by 10.0
                               sum (abs (- (nth-value 2 (road-point (+ s 10.0))) (nth-value 2 (road-point s)))))))
               (when (or (null least) (< turn least)) (setf least turn best d))))
    best))

(defun road-group (parent d)
  "Return a group under PARENT on the road axis at path coordinate D, turned along the road.
Its x points sideways to the right and its z backwards along the path."
  (let ((g (apply #'group parent (road-pos d))))
    (node-3d-set-rotation g (v3 0 (- (road-frame d)) 0))
    g))

(defun beam-between (parent a b &key (thick .05) (rgb "#16161a") (sag 0.0))
  "Add a beam or wire between points A and B, given in the coordinates of PARENT.
SAG is how far it sags in the middle. Return the pieces it is made of."
  (let ((mid (list (/ (+ (first a) (first b)) 2) (- (/ (+ (second a) (second b)) 2) sag) (/ (+ (third a) (third b)) 2))))
    (loop for (p q) in (if (plusp sag) (list (list a mid) (list mid b)) (list (list a b)))
          collect (let* ((dx (- (first q) (first p))) (dy (- (second q) (second p))) (dz (- (third q) (third p)))
                         (flat (sqrt (+ (* dx dx) (* dz dz)))) (len (sqrt (+ (* flat flat) (* dy dy)))))
                    (shape parent :box (list thick thick len) rgb
                           (/ (+ (first p) (first q)) 2) (/ (+ (second p) (second q)) 2) (/ (+ (third p) (third q)) 2)
                           :rot (list (- (atan dy (max flat 1e-4))) (atan dx dz) 0) :shadow nil)))))

(defun chimney-smoke (parent x y z &key (size 1.2) (amount 8) (life 6.0) (rgb "#d4d4d8"))
  "Add chimney smoke under PARENT at (X Y Z) and return the particles node.
The puffs rise, drift with the wind and fade towards the end."
  (let ((p (gdext:make-object "GPUParticles3D")) (pm (gdext:make-object "ParticleProcessMaterial"))
        (mesh (gdext:make-object "QuadMesh")) (mat (gdext:make-object "StandardMaterial3D"))
        (ramp (gdext:make-object "Gradient")) (tex (gdext:make-object "GradientTexture1D")))
    (object-call ramp "set_color" 0 (hex rgb 0.0))
    (object-call ramp "set_color" 1 (hex rgb 0.0))
    (object-call ramp "add_point" .2 (hex rgb .5))
    (object-set tex "gradient" ramp)
    (object-set pm "color_ramp" tex)
    (object-set pm "direction" (v3 0 1 0))
    (object-set pm "spread" 14.0)
    (object-set pm "initial_velocity_min" (* size .5))
    (object-set pm "initial_velocity_max" (* size .9))
    (object-set pm "gravity" (v3 (* size .22) (* size .05) (* size .1)))
    (object-set pm "scale_min" .6)
    (object-set pm "scale_max" 1.5)
    (object-set p "process_material" pm)
    (object-set mesh "size" (vector2 size size))
    (object-set mat "transparency" 1)
    (object-set mat "shading_mode" 0)
    (object-set mat "vertex_color_use_as_albedo" t)
    (object-set mat "billboard_mode" 3)
    (object-set mat "billboard_keep_scale" t)
    (object-set mesh "material" mat)
    (object-set p "draw_pass_1" mesh)
    (object-set p "amount" amount)
    (object-set p "lifetime" life)
    (object-set p "preprocess" life)
    (object-set p "local_coords" t)
    (object-set p "cast_shadow" 0)
    (object-set p "visibility_aabb" (aabb (v3 (* size -4) (- size) (* size -4)) (v3 (* size 22) (* size 16) (* size 16))))
    (node-add-child parent p)
    (node-3d-set-position p (v3 x y z))
    p))

(defun flood (g x y z &key (rgb "#ffd9a0") (energy 2.8) (range 34.0))
  "Add night lighting under G at (X Y Z): a warm light at the foot of a building, off by day."
  (moving (light g rgb energy range x y z) :night))

(defun light-beam (g x y z len &key (rgb "#fff4d0") (wide 5.0) (tilt 0.1) spin sweep (speed .25))
  "Add a beam from the point (X Y Z) under G: a translucent cone of length LEN, visible at night.
TILT is the tilt up from the horizon; SPIN is the speed in radians per second of going round
(a lighthouse), SWEEP is the amplitude of swinging (a searchlight). Return the turning group."
  (let* ((turn (group g x y z)) (arm (group turn)))
    (node-3d-set-rotation arm (v3 0 0 (+ (/ pi 2) tilt)))
    (shape arm :cone (list wide len 10) rgb 0 (- (/ len 2)) 0 :alpha .2 :unshaded t :shadow nil)
    (cond (spin (moving turn :spin :y spin))
          (sweep (moving turn :swing sweep speed (* x .1) :y)))
    (moving turn :night)
    turn))

(defun red-light (g x y z &optional (r .35))
  "Add a blinking obstruction light of radius R under G at (X Y Z), for tall things."
  (moving (shape g :ball (list r 6 4) "#ff3020" x y z :emit "#ff2010" :energy 4 :shadow nil) :blink (+ 1.2 (* .6 (vr x y)))))

;;; ------------------------------------------------------------------ scenes: build and remove

(defun step-scenes ()
  (dolist (s *scenes*)
    ;; whether a scene is near is measured on the map, not along the path: from one road you see what stands by another
    (let ((near (multiple-value-bind (x z) (road-point *dist*)
                  (let ((at (or (scene-at s)
                                (setf (scene-at s) (multiple-value-list (road-point (/ (+ (scene-d0 s) (scene-d1 s)) 2)))))))
                    (< (sqrt (+ (expt (- x (first at)) 2) (expt (- z (second at)) 2)))
                       (+ *far-range* (/ (- (scene-d1 s) (scene-d0 s)) 2)))))))
      (cond ((and near (null (scene-node s)))
             (let ((*moves* '()))
               (setf (scene-node s) (with-solids (funcall (scene-build s))) (scene-moves s) *moves*))
             (return))                                                  ; one scene per frame
            ((and (not near) (scene-node s))
             (node-queue-free (scene-node s))
             (setf (scene-node s) nil (scene-moves s) nil))))))

(defun step-moves (moves)
  (dolist (m moves)
    (destructuring-bind (node kind . numbers) m
      (ecase kind
        (:spin (destructuring-bind (axis speed) numbers
                 (let ((a (* *time* speed)))
                   (node-3d-set-rotation node (ecase axis (:x (v3 a 0 0)) (:y (v3 0 a 0)) (:z (v3 0 0 a)))))))
        (:swing (destructuring-bind (amp speed phase &optional (axis :z) (base 0.0)) numbers
                  (let ((a (+ base (* amp (sin (+ phase (* *time* speed)))))))
                    (node-3d-set-rotation node (case axis (:x (v3 a 0 0)) (:y (v3 0 a 0)) (t (v3 0 0 a)))))))
        (:path (destructuring-bind (x0 y0 z0 x1 y1 z1 period phase) numbers      ; there and back between two points
                 (let* ((u (mod (+ phase (/ *time* period)) 2.0)) (k (if (> u 1) (- 2 u) u)))
                   (node-3d-set-position node (v3 (+ x0 (* k (- x1 x0))) (+ y0 (* k (- y1 y0))) (+ z0 (* k (- z1 z0))))))))
        (:night (object-set node "visible" (< (daylight (sky-now)) .45)))            ; floodlights and beams - in the dark
        (:blink (object-set node "visible" (< (mod (* *time* (first numbers)) 2.0) 1.2)))))))
