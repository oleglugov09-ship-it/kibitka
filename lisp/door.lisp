;;;; The door in the back wall (z+), the porch behind it and the fold-down ladder. E on the door opens or
;;;; closes it (both from inside and from the porch). Also here: where one may stand (WALKABLE-P): the wagon
;;;; floor, the porch, the passage in the open door; at a gas station, while the ladder is down, the ground
;;;; around the truck; in the meta room (meta.lisp), its floor.

(in-package #:kiosk-trip)

(defparameter *door-at* '(0.0 2.85) "The x and z of the doorway.")
(defparameter *porch* '(-.95 .95 3.05 3.8) "The area where one may stand on the porch: x0 x1 z0 z1.")
(defparameter *porch-rgb* '(:deck "#3a3d40" :rail "#4c5052" :edge "#2a2c30") "The colours of the iron porch: deck, railing, edge.")

(defvar *door* nil "The door hinge node.")
(defvar *door-open* nil)
(defvar *door-k* 0.0 "The door position: 0 is closed, 1 is open.")
(defvar *ladder* nil "The hinge node of the fold-down ladder on the porch.")
(defvar *ladder-down* nil)
(defvar *ladder-k* 0.0 "The ladder position: 0 is raised, 1 is lowered to the ground.")

(defun apply-ladder ()
  (when *ladder* (node-3d-set-rotation *ladder* (v3 (- (+ .3 (* (- 1 *ladder-k*) 2.75))) 0 0))))

(defun toggle-ladder ()
  "Lower or raise the ladder (the G key).
It cannot be lowered while moving."
  (cond (*ladder-down* (setf *ladder-down* nil) (say "Лесенка поднята"))
        ((> *speed* .3) (say "На ходу лесенку не опустить. Сначала встань"))
        (t (setf *ladder-down* t) (say "Лесенка опущена"))))

(defun make-door (cart)
  "Build the door and the porch on wagon CART."
  (let* ((f *floor*) (z *half-length*)
         (hinge (group cart -.55 (+ f 1.0) (+ z .1))))
    (shape hinge :box '(1.1 2.0 .08) (kc :roof) .55 0 0)
    (shape hinge :box '(1.0 .06 .1) (kc :trim) .55 .55 0)
    (shape hinge :box '(1.0 .06 .1) (kc :trim) .55 -.55 0)
    (shape hinge :cyl '(.05 .05 .06 10) (kc :trim) .95 0 -.07 :rot (list (/ pi 2) 0 0) :metal .8 :rough .3)
    (setf *door* hinge)
    ;; the porch: a platform with railings on all sides
    (let ((p (group cart 0 0 0)))
      (destructuring-bind (x0 x1 z0 z1) *porch*
        (let ((zc (/ (+ z0 z1) 2)) (zl (+ (- z1 z0) .5)))
          (flet ((pc (k) (getf *porch-rgb* k)))
            (shape p :box (list (+ (- x1 x0) .4) .08 zl) (pc :deck) 0 (- f .04) zc :metal .5 :rough .6)
            (dolist (sx (list (- x0 .15) (+ x1 .15)))
              (shape p :box (list .05 .05 zl) (pc :rail) sx (+ f 1.0) zc :metal .5 :rough .5)
              (shape p :box (list .04 .04 zl) (pc :rail) sx (+ f .5) zc :metal .5 :rough .5)
              (loop for i to 2 do (shape p :box '(.05 1.0 .05) (pc :rail) sx (+ f .5) (+ z0 -.2 (* i (/ zl 2.2))) :metal .5 :rough .5)))
            ;; at the back the railing has a gap in the middle: the ladder goes there
            (dolist (sx '(-1 1))
              (shape p :box '(.78 .05 .05) (pc :rail) (* sx .74) (+ f 1.0) (+ z1 .2) :metal .5 :rough .5)
              (shape p :box '(.78 .04 .04) (pc :rail) (* sx .74) (+ f .5) (+ z1 .2) :metal .5 :rough .5))
            (let ((h (group p 0 (- f .1) (+ z1 .3))))
              (dolist (sx '(-.27 .27)) (shape h :box '(.05 1.05 .05) (pc :rail) sx -.52 0 :metal .5 :rough .5))
              (loop for i below 3 do (shape h :box '(.56 .04 .16) (pc :deck) 0 (- -.25 (* i .32)) 0 :metal .5 :rough .6))
              (setf *ladder* h))
            (loop for i to 3 do (shape p :box '(.05 1.0 .05) (pc :rail) (+ x0 -.15 (* i (/ (+ (- x1 x0) .3) 3))) (+ f .5) (+ z1 .2) :metal .5 :rough .5))
            (shape p :box (list (+ (- x1 x0) .4) .1 .08) (pc :edge) 0 (- f .07) (+ z1 .25)))))
      p))
  (apply-ladder)
  (apply-door))

(defun apply-door ()
  (when *door*
    (node-3d-set-rotation *door* (v3 0 (* -1.55 *door-k*) 0))))

(defun all-shut-p ()
  "Return true if the wagon is closed off from the street: the window is slid shut (for the spell)."
  (and (not *window-open*) (< *window-k* .03)))

(defun inside-cart-p ()
  "Return true if we stand in the wagon itself (not in the doorway, on the porch or in the room)."
  (< (vector3-z *pos*) 2.7))

(defun toggle-door ()
  (cond ((not *door-open*)
         (setf *door-open* t))
        ((< 2.62 (vector3-z *pos*) 3.1) (say "Ты стоишь в дверях"))   ; it can be closed both from inside and from the porch
        (t (setf *door-open* nil))))

(defun step-ladder (delta)
  (when (and *ladder-down* (> *speed* .8)) (setf *ladder-down* nil) (say "Тронулись — лесенка поднята"))
  (let ((target (if *ladder-down* 1.0 0.0)))
    (unless (< (abs (- target *ladder-k*)) .001)
      (incf *ladder-k* (* (- target *ladder-k*) (min 1 (* delta 4))))
      (apply-ladder))))

(defun step-door (delta)
  (step-ladder delta)
  (step-fridge delta)
  (let ((target (if *door-open* 1.0 0.0)))
    (unless (< (abs (- target *door-k*)) .001)
      (incf *door-k* (* (- target *door-k*) (min 1 (* delta 5))))
      (when (< (abs (- target *door-k*)) .002)
        (setf *door-k* target))
      (apply-door))))

(defun outdoors-p (x z)
  "Return true if point (X Z) is on the ground outside (neither the wagon floor nor the porch)."
  (or (> (abs x) 1.75) (> z 3.85) (< z -3.0)))

(defun solid-here-p (x z)
  "Return true if point (X Z), relative to the wagon, is inside one of the map's solids (tverd.lisp)."
  (multiple-value-bind (wx wz) (cart-world x z) (solid-at-p wx wz)))

(defun terrain-ok-p (x z)
  "Return true if the terrain lets us step to point (X Z), relative to the wagon.
Far from the wagon: not into water, not off a cliff and not onto a slope wall. Right by the wagon the
land is flat, so we do not ask there. If you already stand in water (the wagon stopped by the shore),
you are allowed to walk out."
  (or (and (<= (abs x) 8.0) (<= (abs z) 16.0))
      (let ((px (vector3-x *pos*)) (pz (vector3-z *pos*)))
        (multiple-value-bind (wx wz) (cart-world x z)
          (multiple-value-bind (cx cz) (cart-world px pz)
            (and (or (not (deep-at-p wx wz)) (deep-at-p cx cz))
                 (let* ((dx (- x px)) (dz (- z pz)) (len (sqrt (+ (* dx dx) (* dz dz)))))
                   (or (< len 1e-4)                                          ; a 60 cm step forward must be no steeper than 55°
                       (multiple-value-bind (ax az) (cart-world (+ px (* .6 (/ dx len))) (+ pz (* .6 (/ dz len))))
                         (< (- (land-height ax az) (land-height cx cz)) .85))))))))))

(defun ground-walkable-p (x z)
  "Return true if one may walk at point (X Z) on the ground outside the wagon.
Outside we walk anywhere, as long as the wagon is parked with the ladder down; there is no passing
through the truck itself, walls, posts and trees, nor into water or onto a sheer slope. If you already
stand inside a solid (the wagon pulled up right against it), you are allowed to walk out. The way down
is the ladder behind the porch."
  (and *ladder-down* (> *ladder-k* .8)
       (or (and (< (abs x) .5) (<= 3.8 z 4.7))                                   ; the ladder
           (and (not (and (< (abs x) 2.0) (< -8.5 z 4.0)))                       ; the truck itself
                (or (not (solid-here-p x z))
                    (solid-here-p (vector3-x *pos*) (vector3-z *pos*)))
                (terrain-ok-p x z)))))                         ; the truck itself

(defun ground-rise (x z)
  "Return how far the walking surface at point (X Z) from the wagon lies above the road under the wagon.
On the roadway (and on a bridge) it is the road; in a town the sidewalks and squares lie level with
it, so that is road too, only wider; off to the side it is the land. Between them the height changes
smoothly, over a couple of metres: no step at the kerb."
  (multiple-value-bind (wx wz) (cart-world x z)
    (let ((near (first (sort (roads-near wx wz) #'< :key #'third))) (land (land-height wx wz)))
      (- (if near
             (destructuring-bind (d lat &rest more) near
               (declare (ignore more))
               (let* ((half (if (town-at d) 14.0 5.3))                    ; half-width of what lies level with the road
                      (k (max 0.0 (min 1.0 (/ (- (+ half 2.0) (abs (+ lat 2.25))) 2.0)))))
                 (+ (* k (road-y d)) (* (- 1 k) land))))
             land)
         (road-y *dist*)))))

(defun anyone-afoot-p ()
  "Return true if anyone (we or another player) is walking on the ground outside right now."
  (and (not *meta*)
       (or (outdoors-p (vector3-x *pos*) (vector3-z *pos*))
           (some (lambda (o) (and (sixth o) (outdoors-p (third o) (fourth o)))) *net-others*))))                   ; the truck itself

(defun walkable-p (x z)
  "Return true if one may stand at point (X Z).
That is the wagon floor, the porch, the doorway (if the door is open), at a gas station the ground
around; in the meta room, its floor."
  (flet ((in (rect) (destructuring-bind (x0 x1 z0 z1) rect (and (<= x0 x x1) (<= z0 z z1)))))
    (if *meta*
        (meta-walkable-p x z)
        (or (in *bounds*)
            (in *porch*)
            (and (> *door-k* .6) (in '(-.4 .4 2.5 3.1)))
            (ground-walkable-p x z)))))
