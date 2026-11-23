;;;; The fridge and what it is for. Milk and fish go bad overnight: at every dawn whatever of them stands in
;;;; the wagon outside the fridge is thrown away (the driver counts, and tells everyone). The fridge stands by
;;;; the carpet wall between the bunks and the stove: E opens and shuts its door, and while it is open its
;;;; two shelves take things like any other shelf. A thing is 'in the fridge' simply by where it stands, so
;;;; nothing new is saved or sent for it; only the door is a common thing (DEFCOMMON).

(in-package #:kiosk-trip)

(defparameter *fridge-at* '(-1.19 .65) "The x and z of the middle of the fridge.")
(defparameter *fridge-size* '(.56 1.1) "The width (and depth) and the height of the fridge.")
(defparameter *fridge-shelves* '(.06 .57) "The heights of the two shelves inside, above the floor.")

(defvar *fridge-open* nil "True while the fridge door is open.")
(defvar *fridge-door* nil "The hinge node of the fridge door.")
(defvar *spoil-day* nil "The game day up to which spoiling has been counted, or NIL before the first check.")

(defvar *fridge-k* 0.0 "How far the fridge door is open right now: 0 shut, 1 open; it follows *FRIDGE-OPEN*.")

(defun apply-fridge ()
  (when *fridge-door* (node-3d-set-rotation *fridge-door* (v3 0 (* 1.75 *fridge-k*) 0))))

(defun step-fridge (delta)
  "Swing the fridge door towards where it should be, like the wagon door (STEP-DOOR)."
  (let ((target (if *fridge-open* 1.0 0.0)))
    (unless (< (abs (- target *fridge-k*)) .001)
      (incf *fridge-k* (* (- target *fridge-k*) (min 1 (* delta 6))))
      (when (< (abs (- target *fridge-k*)) .002) (setf *fridge-k* target))
      (apply-fridge))))

(defun make-fridge (c)
  "Build the fridge under C: a white enamel box open towards the room, two shelves, a door on a hinge."
  (destructuring-bind (fx fz) *fridge-at*
    (destructuring-bind (w h) *fridge-size*
      (let* ((g (group c fx *floor* fz)) (half (/ w 2)) (white "#e8e6dc")
             (hinge (group g half 0 (- half))))
        ;; the box, its shelves and the dryer over the stove are drawn in Blender (models/appliances.blend), the door in fridge-door.blend
        (model c "appliances")
        (model hinge "fridge-door")
        (let ((name (sign hinge "" .048 (* h .86) half :h .07 :rgb "#c8ccd0" :yaw (/ pi 2))))
          (godot:object-set name "text" "ЗИЛ"))
        (setf *fridge-door* hinge)
        (apply-fridge)))))

(defun in-fridge-p (spec)
  "Return true if the item record SPEC stands inside the fridge."
  (destructuring-bind (fx fz) *fridge-at*
    (let ((half (/ (first *fridge-size*) 2)))
      (and (< (abs (- (spec-x spec) fx)) half) (< (abs (- (spec-z spec) fz)) half)
           (< (spec-y spec) (- (second *fridge-size*) .05))))))

(defun fridge-surfaces ()
  "Return the shelves of the fridge as (x z width depth height) entries.
They hold things whether the door is open or shut; putting things in asks for an open door (PUT-SPOT)."
  (destructuring-bind (fx fz) *fridge-at*
    (let ((inner (- (first *fridge-size*) .1)))
      (loop for top in *fridge-shelves* collect (list fx fz inner inner top)))))

(defun fridge-shelf (x z top)
  "Return the height of the fridge shelf that the point (X Z TOP) lies on, or NIL."
  (destructuring-bind (fx fz) *fridge-at*
    (let ((half (/ (first *fridge-size*) 2)))
      (and (< (abs (- x fx)) half) (< (abs (- z fz)) half)
           ;; the shelf, the top of what stands on it, or the floor of the fridge under the lower shelf: all of the
           ;; footprint belongs to the shelves, so nothing is put 'into the fridge' past them
           (or (find-if (lambda (level) (< -.04 (- top level) .45)) (reverse *fridge-shelves*))
               (first *fridge-shelves*))))))

(defun fridge-slot (x z level kind)
  "Return the free place on the fridge shelf LEVEL nearest to (X Z) for a thing of KIND, as (X Z), or NIL if the shelf is full.
A shelf has four places, two by two, like a section of the rack in small; things that stack go on their own kind."
  (destructuring-bind (fx fz) *fridge-at*
    (flet ((taken-p (place)
             (and (not (stack-room-p kind (first place) (second place) level))
                  (some (lambda (it) (destructuring-bind (ikind ix iy iz &rest more) (car it)
                                       (declare (ignore ikind more))
                                       (and (< -.05 (- iy level) .45) (< (abs (- ix (first place))) .16) (< (abs (- iz (second place))) .16))))
                        *items*))))
      (first (sort (remove-if #'taken-p (loop for dx in '(-.11 .11) append (loop for dz in '(-.11 .11) collect (list (+ fx dx) (+ fz dz)))))
                   #'< :key (lambda (place) (+ (expt (- (first place) x) 2) (expt (- (second place) z) 2))))))))

;;; The dryer: a slatted shelf on brackets above the stove. Mushrooms, apples and herbs left on it overnight
;;; are dried by the morning (the recipes with the hearth :LINE): they keep for ever and sell as a dish.

(defun dried-of (kind)
  "Return what KIND turns into on the dryer overnight, or NIL."
  (second (find-if (lambda (r) (and (eq (first r) kind) (eq (fifth r) :line))) *recipes*)))

(defparameter *dryer-top* 1.72 "The height of the dryer above the floor.")

(defun dryer-surface ()
  "Return the dryer as (x z width depth height)."
  (or *dryer-surface*                                                     ; from the appliances model (mesta.lisp)
      (destructuring-bind (sx sz) *stove-at*
        (list (- sx .02) (- sz .45) .5 1.2 *dryer-top*))))                      ; long enough for four things in a row

(defun on-dryer-p (spec)
  (destructuring-bind (cx cz w d top) (dryer-surface)
    (and (< (abs (- (spec-x spec) cx)) (/ w 2)) (< (abs (- (spec-z spec) cz)) (/ d 2)) (< (abs (- (spec-y spec) top)) .1))))

(defun toggle-fridge ()
  (setf *fridge-open* (not *fridge-open*))
  (apply-fridge))

(defun fridge-usables ()
  "Return the thing to press on the fridge: its door, wherever it is now."
  (destructuring-bind (fx fz) *fridge-at*
    (let ((half (/ (first *fridge-size*) 2)) (y (* (second *fridge-size*) .62)))
      (list (if *fridge-open*
                (usable (+ fx half .26) y (- fz half .05) (lambda () "E — закрыть холодильник") #'toggle-fridge nil 2.6 .28)
                (usable (+ fx half .04) y fz (lambda () "E — открыть холодильник") #'toggle-fridge nil 2.6 .3))))))

(defun step-spoil ()
  "At every dawn throw away what goes bad and stood outside the fridge; the driver counts.
Mushrooms on the dryer are dried instead."
  (unless (net-guest-p)
    (let ((day (today)))
      (on-new-day (*spoil-day* day)
             (let* ((dry (remove-if-not (lambda (spec) (and (dried-of (spec-kind spec)) (on-dryer-p spec))) (mapcar #'car *items*)))
                    (bad (remove-if-not (lambda (spec) (and (trait (spec-kind spec) :spoils) (not (in-fridge-p spec))
                                                            (not (and (on-dryer-p spec) (dried-of (spec-kind spec))))))      ; the dryer saves only what dries
                                        (mapcar #'car *items*))))
               (when dry
                 (set-items (mapcar (lambda (spec) (if (member spec dry :test #'equal) (list* (dried-of (spec-kind spec)) (rest spec)) spec)) (mapcar #'car *items*)))
                 (save-interior)
                 (say-all "На сушилке высохло: теперь не испортится"))
               (when (and bad (boundp '*trip-day0*) (= day (1+ (symbol-value '*trip-day0*))))   ; the first dawn of a trip forgives: a warning, nothing lost
                 (setf bad nil)
                 (log-event :spoil-warned :dist (round *dist*))
                 (say-all "Утро. Продукты на полке чуть не испортились — на этот раз обошлось. Молоко, рыбу и грибы на ночь убирай в холодильник — он у стены с ковром"))
               (when bad
                 (set-items (remove-if (lambda (spec) (member spec bad :test #'equal)) (mapcar #'car *items*)))
                 (save-interior)
                 (log-event :spoiled :n (length bad) :dist (round *dist*))
                 (say-all (format nil "За ночь испортилось: ~d шт. Молоко, рыбу и грибы на ночь убирай в холодильник — он у стены с ковром"
                                  (length bad)))))))))

(defstate :fridge :clear (lambda () (setf *fridge-open* nil *spoil-day* nil)) :show (lambda () (apply-fridge)))

(defcommon :fridge :value (and *fridge-open* t) :spec '(:member t nil)
  :accept (lambda (open) (unless (eq open (and *fridge-open* t)) (setf *fridge-open* open) (apply-fridge))))
