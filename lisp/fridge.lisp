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

(defun apply-fridge ()
  (when *fridge-door* (node-3d-set-rotation *fridge-door* (v3 0 (if *fridge-open* 1.75 0.0) 0))))

(defun make-fridge (c)
  "Build the fridge under C: a white enamel box open towards the room, two shelves, a door on a hinge."
  (destructuring-bind (fx fz) *fridge-at*
    (destructuring-bind (w h) *fridge-size*
      (let* ((g (group c fx *floor* fz)) (half (/ w 2)) (white "#e8e6dc")
             (hinge (group g half 0 (- half))))
        (shape g :box (list .03 h w) white (- .015 half) (/ h 2) 0)                        ; the back
        (dolist (sz (list (- .015 half) (- half .015))) (shape g :box (list w h .03) white 0 (/ h 2) sz))
        (shape g :box (list w .03 w) white 0 (- h .015) 0)
        (shape g :box (list w (first *fridge-shelves*) w) white 0 (/ (first *fridge-shelves*) 2) 0)
        (shape g :box (list (- w .08) .02 (- w .08)) "#c8ccd0" 0 (- (second *fridge-shelves*) .01) 0 :metal .5 :rough .4 :shadow nil)
        (shape g :box (list (- w .1) .18 (- w .08)) "#d0d8e0" -.02 (- h .13) 0 :shadow nil)   ; the freezer box under the top
        (shape hinge :box (list .04 (- h .04) w) white .02 (/ h 2) half)                   ; the door
        (shape hinge :box '(.02 .22 .03) "#b0b4b8" .05 (* h .62) (- w .06) :metal .7 :rough .3 :shadow nil)
        (shape hinge :box '(.006 .05 .2) "#3f6f73" .042 (* h .86) half :shadow nil)        ; the name plate
        (setf *fridge-door* hinge)
        (apply-fridge)))))

(defun in-fridge-p (spec)
  "Return true if the item record SPEC stands inside the fridge."
  (destructuring-bind (fx fz) *fridge-at*
    (let ((half (/ (first *fridge-size*) 2)))
      (and (< (abs (- (second spec) fx)) half) (< (abs (- (fourth spec) fz)) half)
           (< (third spec) (- (second *fridge-size*) .05))))))

(defun fridge-surfaces ()
  "Return the shelves of the fridge as (x z width depth height) entries; none while the door is shut."
  (when *fridge-open*
    (destructuring-bind (fx fz) *fridge-at*
      (let ((inner (- (first *fridge-size*) .1)))
        (loop for top in *fridge-shelves* collect (list fx fz inner inner top))))))

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
  "At every dawn throw away the milk and fish that stood outside the fridge; the driver counts."
  (unless (net-guest-p)
    (let ((day (today)))
      (cond ((or (null *spoil-day*) (< day *spoil-day*)) (setf *spoil-day* day))      ; the first look, or the clock was turned back
            ((> day *spoil-day*)
             (setf *spoil-day* day)
             (let ((bad (remove-if-not (lambda (spec) (and (member (first spec) *perishable*) (not (in-fridge-p spec))))
                                       (mapcar #'car *items*))))
               (when bad
                 (set-items (remove-if (lambda (spec) (member spec bad :test #'equal)) (mapcar #'car *items*)))
                 (save-interior)
                 (log-event :spoiled :n (length bad) :dist (round *dist*))
                 (say-all (format nil "За ночь испортилось: ~d шт. Молоко и рыбу на ночь убирай в холодильник — он у стены с ковром"
                                  (length bad))))))))))

(defcommon :fridge :value (and *fridge-open* t) :spec '(:member t nil)
  :accept (lambda (open) (unless (eq open (and *fridge-open* t)) (setf *fridge-open* open) (apply-fridge))))
