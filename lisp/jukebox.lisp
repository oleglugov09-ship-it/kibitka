;;;; The radiogram: one cabinet on the shelf by the window wall that is both the radio (its dial and
;;;; knob are on the front, uyut.lisp) and the record player (the turntable under the lid), with a rack
;;;; of records in sleeves next to it. The game has no sound: a record on the turntable
;;;; only turns the notes on that rise above the player.

(in-package #:kiosk-trip)

(defparameter *jukebox-at* '(1.3 2.25) "The x and z of the radiogram.")
(defparameter *crate-at* '(1.3 1.86) "The x and z of the rack of records.")
(defparameter *jukebox-y* 1.33 "The height of the shelf they stand on.")
(defparameter *disc-colors* '("#e05050" "#50b0e0" "#e0c040" "#60c070" "#c070e0" "#e08a40" "#f0f0f0" "#40d0c0"))

(defvar *jukebox* nil "The record player node (a part of the wagon).")
(defparameter *disc-names* '("placeholder-1" "placeholder-2" "placeholder-3") "The records in the rack, by the names their sleeves are looked up with.")
(defvar *discs* '() "The discs, each a list (name nil colour).")
(defvar *crate* '() "The numbers of the discs in the crate.")
(defvar *held* nil "The number of the disc in hand.")
(defvar *playing* nil "The number of the disc in the record player.")
(defvar *held-node* nil)
(defvar *slot-node* nil "The disc sticking out of the record player.")
(defvar *crate-node* nil)
(defvar *notes* '() "The notes above the record player, each a list (node age x y z).")
(defvar *note-timer* 0.0)

(defun load-discs ()
  (setf *discs* (loop for name in *disc-names*
                      for i from 0
                      collect (list name nil (nth (mod i (length *disc-colors*)) *disc-colors*)))
        *crate* (loop for i below (length *discs*) collect i)
        *held* nil *playing* nil)
  (length *discs*))

(defun disc-name (i) (first (nth i *discs*)))

(defvar *disc-titles* '()
  "What is written on the sleeves of the records that come with the game, from mods: entries of (name title).")

(defun disc-title (i)
  "Return the title on the sleeve of disc I, in the game language."
  (let ((name (disc-name i))) (tr (or (second (assoc name *disc-titles* :test #'string=)) name))))

;;; Models: record player, crate, disc

(defun disc-mesh (parent color &key (rot (list 0 0 (/ pi 2))) (x 0.0) (y 0.0) (z 0.0))
  (let ((g (group parent x y z)))
    (node-3d-set-rotation g (v3 (first rot) (second rot) (third rot)))
    (shape g :cyl '(.13 .13 .012 20) "#141418" 0 0 0 :rough .35 :metal .2)
    (shape g :cyl '(.045 .045 .016 14) color 0 0 0 :emit color :energy .25)
    (shape g :cyl '(.008 .008 .02 6) "#000000" 0 0 0)
    g))

(defun make-jukebox (cart)
  "Build under CART the shelf by the window wall, the radiogram on it and the rack of records beside it."
  (destructuring-bind (jx jz) *jukebox-at*
    ;; the shelf, the radiogram and the crate for the records are drawn in Blender (models/appliances.blend)
    (let ((f *floor*))
      (let ((g (group cart jx (+ f *jukebox-y*) jz)))
        (setf *slot-node* nil *jukebox* g *notes* '())
        (when *playing* (play-disc *playing*)))))
  (destructuring-bind (cx cz) *crate-at*
    (setf *crate-node* (group cart cx (+ *floor* *jukebox-y*) cz))
    (node-3d-set-rotation *crate-node* (v3 0 (/ pi 2) 0)))                   ; the sleeves stand in a row along the wall
  (show-discs))

(defun show-discs ()
  "Show the discs in the crate, in hand and in the record player according to the state."
  (when *crate-node*
    (loop for child across (node-get-children *crate-node*)
          when (object-call child "has_meta" "disc") do (node-queue-free child))
    (loop for i in *crate* for k from 0                                    ; a record in its sleeve: a square of coloured card
          do (let ((d (group *crate-node* (+ -.17 (* k .05)) .13 0)))
               (shape d :box '(.012 .24 .24) (third (nth i *discs*)) 0 0 0 :shadow nil)
               (shape d :cyl '(.05 .05 .014 10) "#141418" 0 0 0 :rot (list 0 0 (/ pi 2)) :shadow nil)
               (object-call d "set_meta" "disc" t))))
  (when *held-node* (node-queue-free *held-node*) (setf *held-node* nil))
  (when (and *held* *camera*)
    (setf *held-node* (disc-mesh *camera* (third (nth *held* *discs*)) :rot (list 1.2 0 .3) :x .22 :y -.2 :z -.45)))
  (when *slot-node* (node-queue-free *slot-node*) (setf *slot-node* nil))
  (let ((jb *jukebox*))
    (when (and *playing* jb)
      (setf *slot-node* (disc-mesh jb (third (nth *playing* *discs*)) :rot (list 0 0 0) :x -.02 :y .27 :z -.08)))))   ; on the turntable

(defun play-disc (i)
  (setf *playing* i)
  (when (and i *radio-on*) (set-radio nil))                                  ; one cabinet: a record takes it from the radio
  (show-discs))

(defun step-notes (delta)
  (let ((jb *jukebox*))
    (when (and *playing* jb)
      (decf *note-timer* delta)
      (when (<= *note-timer* 0)
        (setf *note-timer* (+ .5 (random .4)))
        (let ((c (nth (random (length *fairy*)) *fairy*)) (x (- (random .3) .15)) (z (- (random .3) .15)))
          (push (list (shape jb :box '(.05 .05 .01) c x .45 z :emit c :energy 2 :shadow nil :unshaded t) 0.0 x .45 z)
                *notes*))))
    (setf *notes*
          (remove-if (lambda (n)
                       (destructuring-bind (node age x y z) n
                         (let ((age (+ age delta)))
                           (setf (second n) age (fourth n) (+ y (* .35 delta)))
                           (if (> age 1.6)
                               (progn (node-queue-free node) t)
                               (progn (node-3d-set-position node (v3 (+ x (* .04 (sin (* 5 age)))) (fourth n) z)) nil)))))
                     *notes*))))

