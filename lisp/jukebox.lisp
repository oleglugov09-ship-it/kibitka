;;;; The record player: a small box on the bottom shelf of the rack on the left, with a crate of
;;;; discs next to it. The discs are every .wav and .mp3 in project/discs/ (your own go there too;
;;;; reread them with (live:discs)). While it plays, notes rise above the player.

(in-package #:kiosk-trip)

(defparameter *jukebox-at* '(-1.12 -2.5) "The x and z of the record player.")
(defparameter *crate-at* '(-.6 -2.5) "The x and z of the crate of discs.")
(defparameter *jukebox-y* .55 "The height of the shelf they stand on.")
(defparameter *jukebox-scale* .6 "The scale of the record player, which is compact: a 36 cm box.")
(defparameter *disc-colors* '("#e05050" "#50b0e0" "#e0c040" "#60c070" "#c070e0" "#e08a40" "#f0f0f0" "#40d0c0"))

(defvar *jukebox* nil "The record player node (a part of the wagon).")
(defvar *music* nil "The AudioStreamPlayer3D of the record player.")
(defvar *discs* '() "The discs, each a list (name path colour).")
(defvar *crate* '() "The numbers of the discs in the crate.")
(defvar *held* nil "The number of the disc in hand.")
(defvar *playing* nil "The number of the disc in the record player.")
(defvar *held-node* nil)
(defvar *slot-node* nil "The disc sticking out of the record player.")
(defvar *crate-node* nil)
(defvar *notes* '() "The notes above the record player, each a list (node age x y z).")
(defvar *note-timer* 0.0)

(defun load-discs ()
  (let ((files (sort (remove-if-not (lambda (f) (or (path-type-p f ".wav") (path-type-p f ".mp3")))
                                    (coerce (dir-access-get-files-at "res://discs") 'list))
                     #'string<)))
    (setf *discs*
          (loop for file in files
                for i from 0
                collect (list (subseq file 0 (- (length file) 4)) (format nil "res://discs/~a" file)
                              (nth (mod i (length *disc-colors*)) *disc-colors*)))
          *crate* (loop for i below (length *discs*) collect i)
          *held* nil *playing* nil)
    (length *discs*)))

(defun disc-name (i) (first (nth i *discs*)))

;;; Models: record player, crate, disc

(defun disc-mesh (parent color &key (rot (list 0 0 (/ pi 2))) (x 0.0) (y 0.0) (z 0.0))
  (let ((g (group parent x y z)))
    (node-3d-set-rotation g (v3 (first rot) (second rot) (third rot)))
    (shape g :cyl '(.13 .13 .012 20) "#141418" 0 0 0 :rough .35 :metal .2)
    (shape g :cyl '(.045 .045 .016 14) color 0 0 0 :emit color :energy .25)
    (shape g :cyl '(.008 .008 .02 6) "#000000" 0 0 0)
    g))

(defun make-jukebox (cart)
  "Build the record player box and the crate of discs in the front left corner of CART."
  (destructuring-bind (jx jz) *jukebox-at*
    (let ((g (group cart jx (+ *floor* *jukebox-y*) jz)))
      (node-3d-set-scale g (v3 *jukebox-scale* *jukebox-scale* *jukebox-scale*))
      (shape g :box '(.6 .6 .6) "#5a3a22" 0 .3 0)
      (dolist (c '((-1 -1) (-1 1) (1 -1) (1 1)))           ; lighter edges, as in Minecraft
        (shape g :box '(.08 .62 .08) "#8a5a34" (* (first c) .27) .3 (* (second c) .27)))
      (shape g :box '(.62 .06 .62) "#7a4e2c" 0 .6 0)
      (shape g :box '(.42 .02 .07) "#120c0a" 0 .635 0)       ; the disc slot
      (dolist (s '(-1 1))
        (shape g :box '(.36 .36 .01) "#6a4428" 0 .3 (* s .306)))
      (setf *slot-node* nil *jukebox* g *notes* '()
            *music* (player g (wav-stream (sound-path "step-1")) :positional t :y .5 :db -14.0 :bus "Музыка" :play nil))
      (when *playing* (play-disc *playing*))))
  (destructuring-bind (cx cz) *crate-at*
    (setf *crate-node* (group cart cx (+ *floor* *jukebox-y*) cz))
    (node-3d-set-scale *crate-node* (v3 .8 .8 .8))
    (let ((g *crate-node*))
      (shape g :box '(.5 .04 .32) "#7a5a3a" 0 .02 0)
      (dolist (s '(-1 1))
        (shape g :box '(.5 .22 .03) "#7a5a3a" 0 .11 (* s .15))
        (shape g :box '(.03 .22 .32) "#7a5a3a" (* s .235) .11 0))))
  (show-discs))

(defun show-discs ()
  "Show the discs in the crate, in hand and in the record player according to the state."
  (when *crate-node*
    (loop for child across (node-get-children *crate-node*)
          when (object-call child "has_meta" "disc") do (node-queue-free child))
    (loop for i in *crate* for k from 0
          do (let ((d (disc-mesh *crate-node* (third (nth i *discs*)) :x (+ -.19 (* k .055)) :y .17)))
               (object-call d "set_meta" "disc" t))))
  (when *held-node* (node-queue-free *held-node*) (setf *held-node* nil))
  (when (and *held* *camera*)
    (setf *held-node* (disc-mesh *camera* (third (nth *held* *discs*)) :rot (list 1.2 0 .3) :x .22 :y -.2 :z -.45)))
  (when *slot-node* (node-queue-free *slot-node*) (setf *slot-node* nil))
  (let ((jb *jukebox*))
    (when (and *playing* jb)
      (setf *slot-node* (disc-mesh jb (third (nth *playing* *discs*)) :rot (list (/ pi 2) 0 0) :y .66)))))

(defun play-disc (i)
  (let ((m *music*))
    (setf *playing* i)
    (if i
        (progn (object-set m "stream" (audio-stream (second (nth i *discs*)) :loop t)) (object-call m "play"))
        (object-call m "stop"))
    (show-discs)))

(defun step-notes (delta)
  (let ((jb *jukebox*))
    (when (and *playing* jb)
      (decf *note-timer* delta)
      (when (<= *note-timer* 0)
        (setf *note-timer* (+ .5 (random .4)))
        (let ((c (nth (random (length *fairy*)) *fairy*)) (x (- (random .3) .15)) (z (- (random .3) .15)))
          (push (list (shape jb :box '(.05 .05 .01) c x .7 z :emit c :energy 2 :shadow nil :unshaded t) 0.0 x .7 z)
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

