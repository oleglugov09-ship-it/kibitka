;;;; Bus stops. In the Soviet Union everyone built them however they pleased, and each is a small sculpture:
;;;; a wave, a mushroom, a tent, a bird, a rainbow, a yurt, a saucer on legs. There are seventeen kinds here
;;;; (three old ones are in doroga.lisp); BUILD-STOP decides which one stands at path coordinate D, the same
;;;; for all players.
;;;; A bus stop is assembled in group G: x+ is away from the road (the back wall is there), z runs along the
;;;; road, the floor is y 0.
;;;; The bench, the slab and the sign are shared (ADD-STOP); the canopy must not come closer to the road
;;;; than x −1.3.

(in-package #:kiosk-trip)

(defparameter *stop-rgb* '("#3f6f73" "#a8843c" "#8a4038" "#55708a" "#b0aa98" "#6a8a5a" "#c9a13a" "#7a5a7a")
  "The faded paint colours of the bus stops.")
(defun stop-rgb (d &optional (k 0)) (nth (mod (+ (floor d 11) k) (length *stop-rgb*)) *stop-rgb*))

(defun stop-wave (g d)
  "Build the wave stop in G: a concrete canopy shaped like a crest and a stepped back wall."
  (loop for i below 9 for a = (* i .42)
        do (shape g :box '(2.6 .14 .56) (stop-rgb d (mod i 2)) -.1 (+ 2.5 (* .28 (sin a))) (- (* i .5) 2.0) :rot (list (* .3 (cos a)) 0 0)))
  (dolist (sz '(-1.9 0 1.9)) (shape g :box '(.2 2.5 .3) "#85857f" .95 1.25 sz))
  (loop for i below 4 do (shape g :box (list .14 (- 2.2 (* i .4)) .9) "#85857f" 1.0 (/ (- 2.2 (* i .4)) 2) (- (* i .95) 1.4))))

(defun stop-mushroom (g d)
  "Build the mushroom stop in G: a round cap on a single leg."
  (shape g :cyl '(.22 .34 2.5 10) "#d8d4c4" .6 1.25 0)
  (shape g :cyl '(2.3 2.5 .22 16) (stop-rgb d) .3 2.6 0)
  (shape g :ball '(2.3 14 6) (stop-rgb d) .3 2.7 0 :scale '(1 .34 1))
  (loop for i below 6 for a = (* i 1.05) do (shape g :cyl '(.22 .22 .03 8) "#e8e0d0" (+ .3 (* 1.4 (cos a))) 3.28 (* 1.4 (sin a)) :shadow nil)))

(defun stop-tent (g d)
  "Build the tent stop in G: a pointed four-sided roof on posts."
  (dolist (sx '(-.9 1.0)) (dolist (sz '(-1.8 1.8)) (shape g :box '(.2 2.4 .2) "#85857f" sx 1.2 sz)))
  (shape g :cone '(3.0 2.6 4) (stop-rgb d) .05 3.7 0 :rot (list 0 (/ pi 4) 0))
  (shape g :box '(2.6 .2 4.2) "#6e6e6a" .05 2.45 0)
  (shape g :box '(.12 1.2 3.4) (stop-rgb d 2) 1.0 1.0 0)
  (shape g :ball '(.18 6 4) "#c9a13a" .05 5.1 0 :metal .6 :rough .4))

(defun stop-bird (g d)
  "Build the bird stop in G: two canopy wings spreading from a post."
  (shape g :box '(.5 3.0 .5) "#85857f" .8 1.5 0)
  (dolist (s '(-1 1))
    (shape g :box '(2.6 .14 2.6) (stop-rgb d) -.1 (+ 2.9 .0) (* s 1.35) :rot (list (* s -.32) 0 -.12))
    (shape g :box '(.14 .14 2.9) "#6e6e6a" .8 2.6 (* s 1.3) :rot (list (* s -.32) 0 0)))
  (shape g :ball '(.3 8 6) "#c9a13a" .8 3.2 0))

(defun stop-rainbow (g d)
  "Build the rainbow stop in G: three arches one inside another, each in its own colour."
  (loop for k below 3 for r = (- 2.5 (* k .42))
        do (loop for i below 9 for a = (* (+ i .5) (/ pi 9))
                 do (shape g :box (list .5 .2 (* r .37)) (stop-rgb d k) (+ .75 (* k .12)) (* r (sin a)) (* r (cos a))
                           :rot (list (- (/ pi 2) a) 0 0))))
  (shape g :box '(2.2 .14 3.4) "#6e6e6a" 0 2.3 0 :rot '(0 0 -.1)))

(defun stop-crystal (g d)
  "Build the crystal stop in G: a broken fold of three tilted slabs."
  (shape g :box '(.16 2.8 2.0) (stop-rgb d) 1.0 1.35 -1.5 :rot '(0 .35 .1))
  (shape g :box '(.16 3.2 2.0) (stop-rgb d 1) 1.1 1.55 0 :rot '(0 0 .06))
  (shape g :box '(.16 2.8 2.0) (stop-rgb d) 1.0 1.35 1.5 :rot '(0 -.35 .1))
  (shape g :box '(2.4 .14 2.2) "#6e6e6a" -.1 2.85 -1.2 :rot '(.22 0 -.16))
  (shape g :box '(2.4 .14 2.2) "#6e6e6a" -.1 2.85 1.2 :rot '(-.22 0 -.16)))

(defun stop-yurt (g d)
  "Build the yurt stop in G: a dome with a round opening and a band of ornament."
  (shape g :ball '(2.3 14 8) "#d8d4c4" .9 0 0 :scale '(.8 1.25 1))
  (shape g :cyl '(1.86 1.86 .3 16) (stop-rgb d) .9 1.5 0 :shadow nil)
  (loop for i below 10 for a = (* i .63) do (shape g :box '(.1 .3 .3) "#c9a13a" (+ .9 (* 1.88 (cos a) .8)) 1.5 (* 1.88 (sin a)) :rot (list (/ pi 4) 0 0) :shadow nil))
  (shape g :ball '(1.3 10 8) "#2a2c30" -.5 1.0 0 :scale '(.5 1.15 1) :shadow nil))

(defun stop-saucer (g d)
  "Build the saucer stop in G: a disc on three legs with a spire, the bus stop by the cosmodrome."
  (loop for i below 3 for a = (+ .5 (* i 2.09)) do (shape g :cyl '(.1 .14 2.6 6) "#8a8e92" (+ .3 (* 1.5 (cos a))) 1.3 (* 1.5 (sin a)) :rot (list (* .12 (sin a)) 0 (* -.12 (cos a))) :metal .5))
  (shape g :cyl '(2.5 2.0 .3 18) "#c4c8cc" .3 2.75 0 :metal .5 :rough .4)
  (shape g :ball '(1.1 12 6) (stop-rgb d) .3 2.9 0 :scale '(1 .5 1))
  (shape g :cone '(.12 1.6 6) "#a3262c" .3 4.2 0)
  (loop for i below 8 for a = (* i .785) do (shape g :ball '(.09 5 3) "#ffe080" (+ .3 (* 2.25 (cos a))) 2.62 (* 2.25 (sin a)) :emit "#ffc040" :energy 2 :shadow nil)))

(defun stop-hut (g d)
  "Build the carved hut stop in G: a log frame, a steep roof with a ridge, a window surround."
  (shape g :box '(.2 2.2 3.4) "#6a5038" .95 1.1 0)
  (dolist (sz '(-1.7 1.7)) (shape g :box '(1.5 2.2 .2) "#6a5038" .2 1.1 sz))
  (dolist (s '(-1 1)) (shape g :box '(2.6 .14 2.6) (stop-rgb d) .1 3.0 (* s 1.05) :rot (list (* s .75) 0 0)))
  (loop for i below 7 do (shape g :box '(.06 .3 .3) "#e8e0d0" -1.15 2.25 (- (* i .55) 1.65) :rot (list (/ pi 4) 0 0) :shadow nil))
  (shape g :box '(.08 .5 .08) "#c9a13a" .1 4.1 0)
  (shape g :box '(.1 .9 .9) "#e8e0d0" .84 1.5 0 :shadow nil) (shape g :box '(.12 .6 .6) "#2e333b" .82 1.5 0 :shadow nil))

(defun stop-mosaic-wall (g d)
  "Build the mural wall stop in G.
It is a full-height slab bearing the ornament of the town nearest to D, with a short canopy on top."
  (shape g :box '(.24 3.4 3.8) "#85857f" 1.0 1.7 0)
  (shape g :box '(1.9 .16 4.2) "#6e6e6a" .1 3.3 0 :rot '(0 0 -.12))
  (let ((tw (or (town-at d) (find-if (lambda (tw) (> (town-start tw) d)) *towns*))))
    (or (and tw (mural-quad g .86 1.75 0 -1 2.9 (town-index tw) :b))
        (shape g :box '(.04 2.2 2.6) (stop-rgb d) .86 1.7 0 :shadow nil))))

(defun stop-gate (g d)
  "Build the gate stop in G: two tall pylons with a crossbar and a star."
  (dolist (sz '(-2.0 2.0)) (shape g :box '(.6 4.4 .6) (stop-rgb d) .8 2.2 sz) (shape g :box '(.7 .3 .7) "#d8d4c4" .8 4.5 sz))
  (shape g :box '(.4 .5 4.6) "#d8d4c4" .8 3.6 0)
  (shape g :box '(2.4 .14 3.6) "#6e6e6a" -.1 2.7 0)
  (shape g :cyl '(.4 .4 .08 5) "#a3262c" .58 3.6 0 :rot (list 0 0 (/ pi 2)) :shadow nil))

(defun stop-brick (g d)
  "Build the brick hut stop in G, with a round window and a pitched roof."
  (shape g :box '(.24 2.4 3.6) "#8a4a38" 1.0 1.2 0)
  (dolist (sz '(-1.8 1.8)) (shape g :box '(1.7 2.4 .24) "#8a4a38" .2 1.2 sz))
  (shape g :box '(2.8 .16 4.4) "#4c4e52" 0 2.75 0 :rot '(0 0 -.22))
  (shape g :cyl '(.5 .5 .26 12) (stop-rgb d) 1.0 1.6 0 :rot (list 0 0 (/ pi 2)) :shadow nil)
  (loop for y from .4 to 2.2 by .45 do (shape g :box '(.02 .03 3.5) "#6a3a2c" .87 y 0 :shadow nil)))

(defun stop-zigzag (g d)
  "Build the zigzag stop in G: a folded zigzag roof on thin pipes."
  (loop for i below 6
        do (shape g :box '(2.6 .12 .86) (stop-rgb d (mod i 2)) -.1 2.8 (- (* i .74) 1.85) :rot (list (if (evenp i) .5 -.5) 0 -.08)))
  (dolist (sz '(-1.9 1.9)) (dolist (sx '(-.9 .95)) (shape g :cyl '(.06 .06 2.6 6) "#8a8e92" sx 1.3 sz :metal .5)))
  (shape g :box '(.1 1.0 3.6) "#d8d4c4" 1.0 1.6 0))

(defun stop-shell-sea (g d)
  "Build the seashell stop in G: a fan of slabs spreading up from the ground."
  (loop for i below 9 for a = (* (- i 4) .2)
        do (shape g :box '(.14 3.3 .62) (if (evenp i) (stop-rgb d) "#d8d4c4") (+ 1.0 (* .5 (- 1 (cos a)))) 1.6 (* 2.6 (sin a)) :rot (list (* a 1.0) 0 -.28))))

(defun stop-tower (g d)
  "Build the clock tower stop in G: a narrow tower next to a flat canopy."
  (shape g :box '(.9 5.0 .9) "#d8d4c4" .9 2.5 -2.0)
  (shape g :cone '(.8 1.2 4) (stop-rgb d) .9 5.6 -2.0 :rot (list 0 (/ pi 4) 0))
  (shape g :cyl '(.32 .32 .06 12) "#f0ece0" .42 4.2 -2.0 :rot (list 0 0 (/ pi 2)) :shadow nil)
  (shape g :box '(2.4 .16 3.4) "#6e6e6a" 0 2.6 .4)
  (dolist (sz '(-.9 1.9)) (shape g :box '(.18 2.6 .18) "#85857f" .9 1.3 sz))
  (shape g :box '(.12 1.3 2.6) (stop-rgb d 3) 1.0 1.1 .5))

(defparameter *stop-kinds*
  '(stop-wave stop-mushroom stop-tent stop-bird stop-rainbow stop-crystal stop-yurt stop-saucer stop-hut
    stop-mosaic-wall stop-gate stop-brick stop-zigzag stop-shell-sea stop-tower)
  "The new kinds of bus stops; together with the three old ones (box, wing, arch) they are the whole set.")

(defun build-stop (g d)
  "Build in group G the kind of bus stop that belongs at path coordinate D."
  (let ((k (mod (floor d 7) (+ 3 (length *stop-kinds*)))))
    (case k
      (0 (stop-box g d)) (1 (stop-wing g)) (2 (stop-arch g))
      (t (funcall (nth (- k 3) *stop-kinds*) g d)))))
