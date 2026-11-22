;;;; Roadside buildings between towns: landmarks (*LANDMARK-PLAN*), the bridge over the gorge, the tunnel vault,
;;;; diners. Each is a function that assembles the thing from shapes in a group already standing in its place.

(in-package #:kiosk-trip)

;;; ------------------------------------------------------------------ landmarks

(defun build-landmark (kind d side)
  (if (eq kind :pylons)
      (let ((g (group *world*))) (mark-pylons g d) g)
      (destructuring-bind (x r y m) (getf *landmark-kinds* kind)
        (declare (ignore r y m))
        (let ((g (place-at d (* side x) -.05 side)))
          (ecase kind
            (:windmill (mark-windmill g)) (:plane (mark-plane g)) (:church (mark-church g)) (:pumpjacks (mark-pumpjacks g))
            (:mast (mark-mast g)) (:elevator (mark-elevator g)) (:wheel (mark-ferris-wheel g)) (:watchtower (mark-watchtower g))
            (:oldbus (mark-old-bus g)) (:radar (mark-radar g)) (:farm (mark-farm g)) (:rocket (mark-rocket g))
            (:camp (mark-camp g)))                                   ; the pioneer camp: life.lisp
          g))))

(defun mark-windmill (g)
  "Build in group G a wooden windmill whose sails turn."
  (shape g :cyl '(2.1 3.3 9.5 8) "#6a5038" 0 4.75 0)
  (shape g :cone '(2.9 2.8 8) "#4a3a2a" 0 10.9 0)
  (shape g :box '(.12 2.0 1.2) "#3a2a1e" -3.0 1.0 0 :shadow nil)
  (shape g :box '(1.2 .3 .3) "#3a2a1e" -2.7 9.0 0)
  (let ((hub (group g -3.4 9.0 0)))
    (dotimes (i 4)
      (let ((blade (group hub)))
        (node-3d-set-rotation blade (v3 (* i (/ pi 2)) 0 0))
        (shape blade :box '(.1 7.6 .16) "#4a3a2a" 0 3.8 0)
        (shape blade :box '(.06 5.6 1.3) "#c8b890" 0 4.6 .7)))
    (moving hub :spin :x .45)))

(defun mark-plane (g)
  "Build in group G an aeroplane on a pedestal."
  (shape g :box '(1.6 4.4 1.6) "#85857f" 0 2.2 0)
  (shape g :box '(3.2 .5 3.2) "#6e6e6a" 0 .25 0)
  (let ((p (group g 0 6.2 0)) (silver "#b4b8bc"))
    (node-3d-set-rotation p (v3 -.3 0 .12))
    (shape p :cyl '(.55 .75 9.0 10) silver 0 0 0 :rot (list (/ pi 2) 0 0) :metal .5 :rough .4)
    (shape p :cone '(.55 1.8 10) silver 0 0 -5.4 :rot (list (- (/ pi 2)) 0 0) :metal .5 :rough .4)
    (shape p :ball '(.5 8 5) "#2a3a4a" 0 .45 -2.6 :scale '(.9 .7 2.0) :rough .2)
    (dolist (s '(-1 1))
      (shape p :box '(4.4 .12 1.9) silver (* s 2.4) -.1 1.0 :rot (list 0 (* s -.5) 0) :metal .5 :rough .4)
      (shape p :box '(1.7 .1 .9) silver (* s 1.0) .1 4.2 :rot (list 0 (* s -.4) 0) :metal .5 :rough .4))
    (shape p :box '(.12 2.0 1.6) silver 0 1.1 3.9 :rot (list -.5 0 0) :metal .5 :rough .4)
    (shape p :box '(.14 .5 .5) "#a3262c" 0 1.4 3.9 :shadow nil)))

(defun mark-church (g)
  "Build in group G a white church on a hill: a dome and a bell tower with a tented roof."
  (shape g :box '(7.0 8.0 10.0) "#e4e0d4" 0 4.0 0)
  (shape g :cyl '(3.0 3.0 6.0 8) "#e4e0d4" 0 3.0 5.6)
  (shape g :box '(7.6 .4 10.6) "#5a6a60" 0 8.2 0 :shadow nil)
  (shape g :cyl '(1.7 1.7 3.2 10) "#e4e0d4" 0 9.9 0)
  (shape g :ball '(2.2 12 8) "#2c4a7a" 0 12.6 0 :scale '(1 1.25 1) :rough .4)
  (shape g :box '(.12 2.2 .12) "#c9a13a" 0 16.2 0 :shadow nil) (shape g :box '(.9 .12 .12) "#c9a13a" 0 16.6 0 :shadow nil)
  (shape g :box '(3.6 15.0 3.6) "#e4e0d4" 0 7.5 -7.2)
  (shape g :cone '(3.0 5.6 8) "#3c5a48" 0 17.8 -7.2)
  (shape g :ball '(.8 8 6) "#c9a13a" 0 21.0 -7.2 :scale '(1 1.25 1) :metal .6 :rough .3)
  (shape g :box '(.1 1.6 .1) "#c9a13a" 0 22.6 -7.2 :shadow nil) (shape g :box '(.7 .1 .1) "#c9a13a" 0 22.9 -7.2 :shadow nil)
  (dolist (z '(-2.5 0 2.5))
    (dolist (s '(-1 1))
      (shape g :box '(.14 2.2 .9) (if (zerop z) "#e8c070" "#2e333b") (* s 3.5) 4.6 z
             :emit (when (zerop z) "#ffc060") :energy .9 :shadow nil)))
  (shape g :box '(.14 2.0 2.0) "#1a1a1e" 0 12.4 -9.0 :shadow nil)
  ;; the fence and the birches
  (loop for i below 9 do (shape g :box '(.12 1.1 .12) "#4a4a4c" -9.0 .55 (- (* i 3.0) 12)))
  (shape g :box '(.08 .08 24.0) "#4a4a4c" -9.0 1.0 0 :shadow nil)
  (flood g -7 2.0 -3 :range 36.0 :energy 3.2))

(defun mark-pumpjacks (g)
  "Build in group G oil pumpjacks that nod."
  (loop for z in '(-13 0 13) for i from 0
        do (let ((u (group g (* i 3.0) 0 z)))
             (shape u :box '(6.0 .3 2.0) "#55554f" 0 .15 0)
             (dolist (s '(-1 1)) (shape u :box '(.25 4.6 .25) "#3c4638" (* s .9) 2.2 0 :rot (list 0 0 (* s .35))))
             (shape u :box '(1.4 1.2 1.4) "#6a3a2e" 2.4 .9 0)
             (let ((beam (group u 0 4.3 0)))
               (shape beam :box '(6.4 .4 .3) "#a8843c" 0 0 0)
               (shape beam :box '(.7 1.9 .5) "#a8843c" -3.3 -.5 0)
               (shape beam :box '(.08 3.4 .08) "#16161a" -3.3 -2.6 0 :shadow nil)
               (shape beam :box '(1.2 .9 .8) "#3a3a40" 3.0 -.3 0)
               (moving beam :swing .3 (+ .9 (* i .12)) (* i 1.7)))))
  (shape g :cyl '(2.6 2.6 4.0 12) "#8a8580" 14 2.0 -6)
  (shape g :cyl '(2.6 2.6 4.0 12) "#7a7570" 14 2.0 2))

(defun mark-mast (g)
  "Build in group G a guyed radio mast: red and white, with blinking lights."
  (dotimes (i 6) (shape g :box '(.7 20.0 .7) (if (evenp i) "#b0342c" "#e0dcd0") 0 (+ 10 (* i 20)) 0))
  (shape g :box '(3.0 2.6 4.0) "#84807a" 3.5 1.3 0)
  (dolist (h '(118 78))
    (dotimes (i 3)
      (let ((a (* i (/ (* 2 pi) 3))) (out (* h .52)))
        (beam-between g (list 0 h 0) (list (* out (cos a)) -1.0 (* out (sin a))) :thick .07 :rgb "#3a3a40"))))
  (red-light g 0 120.6 0 .6) (red-light g .5 80 0) (red-light g .5 40 0))

(defun mark-elevator (g)
  "Build in group G a grain elevator: a row of silos, a gallery on top, a working tower."
  (dotimes (i 5) (shape g :cyl '(3.3 3.3 24.0 12) (if (evenp i) "#b8b4a8" "#aeaa9e") 0 12.0 (- (* i 6.8) 10)))
  (shape g :box '(4.6 4.0 36.0) "#9a968a" 0 26.0 3.6)
  (shape g :box '(7.5 38.0 7.5) "#a8a498" 0 19.0 -19.0)
  (shape g :box '(.14 1.0 5.0) "#2e333b" -3.8 34.0 -19.0 :shadow nil)
  (shape g :box '(5.0 5.0 9.0) "#84807a" -6.0 2.5 8.0)
  (beam-between g '(-6.0 5.0 8.0) '(-1.0 22.0 3.0) :thick .9 :rgb "#7b7d82")
  (sign g "ЭЛЕВАТОР" -3.9 30.5 -19.0 :h 1.6 :rgb "#8a4038")
  (flood g -9 5.0 0 :range 52.0 :rgb "#fff0d0")
  (red-light g 0 38.6 -19.0))

(defun mark-ferris-wheel (g)
  "Build in group G an old Ferris wheel that creaks and still turns."
  (let ((r 16.0) (hub-y 19.0))
    (dolist (sx '(-1.4 1.4))
      (dolist (sz '(-1 1))
        (beam-between g (list sx hub-y 0) (list sx -1.0 (* sz 9.0)) :thick .45 :rgb "#5a6e78")))
    (shape g :cyl '(.5 .5 3.4 8) "#3a3a40" 0 hub-y 0 :rot (list 0 0 (/ pi 2)))
    (let ((w (group g 0 hub-y 0)))
      (dotimes (i 16)
        (let ((a (* i (/ pi 8))))
          (shape w :box (list .28 .28 (* 2.03 r (sin (/ pi 16)))) "#c9a13a" 0 (* r (sin (+ a (/ pi 16)))) (* r (cos (+ a (/ pi 16))))
                 :rot (list (- (/ pi 2) (+ a (/ pi 16))) 0 0) :shadow nil)))
      (dotimes (i 8)
        (shape w :box (list .14 (* 2 r) .14) "#8a8e92" 0 0 0 :rot (list (* i (/ pi 8)) 0 0) :shadow nil))
      (dotimes (i 32)                                                   ; bulbs along the rim: at night the wheel is lit
        (let ((a (* i (/ pi 16))) (c (nth (mod i 5) *fairy*)))
          (shape w :ball '(.5 6 4) c .3 (* r (sin a)) (* r (cos a)) :emit c :energy 6 :shadow nil)))
      (dotimes (i 8)
        (let ((a (* i (/ pi 4))))
          (shape w :box '(1.7 1.5 1.5) (nth (mod i 4) '("#a3262c" "#2c4a7a" "#c9a13a" "#3f6f73"))
                 0 (* r (sin a)) (* r (cos a)))))
      (moving w :spin :x .07))
    (shape g :box '(3.0 2.6 3.0) "#6a5a48" -6.0 1.3 6.0)
    (sign g "ПАРК" -7.55 2.0 6.0 :h .8 :rgb "#e8e0d0")
    (flood g -8 2.0 0 :range 44.0 :rgb "#ffd0f0")))

(defun mark-pylons (g d)
  "Build a high-voltage line across the road at path coordinate D.
The pylons and wires are in World coordinates."
  (let ((tops '()))
    (loop for x in '(-330 -190 -60 70 200 340)
          do (let ((dd (+ d (* x .28))))
               (destructuring-bind (px py pz) (side-point x dd 0.0)
                 (let ((p (group g px py pz)) (yaw (- .28 (road-frame dd))))
                   (node-3d-set-rotation p (v3 0 yaw 0))
                   (dolist (sx '(-1 1))
                     (dolist (sz '(-1 1))
                       (beam-between p (list (* sx 3.2) -4.0 (* sz 3.2)) (list (* sx .6) 22.0 (* sz .6)) :thick .22 :rgb "#8a8e90")))
                   (shape p :box '(1.2 9.0 1.2) "#8a8e90" 0 26.5 0 :shadow nil)
                   (loop for (w y) in '((15.0 22.0) (11.0 26.5) (7.0 30.5)) do (shape p :box (list .3 .3 w) "#8a8e90" 0 y 0 :shadow nil))
                   (loop for y from 3 to 19 by 4 do (shape p :box (list .12 .12 (- 6.0 (* y .24))) "#8a8e90" 0 y 0 :shadow nil))
                   (push (loop for (w y) in '((15.0 22.0) (11.0 26.5) (7.0 30.5))
                               append (loop for s in '(-1 1)
                                            collect (list (+ px (* s (/ w 2) (sin yaw))) (+ py y -.4) (+ pz (* s (/ w 2) (cos yaw))))))
                         tops)))))
    (loop for (a b) on tops while b
          do (loop for p in a for q in b do (beam-between g p q :thick .06 :sag 5.0)))))

(defun mark-watchtower (g)
  "Build in group G the forester's fire watchtower and his hut, with smoke from the chimney."
  (dolist (sx '(-1 1))
    (dolist (sz '(-1 1))
      (beam-between g (list (* sx 2.4) -1.0 (* sz 2.4)) (list (* sx 1.2) 14.0 (* sz 1.2)) :thick .22 :rgb "#5a4636")))
  (loop for y in '(4 8 12) do (shape g :box (list (- 4.6 (* y .16)) .14 (- 4.6 (* y .16))) "#5a4636" 0 y 0 :shadow nil))
  (shape g :box '(3.6 .2 3.6) "#6a5038" 0 14.1 0)
  (shape g :box '(2.6 2.2 2.6) "#7a6a4a" 0 15.3 0)
  (shape g :box '(2.7 .9 2.7) "#2e333b" 0 15.6 0 :shadow nil)
  (shape g :cone '(2.6 1.6 4) "#3c5a48" 0 17.2 0 :rot (list 0 (/ pi 4) 0))
  (beam-between g '(-2.6 0 0) '(-1.5 14.0 0) :thick .12 :rgb "#3a2a1e")
  (shape g :box '(4.0 2.6 5.0) "#6a5038" 5.0 1.3 3.0)
  (dolist (s '(-1 1)) (shape g :box '(2.9 .16 5.6) "#4c4e52" (+ 5.0 (* s 1.1)) 3.2 3.0 :rot (list 0 0 (* s -.6))))
  (shape g :box '(.4 1.4 .4) "#5a4a44" 5.8 3.9 2.0)
  (shape g :box '(.12 1.0 .8) "#e8c070" 2.95 1.5 3.0 :emit "#ffc060" :energy 1.0 :shadow nil)
  (chimney-smoke g 5.8 4.8 2.0 :size 1.0))

(defun mark-old-bus (g)
  "Build in group G a bus abandoned in a field and a scarecrow."
  (let ((b (group g 0 .1 0)))
    (node-3d-set-rotation b (v3 .03 .5 .09))
    (shape b :box '(2.4 2.2 8.0) "#7a4a30" 0 1.3 0)
    (shape b :box '(2.44 .4 8.04) "#8a8070" 0 2.5 0 :shadow nil)
    (shape b :box '(2.46 .7 7.0) "#16181c" 0 1.8 0 :shadow nil)
    (dolist (z '(-2.6 2.4)) (shape b :cyl '(.45 .45 .3 10) "#141416" -1.15 .3 z :rot (list 0 0 (/ pi 2)))))
  (shape g :box '(.1 2.4 .1) "#5a4636" 4.5 1.2 -3.0)
  (shape g :box '(1.6 .1 .1) "#5a4636" 4.5 1.7 -3.0)
  (shape g :ball '(.28 8 6) "#d8b050" 4.5 2.5 -3.0)
  (shape g :box '(.9 1.0 .12) "#6a3030" 4.5 1.5 -3.0 :rot '(0 0 .08)))

(defun mark-radar (g)
  "Build in group G a station on a mountain: a white dome and a turning antenna."
  (shape g :box '(10.0 5.0 10.0) "#9a9a94" 0 2.5 0)
  (shape g :cyl '(3.0 3.6 8.0 12) "#a8a8a2" 0 9.0 0)
  (shape g :ball '(6.0 16 10) "#e8e8e4" 0 17.0 0 :rough .5)
  (shape g :box '(.14 1.2 4.0) "#e8c070" -5.05 2.8 0 :emit "#ffc060" :energy .9 :shadow nil)
  (shape g :box '(.8 9.0 .8) "#6e6e6a" 2.0 4.5 -13.0)
  (let ((dish (group g 2.0 9.6 -13.0)))
    (shape dish :cyl '(3.4 3.4 .25 14) "#c8c8c4" 0 .8 .9 :rot (list (- (/ pi 2) .25) 0 0))
    (shape dish :box '(.2 1.2 .2) "#6e6e6a" 0 .3 0)
    (moving dish :spin :y .9))
  (beam-between g '(-7 0 -4) '(-7 16 -4) :thick .3 :rgb "#6e6e6a")
  (red-light g -7 16.4 -4))

(defun mark-cow (g x z yaw rgb)
  (let ((c (group g x 0 z)))
    (node-3d-set-rotation c (v3 0 yaw 0))
    (shape c :box '(.75 .85 1.7) rgb 0 1.0 0)
    (shape c :box '(.5 .4 .6) "#2a2424" 0 1.15 -.35 :shadow nil)
    (dolist (sx '(-.25 .25)) (dolist (sz '(-.6 .6)) (shape c :box '(.16 .6 .16) rgb sx .3 sz)))
    (let ((head (group c 0 1.15 .95)))
      (shape head :box '(.42 .42 .6) rgb 0 -.1 .2)
      (shape head :box '(.3 .2 .2) "#d8a8a0" 0 -.18 .52 :shadow nil)
      (moving head :swing .5 (+ .5 (* .3 (vr x z))) (* 6 (vr z x)) :x -.55))))

(defun mark-farm (g)
  "Build in group G a farm: a long cowshed, a water tower, cows in the pasture."
  (shape g :box '(10.0 3.6 36.0) "#d8d4c4" 6.0 1.8 0)
  (dolist (s '(-1 1)) (shape g :box '(5.9 .2 36.6) "#7a5a4a" (+ 6.0 (* s 2.6)) 4.5 0 :rot (list 0 0 (* s -.42))))
  (loop for z from -15 to 15 by 6 do (shape g :box '(.12 .8 1.6) "#2e333b" .95 2.4 z :shadow nil))
  (shape g :cyl '(.9 .9 13.0 8) "#8a8580" 16.0 6.5 -22.0)
  (shape g :cyl '(2.0 2.0 5.0 10) "#8a8580" 16.0 15.5 -22.0)
  (shape g :cone '(2.2 1.4 10) "#6a4a44" 16.0 18.7 -22.0)
  (loop for z from -24 to 24 by 4 do (shape g :box '(.12 1.1 .12) "#6a5a48" -22.0 .55 z))
  (dolist (y '(.5 .95)) (shape g :box '(.07 .09 48.0) "#6a5a48" -22.0 y 0 :shadow nil))
  (loop for (x z yaw rgb) in '((-6 -14 .4 "#e8e4d8") (-12 -6 2.2 "#3a2e28") (-16 4 -.8 "#8a5a3a") (-8 9 1.4 "#e8e4d8")
                               (-14 17 3.0 "#3a2e28") (-4 20 -2.0 "#8a5a3a") (-18 -18 .9 "#e8e4d8"))
        do (mark-cow g x z yaw rgb))
  (dolist (z '(-3 0 3)) (shape g :cyl '(.7 .7 1.2 10) "#d8b050" -1.5 .7 (+ z 22) :rot (list 0 0 (/ pi 2)))))

(defun mark-rocket (g)
  "Build in group G a rocket on a pedestal by the road."
  (shape g :box '(5.0 2.6 5.0) "#85857f" 0 1.3 0)
  (shape g :cyl '(1.1 1.5 12.0 12) "#c4c8cc" 0 9.0 0 :metal .5 :rough .35)
  (shape g :cone '(1.1 4.0 12) "#a3262c" 0 17.0 0)
  (dotimes (i 4)
    (let ((a (* i (/ pi 2))))
      (shape g :box '(.14 4.0 2.4) "#a3262c" (* 1.9 (cos a)) 4.6 (* -1.9 (sin a)) :rot (list 0 (+ a (/ pi 2)) 0))))
  (shape g :cyl '(1.3 1.3 .5 12) "#2c4a7a" 0 11.0 0 :shadow nil)
  (sign g "К ЗВЁЗДАМ" -2.56 1.5 0 :h .7 :rgb "#d8d4c4")
  (flood g -5 1.0 0 :range 30.0 :rgb "#d0e0ff" :energy 3.5))

;;; ------------------------------------------------------------------ the bridge over the chasm and the tunnel

(defun build-chasm-bridge (c h)
  "Build the arch bridge: a deck with railings and, under it, an arch on struts.
The land under it has dropped some fifty metres, and a river runs along the bottom."
  (let ((g (group *world*)) (span (+ h 26.0)) (rise 26.0) (prev nil))
    (shape (road-group g c) :box (list 1500.0 .3 (+ (* 2 h) 40.0)) *water-rgb* 0 -46.0 0 :rough .3 :shadow nil)   ; gorge river
    (loop for d from (- c span) to (+ c span) by 6.0
          do (let* ((u (/ (- d c) span)) (arch (- (* rise u u)))                    ; the arch touches the deck midway
                    (seg (road-group g d)))
               (shape seg :box '(10.8 .5 6.3) "#5a5a58" -1.8 -.3 0)
               (dolist (x '(3.7 -7.3))
                 (shape seg :box '(.5 1.4 6.3) "#8a3a2c" x -.85 0)
                 (shape seg :box '(.1 1.15 .1) "#8a8e92" x .55 0 :shadow nil)
                 (shape seg :box '(.08 .08 6.3) "#8a8e92" x 1.1 0 :shadow nil)
                 (when (< arch -1.0) (shape seg :box (list .4 (- arch) .4) "#8a3a2c" x (/ arch 2) 0)))
               (let ((here (loop for x in '(3.7 -7.3)
                                 collect (road-pos d x (+ arch -1.2)))))
                 (when prev (loop for a in prev for b in here do (beam-between g a b :thick .7 :rgb "#8a3a2c")))
                 (setf prev here))
               (when (zerop (mod (round (- d (- c span))) 12))             ; lamps along the railings
                 (dolist (x '(3.7 -7.3))
                   (shape seg :box '(.1 3.4 .1) "#5a5e60" x 2.2 0 :shadow nil)
                   (shape seg :box '(.3 .2 .3) "#ffe2a0" x 4.0 0 :emit "#ffd080" :energy 2.2 :shadow nil)))))
    ;; two tall pylons and stays from their tops to the deck: the bridge is seen from afar, and from it you see down
    (dolist (dp (list (- c (* h .62)) (+ c (* h .62))))
      (let ((p (road-group g dp)))
        (dolist (x '(4.6 -8.2))
          (shape p :box '(1.4 46.0 1.8) "#8a3a2c" x 21.0 0)
          (red-light p x 44.6 0 .45))
        (dolist (y '(30.0 42.0)) (shape p :box '(12.8 1.6 1.2) "#8a3a2c" -1.8 y 0))
        (flood p -1.8 6.0 0 :range 40.0 :energy 3.0)
        (dolist (x '(4.6 -8.2))
          (loop for k from 1 to 6
                do (dolist (sz '(-1 1))
                     (beam-between p (list x 43.0 0) (list (if (> x 0) 3.7 -7.3) .4 (* sz k 7.5)) :thick .09 :rgb "#d8d4c4"))))))
    g))

(defun add-tunnel-piece (d first last)
  "Add a piece of the tunnel vault at path coordinate D.
It is walls and a ceiling over both lanes, with a lamp under the ceiling.
FIRST and LAST mark the portals."
  (let ((g (keep (apply #'group *world* (road-pos d)) d)))
    (node-3d-set-rotation g (v3 0 (- (road-frame d)) 0))
    (shape g :box '(.8 7.0 8.3) "#3c3c40" -7.9 3.5 0)
    (shape g :box '(.8 7.0 8.3) "#3c3c40" 4.3 3.5 0)
    (shape g :box '(13.0 .8 8.3) "#34343a" -1.8 7.4 0)
    (if (and *tunnel* (> (min (- d (first *tunnel*)) (- (second *tunnel*) d)) 70.0))   ; deep inside the lamps are dead
        (shape g :box '(.5 .12 2.4) "#2a2620" -1.8 6.95 0 :shadow nil)
        (shape g :box '(.5 .12 2.4) "#ffb060" -1.8 6.95 0 :emit "#ff9a40" :energy 3 :shadow nil))
    (when (or first last)                                              ; portal: a concrete frame wider than the vault
      (let ((z (if first 4.4 -4.4)))
        (dolist (x '(-13.0 9.4)) (shape g :box '(9.0 12.0 1.6) "#9a9890" x 6.0 z))   ; the frame hides the cut in the hill
        (shape g :box '(31.4 4.6 1.6) "#9a9890" -1.8 10.0 z)
        (shape g :box '(8.0 .9 .2) "#2c4a7a" -1.8 9.4 (+ z (if first .65 -.65)) :shadow nil)
        (sign g "ТУННЕЛЬ" -1.8 9.4 (+ z (if first .78 -.78)) :h .6 :rgb "#f0ece0" :yaw (if first 0.0 pi))))))

;;; The megastructure. The tunnel does not come out on the other side of its hill: some way in, the old world
;;; is no longer shown at all - no land, no towns, no cars, nobody - and what there is instead is built around
;;; the wagon itself and goes by it without end. It is another place, not a piece of the ring: on the ring the
;;; wagon stands still from then on (STEP-WORLD), and the way driven inside, *MEGA-S*, is counted apart; the
;;; megastructure is laid out along that. There is no road of its own in it: one drives over the structures
;;; themselves - the floor of a level, the back of a pipe, a gangway, the bed of a railway.
;;;
;;; It is made of zones some two and a half kilometres long, each of its own kind, one after another and never the
;;; same twice in a row: a level on columns, a forest of panel houses on poles, a maze of pipes, a hole through
;;; the levels with the day falling in, walls of black drawers, a row of pale statues, a railway under its lamps.
;;; The kinds are taken from the city of «Girls' Last Tour». A zone is built cell by cell (*MEGA-CELL* metres)
;;; ahead of the wagon and taken away behind it; what a cell holds depends on its number alone, so everybody
;;; in the wagon sees the same.

(defparameter *mega-cell* 42.0 "The length of one cell of the megastructure, in metres.")
(defparameter *mega-zone* 64 "The number of cells in a zone: some five minutes of driving.")
(defparameter *mega-first* 16 "The number of cells in the first zone, the one the tunnel opens into: a short one.")
(defparameter *mega-names* '(:columns "Уровень на колоннах" :poles "Дома на столбах" :pipes "Трубы" :crater "Пролом"
                             :graves "Картотека" :statues "Истуканы" :rail "Сортировочная" :spiral "Спуск")
  "What each kind of zone is called, in the corner of the screen.")
(defparameter *mega-kinds* '(:poles :pipes :graves :crater :spiral :statues :rail :columns) "The kinds of zones, after the first one.")
(defvar *mega-parts* nil "The megastructure's moving parts, a plist: :cells (the group the cells hang in) and :entry (the tunnel we came by).")
(defvar *mega-cells* (make-hash-table) "The cells built right now: a table from a cell's number to its node.")
(defvar *mega-s* 0.0 "How far we have driven since the old world went out of sight.")
(defparameter *mega-haze* '(:columns .75 :poles 1.0 :pipes .9 :graves .2 :crater .9 :statues 1.0 :rail .7 :spiral .3)
  "How much haze stands in each kind of zone: in the open ones the far shapes show, the closed ones are dark.")

(defun mega-kind (i)
  "Return the kind of the zone that cell I belongs to. The first zone is the level on columns."
  (let ((z (if (< i *mega-first*) 0 (1+ (floor (- i *mega-first*) *mega-zone*)))))
    (if (<= z 0)
        :columns
        (let* ((n (length *mega-kinds*)) (block (floor (1- z) n)) (pos (mod (1- z) n)))
          (nth (mod (+ (floor (* n (vr block 9.0))) (* pos (1+ (floor (* (1- n) (vr block 4.0)))))) n) *mega-kinds*)))))

;;; The way through the megastructure is not straight. Every cell turns a little and climbs or drops a little
;;; against the one before, by its number alone; how much depends on the zone: a level on columns only bends, a
;;; pipe bends and dips, the drawers run dead straight and down, the descent winds down and down to the left.
;;; A cell's place is the sum of all the turns before it, kept in *MEGA-POSES* as it is first asked for.

(defvar *mega-poses* (make-hash-table) "A table from a cell's number to where it starts and how it lies: (x y z yaw pitch).")
(defvar *mega-pose-top* 0 "The highest cell whose place is known.")

(defun mega-bend (i)
  "Return how cell I turns against the one before it, in radians (left is positive), and how steep it is (up is positive)."
  (if (< i *mega-first*)
      (values 0.0 0.0)
      (let* ((kind (mega-kind i)) (b (floor i 8)) (way (vr b 21.0)) (grade (vr b 33.0))
             (turn (ecase kind (:columns 2.5) (:poles 4.0) (:pipes 4.5) (:graves 0.0) (:crater 3.0) (:statues 1.5) (:rail 2.0) (:spiral 6.0)))
             (steep (ecase kind (:columns 0.0) (:poles 0.0) (:pipes .07) (:graves .03) (:crater 0.0) (:statues 0.0) (:rail .015) (:spiral .07))))
        (values (* (/ pi 180) turn (cond ((eq kind :spiral) 1) ((< way .3) 0) ((< way .55) 1) ((< way .8) -1) ((< way .9) 2) (t -2)))
                (atan (* steep (cond ((member kind '(:spiral :graves)) -1) ((< grade .4) 0) ((< grade .7) -1) (t 1))))))))

(defun mega-pose (i)
  "Return where cell I starts and how it lies, as (x y z yaw pitch). Cells before the first run straight and level."
  (cond ((<= i 0) (list 0.0 0.0 (- (* i *mega-cell*)) 0.0 0.0))
        ((gethash i *mega-poses*))
        (t (when (zerop (hash-table-count *mega-poses*)) (setf *mega-pose-top* 0 (gethash 0 *mega-poses*) (list 0.0 0.0 0.0 0.0 0.0)))
           (loop for j from *mega-pose-top* below i                          ; walk on from the last one known
                 do (destructuring-bind (x y z yaw pitch) (gethash j *mega-poses*)
                      (multiple-value-bind (turn tilt) (mega-bend (1+ j))
                        (let ((c *mega-cell*))
                          (setf (gethash (1+ j) *mega-poses*)
                                (list (- x (* c (cos pitch) (sin yaw))) (+ y (* c (sin pitch))) (- z (* c (cos pitch) (cos yaw)))
                                      (+ yaw turn) tilt))))))
           (setf *mega-pose-top* (max *mega-pose-top* i))
           (gethash i *mega-poses*))))

(defun mega-columns (g i lamps)
  "Put a row of columns across cell I, every other cell; with LAMPS, a lamp on the two nearest."
  (when (evenp i)
    (dolist (x '(-230 -146 -62 58 142 226))
      (shape g :box '(9.0 440.0 9.0) "#7c7b76" (+ -1.8 x) -40.0 0 :shadow nil)
      (shape g :box '(20.0 7.0 20.0) "#6b6a66" (+ -1.8 x) 176.5 0 :shadow nil))
    (when lamps
      (dolist (x '(-57.0 53.0))
        (shape g :box '(.4 1.2 1.6) "#ffcf8a" (+ -1.8 x) 7.0 0 :emit "#ffb060" :energy 3 :shadow nil)
        (light g "#ffb866" 1.6 70.0 (+ -1.8 (* .8 x)) 7.0 0)))))

(defun build-mega-cell (cells i)
  "Build cell I of the megastructure under CELLS and return its group. Inside it x is to the right, y up, and the
cell runs from z = 0 back to z = -*MEGA-CELL*."
  (let* ((c *mega-cell*) (pose (mega-pose i)) (g (group cells (first pose) (second pose) (third pose)))
         (kind (mega-kind i)) (at (mod i 16)) (mid (- (/ c 2))))
    (node-3d-set-rotation g (v3 (fifth pose) (fourth pose) 0))
    (flet ((r (k) (vr (+ i 1000.5) (float k)))                        ; this cell's own dice
           (floor-slab (w rgb &optional (x -1.8)) (shape g :box (list w 1.0 c) rgb x -.55 mid :shadow nil)))
      ;; what we drive over is the structure itself, a different thing in every zone
      (ecase kind
        (:columns (floor-slab 520.0 "#5f5e5a") (shape g :box '(520.0 .02 .5) "#4a4946" -1.8 -.03 0 :shadow nil)
                  (shape g :box (list 520.0 4.0 c) "#4a4946" -1.8 182.0 mid :shadow nil))                    ; the level above
        (:poles (shape g :box (list 5.0 .3 c) "#4a4c50" 0.0 -.2 mid :shadow nil)                               ; a hanging gangway
                (dolist (x '(-2.6 2.6))
                  (shape g :box (list .3 .5 c) "#3a3c40" x .1 mid :shadow nil)
                  (shape g :box '(.12 300.0 .12) "#3a3c40" x 150.0 0 :shadow nil)))
        (:pipes nil)                                                                                         ; the back of a pipe, built below
        (:graves (floor-slab 44.0 "#1e1f22" -2.0))
        (:crater (floor-slab 520.0 "#62615d")
                 (dotimes (k 2)                                                                                ; slabs fallen from above
                   (shape g :box (list (+ 4.0 (* 9.0 (r (+ 200 k)))) .8 (+ 3.0 (* 6.0 (r (+ 210 k))))) "#55544f"
                          (* (if (< (r (+ 220 k)) .5) -1 1) (+ 14.0 (* 60.0 (r (+ 230 k))))) .3 (* (- c) (r (+ 240 k)))
                          :rot (list (* .3 (r (+ 250 k))) (* 3.0 (r (+ 260 k))) (* .25 (r (+ 270 k)))) :shadow nil)))
        (:statues (floor-slab 520.0 "#8a887f"))
        (:spiral (floor-slab 17.0 "#5f5e5a" -1.0))                                                           ; a ramp along a wall
        (:rail (floor-slab 44.0 "#3f3d3a" -6.0)))
      (unless (member kind '(:graves :spiral))                           ; far off, what makes the place big: piers and the edges of other levels
        (when (zerop (mod i 3))
          (let ((side (if (< (r 400) .5) -1 1)))
            (shape g :box '(46.0 1400.0 46.0) "#39474d" (* side (+ 330.0 (* 420.0 (r 401)))) 100.0 (* (- c) (r 402)) :shadow nil)))
        (when (zerop (mod i 5))
          (let ((side (if (< (r 410) .5) -1 1)))
            (shape g :box (list 520.0 14.0 (* 3 c)) "#323f45" (* side (+ 560.0 (* 300.0 (r 411)))) (- (* 520.0 (r 412)) 220.0) mid :shadow nil))))
      (when (= at 8)                                                     ; one of our bus stops every so often, under a lamp
        (let ((stop (group g *stop-x* 0.0 -20.0)))
          (shape g :box '(9.0 .5 14.0) "#6f6e69" 8.6 -.3 -20.0 :shadow nil)
          (build-stop stop (* 7.0 (floor i 16)))
          (shape g :box '(.14 5.6 .14) "#3a3c40" 5.0 2.8 -15.0 :shadow nil)
          (shape g :box '(.6 .2 .4) "#ffe2a8" 5.3 5.6 -15.0 :emit "#ffcf8a" :energy 3 :shadow nil)
          (light g "#ffcf8a" 2.2 26.0 5.6 5.0 -15.0)))
      (ecase kind
        (:columns (mega-columns g i (zerop (mod i 6))))                 ; a level on columns; here and there the lamps still burn
        (:poles                                                          ; panel houses, each on one pole
         (dotimes (k 2)
           (let* ((side (if (< (r (+ 1 k)) .5) -1 1)) (x (* side (+ 34.0 (* 230.0 (r (+ 3 k))))))
                  (up (+ 14.0 (* 110.0 (r (+ 5 k))))) (z (- (* c (r (+ 7 k)))))
                  (h (group g x (+ up 7.5) z)))
             (shape g :cyl '(1.6 1.6 360.0 8) "#6a6c70" x (- up 180.0) z :shadow nil)
             (node-3d-set-rotation h (v3 0 (* 1.2 (- (r (+ 9 k)) .5)) 0))
             (shape h :box '(12.0 15.0 64.0) "#85857f" 0 0 0 :shadow nil)
             (when (< (r (+ 11 k)) .45)                                   ; somebody is at home
               (shape h :box '(.3 1.6 2.0) "#ffcf7a" (* side -6.1) 1.5 (- (* 40.0 (r (+ 13 k))) 20.0) :emit "#ffb866" :energy 3 :shadow nil)
               (when (< (abs x) 110.0) (light h "#ffb866" 2.2 46.0 (* side -9.0) 1.5 0.0))))))
        (:pipes                                                          ; a maze of pipes; the road lies on one of them
         (shape g :cyl (list 5.6 5.6 c 14) "#d4d6d2" 0.0 -5.62 (- (/ c 2)) :rot (list (/ pi 2) 0 0) :shadow nil)
         (when (evenp i) (shape g :cyl '(6.5 6.5 1.6 14) "#a9aca8" 0.0 -5.62 0 :rot (list (/ pi 2) 0 0) :shadow nil))
         (dotimes (k 5)
           (let* ((rad (+ .8 (* 4.6 (r (+ 20 k)) (r (+ 30 k))))) (z (- (* c (r (+ 40 k)))))
                  (rgb (nth (floor (* 3 (r (+ 50 k)))) '("#c9ccc8" "#8a8f93" "#2a2a2e"))) (way (r (+ 60 k))))
             (cond ((< way .4)                                             ; across, over the road or under it
                    (shape g :cyl (list rad rad 520.0 10) rgb -1.8 (if (< (r (+ 70 k)) .7) (+ 9.5 rad (* 60.0 (r (+ 80 k)))) (- -14.0 rad (* 60.0 (r (+ 80 k))))) z
                           :rot (list 0 0 (/ pi 2)) :shadow nil))
                   ((< way .75)                                            ; a riser beside it
                    (shape g :cyl (list rad rad 500.0 10) rgb (* (if (< (r (+ 90 k)) .5) -1 1) (+ 11.0 rad (* 130.0 (r (+ 100 k))))) 0.0 z :shadow nil))
                   (t                                                      ; a slanting one, off to a side
                    (shape g :cyl (list rad rad 460.0 10) rgb (* (if (< (r (+ 90 k)) .5) -1 1) (+ 60.0 (* 120.0 (r (+ 100 k))))) (* 40.0 (r (+ 110 k))) z
                           :rot (list 0 (* 3.0 (r (+ 120 k))) 1.0) :shadow nil))))))
        (:graves                                                         ; walls of black drawers along the road
         (dolist (x '(-22.0 18.0))
           (shape g :box (list 3.0 46.0 c) "#16171a" x 14.0 (- (/ c 2)) :shadow nil)
           (loop for y from -6.0 to 34.0 by 2.5
                 do (shape g :box (list 3.2 .08 c) "#2c2e33" x y (- (/ c 2)) :shadow nil))
           (loop for z from 0.0 below c by 3.0 do (shape g :box '(3.2 46.0 .08) "#2c2e33" x 14.0 (- z) :shadow nil)))
         (when (zerop (mod at 3))                                         ; a bare bulb on the wall now and then
           (shape g :box '(.3 .3 .3) "#fff0c8" 16.3 6.0 -8.0 :emit "#ffe2a8" :energy 3 :shadow nil)
           (light g "#ffe2a8" 1.4 30.0 12.0 6.0 -8.0))
         (when (< (r 1) .5)                                               ; one drawer pulled out
           (shape g :box '(2.4 2.2 2.7) "#3a3c42" (if (< (r 2) .5) -19.6 15.6) (+ 1.5 (* 2.5 (floor (* 4 (r 3))))) (- (* 3.0 (floor (* 13 (r 4))))) :shadow nil)))
        (:crater                                                         ; a hole through the levels above: the day falls in, and snow lies under it
         (if (= at 8)
             (progn (shape g :cyl '(64.0 64.0 1.0 24) "#6f6e69" 62.0 -.6 -20.0 :shadow nil)                 ; the floor under the hole
                    (shape g :ball '(30.0 14 8) "#e6eaf0" 70.0 -.4 -24.0 :scale '(1.0 .08 1.0) :shadow nil)   ; the snow heap
                    (shape g :cyl '(46.0 46.0 420.0 20) "#cfd8e6" 70.0 210.0 -24.0 :alpha .05 :unshaded t :shadow nil)   ; the shaft of light
                    (shape g :cyl '(46.0 46.0 1.0 20) "#b9d6de" 70.0 420.0 -24.0 :emit "#b9d6de" :energy 2.5 :unshaded t :shadow nil)   ; the hole itself
                    (light g "#cfe2ea" 3.2 120.0 62.0 26.0 -22.0) (light g "#cfe2ea" 2.2 80.0 20.0 12.0 -20.0))
             (mega-columns g i nil)))
        (:statues                                                        ; pale pillars with faces, standing along the road
         (let* ((side (if (evenp i) -1 1)) (h (+ 16.0 (* 26.0 (r 1)))) (x (* side (+ 15.0 (* 9.0 (r 2))))))
           (shape g :box (list 4.6 h 4.6) "#d8d6cc" x (/ h 2) -20.0 :shadow nil)
           (dolist (ez '(-1.1 1.1)) (shape g :box '(.2 .9 .7) "#1a1a1c" (- x (* side 2.35)) (- h 3.2) (+ -20.0 ez) :shadow nil))
           (shape g :box '(.2 .25 1.6) "#1a1a1c" (- x (* side 2.35)) (- h 5.6) -20.0 :shadow nil)
           (when (evenp (floor i 2)) (light g "#dfe8ff" 1.2 34.0 (- x (* side 7.0)) 3.0 -20.0))        ; lit from below, some of them
           (when (< (r 3) .25)                                            ; a very big one, far off
             (let ((bx (* (- side) (+ 110.0 (* 80.0 (r 4))))))
               (shape g :box '(18.0 300.0 18.0) "#c9c7be" bx 60.0 -10.0 :shadow nil)
               (dolist (ez '(-4.0 4.0)) (shape g :box '(.6 3.6 2.6) "#1a1a1c" (+ bx (* side 9.2)) 196.0 (+ -10.0 ez) :shadow nil))))))
        (:spiral                                                         ; the long way down: a wall on the left, nothing on the right
         (shape g :box (list 1.2 90.0 c) "#6b6a66" -10.0 30.0 mid :shadow nil)
         (shape g :box (list .5 1.0 c) "#4a4946" 7.2 .5 mid :shadow nil)
         (dotimes (k 3)                                                   ; far across the shaft, lamps on other ramps
           (shape g :box '(.9 .9 .9) "#ffcf8a" (+ 120.0 (* 260.0 (r (+ 300 k)))) (- (* 220.0 (r (+ 310 k))) 150.0) (* (- c) (r (+ 320 k)))
                  :emit "#ffb060" :energy 2.5 :unshaded t :shadow nil))
         (when (zerop (mod at 4))
           (shape g :box '(.3 .5 1.4) "#ffe2a8" -9.3 5.0 mid :emit "#ffcf8a" :energy 3 :shadow nil)
           (light g "#ffcf8a" 1.5 34.0 -7.0 5.0 mid)))
        (:rail                                                           ; a goods railway beside the road, under a row of white lamps
         (dolist (x '(-13.4 -11.6)) (shape g :box (list .16 .2 c) "#8a8e92" x .1 (- (/ c 2)) :shadow nil))
         (shape g :box (list 4.6 .4 c) "#4a4a48" -12.5 -.3 (- (/ c 2)) :shadow nil)
         (loop for z from 0.0 below c by 3.0 do (shape g :box '(3.2 .14 .5) "#3c2a20" -12.5 0.0 (- z) :shadow nil))
         (shape g :box '(.16 7.0 .16) "#3a3c40" -16.0 3.5 0 :shadow nil)
         (shape g :box '(2.2 .16 .5) "#f4f6ff" -15.0 7.0 0 :emit "#e8f0ff" :energy 3 :shadow nil)
         (when (evenp i) (light g "#dfe8ff" 1.6 40.0 -13.0 6.0 0))
         (when (<= 5 at 9)                                                ; a train left standing, now and then
           (dolist (z '(-3.0 -24.0))
             (shape g :box '(3.6 4.2 19.0) (if (= at 5) "#5a2a22" "#33363a") -12.5 2.6 (- z 9.0) :shadow nil)
             (shape g :box '(3.0 .8 17.0) "#1e1f22" -12.5 .5 (- z 9.0) :shadow nil))))))
    g))

(defun build-mega ()
  "Build what holds the megastructure around the wagon, in the wagon's own axes (x to the right, z backwards), and
return its group. The cells themselves are built by STEP-MEGA as the wagon goes."
  (let* ((g (group *cart*)) (tilt (group g)) (turn (group tilt)) (cells (group turn)) (entry (group g)))
    ;; the tunnel we came by: its vault goes on over us for a while and is left behind
    (shape entry :box '(.8 7.0 330.0) "#3c3c40" -7.9 3.5 -35.0 :shadow nil)
    (shape entry :box '(.8 7.0 330.0) "#3c3c40" 4.3 3.5 -35.0 :shadow nil)
    (shape entry :box '(13.0 .8 330.0) "#34343a" -1.8 7.4 -35.0 :shadow nil)
    (shape entry :box '(11.4 .12 330.0) "#2c2c2f" -1.8 .0 -35.0 :shadow nil)                              ; its roadway
    (loop for z from 126.0 downto -196.0 by 8.0 do (shape entry :box '(.5 .12 2.4) "#2a2620" -1.8 6.95 z :shadow nil))
    (dolist (x '(-13.0 9.4)) (shape entry :box '(9.0 12.0 1.6) "#9a9890" x 6.0 -200.4 :shadow nil))
    (shape entry :box '(31.4 4.6 1.6) "#9a9890" -1.8 10.0 -200.4 :shadow nil)
    (light g "#ffd9a0" 2.6 120.0 0.0 6.0 -6.0)                    ; our own light: a lantern over the wagon, so what is near is seen all round
    (clrhash *mega-cells*) (clrhash *mega-poses*)
    (setf *mega-parts* (list :cells cells :turn turn :tilt tilt :entry entry) *mega-s* 0.0)
    g))

(defun step-mega-cells ()
  "Keep the cells around the wagon built: some way behind, far ahead, no more than two new ones a frame."
  (let* ((c *mega-cell*) (cells (getf *mega-parts* :cells))
         (lo (floor (- *mega-s* 170.0) c)) (hi (floor (+ *mega-s* 760.0) c)) (fresh 0))
    ;; where we are on the way, and which way we face there: the megastructure is turned and moved the other way
    (let* ((i (floor *mega-s* c)) (f (- (/ *mega-s* c) i)) (a (mega-pose i)) (b (mega-pose (1+ i))))
      (flet ((mix (k) (+ (nth k a) (* f (- (nth k b) (nth k a))))))
        (node-3d-set-rotation (getf *mega-parts* :tilt) (v3 (- (mix 4)) 0 0))
        (node-3d-set-rotation (getf *mega-parts* :turn) (v3 0 (- (mix 3)) 0))
        (node-3d-set-position cells (v3 (- (mix 0)) (- (mix 1)) (- (mix 2))))))
    (loop for i being the hash-keys of *mega-cells* using (hash-value node)
          do (unless (<= (1- lo) i (1+ hi)) (node-queue-free node) (remhash i *mega-cells*)))
    (loop for i from (floor *mega-s* c) to hi                             ; the nearest first
          while (< fresh 2)
          do (unless (gethash i *mega-cells*) (setf (gethash i *mega-cells*) (build-mega-cell cells i)) (incf fresh)))
    (loop for i from (1- (floor *mega-s* c)) downto lo
          while (< fresh 2)
          do (unless (gethash i *mega-cells*) (setf (gethash i *mega-cells*) (build-mega-cell cells i)) (incf fresh)))))

(defvar *mega-in* nil "True while we are in the megastructure: from the moment we drive deep into the tunnel until we back out of it.")

(defun inside-mega-p ()
  "Return true while we are in the megastructure. One gets in by driving forward into the tunnel, far enough that no
daylight shows; one stays in whatever the path coordinate does after that, and gets out only by backing out."
  (when (and *tunnel* *mega*)
    (let ((d0 (first *tunnel*)) (d1 (second *tunnel*)))
      (cond ((< *dist* (+ d0 85.0)) (setf *mega-in* nil))
            ((and (> *dir* 0) (< (+ d0 90.0) *dist* (- d1 40.0))) (setf *mega-in* t)))))
  (and *tunnel* *mega* *mega-in*))

(defun step-gloom ()
  "Set how deep inside we are: the dark comes over the first stretch of the tunnel, for those driving into it."
  (setf *gloom*
        (cond ((inside-mega-p) 1.0)
              ((and *tunnel* *mega* (> *dir* 0) (< *dist* (- (second *tunnel*) 40.0)))
               (float (max 0.0 (min 1.0 (/ (- *dist* (first *tunnel*) 20.0) 70.0))) 1.0))
              (t 0.0))))

(defun step-mega ()
  "Show the megastructure to those inside it, and nothing of the old world; lay it out along the way driven; keep the
fuel up, since there is nowhere to fill it."
  (let ((in (inside-mega-p)))
    (when *land* (object-set *land* "visible" (not in)))
    (when *world* (object-set *world* "visible" (not in)))
    (let ((want (if in (getf *mega-haze* (mega-kind (floor *mega-s* *mega-cell*)) 0.0) 0.0)))   ; the haze of the zone comes on slowly
      (setf *haze* (if in (+ *haze* (* .012 (- want *haze*))) 0.0)))
    (cond ((and in (null *mega-node*)) (setf *mega-node* (build-mega)))
          ((and (not in) *mega-node*) (node-queue-free *mega-node*) (clrhash *mega-cells*) (setf *mega-node* nil *mega-parts* nil)))
    (when (and in *mega-parts*)
      (step-mega-cells)
      (node-3d-set-position (getf *mega-parts* :entry) (v3 0 0 *mega-s*))
      (object-set (getf *mega-parts* :entry) "visible" (< *mega-s* 600.0))
      (unless (net-guest-p)
        (when (< *fuel* .5) (setf *fuel* 1.0))))))                               ; nowhere to fill up here

(defun mega-name ()
  "Return what the part of the megastructure we are in is called."
  (getf *mega-names* (mega-kind (floor *mega-s* *mega-cell*))))

;;; A way straight into the tunnel, for working on what is inside it: the page address with ?tunnel (or the
;;; variable KIOSK_TUNNEL on the desktop). Once the lobby is closed, whoever owns the world is put a hundred
;;; metres before the tunnel with a full tank, the intro skipped, and the autopilot set through it.

(defvar *tunnel-spawn* :ask "Whether to start at the tunnel: :ASK until the address has been read, then T or NIL; :LOBBY while the lobby is open, :DONE once we are there.")

(defun tunnel-mode-p () "Return true if this game was started at the tunnel." (and (member *tunnel-spawn* '(t :lobby :done)) t))

(defun step-tunnel-spawn ()
  (when (eq *tunnel-spawn* :ask)
    (setf *tunnel-spawn*
          (and (or (let ((e (ext:getenv "KIOSK_TUNNEL"))) (and e (plusp (length e))))
                   (and (object-call (gdext:singleton "OS") "has_feature" "web")
                        (let ((q (ignore-errors (object-call (gdext:singleton "JavaScriptBridge") "eval" "location.search"))))
                          (and (stringp q) (search "tunnel" q)))))
               t)))
  (when (and (eq *tunnel-spawn* t) *lobby-open*) (setf *tunnel-spawn* :lobby))        ; wait for the lobby to be answered
  (when (and (eq *tunnel-spawn* :lobby) *tunnel* *cart* (not *lobby-open*)
             (or (not *dev*) (and (boundp '*dev-started*) (symbol-value '*dev-started*))))     ; on the polygon: after its own start
    (setf *tunnel-spawn* :done)
    (unless (net-guest-p)
      (when (eq *story-stage* :intro)                                                   ; no intro: straight to the road
        (setf *story-stage* :owing)
        (when *show* (setf *show* nil) (show-card)))
      (setf *fuel* 1.0 *auto* nil *speed* 0.0 *dir* 1)
      (relocate (- (first *tunnel*) 100.0))
      (let ((next (or (find-if (lambda (tw) (> (town-stop-d tw) (second *tunnel*))) *towns*) (first *towns*))))
        (go-to-town next))
      (say-all "Старт у туннеля: бак полный, автопилот ведёт сквозь него"))))

(defun step-tunnel ()
  (step-tunnel-spawn)
  (step-gloom)
  (step-mega)
  (when *tunnel*
    (destructuring-bind (d0 d1) *tunnel*
      (let ((from (- d0 36.0)) (to (+ d1 36.0)))
        (cover :tunnel 8.0
               (lambda (d i) (declare (ignore i))
                 (when (<= from d to)
                   (let ((*keep-reach* 290.0)) (add-tunnel-piece d (< (- d from) 8) (< (- to d) 8)))))
               290.0)))))

;;; ------------------------------------------------------------------ mist and fireflies on the marsh

(defun build-mist (d)
  "Build wisps of mist over the marsh at path coordinate D, with fireflies in it (those only at night)."
  (let ((g (group *world*)))
    (dotimes (i 7)
      (let ((x (* (if (evenp i) 1 -1) (+ 16 (* 60 (vr d i))))) (dd (+ d (* 120 (- (vr i d) .5)))))
        (destructuring-bind (px py pz) (side-point x dd 0.0)
          (shape g :box (list (+ 22 (* 20 (vr i 3))) .05 (+ 14 (* 16 (vr d (+ i 5))))) "#e8f0f0" px (+ (max py (+ (road-y dd) *water-y*)) .9 (* .8 (vr i 7))) pz
                 :alpha .13 :unshaded t :shadow nil))))
    (dotimes (i 16)
      (let ((x (* (if (evenp i) 1 -1) (+ 6 (* 34 (vr d (+ 20 i)))))) (dd (+ d (* 110 (- (vr (+ i 9) d) .5)))))
        (destructuring-bind (px py pz) (side-point x dd 0.0)
          (let ((holder (moving (group g px (+ (max py (+ (road-y dd) *water-y*)) .6 (* 1.6 (vr i 11))) pz) :night)))
            (moving (shape holder :ball '(.07 5 3) "#d8ff70" 0 0 0 :emit "#c8ff40" :energy 5 :shadow nil) :blink (+ .25 (* .5 (vr i 13))))))))
    g))

;;; ------------------------------------------------------------------ diners

(defun diner-cat (g x z yaw n)
  (let ((c (group g x 0 z)))
    (node-3d-set-rotation c (v3 0 yaw 0))
    (node-3d-set-scale (first (make-cat c n)) (v3 .62 .62 .62))
    c))

(defun diner-table (g x z &optional umbrella)
  "Build in group G a standing table at (X Z); with UMBRELLA it is an outdoor one."
  (shape g :cyl '(.05 .05 .7 6) "#8a8e92" x .35 z)
  (shape g :cyl '(.5 .5 .05 10) "#e4e0d4" x .72 z)
  (shape g :box '(.12 .16 .12) "#c9a13a" (+ x .15) .83 (- z .1) :shadow nil)
  (when umbrella
    (shape g :cyl '(.03 .03 2.2 6) "#8a8e92" x 1.1 z :shadow nil)
    (shape g :cone '(1.3 .5 8) umbrella x 2.3 z)))

(defun diner-apron (g name rgb)
  "Build in group G what all diners share: a parking lot, a signpost by the road, a lamp, a bin, cars."
  (shape g :box '(24.0 .1 36.0) "#4e4d4a" 13.0 .03 0 :shadow nil)
  (shape g :box '(.25 6.4 .25) "#8a8a84" 1.6 3.2 15.0)
  (shape g :box '(.3 1.5 5.2) rgb 1.6 5.6 15.0 :shadow nil)
  (sign g name 1.42 5.6 15.0 :h .62 :rgb "#f0ece0")
  (sign g name 1.78 5.6 15.0 :h .62 :rgb "#f0ece0" :yaw (/ pi 2))
  (shape g :box '(.13 5.6 .13) "#5a5e60" 4.0 2.8 -12.0)
  (shape g :box '(.5 .12 .26) "#ffe2a0" 4.0 5.62 -12.0 :emit "#ffd080" :energy 1.6 :shadow nil)
  (shape g :cyl '(.25 .2 .6 8) "#3c4638" 7.2 .32 -2.0)
  (flood g 7.0 4.5 0 :range 26.0 :rgb rgb :energy 2.2)              ; at night the sign floods the lot with its colour
  (parked-car g 6.5 (- -9 (random 3.0)))
  (when (zerop (random 2)) (parked-car g 6.3 (+ 10 (random 3.0)))))

(defun diner-glass (g name rgb)
  "Build in group G the 'Steklyashka' diner: a glass pavilion under a flat roof, letters on the roof."
  (shape g :box '(12.0 .4 18.0) "#8a8a84" 17.0 .2 0)
  (shape g :box '(10.6 3.2 16.6) "#a8c8d0" 17.0 2.0 0 :alpha .3 :rough .1 :shadow nil)
  (shape g :box '(.4 3.2 16.6) "#d8d4c4" 22.1 2.0 0)
  (dolist (x '(11.75 16.9)) (dolist (z '(-8.3 0 8.3)) (shape g :box '(.28 3.2 .28) "#e4e0d4" x 2.0 z)))
  (shape g :box '(13.6 .5 19.6) "#e4e0d4" 16.6 3.85 0)
  (shape g :box '(.3 .9 19.6) rgb 9.75 3.85 0 :shadow nil)
  (dolist (z '(-6 6)) (shape g :box '(.12 1.2 .12) "#8a8a84" 10.6 4.6 z :shadow nil))
  (sign g name 10.5 5.5 0 :h 1.4 :rgb rgb)
  (shape g :box '(8.0 .1 .5) "#fff0c8" 17.0 3.5 0 :emit "#ffe0a0" :energy 1.8 :shadow nil)
  (shape g :box '(1.0 1.1 9.0) "#8a6440" 20.6 .95 0)
  (loop for z in '(-5.5 0 5.5) for i from 0
        do (shape g :box '(1.1 .08 1.1) "#e4e0d4" 15.0 1.1 z :shadow nil) (shape g :box '(.1 .7 .1) "#8a8e92" 15.0 .75 z :shadow nil)
           (diner-cat g 15.0 (+ z .9) pi (+ i 3)))
  (diner-cat g 20.0 2.0 (- (/ pi 2)) 11)
  (diner-table g 8.0 -5.0 rgb) (diner-table g 8.0 5.0 "#e4e0d4")
  (diner-cat g 8.0 -4.1 pi 7) (diner-cat g 8.9 5.2 (- (/ pi 2)) 20))

(defun diner-grill (g name rgb)
  "Build in group G a shashlik house: a log cabin, a canopy on posts, a smoking grill, fairy lights."
  (shape g :box '(6.0 3.0 7.0) "#6a5038" 19.0 1.5 -3.0)
  (dolist (s '(-1 1)) (shape g :box '(4.0 .2 7.6) "#4c4e52" (+ 19.0 (* s 1.6)) 3.75 -3.0 :rot (list 0 0 (* s -.5))))
  (shape g :box '(.12 1.2 1.6) "#e8c070" 15.95 1.7 -3.0 :emit "#ffc060" :energy 1.0 :shadow nil)
  (sign g name 15.8 3.3 -3.0 :h .8 :rgb "#f0ece0")
  (shape g :box '(.3 .9 6.4) rgb 15.9 3.3 -3.0 :shadow nil)
  (shape g :box '(7.4 .16 10.0) "#5a4636" 12.0 3.0 2.0 :rot '(0 0 .06))
  (dolist (x '(8.6 15.4)) (dolist (z '(-2.6 6.6)) (shape g :box '(.2 3.0 .2) "#5a4636" x 1.5 z)))
  (loop for i below 9 do (shape g :ball '(.07 6 4) (nth (mod i 5) *fairy*) 8.6 (- 2.7 (* .12 (sin (* (/ i 8) pi)))) (+ -2.6 (* i 1.15))
                                :emit (nth (mod i 5) *fairy*) :energy 3 :shadow nil))
  (loop for z in '(-.5 2.5 5.5) for i from 0
        do (shape g :box '(1.8 .08 .9) "#8a6440" 12.0 .78 z) (shape g :box '(.1 .78 .1) "#5a4636" 12.0 .39 z)
           (dolist (s '(-1 1)) (shape g :box '(1.8 .06 .3) "#6a5038" 12.0 .45 (+ z (* s .75))))
           (when (< i 2) (diner-cat g 12.0 (+ z .75) pi (+ 14 (* i 9)))))
  ;; the grill
  (shape g :box '(1.7 .28 .6) "#2a2c30" 7.0 .95 9.0)
  (dolist (sx '(-.7 .7)) (shape g :box '(.08 .9 .08) "#2a2c30" (+ 7.0 sx) .45 9.0))
  (shape g :box '(1.5 .04 .45) "#ff7a30" 7.0 1.08 9.0 :emit "#ff5a10" :energy 2.5 :shadow nil)
  (chimney-smoke g 7.0 1.3 9.0 :size .9 :amount 10 :life 5.0)
  (diner-cat g 7.0 9.9 pi 2)
  (loop for i below 5 do (shape g :cyl '(.14 .14 1.0 6) "#7a5a3a" (+ 9.2 (* .3 (mod i 3))) (+ .15 (* .28 (floor i 3))) 10.5 :rot (list (/ pi 2) 0 0))))

(defun diner-kiosk (g name rgb)
  "Build in group G a round pavilion with a serving window, standing tables and a barrel of kvass."
  (shape g :cyl '(3.3 3.3 3.0 8) "#d8d4c4" 15.0 1.5 0)
  (shape g :cyl '(3.36 3.36 .7 8) rgb 15.0 .55 0 :shadow nil)
  (shape g :cone '(4.6 1.9 8) rgb 15.0 3.95 0)
  (shape g :box '(.2 1.1 2.6) "#ffe6b0" 11.75 2.0 0 :emit "#ffd890" :energy 1.3 :shadow nil)
  (shape g :box '(.7 .08 2.8) "#8a6440" 11.5 1.4 0)
  (sign g name 11.55 3.25 0 :h .6 :rgb "#f0ece0")
  (diner-cat g 13.0 0 (- (/ pi 2)) 5)
  (loop for (x z) in '((8.0 -4.0) (7.4 .5) (8.4 4.5)) for i from 0
        do (diner-table g x z (when (oddp i) rgb))
           (diner-cat g (+ x .8) z (- (/ pi 2)) (+ 9 (* i 7))))
  (kvas g 9.0 9.5))

(defun build-diner (d name kind rgb)
  (let ((g (apply #'group *world* (road-pos d 3.2 -.35))) (*seed* (rs-for (+ d 3))))
    (node-3d-set-rotation g (v3 0 (- (road-frame d)) 0))
    (diner-apron g name rgb)
    (ecase kind (:glass (diner-glass g name rgb)) (:grill (diner-grill g name rgb)) (:kiosk (diner-kiosk g name rgb)))
    g))
