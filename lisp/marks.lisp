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
  (model g "mark-windmill")
  (let ((hub (group g -3.4 9.0 0)))
    (dotimes (i 4)
      (let ((blade (group hub)))
        (node-3d-set-rotation blade (v3 (* i (/ pi 2)) 0 0))
        (model blade "mark-sail")))
    (moving hub :spin :x .45)))

(defun mark-plane (g)
  "Build in group G an aeroplane on a pedestal."
  (model g "mark-plane"))

(defun mark-church (g)
  "Build in group G a white church on a hill: a dome and a bell tower with a tented roof."
  (model g "mark-church")
  (flood g -7 2.0 -3 :range 36.0 :energy 3.2))

(defun mark-pumpjacks (g)
  "Build in group G oil pumpjacks that nod."
  (loop for z in '(-13 0 13) for i from 0
        do (let ((u (group g (* i 3.0) 0 z)))
             (model u "mark-pumpjack")
             (let ((beam (group u 0 4.3 0)))
               (model beam "mark-pumpjack-beam")
               (moving beam :swing .3 (+ .9 (* i .12)) (* i 1.7)))))
  (model g "mark-pumpjacks"))

(defun mark-mast (g)
  "Build in group G a guyed radio mast: red and white, with blinking lights."
  (model g "mark-mast")
  (red-light g 0 120.6 0 .6) (red-light g .5 80 0) (red-light g .5 40 0))

(defun mark-elevator (g)
  "Build in group G a grain elevator: a row of silos, a gallery on top, a working tower."
  (model g "mark-elevator")
  (sign g "ЭЛЕВАТОР" -3.9 30.5 -19.0 :h 1.6 :rgb "#8a4038")
  (flood g -9 5.0 0 :range 52.0 :rgb "#fff0d0")
  (red-light g 0 38.6 -19.0))

(defun mark-ferris-wheel (g)
  "Build in group G an old Ferris wheel that creaks and still turns."
  (let ((r 16.0) (hub-y 19.0))
    (model g "mark-wheel")
    (let ((w (group g 0 hub-y 0)))
      (model w "mark-wheel-rim")
      (moving w :spin :x .07))
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
  (model g "mark-watchtower")
  (chimney-smoke g 5.8 4.8 2.0 :size 1.0))

(defun mark-old-bus (g)
  "Build in group G a bus abandoned in a field and a scarecrow."
  (model g "mark-old-bus"))

(defun mark-radar (g)
  "Build in group G a station on a mountain: a white dome and a turning antenna."
  (model g "mark-radar")
  (let ((dish (group g 2.0 9.6 -13.0)))
    (model dish "mark-radar-dish")
    (moving dish :spin :y .9))
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
  (model g "mark-farm")
  (loop for (x z yaw rgb) in '((-6 -14 .4 "#e8e4d8") (-12 -6 2.2 "#3a2e28") (-16 4 -.8 "#8a5a3a") (-8 9 1.4 "#e8e4d8")
                               (-14 17 3.0 "#3a2e28") (-4 20 -2.0 "#8a5a3a") (-18 -18 .9 "#e8e4d8"))
        do (mark-cow g x z yaw rgb))
  )

(defun mark-rocket (g)
  "Build in group G a rocket on a pedestal by the road."
  (model g "mark-rocket")
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
