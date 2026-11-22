;;;; The megastructure: its monumental zones, how a cell is built, and how the whole is kept around the wagon.

(in-package #:kiosk-trip)

;;; The monumental zones. What makes a thing monumental here: an axis we drive along, a rhythm of great piers on
;;; either side, one thing far too large to be seen whole, and light that comes from somewhere.

(defzone zone-lift                                                           ; the way down from the tunnel: a platform on a slope, the whole hall open below
         (when (>= i 5)
           (let* ((drop (mega-drop i)) (y0 (second (mega-pose i))) (len (sqrt (+ (* c c) (* drop drop)))) (a (atan drop c)) (half (/ drop -2)))
             (flet ((ay (y) (- y y0)))                                    ; a height in the hall, whatever height this cell is at
               (shape g :box (list 6.0 1500.0 (+ c .5)) "#39474d" -21.0 (ay -200.0) mid :shadow nil)                  ; the wall we go down along
               (if (zerop drop)
                   (shape g :box (list 44.0 2.0 c) "#55524f" -1.8 -1.1 mid :shadow nil)                               ; the landing at the tunnel's mouth
                   (progn
                     (shape g :box (list 26.0 2.0 len) "#2c2c2f" -3.0 (- half 5.0) mid :rot (list (- a) 0 0) :shadow nil)   ; the bed of the track
                     (dolist (x '(-13.0 7.0)) (shape g :box (list 1.0 1.6 len) "#8a8e92" x (- half 3.4) mid :rot (list (- a) 0 0) :shadow nil))
                     (dotimes (k 4)
                       (shape g :box '(24.0 .8 1.6) "#c9a13a" -3.0 (- (* (/ drop -4) (+ k .5)) 3.9) (* (/ c -4) (+ k .5)) :rot (list (- a) 0 0) :shadow nil))))
               (when (evenp i)                                            ; a lamp on the wall
                 (shape g :box '(.6 2.0 6.0) "#ffe2a8" -17.8 (+ half 9.0) mid :emit "#ffcf8a" :energy 3 :shadow nil)
                 (light g "#ffcf8a" 2.0 70.0 -10.0 (+ half 8.0) mid))
               (shape g :box (list 1800.0 8.0 c) "#161c20" 700.0 (ay 180.0) mid :shadow nil)                           ; the roof of the hall
               (when (zerop (mod i 3))
                 (shape g :box (list (+ 200.0 (* 400.0 (r 1))) 1.0 5.0) "#cfe6ee" (+ 200.0 (* 1000.0 (r 2))) (ay 175.0) (* (- c) (r 3))
                        :emit "#b9d6de" :energy 2.2 :unshaded t :shadow nil))
               (shape g :box (list 1800.0 8.0 c) "#12171a" 700.0 (ay -440.0) mid :shadow nil)                          ; its floor, and lamps on it
               (dotimes (k 3)
                 (shape g :box '(3.0 3.0 3.0) "#ffcf8a" (+ 40.0 (* 1300.0 (r (+ 10 k)))) (ay -434.0) (* (- c) (r (+ 20 k))) :emit "#ffb060" :energy 2.5 :unshaded t :shadow nil))
               (shape g :box (list 10.0 1500.0 c) "#1c2428" 1560.0 (ay -200.0) mid :shadow nil)                        ; the far wall
               (when (zerop (mod i 4))                                    ; piers from floor to roof
                 (let ((w (+ 50.0 (* 60.0 (r 30)))))
                   (shape g :box (list w 620.0 w) "#39474d" (+ 300.0 (* 1000.0 (r 31))) (ay -130.0) (* (- c) (r 32)) :shadow nil)))
               (when (= i 15)                                             ; a hand of bronze rising from the floor: whose, one cannot see
                 (let ((h (group g 250.0 (ay -440.0) mid)))
                   (node-3d-set-rotation h (v3 0 .5 0))
                   (shape h :cyl '(42.0 50.0 240.0 12) "#8a7a52" 0 120.0 0 :metal .3 :shadow nil)
                   (shape h :box '(120.0 130.0 44.0) "#8a7a52" 0 300.0 0 :rot '(0 0 .06) :metal .3 :shadow nil)
                   (dotimes (k 4)
                     (shape h :box (list 22.0 (+ 100.0 (* 14.0 (- 2 (abs (- k 1.5))))) 24.0) "#8a7a52" (- (* 30.0 k) 45.0) (+ 412.0 (* 7.0 (- 2 (abs (- k 1.5))))) 6.0
                            :rot '(.14 0 0) :metal .3 :shadow nil))
                   (shape h :box '(24.0 84.0 24.0) "#8a7a52" 80.0 300.0 4.0 :rot '(0 0 -.6) :metal .3 :shadow nil)
                   (shape h :box '(2.0 2.0 2.0) "#ff3020" -15.0 484.0 10.0 :emit "#ff2010" :energy 3 :unshaded t :shadow nil)
                   (light h "#ffcf8a" 2.6 280.0 -130.0 330.0 90.0)))))))

(defun metro-train (g x way phase)
  "Put a train of four cars on the track at X of the cell G; it comes in from far off, stands and goes, WAY being 1 if
it runs our way. A car is a block of flats long."
  (let ((tr (group g x 30.0 -21.0)))
    (dotimes (k 4)
      (let ((z (* 168.0 (- k 1.5))))
        (shape tr :box '(64.0 70.0 160.0) "#2f5f8a" 0 0 z :emit "#2f5f8a" :energy .25 :shadow nil)
        (shape tr :box '(65.0 10.0 161.0) "#8ab0c8" 0 -16.0 z :emit "#8ab0c8" :energy .25 :shadow nil)
        (dolist (side '(-1 1)) (shape tr :box '(.6 20.0 140.0) "#ffe2a8" (* side 32.4) 10.0 z :emit "#ffcf8a" :energy 2.0 :unshaded t :shadow nil))))
    (dolist (z '(-332.6 332.6))
      (dolist (dx '(-18.0 18.0)) (shape tr :ball '(6.0 8 6) "#fff0c8" dx -18.0 z :emit "#ffe2a8" :energy 3 :unshaded t :shadow nil)))
    (live :train tr x 30.0 -21.0 way 80.0 phase)))

(defzone zone-metro                                                          ; a station of the underground, a palace the size of a valley: we are a beetle on its floor, and far off to either side trains the size of streets come and go
         (shape g :box (list 520.0 1.0 c) "#6a2a26" -1.0 -.55 mid :emit "#6a2a26" :energy .25 :rough .25 :shadow nil)   ; red granite, to the edge of the platform
         (when (evenp i) (shape g :box (list 520.0 .02 c) "#2a2426" -1.0 -.03 mid :shadow nil))                       ; and black, slab after slab
         (dolist (x '(-13.0 11.0)) (shape g :box (list 1.2 .03 c) "#c9a13a" x -.02 mid :metal .5 :shadow nil))        ; a strip of brass on either hand: the axis of the hall
         (dolist (side '(-1 1))
           (let ((x (+ -1.0 (* side 190.0))) (track (+ -1.0 (* side 330.0))))
             (when (zerop (mod i 4))                                      ; a pier of marble, a tower block high
               (shape g :box '(56.0 300.0 84.0) "#d8d2c4" x 150.0 -21.0 :emit "#d8c8a8" :energy .4 :shadow nil)
               (shape g :box '(60.0 24.0 88.0) "#4a2a26" x 12.0 -21.0 :shadow nil)
               (shape g :box '(60.0 12.0 88.0) "#c9a13a" x 294.0 -21.0 :metal .5 :emit "#c9a13a" :energy .3 :shadow nil)
               (shape g :box '(1.0 34.0 10.0) "#ffe2a8" (- x (* side 28.6)) 110.0 -21.0 :emit "#ffcf8a" :energy 3 :unshaded t :shadow nil)   ; a lamp on it, itself a house high
               (shape g :box '(.6 2.1 1.0) "#3c2a20" (- x (* side 28.4)) 1.05 -21.0 :shadow nil))                     ; and at its foot a door of our size
             (shape g :box (list 56.0 120.0 c) "#d8d2c4" x 360.0 mid :emit "#d8c8a8" :energy .32 :shadow nil)         ; the wall over the arches
             (shape g :box (list 80.0 60.0 c) "#141416" track -30.5 mid :shadow nil)                                  ; the track, in its pit
             (dolist (dx '(-22.0 22.0)) (shape g :box (list 3.0 3.0 c) "#8a8e92" (+ track dx) 1.0 mid :shadow nil))
             (shape g :box (list 8.0 420.0 c) "#cfc8b4" (+ -1.0 (* side 374.0)) 210.0 mid :emit "#cfc0a0" :energy .25 :shadow nil)   ; the wall behind the track
             (shape g :box (list .6 30.0 c) "#6a2a26" (+ -1.0 (* side 369.6)) 120.0 mid :shadow nil)
             (shape g :box (list 160.0 8.0 c) "#e8e2d2" (+ -1.0 (* side 298.0)) 304.0 mid :emit "#e8d8b8" :energy .2 :shadow nil)   ; the vault over the track
             (when (= (mod at 8) 1)
               (sign g (nth (mod (floor i 8) 5) '("ЗАВОДСКАЯ" "ЮБИЛЕЙНАЯ" "ПИОНЕРСКАЯ" "ОКТЯБРЬСКАЯ" "КОСМОНАВТОВ"))
                     (+ -1.0 (* side 369.0)) 170.0 mid :h 36.0 :rgb "#6a2a26" :yaw (* side (/ pi -2))))
             (when (= (mod i 20) (if (< side 0) 2 12))
               (metro-train g track (- side) (r (+ 5 side))))))
         (shape g :box (list 380.0 8.0 c) "#e8e2d2" -1.0 424.0 mid :emit "#e8d8b8" :energy .3 :shadow nil)             ; the vault of the hall
         (when (= (mod i 4) 2)                                            ; a panel of mosaic in it
           (shape g :box '(200.0 1.0 120.0) "#c9a13a" -1.0 419.6 -21.0 :metal .5 :emit "#c9a13a" :energy .3 :shadow nil)
           (shape g :box '(170.0 1.0 96.0) (nth (mod (floor i 4) 3) '("#a3262c" "#55708a" "#3c5a48")) -1.0 419.0 -21.0 :emit (nth (mod (floor i 4) 3) '("#a3262c" "#55708a" "#3c5a48")) :energy .4 :shadow nil))
         (when (zerop (mod i 4))                                          ; a chandelier: a ring of lamps the width of a street, high over us
           (shape g :cyl '(1.0 1.0 120.0 4) "#c9a13a" -1.0 360.0 0 :shadow nil)
           (shape g :cyl '(46.0 46.0 5.0 16) "#c9a13a" -1.0 298.0 0 :metal .5 :emit "#c9a13a" :energy .4 :shadow nil)
           (dotimes (k 8)
             (shape g :ball '(7.0 8 6) "#fff0c8" (+ -1.0 (* 46.0 (cos (* k .785)))) 290.0 (* 46.0 (sin (* k .785))) :emit "#ffe2a8" :energy 3 :unshaded t :shadow nil))
           (light g "#ffd9a0" 6.0 520.0 -1.0 200.0 0))
         (when (= (mod at 8) 5)                                           ; a bench and a lamp post of our own size, so that the eye can measure
           (shape g :box '(3.0 .5 .8) "#5a3e26" 14.0 .5 -20.0 :shadow nil)
           (shape g :box '(.14 5.0 .14) "#c9a13a" 14.0 2.5 -22.0 :shadow nil)
           (shape g :ball '(.4 8 6) "#fff0c8" 14.0 5.2 -22.0 :emit "#ffe2a8" :energy 3 :shadow nil)
           (light g "#ffd9a0" 1.8 24.0 14.0 4.6 -22.0)))

(defzone zone-quarry                                                         ; an open pit: down its benches, round and round, the diggers at work
         (shape g :box (list 26.0 60.0 (+ c 1.5)) "#6a5f52" -1.0 -30.05 mid :shadow nil)                              ; our bench
         (shape g :box (list 2.0 1.6 c) "#55504a" 11.0 .6 mid :shadow nil)                                            ; a bank of rock along its edge
         (dotimes (k 5)
           (let ((far (* 60.0 k)))
             (shape g :box (list 60.0 400.0 (+ c 3.0 (* .06 (+ far 60.0)))) (if (evenp k) "#5f564a" "#6a5f52") (- -44.0 far) (- (* 44.0 (1+ k)) 200.0) mid :shadow nil)
             (shape g :box (list 60.0 400.0 (+ c 1.5)) (if (evenp k) "#6a5f52" "#5f564a") (+ 42.0 far) (- (* -44.0 (1+ k)) 200.0) mid :shadow nil)))
         (shape g :box (list 900.0 400.0 (+ c 1.5)) "#55504a" 762.0 -420.0 mid :shadow nil)                           ; the bottom of the pit
         (when (< (r 1) .4) (shape g :box (list .5 (+ 6.0 (* 20.0 (r 2))) c) "#7a4a32" -13.9 (+ 6.0 (* 20.0 (r 3))) mid :shadow nil))   ; a seam of ore in the wall
         (when (evenp i)                                                  ; a floodlight on the bench below
           (shape g :box '(.8 70.0 .8) "#3a3c40" 66.0 -9.0 -8.0 :shadow nil)
           (shape g :box '(6.0 2.0 2.0) "#f4f6ff" 66.0 26.0 -8.0 :emit "#e8f0ff" :energy 3 :shadow nil)
           (light g "#dfe8ff" 2.2 140.0 60.0 22.0 -8.0))
         (when (= (mod at 8) 2)                                           ; a walking dragline on the bench above: its boom goes over our heads
           (let* ((ex (group g -46.0 44.0 mid)) (boom (group ex 0 22.0 0)) (arm (group boom)))
             (shape ex :cyl '(17.0 17.0 6.0 12) "#3a3a3c" 0 3.0 0 :shadow nil)
             (shape ex :box '(30.0 22.0 36.0) "#b8902e" 0 17.0 0 :shadow nil)
             (shape ex :box '(.5 5.0 9.0) "#ffe2a8" 15.2 20.0 -8.0 :emit "#ffcf8a" :energy 3 :shadow nil)
             (shape ex :box '(4.0 56.0 4.0) "#b8902e" 6.0 54.0 0 :rot '(0 0 -.25) :shadow nil)
             (node-3d-set-rotation arm (v3 0 0 .5))
             (shape arm :box '(190.0 5.0 5.0) "#b8902e" 95.0 0 0 :shadow nil)
             (shape boom :box '(.6 100.0 .6) "#2a2c30" 167.0 41.0 0 :shadow nil)
             (shape boom :box '(18.0 14.0 16.0) "#3a3a3c" 167.0 -14.0 0 :shadow nil)
             (live :swing boom .5 (+ 24.0 (* 12.0 (r 4))) (* 6.0 (r 5)))
             (light ex "#ffcf8a" 2.4 130.0 30.0 30.0 0)))
         (when (= (mod at 8) 6)                                           ; a dump truck the size of a house, on the bench below
           (let ((tr (group g 40.0 -44.0 mid)))
             (node-3d-set-rotation tr (v3 0 (* .4 (- (r 6) .5)) 0))
             (shape tr :box '(14.0 7.0 22.0) "#c9a13a" 0 9.0 0 :shadow nil)
             (shape tr :box '(17.0 8.0 20.0) "#b8902e" 0 16.0 4.0 :rot '(.16 0 0) :shadow nil)
             (shape tr :box '(8.0 5.0 6.0) "#c9a13a" -3.0 15.0 -11.0 :shadow nil)
             (dolist (x '(-7.5 7.5)) (dolist (z '(-7.0 7.0)) (shape tr :cyl '(4.6 4.6 4.0 10) "#1c1c1e" x 4.6 z :rot (list 0 0 (/ pi 2)) :shadow nil)))))
         (when (= at 12)                                                  ; and on the bottom a bucket-wheel digger, its wheel turning
           (let* ((bw (group g 430.0 -220.0 mid)) (wheel (group bw -230.0 96.0 0)))
             (shape bw :box '(80.0 40.0 60.0) "#b8902e" 0 30.0 0 :shadow nil)
             (shape bw :box '(90.0 10.0 70.0) "#3a3a3c" 0 5.0 0 :shadow nil)
             (shape bw :box '(250.0 9.0 9.0) "#b8902e" -115.0 72.0 0 :rot '(0 0 -.2) :shadow nil)
             (shape bw :box '(200.0 7.0 7.0) "#b8902e" 95.0 66.0 0 :rot '(0 0 .12) :shadow nil)
             (shape bw :box '(5.0 110.0 5.0) "#b8902e" 0 100.0 0 :shadow nil)
             (shape wheel :cyl '(42.0 42.0 5.0 16) "#8a6a1e" 0 0 0 :rot (list (/ pi 2) 0 0) :shadow nil)
             (dotimes (k 8)
               (shape wheel :box '(12.0 12.0 9.0) "#3a3a3c" (* 46.0 (cos (* k .785))) (* 46.0 (sin (* k .785))) 0 :rot (list 0 0 (* k .785)) :shadow nil))
             (live :spin wheel .3 22.0)
             (shape bw :box '(6.0 3.0 3.0) "#f4f6ff" -20.0 52.0 31.0 :emit "#e8f0ff" :energy 3 :shadow nil)
             (light bw "#dfe8ff" 2.6 260.0 -120.0 110.0 40.0))))

(defun dam-hall-p (i) "Return true if cell I of a dam is in its machine hall, not on its crest." (>= (vr (floor i 16) 79.0) .6))

(defzone zone-dam                                                            ; a dam: along its crest, water on one side and a fall on the other; then through its machine hall
         (if (dam-hall-p i)
             (progn
               (shape g :box (list 210.0 1.0 c) "#8a8478" -1.0 -.55 mid :shadow nil)
               (dolist (x '(-22.0 20.0)) (shape g :box (list 1.0 .03 c) "#c9a13a" x -.02 mid :shadow nil))
               (dolist (side '(-1 1))                                     ; the generators, one after another down both sides
                 (let ((x (+ -1.0 (* side 58.0))))
                   (shape g :cyl '(27.0 27.0 1.0 18) "#55504a" x .1 -21.0 :shadow nil)
                   (shape g :cyl '(24.0 24.0 10.0 18) "#a3262c" x 5.0 -21.0 :shadow nil)
                   (shape g :cyl '(15.0 17.0 8.0 18) "#d8d2c4" x 14.0 -21.0 :shadow nil)
                   (shape g :cyl '(5.0 5.0 10.0 10) "#c9a13a" x 23.0 -21.0 :metal .5 :shadow nil)
                   (shape g :box '(.5 2.0 2.0) (if (< (r (+ 40 side)) .8) "#3ad06a" "#ff3020") (- x (* side 24.2)) 6.0 -21.0
                          :emit (if (< (r (+ 40 side)) .8) "#3ad06a" "#ff2010") :energy 2.5 :shadow nil)
                   (shape g :box (list 4.0 160.0 c) "#cfc8b4" (+ -1.0 (* side 106.0)) 80.0 mid :emit "#b9c8c8" :energy (if (> side 0) .28 .08) :shadow nil)))
               (shape g :box '(.6 96.0 14.0) "#cfe6ee" -104.6 78.0 -21.0 :emit "#b9d6de" :energy 1.8 :unshaded t :shadow nil)   ; the day comes in on one side only
               (when (evenp i) (light g "#dfe8ff" 4.5 240.0 -84.0 60.0 -21.0))
               (shape g :box (list 214.0 4.0 c) "#39474d" -1.0 152.0 mid :shadow nil)
               (shape g :box '(210.0 6.0 2.0) "#2a2c30" -1.0 146.0 0 :shadow nil)
               (when (= (mod at 8) 2)                                     ; the crane of the hall, a rotor on its hook
                 (shape g :box '(206.0 10.0 10.0) "#c9a13a" -1.0 120.0 -8.0 :shadow nil)
                 (shape g :box '(.6 40.0 .6) "#2a2c30" 30.0 96.0 -8.0 :shadow nil)
                 (shape g :cyl '(16.0 16.0 26.0 14) "#8a8f93" 30.0 64.0 -8.0 :metal .4 :shadow nil))
               (when (= (mod at 8) 6)
                 (shape g :box '(150.0 14.0 .6) "#a3262c" -1.0 112.0 -30.0 :shadow nil)
                 (sign g "ПЛЮС ЭЛЕКТРИФИКАЦИЯ ВСЕЙ СТРАНЫ" -1.0 112.0 -29.5 :h 6.0 :rgb "#f0ece0")))
             (progn
               (shape g :box (list 40.0 330.0 (+ c 1.0)) "#6f6e69" 5.0 -165.1 mid :shadow nil)                        ; the body of the dam, and the road on top of it
               (dolist (x '(-10.6 8.6)) (shape g :box (list .5 1.2 c) "#8a887f" x .6 mid :shadow nil))
               (shape g :box (list 900.0 2.0 (+ c 30.0)) "#1e3a44" -465.0 -5.0 mid :rough .12 :metal .3 :shadow nil)   ; the water held back
               (when (evenp i)                                            ; a gate tower over the water
                 (shape g :box '(10.0 30.0 12.0) "#8a887f" -20.0 3.0 -21.0 :shadow nil)
                 (shape g :box '(.5 2.0 4.0) "#ffe2a8" -14.8 10.0 -21.0 :emit "#ffcf8a" :energy 3 :shadow nil)
                 (light g "#ffcf8a" 1.8 50.0 -8.0 9.0 -21.0))
               (shape g :box (list 340.0 8.0 (+ c 1.0)) "#77756f" 82.5 -160.0 mid :rot '(0 0 -1.2257) :shadow nil)     ; the face going down
               (if (< (mod at 4) 2)
                   (progn                                                 ; a spillway open: a sheet of white water down the face
                     (shape g :box (list 340.0 1.0 (- c 8.0)) "#dfeaf0" 87.5 -158.0 mid :rot '(0 0 -1.2257) :emit "#b9d6de" :energy .7 :shadow nil)
                     (shape g :ball '(40.0 10 6) "#dfeaf0" 160.0 -318.0 mid :scale '(1.4 .35 .5) :alpha .5 :unshaded t :shadow nil))
                   (progn                                                 ; or the power house at its foot
                     (shape g :box (list 70.0 56.0 c) "#8a887f" 185.0 -292.0 mid :shadow nil)
                     (shape g :box (list 6.0 .5 (- c 10.0)) "#ffe2a8" 185.0 -263.6 mid :emit "#ffcf8a" :energy 2.5 :unshaded t :shadow nil)))
               (shape g :box (list 900.0 2.0 (+ c 1.0)) "#1e3a44" 590.0 -321.0 mid :rough .12 :metal .3 :shadow nil)   ; the river below
               (shape g :box '(.2 7.0 .2) "#3a3c40" 8.9 3.5 -8.0 :shadow nil)
               (shape g :box '(.9 .3 .4) "#ffe2a8" 8.5 7.0 -8.0 :emit "#ffcf8a" :energy 3 :shadow nil)
               (when (oddp i) (light g "#ffcf8a" 1.6 36.0 6.0 6.0 -8.0))
               (when (= (mod at 8) 4)                                     ; a gantry crane astride the road
                 (dolist (x '(-14.0 12.0)) (dolist (z '(-12.0 -30.0)) (shape g :box '(3.0 36.0 3.0) "#c9a13a" x 18.0 z :shadow nil)))
                 (shape g :box '(32.0 6.0 22.0) "#c9a13a" -1.0 39.0 -21.0 :shadow nil)
                 (shape g :box '(.5 16.0 .5) "#2a2c30" -16.0 30.0 -21.0 :shadow nil)
                 (sign g "СЛАВА ПОКОРИТЕЛЯМ РЕКИ" -1.0 39.0 -9.8 :h 2.6 :rgb "#2a2420")))))

(defzone zone-reactor                                                        ; a power unit after the accident: a gallery along the wall of its hall, the roof gone over the reactor
         (let ((hole (<= 7 at 9)))
           (shape g :box (list 16.0 1.0 c) "#5f5e5a" -1.0 -.55 mid :shadow nil)                                       ; the gallery
           (shape g :box '(3.0 62.0 3.0) "#4a4c50" 4.0 -31.0 0 :shadow nil)
           (unless (< (r 1) .3) (shape g :box (list .25 1.1 c) "#3a3c40" 6.9 .55 mid :shadow nil))                    ; its rail, where it is left
           (shape g :box (list 4.0 320.0 c) "#4f5458" -11.0 80.0 mid :shadow nil)
           (shape g :box (list 430.0 2.0 c) "#3a3a3c" 222.0 -61.0 mid :shadow nil)                                    ; the floor of the hall
           (shape g :box (list 6.0 320.0 c) "#4f5458" 438.0 80.0 mid :shadow nil)
           (unless hole (shape g :box (list 452.0 6.0 c) "#2a3034" 213.0 200.0 mid :shadow nil))                      ; the roof
           (when (zerop (mod at 4))                                       ; an emergency lamp
             (shape g :box '(.4 .6 1.2) "#ff3020" -8.8 4.0 mid :emit "#ff2010" :energy 2.5 :shadow nil)
             (light g "#ff5a40" 1.3 34.0 -6.0 4.0 mid))
           (dotimes (k 3)                                                 ; blocks of graphite thrown about
             (let ((w (+ 3.0 (* 10.0 (r (+ 10 k))))))
               (shape g :box (list w w w) "#1c1c1e" (+ 20.0 (* 380.0 (r (+ 20 k)))) (- (/ w 2) 60.5) (* (- c) (r (+ 30 k))) :rot (list (r (+ 40 k)) (* 3.0 (r (+ 50 k))) (r (+ 60 k))) :shadow nil)))
           (when (< (r 2) .4)                                             ; a beam of the roof, fallen and leaning
             (shape g :box '(7.0 7.0 230.0) "#6a4a3a" (+ 80.0 (* 250.0 (r 3))) 40.0 mid :rot (list (+ .9 (* .4 (r 4))) (* 3.0 (r 5)) 0) :shadow nil))
           (when (= (mod at 4) 1)                                         ; a drum of the plant, still standing
             (shape g :cyl '(26.0 26.0 150.0 14) "#8a8f93" (+ 110.0 (* 260.0 (r 6))) 15.0 mid :shadow nil))
           (when (= (mod at 8) 3)                                         ; a board of dials on our wall, three lamps still lit
             (shape g :box '(.3 6.0 24.0) "#6f7a70" -8.9 4.0 mid :shadow nil)
             (dotimes (k 3) (shape g :box '(.3 .3 .3) (if (= k 1) "#3ad06a" "#ff3020") -8.7 (+ 3.0 k) (- mid (* 6.0 (- k 1))) :emit (if (= k 1) "#3ad06a" "#ff2010") :energy 2.5 :shadow nil))
             (sign g "БЛОЧНЫЙ ЩИТ УПРАВЛЕНИЯ" -8.7 7.8 mid :h .7 :rgb "#f0ece0" :yaw (/ pi 2)))
           (when (= at 8)                                                 ; the reactor: its pit, its lid stood on edge, the day falling in from above
             (shape g :cyl '(72.0 72.0 8.0 20) "#2a2c30" 215.0 -57.0 mid :shadow nil)
             (shape g :cyl '(60.0 60.0 .5 20) "#0a0c0e" 215.0 -52.8 mid :shadow nil)
             (shape g :cyl '(28.0 28.0 .4 16) "#6ac8ff" 215.0 -52.4 mid :emit "#6ac8ff" :energy 1.4 :unshaded t :shadow nil)
             (light g "#8ad8ff" 2.4 170.0 215.0 -20.0 mid)
             (shape g :cyl '(62.0 62.0 10.0 20) "#6a6c70" 262.0 14.0 (- mid 14.0) :rot '(.3 0 1.15) :shadow nil)
             (dotimes (k 6)
               (shape g :cyl '(1.6 1.6 70.0 6) "#8a8f93" (+ 240.0 (* 40.0 (r (+ 70 k)))) (+ 10.0 (* 50.0 (r (+ 80 k)))) (- mid (* 60.0 (- (r (+ 90 k)) .5)))
                      :rot (list (* 2.0 (r (+ 100 k))) 0 (+ .6 (r (+ 110 k)))) :shadow nil))
             (shape g :cyl '(95.0 95.0 420.0 16) "#cfd8e6" 215.0 150.0 mid :alpha .05 :unshaded t :shadow nil)
             (light g "#cfe2ea" 6.0 420.0 215.0 70.0 mid))))

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
        (:columns (mega-ground g i "#5f5e5a")
                  (shape g :box (list 520.0 4.0 c) "#4a4946" -1.8 182.0 mid :shadow nil))                    ; the level above
        ((:lift :metro :quarry :dam :reactor) nil)                                                           ; built below, each by its zone
        (:poles (shape g :box (list 5.0 .3 c) "#4a4c50" 0.0 -.2 mid :shadow nil)                               ; a hanging gangway
                (dolist (x '(-2.6 2.6))
                  (shape g :box (list .3 .5 c) "#3a3c40" x .1 mid :shadow nil)
                  (shape g :box '(.12 300.0 .12) "#3a3c40" x 150.0 0 :shadow nil)))
        (:pipes nil)                                                                                         ; a pipe, on it or in it: built below
        (:archive (floor-slab 62.0 "#8a6a4a" -1.0) (shape g :box (list 62.0 3.0 c) "#5a3e26" -1.0 -2.55 mid :shadow nil))   ; the board of a shelf
        (:paper (floor-slab 900.0 "#d8d2c0")                                                                 ; a sheet of ruled paper
                (loop for z from 0.0 below c by 10.5 do (shape g :box '(900.0 .02 .35) "#9fb0c8" -1.8 -.03 (- z) :shadow nil))
                (shape g :box (list .5 .02 c) "#a3262c" 90.0 -.03 mid :shadow nil)                           ; the margin
                (live :ink (shape g :box (list 6.0 .04 c) "#1a2a6a" 0.0 -.02 mid :rough .2 :shadow nil)))   ; the line the pen has written: our road
        (:statues (mega-ground g i "#8a887f"))
        (:factory (shape g :box (list 13.0 1.2 c) "#1e1f22" -1.0 -.65 mid :shadow nil)                       ; a conveyor belt, and we are on it
                  (dolist (x '(-8.2 6.2)) (shape g :box (list 1.0 2.4 c) "#c9a13a" x -.6 mid :shadow nil))
                  (loop for z from 3.0 below c by 14.0 do (shape g :cyl '(1.0 1.0 15.0 8) "#5a5e60" -1.0 -2.3 (- z) :rot (list 0 0 (/ pi 2)) :shadow nil))
                  (shape g :box '(4.0 110.0 4.0) "#8a6a1e" -1.0 -58.0 0 :shadow nil))
        (:communal (if (< (vr (floor i 12) 78.0) .5)
                       (progn (floor-slab 70.0 "#5a3e26" -2.0)                                               ; inside: the floorboards of the corridor
                              (loop for x from -34.0 to 30.0 by 8.0 do (shape g :box (list .2 .02 c) "#3c2a20" x -.03 mid :shadow nil)))
                       (progn (shape g :box (list 12.0 .8 c) "#6f6e69" -3.0 -.45 mid :shadow nil)            ; outside: a gallery over the yard
                              (shape g :box (list .25 1.1 c) "#3a3c40" 3.1 .55 mid :shadow nil))))
        (:garages (shape g :box (list 10.0 1.0 c) "#55524f" -1.0 -.55 mid :shadow nil)                       ; a flyover across the co-operative
                  (dolist (x '(-6.2 4.2)) (shape g :box (list .3 .9 c) "#4a4c50" x .4 mid :shadow nil))
                  (shape g :box '(3.0 70.0 3.0) "#5a5a58" -1.0 -36.0 0 :shadow nil))
        (:spiral (floor-slab 17.0 "#5f5e5a" -1.0))                                                           ; a ramp along a wall
        (:rail (live :drop (shape g :box (list 12.0 12.0 (- c .8)) (nth (mod i 5) '("#5a2a22" "#3c5a48" "#55708a" "#a8843c" "#6a3a5a")) 0.0 -6.05 mid :shadow nil)
                     0.0 -6.05 mid)))                                                                        ; a container set down for us to drive over
      (unless (member kind '(:graves :spiral :communal :factory :archive :paper :lift :metro :quarry :reactor))   ; we are indoors: a ceiling, far walls and a bottom, and piers between
        (shape g :box (list 1200.0 8.0 c) "#161c20" -1.8 470.0 mid :shadow nil)
        (dolist (x '(-600.0 596.0)) (shape g :box (list 10.0 900.0 c) "#1c2428" x 30.0 mid :shadow nil))
        (when (member kind '(:pipes :poles :garages :rail)) (shape g :box (list 1200.0 6.0 c) "#12171a" -1.8 -420.0 mid :shadow nil))
        (when (zerop (mod i 3))                                          ; the day comes in only through slits
          (shape g :box (list (+ 120.0 (* 300.0 (r 420))) 1.0 4.0) "#cfe6ee" (- (* 800.0 (r 421)) 400.0) 465.0 (* (- c) (r 422))
                 :emit "#b9d6de" :energy 2.2 :unshaded t :shadow nil))
        (when (zerop (mod i 4))
          (let ((side (if (< (r 430) .5) -1 1)))
            (shape g :box (list 1.0 (+ 60.0 (* 220.0 (r 431))) 3.0) "#cfe6ee" (* side 594.0) (- (* 500.0 (r 432)) 150.0) (* (- c) (r 433))
                   :emit "#b9d6de" :energy 2.2 :unshaded t :shadow nil)))
        (when (member kind '(:pipes :poles))                             ; and far below, a lamp or two on the bottom
          (dotimes (k 2)
            (shape g :box '(2.0 2.0 2.0) "#ffcf8a" (- (* 700.0 (r (+ 440 k))) 350.0) -414.0 (* (- c) (r (+ 450 k))) :emit "#ffb060" :energy 2.5 :unshaded t :shadow nil)))
        (when (zerop (mod i 3))
          (let ((side (if (< (r 400) .5) -1 1)))
            (shape g :box (list (+ 30.0 (* 40.0 (r 403))) 900.0 (+ 30.0 (* 40.0 (r 404)))) "#39474d" (* side (+ 300.0 (* 260.0 (r 401)))) 25.0 (* (- c) (r 402)) :shadow nil))))
      (when (and (= at 8) (not (eq kind :lift)))                         ; one of our bus stops every so often, under a lamp
        (let ((stop (group g *stop-x* 0.0 -20.0)))
          (shape g :box '(9.0 .5 14.0) "#6f6e69" 8.6 -.3 -20.0 :shadow nil)
          (build-stop stop (* 7.0 (floor i 16)))
          (shape g :box '(.14 5.6 .14) "#3a3c40" 5.0 2.8 -15.0 :shadow nil)
          (shape g :box '(.6 .2 .4) "#ffe2a8" 5.3 5.6 -15.0 :emit "#ffcf8a" :energy 3 :shadow nil)
          (light g "#ffcf8a" 2.2 26.0 5.6 5.0 -15.0)))
      ;; Everything here is out of scale with us: a machine is a tower block, a door is seven storeys, a bust's head
      ;; is a house. Now and then a thing of our own size stands by - a door, a lamp, a whole five-storey house
      ;; hanging from a crane - so that the eye can measure.
      (ecase kind
        (:columns (mega-columns g i (zerop (mod i 6))))
        (:lift (zone-lift g i c mid at))
        (:metro (zone-metro g i c mid at))
        (:quarry (zone-quarry g i c mid at))
        (:dam (zone-dam g i c mid at))
        (:reactor (zone-reactor g i c mid at))
        (:poles (zone-poles g i c mid at))
        (:pipes (zone-pipes g i c mid at))
        (:graves nil)
        (:archive (zone-archive g i c mid at))
        (:paper (zone-paper g i c mid at))
        (:crater (zone-crater g i c mid at))
        (:statues (zone-statues g i c mid at))
        (:factory (zone-factory g i c mid at))
        (:communal (zone-communal g i c mid at))
        (:garages (zone-garages g i c mid at))
        (:spiral (zone-spiral g i c mid at))
        (:rail (zone-rail g i c mid at))))
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
    ;; the wall the tunnel comes out of: it goes up and down and to the right out of sight
    (shape entry :box '(1700.0 600.0 6.0) "#39474d" 800.0 312.0 -197.0 :shadow nil)
    (shape entry :box '(1700.0 1000.0 6.0) "#39474d" 800.0 -501.0 -197.0 :shadow nil)
    (shape entry :box '(1600.0 14.0 6.0) "#39474d" 814.0 6.0 -197.0 :shadow nil)
    ;; The light of the place has a source: far off and high up there is a breach, and the day comes in through it
    ;; from one side. It hangs in the megastructure's own frame, so as the way turns it swings round us like a sun.
    (let ((sun (gdext:make-object "DirectionalLight3D")))
      (object-set sun "light_color" (hex "#b9d6de")) (object-set sun "light_energy" 0.0) (object-set sun "shadow_enabled" nil)
      (node-add-child turn sun)
      (node-3d-set-rotation sun (v3 -.36 .85 0))                 ; low: it rakes the sides of things and hardly touches the floors
      (shape turn :ball '(70.0 12 8) "#cfe6ee" -430.0 240.0 -380.0 :emit "#b9d6de" :energy 2.0 :unshaded t :shadow nil)   ; the breach itself
      (let ((pen (group g 0.0 0.0 (- *mega-pen*))) (crane (group g 0.0 0.0 -150.0)) (lift (group g)))
        ;; the platform that carries us down from the tunnel: we stand on it and look about
        (shape lift :box '(22.0 1.2 34.0) "#4a4c50" -1.8 -.72 0.0 :shadow nil)
        (dolist (x '(-12.6 9.0)) (shape lift :box '(.5 .14 34.0) "#c9a13a" x -.1 0.0 :shadow nil))
        (dolist (z '(-16.8 16.8)) (shape lift :box '(22.0 .14 .5) "#c9a13a" -1.8 -.1 z :shadow nil))
        (shape lift :box '(.2 1.1 34.0) "#3a3c40" 9.1 .5 0.0 :shadow nil)
        (dolist (x '(-12.4 8.8))
          (dolist (z '(-16.6 16.6))
            (shape lift :box '(.3 2.4 .3) "#3a3c40" x 1.1 z :shadow nil)
            (shape lift :box '(.4 .4 .4) "#ff3020" x 2.5 z :emit "#ff2010" :energy 2.5 :shadow nil)))
        (shape lift :box '(8.0 3.0 20.0) "#3a3c40" -15.0 -2.0 0.0 :shadow nil)
        (object-set lift "visible" nil)
        ;; the pen that writes our road: its nib on the paper far ahead, the barrel leaning back over us
        (let ((lean (group pen)))
          (node-3d-set-rotation lean (v3 .55 .3 0))
          (shape lean :cyl '(13.0 .5 60.0 12) "#c9a13a" 0 30.0 0 :metal .5 :shadow nil)
          (shape lean :cyl '(15.0 15.0 620.0 12) "#1a2a6a" 0 370.0 0 :shadow nil)
          (shape lean :cyl '(16.0 16.0 30.0 12) "#c9a13a" 0 75.0 0 :metal .5 :shadow nil))
        ;; the crane that sets the road down: a gantry astride it, a container on its ropes
        (dolist (x '(-46.0 46.0)) (shape crane :box '(7.0 260.0 7.0) "#c9a13a" x 40.0 0 :shadow nil))
        (shape crane :box '(110.0 9.0 9.0) "#c9a13a" 0 172.0 0 :shadow nil)
        (shape crane :box '(16.0 10.0 16.0) "#8a6a1e" 0 162.0 0 :shadow nil)
        (dolist (dz '(-14.0 14.0)) (shape crane :box '(.5 130.0 .5) "#2a2c30" 0 94.0 dz :shadow nil))
        (object-set pen "visible" nil) (object-set crane "visible" nil)
        (clrhash *mega-cells*) (clrhash *mega-poses*) (clrhash *mega-lives*)
        (setf *mega-parts* (list :cells cells :turn turn :tilt tilt :entry entry :sun sun :pen pen :crane crane :lift lift :at nil) *mega-s* 0.0)))
    g))

(defun step-mega-cells ()
  "Keep the cells around the wagon built: some way behind, far ahead, no more than two new ones a frame."
  (let* ((c *mega-cell*) (cells (getf *mega-parts* :cells))
         (lo (floor (- *mega-s* 170.0) c)) (hi (floor (+ *mega-s* 620.0) c)) (fresh 0))
    ;; where we are on the way, and which way we face there: the megastructure is turned and moved the other way
    (let* ((i (floor *mega-s* c)) (f (- (/ *mega-s* c) i)) (a (mega-pose i)) (b (mega-pose (1+ i))))
      (flet ((mix (k) (+ (nth k a) (* f (- (nth k b) (nth k a))))))
        (node-3d-set-rotation (getf *mega-parts* :tilt) (v3 (- (mix 4)) 0 0))
        (node-3d-set-rotation (getf *mega-parts* :turn) (v3 0 (- (mix 3)) 0))
        (node-3d-set-position cells (v3 (- (mix 0)) (- (mix 1)) (- (mix 2))))))
    (object-set (getf *mega-parts* :sun) "light_energy" (* (getf *mega-sun* (mega-kind (floor *mega-s* c)) .5) *haze*))
    (let ((here (mega-kind (floor *mega-s* c))) (there (mega-kind (floor (+ *mega-s* *mega-pen*) c))))
      (object-set (getf *mega-parts* :pen) "visible" (and (eq here :paper) (eq there :paper)))
      (object-set (getf *mega-parts* :crane) "visible" (and (eq here :rail) (eq there :rail)))
      (object-set (getf *mega-parts* :lift) "visible" (and (eq here :lift) (> *mega-s* 196.0))))
    (step-mega-lives)
    (unless (eql (getf *mega-parts* :at) (floor *mega-s* c))             ; nothing to do while we stay in one cell and all is built
      (loop for i being the hash-keys of *mega-cells* using (hash-value node)
            do (unless (<= (1- lo) i (1+ hi)) (node-queue-free node) (remhash i *mega-cells*) (remhash i *mega-lives*)))
      (loop for i from (floor *mega-s* c) to hi                           ; the nearest first
            while (< fresh 2)
            do (unless (gethash i *mega-cells*)
                 (let ((*mega-live-now* '()))
                   (setf (gethash i *mega-cells*) (build-mega-cell cells i) (gethash i *mega-lives*) *mega-live-now*))
                 (incf fresh)))
      (loop for i from (1- (floor *mega-s* c)) downto lo
            while (< fresh 2)
            do (unless (gethash i *mega-cells*)
                 (let ((*mega-live-now* '()))
                   (setf (gethash i *mega-cells*) (build-mega-cell cells i) (gethash i *mega-lives*) *mega-live-now*))
                 (incf fresh)))
      (when (zerop fresh) (setf (getf *mega-parts* :at) (floor *mega-s* c))))))

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
    (let ((want (getf *mega-fog* (mega-kind (floor *mega-s* *mega-cell*)) .0047)))             ; and so does its air
      (setf *fog-inside* (and in (+ (or *fog-inside* want) (* .012 (- want (or *fog-inside* want)))))))
    (cond ((and in (null *mega-node*)) (setf *mega-node* (build-mega)))
          ((and (not in) *mega-node*) (node-queue-free *mega-node*) (clrhash *mega-cells*) (clrhash *mega-lives*) (setf *mega-node* nil *mega-parts* nil)))
    (when (and in *mega-parts*)
      (step-mega-cells)
      (node-3d-set-position (getf *mega-parts* :entry) (v3 0 (- (mega-height *mega-s*)) *mega-s*))
      (object-set (getf *mega-parts* :entry) "visible" (< *mega-s* 900.0))
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

