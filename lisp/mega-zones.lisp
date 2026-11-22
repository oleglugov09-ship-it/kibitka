;;;; The megastructure: its everyday zones - late-Soviet things grown past all measure, and the way through each
;;;; is a part of the thing itself.

(in-package #:kiosk-trip)

(defzone zone-columns                                                        ; the level the old zones began with: a floor on columns, another above
         (mega-ground g i "#5f5e5a")
         (shape g :box (list 520.0 4.0 c) "#4a4946" -1.8 182.0 mid :shadow nil)
         (mega-columns g i (zerop (mod i 6))))

(defzone zone-poles                                                          ; panel houses, each on one pole, up and up
         (shape g :box (list 5.0 .3 c) "#4a4c50" 0.0 -.2 mid :shadow nil)                               ; a hanging gangway
         (dolist (x '(-2.6 2.6))
           (shape g :box (list .3 .5 c) "#3a3c40" x .1 mid :shadow nil)
           (shape g :box '(.12 300.0 .12) "#3a3c40" x 150.0 0 :shadow nil))
         (dotimes (k 3)
           (let* ((side (if (< (r (+ 1 k)) .5) -1 1)) (x (* side (+ 30.0 (* 250.0 (r (+ 3 k))))))
                  (up (+ 14.0 (* 280.0 (r (+ 5 k)) (r (+ 15 k))))) (z (- (* c (r (+ 7 k)))))
                  (h (group g x (+ up 7.5) z)))
             (shape g :cyl '(1.6 1.6 700.0 8) "#6a6c70" x (- up 350.0) z :shadow nil)
             (node-3d-set-rotation h (v3 0 (* 1.2 (- (r (+ 9 k)) .5)) 0))
             (shape h :box '(12.0 15.0 64.0) "#85857f" 0 0 0 :shadow nil)
             (when (< (r (+ 11 k)) .45)                                   ; somebody is at home
               (shape h :box '(.3 1.6 2.0) "#ffcf7a" (* side -6.1) 1.5 (- (* 40.0 (r (+ 13 k))) 20.0) :emit "#ffb866" :energy 3 :shadow nil)
               (when (< (abs x) 110.0) (light h "#ffb866" 2.2 46.0 (* side -9.0) 1.5 0.0))))))

(defzone zone-pipes                                                          ; a heating main; the road lies on one of its pipes
         (if (< (vr (floor i 8) 77.0) .5)
             (progn                                                       ; inside the pipe: its wall all round us, plates missing here and there
               (dotimes (k 10)
                 (let ((a (* k (/ pi 5))))
                   (unless (and (< 1 k 9) (< (r (+ 900 k)) .1))
                     (shape g :box (list 6.2 .5 c) (if (< (r (+ 910 k)) .2) "#6a4a3a" "#8a8f93") (* 9.2 (sin a)) (- 8.9 (* 9.2 (cos a))) mid :rot (list 0 0 a) :shadow nil))))
               (when (evenp i) (dotimes (k 10) (let ((a (* k (/ pi 5)))) (shape g :box '(6.4 1.4 1.6) "#5a5e60" (* 8.6 (sin a)) (- 8.9 (* 8.6 (cos a))) 0 :rot (list 0 0 a) :shadow nil))))
               (when (zerop (mod at 4)) (shape g :box '(.6 .3 1.6) "#ffe2a8" 0.0 17.2 mid :emit "#ffcf8a" :energy 3 :shadow nil) (light g "#ffcf8a" 1.6 30.0 0.0 14.0 mid)))
             (progn                                                       ; on its back
               (shape g :cyl (list 5.6 5.6 c 14) "#d4d6d2" 0.0 -5.62 (- (/ c 2)) :rot (list (/ pi 2) 0 0) :shadow nil)
               (when (evenp i) (shape g :cyl '(6.5 6.5 1.6 14) "#a9aca8" 0.0 -5.62 0 :rot (list (/ pi 2) 0 0) :shadow nil))))
         (when (zerop (mod i 3))                                          ; a pipe one could drive a train through
           (let ((rad (+ 22.0 (* 30.0 (r 15)))))
             (shape g :cyl (list rad rad 1400.0 16) (if (< (r 16) .5) "#aeb2ae" "#3a3a3e") -1.8
                    (if (< (r 17) .5) (+ 30.0 rad (* 150.0 (r 18))) (- -30.0 rad (* 150.0 (r 18)))) mid
                    :rot (list 0 (* 1.2 (- (r 19) .5)) (/ pi 2)) :shadow nil)))
         (dotimes (k 4)
           (let* ((rad (+ .8 (* 8.0 (r (+ 20 k)) (r (+ 30 k))))) (z (- (* c (r (+ 40 k)))))
                  (rgb (nth (floor (* 3 (r (+ 50 k)))) '("#c9ccc8" "#8a8f93" "#2a2a2e"))) (way (r (+ 60 k))))
             (cond ((< way .4)
                    (shape g :cyl (list rad rad 700.0 10) rgb -1.8 (if (< (r (+ 70 k)) .7) (+ 9.5 rad (* 90.0 (r (+ 80 k)))) (- -14.0 rad (* 90.0 (r (+ 80 k))))) z
                           :rot (list 0 0 (/ pi 2)) :shadow nil))
                   ((< way .75)
                    (shape g :cyl (list rad rad 900.0 10) rgb (* (if (< (r (+ 90 k)) .5) -1 1) (+ 11.0 rad (* 180.0 (r (+ 100 k))))) 0.0 z :shadow nil))
                   (t
                    (shape g :cyl (list rad rad 700.0 10) rgb (* (if (< (r (+ 90 k)) .5) -1 1) (+ 60.0 (* 160.0 (r (+ 100 k))))) (* 60.0 (r (+ 110 k))) z
                           :rot (list 0 (* 3.0 (r (+ 120 k))) 1.0) :shadow nil))))))

(defzone zone-archive                                                        ; along a shelf of the archive, between the files
         (mega-slab g 62.0 "#8a6a4a" -1.0) (shape g :box (list 62.0 3.0 c) "#5a3e26" -1.0 -2.55 mid :shadow nil)
         (dolist (side '(-1 1))
           (let ((z 0.0) (k 0))
             (loop while (< z (- c 3.0))                                  ; files of every thickness, standing close; a gap now and then
                   do (let* ((w (+ 3.0 (* 9.0 (r (+ 1000 (* 40 side) k))))) (h (+ 46.0 (* 34.0 (r (+ 1100 (* 40 side) k)))))
                             (x (* side (+ 9.5 26.0))) (lean (if (< (r (+ 1200 (* 40 side) k)) .12) (* .16 (- (r (+ 1300 k)) .5)) 0.0)))
                        (unless (< (r (+ 1400 (* 40 side) k)) .14)
                          (shape g :box (list 52.0 h (- w .3)) (nth (floor (* 5 (r (+ 1500 (* 40 side) k)))) '("#8a7458" "#6a5a48" "#55708a" "#7a6a52" "#5a4a3a"))
                                 x (/ h 2) (- (+ z (/ w 2))) :rot (list lean 0 0) :shadow nil)
                          (shape g :box (list .3 9.0 (* w .6)) "#e8e0d0" (- x (* side 26.1)) (* h .62) (- (+ z (/ w 2))) :shadow nil))    ; the label on its spine
                        (incf z w) (incf k)))))
         (shape g :box (list 130.0 4.0 c) "#5a3e26" -1.0 92.0 mid :shadow nil)                                        ; the shelf above
         (when (zerop (mod at 8)) (shape g :box '(8.0 96.0 3.0) "#5a3e26" -1.0 44.0 0 :shadow nil))                   ; an upright of the rack, off to the side
         (when (zerop (mod at 4))                                                                                     ; a lamp under the shelf
           (shape g :box '(3.0 .5 1.2) "#ffe2a8" -1.0 89.5 mid :emit "#ffcf8a" :energy 3 :shadow nil)
           (light g "#ffcf8a" 2.4 110.0 -1.0 70.0 mid))
         (when (= (mod at 8) 3) (sign g "ДЕЛО №" 9.3 30.0 mid :h 5.0 :rgb "#2a2420"))
         (when (< (r 1) .1)                                                                                           ; a file pulled out and left on the board
           (shape g :box '(8.0 6.0 30.0) "#7a6a52" (* (if (< (r 2) .5) -1 1) 6.0) 3.0 mid :rot (list 0 (* .5 (- (r 3) .5)) 0) :shadow nil)))

(defzone zone-paper                                                          ; a letter being written: what stands on the desk, far off
         (mega-slab g 900.0 "#d8d2c0")                                                                 ; a sheet of ruled paper
         (loop for z from 0.0 below c by 10.5 do (shape g :box '(900.0 .02 .35) "#9fb0c8" -1.8 -.03 (- z) :shadow nil))
         (shape g :box (list .5 .02 c) "#a3262c" 90.0 -.03 mid :shadow nil)                           ; the margin
         (live :ink (shape g :box (list 6.0 .04 c) "#1a2a6a" 0.0 -.02 mid :rough .2 :shadow nil))
         (when (zerop (mod i 11))
           (let ((x (* (if (< (r 1) .5) -1 1) (+ 220.0 (* 160.0 (r 2))))))
             (shape g :cyl '(46.0 60.0 80.0 14) "#1c2a3a" x 40.0 mid :rough .3 :shadow nil)                           ; an inkwell
             (shape g :cyl '(30.0 30.0 6.0 14) "#0a0f1e" x 82.0 mid :shadow nil)))
         (when (zerop (mod i 7))                                                                                      ; a pencil lying across the sheet
           (shape g :cyl '(7.0 7.0 520.0 6) "#c9a13a" (* (if (< (r 3) .5) -1 1) (+ 120.0 (* 200.0 (r 4)))) 7.0 mid :rot (list (/ pi 2) (* 1.2 (- (r 5) .5)) 0) :shadow nil))
         (when (zerop (mod i 5))                                                                                      ; lines already written, to either side
           (let ((x (* (if (evenp (floor i 5)) -1 1) (+ 40.0 (* 60.0 (r 6))))))
             (dotimes (k 6) (shape g :box (list (+ 6.0 (* 14.0 (r (+ 10 k)))) .03 1.2) "#1a2a6a" (+ x (* 24.0 k (if (plusp x) 1 -1))) -.02 (- (* 6.0 (r (+ 20 k)))) :shadow nil)))))

(defzone zone-statues                                                        ; the avenue of labour glory: busts whose heads are houses
         (mega-ground g i "#8a887f")
         (when (< (r 20) .62)
         (let* ((side (if (< (r 21) .5) -1 1)) (h (+ 20.0 (* 140.0 (r 1) (r 22)))) (x (* side (+ 30.0 (* 150.0 (r 2) (r 23))))))
           (shape g :box (list 24.0 (+ h 80.0) 24.0) "#8a887f" x (- (/ h 2) 40.0) -20.0 :rot (list 0 (* 1.5 (r 24)) 0) :shadow nil)
           (shape g :box '(16.0 14.0 34.0) "#d8d6cc" x (+ h 7.0) -20.0 :shadow nil)
           (shape g :ball '(11.0 12 10) "#d8d6cc" x (+ h 23.0) -20.0 :shadow nil)
           (shape g :box '(.4 6.0 15.0) "#a3262c" (- x (* side 12.2)) (* h .5) -20.0 :shadow nil)       ; a red plate with the name
           (shape g :box '(.3 2.1 1.0) "#3c2a20" (- x (* side 12.2)) 1.05 -27.0 :shadow nil)            ; a door of our size at its foot
           (shape g :box '(.4 .3 .3) "#ffe2a8" (- x (* side 12.3)) 2.6 -27.0 :emit "#ffcf8a" :energy 3 :shadow nil)
           (when (evenp (floor i 2)) (light g "#dfe8ff" 1.6 90.0 (- x (* side 24.0)) 12.0 -20.0))
           (when (< (r 3) .3)                                             ; and one beyond measure, far off
             (let ((bx (* (- side) (+ 260.0 (* 200.0 (r 4))))))
               (shape g :box '(120.0 420.0 120.0) "#8a887f" bx 210.0 -10.0 :shadow nil)
               (shape g :box '(100.0 90.0 210.0) "#c9c7be" bx 465.0 -10.0 :shadow nil)
               (shape g :ball '(78.0 16 12) "#c9c7be" bx 590.0 -10.0 :shadow nil))))))

(defzone zone-factory                                                        ; the conveyor: it carries us through the machines; the shop floor is far below
         (shape g :box (list 13.0 1.2 c) "#1e1f22" -1.0 -.65 mid :shadow nil)                       ; a conveyor belt, and we are on it
         (dolist (x '(-8.2 6.2)) (shape g :box (list 1.0 2.4 c) "#c9a13a" x -.6 mid :shadow nil))
         (loop for z from 3.0 below c by 14.0 do (shape g :cyl '(1.0 1.0 15.0 8) "#5a5e60" -1.0 -2.3 (- z) :rot (list 0 0 (/ pi 2)) :shadow nil))
         (shape g :box '(4.0 110.0 4.0) "#8a6a1e" -1.0 -58.0 0 :shadow nil)
         (when (= (mod at 4) 2)                                           ; a press astride the belt: we pass under its head
           (dolist (x '(-22.0 20.0)) (shape g :box '(12.0 150.0 18.0) "#3f6f73" x 60.0 mid :shadow nil))
           (shape g :box '(54.0 40.0 18.0) "#3f6f73" -1.0 150.0 mid :shadow nil)
           (live :press (shape g :box '(28.0 26.0 15.0) "#8a8f93" -1.0 60.0 mid :metal .4 :shadow nil) -1.0 mid 26.0 110.0 (+ 5.0 (* 4.0 (r 580))) (* 6.0 (r 581)))
           (shape g :box '(.6 1.6 1.6) "#ff3020" -15.7 12.0 mid :emit "#ff2010" :energy 2.5 :shadow nil))
         (when (= (mod at 4) 0)                                           ; a crate on the belt ahead of us, the size of a house
           (shape g :box '(9.0 8.0 12.0) "#a8843c" -1.0 4.0 -30.0 :rot (list 0 (* .3 (- (r 582) .5)) 0) :shadow nil))
         (let ((low (group g 0.0 -112.0 0.0)))
           (shape low :box (list 900.0 2.0 c) "#4f4e4a" -1.0 -1.0 mid :shadow nil)                                    ; the shop floor
           (shape low :box (list 3.0 .1 c) "#c9a13a" 40.0 .1 mid :shadow nil)                                         ; a yellow line on it, a road wide
           (dolist (side '(-1 1))
             (let* ((w (+ 60.0 (* 90.0 (r (+ 510 side))))) (h (+ 60.0 (* 260.0 (r (+ 520 side))))) (x (* side (+ 70.0 (/ w 2) (* 110.0 (r (+ 500 side))))))
                    (face (- x (* side (/ w 2)))))
               (when (zerop (mod (+ i (if (> side 0) 1 0)) 2))
                 (shape low :box (list w h (* 1.7 c)) (if (< (r (+ 540 side)) .5) "#3f6f73" "#55708a") x (/ h 2) mid :shadow nil)
                 (shape low :box (list (* w .6) 30.0 (* .8 c)) "#2a2c30" x (+ h 15.0) mid :shadow nil)
                 (shape low :cyl (list 9.0 9.0 (+ h 160.0) 10) "#8a8f93" (+ x (* side w .3)) (/ (+ h 160.0) 2) (- (* .3 c)) :shadow nil)
                 (dotimes (k 5)
                   (shape low :box '(.6 2.4 2.4) (if (evenp k) "#ff3020" "#ffcf7a") (- face (* side .4)) (+ 14.0 (* 16.0 k)) (- (* 9.0 k) 30.0)
                          :emit (if (evenp k) "#ff2010" "#ffb866") :energy 2.5 :shadow nil))
                 (light low "#ffcf8a" 2.0 90.0 (- face (* side 20.0)) 30.0 mid))))
           (when (zerop (mod i 2))                                                                                    ; lamps down on the floor, in rows
             (dolist (x '(-150.0 -40.0 60.0 170.0))
               (shape low :box '(1.6 .6 1.6) "#ffe2a8" x 14.0 -10.0 :emit "#ffcf8a" :energy 3 :shadow nil)
               (shape low :box '(.3 14.0 .3) "#3a3c40" x 7.0 -10.0 :shadow nil))))
         (dolist (x '(-460.0 456.0))                                                                                  ; the far walls, with their rows of windows
           (shape g :box (list 10.0 900.0 c) "#39474d" x 240.0 mid :shadow nil)
           (dotimes (k 3)
             (shape g :box '(.8 9.0 14.0) "#b9d6de" (if (< x 0) (+ x 5.4) (- x 5.4)) (- (* 520.0 (r (+ 560 k (round x)))) 100.0) (* (- c) (r (+ 570 k (round x))))
                    :emit "#b9d6de" :energy 1.6 :unshaded t :shadow nil)))
         (when (zerop (mod at 8))                                                                                    ; another crane abreast of us, and what it carries
           (let ((cx (+ 90.0 (* 200.0 (r 550)))))
             (shape g :box '(900.0 12.0 12.0) "#c9a13a" -2.0 60.0 -10.0 :shadow nil)
             (shape g :box '(30.0 18.0 30.0) "#c9a13a" cx 46.0 -10.0 :shadow nil)
             (dolist (dz '(-20.0 20.0)) (shape g :box '(.6 60.0 .6) "#2a2c30" cx 8.0 (+ -10.0 dz) :shadow nil))
             (shape g :box '(12.0 15.0 64.0) "#85857f" cx -28.0 -10.0 :shadow nil)))                                 ; a whole five-storey house on the hook
         (shape g :box (list 120.0 2.0 c) "#dfeaf0" (* (if (evenp i) -1 1) 150.0) 640.0 mid :emit "#b9d6de" :energy 1.6 :unshaded t :shadow nil)   ; skylights far above
         (when (= (mod at 8) 4)                                                                                       ; a banner across the hall
           (shape g :box '(2.0 44.0 300.0) "#a3262c" 200.0 90.0 mid :shadow nil)
           (sign g (nth (mod (floor i 8) 5) '("СЛАВА ТРУДУ!" "ПЛАН — ЗАКОН" "ПЯТИЛЕТКУ — В ЧЕТЫРЕ ГОДА" "ВЫШЕ ЗНАМЯ СОРЕВНОВАНИЯ" "ЭКОНОМИКА ДОЛЖНА БЫТЬ ЭКОНОМНОЙ"))
                 198.8 90.0 mid :h 20.0 :rgb "#f0ece0")))

(defzone zone-communal                                                       ; a communal flat without end: its corridor, then a gallery over its yard, then the corridor again
         (if (< (vr (floor i 12) 78.0) .5)
             (progn (mega-slab g 70.0 "#5a3e26" -2.0)                                               ; inside: the floorboards of the corridor
                    (loop for x from -34.0 to 30.0 by 8.0 do (shape g :box (list .2 .02 c) "#3c2a20" x -.03 mid :shadow nil)))
             (progn (shape g :box (list 12.0 .8 c) "#6f6e69" -3.0 -.45 mid :shadow nil)            ; outside: a gallery over the yard
                    (shape g :box (list .25 1.1 c) "#3a3c40" 3.1 .55 mid :shadow nil)))
         (when (< (vr (floor i 12) 78.0) .5)
           (dolist (x '(-36.0 32.0))
             (shape g :box (list 2.0 130.0 c) "#6a7a5a" x 65.0 mid :shadow nil)
             (shape g :box (list 2.2 40.0 c) "#3c5a48" x 20.0 mid :shadow nil))
           (shape g :box (list 72.0 2.0 c) "#b8ac94" -2.0 131.0 mid :shadow nil)
           (dolist (side '(-1 1))
             (let ((x (if (< side 0) -34.8 30.8)) (lit (< (r (+ 600 side)) .35)) (open (< (r (+ 650 side)) .18)))
               (if open                                                   ; a door standing open: a room beyond, with its own light
                   (progn (shape g :box '(.6 24.0 10.0) "#16171a" x 12.0 -21.0 :shadow nil)
                          (shape g :box '(.7 18.0 7.0) "#ffcf7a" (+ x (* side .2)) 11.0 -21.0 :emit "#ffb866" :energy 2.0 :shadow nil)
                          (light g "#ffb866" 2.0 60.0 (- x (* side 8.0)) 10.0 -21.0))
                   (shape g :box '(.6 24.0 10.0) "#5a3e26" x 12.0 -21.0 :shadow nil))
               (shape g :box '(.7 5.0 10.0) (if lit "#ffcf7a" "#2a2c30") x 27.5 -21.0 :emit (and lit "#ffb866") :energy 2.5 :shadow nil)
               (dotimes (k 4) (shape g :box '(.6 1.4 1.0) "#16171a" x (+ 8.0 (* 2.0 k)) -28.5 :shadow nil))))
           (shape g :cyl '(.15 .15 40.0 4) "#2a2a2e" -2.0 110.0 mid :shadow nil)
           (shape g :ball '(2.6 10 8) "#fff0c8" -2.0 88.0 mid :emit "#ffe2a8" :energy 3 :shadow nil)
           (light g "#ffd9a0" 2.4 120.0 -2.0 84.0 mid)
           (when (zerop (mod at 3))                                       ; things left in the corridor: a trunk, a bicycle wheel, a basin
             (shape g :box '(9.0 6.0 14.0) "#5a2a22" (if (< (r 660) .5) -27.0 23.0) 3.0 -34.0 :shadow nil)
             (shape g :cyl '(8.0 8.0 .6 14) "#8a8f93" (if (< (r 661) .5) -33.6 29.6) 30.0 -8.0 :rot (list 0 0 (/ pi 2)) :shadow nil)))
         (unless (< (vr (floor i 12) 78.0) .5)
         (shape g :box (list 4.0 700.0 c) "#6a7a5a" -11.0 0.0 mid :shadow nil)                                        ; our wall, storeys up and storeys down
         (loop for z from 4.0 below c by 10.0 for k from 0                                                            ; doors of the flats along the gallery
               do (let ((lit (< (r (+ 600 k)) .35)))
                    (shape g :box '(.3 2.1 1.0) "#5a3e26" -8.9 1.05 (- z) :shadow nil)
                    (shape g :box '(.3 1.2 1.4) (if lit "#ffcf7a" "#2a2c30") -8.9 1.6 (- (+ z 2.4)) :emit (and lit "#ffb866") :energy 2.5 :shadow nil)))
         (when (evenp i) (shape g :box '(.3 .3 .3) "#fff0c8" -8.6 3.0 mid :emit "#ffe2a8" :energy 3 :shadow nil) (light g "#ffd9a0" 1.4 22.0 -6.0 3.0 mid))
         (dolist (level '(-180.0 -120.0 -60.0 60.0 120.0 180.0))                                                      ; the galleries of the other floors
           (shape g :box (list 12.0 .8 c) "#5f5e5a" -3.0 level mid :shadow nil))
         (shape g :box (list 4.0 700.0 c) "#6a7a5a" 204.0 0.0 mid :shadow nil)                                        ; the wall across the yard
         (dolist (level '(-180.0 -120.0 -60.0 0.0 60.0 120.0 180.0))
           (shape g :box (list 12.0 .8 c) "#5f5e5a" 196.0 level mid :shadow nil))
         (dotimes (k 7)                                                                                               ; windows lit across the yard, on every floor
           (shape g :box '(.4 1.6 2.2) "#ffcf7a" 201.6 (+ 1.6 (* 60.0 (- (floor (* 7 (r (+ 620 k)))) 3))) (* (- c) (r (+ 630 k))) :emit "#ffb866" :energy 3 :unshaded t :shadow nil))
         (when (zerop (mod at 2))                                                                                     ; washing on lines right across the well
           (let ((y (+ 6.0 (* 60.0 (- (floor (* 5 (r 640))) 2)))))
             (shape g :box '(196.0 .1 .1) "#d8d0b8" 99.0 y -30.0 :shadow nil)
             (dotimes (k 6) (shape g :box '(5.0 4.4 .1) (nth (mod k 4) '("#e8e0d0" "#c9a13a" "#e8e0d0" "#8a4038")) (+ 20.0 (* 28.0 k)) (- y 2.4) -30.0 :shadow nil))))
         (shape g :box (list 196.0 2.0 c) "#3a3a3c" 100.0 -330.0 mid :shadow nil)                                     ; the yard itself, far below
         (when (zerop (mod at 4)) (shape g :box '(2.0 2.0 2.0) "#ffe2a8" 100.0 -322.0 mid :emit "#ffcf8a" :energy 3 :unshaded t :shadow nil))
         (when (= (mod at 8) 5)
           (sign g "УХОДЯ, ГАСИТЕ СВЕТ" -8.8 3.4 mid :h .5 :rgb "#f0ece0" :yaw (/ pi 2)))))

(defzone zone-garages                                                        ; the garage co-operative from a flyover: built by many hands over many years
         (shape g :box (list 10.0 1.0 c) "#55524f" -1.0 -.55 mid :shadow nil)                       ; a flyover across the co-operative
         (dolist (x '(-6.2 4.2)) (shape g :box (list .3 .9 c) "#4a4c50" x .4 mid :shadow nil))
         (shape g :box '(3.0 70.0 3.0) "#5a5a58" -1.0 -36.0 0 :shadow nil)
         (let ((low (group g 0.0 -72.0 0.0)))
           (dolist (side '(-1 1))
             (dotimes (row 6)
               (let* ((k (+ 700 (* 20 row) side)) (step (* 9.0 (- (floor (* 5 (vr (+ (floor i 4) (* 3 row) side) 71.0))) 2)))    ; each row on its own terrace
                      (x (+ (* side (+ 34.0 (* 78.0 row))) (* 24.0 (- (r (+ k 1)) .5)))) (h (+ 16.0 (* 30.0 (r (+ k 2)) (r (+ k 3)))))
                      (len (* c (+ .5 (* .45 (r (+ k 4)))))) (z (- mid (* 8.0 (- (r (+ k 5)) .5))))
                      (rgb (nth (floor (* 5 (r (+ k 6)))) '("#5a2a22" "#3c5a48" "#55708a" "#8a8478" "#6a3a2e"))))
                 (shape low :box (list 78.0 60.0 c) "#4a4844" (* side (+ 34.0 (* 78.0 row))) (- step 30.0) mid :shadow nil)       ; the terrace
                 (unless (< (r (+ k 7)) .16)                                                                                       ; a plot left empty
                   (shape low :box (list (+ 40.0 (* 16.0 (r (+ k 8)))) h len) "#6f6e69" x (+ step (/ h 2)) z :rot (list 0 (* .1 (- (r (+ k 9)) .5)) 0) :shadow nil)
                   (shape low :box (list .5 (* h .8) (* len .7)) rgb (- x (* side 24.0)) (+ step (* h .4)) z :shadow nil)
                   (when (< (r (+ k 10)) .25)                                                                                      ; a second one built on its roof
                     (shape low :box (list 26.0 (+ 10.0 (* 14.0 (r (+ k 11)))) (* len .5)) "#77756f" (+ x (* 6.0 (- (r (+ k 12)) .5))) (+ step h 8.0) z :shadow nil))))))
           (when (zerop (mod i 3))                                                                                                 ; lamps where somebody put one
             (let ((x (- (* 480.0 (r 760)) 240.0)))
               (shape low :box '(.3 46.0 .3) "#3a3c40" x 23.0 -6.0 :shadow nil)
               (shape low :box '(3.0 .6 1.2) "#ffe2a8" x 46.0 -6.0 :emit "#ffcf8a" :energy 3 :shadow nil)
               (light low "#ffcf8a" 2.2 90.0 x 40.0 -6.0)))
           (when (= (mod at 4) 1)                                                                                                  ; somebody is in: doors ajar, light on the lane
             (shape low :box '(.6 14.0 8.0) "#ffcf7a" (- (* 200.0 (r 770)) 100.0) 8.0 -12.0 :emit "#ffb866" :energy 2.5 :shadow nil)))
         (when (= (mod at 8) 6) (sign g "ГСК «ЗАРЯ» · СТОРОЖ В БОКСЕ № 1" 4.4 2.4 mid :h .6 :rgb "#f0ece0")))

(defzone zone-spiral                                                         ; the ramp: a wall on the left that has no top, nothing on the right
         (mega-slab g 17.0 "#5f5e5a" -1.0)
         (shape g :box (list 2.0 600.0 c) "#6b6a66" -10.5 220.0 mid :shadow nil)
         (shape g :box (list .5 1.0 c) "#4a4946" 7.2 .5 mid :shadow nil)
         (dotimes (k 3)                                                   ; far across the shaft, lamps on other ramps
           (shape g :box '(.9 .9 .9) "#ffcf8a" (+ 120.0 (* 360.0 (r (+ 300 k)))) (- (* 500.0 (r (+ 310 k))) 320.0) (* (- c) (r (+ 320 k)))
                  :emit "#ffb060" :energy 2.5 :unshaded t :shadow nil))
         (when (zerop (mod at 4))
           (shape g :box '(.3 .5 1.4) "#ffe2a8" -9.3 5.0 mid :emit "#ffcf8a" :energy 3 :shadow nil)
           (light g "#ffcf8a" 1.5 34.0 -7.0 5.0 mid)))

(defzone zone-rail                                                           ; the marshalling yard: a crane sets the containers down ahead of us, and takes them up behind
         (live :drop (shape g :box (list 12.0 12.0 (- c .8)) (nth (mod i 5) '("#5a2a22" "#3c5a48" "#55708a" "#a8843c" "#6a3a5a")) 0.0 -6.05 mid :shadow nil)
               0.0 -6.05 mid)
         (let ((low (group g 0.0 -84.0 0.0)))
           (shape low :box (list 900.0 2.0 c) "#33312e" -1.0 -1.0 mid :shadow nil)                                    ; the yard, far below
           (dolist (x '(-150.0 -126.0 130.0 154.0)) (shape low :box (list 1.6 2.2 c) "#8a8e92" x 1.1 mid :shadow nil))  ; rails
           (dotimes (k 3)                                                                                             ; stacks of containers, as they were left
             (let ((x (* (if (< (r (+ 900 k)) .5) -1 1) (+ 22.0 (* 230.0 (r (+ 910 k)) (r (+ 915 k)))))) (n (1+ (floor (* 6 (r (+ 920 k)) (r (+ 925 k)))))))
               (dotimes (j n)
                 (shape low :box '(12.0 12.0 40.0) (nth (floor (* 5 (r (+ 930 (* 10 k) j)))) '("#5a2a22" "#3c5a48" "#55708a" "#a8843c" "#6a3a5a"))
                        (+ x (* 1.4 (- (r (+ 940 (* 10 k) j)) .5))) (+ 6.0 (* 12.0 j)) (- mid (* 4.0 (- (r (+ 950 k)) .5))) :shadow nil))))
           (when (zerop (mod i 3))
             (let ((x (- (* 500.0 (r 960)) 250.0)))
               (shape low :box '(.6 110.0 .6) "#3a3c40" x 55.0 -6.0 :shadow nil)
               (shape low :box '(8.0 1.0 3.0) "#f4f6ff" x 110.0 -6.0 :emit "#e8f0ff" :energy 3 :shadow nil)
               (light low "#dfe8ff" 2.2 150.0 x 100.0 -6.0)))))
