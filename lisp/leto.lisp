;;;; The summer behind the tunnel: the other place one can come out into (*MEGA-WORLD* is :SUMMER). It is laid
;;;; out by the same machinery as the megastructure (mega.lisp) - cells ahead of the wagon, taken away behind -
;;;; but under the open sky, and its zones are not places but the hours of one day that does not end: morning by
;;;; the river, noon in the sunflowers, a thunderstorm, the golden evening by the gates of a pioneer camp, a
;;;; fire at night, and morning again. Ahead, always, stands a town one never reaches. The clock of the game
;;;; stands still here: the sky is shown at the hour of the place, and the calendar does not turn.

(in-package #:kiosk-trip)

(defparameter *summer-town* 1700.0 "How far ahead the town stands, in metres: always.")
(defparameter *summer-grass* "#3c7a30")

(defparameter *summer-hours* '(:morning 62.0 :noon 350.0 :storm 470.0 :evening 722.0 :night 960.0)
  "The hours of the summer day, in the order they come, each with the second of the day cycle its zone is shown at.")

(defun summer-zone (s)
  "Return the number of the summer zone at S metres along the way, and how far through it we are, from 0 to 1."
  (multiple-value-bind (z start cells) (way-zone (floor s *mega-cell*))
    (values z (/ (- (max s 0.0) (* start *mega-cell*)) (* cells *mega-cell*)))))

(defun summer-hour (s)
  "Return the second of the day cycle the sky shows at S metres along the way: each zone holds its hour, and over
its last third the day moves on to the hour of the next."
  (multiple-value-bind (z f) (summer-zone s)
    (let* ((n (/ (length *summer-hours*) 2)) (k (mod z n))
           (here (nth (1+ (* 2 k)) *summer-hours*)) (next (nth (1+ (* 2 (mod (1+ k) n))) *summer-hours*)))
      (when (< next here) (incf next *day-len*))
      (if (< f .68) (+ here (* 12.0 f)) (+ here 8.2 (* (- next here 8.2) (sstep .68 1.0 f)))))))

(defvar *summer-air* (list :gloom 0.0 :hour 0.0 :weather (list :cloud 0.0 :rain 0.0 :snow 0.0 :fog 0.0) :juice 1.0)
  "The air of the summer as SUMMER-AIR last described it: one plist, set anew every frame.")

(defun summer-weather (s)
  "Return the weather at S metres along the way, as a plist of what STEP-CLIMATE is to aim at: mist in the morning,
a storm in its hour, clear otherwise. It is the weather plist of *SUMMER-AIR*, set anew."
  (multiple-value-bind (z f) (summer-zone s)
    (let* ((kind (nth (* 2 (mod z (/ (length *summer-hours*) 2))) *summer-hours*)) (wx (getf *summer-air* :weather))
           (storm (if (eq kind :storm) (* (sstep 0.0 .12 f) (- 1.0 (sstep .8 .98 f))) 0.0)))
      (setf (getf wx :cloud) storm (getf wx :rain) (* .9 storm)
            (getf wx :fog) (if (eq kind :morning) (* .22 (- 1.0 (sstep .2 .8 f))) (* .1 storm)))
      wx)))

;;; What every cell of the summer has: the concrete road, its sandy shoulders, a pole of the power line.

(defun leto-road (g i c mid)
  (mega-slab g 9.0 "#9a9484")
  (dolist (x '(-6.9 3.3)) (shape g :box (list 1.2 .9 c) "#b0a070" x -.52 mid :shadow nil))
  (dolist (z (list 0.0 mid)) (shape g :box '(9.0 .02 .14) "#8d8a7c" *way-x* -.03 z :shadow nil))               ; the seams of the slabs
  (shape g :box '(.28 8.5 .28) "#6a4a32" -8.6 4.2 -20.0 :shadow nil)
  (shape g :box '(2.4 .16 .16) "#6a4a32" -8.6 7.8 -20.0 :shadow nil)
  (dolist (x '(-9.6 -7.6)) (shape g :box (list .05 .05 c) "#20242c" x 7.7 mid :shadow nil))
  (when (zerop (mod i 5)) (shape g :box '(.5 .9 .12) "#f0ece0" 3.6 .45 -9.0 :shadow nil)))                 ; a kilometre post, now and then

(defun leto-pine (g x z s &optional (needle "#2f8f4a"))
  (shape g :cyl '(.28 .4 9.0 6) "#8a5a3a" x (* 4.5 s) z :scale (list s s s) :shadow nil)
  (shape g :cone '(4.4 9.0 7) needle x (* 9.5 s) z :scale (list s s s))
  (shape g :cone '(3.0 7.5 7) needle x (* 14.0 s) z :scale (list s s s) :shadow nil))

(defun leto-birch (g x z s leaf)
  (shape g :cyl '(.16 .22 7.0 5) "#f1efe6" x (* 3.5 s) z :scale (list s s s) :shadow nil)
  (shape g :ball '(2.7 7 5) leaf x (* 8.0 s) z :scale (list s (* 1.25 s) s)))

(defun leto-wood (g i c needle &key (near 12.0) (count 5) birches)
  "Scatter COUNT pines over the cell on either side of the road, none nearer to it than NEAR; with BIRCHES a birch
or two of that leaf colour by the roadside."
  (flet ((r (k) (vr (+ i 5000.5) (float k))))
    (dotimes (k count)
      (let ((side (if (< (r (+ 10 k)) .5) -1 1)))
        (leto-pine g (+ *way-x* (* side (+ near (* 170.0 (r (+ 20 k)) (r (+ 30 k)))))) (* (- c) (r (+ 40 k))) (+ .8 (* .8 (r (+ 50 k)))) needle)))
    (when (and birches (< (r 1) .6))
      (leto-birch g (+ *way-x* (* (if (< (r 2) .5) -1 1) (+ 9.0 (* 6.0 (r 3))))) (* (- c) (r 4)) (+ .8 (* .5 (r 5))) birches))))

(defun leto-sunflowers (g i c mid at &optional (head "#ffd21e"))
  "Put a field of sunflowers on either side of the road: the plants one can count by the roadside, and behind them
the field as one sheet of the same yellow."
  (flet ((r (k) (vr (+ i 6000.5) (float k))))
    (dolist (side '(-1 1))
      (let ((edge (+ *way-x* (* side (if (and (= at 8) (> side 0)) 22.0 8.4)))))               ; room for the bus stop
        (shape g :box (list 600.0 1.5 c) "#d2a416" (+ edge (* side 303.0)) .7 mid :shadow nil)             ; the field
        (shape g :box (list 3.0 1.3 c) "#3f7d28" (+ edge (* side 1.5)) .6 mid :shadow nil)                  ; its green edge
        (dotimes (k 14)
          (let ((x (+ edge (* side (+ .3 (* 2.8 (r (+ 10 k (* 40 side)))))))) (z (* (- c) (/ (+ k (r (+ 30 k))) 14.0)))
                   (h (+ 1.5 (* .5 (r (+ 50 k))))))
            (shape g :box (list .08 h .08) "#3f8f2e" x (/ h 2) z :shadow nil)
            (shape g :cyl '(.5 .5 .06 9) head x h z :rot (list (/ pi 2) (* side -.6) 0) :emit head :energy .35 :shadow nil)
            (shape g :cyl '(.19 .19 .08 8) "#6a3a12" x h z :rot (list (/ pi 2) (* side -.6) 0) :shadow nil)))))))

;;; The pioneer camp by the road, our own: gates with its name, a fence, a flag, plaster pioneers, an alley of
;;; lamps and the houses of the detachments under the pines. LIT says the lamps and the windows are on.

(defun leto-camp (g i c lit)
  (let* ((x 9.0) (z -21.0) (white "#f4f1e6") (red "#d8362b"))
    (model g "leto-camp")
    (sign g "ПИОНЕРСКИЙ ЛАГЕРЬ «ЛАСТОЧКА»" (- x .2) 7.3 z :h .9 :rgb "#fff6dc" :yaw (/ pi -2))
                                      ; the flag
    (dotimes (k 5)
      (let ((lx (+ x 8.0 (* 10.0 k))) (lz (+ z (if (evenp k) -3.2 3.2))))
        (shape g :cyl '(.07 .07 3.6 5) "#3a4048" lx 1.8 lz :shadow nil)
        (shape g :ball '(.45 8 6) (if lit "#fff2c0" white) lx 3.8 lz :emit (and lit "#ffe2a8") :energy 3 :shadow nil)
        (when (and lit (evenp k)) (light g "#ffd890" 1.6 22.0 lx 3.6 lz))))
    (loop for (hx hz rgb) in '((28.0 -15.0 "#f2d06a") (44.0 15.0 "#8fd0a8") (60.0 -15.0 "#f0a06a") (74.0 16.0 "#9fc0f0"))
          do (let ((bx (+ x hx)) (bz (+ z hz)))
               (shape g :box '(9.0 3.2 12.0) rgb bx 1.9 bz)
               (shape g :box '(7.4 7.4 12.6) "#b04a32" bx 3.6 bz :rot (list 0 0 (/ pi 4)) :scale '(1 .55 1))
               (dolist (dz '(-2.5 2.5))
                 (shape g :box '(.1 1.3 1.6) (if lit "#ffcf70" "#dfeaf2") (- bx 4.56) 2.2 (+ bz dz) :emit (and lit "#ffb866") :energy 2.5
                        :shadow nil))))
    (when lit                                                            ; a garland over the dance floor
      (dotimes (k 14)
        (let ((rgb (nth (mod k 4) '("#ffd21e" "#ff5a3a" "#5ad0ff" "#7dff7a"))))
          (shape g :ball '(.28 6 4) rgb (+ x 22.0 (* 1.6 k)) (- 5.2 (* 1.2 (sin (* k .24)))) (+ z 30.0) :emit rgb :energy 3 :shadow nil)))
      (light g "#ffb860" 1.8 40.0 (+ x 32.0) 4.5 (+ z 30.0)))))

(defun leto-fire (g)
  "Put a fire by the road, and those who sit round it."
  (let ((f (group g -14.0 0.0 -21.0)))
    (model f "leto-fire")
    (live :press (shape f :cone '(.3 1.2 7) "#fff0a0" 0 1.4 0 :emit "#fff0a0" :energy 3 :unshaded t :shadow nil) 0.0 0.0 1.3 1.75 .7 0.0)
    (light f "#ff8a2e" 3.2 46.0 0 1.8 0)
    (dotimes (p 6)
      (let* ((a (+ .4 (* p 1.05))) (s (group f (* 3.6 (cos a)) 0.0 (* 3.6 (sin a)))))
        (node-3d-set-rotation s (v3 0 (- (/ pi 2) a) 0))
        (model s "leto-sitter")))))

;;; The hours.

;; morning by the river: mist over the meadow, the pines coming out of it one by one
(defzone zone-morning
         (leto-road g i c mid)
         (if (= (mod (zone-cell i) 32) 20)
             (progn                                                       ; the river, and the bridge over it
               (shape g :box (list 1400.0 .3 (- c 4.0)) "#8fc6ec" *way-x* -.3 mid :rough .1 :metal .3 :shadow nil)
               (shape g :box (list 420.0 .32 8.0) "#ffe9d2" -230.0 -.28 (- mid 6.0) :emit "#ffe9d2" :energy .5 :shadow nil)
               (dolist (x '(-6.5 2.9)) (shape g :box (list .25 1.1 c) "#f4f1e6" x .55 mid :shadow nil)))
             (leto-wood g i c "#2f7d48" :count 5 :birches "#74b848")))

(defzone zone-noon                                                           ; noon: sunflowers to the horizon on either side
         (leto-road g i c mid)
         (leto-sunflowers g i c mid at)
         (when (= (zone-cell i) 30)                                        ; a water tower over the field
           (shape g :cyl '(5.0 5.0 34.0 10) "#e8e4d8" 260.0 17.0 mid :shadow nil)
           (shape g :cyl '(7.5 7.5 8.0 10) "#c8472e" 260.0 38.0 mid :shadow nil)))

(defzone zone-storm                                                          ; the storm: the same field under a cloud, burning yellow against it
         (leto-road g i c mid)
         (leto-sunflowers g i c mid at "#ffd83a")
         (when (= (mod (zone-cell i) 16) 6) (leto-pine g -40.0 -20.0 1.8 "#2a7a44")))

(defzone zone-evening                                                        ; the golden hour: the road goes by the gates of the camp
         (leto-road g i c mid)
         (let ((k (- (zone-cell i) 26)))                                  ; the camp stands once in the hour, and the wood stands back from it
           (if (zerop k)
               (leto-camp g i c nil)
               (leto-wood g i c "#2a7540" :near (if (<= -1 k 2) 110.0 14.0) :count 6 :birches "#8fb832"))))

(defzone zone-night                                                          ; night: a fire by the road, the lamps of the camp, and morning after it
         (leto-road g i c mid)
         (let ((k (- (zone-cell i) 14)))                                  ; the lamps of the camp early in the night, the fire later
           (cond ((zerop k) (leto-camp g i c t))
                 (t (leto-wood g i c "#1f5a3a" :near (if (<= -1 k 2) 110.0 22.0) :count 6)
                    (when (= (zone-cell i) 34) (leto-fire g))))))

;;; The town one drives towards and never reaches. It hangs in the wagon's own axes, straight ahead, so it stays
;;; as far as it was however long we drive: pale blocks and a television tower in the haze by day, their windows
;;; and the red lamps of the tower by night.

(defun build-summer-far (g)
  "Put under G, in the wagon's axes, what does not go by: the land to the horizon and the town ahead."
  (shape g :box '(5200.0 .3 5200.0) *summer-grass* *way-x* -.45 0.0 :shadow nil)
  (let ((town (group g *way-x* 0.0 (- *summer-town*))))
    (dotimes (k 22)
      (let* ((x (- (* 60.0 k) 630.0 (* 40.0 (vr k 3.0)))) (h (+ 50.0 (* 150.0 (vr k 5.0) (vr k 6.0)))) (w (+ 34.0 (* 40.0 (vr k 7.0))))
                (z (* 260.0 (- (vr k 8.0) .5))))
        (shape town :box (list w h 30.0) (if (evenp k) "#e9edf2" "#d6dde6") x (/ h 2) z :emit "#c9d6e6" :energy .35 :shadow nil)
        (dotimes (j 3)                                                   ; a few windows that show at night
          (shape town :box '(3.0 3.0 .6) "#ffcf70" (+ x (* w (- (vr (+ k (* 9 j)) 11.0) .5) .8)) (* h (+ .2 (* .6 (vr (+ k (* 9 j)) 12.0))))
                 (+ z 15.4)
                 :emit "#ffb866" :energy 2.0 :unshaded t :shadow nil))))
    (shape town :cyl '(3.0 9.0 330.0 8) "#dfe4ea" 120.0 165.0 -80.0 :emit "#c9d6e6" :energy .35 :shadow nil)      ; the television tower
    (shape town :cyl '(13.0 13.0 14.0 10) "#dfe4ea" 120.0 215.0 -80.0 :emit "#c9d6e6" :energy .35 :shadow nil)
    (dolist (y '(120.0 215.0 328.0)) (shape town :ball '(3.2 6 4) "#ff3020" 120.0 y -80.0 :emit "#ff2010" :energy 3 :unshaded t :shadow nil))
    town))

;;; The summer as a world (see *WORLDS* in mega.lisp).

(add-zones
 '(;; flat land, a road that bends a little
    :morning  (:name "Утро"                 :build zone-morning)
    :noon     (:name "Полдень"              :build zone-noon)
    :storm    (:name "Гроза"                :build zone-storm)
    :evening  (:name "Вечер"                :build zone-evening)
    :night    (:name "Ночь"                 :build zone-night)))

(defun summer-zone-kind (z) "Return the kind of the summer's zone number Z: the hours, round and round."
       (nth (* 2 (mod z (/ (length *summer-hours*) 2))) *summer-hours*))

(defun build-summer-parts (g turn entry)
  "Build what in the summer does not go by: the hill the tunnel comes out of, the land to the horizon and the town ahead."
  (declare (ignore turn))
  (shape entry :box '(320.0 34.0 330.0) *summer-grass* *way-x* 24.8 -35.0 :shadow nil)
  (dolist (x '(-88.4 84.8)) (shape entry :box '(160.0 8.0 330.0) *summer-grass* x 3.9 -35.0 :shadow nil))
  (list :town (build-summer-far g)))

(defun summer-air (s)
  "Return the air of the summer S metres in: it is under the sky, so the dark ends with the tunnel; the sky shows the
hour of the place and the weather is its own; the picture is in full colour, not a faded film."
  (summer-weather s)
  (setf (getf *summer-air* :gloom) (- 1.0 (sstep 150.0 235.0 s)) (getf *summer-air* :hour) (summer-hour s))
  *summer-air*)

(add-world :summer :flag "summer" :kind 'summer-zone-kind :parts 'build-summer-parts :air 'summer-air
           :rules '()                                                    ; nothing of the ring goes on here, and the clock stands
           :offmap "КАРТА ОБЛАСТИ~%~%Этой дороги на ней нет.~%~%Проехано: ~,1f км~%До города — всё столько же.")
