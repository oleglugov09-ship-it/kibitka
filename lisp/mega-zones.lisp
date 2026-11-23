;;;; The megastructure: its everyday zones - late-Soviet things grown past all measure, and the way through each
;;;; is a part of the thing itself.

(in-package #:kiosk-trip)

;;; The megastructure as a world (see *WORLDS* in mega.lisp): its zones, the order they come in, the way down from
;;; the tunnel, and what does not go by - the wall the tunnel comes out of, the breach the day comes in through,
;;; the platform, the pen and the crane.

(defparameter *mega-lift* '(6 .45) "The cell at which the platform starts down, and how steeply it goes: metres down per metre forward.")

(defvar *mega-order* '(:metro :quarry :dam :reactor :factory :pipes :communal :archive :paper :rail :statues :garages :spiral :poles :columns)
  "The kinds of zones, after the first one. Late-Soviet things grown past all measure: a workshop, a communal
corridor, a garage co-operative, a heating main, a left-luggage hall, an avenue of busts.")

;;; The paints of the megastructure. It is one cold grey place with a few colours that mean something: the yellow
;;; of cranes and safety lines, the red of banners, the pale blue of the day where it gets in. What is painted the
;;; same is named once here; the one-off colours of single things stay where the things are built.

(defparameter *mega-paints*
  '(:wall "#39474d" :concrete "#5f5e5a" :slab "#6f6e69" :stone "#8a887f" :floor "#3a3a3c" :marble "#d8d2c4"   ; what the place is built of
    :iron "#3a3c40" :soot "#2a2c30" :steel "#8a8f93" :gangway "#4a4c50" :wood "#5a3e26"                       ; what is bolted to it
    :yellow "#c9a13a" :ochre "#b8902e" :teal "#3f6f73" :blue "#55708a" :green "#3c5a48"                       ; its paints: cranes and brass, diggers, machines
    :flag "#a3262c" :letters "#f0ece0"                                                                        ; what speaks: banners and what is written on them
    :day "#b9d6de" :daylight "#cfe6ee")                                                                       ; the day, where it gets in
  "The paints of the megastructure: a plist from what is painted to its colour.")

(defun paint (what) "Return the colour the megastructure paints WHAT with."
       (or (getf *mega-paints* what) (error "The megastructure has no paint for ~s" what)))

;;; Of the megastructure's zones besides what every zone has: :HAZE is how much pale light stands there, :SUN how
;;; strong the day from the breach is against it (.5 unless said), :FOG how thick the air is (the usual murk unless
;;; said: a great hall is clear from end to end). :HALL says the zone stands in the common hall - a roof, far
;;; walls, slits of day: T, or :DEEP with a floor far below, or :LAMPS with lamps on that floor too.

(add-zones
  '(:lift     (:name "Спуск"                :build zone-lift :bare t)
    :metro    (:name "Метро"                :build zone-metro)
    :quarry   (:name "Карьер"               :build zone-quarry :way -1 :down t)
    :dam      (:name "Плотина"              :build zone-dam :way 1 :hall t)
    :reactor  (:name "Энергоблок"           :build zone-reactor)
    :factory  (:name "Конвейер"             :build zone-factory)
    :pipes    (:name "Теплотрасса"          :build zone-pipes :hall :lamps)
    :communal (:name "Коммуналка"           :build zone-communal)
    :archive  (:name "Архив"                :build zone-archive)
    :paper    (:name "Письмо"               :build zone-paper)
    :rail     (:name "Сортировочная"        :build zone-rail :hall :deep)
    :statues  (:name "Аллея трудовой славы" :build zone-statues :hall t)
    :garages  (:name "Гаражный кооператив"  :build zone-garages :hall :deep)
    :spiral   (:name "Пандус"               :build zone-spiral :way 1 :down t)
    :poles    (:name "Микрорайон на сваях"  :build zone-poles :hall :lamps)
    :columns  (:name "Вестибюль"            :build zone-columns :hall t)))

(defun mega-zone-kind (z)
  "Return the kind of the megastructure's zone number Z: the way down first, then the others, the first time round in
the order they are listed and after that shuffled, never the same twice in a row."
  (if (<= z 0)
      :lift
      (let* ((n (length *mega-order*)) (block (floor (1- z) n)) (pos (mod (1- z) n)))
        (if (zerop block)
            (nth pos *mega-order*)
            (let ((step (loop for s from (1+ (floor (* (1- n) (vr block 4.0)))) when (= 1 (gcd s n)) return s)))
              (nth (mod (+ (floor (* n (vr block 9.0))) (* pos step)) n) *mega-order*))))))

(defun mega-lift-drop (j)
  "Return how far cell J goes down without tilting: on the way down from the tunnel the platform we stand on stays level."
  (and (<= (first *mega-lift*) j (1- (zone :lift :cells))) (* (second *mega-lift*) *mega-cell*)))

(defun build-mega-parts (g turn entry)
  "Build what in the megastructure does not go by, and return it as a plist."
  ;; the wall the tunnel comes out of: it goes up and down and to the right out of sight
  (shape entry :box '(1700.0 600.0 6.0) (paint :wall) 800.0 312.0 -197.0 :shadow nil)
  (shape entry :box '(1700.0 1000.0 6.0) (paint :wall) 800.0 -501.0 -197.0 :shadow nil)
  (shape entry :box '(1600.0 14.0 6.0) (paint :wall) 814.0 6.0 -197.0 :shadow nil)
  ;; The light of the place has a source: far off and high up there is a breach, and the day comes in through it
  ;; from one side. It hangs in the megastructure's own frame, so as the way turns it swings round us like a sun.
  (let ((sun (gdext:make-object "DirectionalLight3D")) (pen (group g 0.0 0.0 (- *mega-pen*))) (crane (group g 0.0 0.0 -150.0)) (lift (group g)))
    (object-set sun "light_color" (hex (paint :day))) (object-set sun "light_energy" 0.0) (object-set sun "shadow_enabled" nil)
    (node-add-child turn sun)
    (node-3d-set-rotation sun (v3 -.36 .85 0))                 ; low: it rakes the sides of things and hardly touches the floors
    (shape turn :ball '(70.0 12 8) (paint :daylight) -430.0 240.0 -380.0 :emit (paint :day) :energy 2.0 :unshaded t :shadow nil)   ; the breach itself
    ;; the platform that carries us down from the tunnel: we stand on it and look about
    (shape lift :box '(22.0 1.2 34.0) (paint :gangway) *way-x* -.72 0.0 :shadow nil)
    (dolist (x '(-12.6 9.0)) (shape lift :box '(.5 .14 34.0) (paint :yellow) x -.1 0.0 :shadow nil))
    (dolist (z '(-16.8 16.8)) (shape lift :box '(22.0 .14 .5) (paint :yellow) *way-x* -.1 z :shadow nil))
    (shape lift :box '(.2 1.1 34.0) (paint :iron) 9.1 .5 0.0 :shadow nil)
    (dolist (x '(-12.4 8.8))
      (dolist (z '(-16.6 16.6))
        (shape lift :box '(.3 2.4 .3) (paint :iron) x 1.1 z :shadow nil)
        (shape lift :box '(.4 .4 .4) "#ff3020" x 2.5 z :emit "#ff2010" :energy 2.5 :shadow nil)))
    (shape lift :box '(8.0 3.0 20.0) (paint :iron) -15.0 -2.0 0.0 :shadow nil)
    ;; the pen that writes our road: its nib on the paper far ahead, the barrel leaning back over us
    (let ((lean (group pen)))
      (node-3d-set-rotation lean (v3 .55 .3 0))
      (shape lean :cyl '(13.0 .5 60.0 12) (paint :yellow) 0 30.0 0 :metal .5 :shadow nil)
      (shape lean :cyl '(15.0 15.0 620.0 12) "#1a2a6a" 0 370.0 0 :shadow nil)
      (shape lean :cyl '(16.0 16.0 30.0 12) (paint :yellow) 0 75.0 0 :metal .5 :shadow nil))
    ;; the crane that sets the road down: a gantry astride it, a container on its ropes
    (dolist (x '(-46.0 46.0)) (shape crane :box '(7.0 260.0 7.0) (paint :yellow) x 40.0 0 :shadow nil))
    (shape crane :box '(110.0 9.0 9.0) (paint :yellow) 0 172.0 0 :shadow nil)
    (shape crane :box '(16.0 10.0 16.0) "#8a6a1e" 0 162.0 0 :shadow nil)
    (dolist (dz '(-14.0 14.0)) (shape crane :box '(.5 130.0 .5) (paint :soot) 0 94.0 dz :shadow nil))
    (dolist (part (list pen crane lift)) (object-set part "visible" nil))
    (list :sun sun :pen pen :crane crane :lift lift)))

(defun step-mega-parts (parts)
  "Light the megastructure by its zone, and show the platform, the pen and the crane where they belong."
  (let* ((c *mega-cell*) (here (mega-kind (floor *mega-s* c))) (there (mega-kind (floor (+ *mega-s* *mega-pen*) c))))
    (object-set (getf parts :sun) "light_energy" (* (zone here :sun .5) *haze*))
    (object-set (getf parts :pen) "visible" (and (eq here :paper) (eq there :paper)))
    (object-set (getf parts :crane) "visible" (and (eq here :rail) (eq there :rail)))
    (object-set (getf parts :lift) "visible" (and (eq here :lift) (> *mega-s* 196.0)))))

(defun mega-air (s)
  "Return the air of the megastructure S metres in: no day and no sky, the haze and the murk of the zone."
  (let ((kind (mega-kind (floor s *mega-cell*))))
    (list :gloom 1.0 :haze (zone kind :haze 0.0) :fog (zone kind :fog .0047))))

(add-world :mega :flag "tunnel" :kind 'mega-zone-kind :drop 'mega-lift-drop :parts 'build-mega-parts :step 'step-mega-parts :air 'mega-air
           :rules '(:clock t)                                            ; nothing of the ring goes on here, but time passes
           :offmap "КАРТА ОБЛАСТИ~%~%Этого места на ней нет.~%~%По эстакаде пройдено: ~,1f км~%Сколько осталось — не написано.")

;;; What the megastructure's zones are built of.

(defun mega-columns (g i lamps)
  "Put the columns of cell I: two or three of them wherever they happen to stand, thick and thin, some broken off
short; with LAMPS, a lamp on one."
  (let ((c *mega-cell*))
    (flet ((r (k) (vr (+ i 2000.5) (float k))))
      (dotimes (k (+ 2 (floor (* 2 (r 1)))))
        (let* ((side (if (< (r (+ 10 k)) .5) -1 1)) (x (* side (+ 18.0 (* 240.0 (r (+ 20 k)) (r (+ 25 k))))))
               (w (+ 8.0 (* 26.0 (r (+ 30 k)) (r (+ 35 k))))) (z (- (* c (r (+ 40 k))))) (broken (< (r (+ 50 k)) .22))
               (top (if broken (+ 20.0 (* 110.0 (r (+ 60 k)))) 430.0)))
          (shape g :box (list w (+ top 260.0) w) (if (< (r (+ 70 k)) .5) "#7c7b76" "#6b6a66") x (/ (- top 260.0) 2) z
                 :rot (list 0 (* 1.5 (r (+ 80 k))) (if broken (* .12 (- (r (+ 90 k)) .5)) 0)) :shadow nil)
          (when (and lamps (zerop k))
            (lamp g :amber :box '(.5 1.2 1.6) (- x (* side (+ .3 (/ w 2)))) 7.0 z :reach 70.0 :power 1.6
                  :at (list (- x (* side (+ 8.0 (/ w 2)))) 7.0 z))))))))

(defun mega-ground (g i rgb)
  "Lay the ground of cell I where a zone is driven over at floor level: not one plane but a strip under the wheels
and patches on either side, each at its own height, so that the place has ledges, pits and steps."
  (let* ((c *mega-cell*) (mid (- (/ c 2))))
    (flet ((r (k) (vr (+ i 3000.5) (float k))))
      (shape g :box (list 18.0 40.0 c) rgb *way-x* -20.05 mid :shadow nil)                          ; what we drive on
      (dolist (side '(-1 1))
        (let* ((b (floor i 3)) (near (+ 30.0 (* 90.0 (vr (+ b (* 7 side)) 31.0))))                 ; the patches hold for three cells
               (dy1 (nth (floor (* 6 (vr (+ b (* 7 side)) 32.0))) '(-14.0 -6.0 -2.0 0.0 3.0 9.0)))
               (dy2 (nth (floor (* 6 (vr (+ b (* 7 side)) 33.0))) '(-40.0 -16.0 -5.0 4.0 12.0 30.0))))
          (shape g :box (list near 60.0 c) rgb (* side (+ 9.0 (/ near 2))) (- dy1 30.0) mid :shadow nil)
          (unless (< (vr (+ b (* 7 side)) 34.0) .2)                                                  ; and now and then a pit to the level below
            (shape g :box (list (- 300.0 near) 80.0 c) rgb (* side (+ 9.0 near (/ (- 300.0 near) 2))) (- dy2 40.0) mid :shadow nil))
          (when (< (r (+ 5 side)) .25)                                                               ; a block left lying
            (shape g :box (list (+ 3.0 (* 9.0 (r 6))) (+ 2.0 (* 6.0 (r 7))) (+ 3.0 (* 8.0 (r 8)))) rgb
                   (* side (+ 14.0 (* 40.0 (r 9)))) (+ dy1 2.0) (* (- c) (r 10)) :rot (list 0 (* 3.0 (r 11)) (* .2 (r 12))) :shadow nil)))))))

(defun mega-hall (g i hall)
  "Put the common hall around cell I: a roof, far walls and piers, the day coming in through slits only. With HALL
:DEEP or :LAMPS there is a floor far below, and with :LAMPS a lamp or two on it."
  (let* ((c *mega-cell*) (mid (- (/ c 2))))
    (flet ((r (k) (vr (+ i 1000.5) (float k))))
      (shape g :box (list 1200.0 8.0 c) "#161c20" *way-x* 470.0 mid :shadow nil)
      (dolist (x '(-600.0 596.0)) (shape g :box (list 10.0 900.0 c) "#1c2428" x 30.0 mid :shadow nil))
      (unless (eq hall t) (shape g :box (list 1200.0 6.0 c) "#12171a" *way-x* -420.0 mid :shadow nil))
      (when (zerop (mod i 3))                                            ; a slit in the roof
        (shape g :box (list (+ 120.0 (* 300.0 (r 420))) 1.0 4.0) (paint :daylight) (- (* 800.0 (r 421)) 400.0) 465.0 (* (- c) (r 422))
               :emit (paint :day) :energy 2.2 :unshaded t :shadow nil))
      (when (zerop (mod i 4))                                            ; and one in a wall
        (let ((side (if (< (r 430) .5) -1 1)))
          (shape g :box (list 1.0 (+ 60.0 (* 220.0 (r 431))) 3.0) (paint :daylight) (* side 594.0) (- (* 500.0 (r 432)) 150.0) (* (- c) (r 433))
                 :emit (paint :day) :energy 2.2 :unshaded t :shadow nil)))
      (when (eq hall :lamps)
        (dotimes (k 2)
          (shape g :box '(2.0 2.0 2.0) "#ffcf8a" (- (* 700.0 (r (+ 440 k))) 350.0) -414.0 (* (- c) (r (+ 450 k))) :emit "#ffb060" :energy 2.5
                 :unshaded t :shadow nil)))
      (when (zerop (mod i 3))                                            ; a pier from floor to roof
        (let ((side (if (< (r 400) .5) -1 1)))
          (shape g :box (list (+ 30.0 (* 40.0 (r 403))) 900.0 (+ 30.0 (* 40.0 (r 404)))) (paint :wall) (* side (+ 300.0 (* 260.0 (r 401)))) 25.0
                 (* (- c) (r 402)) :shadow nil))))))

(defzone zone-columns                                                        ; the level the old zones began with: a floor on columns, another above
         (mega-ground g i (paint :concrete))
         (shape g :box (list 520.0 4.0 c) "#4a4946" *way-x* 182.0 mid :shadow nil)
         (mega-columns g i (zerop (mod i 6))))

(defzone zone-poles                                                          ; panel houses, each on one pole, up and up
         (shape g :box (list 5.0 .3 c) (paint :gangway) 0.0 -.2 mid :shadow nil)                               ; a hanging gangway
         (dolist (x '(-2.6 2.6))
           (shape g :box (list .3 .5 c) (paint :iron) x .1 mid :shadow nil)
           (shape g :box '(.12 300.0 .12) (paint :iron) x 150.0 0 :shadow nil))
         (dotimes (k 3)
           (let* ((side (if (< (r (+ 1 k)) .5) -1 1)) (x (* side (+ 30.0 (* 250.0 (r (+ 3 k))))))
                  (up (+ 14.0 (* 280.0 (r (+ 5 k)) (r (+ 15 k))))) (z (- (* c (r (+ 7 k)))))
                  (h (group g x (+ up 7.5) z)))
             (shape g :cyl '(1.6 1.6 700.0 8) "#6a6c70" x (- up 350.0) z :shadow nil)
             (node-3d-set-rotation h (v3 0 (* 1.2 (- (r (+ 9 k)) .5)) 0))
             (shape h :box '(12.0 15.0 64.0) "#85857f" 0 0 0 :shadow nil)
             (when (< (r (+ 11 k)) .45)                                   ; somebody is at home
               (lamp h :window :box '(.3 1.6 2.0) (* side -6.1) 1.5 (- (* 40.0 (r (+ 13 k))) 20.0) :reach (and (< (abs x) 110.0) 46.0) :power 2.2
                     :at (list (* side -9.0) 1.5 0.0))))))

(defzone zone-pipes                                                          ; a heating main; the road lies on one of its pipes
         (if (< (vr (floor i 8) 77.0) .5)
             (progn                                                       ; inside the pipe: its wall all round us, plates missing here and there
               (dotimes (k 10)
                 (let ((a (* k (/ pi 5))))
                   (unless (and (< 1 k 9) (< (r (+ 900 k)) .1))
                     (shape g :box (list 6.2 .5 c) (if (< (r (+ 910 k)) .2) "#6a4a3a" (paint :steel)) (* 9.2 (sin a)) (- 8.9 (* 9.2 (cos a))) mid
                            :rot (list 0 0 a) :shadow nil))))
               (when (evenp i) (dotimes (k 10) (let ((a (* k (/ pi 5))))
                                                    (shape g :box '(6.4 1.4 1.6) "#5a5e60" (* 8.6 (sin a)) (- 8.9 (* 8.6 (cos a))) 0 :rot (list 0 0 a)
                                                           :shadow nil))))
               (when (zerop (mod at 4)) (lamp g :warm :box '(.6 .3 1.6) 0.0 17.2 mid :reach 30.0 :power 1.6 :at (list 0.0 14.0 mid))))
             (progn                                                       ; on its back
               (shape g :cyl (list 5.6 5.6 c 14) "#d4d6d2" 0.0 -5.62 (- (/ c 2)) :rot (list (/ pi 2) 0 0) :shadow nil)
               (when (evenp i) (shape g :cyl '(6.5 6.5 1.6 14) "#a9aca8" 0.0 -5.62 0 :rot (list (/ pi 2) 0 0) :shadow nil))))
         (when (zerop (mod i 3))                                          ; a pipe one could drive a train through
           (let ((rad (+ 22.0 (* 30.0 (r 15)))))
             (shape g :cyl (list rad rad 1400.0 16) (if (< (r 16) .5) "#aeb2ae" "#3a3a3e") *way-x*
                    (if (< (r 17) .5) (+ 30.0 rad (* 150.0 (r 18))) (- -30.0 rad (* 150.0 (r 18)))) mid
                    :rot (list 0 (* 1.2 (- (r 19) .5)) (/ pi 2)) :shadow nil)))
         (dotimes (k 4)
           (let* ((rad (+ .8 (* 8.0 (r (+ 20 k)) (r (+ 30 k))))) (z (- (* c (r (+ 40 k)))))
                  (rgb (nth (floor (* 3 (r (+ 50 k)))) '("#c9ccc8" "#8a8f93" "#2a2a2e"))) (way (r (+ 60 k))))
             (cond ((< way .4)
                    (shape g :cyl (list rad rad 700.0 10) rgb *way-x*
                           (if (< (r (+ 70 k)) .7) (+ 9.5 rad (* 90.0 (r (+ 80 k)))) (- -14.0 rad (* 90.0 (r (+ 80 k))))) z
                           :rot (list 0 0 (/ pi 2)) :shadow nil))
                   ((< way .75)
                    (shape g :cyl (list rad rad 900.0 10) rgb (* (if (< (r (+ 90 k)) .5) -1 1) (+ 11.0 rad (* 180.0 (r (+ 100 k))))) 0.0 z
                           :shadow nil))
                   (t
                    (shape g :cyl (list rad rad 700.0 10) rgb (* (if (< (r (+ 90 k)) .5) -1 1) (+ 60.0 (* 160.0 (r (+ 100 k)))))
                           (* 60.0 (r (+ 110 k))) z
                           :rot (list 0 (* 3.0 (r (+ 120 k))) 1.0) :shadow nil))))))

(defzone zone-archive                                                        ; along a shelf of the archive, between the files
         (mega-slab g 62.0 "#8a6a4a" -1.0) (shape g :box (list 62.0 3.0 c) (paint :wood) -1.0 -2.55 mid :shadow nil)
         (dolist (side '(-1 1))
           (let ((z 0.0) (k 0))
             (loop while (< z (- c 3.0))                                  ; files of every thickness, standing close; a gap now and then
                   do (let* ((w (+ 3.0 (* 9.0 (r (+ 1000 (* 40 side) k))))) (h (+ 46.0 (* 34.0 (r (+ 1100 (* 40 side) k)))))
                             (x (* side (+ 9.5 26.0))) (lean (if (< (r (+ 1200 (* 40 side) k)) .12) (* .16 (- (r (+ 1300 k)) .5)) 0.0)))
                        (unless (< (r (+ 1400 (* 40 side) k)) .14)
                          (shape g :box (list 52.0 h (- w .3))
                                 (nth (floor (* 5 (r (+ 1500 (* 40 side) k)))) '("#8a7458" "#6a5a48" "#55708a" "#7a6a52" "#5a4a3a"))
                                 x (/ h 2) (- (+ z (/ w 2))) :rot (list lean 0 0) :shadow nil)
                          ;; the label on its spine
                          (shape g :box (list .3 9.0 (* w .6)) "#e8e0d0" (- x (* side 26.1)) (* h .62) (- (+ z (/ w 2))) :shadow nil))
                        (incf z w) (incf k)))))
         (shape g :box (list 130.0 4.0 c) (paint :wood) -1.0 92.0 mid :shadow nil)                                        ; the shelf above
         ;; an upright of the rack, off to the side
         (when (zerop (mod at 8)) (shape g :box '(8.0 96.0 3.0) (paint :wood) -1.0 44.0 0 :shadow nil))
         (when (zerop (mod at 4))                                                                                     ; a lamp under the shelf
           (lamp g :warm :box '(3.0 .5 1.2) -1.0 89.5 mid :reach 110.0 :power 2.4 :at (list -1.0 70.0 mid)))
         (when (= (mod at 8) 3) (sign g "ДЕЛО №" 9.3 30.0 mid :h 5.0 :rgb "#2a2420"))
         ;; a file pulled out and left on the board
         (when (< (r 1) .1)
           (shape g :box '(8.0 6.0 30.0) "#7a6a52" (* (if (< (r 2) .5) -1 1) 6.0) 3.0 mid :rot (list 0 (* .5 (- (r 3) .5)) 0) :shadow nil)))

(defzone zone-paper                                                          ; a letter being written: what stands on the desk, far off
         (mega-slab g 900.0 "#d8d2c0")                                                                 ; a sheet of ruled paper
         (loop for z from 0.0 below c by 10.5 do (shape g :box '(900.0 .02 .35) "#9fb0c8" *way-x* -.03 (- z) :shadow nil))
         (shape g :box (list .5 .02 c) (paint :flag) 90.0 -.03 mid :shadow nil)                           ; the margin
         (live :ink (shape g :box (list 6.0 .04 c) "#1a2a6a" 0.0 -.02 mid :rough .2 :shadow nil))
         (when (zerop (mod i 11))
           (let ((x (* (if (< (r 1) .5) -1 1) (+ 220.0 (* 160.0 (r 2))))))
             (shape g :cyl '(46.0 60.0 80.0 14) "#1c2a3a" x 40.0 mid :rough .3 :shadow nil)                           ; an inkwell
             (shape g :cyl '(30.0 30.0 6.0 14) "#0a0f1e" x 82.0 mid :shadow nil)))
         ;; a pencil lying across the sheet
         (when (zerop (mod i 7))
           (shape g :cyl '(7.0 7.0 520.0 6) (paint :yellow) (* (if (< (r 3) .5) -1 1) (+ 120.0 (* 200.0 (r 4)))) 7.0 mid
                  :rot (list (/ pi 2) (* 1.2 (- (r 5) .5)) 0) :shadow nil))
         ;; lines already written, to either side
         (when (zerop (mod i 5))
           (let ((x (* (if (evenp (floor i 5)) -1 1) (+ 40.0 (* 60.0 (r 6))))))
             (dotimes (k 6) (shape g :box (list (+ 6.0 (* 14.0 (r (+ 10 k)))) .03 1.2) "#1a2a6a" (+ x (* 24.0 k (if (plusp x) 1 -1))) -.02
                                   (- (* 6.0 (r (+ 20 k)))) :shadow nil)))))

(defzone zone-statues                                                        ; the avenue of labour glory: busts whose heads are houses
         (mega-ground g i (paint :stone))
         (when (< (r 20) .62)
         (let* ((side (if (< (r 21) .5) -1 1)) (h (+ 20.0 (* 140.0 (r 1) (r 22)))) (x (* side (+ 30.0 (* 150.0 (r 2) (r 23))))))
           (shape g :box (list 24.0 (+ h 80.0) 24.0) (paint :stone) x (- (/ h 2) 40.0) -20.0 :rot (list 0 (* 1.5 (r 24)) 0) :shadow nil)
           (shape g :box '(16.0 14.0 34.0) "#d8d6cc" x (+ h 7.0) -20.0 :shadow nil)
           (shape g :ball '(11.0 12 10) "#d8d6cc" x (+ h 23.0) -20.0 :shadow nil)
           (shape g :box '(.4 6.0 15.0) (paint :flag) (- x (* side 12.2)) (* h .5) -20.0 :shadow nil)       ; a red plate with the name
           (shape g :box '(.3 2.1 1.0) "#3c2a20" (- x (* side 12.2)) 1.05 -27.0 :shadow nil)            ; a door of our size at its foot
           (shape g :box '(.4 .3 .3) "#ffe2a8" (- x (* side 12.3)) 2.6 -27.0 :emit "#ffcf8a" :energy 3 :shadow nil)
           (when (evenp (floor i 2)) (light g "#dfe8ff" 1.6 90.0 (- x (* side 24.0)) 12.0 -20.0))
           (when (< (r 3) .3)                                             ; and one beyond measure, far off
             (let ((bx (* (- side) (+ 260.0 (* 200.0 (r 4))))))
               (shape g :box '(120.0 420.0 120.0) (paint :stone) bx 210.0 -10.0 :shadow nil)
               (shape g :box '(100.0 90.0 210.0) "#c9c7be" bx 465.0 -10.0 :shadow nil)
               (shape g :ball '(78.0 16 12) "#c9c7be" bx 590.0 -10.0 :shadow nil))))))

;; the conveyor: it carries us through the machines; the shop floor is far below
(defzone zone-factory
         (shape g :box (list 13.0 1.2 c) "#1e1f22" -1.0 -.65 mid :shadow nil)                       ; a conveyor belt, and we are on it
         (dolist (x '(-8.2 6.2)) (shape g :box (list 1.0 2.4 c) (paint :yellow) x -.6 mid :shadow nil))
         (loop for z from 3.0 below c by 14.0 do (shape g :cyl '(1.0 1.0 15.0 8) "#5a5e60" -1.0 -2.3 (- z) :rot (list 0 0 (/ pi 2)) :shadow nil))
         (shape g :box '(4.0 110.0 4.0) "#8a6a1e" -1.0 -58.0 0 :shadow nil)
         (when (= (mod at 4) 2)                                           ; a press astride the belt: we pass under its head
           (dolist (x '(-22.0 20.0)) (shape g :box '(12.0 150.0 18.0) (paint :teal) x 60.0 mid :shadow nil))
           (shape g :box '(54.0 40.0 18.0) (paint :teal) -1.0 150.0 mid :shadow nil)
           (live :press (shape g :box '(28.0 26.0 15.0) (paint :steel) -1.0 60.0 mid :metal .4 :shadow nil) -1.0 mid 26.0 110.0
                 (+ 5.0 (* 4.0 (r 580))) (* 6.0 (r 581)))
           (shape g :box '(.6 1.6 1.6) "#ff3020" -15.7 12.0 mid :emit "#ff2010" :energy 2.5 :shadow nil))
         (when (= (mod at 4) 0)                                           ; a crate on the belt ahead of us, the size of a house
           (shape g :box '(9.0 8.0 12.0) "#a8843c" -1.0 4.0 -30.0 :rot (list 0 (* .3 (- (r 582) .5)) 0) :shadow nil))
         (let ((low (group g 0.0 -112.0 0.0)))
           (shape low :box (list 900.0 2.0 c) "#4f4e4a" -1.0 -1.0 mid :shadow nil)                                    ; the shop floor
           ;; a yellow line on it, a road wide
           (shape low :box (list 3.0 .1 c) (paint :yellow) 40.0 .1 mid :shadow nil)
           (dolist (side '(-1 1))
             (let* ((w (+ 60.0 (* 90.0 (r (+ 510 side))))) (h (+ 60.0 (* 260.0 (r (+ 520 side)))))
                       (x (* side (+ 70.0 (/ w 2) (* 110.0 (r (+ 500 side))))))
                    (face (- x (* side (/ w 2)))))
               (when (zerop (mod (+ i (if (> side 0) 1 0)) 2))
                 (shape low :box (list w h (* 1.7 c)) (if (< (r (+ 540 side)) .5) (paint :teal) (paint :blue)) x (/ h 2) mid :shadow nil)
                 (shape low :box (list (* w .6) 30.0 (* .8 c)) (paint :soot) x (+ h 15.0) mid :shadow nil)
                 (shape low :cyl (list 9.0 9.0 (+ h 160.0) 10) (paint :steel) (+ x (* side w .3)) (/ (+ h 160.0) 2) (- (* .3 c)) :shadow nil)
                 (dotimes (k 5)
                   (shape low :box '(.6 2.4 2.4) (if (evenp k) "#ff3020" "#ffcf7a") (- face (* side .4)) (+ 14.0 (* 16.0 k)) (- (* 9.0 k) 30.0)
                          :emit (if (evenp k) "#ff2010" "#ffb866") :energy 2.5 :shadow nil))
                 (light low "#ffcf8a" 2.0 90.0 (- face (* side 20.0)) 30.0 mid))))
           ;; lamps down on the floor, in rows
           (when (zerop (mod i 2))
             (dolist (x '(-150.0 -40.0 60.0 170.0))
               (shape low :box '(1.6 .6 1.6) "#ffe2a8" x 14.0 -10.0 :emit "#ffcf8a" :energy 3 :shadow nil)
               (shape low :box '(.3 14.0 .3) (paint :iron) x 7.0 -10.0 :shadow nil))))
         ;; the far walls, with their rows of windows
         (dolist (x '(-460.0 456.0))
           (shape g :box (list 10.0 900.0 c) (paint :wall) x 240.0 mid :shadow nil)
           (dotimes (k 3)
             (shape g :box '(.8 9.0 14.0) (paint :day) (if (< x 0) (+ x 5.4) (- x 5.4)) (- (* 520.0 (r (+ 560 k (round x)))) 100.0)
                    (* (- c) (r (+ 570 k (round x))))
                    :emit (paint :day) :energy 1.6 :unshaded t :shadow nil)))
         ;; another crane abreast of us, and what it carries
         (when (zerop (mod at 8))
           (let ((cx (+ 90.0 (* 200.0 (r 550)))))
             (shape g :box '(900.0 12.0 12.0) (paint :yellow) -2.0 60.0 -10.0 :shadow nil)
             (shape g :box '(30.0 18.0 30.0) (paint :yellow) cx 46.0 -10.0 :shadow nil)
             (dolist (dz '(-20.0 20.0)) (shape g :box '(.6 60.0 .6) (paint :soot) cx 8.0 (+ -10.0 dz) :shadow nil))
             ;; a whole five-storey house on the hook
             (shape g :box '(12.0 15.0 64.0) "#85857f" cx -28.0 -10.0 :shadow nil)))
         ;; skylights far above
         (shape g :box (list 120.0 2.0 c) "#dfeaf0" (* (if (evenp i) -1 1) 150.0) 640.0 mid :emit (paint :day) :energy 1.6 :unshaded t :shadow nil)
         (when (= (mod at 8) 4)                                                                                       ; a banner across the hall
           (shape g :box '(2.0 44.0 300.0) (paint :flag) 200.0 90.0 mid :shadow nil)
           (sign g (nth (mod (floor i 8) 5) '("СЛАВА ТРУДУ!" "ПЛАН — ЗАКОН" "ПЯТИЛЕТКУ — В ЧЕТЫРЕ ГОДА" "ВЫШЕ ЗНАМЯ СОРЕВНОВАНИЯ"
                                               "ЭКОНОМИКА ДОЛЖНА БЫТЬ ЭКОНОМНОЙ"))
                 198.8 90.0 mid :h 20.0 :rgb (paint :letters))))

;; a communal flat without end: its corridor, then a gallery over its yard, then the corridor again
(defzone zone-communal
         (if (< (vr (floor i 12) 78.0) .5)
             (progn (mega-slab g 70.0 (paint :wood) -2.0)                                               ; inside: the floorboards of the corridor
                    (loop for x from -34.0 to 30.0 by 8.0 do (shape g :box (list .2 .02 c) "#3c2a20" x -.03 mid :shadow nil)))
             (progn (shape g :box (list 12.0 .8 c) (paint :slab) -3.0 -.45 mid :shadow nil)            ; outside: a gallery over the yard
                    (shape g :box (list .25 1.1 c) (paint :iron) 3.1 .55 mid :shadow nil)))
         (when (< (vr (floor i 12) 78.0) .5)
           (dolist (x '(-36.0 32.0))
             (shape g :box (list 2.0 130.0 c) "#6a7a5a" x 65.0 mid :shadow nil)
             (shape g :box (list 2.2 40.0 c) (paint :green) x 20.0 mid :shadow nil))
           (shape g :box (list 72.0 2.0 c) "#b8ac94" -2.0 131.0 mid :shadow nil)
           (dolist (side '(-1 1))
             (let ((x (if (< side 0) -34.8 30.8)) (lit (< (r (+ 600 side)) .35)) (open (< (r (+ 650 side)) .18)))
               (if open                                                   ; a door standing open: a room beyond, with its own light
                   (progn (shape g :box '(.6 24.0 10.0) "#16171a" x 12.0 -21.0 :shadow nil)
                          (lamp g :window :box '(.7 18.0 7.0) (+ x (* side .2)) 11.0 -21.0 :shine 2.0 :reach 60.0 :power 2.0
                                :at (list (- x (* side 8.0)) 10.0 -21.0)))
                   (shape g :box '(.6 24.0 10.0) (paint :wood) x 12.0 -21.0 :shadow nil))
               (shape g :box '(.7 5.0 10.0) (if lit "#ffcf7a" (paint :soot)) x 27.5 -21.0 :emit (and lit "#ffb866") :energy 2.5 :shadow nil)
               (dotimes (k 4) (shape g :box '(.6 1.4 1.0) "#16171a" x (+ 8.0 (* 2.0 k)) -28.5 :shadow nil))))
           (shape g :cyl '(.15 .15 40.0 4) "#2a2a2e" -2.0 110.0 mid :shadow nil)
           (lamp g :soft :ball '(2.6 10 8) -2.0 88.0 mid :reach 120.0 :power 2.4 :at (list -2.0 84.0 mid))
           (when (zerop (mod at 3))                                       ; things left in the corridor: a trunk, a bicycle wheel, a basin
             (shape g :box '(9.0 6.0 14.0) "#5a2a22" (if (< (r 660) .5) -27.0 23.0) 3.0 -34.0 :shadow nil)
             (shape g :cyl '(8.0 8.0 .6 14) (paint :steel) (if (< (r 661) .5) -33.6 29.6) 30.0 -8.0 :rot (list 0 0 (/ pi 2)) :shadow nil)))
         (unless (< (vr (floor i 12) 78.0) .5)
         ;; our wall, storeys up and storeys down
         (shape g :box (list 4.0 700.0 c) "#6a7a5a" -11.0 0.0 mid :shadow nil)
         ;; doors of the flats along the gallery
         (loop for z from 4.0 below c by 10.0 for k from 0
               do (let ((lit (< (r (+ 600 k)) .35)))
                    (shape g :box '(.3 2.1 1.0) (paint :wood) -8.9 1.05 (- z) :shadow nil)
                    (shape g :box '(.3 1.2 1.4) (if lit "#ffcf7a" (paint :soot)) -8.9 1.6 (- (+ z 2.4)) :emit (and lit "#ffb866") :energy 2.5
                           :shadow nil)))
         (when (evenp i) (lamp g :soft :box '(.3 .3 .3) -8.6 3.0 mid :reach 22.0 :power 1.4 :at (list -6.0 3.0 mid)))
         ;; the galleries of the other floors
         (dolist (level '(-180.0 -120.0 -60.0 60.0 120.0 180.0))
           (shape g :box (list 12.0 .8 c) (paint :concrete) -3.0 level mid :shadow nil))
         (shape g :box (list 4.0 700.0 c) "#6a7a5a" 204.0 0.0 mid :shadow nil)                                        ; the wall across the yard
         (dolist (level '(-180.0 -120.0 -60.0 0.0 60.0 120.0 180.0))
           (shape g :box (list 12.0 .8 c) (paint :concrete) 196.0 level mid :shadow nil))
         ;; windows lit across the yard, on every floor
         (dotimes (k 7)
           (shape g :box '(.4 1.6 2.2) "#ffcf7a" 201.6 (+ 1.6 (* 60.0 (- (floor (* 7 (r (+ 620 k)))) 3))) (* (- c) (r (+ 630 k))) :emit "#ffb866"
                  :energy 3 :unshaded t :shadow nil))
         ;; washing on lines right across the well
         (when (zerop (mod at 2))
           (let ((y (+ 6.0 (* 60.0 (- (floor (* 5 (r 640))) 2)))))
             (shape g :box '(196.0 .1 .1) "#d8d0b8" 99.0 y -30.0 :shadow nil)
             (dotimes (k 6) (shape g :box '(5.0 4.4 .1) (nth (mod k 4) '("#e8e0d0" "#c9a13a" "#e8e0d0" "#8a4038")) (+ 20.0 (* 28.0 k)) (- y 2.4) -30.0
                                   :shadow nil))))
         ;; the yard itself, far below
         (shape g :box (list 196.0 2.0 c) (paint :floor) 100.0 -330.0 mid :shadow nil)
         (when (zerop (mod at 4)) (shape g :box '(2.0 2.0 2.0) "#ffe2a8" 100.0 -322.0 mid :emit "#ffcf8a" :energy 3 :unshaded t :shadow nil))
         (when (= (mod at 8) 5)
           (sign g "УХОДЯ, ГАСИТЕ СВЕТ" -8.8 3.4 mid :h .5 :rgb (paint :letters) :yaw (/ pi 2)))))

;; the garage co-operative from a flyover: built by many hands over many years
(defzone zone-garages
         (shape g :box (list 10.0 1.0 c) "#55524f" -1.0 -.55 mid :shadow nil)                       ; a flyover across the co-operative
         (dolist (x '(-6.2 4.2)) (shape g :box (list .3 .9 c) (paint :gangway) x .4 mid :shadow nil))
         (shape g :box '(3.0 70.0 3.0) "#5a5a58" -1.0 -36.0 0 :shadow nil)
         (let ((low (group g 0.0 -72.0 0.0)))
           (dolist (side '(-1 1))
             (dotimes (row 6)
               ;; each row on its own terrace
               (let* ((k (+ 700 (* 20 row) side)) (step (* 9.0 (- (floor (* 5 (vr (+ (floor i 4) (* 3 row) side) 71.0))) 2)))
                      (x (+ (* side (+ 34.0 (* 78.0 row))) (* 24.0 (- (r (+ k 1)) .5)))) (h (+ 16.0 (* 30.0 (r (+ k 2)) (r (+ k 3)))))
                      (len (* c (+ .5 (* .45 (r (+ k 4)))))) (z (- mid (* 8.0 (- (r (+ k 5)) .5))))
                      (rgb (nth (floor (* 5 (r (+ k 6)))) '("#5a2a22" "#3c5a48" "#55708a" "#8a8478" "#6a3a2e"))))
                 (shape low :box (list 78.0 60.0 c) "#4a4844" (* side (+ 34.0 (* 78.0 row))) (- step 30.0) mid :shadow nil)       ; the terrace
                 (unless (< (r (+ k 7)) .16)                                                                                       ; a plot left empty
                   (shape low :box (list (+ 40.0 (* 16.0 (r (+ k 8)))) h len) (paint :slab) x (+ step (/ h 2)) z
                          :rot (list 0 (* .1 (- (r (+ k 9)) .5)) 0) :shadow nil)
                   (shape low :box (list .5 (* h .8) (* len .7)) rgb (- x (* side 24.0)) (+ step (* h .4)) z :shadow nil)
                   ;; a second one built on its roof
                   (when (< (r (+ k 10)) .25)
                     (shape low :box (list 26.0 (+ 10.0 (* 14.0 (r (+ k 11)))) (* len .5)) "#77756f" (+ x (* 6.0 (- (r (+ k 12)) .5))) (+ step h 8.0)
                            z :shadow nil))))))
           ;; lamps where somebody put one
           (when (zerop (mod i 3))
             (let ((x (- (* 480.0 (r 760)) 240.0)))
               (shape low :box '(.3 46.0 .3) (paint :iron) x 23.0 -6.0 :shadow nil)
               (lamp low :warm :box '(3.0 .6 1.2) x 46.0 -6.0 :reach 90.0 :power 2.2 :at (list x 40.0 -6.0))))
           ;; somebody is in: doors ajar, light on the lane
           (when (= (mod at 4) 1)
             (shape low :box '(.6 14.0 8.0) "#ffcf7a" (- (* 200.0 (r 770)) 100.0) 8.0 -12.0 :emit "#ffb866" :energy 2.5 :shadow nil)))
         (when (= (mod at 8) 6) (sign g "ГСК «ЗАРЯ» · СТОРОЖ В БОКСЕ № 1" 4.4 2.4 mid :h .6 :rgb (paint :letters))))

(defzone zone-spiral                                                         ; the ramp: a wall on the left that has no top, nothing on the right
         (mega-slab g 17.0 (paint :concrete) -1.0)
         (shape g :box (list 2.0 600.0 c) "#6b6a66" -10.5 220.0 mid :shadow nil)
         (shape g :box (list .5 1.0 c) "#4a4946" 7.2 .5 mid :shadow nil)
         (dotimes (k 3)                                                   ; far across the shaft, lamps on other ramps
           (shape g :box '(.9 .9 .9) "#ffcf8a" (+ 120.0 (* 360.0 (r (+ 300 k)))) (- (* 500.0 (r (+ 310 k))) 320.0) (* (- c) (r (+ 320 k)))
                  :emit "#ffb060" :energy 2.5 :unshaded t :shadow nil))
         (when (zerop (mod at 4))
           (lamp g :warm :box '(.3 .5 1.4) -9.3 5.0 mid :reach 34.0 :power 1.5 :at (list -7.0 5.0 mid))))

;; the marshalling yard: a crane sets the containers down ahead of us, and takes them up behind
(defzone zone-rail
         (live :drop (shape g :box (list 12.0 12.0 (- c .8)) (nth (mod i 5) '("#5a2a22" "#3c5a48" "#55708a" "#a8843c" "#6a3a5a")) 0.0 -6.05 mid
                            :shadow nil)
               0.0 -6.05 mid)
         (let ((low (group g 0.0 -84.0 0.0)))
           (shape low :box (list 900.0 2.0 c) "#33312e" -1.0 -1.0 mid :shadow nil)                                    ; the yard, far below
           (dolist (x '(-150.0 -126.0 130.0 154.0)) (shape low :box (list 1.6 2.2 c) "#8a8e92" x 1.1 mid :shadow nil))  ; rails
           ;; stacks of containers, as they were left
           (dotimes (k 3)
             (let ((x (* (if (< (r (+ 900 k)) .5) -1 1) (+ 22.0 (* 230.0 (r (+ 910 k)) (r (+ 915 k))))))
                      (n (1+ (floor (* 6 (r (+ 920 k)) (r (+ 925 k)))))))
               (dotimes (j n)
                 (shape low :box '(12.0 12.0 40.0) (nth (floor (* 5 (r (+ 930 (* 10 k) j)))) '("#5a2a22" "#3c5a48" "#55708a" "#a8843c" "#6a3a5a"))
                        (+ x (* 1.4 (- (r (+ 940 (* 10 k) j)) .5))) (+ 6.0 (* 12.0 j)) (- mid (* 4.0 (- (r (+ 950 k)) .5))) :shadow nil))))
           (when (zerop (mod i 3))
             (let ((x (- (* 500.0 (r 960)) 250.0)))
               (shape low :box '(.6 110.0 .6) (paint :iron) x 55.0 -6.0 :shadow nil)
               (lamp low :cold :box '(8.0 1.0 3.0) x 110.0 -6.0 :reach 150.0 :power 2.2 :at (list x 100.0 -6.0))))))
