;;;; The megastructure behind the tunnel: its zones in one table, the way through them, how a cell is built and how the
;;;; whole is kept around the wagon. What each zone looks like is in mega-zones.lisp and mega-monuments.lisp.

(in-package #:kiosk-trip)

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
(defparameter *mega-first* 24 "The number of cells in the first zone, the one the tunnel opens into: the way down on a platform.")
(defparameter *mega-lift* '(6 .45) "The cell at which the platform starts down, and how steeply it goes: metres down per metre forward.")

(defparameter *mega-order* '(:metro :quarry :dam :reactor :factory :pipes :communal :archive :paper :rail :statues :garages :spiral :poles :columns)
  "The kinds of zones, after the first one. Late-Soviet things grown past all measure: a workshop, a communal
corridor, a garage co-operative, a heating main, a left-luggage hall, an avenue of busts.")
(defvar *mega-parts* nil "The megastructure's moving parts, a plist: :cells (the group the cells hang in) and :entry (the tunnel we came by).")
(defvar *mega-cells* (make-hash-table) "The cells built right now: a table from a cell's number to its node.")
(defvar *mega-s* 0.0 "How far we have driven since the old world went out of sight.")




;;; What is known of each kind of zone, in one place. :NAME is what the corner of the screen says; :BUILD the
;;; function that builds a cell of it (mega-zones.lisp, mega-monuments.lisp). :HAZE is how much pale light stands
;;; there, :SUN how strong the day from the breach is against it (.5 unless said), :FOG how thick the air is (the
;;; usual murk unless said: a great hall is clear from end to end). :TURN is how many degrees a cell may turn and
;;; :STEEP how steep it may be; :WAY fixes the side it turns to, :DOWN makes it go down only. :HALL says the zone
;;; stands in the common hall - a roof, far walls, slits of day: T, or :DEEP with a floor far below, or :LAMPS
;;; with lamps on that floor too.

(defparameter *mega-zones*
  '(:lift     (:name "Спуск"                :build zone-lift     :haze .5  :sun 1.4 :fog .0009)
    :metro    (:name "Метро"                :build zone-metro    :haze .5  :sun 0.0 :fog .0009)
    :quarry   (:name "Карьер"               :build zone-quarry   :haze .7  :sun 3.0 :fog .0009 :turn 3.0 :steep .05 :way -1 :down t)
    :dam      (:name "Плотина"              :build zone-dam      :haze .5  :sun 1.4 :fog .0012 :turn .6 :way 1 :hall t)
    :reactor  (:name "Энергоблок"           :build zone-reactor  :haze .5  :sun 1.0 :fog .0016 :turn 1.5)
    :factory  (:name "Конвейер"             :build zone-factory  :haze .5  :turn 1.5)
    :pipes    (:name "Теплотрасса"          :build zone-pipes    :haze .24 :turn 4.5 :steep .07 :hall :lamps)
    :communal (:name "Коммуналка"           :build zone-communal :haze .38 :turn 1.0)
    :archive  (:name "Архив"                :build zone-archive  :haze .34 :turn 2.0)
    :paper    (:name "Письмо"               :build zone-paper    :haze .55 :turn 4.5)
    :rail     (:name "Сортировочная"        :build zone-rail     :haze .3  :turn 2.0 :steep .015 :hall :deep)
    :statues  (:name "Аллея трудовой славы" :build zone-statues  :haze .3  :turn 1.5 :hall t)
    :garages  (:name "Гаражный кооператив"  :build zone-garages  :haze .3  :turn 5.0 :steep .045 :hall :deep)
    :spiral   (:name "Пандус"               :build zone-spiral   :haze .12 :turn 6.0 :steep .07 :way 1 :down t)
    :poles    (:name "Микрорайон на сваях"  :build zone-poles    :haze .3  :turn 4.0 :hall :lamps)
    :columns  (:name "Вестибюль"            :build zone-columns  :haze .22 :turn 2.5 :hall t)
    ;; the hours of the summer (leto.lisp): flat land, a road that bends a little
    :morning  (:name "Утро"                 :build zone-morning  :turn 2.0)
    :noon     (:name "Полдень"              :build zone-noon     :turn 1.5)
    :storm    (:name "Гроза"                :build zone-storm    :turn 1.5)
    :evening  (:name "Вечер"                :build zone-evening  :turn 2.0)
    :night    (:name "Ночь"                 :build zone-night    :turn 2.0))
  "The kinds of zones and what is known of each: see above.")

(defun zone (kind key &optional default)
  "Return what is known under KEY of the zones of KIND, or DEFAULT."
  (getf (getf *mega-zones* kind) key default))

(defvar *mega-world* :mega "Which place lies behind the tunnel: :MEGA, the megastructure, or :SUMMER, the summer day that does not end (leto.lisp).")
(defparameter *summer-hours* '(:morning 62.0 :noon 350.0 :storm 470.0 :evening 722.0 :night 960.0)
  "The hours of the summer day, in the order they come, each with the second of the day cycle its zone is shown at.")

(defun mega-kind (i)
  "Return the kind of the zone that cell I belongs to. The first zone is the level on columns."
  (let ((z (if (< i *mega-first*) 0 (1+ (floor (- i *mega-first*) *mega-zone*)))))
    (if (or (<= z 0) (eq *mega-world* :summer))
        (if (eq *mega-world* :summer) (nth (* 2 (mod z (/ (length *summer-hours*) 2))) *summer-hours*) :lift)
        (let* ((n (length *mega-order*)) (block (floor (1- z) n)) (pos (mod (1- z) n)))
          (if (zerop block)
              (nth pos *mega-order*)                                     ; the first time round, in the order they are listed
              (let ((step (loop for s from (1+ (floor (* (1- n) (vr block 4.0)))) when (= 1 (gcd s n)) return s)))
                (nth (mod (+ (floor (* n (vr block 9.0))) (* pos step)) n) *mega-order*)))))))

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
      (let* ((kind (mega-kind i)) (b (floor i 8)) (way (vr b 21.0)) (grade (vr b 33.0)))
        (values (* (/ pi 180) (zone kind :turn 0.0)
                   (or (zone kind :way) (cond ((< way .3) 0) ((< way .55) 1) ((< way .8) -1) ((< way .9) 2) (t -2))))
                (atan (* (zone kind :steep 0.0) (cond ((zone kind :down) -1) ((< grade .4) 0) ((< grade .7) -1) (t 1))))))))

(defun mega-drop (j)
  "Return how far cell J goes down without tilting: on the way down from the tunnel the platform we stand on stays level."
  (if (and (eq *mega-world* :mega) (<= (first *mega-lift*) j (1- *mega-first*))) (* (second *mega-lift*) *mega-cell*) 0.0))

(defun mega-height (s)
  "Return the height of the way at S metres along it."
  (let* ((c *mega-cell*) (i (floor s c)) (f (- (/ s c) i)) (a (second (mega-pose i))))
    (+ a (* f (- (second (mega-pose (1+ i))) a)))))

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
                                (list (- x (* c (cos pitch) (sin yaw))) (- (+ y (* c (sin pitch))) (mega-drop j)) (- z (* c (cos pitch) (cos yaw)))
                                      (+ yaw turn) tilt))))))
           (setf *mega-pose-top* (max *mega-pose-top* i))
           (gethash i *mega-poses*))))

;;; The megastructure is at work, and the way is part of the work: a conveyor belt, the inside of a pipe, a shelf
;;; of an archive, the line a pen is writing ahead of us, containers a crane sets down one by one so that the road
;;; does not end. What moves is kept by cell in *MEGA-LIVES* as (kind node . numbers) and stepped every frame:
;;; :ink - a stretch of the line, there only once the pen has passed; :drop - a thing that hangs high far ahead,
;;; is lowered into place as we come and is lifted away behind; :press - a thing going up and down.

(defvar *mega-lives* (make-hash-table) "A table from a cell's number to what moves in it: a list of (kind node . numbers).")
(defvar *mega-live-now* '() "What moves in the cell being built right now.")
(defparameter *mega-pen* 240.0 "How far ahead of us the pen writes, in metres.")

(defun live (kind node &rest numbers) "Record that NODE of the cell being built moves in the manner KIND; return NODE." (push (list* kind node numbers) *mega-live-now*) node)

(defun step-mega-lives ()
  (let ((c *mega-cell*))
    (maphash (lambda (i lives)
               (let ((ahead (- (* i c) *mega-s*)))
                 (dolist (l lives)
                   (destructuring-bind (kind node . numbers) l
                     (ecase kind
                       (:ink (object-set node "visible" (< ahead *mega-pen*)))
                       (:drop (destructuring-bind (x y z) numbers                       ; lowered ahead of us, lifted behind
                                (node-3d-set-position node (v3 x (+ y (cond ((> ahead 130.0) (* 1.1 (- ahead 130.0))) ((< ahead -90.0) (* 1.1 (- -90.0 ahead))) (t 0.0))) z))))
                       (:train (destructuring-bind (x y z way period phase) numbers      ; comes in, stands at the platform, goes
                                 (let* ((u (mod (+ phase (/ *time* period)) 1.0))
                                        (off (cond ((< u .3) (* 2600.0 (expt (- 1.0 (/ u .3)) 2))) ((< u .7) 0.0) (t (- (* 2600.0 (expt (/ (- u .7) .3) 2)))))))
                                   (node-3d-set-position node (v3 x y (+ z (* way off)))))))
                       (:swing (destructuring-bind (amp period phase) numbers            ; a boom going from side to side
                                 (node-3d-set-rotation node (v3 0 (* amp (sin (+ phase (/ (* 6.283 *time*) period)))) 0))))
                       (:spin (destructuring-bind (yaw period) numbers                   ; a wheel turning
                                (node-3d-set-rotation node (v3 0 yaw (/ (* 6.283 *time*) period)))))
                       (:press (destructuring-bind (x z y0 y1 period phase) numbers
                                 (node-3d-set-position node (v3 x (+ y0 (* (- y1 y0) (+ .5 (* .5 (sin (+ phase (/ (* 6.283 *time*) period))))))) z)))))))))
             *mega-lives*)))

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
            (shape g :box '(.5 1.2 1.6) "#ffcf8a" (- x (* side (+ .3 (/ w 2)))) 7.0 z :emit "#ffb060" :energy 3 :shadow nil)
            (light g "#ffb866" 1.6 70.0 (- x (* side (+ 8.0 (/ w 2)))) 7.0 z)))))))

(defun mega-ground (g i rgb)
  "Lay the ground of cell I where a zone is driven over at floor level: not one plane but a strip under the wheels
and patches on either side, each at its own height, so that the place has ledges, pits and steps."
  (let* ((c *mega-cell*) (mid (- (/ c 2))))
    (flet ((r (k) (vr (+ i 3000.5) (float k))))
      (shape g :box (list 18.0 40.0 c) rgb -1.8 -20.05 mid :shadow nil)                          ; what we drive on
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

;;; Each kind of zone builds its cell in a function of its own (a single function for all of them grew past what
;;; the browser build could take). Inside, G is the cell's group, I its number, C its length, MID the z of its
;;; middle, AT its place in a run of sixteen, and (R k) the cell's own dice.

(defmacro defzone (name &body body)
  `(defun ,name (g i c mid at)
     (declare (ignorable g i c mid at))
     (flet ((r (k) (vr (+ i 1000.5) (float k))))
       (declare (ignorable (function r)))
       ,@body)))

;;; A cell is built by its zone; then, for the zones that stand in the common hall, the hall around it; then,
;;; every sixteenth cell, one of our own bus stops under a lamp.

(defun mega-slab (g w rgb &optional (x -1.8))
  "Lay a slab W metres wide along the cell G, its top just under the wheels."
  (shape g :box (list w 1.0 *mega-cell*) rgb x -.55 (- (/ *mega-cell* 2)) :shadow nil))

(defun mega-hall (g i hall)
  "Put the common hall around cell I: a roof, far walls and piers, the day coming in through slits only. With HALL
:DEEP or :LAMPS there is a floor far below, and with :LAMPS a lamp or two on it."
  (let* ((c *mega-cell*) (mid (- (/ c 2))))
    (flet ((r (k) (vr (+ i 1000.5) (float k))))
      (shape g :box (list 1200.0 8.0 c) "#161c20" -1.8 470.0 mid :shadow nil)
      (dolist (x '(-600.0 596.0)) (shape g :box (list 10.0 900.0 c) "#1c2428" x 30.0 mid :shadow nil))
      (unless (eq hall t) (shape g :box (list 1200.0 6.0 c) "#12171a" -1.8 -420.0 mid :shadow nil))
      (when (zerop (mod i 3))                                            ; a slit in the roof
        (shape g :box (list (+ 120.0 (* 300.0 (r 420))) 1.0 4.0) "#cfe6ee" (- (* 800.0 (r 421)) 400.0) 465.0 (* (- c) (r 422))
               :emit "#b9d6de" :energy 2.2 :unshaded t :shadow nil))
      (when (zerop (mod i 4))                                            ; and one in a wall
        (let ((side (if (< (r 430) .5) -1 1)))
          (shape g :box (list 1.0 (+ 60.0 (* 220.0 (r 431))) 3.0) "#cfe6ee" (* side 594.0) (- (* 500.0 (r 432)) 150.0) (* (- c) (r 433))
                 :emit "#b9d6de" :energy 2.2 :unshaded t :shadow nil)))
      (when (eq hall :lamps)
        (dotimes (k 2)
          (shape g :box '(2.0 2.0 2.0) "#ffcf8a" (- (* 700.0 (r (+ 440 k))) 350.0) -414.0 (* (- c) (r (+ 450 k))) :emit "#ffb060" :energy 2.5 :unshaded t :shadow nil)))
      (when (zerop (mod i 3))                                            ; a pier from floor to roof
        (let ((side (if (< (r 400) .5) -1 1)))
          (shape g :box (list (+ 30.0 (* 40.0 (r 403))) 900.0 (+ 30.0 (* 40.0 (r 404)))) "#39474d" (* side (+ 300.0 (* 260.0 (r 401)))) 25.0 (* (- c) (r 402)) :shadow nil))))))

(defun mega-stop (g i)
  "Put one of our bus stops by the way in the cell G, under a lamp."
  (let ((stop (group g *stop-x* 0.0 -20.0)))
    (shape g :box '(9.0 .5 14.0) "#6f6e69" 8.6 -.3 -20.0 :shadow nil)
    (build-stop stop (* 7.0 (floor i 16)))
    (shape g :box '(.14 5.6 .14) "#3a3c40" 5.0 2.8 -15.0 :shadow nil)
    (shape g :box '(.6 .2 .4) "#ffe2a8" 5.3 5.6 -15.0 :emit "#ffcf8a" :energy 3 :shadow nil)
    (light g "#ffcf8a" 2.2 26.0 5.6 5.0 -15.0)))

(defun build-mega-cell (cells i)
  "Build cell I of the megastructure under CELLS and return its group. Inside it x is to the right, y up, and the
cell runs from z = 0 back to z = -*MEGA-CELL*."
  (let* ((c *mega-cell*) (pose (mega-pose i)) (g (group cells (first pose) (second pose) (third pose)))
         (kind (mega-kind i)) (at (mod i 16)))
    (node-3d-set-rotation g (v3 (fifth pose) (fourth pose) 0))
    (funcall (zone kind :build) g i c (- (/ c 2)) at)
    (when (zone kind :hall) (mega-hall g i (zone kind :hall)))
    (when (and (= at 8) (not (eq kind :lift))) (mega-stop g i))
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
    (if (eq *mega-world* :summer)
        (progn                                                           ; the hill the tunnel comes out of, and what does not go by
          (shape entry :box '(320.0 34.0 330.0) *summer-grass* -1.8 24.8 -35.0 :shadow nil)
          (dolist (x '(-88.4 84.8)) (shape entry :box '(160.0 8.0 330.0) *summer-grass* x 3.9 -35.0 :shadow nil))
          (build-summer-far g))
        (progn                                                           ; the wall the tunnel comes out of: it goes up and down and to the right out of sight
          (shape entry :box '(1700.0 600.0 6.0) "#39474d" 800.0 312.0 -197.0 :shadow nil)
          (shape entry :box '(1700.0 1000.0 6.0) "#39474d" 800.0 -501.0 -197.0 :shadow nil)
          (shape entry :box '(1600.0 14.0 6.0) "#39474d" 814.0 6.0 -197.0 :shadow nil)))
    ;; The light of the place has a source: far off and high up there is a breach, and the day comes in through it
    ;; from one side. It hangs in the megastructure's own frame, so as the way turns it swings round us like a sun.
    (let ((sun (gdext:make-object "DirectionalLight3D")))
      (object-set sun "light_color" (hex "#b9d6de")) (object-set sun "light_energy" 0.0) (object-set sun "shadow_enabled" nil)
      (node-add-child turn sun)
      (node-3d-set-rotation sun (v3 -.36 .85 0))                 ; low: it rakes the sides of things and hardly touches the floors
      (unless (eq *mega-world* :summer) (shape turn :ball '(70.0 12 8) "#cfe6ee" -430.0 240.0 -380.0 :emit "#b9d6de" :energy 2.0 :unshaded t :shadow nil))   ; the breach itself
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
    (object-set (getf *mega-parts* :sun) "light_energy" (* (zone (mega-kind (floor *mega-s* c)) :sun .5) *haze*))
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
        (cond ((inside-mega-p) (if (eq *mega-world* :summer) (- 1.0 (sstep 150.0 235.0 *mega-s*)) 1.0))   ; the summer is under the sky: the dark ends with the tunnel
              ((and *tunnel* *mega* (> *dir* 0) (< *dist* (- (second *tunnel*) 40.0)))
               (float (max 0.0 (min 1.0 (/ (- *dist* (first *tunnel*) 20.0) 70.0))) 1.0))
              (t 0.0))))

(defvar *summer-grade* 0.0 "How far the picture has been turned from the faded film to the juicy summer, from 0 to 1.")

(defun step-mega ()
  "Show the megastructure to those inside it, and nothing of the old world; lay it out along the way driven; keep the
fuel up, since there is nowhere to fill it."
  (let ((in (inside-mega-p)))
    (when *land* (object-set *land* "visible" (not in)))
    (when *world* (object-set *world* "visible" (not in)))
    (let ((summer (and in (eq *mega-world* :summer))))                    ; there the sky shows the hour of the place, and the weather is its own
      (setf *sky-hour* (and summer (summer-hour *mega-s*)) *wx-force* (and summer (summer-weather *mega-s*)))
      (let ((k (if summer (- 1.0 *gloom*) 0.0)))                           ; and the picture is juicy, not a faded film
        (when (and *grade* (not (eql k *summer-grade*)))
          (setf *summer-grade* k)
          (object-call *grade* "set_shader_parameter" "sat" (+ .74 (* .3 k)))
          (object-call *grade* "set_shader_parameter" "gain" (+ .94 (* .04 k))))))
    (let ((want (if in (zone (mega-kind (floor *mega-s* *mega-cell*)) :haze 0.0) 0.0)))   ; the haze of the zone comes on slowly
      (setf *haze* (if in (+ *haze* (* .012 (- want *haze*))) 0.0)))
    (let ((want (zone (mega-kind (floor *mega-s* *mega-cell*)) :fog .0047)))             ; and so does its air
      (setf *fog-inside* (and in (not (eq *mega-world* :summer)) (+ (or *fog-inside* want) (* .012 (- want (or *fog-inside* want)))))))
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
  (zone (mega-kind (floor *mega-s* *mega-cell*)) :name))

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
                          (when (and (stringp q) (search "summer" q)) (setf *mega-world* :summer))   ; ?summer: the other place behind it
                          (and (stringp q) (or (search "tunnel" q) (search "summer" q))))))
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
