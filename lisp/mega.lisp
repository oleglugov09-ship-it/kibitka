;;;; The megastructure behind the tunnel: its zones and the way through them. How it is stepped is in mega-go.lisp.

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
(defparameter *mega-names* '(:columns "Вестибюль" :poles "Микрорайон на сваях" :pipes "Теплотрасса" :crater "Пролом"
                             :graves "Камера хранения" :statues "Аллея трудовой славы" :rail "Сортировочная" :spiral "Пандус"
                             :factory "Конвейер" :communal "Коммуналка" :garages "Гаражный кооператив" :archive "Архив" :paper "Письмо"
                             :lift "Спуск" :metro "Метро" :quarry "Карьер" :dam "Плотина" :reactor "Энергоблок")
  "What each kind of zone is called, in the corner of the screen.")
(defparameter *mega-kinds* '(:metro :quarry :dam :reactor :factory :pipes :communal :archive :paper :rail :statues :garages :spiral :poles :columns)
  "The kinds of zones, after the first one. Late-Soviet things grown past all measure: a workshop, a communal
corridor, a garage co-operative, a heating main, a left-luggage hall, an avenue of busts.")
(defvar *mega-parts* nil "The megastructure's moving parts, a plist: :cells (the group the cells hang in) and :entry (the tunnel we came by).")
(defvar *mega-cells* (make-hash-table) "The cells built right now: a table from a cell's number to its node.")
(defvar *mega-s* 0.0 "How far we have driven since the old world went out of sight.")
(defparameter *mega-haze* '(:columns .22 :poles .3 :pipes .24 :graves .2 :crater .3 :statues .3 :rail .3 :spiral .12
                            :factory .5 :communal .38 :garages .3 :archive .34 :paper .55
                            :lift .5 :metro .5 :quarry .7 :dam .5 :reactor .5)
  "How much haze stands in each kind of zone: in the open ones the far shapes show, the closed ones are dark.")

(defparameter *mega-sun* '(:quarry 3.0 :lift 1.4 :dam 1.4 :reactor 1.0 :metro 0.0)
  "How strong the day is in a kind of zone, against the haze there; half where nothing is said. A pit open to the sky is in daylight, a station has none.")

(defparameter *mega-fog* '(:lift .0009 :metro .0009 :quarry .0009 :dam .0012 :reactor .0016)
  "How thick the air is in a kind of zone; the usual murk where nothing is said. The great halls are clear from end to end.")

(defun mega-kind (i)
  "Return the kind of the zone that cell I belongs to. The first zone is the level on columns."
  (let ((z (if (< i *mega-first*) 0 (1+ (floor (- i *mega-first*) *mega-zone*)))))
    (if (<= z 0)
        :lift
        (let* ((n (length *mega-kinds*)) (block (floor (1- z) n)) (pos (mod (1- z) n)))
          (if (zerop block)
              (nth pos *mega-kinds*)                                     ; the first time round, in the order they are listed
              (let ((step (loop for s from (1+ (floor (* (1- n) (vr block 4.0)))) when (= 1 (gcd s n)) return s)))
                (nth (mod (+ (floor (* n (vr block 9.0))) (* pos step)) n) *mega-kinds*)))))))

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
             (turn (ecase kind (:columns 2.5) (:poles 4.0) (:pipes 4.5) (:graves 0.0) (:crater 3.0) (:statues 1.5) (:rail 2.0) (:spiral 6.0)
                           (:factory 1.5) (:communal 1.0) (:garages 5.0) (:archive 2.0) (:paper 4.5)
                           (:lift 0.0) (:metro 0.0) (:quarry 3.0) (:dam .6) (:reactor 1.5)))
             (steep (ecase kind (:columns 0.0) (:poles 0.0) (:pipes .07) (:graves .03) (:crater 0.0) (:statues 0.0) (:rail .015) (:spiral .07)
                            (:factory 0.0) (:communal 0.0) (:garages .045) (:archive 0.0) (:paper 0.0)
                            (:lift 0.0) (:metro 0.0) (:quarry .05) (:dam 0.0) (:reactor 0.0))))
        (values (* (/ pi 180) turn (cond ((eq kind :spiral) 1) ((eq kind :quarry) -1) ((eq kind :dam) 1) ((< way .3) 0) ((< way .55) 1) ((< way .8) -1) ((< way .9) 2) (t -2)))
                (atan (* steep (cond ((member kind '(:spiral :graves :quarry)) -1) ((< grade .4) 0) ((< grade .7) -1) (t 1))))))))

(defun mega-drop (j)
  "Return how far cell J goes down without tilting: on the way down from the tunnel the platform we stand on stays level."
  (if (<= (first *mega-lift*) j (1- *mega-first*)) (* (second *mega-lift*) *mega-cell*) 0.0))

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

(defzone zone-poles                                                          ; panel houses, each on one pole, up and up
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
         (when (zerop (mod i 11))
           (let ((x (* (if (< (r 1) .5) -1 1) (+ 220.0 (* 160.0 (r 2))))))
             (shape g :cyl '(46.0 60.0 80.0 14) "#1c2a3a" x 40.0 mid :rough .3 :shadow nil)                           ; an inkwell
             (shape g :cyl '(30.0 30.0 6.0 14) "#0a0f1e" x 82.0 mid :shadow nil)))
         (when (zerop (mod i 7))                                                                                      ; a pencil lying across the sheet
           (shape g :cyl '(7.0 7.0 520.0 6) "#c9a13a" (* (if (< (r 3) .5) -1 1) (+ 120.0 (* 200.0 (r 4)))) 7.0 mid :rot (list (/ pi 2) (* 1.2 (- (r 5) .5)) 0) :shadow nil))
         (when (zerop (mod i 5))                                                                                      ; lines already written, to either side
           (let ((x (* (if (evenp (floor i 5)) -1 1) (+ 40.0 (* 60.0 (r 6))))))
             (dotimes (k 6) (shape g :box (list (+ 6.0 (* 14.0 (r (+ 10 k)))) .03 1.2) "#1a2a6a" (+ x (* 24.0 k (if (plusp x) 1 -1))) -.02 (- (* 6.0 (r (+ 20 k)))) :shadow nil)))))

(defzone zone-crater                                                         ; a hole through the levels above: the day falls in, and snow lies under it
         (if (= at 8)
             (progn (shape g :ball '(60.0 14 8) "#e6eaf0" 110.0 -.4 -24.0 :scale '(1.0 .07 1.0) :shadow nil)
                    (shape g :cyl '(110.0 110.0 900.0 20) "#cfd8e6" 110.0 450.0 -24.0 :alpha .05 :unshaded t :shadow nil)
                    (shape g :cyl '(110.0 110.0 1.0 20) "#b9d6de" 110.0 900.0 -24.0 :emit "#b9d6de" :energy 2.5 :unshaded t :shadow nil)
                    (light g "#cfe2ea" 3.4 240.0 100.0 40.0 -22.0) (light g "#cfe2ea" 2.2 90.0 20.0 12.0 -20.0))
             (mega-columns g i nil)))

(defzone zone-statues                                                        ; the avenue of labour glory: busts whose heads are houses
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
         (shape g :box (list 2.0 600.0 c) "#6b6a66" -10.5 220.0 mid :shadow nil)
         (shape g :box (list .5 1.0 c) "#4a4946" 7.2 .5 mid :shadow nil)
         (dotimes (k 3)                                                   ; far across the shaft, lamps on other ramps
           (shape g :box '(.9 .9 .9) "#ffcf8a" (+ 120.0 (* 360.0 (r (+ 300 k)))) (- (* 500.0 (r (+ 310 k))) 320.0) (* (- c) (r (+ 320 k)))
                  :emit "#ffb060" :energy 2.5 :unshaded t :shadow nil))
         (when (zerop (mod at 4))
           (shape g :box '(.3 .5 1.4) "#ffe2a8" -9.3 5.0 mid :emit "#ffcf8a" :energy 3 :shadow nil)
           (light g "#ffcf8a" 1.5 34.0 -7.0 5.0 mid)))

(defzone zone-rail                                                           ; the marshalling yard: a crane sets the containers down ahead of us, and takes them up behind
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

