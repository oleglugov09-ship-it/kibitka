;;;; Traffic outside the window: the train on rails, cars (oncoming, overtaking, emergency vehicles with a
;;;; siren), birds, passers-by in towns. Decoration: every player has their own, it is not reconciled over
;;;; the network.

(in-package #:kiosk-trip)

;;; ------------------------------------------------------------------ the railway and the train

(defvar *trains* '() "The trains as (track head-path-coordinate car-nodes resting-until-time) lists.")
(defparameter *train-speed* 16.0)
(defparameter *car-gap* 15.0 "The spacing of train cars, in metres.")

(defun track-frame (track d &optional (up 0.0))
  "Return the point on the rails of TRACK at path coordinate D and the rotation along them.
The two values are (x y z), raised by UP, and (pitch heading 0)."
  (let* ((a (side-point (track-x track (- d 6)) (- d 6) up)) (b (side-point (track-x track (+ d 6)) (+ d 6) up))
         (dx (- (first b) (first a))) (dy (- (second b) (second a))) (dz (- (third b) (third a))))
    (values (mapcar (lambda (p q) (/ (+ p q) 2)) a b)
            (list (- (atan dy (sqrt (+ (* dx dx) (* dz dz))))) (atan dx dz) 0))))

(defun add-rail (track d)
  (multiple-value-bind (at rot) (track-frame track d)
    (let ((g (keep (group *world* (first at) (second at) (third at)) d)))
      (node-3d-set-rotation g (v3 (first rot) (second rot) 0))
      (shape g :box '(3.6 .6 12.6) "#6a6660" 0 -.12 0 :shadow nil)
      (shape g :box '(2.6 .1 12.6) "#3a3028" 0 .22 0 :shadow nil)
      (dolist (sx '(-.76 .76)) (shape g :box '(.1 .14 12.6) "#8a8e92" sx .32 0 :metal .6 :rough .4 :shadow nil)))))

(defun make-train (track)
  "Build the diesel locomotive and the cars for TRACK.
Each is a node of its own; they run along the rails one after another. Return the list of nodes."
  (declare (ignorable track))
  (flet ((car-node () (let ((g (group *world*))) (object-set g "visible" nil) g)))
    (let ((loco (car-node)))
      (shape loco :box '(2.4 .7 14.0) "#1e2022" 0 .8 0)
      (shape loco :box '(3.0 2.6 15.0) "#2f5a3c" 0 2.4 0)
      (shape loco :box '(3.04 .4 15.04) "#b0342c" 0 1.5 0 :shadow nil)
      (shape loco :box '(3.1 1.5 3.4) "#3a6a48" 0 3.9 4.6)
      (shape loco :box '(3.14 .8 3.0) "#16181c" 0 4.1 4.6 :shadow nil)
      (shape loco :ball '(.3 8 6) "#ffe9b0" 0 3.0 7.55 :emit "#ffd890" :energy 3 :shadow nil)
      (chimney-smoke loco 0 4.4 -2.0 :size 1.6 :amount 10 :life 3.0 :rgb "#6a6a70")
      (cons loco
            (loop for i from 1 to 9
                  collect (let ((g (car-node)))
                            (shape g :box '(2.4 .6 12.6) "#1e2022" 0 .8 0)
                            (case (mod (* i 5) 4)
                              (0 (shape g :box '(2.9 2.9 13.0) (nth (mod i 3) '("#6a3a2e" "#5a4636" "#7a4a30")) 0 2.55 0)
                               (shape g :box '(2.96 2.2 3.0) "#4a2e24" 0 2.3 0 :shadow nil))
                              (1 (shape g :cyl '(1.45 1.45 12.0 12) (if (evenp i) "#2a2c30" "#c9a13a") 0 2.6 0 :rot (list (/ pi 2) 0 0))
                               (shape g :cyl '(.5 .5 .6 8) "#2a2c30" 0 4.2 0))
                              (2 (shape g :box '(2.9 1.9 13.0) "#4a4e52" 0 2.05 0)
                               (shape g :box '(2.5 .7 12.2) "#141416" 0 3.1 0 :shadow nil))
                              (t (dolist (sx '(-.7 0 .7))
                                   (shape g :cyl '(.36 .36 11.5 8) "#6a5038" sx 1.6 0 :rot (list (/ pi 2) 0 0)))
                               (dolist (sx '(-.35 .35))
                                 (shape g :cyl '(.36 .36 11.5 8) "#7a5a3a" sx 2.2 0 :rot (list (/ pi 2) 0 0)))))
                            g))))))

(defun step-rail (delta)
  (dolist (track *tracks*)
    (destructuring-bind (d0 d1) track
      ;; rails: in 12 m pieces wherever they are not far from the highway
      (let ((track track))
        (cover (list :rail d0) 12.0
               (lambda (d i) (declare (ignore i))
                 (when (and (< d0 d d1) (< (track-x track d) 330)) (let ((*keep-reach* 270.0)) (add-rail track d))))
               270.0))
      ;; the train: catches up with us from behind, overtakes and leaves; then it rests
      (let ((tr (assoc track *trains*)) (inside (< (+ d0 120) *dist* (- d1 200))))
        (when (and inside (null tr) (not (net-guest-p)))
          (setf tr (list track (- *dist* 280.0) (make-train track) 0.0))
          (push tr *trains*))
        (when tr
          (destructuring-bind (s cars rest) (rest tr)
            (cond ((> rest 0.0)                                            ; resting
                   (when (and (< (decf (fourth tr) delta) 0.0) inside) (setf (second tr) (- *dist* 280.0))))
                  ((> (- s (* *car-gap* (length cars))) (- d1 40))          ; gone
                   (dolist (c cars) (object-set c "visible" nil))
                   (setf (fourth tr) 150.0))
                  (t
                   (incf (second tr) (* delta *train-speed*))
                   (loop for c in cars for i from 0
                         do (let ((d (- (second tr) (* i *car-gap*))))
                              (if (and (< (+ d0 30) d (- d1 30)) (< (track-x track d) 330) (< (abs (- d *dist*)) 520))
                                  (multiple-value-bind (at rot) (track-frame track d .1)
                                    (node-3d-set-position c (v3 (first at) (second at) (third at)))
                                    (node-3d-set-rotation c (v3 (first rot) (second rot) 0))
                                    (object-set c "visible" t))
                                  (object-set c "visible" nil))))))))))))

;;; ------------------------------------------------------------------ cars on the road
;;; Oncoming cars keep to their own lane; cars going our way catch up from behind, overtake in the oncoming
;;; lane and pull ahead.
;;; Once in a while there is an event: a fire engine, an ambulance or a chase, with flashers and a siren.

(defvar *cars* '() "The cars, each a plist (:node :d :v :dir :blink :n).")
(defvar *next-car* 8.0 "The time (by *TIME*) to release the next oncoming car.")
(defvar *next-pass* 35.0 "The time to release the next car going our way that will overtake.")
(defparameter *event-gap* '(900.0 2100.0)
  "The shortest and the longest pause between emergency vehicles, in seconds: a fire engine, an ambulance
or a chase is a rare sight, once in a quarter of an hour at the most.")
(defvar *next-event* 700.0 "The time to release the next emergency vehicles.")
(defvar *car-n* 0)

(defun car-wheels (g half zs &optional (r .34))
  (dolist (sx (list (- half) half))
    (dolist (sz zs) (shape g :cyl (list r r .22 8) "#141416" sx r sz :rot (list 0 0 (/ pi 2))))))

(defun make-car (n &optional kind)
  "Build car number N (its nose points to -z).
KIND is NIL for a passenger car or a truck, one of :fire :ambulance :police :runaway, or a vehicle on
business: :milk :bread :lavka :army. Return the node
and the flashers."
  (let ((g (group *world*)) (rgb (nth (mod n 6) '("#8a4038" "#3f6f73" "#b0aa98" "#55708a" "#a8843c" "#e0dcd0")))
        (blink '()) (nose -2.02))
    (flet ((flasher (x y z rgb) (push (shape g :box '(.3 .2 .3) rgb x y z :emit rgb :energy 5 :shadow nil) blink)))
      (case (or kind (if (zerop (mod n 5)) :truck :car))
        (:truck
         (shape g :box '(2.1 1.9 2.0) "#55708a" 0 1.6 -2.6) (shape g :box '(2.14 .7 1.6) "#16181c" 0 2.0 -2.75 :shadow nil)
         (shape g :box '(2.3 1.8 5.0) "#6a5a48" 0 1.9 1.2) (shape g :box '(2.0 .5 7.0) "#1e2022" 0 .75 0)
         (car-wheels g .95 '(-2.6 1.0 2.4) .45) (setf nose -3.62))
        (:fire
         (shape g :box '(2.3 2.2 2.2) "#b0241c" 0 1.8 -2.9) (shape g :box '(2.34 .8 1.7) "#16181c" 0 2.2 -3.1 :shadow nil)
         (shape g :box '(2.4 2.0 5.6) "#b0241c" 0 1.9 1.1) (shape g :box '(2.44 .3 5.6) "#e8e4d8" 0 1.4 1.1 :shadow nil)
         (shape g :box '(.7 .16 6.4) "#c8c8c4" 0 3.05 .6) (shape g :box '(2.0 .5 8.0) "#1e2022" 0 .75 0)
         (car-wheels g 1.0 '(-2.9 1.2 2.8) .5) (flasher -.7 3.0 -2.9 "#3060ff") (flasher .7 3.0 -2.9 "#3060ff") (setf nose -4.02))
        (:milk                                                        ; the milk lorry: a yellow tank behind the cab
         (shape g :box '(2.1 1.9 2.0) "#55708a" 0 1.6 -2.6) (shape g :box '(2.14 .7 1.6) "#16181c" 0 2.0 -2.75 :shadow nil)
         (shape g :cyl '(1.0 1.0 4.6 12) "#e0b830" 0 1.95 1.1 :rot (list (/ pi 2) 0 0))
         (shape g :box '(2.04 .3 4.0) "#2c4a7a" 0 1.95 1.1 :shadow nil) (shape g :box '(2.0 .5 7.0) "#1e2022" 0 .75 0)
         (car-wheels g .95 '(-2.6 1.0 2.4) .45) (setf nose -3.62))
        (:bread                                                       ; the bread van: a closed body with trays behind doors
         (shape g :box '(2.1 1.9 2.0) "#b0aa98" 0 1.6 -2.6) (shape g :box '(2.14 .7 1.6) "#16181c" 0 2.0 -2.75 :shadow nil)
         (shape g :box '(2.3 2.3 4.8) "#c8b890" 0 2.0 1.1) (shape g :box '(2.34 .5 4.84) "#a8843c" 0 1.4 1.1 :shadow nil)
         (dotimes (i 4) (shape g :box '(.02 1.5 .9) "#b8a880" 1.16 2.2 (- (* i 1.15) .6) :shadow nil))
         (shape g :box '(2.0 .5 7.0) "#1e2022" 0 .75 0) (car-wheels g .95 '(-2.6 1.0 2.4) .45) (setf nose -3.62))
        (:lavka                                                       ; the travelling shop: a blue van with a counter flap
         (shape g :box '(2.0 2.2 5.2) "#3f6f73" 0 1.7 0) (shape g :box '(2.04 .7 1.4) "#16181c" 0 2.2 -1.85 :shadow nil)
         (shape g :box '(.06 .9 2.0) "#e8e0d0" 1.03 1.9 .9 :shadow nil) (shape g :box '(.5 .06 2.0) "#8a6440" 1.2 1.45 .9 :shadow nil)
         (car-wheels g .9 '(-1.6 1.6) .38) (setf nose -2.62))
        (:army                                                        ; an army lorry under canvas
         (shape g :box '(2.1 1.9 2.0) "#4a5a3a" 0 1.6 -2.6) (shape g :box '(2.14 .7 1.6) "#16181c" 0 2.0 -2.75 :shadow nil)
         (shape g :box '(2.3 1.0 5.0) "#4a5a3a" 0 1.5 1.2) (shape g :cyl '(1.15 1.15 5.0 10) "#5a6a48" 0 2.0 1.2 :rot (list (/ pi 2) 0 0))
         (shape g :box '(2.0 .5 7.0) "#1e2022" 0 .75 0) (car-wheels g .95 '(-2.6 1.0 2.4) .5) (setf nose -3.62))
        (:ambulance
         (shape g :box '(2.0 2.1 5.0) "#e8e4d8" 0 1.65 0) (shape g :box '(2.04 .35 5.04) "#b0241c" 0 1.5 0 :shadow nil)
         (shape g :box '(2.04 .7 1.4) "#16181c" 0 2.1 -1.75 :shadow nil)
         (car-wheels g .9 '(-1.6 1.6) .38) (flasher 0 2.8 -1.6 "#3060ff") (setf nose -2.52))
        (t
         (when (eq kind :runaway) (setf rgb "#22222a"))
         (when (eq kind :police) (setf rgb "#e0dcd0"))
         (shape g :box '(1.6 .6 4.0) rgb 0 .65 0) (shape g :box '(1.5 .55 2.0) rgb 0 1.2 .2)
         (shape g :box '(1.52 .4 1.9) "#1a2026" 0 1.22 .2 :shadow nil)
         (car-wheels g .8 '(-1.3 1.3) .32)
         (when (eq kind :police)
           (shape g :box '(1.62 .22 4.02) "#2c4a7a" 0 .7 0 :shadow nil)
           (flasher -.35 1.6 .2 "#ff3020") (flasher .35 1.6 .2 "#3060ff"))))
      (dolist (sx '(-.6 .6))
        (shape g :ball '(.13 6 4) "#ffe9b0" sx .8 nose :emit "#ffd890" :energy 3 :shadow nil))
      (when blink
        (player g (wav-stream (sound-path "siren") :loop t) :positional t :db 4.0 :bus "Улица")))
    (values g blink)))

(defun army-near-p ()
  "Return true if the garrison town is within two and a half kilometres along the road."
  (some (lambda (tw) (and (eq (town-theme tw) :military) (< (abs (- (town-stop-d tw) *dist*)) 2500.0))) *towns*))

(defun add-car (d v dir &optional kind)
  (incf *car-n*)
  (multiple-value-bind (node blink) (make-car *car-n* kind)
    (push (list :node node :d d :v v :dir dir :blink blink :n *car-n* :kind kind :out nil :x nil) *cars*)))

(defun walker-on-road ()
  "Return (path-coordinate sideways-offset) when the player has got out and stands on the roadway, else NIL."
  (when (and (not *meta*) *ladder-down*)
    (let ((x (vector3-x *pos*)) (z (vector3-z *pos*)))
      (when (and (outdoors-p x z) (<= -7.2 x 2.6))
        (list (- *dist* (* z *dir*)) (side-x x))))))

(defun car-room (d dir x walker)
  "Return how many metres are free ahead of the car at path coordinate D driving in direction DIR in lane X.
The distance is measured to the wagon standing across the road in a U-turn and to the pedestrian WALKER
in the car's lane. Return NIL when there is nothing to fear."
  (let ((room nil))
    (flet ((mind (at keep) (let ((gap (- (* dir (- at d)) keep)))
                             (when (and (> gap -6.0) (< gap 40.0) (or (null room) (< gap room))) (setf room gap)))))
      (when (turning-p) (mind *dist* 13.0))
      (when (and walker (< (abs (- (second walker) x)) 2.4)) (mind (first walker) 5.0)))
    room))

(defun step-cars (delta)
  "Move the cars by DELTA seconds, each in its own direction (:DIR, along the path or against it) in its lane.
Oncoming cars drive past; a car going our way catches up, goes around in the oncoming lane and returns
to its own lane."
  (unless (or *meta* (net-guest-p) (not (place-rule :traffic)))         ; the driver sends the cars to a guest (sync.lisp); no cars where the place has none
    (flet ((oncoming (v gap &optional kind) (add-car (+ *dist* (* *dir* (+ 330.0 gap))) v (- *dir*) kind))
           (passing (v gap &optional kind) (add-car (- *dist* (* *dir* (+ 120.0 gap))) v *dir* kind)))
      (when (> *time* *next-car*)                                      ; an oncoming car, sometimes one on business
        (let ((r (vr *car-n* 11)) (v (+ 12.0 (* 5 (vr *car-n* 7)))))
          (cond ((and (> r .72) (army-near-p))                         ; by the garrison town: a column of three
                 (dotimes (i 3) (oncoming 12.0 (* i 20.0) :army)))
                (t (oncoming v 0 (cond ((< r .1) :milk) ((< r .2) :bread) ((< r .26) :lavka))))))
        (setf *next-car* (+ *time* (if (town-at *dist*) 9 16) (* 34 (vr *car-n* 3)))))
      (when (> *time* *next-pass*)                                     ; going our way: will catch up and overtake
        (passing (+ 14.0 (* 4 (vr *car-n* 5))) 0)
        (setf *next-pass* (+ *time* 30 (* 50 (vr *car-n* 6)))))
      (when (> *time* *next-event*)                                    ; a rare event: they rush towards us or catch up
        (let ((behind (evenp (floor *time* 13))))
          (flet ((send (kind v gap) (if behind (passing v gap kind) (oncoming v gap kind))))
            (case (mod (floor *time* 7) 3)
              (0 (send :fire 19.0 0) (send :fire 19.0 22))
              (1 (send :ambulance 21.0 0))
              (t (send :runaway 24.0 0) (send :police 24.0 20) (send :police 24.0 38)))))
        (setf *next-event* (+ *time* (first *event-gap*) (* (- (second *event-gap*) (first *event-gap*)) (vr *car-n* 9)))))))
  (let ((walker (walker-on-road)))
   (setf *cars*
        (remove-if
         (lambda (c)
           (let* ((dir (getf c :dir)) (with-us (= dir *dir*))
                  (own (if (> dir 0) 0.0 *lane-x*)) (other (- *lane-x* own))
                  (rel0 (ahead-of (getf c :d)))
                  ;; overtaking: into the oncoming lane only when it is free for 160 m ahead; once started, it finishes
                  (clear (notany (lambda (o) (and (/= (getf o :dir) dir) (< 0.0 (* dir (- (getf o :d) (getf c :d))) 160.0))) *cars*))
                  (out (and with-us (< rel0 44.0) (or (getf c :out) (and (> rel0 -30.0) clear))))
                  (want (cond ((not out) own) ((< rel0 14.0) other) (t (+ own (* (- other own) (- 1 (sstep 14 44 rel0)))))))
                  (x (let* ((old (or (getf c :x) own)) (step (* 3.5 delta)) (gap (- want old)))          ; changes lanes smoothly
                       (if (< (abs gap) step) want (+ old (* step (signum gap))))))
                  (held (and with-us (not out) (< -22.0 rel0 0.0)))                                     ; the oncoming lane is busy: waits behind us
                  (room (car-room (getf c :d) dir x walker))
                  (v (* (cond (held (min (getf c :v) *speed*)) (with-us (max (getf c :v) (+ *speed* 6))) (t (getf c :v)))
                        (if room (max 0.0 (min 1.0 (/ room 16.0))) 1.0)))                               ; gives way: brakes and waits
                  (d (incf (getf c :d) (* dir v delta))) (rel (ahead-of d)))
             (cond ((or (< rel -160) (> rel 360)) (node-queue-free (getf c :node)) t)
                   (t (progn
                        (setf (getf c :out) out (getf c :x) x)
                        (multiple-value-bind (course slope) (road-frame d)
                          (node-3d-set-position (getf c :node) (apply #'v3 (road-pos d x .1)))
                          (node-3d-set-rotation (getf c :node) (v3 (* dir slope) (- (if (> dir 0) 0 pi) course) 0))))
                      (loop for b in (getf c :blink) for i from 0
                            do (object-set b "visible" (evenp (+ i (floor (* *time* 4))))))
                      nil))))
         *cars*))))

;;; ------------------------------------------------------------------ birds

(defvar *flocks* '() "The flocks as (node birds born vx vz) lists.")
(defvar *next-flock* 14.0)
(defvar *flock-n* 0)

(defun make-flock (n &optional at (age 0.0) vx vz)
  "Build flock number N.
The driver starts it by the road ahead; a guest puts it where the driver has it (AT, AGE, VX, VZ)."
  (let* ((side (if (< (vr n 5) .75) 1 -1))                                 ; more often on the window side
         (g (apply #'group *world* (or at (side-point (* side (+ 40 (* 90 (vr n 2)))) (+ *dist* 60 (* 120 (vr n 9))) (+ 16 (* 16 (vr n 4)))))))
         (birds (loop for i below (+ 5 (floor (* 6 (vr n 6))))
                      collect (let ((b (group g (* (- (mod i 2) .5) 2.2 (ceiling i 2)) (* .3 (vr i n)) (* 1.5 (ceiling i 2)))))
                                (list (shape b :box '(.55 .04 .22) "#1c1c20" -.27 0 0 :shadow nil)
                                      (shape b :box '(.55 .04 .22) "#1c1c20" .27 0 0 :shadow nil)
                                      (* 6 (vr i 8)))))))
    (list g birds (- *time* age) (or vx (* side -3.5 (+ .5 (vr n 11)))) (or vz (- (+ 5.0 (* 4 (vr n 12))))) n)))

(defun step-birds (delta)
  (when (and (> *time* *next-flock*) (not *meta*) (not (net-guest-p)))
    (incf *flock-n*)
    (push (make-flock *flock-n*) *flocks*)
    (setf *next-flock* (+ *time* 18 (* 30 (vr *flock-n* 13)))))
  (setf *flocks*
        (remove-if (lambda (f)
                     (destructuring-bind (g birds born vx vz &optional n) f
                       (declare (ignore n))
                       (cond ((> (- *time* born) 50) (node-queue-free g) t)
                             (t (let ((p (node-3d-get-position g)))
                                  (node-3d-set-position g (v3 (+ (vector3-x p) (* vx delta)) (vector3-y p) (+ (vector3-z p) (* vz delta)))))
                                (dolist (b birds)
                                  (let ((a (* .7 (sin (+ (third b) (* *time* 9))))))
                                    (node-3d-set-rotation (first b) (v3 0 0 (- a)))
                                    (node-3d-set-rotation (second b) (v3 0 0 a))))
                                nil))))
                   *flocks*)))

;;; ------------------------------------------------------------------ passers-by in town

(defvar *walkers* '() "The passers-by as (slot node) lists: cats on the pavement.")
(defparameter *walker-step* 46.0 "The spacing, in metres of street, of the slots where a passer-by may walk.")
(defparameter *walker-x* 4.4)

(defun step-walkers ()
  (let ((k0 (ceiling (- *dist* 50) *walker-step*)) (k1 (floor (+ *dist* 165) *walker-step*)))
    (setf *walkers* (remove-if (lambda (w) (unless (<= k0 (first w) k1) (node-queue-free (second w)) t)) *walkers*))
    (loop for k from k0 to k1
          do (let ((d (* k *walker-step*)))
               (when (and (not (assoc k *walkers*)) (town-at (- d 20)) (town-at (+ d 20)) (< (vr k 21) .6))
                 (let ((g (group *world*)))
                   (node-3d-set-scale (first (make-cat g (floor (* 36 (vr k 22))))) (v3 .62 .62 .62))
                   (push (list k g) *walkers*)))))
    (dolist (w *walkers*)
      (destructuring-bind (k g) w
        (let* ((ph (+ (* 6 (vr k 23)) (* *day-t* .06))) (d (+ (* k *walker-step*) (* 17 (sin ph))))
               (x (if (< (vr k 24) .7) *walker-x* -7.2)))                   ; the pavement on the left is beyond the oncoming lane
          (destructuring-bind (px py pz) (side-point x d .08)
            (node-3d-set-position g (v3 px (+ (max py (+ (road-y d) .05)) (* .07 (abs (sin (* *time* 7))))) pz))   ; pavements lie flush with the road: do not sink into them
            (node-3d-set-rotation g (v3 0 (- (if (> (cos ph) 0) pi 0) (road-frame d)) 0))))))))
