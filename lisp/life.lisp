;;;; The life of the roadside: things that stand or move by their own timetable, whether we look or not.
;;;; A queue where the shop sells against coupons, the board of honour, flags and a column on a holiday,
;;;; the shift walking from the factory, the bus that calls at the stop, a fir tree in winter, hay and
;;;; potatoes in the fields, the pioneer camp that is open in summer only. We are workers passing by: none
;;;; of it is a game to play, it is the country we drive through.
;;;;
;;;; A kind of life is declared once with DEFLIFE: where it may stand (SPOTS), what should be there right
;;;; now (STATE: NIL for nothing; when the answer changes, the thing is built anew), how to build it (BUILD)
;;;; and, if it moves, what to do every frame (STEP). The things have no collision: they are scenery.

(in-package #:kiosk-trip)

(defvar *life-kinds* '() "The kinds of life: entries of (name spots state build step), see DEFLIFE.")
(defvar *alive* '() "What stands now: entries of (name key path-coordinate node state).")
(defvar *life-parts* (make-hash-table :test 'eq) "What a moving thing consists of: node -> plist, for its STEP.")
(defvar *life-tick* 0.0)
(defparameter *life-reach* 150.0 "How far beyond the window, in metres, the life of a spot is kept.")

(defmacro deflife (name &key spots state build step)
  "Declare the kind of life NAME.
SPOTS is a function returning entries of (key path-coordinate). STATE is a function of the key and the
path coordinate returning what should stand there now, or NIL. BUILD is a function of key, path coordinate
and state returning the node. STEP, if given, is called every frame with node, key, path coordinate, state."
  `(progn (setf *life-kinds* (append (remove ,name *life-kinds* :key #'first) (list (list ,name ,spots ,state ,build ,step))))
          ,name))

(defun reset-life ()
  "Forget what stood: the nodes are gone together with the previous World."
  (setf *alive* '())
  (clrhash *life-parts*))

(defun drop-life (entry)
  (remhash (fourth entry) *life-parts*)
  (node-queue-free (fourth entry)))

(defun step-life (delta)
  "Bring the life of the roadside up to date twice a second, and move what moves every frame."
  (noting :life
    (when (> (incf *life-tick* delta) .5)
      (setf *life-tick* 0.0)
      (step-speaker)
      (let ((seen '()))
        (loop for (name spots state build) in *life-kinds*
              do (loop for (key d) in (funcall spots)
                       do (when (in-window-p d *life-reach*)
                            (let ((want (funcall state key d))
                                  (entry (find-if (lambda (e) (and (eq (first e) name) (equal (second e) key))) *alive*)))
                              (when (and entry (not (equal want (fifth entry))))
                                (drop-life entry)
                                (setf *alive* (remove entry *alive*) entry nil))
                              (when (and want (null entry))
                                (setf entry (list name key d (funcall build key d want) want))
                                (push entry *alive*))
                              (when entry (push entry seen))))))
        (dolist (e *alive*) (unless (member e seen) (drop-life e)))
        (setf *alive* (remove-if-not (lambda (e) (member e seen)) *alive*))))
    (dolist (e *alive*)
      (let ((step (fifth (assoc (first e) *life-kinds*))))
        (when step (funcall step (fourth e) (second e) (third e) (fifth e)))))))

;;; ------------------------------------------------------------------ the quarterly report on the screen

(defun step-report ()
  "Hold a quarterly report as soon as one more quarter of the loan is paid: cards for us, a line for the guests."
  (when (and (< *quarter-done* (debt-quarter)) (null *show*) (not (and (boundp '*ending*) *ending*)))
    (let ((cards (hold-report)))
      (log-event :report :quarter *quarter-done*)
      (say-all (format nil "~a" (substitute #\Space #\Newline (format nil "~{~a~^ · ~}" (rest cards)))))
      (open-show cards (lambda ())))))

;;; ------------------------------------------------------------------ helpers

(defun town-spots () "Return the markets of the towns as spots: (town-number path-coordinate)."
  (loop for tw in *towns* collect (list (town-index tw) (town-stop-d tw))))

(defun life-group (d x &optional (up 0.0) (parent *world*))
  "Return a group at path coordinate D, X metres to the right of the road axis, UP above the road.
Its x points to the right of the road and its z backwards along the path."
  (let ((g (apply #'group parent (road-pos d x up))))
    (node-3d-set-rotation g (v3 0 (- (road-frame d)) 0))
    g))

(defun land-group (d x parent)
  "Return a group like LIFE-GROUP that stands on the land, not at road height: for the fields."
  (let ((g (apply #'group parent (side-point x d))))
    (node-3d-set-rotation g (v3 0 (- (road-frame d)) 0))
    g))

(defun life-cat (parent x z index &key (yaw 0.0) (size .62))
  "Stand a cat under PARENT at (X Z), turned by YAW (0 looks backwards along the path). Return its group."
  (let ((g (group parent x 0 z)))
    (node-3d-set-rotation g (v3 0 yaw 0))
    (node-3d-set-scale (first (make-cat g index)) (v3 size size size))
    g))

(defun day-light-p () (> (daylight (sky-now)) .35))

;;; ------------------------------------------------------------------ the queue
;;; Where the shop lets something go against coupons, there is a queue: the more goods are rationed, the longer.

(deflife :queue
  :spots #'town-spots
  :state (lambda (town d) (declare (ignore d))
           (let ((n (length (rationed-goods town))))
             (and (plusp n) (day-light-p) (min 7 (+ 1 n)))))
  :build (lambda (town d n)
           (let ((g (life-group (+ d 21.5) 5.2)))
             (dotimes (i n)
               (let ((cat (life-cat g (* .12 (- (mod (* i 7) 3) 1)) (- (* i .78)) (+ (* 5 town) (* 3 i)))))
                 (when (zerop (mod (+ i town) 3))                                   ; a string bag in the paw
                   (shape cat :box '(.16 .2 .12) "#b0aa98" .22 .28 .05 :shadow nil))))
             g)))

;;; ------------------------------------------------------------------ the board of honour
;;; Every town keeps one by its market. Whoever closed its gaps hangs there: when we are its own, so do we.

(deflife :honour
  :spots #'town-spots
  :state (lambda (town d) (declare (ignore d)) (standing town))
  :build (lambda (town d standing)
           (declare (ignore town))
           (let ((g (life-group (- d 9.5) 5.9)))
             (dolist (z '(-1.15 1.15)) (shape g :box '(.1 2.3 .1) "#55554f" 0 1.15 z))
             (shape g :box '(.08 1.25 2.4) "#7a2a24" 0 1.6 0)
             (shape g :box '(.12 .22 2.5) "#d8d4c4" 0 2.33 0 :shadow nil)
             (sign g "ДОСКА ПОЧЁТА" -.07 2.33 0 :h .15 :rgb "#7a2a24")
             (dotimes (i 4) (shape g :box '(.02 .42 .32) "#d8d0bc" -.05 1.72 (- (* i .52) .78) :shadow nil))
             (when (eq standing :friend)                                            ; our own card, in a gilt frame
               (shape g :box '(.02 .34 .5) "#c9a13a" -.05 1.2 0 :shadow nil)
               (shape g :box '(.024 .26 .42) "#e8e0d0" -.055 1.2 0 :shadow nil)
               (sign g "КООПЕРАТИВ «КИБИТКА»" -.075 1.2 0 :h .05 :rgb "#3a2a1e"))
             g)))

;;; ------------------------------------------------------------------ the loudspeaker
;;; A horn on a pole by every market. It has no sound of its own: what it says comes as a line when we park.

(defvar *speaker-told* nil "The market whose loudspeaker we have already heard on this stop.")
(defvar *speaker-due* nil "The line waiting to be said: (time . text).")

(deflife :loudspeaker
  :spots #'town-spots
  :state (lambda (town d) (declare (ignore town d)) t)
  :build (lambda (town d state)
           (declare (ignore town state))
           (let ((g (life-group (+ d 2.2) 6.8)))
             (shape g :box '(.12 5.2 .12) "#5a3e26" 0 2.6 0)
             (dolist (z '(-.3 .3))
               (shape g :cone '(.26 .5 8) "#8a8e92" 0 4.9 z :rot (list (* (signum z) (/ pi 2)) 0 0) :metal .4))
             g)))

(defun speaker-line (town)
  "Return what the loudspeaker in TOWN says now: the holiday, or the news of the shops, or a line for the hour."
  (let* ((ph (day-phase *day-t*)) (h (holiday))
         (lines (getf *speaker-lines* (cond ((< ph 300) :morning) ((< ph 700) :day) (t :evening))))
         (short (first (short-goods town))) (plenty (first (surplus-goods town)))
         (pick (floor (+ (* 3 town) (/ *day-t* 300)))))
    (cond ((not (day-light-p)) nil)                                                 ; at night it is silent
          ((and h (fifth h))
           (format nil "Из репродуктора: «С праздником, товарищи!» Сегодня ~a: к столу ищут ~{~a~^, ~} и платят щедрее"
                   (tr (fourth h)) (mapcar #'good-name (fifth h))))
          (h (format nil "Из репродуктора: «Все на ~a!»" (string-downcase (tr (fourth h)))))
          ((and short (zerop (mod pick 3)))
           (format nil "Из репродуктора: «…население просят экономить ~a»" (good-name short)))
          ((and plenty (= 1 (mod pick 3)))
           (format nil "Из репродуктора: «…в магазины города завезли ~a»" (good-name plenty)))
          (lines (nth (mod pick (length lines)) lines)))))

(defun step-speaker ()
  "Say the loudspeaker's line once we have parked at a market and the first message has faded."
  (let ((place (place-here)))
    (cond ((and place (eq (first place) :market))
           (unless (eql *speaker-told* (third place))
             (setf *speaker-told* (third place) *speaker-due* (cons (+ *time* 9.0) (third place)))))
          ((> *speed* 2.0) (setf *speaker-told* nil *speaker-due* nil)))
    (when (and *speaker-due* (> *time* (car *speaker-due*)) (not (and *toast* (> (cdr *toast*) 0))))
      (let ((line (speaker-line (cdr *speaker-due*))))
        (setf *speaker-due* nil)
        (when line (say line))))))

;;; ------------------------------------------------------------------ columns: a holiday, the shift
;;; A column is cats walking one after another along the street, round and round the town's middle.

(defun build-column (d n x &key flags (speed 1.1) (gap 1.3))
  "Build N cats that walk along the street around path coordinate D, X metres to the right of the axis."
  (let ((holder (group *world*)) (cats '()))
    (dotimes (i n)
      (let ((g (group holder)))
        (node-3d-set-scale (first (make-cat g (+ (round d) (* 7 i)))) (v3 .62 .62 .62))
        (when (and flags (evenp i))
          (shape g :box '(.03 1.5 .03) "#8a6440" .2 .85 0 :shadow nil)
          (shape g :box '(.02 .42 .62) "#c22a22" .2 1.4 -.32 :shadow nil))
        (when (and flags (= i 0))                                                   ; the first two carry the banner
          (shape g :box '(1.5 .4 .03) "#c22a22" .55 1.25 .1 :shadow nil))
        (push g cats)))
    (setf (gethash holder *life-parts*) (list :cats (nreverse cats) :x x :speed speed :gap gap))
    holder))

(defun step-column (holder key d state)
  (declare (ignore key state))
  (let ((parts (gethash holder *life-parts*)))
    (loop for g in (getf parts :cats) for i from 0
          do (let ((s (+ d -55.0 (mod (- (* *time* (getf parts :speed)) (* i (getf parts :gap))) 110.0))))
               (destructuring-bind (px py pz) (road-pos s (getf parts :x) .06)
                 (node-3d-set-position g (v3 px (+ py (* .06 (abs (sin (+ i (* *time* 7)))))) pz))
                 (node-3d-set-rotation g (v3 0 (- pi (road-frame s)) 0)))))))

(deflife :parade                                    ; 1 May and 7 November: flags on the posts, a column down the street
  :spots #'town-spots
  :state (lambda (town d) (declare (ignore town d))
           (and (member (holiday-key) '(:mayday :november)) (day-light-p) (holiday-key)))
  :build (lambda (town d state)
           (declare (ignore town state))
           (let ((holder (build-column d 12 2.5 :flags t)))
             (loop for s from -60 to 60 by 15                                       ; flags along the pavement
                   do (let ((g (life-group (+ d s) 5.95 0.0 holder)))
                        (shape g :box '(.06 3.4 .06) "#8a8a84" 0 1.7 0)
                        (shape g :box '(.03 .55 .85) "#c22a22" 0 3.1 -.45 :shadow nil)))
             holder))
  :step #'step-column)

(defvar *shift-themes* '() "The characters of the towns where a shift walks home from the gates, from mods.")
(defvar *shift-hours* '() "When the shifts change, from mods: seconds since dawn, as (from to) ranges.")

(deflife :shift
  :spots #'town-spots
  :state (lambda (town d) (declare (ignore d))
           (let ((tw (find town *towns* :key #'town-index)) (ph (day-phase *day-t*)))
             (and tw (member (town-theme tw) *shift-themes*) (null (holiday-key))
                  (some (lambda (h) (< (first h) ph (second h))) *shift-hours*))))
  :build (lambda (town d state) (declare (ignore town state)) (build-column d 14 4.4 :speed 1.3 :gap 1.6))
  :step #'step-column)

;;; ------------------------------------------------------------------ the clean-up day, the first of September

(deflife :subbotnik                                 ; the first day of spring: everyone is out with a broom
  :spots #'town-spots
  :state (lambda (town d) (declare (ignore town d)) (and (eq (holiday-key) :subbotnik) (day-light-p)))
  :build (lambda (town d state)
           (declare (ignore state))
           (let ((holder (group *world*)) (brooms '()))
             (loop for (s x) in '((-34 5.1) (-22 5.0) (27 5.2) (40 5.0) (52 5.1)) for i from 0
                   do (let* ((g (life-group (+ d s) x 0.0 holder)) (cat (life-cat g 0 0 (+ town i) :yaw (* i 1.3)))
                             (broom (group cat .25 .5 .1)))
                        (shape broom :box '(.03 1.1 .03) "#8a6440" 0 .05 0 :shadow nil)
                        (shape broom :box '(.2 .22 .06) "#a8843c" 0 -.5 0 :shadow nil)
                        (push broom brooms)
                        (shape g :ball '(.3 6 4) "#6a5038" .7 .12 (* .4 (- (mod i 2) .5)) :scale '(1 .5 1))))   ; a heap of last year's leaves
             (setf (gethash holder *life-parts*) (list :brooms brooms))
             holder))
  :step (lambda (holder key d state)
          (declare (ignore key d state))
          (loop for b in (getf (gethash holder *life-parts*) :brooms) for i from 0
                do (node-3d-set-rotation b (v3 0 0 (* .45 (sin (+ i (* *time* 3.2)))))))))

(deflife :school                                    ; the first of September: the small ones go with flowers
  :spots #'town-spots
  :state (lambda (town d) (declare (ignore town d)) (and (eq (holiday-key) :school) (day-light-p)))
  :build (lambda (town d state)
           (declare (ignore state))
           (let ((g (life-group (- d 14.0) 5.0)))
             (life-cat g .3 -.9 (+ town 4) :yaw pi)                                 ; a grown-up sees them off
             (dotimes (i 4)
               (let ((kid (life-cat g (* .15 (mod i 2)) (* i .6) (+ town (* 5 i)) :yaw pi :size .42)))
                 (shape kid :box '(.02 .4 .02) "#3c5a48" .16 .45 0 :shadow nil)
                 (shape kid :ball '(.07 5 4) (nth (mod i 3) '("#d8504a" "#e8c040" "#e8e0f0")) .16 .68 0 :shadow nil)
                 (shape kid :box '(.16 .2 .08) "#7a4a2e" 0 .34 -.12 :shadow nil)))   ; a satchel on the back
             g)))

;;; ------------------------------------------------------------------ the season at the market
;;; On the paved strip in front of the first row of stalls: a fir tree and a snowman in winter, a lorry of
;;; watermelons in autumn; by the bus stop in spring, people with boxes of seedlings for their plots.

(deflife :fir
  :spots #'town-spots
  :state (lambda (town d) (declare (ignore town d))
           (and (eq *season* :winter) (if (eq (holiday-key) :newyear) :feast :tree)))
  :build (lambda (town d state)
           (let ((g (life-group (+ d 11.4) 8.6)))
             (shape g :cyl '(.12 .14 .5 6) "#5a3e26" 0 .25 0)
             (loop for (r h y) in '((.85 1.3 1.0) (.65 1.1 1.8) (.42 .9 2.5))
                   do (shape g :cone (list r h 8) "#2f5a3a" 0 y 0))
             (shape g :ball '(.1 5 4) "#ff5040" 0 3.05 0 :emit "#ff3020" :energy 2.5 :shadow nil)
             (dotimes (i 12)                                                        ; the garland
               (let* ((a (* i 1.9)) (y (+ .8 (* i .16))) (r (* .8 (- 1 (/ (- y .5) 2.8))))
                      (rgb (nth (mod i 4) '("#ffd040" "#ff5040" "#50a0ff" "#60e080"))))
                 (shape g :ball '(.05 5 3) rgb (* r (cos a)) y (* r (sin a)) :emit rgb :energy 2.2 :shadow nil)))
             (let ((snow (group g 0 0 2.4)))                                        ; a snowman beside it
               (shape snow :ball '(.34 8 6) "#eef2f6" 0 .3 0) (shape snow :ball '(.25 8 6) "#eef2f6" 0 .78 0)
               (shape snow :ball '(.17 8 6) "#eef2f6" 0 1.14 0)
               (shape snow :cone '(.04 .2 5) "#e07828" -.2 1.14 0 :rot (list 0 0 (/ pi 2)) :shadow nil)
               (shape snow :cyl '(.14 .11 .18 8) "#5a5a5e" 0 1.36 0))
             (when (eq state :feast)                                                ; New Year: lights over the lot, people at the tree
               (dotimes (i 9)
                 (let ((rgb (nth (mod i 3) '("#ffd040" "#ff5040" "#50a0ff"))))
                   (shape g :ball '(.06 5 3) rgb 0 (+ 3.3 (* .12 (abs (- i 4)))) (- (* i .7) 2.8) :emit rgb :energy 2.5 :shadow nil)))
               (dotimes (i 3)
                 (let ((cat (life-cat g -1.0 (- (* i 1.1) 1.1) (+ town (* 4 i)) :yaw (/ pi 2))))
                   (shape cat :ball '(.04 4 3) "#fff0a0" .2 .7 0 :emit "#ffe060" :energy 3 :shadow nil))))   ; a sparkler
             g)))

(deflife :melons
  :spots #'town-spots
  :state (lambda (town d) (declare (ignore town d)) (and (eq *season* :autumn) (day-light-p)))
  :build (lambda (town d state)
           (declare (ignore state))
           (let ((g (life-group (+ d 8.0) 8.6)))
             (shape g :box '(1.6 .2 2.9) "#6a5a48" 0 .95 .5) (shape g :box '(1.6 1.2 1.1) "#55708a" 0 1.3 -1.6)
             (dolist (x '(-.8 .8)) (shape g :box '(.06 .4 2.9) "#5a4a3a" x 1.2 .5))
             (car-wheels g .75 '(-1.5 1.2) .32)
             (dotimes (i 11)
               (shape g :ball '(.2 7 5) (if (evenp i) "#2f6a3a" "#3f7a44") (- (* .36 (mod i 4)) .54) (+ 1.2 (* .1 (mod i 2))) (- (* .5 (floor i 4)) .1)
                      :scale '(1 .9 1.15)))
             (life-cat g -1.2 .6 (+ town 2) :yaw (- (/ pi 2)))                       ; the seller, with scales
             (shape g :box '(.3 .06 .3) "#8a8e92" -1.15 .78 1.3 :metal .4) (shape g :box '(.4 .7 .4) "#7a5a3a" -1.15 .37 1.3)
             g)))

(deflife :seedlings
  :spots #'town-spots
  :state (lambda (town d) (declare (ignore town d)) (and (eq *season* :spring) (day-light-p)))
  :build (lambda (town d state)
           (declare (ignore state))
           (let ((g (life-group (- d 6.2) 5.1)))
             (dotimes (i 2)
               (let ((cat (life-cat g (* .2 i) (* i -.9) (+ town 9 i) :yaw (- (/ pi 2)))))
                 (shape cat :box '(.34 .12 .24) "#8a6440" 0 .42 .26 :shadow nil)
                 (dotimes (k 4) (shape cat :box '(.03 .16 .03) "#4a8a3a" (- (* k .08) .12) .56 .26 :shadow nil))))
             g)))

;;; ------------------------------------------------------------------ the bus
;;; A bus calls at the stop across the street by its own timetable: comes, stands, goes. It keeps to the far
;;; side, beyond the oncoming lane; when the wagon itself drives on that side, the bus stays away.

(defparameter *bus-period* 200.0 "The seconds of the day's clock between two buses at a stop.")

(deflife :bus
  :spots #'town-spots
  :state (lambda (town d) (declare (ignore town d)) (day-light-p))
  :build (lambda (town d state)
           (declare (ignore town d state))
           (let* ((holder (group *world*)) (g (group holder)))
             (shape g :box '(2.3 2.3 8.6) "#c8a030" 0 1.65 0) (shape g :box '(2.34 .8 8.0) "#1a2026" 0 2.1 0 :shadow nil)
             (shape g :box '(2.34 .25 8.64) "#e8e0d0" 0 1.25 0 :shadow nil) (shape g :box '(2.0 .4 8.0) "#1e2022" 0 .6 0)
             (car-wheels g 1.05 '(-2.9 2.7) .45)
             (dolist (sx '(-.7 .7)) (shape g :ball '(.13 6 4) "#ffe9b0" sx .9 -4.32 :emit "#ffd890" :energy 3 :shadow nil))
             (setf (gethash holder *life-parts*) (list :bus g))
             holder))
  :step (lambda (holder town d state)
          (declare (ignore state))
          (let* ((bus (getf (gethash holder *life-parts*) :bus))
                 (u (mod (+ *day-t* (* 37 town)) *bus-period*))
                 (far 90.0)
                 ;; along the path it comes from ahead and leaves behind: the far side drives against the path
                 (s (cond ((< u 9.0) (+ d (* far (expt (- 1 (/ u 9.0)) 2))))
                          ((< u 26.0) d)
                          ((< u 35.0) (- d (* far (expt (/ (- u 26.0) 9.0) 2))))))
                 (near (and s (- 1 (min 1.0 (/ (abs (- s d)) 30.0))))))             ; how far it has pulled over to the kerb
            (cond ((or (null s) (< *dir* 0)) (object-set bus "visible" nil))
                  (t (object-set bus "visible" t)
                     (multiple-value-bind (course slope) (road-frame s)
                       (node-3d-set-position bus (apply #'v3 (road-pos s (- *lane-x* (* 2.2 near)) .1)))
                       (node-3d-set-rotation bus (v3 (- slope) (- pi course) 0))))))))

;;; ------------------------------------------------------------------ the fields
;;; By some bus stops between the towns there is a field: haymaking in summer, the potato harvest in autumn.

(defvar *field-spots* nil "The fields: (stops-they-were-picked-from . spots).")

(defun field-spots ()
  (unless (eq (car *field-spots*) *stop-ds*)
    (setf *field-spots* (cons *stop-ds* (loop for d in *stop-ds*
                                              when (and (not (town-at d)) (< (vr (round d) 31) .4))
                                                collect (list (round d) d)))))
  (cdr *field-spots*))

(deflife :field
  :spots #'field-spots
  :state (lambda (key d) (declare (ignore key d))
           (case *season* (:summer :hay) (:autumn :potatoes)))
  :build (lambda (key d state)
           (let ((holder (group *world*)))
             (ecase state
               (:hay
                (loop for (s x) in '((-9 15) (2 19) (12 14) (20 22))                ; haycocks
                      do (let ((g (land-group (+ d s) x holder)))
                           (shape g :ball '(1.3 8 6) "#c8a850" 0 .7 0 :scale '(1 .95 1))
                           (shape g :cone '(.9 .9 8) "#b89840" 0 1.7 0)))
                (dotimes (i 3)                                                       ; and those who rake
                  (let* ((g (land-group (+ d -4 (* i 6)) (+ 13 (* 2 i)) holder)) (cat (life-cat g 0 0 (+ key i) :yaw (* i 2.1))))
                    (shape cat :box '(.03 1.3 .03) "#8a6440" .22 .6 .1 :rot '(.4 0 0) :shadow nil))))
               (:potatoes
                (let ((g (land-group (+ d 4) 13.5 holder)))                          ; the lorry they load
                  (shape g :box '(2.1 1.7 1.9) "#55708a" 0 1.5 -2.6) (shape g :box '(2.3 .9 4.6) "#6a5a48" 0 1.5 1.0)
                  (shape g :box '(2.0 .5 6.6) "#1e2022" 0 .75 0) (car-wheels g .95 '(-2.6 1.0 2.4) .45)
                  (dotimes (i 5) (shape g :ball '(.3 6 4) "#a08a5a" (- (* .45 (mod i 3)) .45) 2.15 (+ .2 (* .6 (floor i 3))) :scale '(1 .7 1.2))))
                (loop for (s x) in '((-8 15) (-3 18) (9 16) (14 20) (20 15)) for i from 0
                      do (let ((g (land-group (+ d s) x holder)))
                           (life-cat g 0 0 (+ key (* 3 i)) :yaw (* i 1.7))
                           (shape g :cyl '(.2 .16 .26 8) "#8a8e92" .4 .13 .2 :metal .3)               ; a bucket
                           (shape g :ball '(.32 6 4) "#a08a5a" -.5 .2 .3 :scale '(1 .75 1.2))))))     ; a sack
             holder)))

;;; ------------------------------------------------------------------ the pioneer camp
;;; A landmark in the pine wood (:camp in the landmark plan). In summer the flag is up and the detachments
;;; stand on parade; the rest of the year the gates are barred and a watchman minds them. We are not let in:
;;; the supply manager takes food for the canteen at the gates and pays by transfer, and the factory that
;;; keeps the camp counts it as a deed.

(defvar *camp-food* '() "The raw goods the camp's canteen takes, besides any cooked dish, from mods.")
(defvar *camp-daily* 6 "How many things the canteen takes in one game day; mods may change it.")
(defparameter *camp-gate-x* 12.0 "How far from the road axis the camp's gates stand.")
(defvar *camp-fed* nil "What the canteen has taken today: (day . count).")
(defvar *life-season* nil "The season the seasonal scenes were built in.")
(defparameter *camp-town* 99 "The camp's number in a parcel's address, in place of a town: no town has it.")
(defvar *camp-parcel-chance* .3 "The chance that a sender in summer brings a parcel for the camp: parents to their children.")
(defvar *camp-intl-town* 10
  "The town with the railway station where the trains from the fraternal republic stop: there the parcels
for the international session's children are handed to us.")

(defun place-name (town) "Return the name of TOWN as a parcel's address has it: a town, or the camp." (if (eql town *camp-town*) "лагерь «Костёр»" (town-name town)))
(defun camp-parcel-p (carry) "Return true if CARRY, a thing in the paws, is a parcel for the camp." (and (cdr carry) (eql (parcel-town (cdr carry)) *camp-town*)))

(defun camp-d ()
  "Return the path coordinate of the pioneer camp, or NIL."
  (let ((entry (find :camp *landmark-plan* :key #'fourth)))
    (and entry (nth (first entry) *towns*) (gap-d (first entry) (second entry)))))

(defun camp-open-p () (eq *season* :summer))

(defun mark-camp (g)
  "Build the pioneer camp in group G: x+ points away from the road, the gates face the road."
  (let ((open (camp-open-p)) (gate (- *camp-gate-x* 26.0)) (blue "#5f8f9a") (white "#e4e0d4"))
    ;; the fence and the gates
    (dolist (seg '((-21.0 -3.4) (3.4 21.0)))
      (let ((mid (/ (+ (first seg) (second seg)) 2)) (len (- (second seg) (first seg))))
        (shape g :box (list .08 .16 len) blue gate 1.25 mid) (shape g :box (list .08 .16 len) blue gate .5 mid)
        (loop for z from (first seg) to (second seg) by 1.1 do (shape g :box '(.07 1.35 .14) blue gate .7 z :shadow nil))))
    (dolist (z '(-3.2 3.2)) (shape g :box '(.7 4.4 .7) white gate 2.2 z) (shape g :ball '(.3 8 6) "#c22a22" gate 4.7 z))
    (shape g :box '(.3 .7 7.0) "#c22a22" gate 4.0 0)
    (sign g "ПИОНЕРЛАГЕРЬ «КОСТЁР»" (- gate .17) 4.0 0 :h .4 :rgb "#f0ece0")
    (cond (open (dolist (z '(-1.6 1.6)) (shape g :box '(3.0 1.5 .06) blue (+ gate 1.5) .85 (* 2 z) :shadow nil)))   ; the leaves stand open
          (t (shape g :box '(.08 1.5 6.2) blue gate .85 0)
             (shape g :box '(.1 .14 6.3) "#c22a22" (- gate .06) 1.0 0 :shadow nil)                                   ; the bar
             (shape g :box '(.1 .16 .12) "#2a2c30" (- gate .08) 1.0 0 :shadow nil)))                                 ; and the lock
    ;; the watchman's hut and the daily routine
    (shape g :box '(1.8 2.2 1.8) "#8a6440" (+ gate 1.4) 1.1 5.4) (shape g :box '(2.2 .16 2.2) "#4c4e52" (+ gate 1.4) 2.3 5.4 :rot '(0 0 -.1))
    (shape g :box '(.06 .7 .6) "#ffd890" (+ gate .48) 1.4 5.4 :emit "#ffd890" :energy .8 :shadow nil)
    (dolist (z '(-6.9 -5.1)) (shape g :box '(.08 1.9 .08) "#55554f" (+ gate 1.2) .95 z))
    (shape g :box '(.06 1.0 1.9) white (+ gate 1.2) 1.5 -6.0)
    (sign g "РАСПОРЯДОК ДНЯ" (+ gate 1.15) 1.85 -6.0 :h .12 :rgb "#7a2a24")
    (dotimes (i 5) (shape g :box '(.02 .04 1.4) "#55554f" (+ gate 1.16) (- 1.65 (* i .13)) -6.0 :shadow nil))
    ;; the plaster bugler
    (let ((b (group g (+ gate 3.2) 0 8.6)))
      (shape b :box '(1.0 1.0 1.0) white 0 .5 0) (shape b :cyl '(.22 .26 1.2 8) white 0 1.6 0)
      (shape b :ball '(.2 8 6) white 0 2.35 0)
      (shape b :cyl '(.03 .07 .6 6) "#c9a13a" -.3 2.45 0 :rot (list 0 0 (- (/ pi 2) .5)) :metal .6)
      (shape b :box '(.4 .08 .08) white -.18 2.2 0 :rot '(0 0 .5)))
    ;; the parade ground: the mast, and in summer the flag and the detachments
    (shape g :box '(9.0 .06 13.0) "#8a8880" -7.0 .03 0 :shadow nil)
    (shape g :box '(.12 9.0 .12) "#c8c8c4" -9.0 4.5 0)
    (shape g :box '(.8 .3 .8) white -9.0 .15 0)
    (when open
      (shape g :box '(.04 .8 1.3) "#c22a22" -9.0 8.5 -.7 :shadow nil)
      (dotimes (row 2)
        (dotimes (i 7)
          (let ((kid (life-cat g (+ -5.5 (* row .8)) (- (* i .75) 2.25) (+ (* 11 row) (* 3 i)) :yaw (- (/ pi 2)) :size .42)))
            (shape kid :box '(.2 .05 .1) "#c22a22" 0 .44 .1 :shadow nil))))                                         ; a red neckerchief
      (life-cat g -7.6 -3.4 7 :yaw (/ pi 2))                                                                         ; the leader
      (let ((fire (group g -2.0 0 9.5)))                                                                             ; the evening fire
        (dotimes (i 4) (shape fire :box '(.1 .1 1.0) "#5a3e26" 0 .3 0 :rot (list .9 (* i (/ pi 2)) 0) :shadow nil))
        (moving (shape fire :cone '(.4 1.1 6) "#ff9030" 0 .7 0 :emit "#ff7020" :energy 3 :shadow nil) :night)
        (moving (light fire "#ff9040" 2.4 14.0 0 1.0 0) :night)))
    (unless open (life-cat g (+ gate 1.5) 4.2 3 :yaw (/ pi 2)))                                                      ; the watchman by his hut
    ;; the dormitories, the canteen, a loudspeaker
    (dolist (z '(-12.0 12.0))
      (shape g :box '(8.0 3.0 9.0) blue 6.0 1.5 z) (shape g :box '(8.6 .2 9.6) "#4c4e52" 6.0 3.2 z :rot '(0 0 -.06))
      (shape g :box '(.06 1.0 6.6) (if open "#ffd890" "#3a4048") 1.97 1.7 z :emit (and open "#ffd890") :energy .5 :shadow nil)
      (shape g :box '(2.0 .1 9.0) "#8a6440" 1.0 .4 z))
    (shape g :box '(7.0 3.4 8.0) white -3.0 1.7 -15.5) (shape g :box '(7.6 .2 8.6) "#4c4e52" -3.0 3.6 -15.5 :rot '(0 0 -.05))
    (sign g "СТОЛОВАЯ" -6.55 2.7 -15.5 :h .4 :rgb "#7a2a24")
    (shape g :box '(.12 6.0 .12) "#5a3e26" -4.0 3.0 7.0) (shape g :cone '(.3 .6 8) "#8a8e92" -4.0 5.8 6.7 :rot (list (- (/ pi 2)) 0 0) :metal .4)
    g))

(defun step-seasonal-scenes ()
  "Take the camp down when the season turns, so that it is built anew as the season has it."
  (unless (eq *life-season* *season*)
    (setf *life-season* *season*)
    (let ((d (camp-d)))
      (when d
        (dolist (s *scenes*)
          (when (and (scene-node s) (< (scene-d0 s) d (scene-d1 s)) (< (- (scene-d1 s) (scene-d0 s)) 70))
            (node-queue-free (scene-node s))
            (setf (scene-node s) nil (scene-moves s) nil)))))))

(defun camp-label ()
  (cond ((not (camp-open-p)) "Пионерлагерь «Костёр». Закрыт до лета")
        ((camp-parcel-p *carry*) "E — отдать вожатой посылку для пионера")
        ((null *carry*) "Хозворота лагеря: завхоз берёт для столовой съестное — блюда, молоко, хлеб, яблоки, мёд")
        (t (format nil "E — сдать завхозу ~a" (carry-name *carry*)))))

(defshared camp-supply :camp ()
  "Hand what we carry to the camp's supply manager: food for the canteen, paid by transfer."
  (let ((kind (car *carry*)) (day (today)))
    (unless (eql (car *camp-fed*) day) (setf *camp-fed* (cons day 0)))
    (cond ((camp-parcel-p *carry*) (camp-parcel))
          ((not (camp-open-p)) (say "Сторож: «Смена кончилась. Приезжай летом»"))
          ((null *carry*) (say "Завхоз: «Столовой нужно съестное: блюда, молоко, хлеб, яблоки, мёд. Неси к воротам»"))
          ((or (cdr *carry*) (not (or (dish-p kind) (member kind *camp-food*)))) (say "Завхоз: «Это нам ни к чему. Детей кормим, а не склад держим»"))
          ((>= (cdr *camp-fed*) *camp-daily*) (say "Завхоз: «На сегодня столовая сыта. Завтра приезжай»"))
          (t (let ((pay (round (* (if (dish-p kind) 12 6) (list-price (raw-of kind))))))   ; an organisation pays by transfer, on its own scale
               (incf (cdr *camp-fed*))
               (coop-pay pay)
               (setf *carry* nil *carry-from* nil)
               (show-carry)
               (let ((turn (add-deeds *rush-town* 1)))                              ; the factory keeps the camp: a deed with its director
                 (save-plan) (save-interior)
                 (log-event :camp :good kind :pay pay)
                 (say-all (format nil "Завхоз расписался: ~a для столовой, ~/kiosk-trip::rub/ на счёт. Шефы с завода это запомнят" (good-name kind) pay))
                 (when turn (say-standing turn))
                 (let ((line (person-speaks :camp))) (when line (say-later line 4.0)))))))))

(defun camp-parcel ()
  "Hand the parcel in the paws to the camp. In summer the leader takes it and pays for the delivery; once
the session is over the watchman takes it to send back to the parents, for half."
  (let* ((open (camp-open-p)) (pay (if open (delivery-pay) (ceiling (delivery-pay) 2))))
    (incf *money* pay)
    (setf *carry* nil *carry-from* nil)
    (show-carry) (save-interior)
    (note-quest :post)
    (log-event :camp-parcel :pay pay :open open)
    (say-all (if open
                 (format nil "Вожатая: «Отряд, построиться — почта!» Посылка вручена: +~/kiosk-trip::rub/" pay)
                 (format nil "Сторож: «Смена разъехалась. Отошлю родителям обратно». +~/kiosk-trip::rub/" pay)))))

(defun life-usables ()
  "Return the things to press outside that belong to the life of the roadside: the camp's gates."
  (let ((d (camp-d)))
    (when (and d (< *speed* .4) (< (abs (- d *dist*)) 80.0))
      (list (road-usable *camp-gate-x* d 1.3 #'camp-label #'camp-supply 4.5 1.4)))))
