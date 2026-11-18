;;;; Coziness: things to do in the wagon while it rolls.
;;;;  • the camera by the window: a shot of what you see is hung as a photo card above the rug;
;;;;  • pancakes: milk on the hot stove turns into pancakes in half a minute, and they sell for more;
;;;;  • dice on the tea table: everyone in the wagon sees the roll;
;;;;  • the guitar: live music, at a bus stop coins are tossed twice as often;
;;;;  • the radio: a tune and short bulletins on what is ahead and where a parcel is awaited;
;;;;  • pennants: one for the first parcel delivered in a town, they hang above the window;
;;;;  • sleep: at night, sit down on the bunk and doze off, and time runs fast until morning.
;;;; What all players share (pennants, the radio, the dice roll) travels over the network in the spare slot
;;;; of the switches (SWITCHES in net.lisp); photo cards, the guitar and sleep are each player's own.

(in-package #:kiosk-trip)

(defvar *pennants* 0 "The town pennants as a bit mask: one bit per town index.")
(defvar *radio-on* nil)
(defvar *dice* (list 0 1 1 "") "The last dice roll as (sequence-number first second who).")
(defvar *guitar-on* nil "True while we are playing the guitar.")
(defvar *swatter* nil "True once the fly swatter is bought (it is sold only at the gas station by the swamp).")
(defvar *mosquito* nil "The mosquito in the wagon: how many seconds it has been here, or NIL.")
(defvar *swamp-time* 0.0 "The number of seconds we have been driving through the swamp.")
(defparameter *swatter-at* '(1.44 1.5 2.62))
(defvar *photos* '() "The photo cards on the wall as (slot caption) lists, newest first.")
(defvar *snapped* '() "The landmarks already photographed, as their path coordinates.")
(defparameter *photo-slots* 12)

(defvar *uyut* nil "The coziness nodes, as a plist.")
(defvar *photo-wait* 0 "The number of frames until the shot is taken; the HUD is hidden for that time.")
(defvar *photo-hud* nil)
(defvar *radio-timer* 20.0)
(defvar *radio-n* 0)
(defvar *sleep* nil "The sleep state: (previous time scale) while we sleep, else NIL.")
(defvar *doze* 0.0 "The number of seconds we have been sitting on the bunk at night.")
(defvar *cook* '() "What is cooking right now: ((item-record . seconds-left) ...).")
(defparameter *camera-at* '(1.44 1.82 1.62))
(defparameter *radio-at* '(1.14 1.45 2.38) "The dial and the knob of the radio: on the front of the radiogram (jukebox.lisp).")
(defparameter *guitar-at* '(1.22 1.35 2.74))
(defparameter *stove-top* .77 "The height of the stove top above the floor, level with the washstand: things are put on it to cook.")

(defun un (key) (getf *uyut* key))

;;; ------------------------------------------------------------------ shared over the network

;;; What is shared here is declared at the end of the file (DEFCOMMON): pennants, radio, dice, fly swatter.

;;; ------------------------------------------------------------------ flashlight (F)
;;; Everyone has their own: a cone beam from the paw; the flashlight itself is visible in the corner of the
;;; screen, and other players see it at their cat's muzzle. Whether it is lit travels with the look (the
;;; hundreds digit in MY-LOOK).

(defvar *torch* nil "Our flashlight: NIL when off, :beam for a beam forward, :chin for held upright under the muzzle.")
(defvar *torch-node* nil "The flashlight in the paw: a node under the camera.")

(defun torch-model (parent x y z &key (yaw 0.0) chin)
  "Build a flashlight with a beam under PARENT at X Y Z and return its node.
It shines towards -z (YAW turns it further). With CHIN it is held upright under the muzzle: the light
comes from below, short-range and wide, which makes a scary face in the dark."
  (let ((g (group parent x y z)) (l (gdext:make-object "SpotLight3D")))
    (node-3d-set-rotation g (v3 (if chin (/ pi 2) 0) yaw 0))
    (shape g :cyl '(.02 .02 .15 8) "#2a2a30" 0 0 .04 :rot (list (/ pi 2) 0 0) :metal .4 :rough .5 :shadow nil)
    (shape g :cyl '(.034 .022 .05 10) "#c8c8c4" 0 0 -.055 :rot (list (- (/ pi 2)) 0 0) :metal .7 :rough .3 :shadow nil)   ; the flared end points forward
    (shape g :cyl '(.03 .03 .006 10) "#fff4c8" 0 0 -.082 :rot (list (/ pi 2) 0 0) :emit "#ffe9b0" :energy 4 :shadow nil)
    (object-set l "light_color" (color 1.0 .93 .78 1.0))
    (object-set l "light_energy" (if chin 2.2 5.0))
    (object-set l "spot_range" (if chin 2.2 32.0))
    (object-set l "spot_angle" (if chin 55.0 24.0))
    (object-set l "spot_angle_attenuation" .6)
    (object-set l "shadow_enabled" t)
    (node-add-child g l)
    (node-3d-set-position l (v3 0 0 -.1))
    g))

(defun make-torch ()
  "Build the flashlight in our paw the way it is switched on now.
That is either a beam forward or upright under the muzzle."
  (when *torch-node* (node-queue-free *torch-node*) (setf *torch-node* nil))
  (case *torch*
    (:beam (setf *torch-node* (torch-model *camera* .2 -.17 -.36)))
    (:chin (setf *torch-node* (torch-model *camera* 0 -.3 -.16 :chin t)))))

(defun toggle-torch (&optional chin)
  "Toggle the flashlight: F for a beam forward, Shift+F (CHIN) for upright under the muzzle.
Pressing the same key again switches it off."
  (let ((want (if chin :chin :beam)))
    (setf *torch* (if (eq *torch* want) nil want))
    (make-torch)
    (say (case *torch* (:beam "Фонарик включён") (:chin "Фонарик под мордой. У-у-у!") (t "Фонарик выключен")))))

;;; ------------------------------------------------------------------ the wall: storage

(defun save-wall ()
  (unless (net-guest-p) (put-save :wall (list *pennants* *photos* *snapped* (and *swatter* t)))))

(defun load-wall ()
  (let ((form (get-save :wall)))
    (when (fits form '(integer list list &optional t))
      (setf *pennants* (first form)
            *photos* (remove-if-not (lambda (p) (and (listp p) (integerp (first p)) (stringp (second p)))) (second form))
            *snapped* (remove-if-not #'realp (third form))
            *swatter* (and (fourth form) t)))))

(defstate :wall
  :clear (lambda () (setf *pennants* 0 *photos* '() *snapped* '() *dice* (list (first *dice*) 1 1 "") *cook* nil *swatter* nil))
  :load #'load-wall :save #'save-wall
  :show (lambda () (when *uyut* (show-pennants) (show-photos) (show-swatter))))

;;; ------------------------------------------------------------------ pennants

(defparameter *pennant-rgb*
  '("#a3262c" "#2c4a7a" "#c9a13a" "#3f6f73" "#8a4038" "#55708a" "#3c5a48" "#a8843c" "#6a3a5a" "#2f5a3c" "#b0342c" "#4a4a7a"))

(defun show-pennants ()
  (loop for node in (un :pennants) for i from 0
        do (object-set node "visible" (logbitp i *pennants*))))

(defun award-pennant (town)
  "Award the pennant of TOWN, hung above the window, for the first parcel delivered there."
  (when (and (integerp town) (< -1 town 12) (not (logbitp town *pennants*)))
    (setf *pennants* (logior *pennants* (ash 1 town)))
    (show-pennants) (save-wall)
    (say (format nil "Вымпел города ~a — над окном. Собрано ~d из 12" (town-name town) (logcount *pennants*)))))

;;; ------------------------------------------------------------------ camera

(defvar *mark-names* '())

(defun photo-path (slot) (format nil "user://photo-~d.png" slot))

(defun photo-subject ()
  "Return what is in the shot as (caption landmark-path-coordinate).
It is the nearest landmark or diner, otherwise the town or the road."
  (let ((best nil))
    (loop for (i f side kind) in *landmark-plan*
          do (let ((d (gap-d i f)))
               (when (and (< (abs (- d *dist*)) 320) (or (null best) (< (abs (- d *dist*)) (abs (- (second best) *dist*)))))
                 (setf best (list (getf *mark-names* kind) d)))))
    (loop for (i f name) in *diner-plan*
          do (let ((d (gap-d i f)))
               (when (and (< (abs (- d *dist*)) 90) (or (null best) (< (abs (- d *dist*)) (abs (- (second best) *dist*)))))
                 (setf best (list (string-capitalize name) d)))))
    (when (> *aurora-k* .35) (setf best (list "Северное сияние" (+ 900000.0 (or (dalny-mid) 0)))))   ; a rare photo card
    (or best
        (let ((tw (town-at *dist*))) (list (if tw (town-title tw) "В дороге") nil)))))

(defun photo-texture (slot)
  (let ((path (photo-path slot)))
    (when (file-here-p path)
      (let ((img (gdext:make-object "Image")) (tx (gdext:make-object "ImageTexture")))
        (object-call img "load_png_from_buffer" (file-bytes path))
        (object-call tx "set_image" img)
        tx))))

(defun show-photos ()
  "Show the photo cards on the string: the newest are closer to the door."
  (loop for (card mat) in (un :cards) for i from 0
        do (let* ((p (nth i *photos*)) (tx (and p (photo-texture (first p)))))
             (object-set card "visible" (and tx t))
             (when tx (object-set mat "albedo_texture" tx)))))

(defun press-camera ()
  (cond ((plusp *photo-wait*) nil)
        (t (setf *photo-hud* *hud-hidden* *hud-hidden* t *photo-wait* 3 *toast* nil))))

(defun take-photo ()
  "Capture what is on the screen, shrink it and hang it on the wall."
  (let* ((image (object-call (object-call (node-get-viewport *root*) "get_texture") "get_image"))
         (used (mapcar #'first *photos*))
         (slot (or (loop for s below *photo-slots* unless (member s used) return s)
                   (first (car (last *photos*))))))
    (destructuring-bind (caption d) (photo-subject)
      (object-call image "resize" 320 180 1)
      (object-call image "save_png" (photo-path slot))
      (setf *photos* (cons (list slot caption)
                           (subseq (remove slot *photos* :key #'first) 0 (min (1- *photo-slots*) (length (remove slot *photos* :key #'first))))))
      (let ((new (and d (not (member d *snapped* :test (lambda (a b) (< (abs (- a b)) 1)))))))
        (when new (push d *snapped*) (note-quest :album))
        (show-photos) (save-wall)
        (say (if new
                 (format nil "Снято: ~a. Ориентиров в альбоме — ~d из ~d" caption (length *snapped*) (1+ (length *landmark-plan*)))
                 (format nil "Снято: ~a. Карточка — над ковром" caption)))))))

;;; ------------------------------------------------------------------ the tool in the paw
;;; The camera and the spyglass are taken off the nail (E) and carried along, both around the wagon and
;;; outside.
;;; The right mouse button, while held, raises the tool to the eyes; with the camera the left one then
;;; takes the shot.
;;; Every player has their own tool, like the flashlight: it is in the other paw.

(defvar *tool* nil "What is in the paw: NIL, :camera or :spyglass.")
(defvar *tool-node* nil "The tool in the paw: a node under the camera.")
(defvar *tool-up* nil "True while the tool is raised to the eyes (the right mouse button is held).")

(defun show-wall-tools ()
  "Show on the nails only the tools that are not in the paw."
  (when (un :camera-node) (object-set (un :camera-node) "visible" (not (eq *tool* :camera))))
  (show-spyglass))

(defun make-tool ()
  (when *tool-node* (node-queue-free *tool-node*) (setf *tool-node* nil))
  (case *tool*
    (:camera
     (let ((g (group *camera* -.21 -.16 -.36)))
       (shape g :box '(.15 .1 .06) "#2a2a2e" 0 0 0 :shadow nil)
       (shape g :cyl '(.036 .036 .05 10) "#16161a" 0 0 -.05 :rot (list (/ pi 2) 0 0) :shadow nil)
       (shape g :cyl '(.028 .028 .006 10) "#5a7a9a" 0 0 -.077 :rot (list (/ pi 2) 0 0) :metal .6 :rough .2 :shadow nil)
       (shape g :box '(.04 .025 .03) "#8a8e92" .04 .06 0 :metal .6 :rough .4 :shadow nil)
       (setf *tool-node* g)))
    (:spyglass
     (let ((g (group *camera* -.2 -.17 -.34)))
       (node-3d-set-rotation g (v3 .12 -.1 0))
       (shape g :cyl '(.022 .034 .3 8) "#c9a13a" 0 0 0 :rot (list (- (/ pi 2)) 0 0) :metal .7 :rough .3 :shadow nil)
       (shape g :cyl '(.038 .038 .04 8) "#2a2c30" 0 0 -.16 :rot (list (/ pi 2) 0 0) :shadow nil)
       (setf *tool-node* g)))))

(defun raise-tool (on)
  "Raise the tool to the eyes when ON is true, or lower it back.
The spyglass zooms in a lot, the camera a little; only the view stays in the frame."
  (setf *tool-up* (and on *tool* t)
        *zoom* (and *tool-up* (eq *tool* :spyglass)))
  (object-set *camera* "fov" (cond ((not *tool-up*) 72.0) ((eq *tool* :spyglass) 26.0) (t 50.0)))
  (when *tool-node* (object-set *tool-node* "visible" (not *tool-up*))))

(defun take-tool (which)
  "Handle E on the nail of the tool WHICH.
Take the tool; with that same tool in the paw, hang it back; with another one, swap them."
  (raise-tool nil)
  (setf *tool* (if (eq *tool* which) nil which))
  (make-tool) (show-wall-tools)
  (say (case *tool*
         (:camera "Фотоаппарат в лапе. Правая кнопка — к глазам, левая — снять")
         (:spyglass "Труба в лапе. Держи правую кнопку — смотреть")
         (t "Повесил на место"))))

(defun step-tool ()
  "Keep the tool raised while the right mouse button is held.
A window, building mode or the outside view lowers it."
  (let ((want (and *tool* (not *build*) (not *meta*) (not (outside-p)) (= (input-get-mouse-mode) 2)
                   (object-call (gdext:singleton "Input") "is_mouse_button_pressed" 2))))
    (unless (eq (and want t) *tool-up*) (raise-tool want))
    (when (and *tool-node* (not *tool-up*)) (object-set *tool-node* "visible" (not (outside-p))))))

;;; ------------------------------------------------------------------ radio and guitar

(defun set-radio (on)
  (setf *radio-on* (and on t) *radio-timer* 6.0)
  (let ((p (un :radio-player)))
    (when p (if on (object-call p "play") (object-call p "stop"))))
  (when *music* (object-set *music* "stream_paused" (and on t)))            ; one cabinet, one loudspeaker: the record waits while the radio talks
  (when (un :radio-dial) (object-set (un :radio-dial) "visible" *radio-on*)))

(defvar *radio-fillers* '()
  "The radio lines for when there is nothing to report; read in turn.")

(defun radio-line ()
  "Return a radio bulletin on what is ahead.
Each time it is about something different."
  (incf *radio-n*)
  (when (evenp *radio-n*)                                    ; every other time: about what we ourselves delivered (istorii.lisp)
    (let ((news (and (not (net-guest-p)) (story-news))))
      (when news (return-from radio-line (format nil "Радио: ~a" news)))))
  (let ((next (find-if (lambda (tw) (> (town-start tw) *dist*)) *towns*)))
    (case (mod *radio-n* 5)
      (0 (if next (format nil "Радио: до города ~a — ~,1f км" (town-title next) (/ (- (town-start next) *dist*) 1000)) "Радио: дальше городов нет"))
      (1 (let ((tw (find-if (lambda (tw) (and (> (town-end tw) *dist*) (plusp (parcels-for (town-index tw))))) *towns*)))
           (if tw (format nil "Радио: в городе ~a ждут посылку" (town-title tw)) "Радио: посылок в пути нет — берите новые на рынках")))
      (2 (format nil "Радио: за окном ~a, ~a" (getf *weather-names* *weather*)
                 (getf '(:spring "весна" :summer "лето" :autumn "осень" :winter "зима") *season*)))
      (3 (let ((s (next-station *dist*)))
           (if s (format nil "Радио: ближайшая заправка — через ~,1f км" (/ (- s *dist*) 1000)) "Радио: заправок впереди нет")))
      (t (and *radio-fillers* (nth (mod (floor *radio-n* 5) (length *radio-fillers*)) *radio-fillers*))))))

(defun toggle-guitar ()
  (setf *guitar-on* (not *guitar-on*))
  (when (un :guitar) (object-set (un :guitar) "visible" (not *guitar-on*)))
  (let ((p (un :guitar-player)))
    (when p (if *guitar-on* (object-call p "play") (object-call p "stop"))))
  (say (if *guitar-on* "Играешь на гитаре. На остановке за живую музыку кидают вдвое чаще" "Повесил гитару")))

;;; ------------------------------------------------------------------ dice

(defun show-dice ()
  (let ((label (un :dice-label)))
    (when label
      (object-set label "text" (if (plusp (first *dice*)) (format nil "~d + ~d" (second *dice*) (third *dice*)) "")))))

(defun roll-dice ()
  (setf *dice* (list (1+ (first *dice*)) (1+ (random 6)) (1+ (random 6)) *net-name*))
  (show-dice)
  (say (format nil "Кости: ~d и ~d" (second *dice*) (third *dice*))))

;;; ------------------------------------------------------------------ pancakes

(defvar *lid* nil "True once the lid for the frying pan is bought (hardware store, Krasny Kut): under it things fry twice as fast.")

(defun stove-surface ()
  "Return where things are put to cook, as (x z width depth height): the top of the stove."
  (destructuring-bind (sx sz) *stove-at*
    (list sx sz .6 .5 *stove-top*)))

(defun stove-recipe-p (kind)
  "Return true if KIND takes part in a recipe cooked on the stove: as the raw good or as what goes with it."
  (some (lambda (r) (destructuring-bind (raw dish name &optional with hearth) r
                      (declare (ignore dish name))
                      (and (null hearth) (or (eq kind raw) (eq kind with)))))
        *recipes*))

(defun single-dish (kind)
  "Return the dish that KIND alone turns into on the stove, or NIL."
  (second (find-if (lambda (r) (and (eq (first r) kind) (null (fourth r)) (null (fifth r)))) *recipes*)))

(defun raw-on-stove ()
  "Return the records of the items that stand on the stove and can be cooked.
Without the frying pan on the stove (kettle.lisp) that is only milk. A toadstool counts: it ruins the lot."
  (destructuring-bind (cx cz w d top) (stove-surface)
    (remove-if-not (lambda (spec) (and (or (stove-recipe-p (first spec)) (eq (first spec) :toadstool)) (null (sixth spec))
                                       (or *pan-on* (eq (first spec) :milk))
                                       (< (abs (- (second spec) cx)) (+ (/ w 2) .03)) (< (abs (- (fourth spec) cz)) (+ (/ d 2) .03))
                                       (< (abs (- (third spec) top)) .08)))
                   (mapcar #'car *items*))))

(defun stove-jobs (&optional going)
  "Return what the stove cooks right now: entries of (dish item-record ...).
GOING are the jobs already cooking: those whose things all still stand on the stove go on as they are, so
putting one more thing down does not start anybody's countdown anew. Of the rest, recipes of two
ingredients come first and take a pair each; whatever is left cooks alone. A toadstool among them makes
one job, :RUINED, of everything on the stove."
  (let ((raws (raw-on-stove)) (jobs '()))
    (when (find :toadstool raws :key #'first) (return-from stove-jobs (list (cons :ruined raws))))
    (dolist (job going)
      (when (and (not (eq (first job) :ruined)) (every (lambda (spec) (member spec raws :test #'equal)) (rest job)))
        (push job jobs)
        (setf raws (remove-if (lambda (spec) (member spec (rest job) :test #'equal)) raws))))
    (dolist (r *recipes*)
      (destructuring-bind (raw dish name &optional with hearth) r
        (declare (ignore name))
        (when (and with (null hearth))
          (loop (let ((a (find raw raws :key #'first)) (b (find with raws :key #'first)))
                  (unless (and a b) (return))
                  (push (list dish a b) jobs)
                  (setf raws (remove a (remove b raws))))))))
    (dolist (raw raws (nreverse jobs))
      (let ((dish (single-dish (first raw)))) (when dish (push (list dish raw) jobs))))))

(defun cook-time (job)
  "Return the number of seconds the JOB takes: less under the lid, less again when everything was cut on the board."
  (* *cook-time* (if *lid* .5 1)
     (if (and (some (lambda (spec) (cuttable-p (first spec))) (rest job))              ; whatever can be cut was cut
              (every (lambda (spec) (or (spec-cut-p spec) (not (cuttable-p (first spec))))) (rest job)))
         *cut-factor* 1)))

(defun step-cook (delta)
  "Advance the cooking by DELTA seconds; the driver computes it.
What stands on the hot stove turns into dishes (STOVE-JOBS); each job has its own countdown."
  (let ((jobs (stove-jobs (mapcar #'car *cook*))))
    (setf *cook* (remove-if-not (lambda (c) (member (car c) jobs :test #'equal)) *cook*))
    (dolist (job jobs)
      (unless (assoc job *cook* :test #'equal)
        (push (cons job (cook-time job)) *cook*)
        (say-all (cond ((<= *fire* 0) "Печка холодная — подбрось доску, и будет готовиться")
                       ((eq (first job) :ruined) "Жарим… Пахнет как-то странно")
                       (t (format nil "Готовим ~a" (good-name (first job))))))))
    (when (> *fire* 0)
      (dolist (c (copy-list *cook*))
        (when (<= (decf (cdr c) delta) 0)
          (destructuring-bind (dish first-raw &rest others) (car c)
            (let ((left (remove-if (lambda (spec) (member spec others :test #'equal)) (mapcar #'car *items*))))
              (set-items (if (eq dish :ruined)
                             (remove first-raw left :test #'equal)
                             ;; a dish from a cut good stays cut; the shop's mark does not pass to the dish
                             (substitute (make-spec dish (second first-raw) (third first-raw) (fourth first-raw) 0 nil (and (spec-cut-p first-raw) '(:cut t)))
                                         first-raw left :test #'equal))))
            (setf *cook* (remove c *cook*))
            (save-interior)
            (log-event :cooked :dish dish :dist (round *dist*))
            (say-all (if (eq dish :ruined)
                         "Мухомор на сковородке! Всё пришлось выбросить. Красный в белую точку — не брать"
                         (format nil "Готово: ~a. Там, где ищут сырьё, за блюдо дадут вдвое больше"
                                 (second (assoc dish *kinds*)))))))))))

(defun good-price (kind price)
  "Return how much a buyer pays for the good KIND when paying PRICE for the raw good.
A dish costs more."
  (let* ((recipe (dish-p kind))
         (price (cond ((null recipe) price)
                      ;; a dish of two goods: the second one is paid for as well, a little above what it costs raw
                      ((fourth recipe) (+ (* *blin-factor* price) *blin-bonus* (round (* *second-share* (good-base (fourth recipe))))))
                      (t (+ (* *blin-factor* price) *blin-bonus*)))))
    (if (and (eq *season* :winter) (preserved-p kind)) (round (* *winter-bonus* price)) price)))  ; what was put by for the winter

(defun good-p (kind) (or (and (dish-p kind) t) (and (assoc kind *goods*) t)))

;;; ------------------------------------------------------------------ mosquito and fly swatter

(defun show-swatter () (when (un :swatter) (object-set (un :swatter) "visible" (and *swatter* t))))

(defshared buy-swatter :swatter ()
  (cond ((< *money* *swatter-price*) (say "На мухобойку не хватает денег"))
        (t (decf *money* *swatter-price*) (setf *swatter* t)
           (show-swatter) (save-wall)
           (log-event :swatter :money *money*)
           (say-all "Купили мухобойку — висит у окна. Комар залетит — E по ней"))))

(defun end-mosquito (text)
  (setf *mosquito* nil)
  (when (un :mosquito) (object-set (un :mosquito) "visible" nil))
  (when (un :mosquito-player) (object-call (un :mosquito-player) "stop"))
  (when text (say text)))

(defvar *swat-seq* 0 "The counter of fly swatter hits: it grows for whoever hit and spreads over the network.")

(defun swat ()
  (cond (*mosquito* (incf *swat-seq*) (end-mosquito "Хлоп! Тишина"))
        (t (say "Мухобойка. Комаров пока нет"))))

(defparameter *mosquito-on* nil
  "Whether the mosquito of the swamp is in the game. It is switched off until its mechanic is made better;
the code, the fly swatter and the sounds stay, so switching it back on is this one flag.")

(defun start-mosquito ()
  (unless *mosquito-on* (return-from start-mosquito nil))
  (setf *mosquito* 1.0 *swamp-time* 0.0)
  (when (un :mosquito) (object-set (un :mosquito) "visible" t))
  (when (un :mosquito-player) (object-call (un :mosquito-player) "play"))
  (say (if *swatter* "В кибитку залетел комар. Мухобойка у окна — E по ней"
           "В кибитку залетел комар. Мухобойки продают на заправке у болота")))

(defun step-mosquito (delta)
  "Advance the mosquito by DELTA seconds.
In the swamp a mosquito flies into the wagon: it whines, circles around the head and bites until swatted."
  (unless *mosquito-on*
    (when *mosquito* (end-mosquito nil))
    (return-from step-mosquito nil))
  (let ((swamp (and (in-swamp-p *dist*) (not *meta*))))
    (setf *swamp-time* (if swamp (+ *swamp-time* delta) 0.0))
    (cond (*mosquito*
           (let* ((was *mosquito*) (now (incf *mosquito* delta)) (tt *time*))
             (when (un :mosquito)
               (node-3d-set-position (un :mosquito)
                                     (v3 (+ (vector3-x *pos*) (* .45 (cos (* tt 2.3))) (* .1 (sin (* tt 9))))
                                         (+ *floor* *eye* .05 (* .18 (sin (* tt 3.1))))
                                         (+ (vector3-z *pos*) (* .45 (sin (* tt 1.7))) (* .1 (cos (* tt 7)))))))
             (when (/= (floor was 24) (floor now 24)) (say "Комар укусил! Чешется"))
             (when (and (not swamp) (> now 150) (not (net-guest-p))) (end-mosquito "Комар наконец улетел"))))
          ((and (> *swamp-time* 14.0) (not (net-guest-p)))                 ; the driver 'sends' the mosquito to a guest
           (start-mosquito)))))

;;; ------------------------------------------------------------------ sleep

(defun step-sleep (delta)
  (let ((abed (and *seat* (member (first *seat*) '(:fold :top)) (not (net-guest-p)))) (day (daylight *day-t*)))
    (cond (*sleep*
           (when (or (not abed) (> day .7) (not (all-abed-p)))
             (setf *time-scale* (first *sleep*) *sleep* nil *doze* 0.0)
             (say (if abed "Утро. Выспались" "Проснулся"))))
          ((and abed (< day .12))
           (when (> (incf *doze* delta) 5.0)
             (cond ((all-abed-p)                                           ; the night is fast-forwarded only when everyone has lain down
                    (setf *sleep* (list *time-scale*) *time-scale* (* 40 *time-scale*))
                    (say "Засыпаешь под стук колёс… E — встать"))
                   (t (setf *doze* -20.0)
                      (say "Не все легли: пока кто-то на ногах, ночь не промотать")))))
          (t (setf *doze* 0.0)))))

;;; ------------------------------------------------------------------ nodes

(defun make-uyut (c)
  (let ((f *floor*) (cards '()) (pennants '()))
    (dolist (key '(:radio-player :guitar-player)) (when (un key) (node-queue-free (un key))))   ; the old ones are from the previous wagon
    (setf *uyut* nil *guitar-on* nil *sleep* nil *photo-wait* 0 *mosquito* nil *swamp-time* 0.0)
    ;; the string with photo cards above the rug (left wall)
    (shape c :box '(.008 .008 5.2) "#3a2a1e" (+ (- *half-width*) .1) (+ f 2.1) 0 :shadow nil)
    (dotimes (i *photo-slots*)
      (let ((mi (gdext:make-object "MeshInstance3D")) (mesh (gdext:make-object "QuadMesh"))
            (mat (gdext:make-object "StandardMaterial3D")) (z (- 2.3 (* i .42))))
        (object-set mesh "size" (vector2 .3 .17))
        (object-set mat "shading_mode" 0)
        (object-set mi "mesh" mesh)
        (object-set mi "material_override" mat)
        (object-set mi "cast_shadow" 0)
        (node-add-child c mi)
        (node-3d-set-position mi (v3 (+ (- *half-width*) .105) (+ f 2.0) z))
        (node-3d-set-rotation mi (v3 0 (/ pi 2) 0))
        (shape mi :box '(.33 .2 .004) "#f0ece0" 0 0 -.004 :shadow nil)
        (shape mi :box '(.02 .04 .012) "#8a6440" 0 .1 0 :shadow nil)
        (push (list mi mat) cards)))
    ;; pennants above the window (right wall)
    (dotimes (i 12)
      (let ((g (group c (- *half-width* .1) (+ f 2.13) (- (* i .21) 1.155))))
        (shape g :box '(.008 .1 .13) (nth i *pennant-rgb*) 0 .02 0 :shadow nil)
        (shape g :box '(.008 .09 .09) (nth i *pennant-rgb*) 0 -.03 0 :rot (list (/ pi 4) 0 0) :shadow nil)
        (sign g (subseq (town-name i) 0 1) -.01 .02 0 :h .07 :rgb "#f0ece0")
        (push g pennants)))
    ;; the camera on a nail
    (destructuring-bind (x y z) *camera-at*
      (let ((g (group c x (+ f y) z)))
        (shape c :box '(.012 .012 .05) "#8a8e92" x (+ f y .27) z :shadow nil)          ; the nail: it stays
        (setf (getf *uyut* :camera-node) g)
        (shape g :box '(.07 .11 .17) "#2a2a2e" 0 0 0)
        (shape g :cyl '(.04 .04 .05 10) "#16161a" -.05 0 0 :rot (list 0 0 (/ pi 2)))
        (shape g :box '(.03 .03 .05) "#8a8e92" 0 .07 .04 :metal .6 :rough .4)
        (shape g :box '(.01 .22 .01) "#5a3e26" .02 .16 0 :shadow nil)))
    ;; the radio is the front of the radiogram (jukebox.lisp): here are only its lit dial and its knob
    (destructuring-bind (x y z) *radio-at*
      (let ((g (group c x (+ f y) z)))
        (shape g :cyl '(.025 .025 .02 8) "#d8d4c4" -.005 -.06 .1 :rot (list 0 0 (/ pi 2)) :shadow nil)
        (shape g :box '(.01 .05 .2) "#2a2a2e" 0 .0 0 :shadow nil)
        (setf (getf *uyut* :radio-dial)
              (shape g :box '(.012 .035 .18) "#ffd890" -.004 0 0 :emit "#ffc060" :energy 2.0 :shadow nil))))
    ;; the guitar on the wall by the door
    (destructuring-bind (x y z) *guitar-at*
      (let ((g (group c x (+ f y) z)))
        (shape g :cyl '(.19 .19 .08 14) "#b8763a" 0 -.22 0 :rot (list (/ pi 2) 0 0))
        (shape g :cyl '(.14 .14 .08 14) "#b8763a" 0 .04 0 :rot (list (/ pi 2) 0 0))
        (shape g :cyl '(.05 .05 .09 10) "#2a1a12" 0 -.14 0 :rot (list (/ pi 2) 0 0) :shadow nil)
        (shape g :box '(.05 .6 .03) "#5a3e26" 0 .44 -.01)
        (shape g :box '(.07 .12 .03) "#3a2a1e" 0 .78 -.01)
        (setf (getf *uyut* :guitar) g)))
    ;; the fly swatter by the window and the mosquito
    (destructuring-bind (x y z) *swatter-at*
      (let ((g (group c x (+ f y) z)))
        (shape g :box '(.012 .34 .014) "#5a3e26" 0 -.12 0 :shadow nil)
        (shape g :box '(.012 .16 .13) "#a3262c" 0 .13 0 :shadow nil)
        (setf (getf *uyut* :swatter) g)))
    (let ((m (shape c :ball '(.018 5 3) "#16161a" 0 (+ f 1.4) 0 :shadow nil)))
      (object-set m "visible" nil)
      (setf (getf *uyut* :mosquito) m
            (getf *uyut* :mosquito-player) (player m (wav-stream (sound-path "mosquito") :loop t) :positional t :db -6.0 :bus "Кибитка" :play nil)))
    ;; dice on the tea table
    (let ((g (group c (+ (first *table-at*) .2) (+ f *table-top*) (- (second *table-at*) .3))))
      (shape g :cyl '(.045 .035 .09 10) "#5a3e26" 0 .045 0)
      (shape g :box '(.035 .035 .035) "#f0ece0" .02 .0175 .12 :rot '(0 .4 0))
      (shape g :box '(.035 .035 .035) "#f0ece0" -.03 .0175 .17 :rot '(0 1.1 0))
      (let ((l (gdext:make-object "Label3D")))
        (object-set l "text" "")
        (object-set l "pixel_size" .002)
        (object-set l "font_size" 48)
        (object-set l "billboard" 1)
        (object-set l "modulate" (hex "#ffe9b0"))
        (node-add-child g l)
        (node-3d-set-position l (v3 0 .22 .14))
        (setf (getf *uyut* :dice-label) l))
      (setf (getf *uyut* :dice) g))
    (setf (getf *uyut* :cards) (nreverse cards) (getf *uyut* :pennants) (nreverse pennants)
          (getf *uyut* :radio-player) (player *root* (wav-stream (sound-path "radio") :loop t) :db -20.0 :bus "Музыка" :play nil)
          (getf *uyut* :guitar-player) (player *root* (wav-stream (sound-path "guitar") :loop t) :db -9.0 :bus "Музыка" :play nil))
    (unless (net-guest-p) (load-wall))
    (show-pennants) (show-photos) (show-dice) (show-swatter) (set-radio *radio-on*) (show-wall-tools)))

(defun uyut-usables ()
  (append
   (list (usable (first *camera-at*) (second *camera-at*) (third *camera-at*)
                 (lambda () (if (eq *tool* :camera) "E — повесить фотоаппарат"
                                (format nil "E — взять фотоаппарат · карточек ~d, ориентиров ~d из ~d"
                                        (length *photos*) (length *snapped*) (1+ (length *landmark-plan*)))))
                 (lambda () (take-tool :camera)))
         (usable (first *radio-at*) (second *radio-at*) (third *radio-at*)
                 (lambda () (if *radio-on* "E — выключить радио" "E — включить радио"))
                 (lambda () (set-radio (not *radio-on*))))
         (usable (first *guitar-at*) (second *guitar-at*) (third *guitar-at*)
                 (lambda () (if *guitar-on* "E — повесить гитару" "E — играть на гитаре")) #'toggle-guitar))
   (when *swatter*
     (list (usable (first *swatter-at*) (second *swatter-at*) (third *swatter-at*)
                   (lambda () (if *mosquito* "E — прихлопнуть комара" "Мухобойка: комаров пока нет")) #'swat)))
   (when (and *mosquito-on* *at-station* (in-swamp-p *dist*) (not *swatter*))
     (flet ((label () (format nil "E — купить мухобойку за ~/kiosk-trip::rub/" *swatter-price*)))
       (list (usable (+ *half-width* .02) 1.55 .75 #'label #'buy-swatter)        ; from the wagon window
             ;; and where the 'FLY SWATTERS' sign is at the gas station: get out, walk up, buy
             (road-usable 7.6 (- (first *extra-stations*) 9.0) 1.8 #'label #'buy-swatter 3.2))))
   (when (table-shown-p)
     (list (usable (+ (first *table-at*) .2) (+ *table-top* .06) (- (second *table-at*) .3)
                   (lambda () "E — бросить кости") #'roll-dice)))))

(defun step-uyut (delta)
  (step-tool)
  (when (un :dice) (object-set (un :dice) "visible" (table-shown-p)))
  (when (plusp *photo-wait*)                                   ; the HUD is hidden: wait for a frame without it and take the shot
    (when (zerop (decf *photo-wait*))
      (handler-case (take-photo) (error (e) (note-error e "фото")))
      (setf *hud-hidden* *photo-hud*)))
  (when (and *radio-on* (not *meta*) (<= (decf *radio-timer* delta) 0))
    (setf *radio-timer* 48.0)
    (unless (and *toast* (> (cdr *toast*) 0)) (say (radio-line))))
  (step-sleep delta)
  (step-mosquito delta)
  (unless (net-guest-p) (step-cook delta)))

;;; ------------------------------------------------------------------ shared over the network

(defcommon :pennants :value *pennants* :spec 'integer :driver t          ; the driver counts the pennants
  :accept (lambda (mask) (unless (= mask *pennants*) (setf *pennants* mask) (show-pennants))))

(defcommon :radio :value (and *radio-on* t) :spec '(:member t nil)
  :accept (lambda (on) (unless (eq on (and *radio-on* t)) (set-radio on))))

(defcommon :dice :value (copy-list *dice*) :spec '(integer integer integer string)
  :accept (lambda (roll)
            (destructuring-bind (seq a b name) roll
              (when (and (> seq (first *dice*)) (<= 1 a 6) (<= 1 b 6))
                (setf *dice* (list seq a b (subseq name 0 (min 24 (length name)))))
                (show-dice)
                (say (format nil "~a бросает кости: ~d и ~d" (fourth *dice*) a b))))))

(defcommon :swatter :value (and *swatter* t) :spec '(:member t nil) :driver t
  :accept (lambda (bought) (setf *swatter* bought) (show-swatter)))

(defcommon :swat :value *swat-seq* :spec 'integer                        ; someone swatted the mosquito, and it is shared
  :accept (lambda (seq)
            (when (> seq *swat-seq*)
              (setf *swat-seq* seq)
              (when *mosquito* (end-mosquito "Хлоп! Кто-то прихлопнул комара")))))
