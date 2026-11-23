;;;; Town gimmicks: every town has one thing to do that exists nowhere else. By the road stands a booth
;;;; with a sign (*GIMMICK-PLAN*); stop next to it and the ladder comes down, walk up to the window and press E.
;;;; The collection point in Krasny Kut and the airmail in Dalny have their own files (finds.lisp, dalny.lisp);
;;;; the factory whistle of Krasny Kut is in goods.lisp (RUSH-P) and here (customers come up by themselves).
;;;; The driver keeps count of money and things: to a guest the booth answers that the driver does this.

(in-package #:kiosk-trip)

(defvar *gimmick-plan* '()
  "The booths, each a list (number, fraction of the town length, sign, colour [, town number]).
The town number is given only when it differs from the booth number.")

(defvar *gimmicks* '() "The places where the booths stand, each a list (path-coordinate town-number).")
(defvar *stamp* nil "The town council stamp: with it parcels pay double.")
(defvar *badge* nil "The 'Sharpshooter' badge.")
(defvar *boost-until* 0.0 "The time until which the mineral water keeps us brisk.")
(defstruct job "An activity that involves waiting (fishing, the mine)." until done actor carry)
(defvar *wait* nil "The activity going on right now, as a JOB.
It holds when it ends, what to do then, who is busy (a guest, or NIL for us) and what they carry
in their paws.")
(defvar *rail-ticket* nil "True when the wagon stands on the flatcar: the next town picked on the map is reached by train.")
(defvar *rush-told* nil)
(defvar *rush-bell* 0.0)

(defun save-prikol () (unless (net-guest-p) (put-save :prikol (list (and *stamp* t) (and *badge* t) *gimmick-spent* (and *lid* t)))))
(defun load-prikol ()
  (let ((f (get-save :prikol)))
    (when (fits f '(t t &optional t t))
      (setf *stamp* (and (first f) t) *badge* (and (second f) t) *lid* (and (fourth f) t)
            *gimmick-spent* (and (fits (third f) '(:each cons)) (third f))))))
(defstate :prikol
  :clear (lambda () (setf *stamp* nil *badge* nil *lid* nil *boost-until* 0.0 *gimmick-spent* '() *wait* nil *rail-ticket* nil))
  :load #'load-prikol :save #'save-prikol :show (lambda () (show-lid)))

(defun boost-factor () (if (< *time* *boost-until*) *water-boost* 1.0))

(defun give-good (kind text)
  "Put a good of KIND into the hands (if they are free) and say TEXT.
Return true if the good was given."
  (cond (*carry* (say "Руки заняты — сначала поставь то, что несёшь, на полку") nil)
        (t (setf *carry* (cons kind nil) *carry-from* nil *carry-props* '())
           (show-carry) (save-interior)
           (say text) t)))

(defun start-wait (seconds text done)
  (if *wait* (say "Погоди, ещё не закончил")
      (progn (setf *wait* (make-job :until (+ *time* seconds) :done done :actor *actor* :carry *carry*) *pending* t)
             (say text))))

;;; ------------------------------------------------------------------ what each booth does

;;; A booth is declared once with DEFBOOTH: what its window says, what E does, what stands next to it and
;;; where else besides the window it answers to E. Where the booths stand and what is written on their
;;; signs is content: the mod's *GIMMICK-PLAN*. Booths that belong to another subsystem are declared in its
;;; file (the town council and the militia in bumagi.lisp, the savings bank in debt.lisp).

(defvar *booths* '() "The booths by number: entries of (number . plist of :label :use :build :spot :daily :shelf).")

(defmacro defbooth (number &key label use build spot daily shelf side)
  "Declare what the booth NUMBER does.
LABEL is the hint at its window: a string or a function returning one. USE is a function called on E.
BUILD is a function of the booth group that adds its landmark. SPOT is (sideways along-the-road height):
where else, besides the window, it answers to E. With DAILY its free thing is given once a game day.
SHELF is a list of colours: what stands on the shelves behind the seller. SIDE is (label-function
action-function) for a booth whose SPOT is a side door with business of its own."
  `(progn (setf *booths* (cons (list ,number :label ,label :use ,use :build ,build :spot ,spot :daily ,daily :shelf ,shelf :side ,side)
                               (remove ,number *booths* :key #'first)))
          ,number))

(defun booth (i key) (getf (rest (assoc i *booths*)) key))

(defun gimmick-label (i)
  "Return the hint at the window of booth I."
  (let ((label (booth i :label)))
    (cond ((functionp label) (funcall label)) (label) (t "Будка"))))

(defvar *gimmick-spent* '() "The record of free takings, each a pair (booth-number . day): when the free thing was last taken.")

(defun today () "Return the number of the current game day." (floor *day-t* *day-len*))

(defun spent-p (i)
  "Return true, with a hint, if the free thing at booth I has already been taken today.
The goat, the fishing and the mine each give theirs once per day cycle."
  (when (and (not *dev*) (eql (cdr (assoc i *gimmick-spent*)) (today)))            ; the dev polygon has no limits
    (say "Здесь на сегодня всё. Приходи завтра")
    t))

(defun spend (i) (setf *gimmick-spent* (cons (cons i (today)) (remove i *gimmick-spent* :key #'car))) (save-prikol))

(defun rail-fare (tw)
  "Return the price of carrying the wagon by train to town TW, by the length of the way."
  (max *rail-min* (round (* *rail-per-km* (/ (plan-way (town-stop-d tw)) 1000)))))

(defun booth-at-hand-p (i)
  "Return true if booth I stands within walking distance of the wagon and the wagon is standing."
  (and (< *speed* .4) (some (lambda (g) (and (eql (second g) i) (< (abs (- (first g) *dist*)) 80.0))) *gimmicks*) t))

(defshared gimmick-use :gimmick (i)
  "Do what E does at booth I."
  (let ((use (booth i :use)))
    (cond ((not (booth-at-hand-p i)) (say "Будка далеко"))                  ; a guest's message may name any booth: the driver checks
          ((and use (not (and (booth i :daily) (spent-p i))))
           (funcall use)
           (let ((line (person-speaks (list :booth i) (trade-town)))) (when line (say-later line 4.0)))))))   ; whoever sits in this booth has a word

(defun rail-ride (tw)
  "Travel by train to town TW: instantly, for money.
Return T if a train ride was pending (GO-TO-TOWN then does nothing itself)."
  (when *rail-ticket*
    (setf *rail-ticket* nil)
    (when (pay (rail-fare tw))
      (log-event :rail :town (town-index tw) :money *money*)
      (relocate (- (town-stop-d tw) 40.0))
      (say (format nil "Поезд довёз до города ~a. Съезжаем с платформы" (town-title tw))))
    t))

;;; ------------------------------------------------------------------ booths

(defvar *tir-plates* '()
  "The plates of the shooting range, as built with its booth: entries of (node x y z swing speed alive).")
(defvar *pond* nil "The water of the fishing booth: the node of its surface, as built with the booth.")

(defun build-range (g)
  "Build the shooting alley beside booth group G: a firing counter, a backboard and five plates."
  (model g "booth-range")
  (setf *tir-plates*
        (loop for i below 5
              collect (let ((x (+ 5.3 (* 1.1 i))) (y (if (evenp i) 1.35 2.05)))
                        (list (shape g :cyl '(.3 .3 .05 14) (if (evenp i) "#e8e0d0" "#e0b040") x y -13.2 :rot (list (/ pi 2) 0 0) :shadow nil)
                              x y -13.2 .45 (+ .9 (* .35 i)) t)))))

(defvar *goat* nil "Manka the goat by her booth: the node, as built with the booth.")
(defvar *goat-milk* nil "The milk in Manka's pail: a node that grows as the pail fills.")
(defvar *rock* '() "The rock face of the mine, as built with its booth: entries of (node x y coal broken).")

(defun build-rock (g)
  "Build the rock face beside booth group G: four rows of eight blocks against a dark backing."
  (model g "booth-rock")
  (setf *rock* (loop for i below 32
                     collect (list (shape g :box '(.5 .5 .5) "#5a554e" (+ 5.58 (* .55 (mod i 8))) (+ .3 (* .55 (floor i 8))) 6.85)
                                   (mod i 8) (floor i 8) nil nil))))

(defun build-pond (g)
  "Build the fishing pond beside booth group G: the water, a short pier and reeds."
  (setf *pond* (shape g :cyl '(4.0 4.0 .05 24) "#3f7f9a" 9.0 .07 -9.5 :shadow nil :rough .15 :metal .3))
  (model g "booth-pond"))

;;; ------------------------------------------------------------------ the booths themselves

;; the pier: fishing
(defbooth 0
  :shelf '("#3f6f73" "#8aa0b0" "#c9a13a")
  :daily t
  :spot '(12.2 5.6 1.2)
  :label "E — порыбачить с мостков"
  :use (lambda () (if *carry* (say "Руки заняты — удочку не взять") (begin-play :fish)))
  :build (lambda (g) (unless (member "nopond" *dev-flags* :test #'string=) (build-pond g)) (model g "booth-0-extra")))

;; the shooting range
(defbooth 1
  :shelf '("#e8e0d0" "#a3262c" "#3c4638")
  :spot '(10.7 4.2 1.3)
  :label (lambda () (format nil "E — тир: пять выстрелов за ~/kiosk-trip::rub/ Целиться самому" *range-price*))
  :use (lambda () (when (pay *range-price*) (begin-play :tir)))
  :build (lambda (g) (unless (member "nopond" *dev-flags* :test #'string=) (build-range g)) (model g "booth-1-extra")))

;; the hardware shop: the lid for the frying pan
(defbooth 2
  :shelf '("#1e1e22" "#8a8e92" "#a3262c")
  :label (lambda () (if *lid* (format nil "E — ~d банки с крышками за ~/kiosk-trip::rub/ · у вас пустых: ~d" (first *jar-deal*) (second *jar-deal*) *jars*)
           (format nil "E — купить чугунную крышку для сковородки за ~/kiosk-trip::rub/: под ней жарится вдвое быстрее" *lid-price*)))
  :use (lambda () (cond (*lid* (when (pay (second *jar-deal*)) (incf *jars* (first *jar-deal*)) (save-home) (show-home)
                                        (say (format nil "Купили банки. Пустых теперь ~d: закатывают у ящика под умывальником" *jars*))))
             ((pay *lid-price*) (setf *lid* t) (save-prikol) (show-lid) (log-event :lid :money *money*)
              (say-all "Купили крышку! Висит над сковородкой: теперь на сковородке жарится вдвое быстрее")))))

;; the library: waste paper for books
(defbooth 3
  :shelf '("#6a3a2e" "#2c4a7a" "#3c5a48" "#c9a13a")
  :label (lambda () (format nil "E — обменять ~d связки макулатуры на книги · у тебя ~d" *book-papers* *papers*))
  :use (lambda () (cond ((and (not *dev*) (< *papers* *book-papers*)) (say (format nil "Нужно ~d связки макулатуры — она лежит у остановок и заправок" *book-papers*)))
             (*carry* (say "Руки заняты — книги не взять"))
             (t (decf *papers* *book-papers*) (save-finds)
                (give-good :books (nth (random 4) '("Выдали стопку книг: «Два капитана» сверху" "Выдали книги: фантастика, зачитанная до дыр"
                                                    "Выдали книги: том энциклопедии и сказки" "Выдали книги: стихи и справочник грибника"))))))
  :build (lambda (g) (model g "booth-3-extra")))

;; Manka the goat
(defbooth 4
  :shelf '("#f0ede4" "#3a6aa8" "#d8a030")
  :daily t
  :spot '(10.2 -3.0 .8)
  :label "E — подоить козу Маньку"
  :use (lambda () (if *carry* (say "Руки заняты — ведро не взять") (begin-play :goat)))
  :build (lambda (g) (let ((s (group g 7.0 0 3.0)))                                        ; Manka the goat and her pail (activities.lisp)
           (node-3d-set-scale s (v3 .55 .55 .55)) (mark-cow s 0 0 1.2 "#e8e4d8")
           (setf *goat* s))
         (model g "booth-4-extra")
         (setf *goat-milk* (shape g :cyl '(.17 .17 .02 10) "#f6f4ee" 7.0 .06 2.2 :shadow nil))
         ))

;; the lighthouse: prices in the two nearest towns
(defbooth 5
  :shelf '("#d8d0b8" "#2c4a7a" "#a3262c")
  :spot '(10.2 3.2 2.0)
  :label "E — подняться на маяк: что почём в ближайших городах"
  :use (lambda () (let* ((here (trade-town))                                ; the two nearest towns by road, in either direction
              (near (sort (loop for tw in *towns* unless (= (town-index tw) here)
                                collect (cons (or (plan-way (town-stop-d tw)) 1e9) (town-index tw)))
                          #'< :key #'car)))
         (say (format nil "С маяка видно далеко. ~{~a~^ · ~}"
                      (loop for (nil . i) in (subseq near 0 2) collect (market-note i))))))
  :build (lambda (g) (dotimes (k 6) (shape g :cyl '(.8 .9 1.0 12) (if (evenp k) "#e8e4d8" "#a3262c") 7.0 (+ .5 k) -3.2))   ; a small lighthouse
         (model g "booth-5-extra")))

;; window no. 3: the stamp
(defbooth 6
  :shelf '("#e8e0d0" "#8a8070" "#6a3a2e")
  :label (lambda () (if *stamp* "Печать уже стоит: за посылки платят вдвое" (format nil "E — получить печать за ~/kiosk-trip::rub/: посылки вдвое дороже" *stamp-price*)))
  :use (lambda () (cond (*stamp* (say "Печать уже стоит"))
             ((pay *stamp-price*) (setf *stamp* t) (save-prikol) (log-event :stamp)
              (say "Справка с круглой печатью! Теперь за посылки платят вдвое")))))

;; the mine
(defbooth 8
  :shelf '("#1a1a1e" "#5a4636" "#c9a13a")
  :daily t
  :spot '(10.7 -5.6 1.2)
  :label "E — спуститься в шахту за углём"
  :use (lambda () (if *carry* (say "Руки заняты — уголь не унести") (begin-play :mine)))
  :build (lambda (g) (build-rock g) (model g "booth-8-extra")))

;; mineral water
(defbooth 9
  :shelf '("#7ab0c8" "#e8e0d0" "#3f6f73")
  :spot '(10.4 2.6 1.0)
  :label (lambda () (format nil "E — стакан минеральной воды за ~/kiosk-trip::rub/: бодрит" *water-price*))
  :use (lambda () (when (pay *water-price*) (setf *boost-until* (+ *time* *water-time*)) (say "Нарзан! В нос шибает. Пять минут бегаешь быстрее")))
  :build (lambda (g) (model g "booth-9-extra")))

;; the flatcar: the wagon goes by train
(defbooth 10
  :shelf '("#7a5a50" "#d8d0b8" "#2a2c30")
  :label (lambda () (format nil "E — погрузить кибитку на платформу: до любого города, ~/kiosk-trip::rub/ за километр" *rail-per-km*))
  :use (lambda () (cond (*actor* (say "На платформу кибитку ставит водитель: маршрут — его дело"))
              (t (setf *rail-ticket* t) (open-map)
                 (say "Кибитка на платформе. Выбери город на карте — поезд довезёт; чем дальше, тем дороже"))))
  :build (lambda (g) (model g "booth-10-extra")))

;; the carpenter: planks
(defbooth 11
  :shelf '("#c8a060" "#8a6440" "#5a3e26")
  :spot '(10.4 2.6 .8)
  :label (lambda () (format nil "E — ~d досок за ~/kiosk-trip::rub/" (first *carpenter-deal*) (second *carpenter-deal*)))
  :use (lambda () (when (pay (second *carpenter-deal*)) (incf *planks* (first *carpenter-deal*))
          (say (format nil "Плотник отсыпал ~d досок. В печку: ~d" (first *carpenter-deal*) *planks*))))
  :build (lambda (g) (model g "booth-11-extra")))

(defparameter *booth-window* '(1.1 2.3 1.0) "The window of a booth: its bottom, its top and half its width.")

(defun tube-sign (g title x y h)
  "Put the sign TITLE on booth G at (X Y): letters of glowing tubes, each on its own, of height H.
As on any old sign, one letter has burnt out and one more flickers; which ones depends on the title."
  (let* ((text (tr title)) (n (length text)) (step (* h .64))
         (letters (loop for i below n unless (char= (char text i) #\Space) collect i))
         (dead (and (> (length letters) 3) (nth (mod (* 7 n) (length letters)) letters)))
         (weak (and (> (length letters) 5) (nth (mod (+ 3 (* 5 n)) (length letters)) letters))))
    (dolist (i letters)
      (let ((letter (sign g "" x y (* step (- i (/ (1- n) 2))) :h h :rgb (if (eql i dead) "#3c3e44" "#ffe9b0"))))
        (plain-text letter (string (char text i)))                 ; one letter: past the translation, it is already done
        (when (and (eql i weak) (not (eql i dead))) (moving letter :blink 2.3))))))

(defun booth-body (g index)
  "Put under G the booth INDEX with its yard, and what lives in it.
The walls in the booth's colour with a window to the road, the lit room behind it with a counter and shelves of this
booth's goods, the awning, the boardwalk, the lamp post, the crates and whatever differs from booth to booth are
drawn in Blender (models/booth-INDEX.blend); the light, the smoke and the seller are made here."
  (model g (format nil "booth-~d" index))
  (light g "#ffd090" 1.6 6.0 10.6 2.3 0)
  (when (= (mod index 3) 0) (chimney-smoke g 12.0 4.0 1.2 :size .6 :amount 6 :life 4.0))
  ;; the seller: a cat behind the counter, looking out at the road; every booth has its own
  (let ((cat (group g 10.25 .16 (if (evenp index) .35 -.35))))
    (node-3d-set-rotation cat (v3 0 (- (/ pi 2)) 0))
    (make-cat cat (+ 3 (* 5 index)))))

(defun build-gimmick (d index title rgb)
  (declare (ignore rgb))                                  ; the booth's colour is in its model now (models/budki)
  (let ((g (apply #'group *world* (road-pos d 3.2 -.35))))
    (node-3d-set-rotation g (v3 0 (- (road-frame d)) 0))
    (booth-body g index)
    (tube-sign g title 8.78 3.5 (if (> (length title) 14) .3 .45))
    (flood g 7.5 3.0 0 :range 14.0)
    (let ((build (booth index :build))) (when build (funcall build g)))      ; every booth has its own landmark
    g))

(defun dev-slots ()
  "Return the testing-ground slots for the booths.
They go in a row from the start of the first town, past gas stations and the market: 14 m apart, and
26 m after each of the first two, whose pond and shooting alley need the room."
  (let ((slots '()))
    (dolist (tw *towns* (nreverse slots))
      (let ((d (+ (town-start tw) 60.0)))
        (loop while (< d (- (town-end tw) 60.0))
              do (cond ((some (lambda (busy) (< (abs (- busy d)) 40.0)) (cons (town-stop-d tw) (town-stations tw))) (incf d 14.0))
                       (t (push d slots) (incf d (if (< (length slots) 3) 26.0 14.0)))))))))

(defun plan-gimmicks ()
  "Place the booths according to the plan; called from PLAN-PLACES."
  (setf *gimmicks* '())
  (loop with slots = (and *dev* (dev-slots))
        for (i f title rgb town) in *gimmick-plan*
        do (let* ((tw (nth (or town i) *towns*)) (i i) (title title) (rgb rgb)
                  (d (or (pop slots) (+ (town-start tw) (* f (- (town-end tw) (town-start tw)))))))
             (push (list d i) *gimmicks*)
             (unless (member "nobare" *dev-flags* :test #'string=)     ; no trees or poles in a booth's yard
               (push (if (member i '(0 1)) (list (- d 11.0) (+ d 17.0)) (list (- d 8.0) (+ d 8.0))) *bare*))   ; the pond and the shooting alley need open ground
             (push (make-scene :d0 (- d 10) :d1 (+ d 10) :build (lambda () (build-gimmick d i title rgb))) *scenes*))))

(defun prikol-usables ()
  "Return the things to press at every booth within walking distance of the wagon: its window and its landmark.
Each booth answers for itself, whichever of them the wagon stopped closest to: on foot one walks from
booth to booth, and a gas station next door does not switch them off."
  (when (< *speed* .4)
    (loop for (d i) in *gimmicks*
          when (< (abs (- d *dist*)) 80.0)
            append (let* ((i i) (d d) (file (format nil "booth-~d" i))
                          ;; where the window and the landmark answer to E: the points at_window and at_spot of the booth's
                          ;; model, in the booth's own axes (it stands 3.2 m off the road axis, .35 m below the road)
                          (window (let ((p (model-point file "window"))) (if p (list (+ 3.2 (first p)) (third p) (- (second p) .35)) '(11.9 0.0 1.3))))
                          (spot (and (booth i :spot)
                                     (let ((p (model-point file "spot"))) (if p (list (+ 3.2 (first p)) (third p) (- (second p) .35)) (booth i :spot))))))
                     (flet ((press (x at up) (road-usable x at up (lambda () (gimmick-label i)) (lambda () (gimmick-use i)) 4.5 1.2)))
                       (cons (press (first window) (+ d (second window)) (third window))
                             (when spot
                               (list (if (booth i :side)                            ; a side door with business of its own
                                         (road-usable (first spot) (+ d (second spot)) (third spot)
                                                      (first (booth i :side)) (second (booth i :side)) 4.5 .8)
                                         (press (first spot) (+ d (second spot)) (third spot)))))))))))

(defun step-prikol ()
  (when (and *wait* (> *time* (job-until *wait*)))                    ; the wait is over: fish, coal
    (let ((job *wait*))
      (setf *wait* nil)
      (if (job-actor job)                                                ; the guest was busy: the catch is theirs
          (run-as (job-actor job) (job-carry job) (job-done job))
          (funcall (job-done job)))))
  ;; the whistle in Krasny Kut: at the market customers come up by themselves, without the bell
  (unless (net-guest-p)
    (let ((place (place-here)))
      (cond ((and place (eq (first place) :market) (rush-p (third place)))
             (unless *rush-told* (setf *rush-told* t) (say-all "Гудок! Смена идёт с завода — покупатели подходят сами и платят щедрее"))
             (when (and (null *client*) (> *window-k* .9) (> (- *time* *rush-bell*) 7.0))
               (setf *rush-bell* *time*) (ring-bell)))
            (t (setf *rush-told* nil))))))
