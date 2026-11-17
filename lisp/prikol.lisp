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

(defun pay (sum)
  "Pay SUM roubles.
Return NIL and show a hint if there is not enough money."
  (cond (*dev* t)                                                        ; on the dev polygon everything is free
        ((< *money* sum) (say (format nil "Нужно ~d р., а у нас ~d" sum *money*)) nil)
        (t (decf *money* sum) t)))

(defun give-good (kind text)
  "Put a good of KIND into the hands (if they are free) and say TEXT.
Return true if the good was given."
  (cond (*carry* (say "Руки заняты — сначала поставь то, что несёшь, на полку") nil)
        (t (setf *carry* (cons kind nil) *carry-from* nil *carry-cut* nil)
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

(defmacro defbooth (number &key label use build spot daily shelf)
  "Declare what the booth NUMBER does.
LABEL is the hint at its window: a string or a function returning one. USE is a function called on E.
BUILD is a function of the booth group that adds its landmark. SPOT is (sideways along-the-road height):
where else, besides the window, it answers to E. With DAILY its free thing is given once a game day.
SHELF is a list of colours: what stands on the shelves behind the seller."
  `(progn (setf *booths* (cons (list ,number :label ,label :use ,use :build ,build :spot ,spot :daily ,daily :shelf ,shelf)
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

(defshared gimmick-use :gimmick (i)
  "Do what E does at booth I."
  (let ((use (booth i :use)))
    (when (and use (not (and (booth i :daily) (spent-p i)))) (funcall use))))

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
  (shape g :box '(1.8 1.0 .5) "#8a6440" 7.5 .5 -4.2)
  (shape g :box '(6.6 3.0 .2) "#4a3a2c" 7.5 1.5 -13.4)
  (dolist (y '(1.02 1.72)) (shape g :box '(6.6 .05 .12) "#2a2c30" 7.5 y -13.25 :shadow nil))
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
  (shape g :box '(4.9 2.7 .4) "#2e2a27" 7.5 1.25 7.3)
  (setf *rock* (loop for i below 32
                     collect (list (shape g :box '(.5 .5 .5) "#5a554e" (+ 5.58 (* .55 (mod i 8))) (+ .3 (* .55 (floor i 8))) 6.85)
                                   (mod i 8) (floor i 8) nil nil))))

(defun build-pond (g)
  "Build the fishing pond beside booth group G: the water, a short pier and reeds."
  (setf *pond* (shape g :cyl '(4.0 4.0 .05 24) "#3f7f9a" 9.0 .07 -9.5 :shadow nil :rough .15 :metal .3))
  (shape g :cyl '(4.3 4.3 .04 24) "#6a6a52" 9.0 .04 -9.5 :shadow nil)
  (shape g :box '(1.3 .1 2.6) "#8a6440" 9.0 .32 -5.6)
  (dolist (x '(8.5 9.5)) (shape g :cyl '(.06 .06 .5 6) "#5a4636" x .15 -6.8))
  (loop for (x z) in '((5.6 -8.2) (5.9 -10.9) (12.2 -8.0) (12.4 -11.2) (9.4 -13.2))
        do (shape g :cyl '(.02 .03 1.1 5) "#5a7a3a" x .55 z :shadow nil)))

;;; ------------------------------------------------------------------ the booths themselves

;; the pier: fishing
(defbooth 0
  :shelf '("#3f6f73" "#8aa0b0" "#c9a13a")
  :daily t
  :spot '(12.2 5.6 1.2)
  :label "E — порыбачить с мостков"
  :use (lambda () (if *carry* (say "Руки заняты — удочку не взять") (begin-play :fish)))
  :build (lambda (g) (unless (member "nopond" *dev-flags* :test #'string=) (build-pond g)) (beam-between g '(7.0 1.0 -2.6) '(5.0 3.4 -2.6) :thick .04 :rgb "#3a2a1e") (shape g :box '(.5 .5 .5) "#55708a" 7.2 .25 -2.4)))

;; the shooting range
(defbooth 1
  :shelf '("#e8e0d0" "#a3262c" "#3c4638")
  :spot '(10.7 4.2 1.3)
  :label (lambda () (format nil "E — тир: пять выстрелов за ~d р. Целиться самому" *range-price*))
  :use (lambda () (when (pay *range-price*) (begin-play :tir)))
  :build (lambda (g) (unless (member "nopond" *dev-flags* :test #'string=) (build-range g)) (dolist (z '(2.6 3.4 4.2)) (shape g :cyl '(.3 .3 .04 12) "#e8e0d0" 12.8 1.6 z :rot (list 0 0 (/ pi 2))) (shape g :cyl '(.1 .1 .05 8) "#a3262c" 12.76 1.6 z :rot (list 0 0 (/ pi 2)) :shadow nil))))

;; the hardware shop: the lid for the frying pan
(defbooth 2
  :shelf '("#1e1e22" "#8a8e92" "#a3262c")
  :label (lambda () (if *lid* (format nil "E — ~d банки с крышками за ~d р. · у вас пустых: ~d" (first *jar-deal*) (second *jar-deal*) *jars*)
           (format nil "E — купить чугунную крышку для сковородки за ~d р.: под ней жарится вдвое быстрее" *lid-price*)))
  :use (lambda () (cond (*lid* (when (pay (second *jar-deal*)) (incf *jars* (first *jar-deal*)) (save-home) (show-jars)
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
  :build (lambda (g) (dotimes (i 4) (shape g :box '(.5 .5 .7) (nth i '("#6a3a2e" "#2c4a7a" "#3c5a48" "#c9a13a")) 7.2 (+ .25 (* i .5)) 3.2 :rot (list 0 (* i .2) 0)))))

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
         (shape g :cyl '(.2 .16 .3 10) "#8a8e92" 7.0 .15 2.2 :metal .5 :rough .4)
         (setf *goat-milk* (shape g :cyl '(.17 .17 .02 10) "#f6f4ee" 7.0 .06 2.2 :shadow nil))
         (shape g :box '(.1 .9 3.0) "#6a5038" 5.4 .45 3.0)))

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
         (shape g :cyl '(1.1 1.1 .12 12) "#2a2c30" 7.0 6.06 -3.2)
         (shape g :cyl '(.5 .5 .8 10) "#ffe9a0" 7.0 6.5 -3.2 :emit "#ffd870" :energy 3.0 :shadow nil)
         (shape g :cyl '(.05 .7 .5 10) "#a3262c" 7.0 7.15 -3.2)))

;; window no. 3: the stamp
(defbooth 6
  :shelf '("#e8e0d0" "#8a8070" "#6a3a2e")
  :label (lambda () (if *stamp* "Печать уже стоит: за посылки платят вдвое" (format nil "E — получить печать за ~d р.: посылки вдвое дороже" *stamp-price*)))
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
  :build (lambda (g) (build-rock g) (dolist (z '(-3.4 -4.6)) (shape g :box '(.3 5.0 .3) "#5a4636" 11.0 2.5 z)) (shape g :box '(.4 .4 1.8) "#5a4636" 11.0 5.0 -4.0) (shape g :box '(1.4 1.8 1.0) "#3a3a40" 11.0 .9 -4.0)))

;; mineral water
(defbooth 9
  :shelf '("#7ab0c8" "#e8e0d0" "#3f6f73")
  :spot '(10.4 2.6 1.0)
  :label (lambda () (format nil "E — стакан минеральной воды за ~d р.: бодрит" *water-price*))
  :use (lambda () (when (pay *water-price*) (setf *boost-until* (+ *time* *water-time*)) (say "Нарзан! В нос шибает. Пять минут бегаешь быстрее")))
  :build (lambda (g) (shape g :cyl '(.5 .6 1.0 10) "#e8e4d8" 7.2 .5 -2.6) (shape g :cyl '(.06 .06 .5 6) "#7ab0c8" 7.2 1.2 -2.6 :alpha .6)))

;; the flatcar: the wagon goes by train
(defbooth 10
  :shelf '("#7a5a50" "#d8d0b8" "#2a2c30")
  :label (lambda () (format nil "E — погрузить кибитку на платформу: до любого города, ~,1f р. за километр" *rail-per-km*))
  :use (lambda () (cond (*actor* (say "На платформу кибитку ставит водитель: маршрут — его дело"))
              (t (setf *rail-ticket* t) (open-map)
                 (say "Кибитка на платформе. Выбери город на карте — поезд довезёт; чем дальше, тем дороже"))))
  :build (lambda (g) (shape g :box '(3.2 .9 12.0) "#4a4e52" 16.0 .45 0) (dolist (x '(15.2 16.8)) (shape g :box '(.1 .14 13.0) "#8a8e92" x .05 0 :metal .6))))

;; the carpenter: planks
(defbooth 11
  :shelf '("#c8a060" "#8a6440" "#5a3e26")
  :spot '(10.4 2.6 .8)
  :label (lambda () (format nil "E — ~d досок за ~d р." (first *carpenter-deal*) (second *carpenter-deal*)))
  :use (lambda () (when (pay (second *carpenter-deal*)) (incf *planks* (first *carpenter-deal*))
          (say (format nil "Плотник отсыпал ~d досок. В печку: ~d" (first *carpenter-deal*) *planks*))))
  :build (lambda (g) (dotimes (i 6) (shape g :box '(2.4 .06 .22) "#c8a060" 7.2 (+ .04 (* i .065)) (+ 2.4 (* .03 i)))) (shape g :box '(1.6 .8 .7) "#8a6440" 7.2 .4 -2.6)))

(defparameter *booth-window* '(1.1 2.3 1.0) "The window of a booth: its bottom, its top and half its width.")

(defun booth-shell (g index rgb)
  "Build under G the walls of booth INDEX in colour RGB, with a window to the road, and what is seen through it:
a lit room, a counter, shelves with the goods of this booth (its :SHELF colours) and the seller."
  (destructuring-bind (y0 y1 half) *booth-window*
    (shape g :box '(.15 2.8 4.6) rgb 12.925 1.4 0)                                        ; the back wall
    (dolist (z '(-2.225 2.225)) (shape g :box '(4.0 2.8 .15) rgb 11.0 1.4 z))              ; the sides
    (shape g :box (list .15 y0 4.6) rgb 9.075 (/ y0 2) 0)                                  ; the front: below the window,
    (shape g :box (list .15 (- 2.8 y1) 4.6) rgb 9.075 (/ (+ y1 2.8) 2) 0)                  ; above it
    (dolist (s '(-1 1))                                                                    ; and beside it
      (shape g :box (list .15 (- y1 y0) (- 2.3 half)) rgb 9.075 (/ (+ y0 y1) 2) (* s (/ (+ 2.3 half) 2))))
    (dolist (s '(-1 1)) (shape g :box (list .2 (- y1 y0) .08) "#e8e0d0" 9.05 (/ (+ y0 y1) 2) (* s half) :shadow nil))   ; the window frame
    (shape g :box (list .2 .08 (* 2 half)) "#e8e0d0" 9.05 y1 0 :shadow nil)
    ;; inside
    (shape g :box '(3.7 .06 4.3) "#6a5a48" 11.0 .13 0 :shadow nil)                         ; the floor
    (shape g :box '(.02 2.6 4.3) "#d8d0b8" 12.84 1.4 0 :shadow nil)                        ; whitewash on the back wall
    (shape g :box '(.5 .95 3.6) "#8a6440" 9.5 .575 0)                                      ; the counter
    (shape g :box '(.56 .04 3.7) "#5a3e26" 9.5 1.07 0 :shadow nil)
    (let ((colours (or (booth index :shelf) '("#8a8e92" "#b0aa98" "#6a5038"))))
      (loop for y in '(.9 1.5 2.1) for row from 0
            do (shape g :box '(.36 .04 3.9) "#5a3e26" 12.64 y 0 :shadow nil)
               (loop for k below 8
                     do (shape g :box (list .2 (+ .2 (* .06 (mod (+ k row) 3))) .24) (nth (mod (+ k row index) (length colours)) colours)
                               12.64 (+ y .02 (/ (+ .2 (* .06 (mod (+ k row) 3))) 2)) (- (* k .48) 1.68) :shadow nil))))
    (shape g :ball '(.09 8 5) "#ffe8b0" 10.9 2.55 0 :emit "#ffc070" :energy 2.5 :shadow nil)   ; the lamp
    (shape g :cyl '(.008 .008 .2 4) "#2a2a2e" 10.9 2.7 0 :shadow nil)
    (light g "#ffd090" 1.6 6.0 10.6 2.3 0)
    ;; the seller: a cat behind the counter, looking out at the road; every booth has its own
    (let ((cat (group g 10.25 .16 (if (evenp index) .35 -.35))))
      (node-3d-set-rotation cat (v3 0 (- (/ pi 2)) 0))
      (make-cat cat (+ 3 (* 5 index))))))

(defun booth-yard (g index)
  "Build under G what stands around booth INDEX: an awning, price cards, a boardwalk, an urn, a lamp post,
crates by the wall; flower boxes or shutters, a notice board, a bicycle and something on the roof, which
differ from booth to booth so that no two yards are alike."
  (let ((paint (nth (mod index 4) '("#a3262c" "#3f6f73" "#c9a13a" "#2c4a7a"))))
    (dotimes (i 6)                                                                       ; a striped awning over the window
      (shape g :box '(.9 .04 .42) (if (evenp i) "#e8e0d0" paint) 8.6 2.52 (- (* i .42) 1.05) :rot '(0 0 .35) :shadow nil))
    (shape g :box '(.01 .01 1.9) "#3a2a1e" 8.98 2.22 0 :shadow nil)                       ; price cards on a string
    (dotimes (i 5) (shape g :box '(.01 .14 .18) "#f0ece0" 8.97 2.12 (- (* i .4) .8) :rot (list (* .08 (- (mod i 3) 1)) 0 0) :shadow nil))
    (shape g :box '(1.2 .04 2.6) "#8a6440" 8.2 .12 0 :shadow nil)                         ; a boardwalk to the window
    (shape g :cyl '(.2 .15 .5 8) "#4c4e52" 8.55 .35 2.0)                                  ; an urn
    (if (evenp index)
        (dolist (s '(-1 1))                                                              ; flower boxes under the window
          (shape g :box '(.22 .16 .8) "#6a3a2e" 8.9 1.0 (* s 1.65))
          (dotimes (i 4) (shape g :ball '(.07 5 4) (nth (mod (+ i index) 3) '("#c8382c" "#e8d060" "#f0ece0")) 8.9 1.14 (+ (* s 1.65) (* .2 (- i 1.5))) :shadow nil)))
        (progn (dolist (s '(-1 1)) (shape g :box '(.05 1.2 .5) paint 8.98 1.7 (* s 1.3) :shadow nil))     ; shutters
               (shape g :box '(.05 .8 .6) "#5a3e26" 8.97 1.7 -1.95 :shadow nil)                       ; a notice board with papers
               (loop for (y z) in '((1.85 -2.05) (1.8 -1.82) (1.55 -1.95))
                     do (shape g :box '(.01 .2 .16) "#f0ece0" 8.94 y z :rot (list (* .1 (- z -1.95) 8) 0 0) :shadow nil))))
    (shape g :box '(.7 .5 .7) "#8a6440" 10.2 .35 2.75) (shape g :box '(.6 .45 .6) "#7a5a3a" 10.25 .82 2.75 :rot '(0 .3 0))   ; crates and a barrel by the wall
    (shape g :cyl '(.3 .3 .8 10) "#5a4636" 11.2 .5 2.8)
    (shape g :cyl '(.05 .07 3.6 6) "#34363a" 8.4 1.9 -2.9)                                ; a lamp post
    (shape g :box '(.7 .05 .05) "#34363a" 8.1 3.65 -2.9 :shadow nil)
    (shape g :ball '(.13 8 5) "#ffe8b0" 7.8 3.55 -2.9 :emit "#ffc070" :energy 2.5 :shadow nil)
    (when (= (mod index 4) 1)                                                            ; somebody's bicycle
      (dolist (x '(10.2 11.1)) (shape g :cyl '(.3 .3 .04 12) "#2a2c30" x .4 -2.72 :rot (list (/ pi 2) 0 0)))
      (shape g :box '(.9 .04 .04) paint 10.65 .62 -2.72 :rot '(0 0 .15)) (shape g :box '(.04 .5 .04) paint 10.45 .7 -2.72)
      (shape g :box '(.22 .05 .1) "#3a2a1e" 10.45 .96 -2.72) (shape g :box '(.04 .45 .04) "#8a8e92" 11.05 .75 -2.72 :rot '(0 0 -.25)))
    (case (mod index 3)                                                                  ; on the roof
      (0 (shape g :cyl '(.1 .1 .9 8) "#2a2c30" 12.0 3.5 1.2) (chimney-smoke g 12.0 4.0 1.2 :size .6 :amount 6 :life 4.0))
      (1 (shape g :cyl '(.02 .02 1.6 4) "#8a8e92" 12.2 3.9 -1.4)
         (dolist (y '(4.3 4.55)) (shape g :box '(.02 .02 .7) "#8a8e92" 12.2 y -1.4 :shadow nil)))
      (2 (dotimes (i 9) (let ((col (nth (mod i 5) *fairy*))) (shape g :ball '(.06 5 4) col 8.72 3.02 (- (* i .5) 2.0) :emit col :energy 2.0 :shadow nil)))))))

(defun build-gimmick (d index title rgb)
  (let ((g (apply #'group *world* (road-pos d 3.2 -.35))))
    (node-3d-set-rotation g (v3 0 (- (road-frame d)) 0))
    (shape g :box '(9.0 .1 10.0) "#5a5a58" 7.5 .05 0 :shadow nil)
    (booth-shell g index rgb)
    (booth-yard g index)
    (shape g :box '(4.6 .25 5.2) "#4c4e52" 11.0 2.95 0 :rot '(0 0 -.06))
    (shape g :box '(.6 .08 1.8) "#8a6440" 8.7 1.05 0)
    (shape g :box '(.2 .7 4.4) "#2a2c30" 8.9 3.5 0 :shadow nil)
    (sign g title 8.78 3.5 0 :h (if (> (length title) 14) .3 .45) :rgb "#f0ece0")
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
             (when (and (member i '(0 1)) (not (member "nobare" *dev-flags* :test #'string=))) (push (list (- d 11.0) (+ d 17.0)) *bare*))   ; the pond and the shooting alley need open ground
             (push (make-scene :d0 (- d 10) :d1 (+ d 10) :build (lambda () (build-gimmick d i title rgb))) *scenes*))))

(defun prikol-usables ()
  "Return the things to press at every booth within walking distance of the wagon: its window and its landmark.
Each booth answers for itself, whichever of them the wagon stopped closest to: on foot one walks from
booth to booth, and a gas station next door does not switch them off."
  (when (< *speed* .4)
    (loop for (d i) in *gimmicks*
          when (< (abs (- d *dist*)) 80.0)
            append (let ((i i) (d d) (spot (booth i :spot)))
                     (flet ((press (x at up) (road-usable x at up (lambda () (gimmick-label i)) (lambda () (gimmick-use i)) 4.5 1.2)))
                       (cons (press 11.9 d 1.3)
                             (when spot (list (press (first spot) (+ d (second spot)) (third spot))))))))))

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
