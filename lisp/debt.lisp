;;;; The story that frames the trip. We come by train and learn that father left his debt to us and went
;;;; abroad (the intro: a few cards over a dark screen). The debt is paid off in parts at the savings bank,
;;;; a booth in the first town (DEFBOOTH 14 below). Once it is paid, the same window
;;;; sells the ticket out: the wagon drives into the sunset, the credits roll and the final tune plays.
;;;; The words (cards, credits) come from mods; the sums are in economy.lisp.
;;;; The debt belongs to whoever owns the game: a guest neither pays it nor sees the intro and the ending.

(in-package #:kiosk-trip)

;; the sums the cards and the calendar may name: {долг}, {цеховик}, {города}
(setf *text-values* (list (cons "долг" (lambda () (money-text *debt-total*)))
                          (cons "цеховик" (lambda () (money-text *shark-total*)))
                          (cons "города" (lambda () (length *town-trade*)))))

(defvar *intro-cards* '() "The cards of the intro, shown one by one: a list of strings.")
(defvar *credits* '() "The lines of the closing credits: a list of strings.")
(defvar *debt* 0 "The number of rubles of father's debt still to be paid.")
(defvar *story-stage* :intro
  "Where the story stands: :INTRO (cards not seen yet), :OWING, :FREE (paid, not left yet) or :DONE.")

(defun save-debt () (unless (net-guest-p) (put-save :debt (list *debt* *story-stage* *debt-total*))))
(defun load-debt ()
  "Read the debt from the save. If the whole debt has been changed since, what was paid still counts."
  (let ((f (get-save :debt)))
    (when (and (fits f '(integer (:member :intro :owing :free :done) &optional integer)) (<= 0 (first f) 10000000))
      (destructuring-bind (left stage &optional (total 300)) f               ; saves without the total are from the days of a debt of 300
        (setf *story-stage* stage
              *debt* (if (and (/= total *debt-total*) (member stage '(:intro :owing)))
                         (max 0 (- *debt-total* (- total left)))
                         left))))))
(defstate :debt
  :clear (lambda ()
           (when (and (boundp '*ending*) *ending*)                         ; a new game in the middle of the credits: roll them up
             (setf *ending* nil)
             (when *ending-ui*
               (object-set (getf *ending-ui* :root) "visible" nil)
               (when (getf *ending-ui* :music) (object-call (getf *ending-ui* :music) "stop"))))
           (setf *debt* *debt-total* *story-stage* :intro *show* nil)
           (when (and (boundp '*show-ui*) *show-ui*) (show-card)))
  :load #'load-debt :save #'save-debt)

(defun debt-hud ()
  "Return the line about the debt for the corner of the screen, or NIL."
  (unless (net-guest-p)
    (format nil "~{~a~^~%~}"
            (remove nil (list (case *story-stage*
                                (:owing (format nil "Ссуда в банке: ~/kiosk-trip::rub/" *debt*))
                                (:free (if (plusp *shark*) "Ссуда выплачена. Остался Аркадий Семёнович" "Долги выплачены. Билет — в сберкассе")))
                              (and (plusp *shark*) (member *story-stage* '(:owing :free))
                                   (format nil "Аркадию Семёновичу: ~/kiosk-trip::rub/, растёт" *shark*)))))))

;;; ------------------------------------------------------------------ cards over a dark screen

(defvar *show* nil "The cards on the screen right now, as a plist of :cards (those left) and :then (what to do after the last), or NIL.")
(defvar *show-ui* nil "The parts of the card screen: a plist of :bg :text.")
(defvar *show-down* t "True if the key or the button that turns the cards was down on the previous frame.")

(defun make-show ()
  (let ((layer (gdext:make-object "CanvasLayer")) (bg (gdext:make-object "ColorRect")))
    (object-set layer "layer" 6)
    (node-add-child *root* layer) (node-add-child layer bg)
    (object-set bg "position" (vector2 0 0)) (object-set bg "size" (vector2 640 360))
    (object-set bg "color" (color .03 .03 .04 .94))
    (object-set bg "mouse_filter" 2)
    (hud-rect bg 120 70 400 1 '(.79 .63 .23)) (hud-rect bg 120 290 400 1 '(.79 .63 .23))
    (let ((text (hud-label bg "" 120 80 12 '(.92 .88 .76))))
      (object-set text "size" (vector2 400 200))
      (object-set text "autowrap_mode" 3)
      (object-set text "horizontal_alignment" 1) (object-set text "vertical_alignment" 1)
      (hud-label bg "Пробел или щелчок — дальше · Q — закрыть" 222 298 8 '(.55 .55 .52))
      (setf *show-ui* (list :bg bg :text text)))))

(defun show-card ()
  (object-set (getf *show-ui* :bg) "visible" (and *show* t))
  (when *show* (object-set (getf *show-ui* :text) "text" (first (getf *show* :cards)))))

(defun open-show (cards then)
  "Show CARDS one by one over a dark screen and call THEN after the last one."
  (unless *show-ui* (make-show))
  (setf *show* (list :cards (mapcar #'fill-text cards) :then then) *show-down* t)
  (show-card))

(defun step-show ()
  (when *show*
    (let ((down (or (input-is-key-pressed 32) (object-call (gdext:singleton "Input") "is_mouse_button_pressed" 1)))                 ; 32 is the space bar
          (quit (input-is-key-pressed 81)))                                                                                    ; Q shuts the lot
      (when (or quit (and down (not *show-down*)))
        (let ((then (getf *show* :then)))
          (if (and (not quit) (rest (getf *show* :cards)))
              (setf (getf *show* :cards) (rest (getf *show* :cards)))
              (setf *show* nil))
          (show-card)
          (unless *show* (funcall then))))
      (setf *show-down* down))))

(defun end-intro ()
  (setf *story-stage* :owing)
  (save-debt)
  (log-event :intro-seen)
  (say (format nil "Долг отца: ~/kiosk-trip::rub/ Гасят его в сберкассе — она в городе ~a" *debt* (town-name 0))))

;;; ------------------------------------------------------------------ the savings bank

(defun debt-share ()
  "Return what the next payment takes: no more than a payment, than the debt, or than there is in the purse."
  (min *debt-chunk* *debt* (max 0 (+ *money* *credit*))))                  ; the bank takes a transfer from the account first

(defun bank-label ()
  (case *story-stage*
    (:free "E — купить билет и уехать к отцу, за границу. Это конец истории")
    (:done "Сберкасса: долгов за вами нет")
    (t (format nil "E — внести ~/kiosk-trip::rub/ в счёт долга · осталось ~/kiosk-trip::rub/" (debt-share) *debt*))))

(defun pay-debt (sum)
  "Take SUM rubles off the debt; when nothing is left, the way out opens.
Return what is left."
  (setf *debt* (max 0 (- *debt* sum)))
  (when (zerop *debt*) (setf *story-stage* :free))
  (save-debt)
  *debt*)

(defun bank-use ()
  "Answer E at the savings bank window: pay a part of the debt, or leave once it is paid."
  (cond (*actor* (say "Долг записан на водителя: платит он"))
        ((eq *story-stage* :done) (say "Сберкасса: долгов за вами нет"))
        ((and (eq *story-stage* :free) (plusp *shark*))
         (say "Кассирша качает головой: «За вами ещё частный долг. Уедете — найдут». Аркадий Семёнович сидит в комиссионном, Первомайск"))
        ((eq *story-stage* :free) (start-ending))
        ((and (not *dev*) (<= (+ *money* *credit*) 0)) (say "Платить нечем: в кошельке пусто"))
        (t (let ((sum (if *dev* (min *debt-chunk* *debt*) (debt-share))))
             (when (pay-by-transfer sum)
               (log-event :debt :paid sum :left (pay-debt sum) :money *money*)
               (if (zerop *debt*)
                   (say-all "Долг закрыт! Кассирша ставит штамп «ПОГАШЕНО». Теперь в этом окне продают билет к отцу — E, когда будете готовы")
                   (say (format nil "Внесли ~/kiosk-trip::rub/ Осталось ~/kiosk-trip::rub/" sum *debt*))))))))

;;; The man father owed sits in the commission shop in the port. At its window his debt is paid, in cash
;;; only; at its side door the co-operative's account is turned into cash for his share. If no payment
;;; comes for *SHARK-PATIENCE* days, his people call at the wagon at dawn and take the dearest thing on board.

(defun shark-label ()
  (cond ((<= *shark* 0) "Комиссионный. Аркадий Семёнович: «Мы в расчёте. Отцу привет»")
        (t (format nil "E — отдать Аркадию Семёновичу наличными ~/kiosk-trip::rub/ · за нами ~/kiosk-trip::rub/, процент в день"
                   (min *money* *shark* *debt-chunk*) *shark*))))

(defun shark-use ()
  "Pay the man in cash: as much as there is, no more than a payment."
  (cond (*actor* (say "Аркадий Семёнович говорит только с тем, на ком долг"))
        ((<= *shark* 0) (say "Аркадий Семёнович: «Мы в расчёте»"))
        ((and (not *dev*) (<= *money* 0)) (say "Аркадий Семёнович берёт только наличными, а их нет. Со счёта обналичат у боковой двери"))
        (t (let ((sum (if *dev* (min *shark* *debt-chunk*) (min *money* *shark* *debt-chunk*))))
             (unless *dev* (decf *money* sum))
             (setf *shark* (- *shark* sum) *shark-paid-day* (today))
             (save-plan)
             (log-event :shark :paid sum :left *shark*)
             (say-all (if (plusp *shark*)
                          (format nil "Отдали ~/kiosk-trip::rub/ Аркадий Семёнович: «За вами ещё ~/kiosk-trip::rub/»" sum *shark*)
                          "Аркадий Семёнович пересчитал и убрал тетрадь: «В расчёте. Отцу привет»"))))))

(defun cash-label ()
  (format nil "E — обналичить со счёта: за ~/kiosk-trip::rub/ дадут ~/kiosk-trip::rub/ · на счёте ~/kiosk-trip::rub/"
          (min *credit* *cash-chunk*) (floor (* *cash-share* (min *credit* *cash-chunk*))) *credit*))

(defshared cash-out :cash ()
  "Turn a part of the co-operative's account into cash, for the man's share."
  (let ((sum (min *credit* *cash-chunk*)))
    (cond ((<= sum 0) (say "На счёте пусто: его пополняют наряды"))
          (t (decf *credit* sum) (incf *money* (floor (* *cash-share* sum)))
             (save-plan)
             (log-event :cash :sum sum :money *money*)
             (say-all (format nil "Обналичили ~/kiosk-trip::rub/: на руки ~/kiosk-trip::rub/" sum (floor (* *cash-share* sum))))))))

(defbooth 15 :label #'shark-label :use #'shark-use :spot '(9.1 -2.75 1.1) :side (list #'cash-label #'cash-out)
  :shelf '("#5a2a5a" "#c9a13a" "#2c4a7a" "#e8e0d0")
  :build (lambda (g) (shape g :box '(.1 2.0 .9) "#3a2a1e" 9.0 1.0 -2.75) (shape g :ball '(.05 6 4) "#d8a840" 8.93 1.0 -2.45 :metal .6)))

(defvar *seize-day* nil "The day up to which the man's people have been counted.")

(defun step-shark ()
  "At dawn, if the man has waited too long, his people take the dearest thing on board against the percent."
  (unless (net-guest-p)
    (let ((day (today)))
      (cond ((or (null *seize-day*) (< day *seize-day*)) (setf *seize-day* day))
            ((> day *seize-day*)
             (setf *seize-day* day)
             (when (shark-overdue-p day)
               (let ((best (first (sort (remove-if-not (lambda (spec) (and (good-p (first spec)) (null (sixth spec)))) (mapcar #'car *items*))
                                        #'> :key (lambda (spec) (good-base (raw-of (first spec))))))))
                 (when best
                   (set-items (remove best (mapcar #'car *items*) :test #'equal :count 1))
                   (save-interior)
                   (setf *shark* (max 0 (- *shark* (good-base (raw-of (first best))))) *shark-paid-day* day)
                   (save-plan)
                   (say-all (format nil "Ночью заходили люди Аркадия Семёновича: забрали ~a в счёт процентов. Он сидит в комиссионном, в Первомайске"
                                    (good-name (first best))))))))))))

;;; The wage. A co-operative may pay its members a wage from its account (the law of 1988): at the savings
;;; bank's side door the whole account comes out in cash, with nothing kept back, but once a week only.
;;; Arkady Semyonovich gives cash any day, for his share.

(defparameter *wage-days* 7 "How many game days must pass between two wages at the savings bank.")
(defvar *wage-day* nil "The game day of the last wage, or NIL.")

(defun wage-wait ()
  "Return how many game days are left before the next wage: 0 when it can be taken now."
  (if *wage-day* (max 0 (- (+ *wage-day* *wage-days*) (today))) 0))

(defun wage-label ()
  (cond ((plusp (wage-wait)) (format nil "Зарплата кооператива: следующая через ~d дн. · на счёте ~/kiosk-trip::rub/" (wage-wait) *credit*))
        ((<= *credit* 0) "Зарплата кооператива: на счёте пусто, его пополняют наряды")
        (t (format nil "E — получить зарплату: весь счёт на руки, ~/kiosk-trip::rub/ Следующая — через ~d дн." *credit* *wage-days*))))

(defshared take-wage :wage ()
  "Take the whole co-operative's account in cash, once a week."
  (cond ((plusp (wage-wait)) (say (format nil "Кассирша: «Зарплату выдаём раз в неделю. Приходите через ~d дн.»" (wage-wait))))
        ((<= *credit* 0) (say "Кассирша: «На счёте кооператива пусто»"))
        (t (let ((sum *credit*))
             (incf *money* sum)
             (setf *credit* 0 *wage-day* (today))
             (save-plan) (save-wage)
             (log-event :wage :sum sum :money *money*)
             (say-all (format nil "Кассирша отсчитала зарплату: ~/kiosk-trip::rub/ на руки. Следующая — через ~d дн." sum *wage-days*))))))

(defun save-wage () (unless (net-guest-p) (put-save :wage (list *wage-day*))))
(defstate :wage
  :clear (lambda () (setf *wage-day* nil))
  :load (lambda () (let ((f (get-save :wage))) (when (fits f '(t)) (setf *wage-day* (and (integerp (first f)) (first f))))))
  :save #'save-wage)

;; the savings bank is a booth like the others (DEFBOOTH, prikol.lisp); the wage is paid at its side door
(defbooth 14 :label #'bank-label :use #'bank-use :shelf '("#4a6a52" "#d8d0b8" "#c9a13a")
  :spot '(9.1 -2.75 1.1) :side (list #'wage-label #'take-wage)
  :build (lambda (g) (shape g :box '(.1 2.0 .9) "#5a4a3a" 9.0 1.0 -2.75) (shape g :ball '(.05 6 4) "#d8a840" 8.93 1.0 -2.45 :metal .6)
           (sign g "ЗАРПЛАТА" 8.93 2.15 -2.75 :h .16 :rgb "#e8e0d0" :yaw (- (/ pi 2)))))

;;; ------------------------------------------------------------------ the ending

(defparameter *ending-time* 92.0 "The length of the closing credits, in seconds.")
(defvar *ending* nil "The number of seconds since the ending started, or NIL.")
(defvar *ending-ui* nil "The parts of the credits screen: a plist of :root :text :music.")

(defun make-ending ()
  (let ((layer (gdext:make-object "CanvasLayer")) (root (gdext:make-object "Control")) (path (sound-path "finale")))
    (object-set layer "layer" 6)
    (node-add-child *root* layer) (node-add-child layer root)
    (object-set root "position" (vector2 0 0)) (object-set root "mouse_filter" 2)
    (let ((text (hud-label root "" 120 360 12 '(.96 .92 .80))))
      (object-set text "size" (vector2 400 0))
      (object-set text "horizontal_alignment" 1)
      (object-call text "add_theme_constant_override" "outline_size" 4)
      (object-call text "add_theme_color_override" "font_outline_color" (color 0 0 0 .85))
      (hud-rect root 0 0 640 34 '(0 0 0)) (hud-rect root 0 326 640 34 '(0 0 0))       ; the bars of a wide film
      (setf *ending-ui* (list :root root :text text
                              :music (and (file-here-p path) (player *root* (wav-stream path) :db -6.0 :bus "Музыка" :play nil)))))))

(defun credit-lines ()
  "Return the lines of the credits: the mod's own, with where the people of the road ended up put in before the last block."
  (let* ((lines (mapcar #'tr *credits*)) (cut (max 0 (- (length lines) 6))))
    (append (subseq lines 0 cut) (if (fboundp 'people-epilogue) (funcall 'people-epilogue) '()) (subseq lines cut))))

(defun start-ending ()
  "Leave for the country father went to: drive into the sunset under the credits and the final tune."
  (unless *ending-ui* (make-ending))
  (destructuring-bind (&key root text music) *ending-ui*
    (object-set text "text" (format nil "~{~a~^~%~}" (credit-lines)))
    (object-set root "visible" t)
    (when music (object-call music "play")))
  (setf *ending* 0.0)
  (set-weather :clear 5)
  (let ((far (nth (mod (+ (or (trade-town) 0) 6) (max 1 (length *towns*))) *towns*)))  ; the far side of the ring: the road will not end
    (when far (set-course (town-stop-d far) "закат")))
  (log-event :ending :money *money*)
  (say-all "Билет в кармане. Кибитка сама выруливает на дорогу — на запад, к отцу"))

(defun stop-ending ()
  (destructuring-bind (&key root text music) *ending-ui*
    (declare (ignore text))
    (object-set root "visible" nil)
    (when music (object-call music "stop")))
  (setf *ending* nil *auto* nil *story-stage* :done)
  (save-debt)
  (say-all "Конец истории. Долгов больше нет, дорога ваша — можно ехать дальше"))

(defun step-ending (delta)
  (when *ending*
    (incf *ending* delta)
    (let ((k (min 1.0 (/ *ending* *ending-time*))))
      (setf *fuel* 1.0                                                       ; the road into the sunset is free
            *day-t* (+ (* *day-len* (floor *day-t* *day-len*))               ; the sun goes down as the credits roll
                       (first *dusk*) -40.0 (* k (+ 40.0 (* .7 (- (second *dusk*) (first *dusk*)))))))
      ;; the credits rise from under the screen and leave over its top; the last line stops in the middle
      (object-set (getf *ending-ui* :text) "position"
                  (vector2 120 (- 360.0 (* k (+ 185.0 (* 17.0 (length (credit-lines)))))))))
    (when (> *ending* (+ *ending-time* 6.0)) (stop-ending))))

(defun step-story (delta)
  "Advance the story by DELTA seconds: open the intro once the lobby is closed, turn the cards, roll the credits."
  (unless (net-guest-p)
    (when (and (eq *story-stage* :intro) (not *show*) (not *lobby-open*))
      (if (or *dev* (null *intro-cards*))
          (setf *story-stage* :owing)                                        ; nothing to show: straight to the debt
          (open-show *intro-cards* #'end-intro))))
  (step-show)
  (step-shark)
  (step-ending delta))
