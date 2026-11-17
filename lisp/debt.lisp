;;;; The story that frames the trip. We come by train and learn that father left his debt to us and went
;;;; abroad (the intro: a few cards over a dark screen). The debt is paid off in parts at the savings bank,
;;;; a booth in the first town (DEFBOOTH 14 below). Once it is paid, the same window
;;;; sells the ticket out: the wagon drives into the sunset, the credits roll and the final tune plays.
;;;; The words (cards, credits) come from mods; the sums are in economy.lisp.
;;;; The debt belongs to whoever owns the game: a guest neither pays it nor sees the intro and the ending.

(in-package #:kiosk-trip)

(defvar *intro-cards* '() "The cards of the intro, shown one by one: a list of strings.")
(defvar *credits* '() "The lines of the closing credits: a list of strings.")
(defvar *debt* 0 "The number of rubles of father's debt still to be paid.")
(defvar *story-stage* :intro
  "Where the story stands: :INTRO (cards not seen yet), :OWING, :FREE (paid, not left yet) or :DONE.")

(defun save-debt () (unless (net-guest-p) (put-save :debt (list *debt* *story-stage*))))
(defun load-debt ()
  (let ((f (get-save :debt)))
    (when (and (fits f '(integer (:member :intro :owing :free :done))) (<= 0 (first f) 1000000))
      (setf *debt* (first f) *story-stage* (second f)))))
(defstate :debt
  :clear (lambda () (setf *debt* *debt-total* *story-stage* :intro))
  :load #'load-debt :save #'save-debt)

(defun debt-hud ()
  "Return the line about the debt for the corner of the screen, or NIL."
  (unless (net-guest-p)
    (case *story-stage*
      (:owing (format nil "Долг отца: ~d р." *debt*))
      (:free "Долг выплачен. Билет — в сберкассе"))))

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
      (hud-label bg "Пробел или щелчок — дальше" 250 298 8 '(.55 .55 .52))
      (setf *show-ui* (list :bg bg :text text)))))

(defun show-card ()
  (object-set (getf *show-ui* :bg) "visible" (and *show* t))
  (when *show* (object-set (getf *show-ui* :text) "text" (first (getf *show* :cards)))))

(defun open-show (cards then)
  "Show CARDS one by one over a dark screen and call THEN after the last one."
  (unless *show-ui* (make-show))
  (setf *show* (list :cards cards :then then) *show-down* t)
  (show-card))

(defun step-show ()
  (when *show*
    (let ((down (or (input-is-key-pressed 32) (object-call (gdext:singleton "Input") "is_mouse_button_pressed" 1))))                ; 32 is the space bar
      (when (and down (not *show-down*))
        (let ((then (getf *show* :then)))
          (if (rest (getf *show* :cards))
              (setf (getf *show* :cards) (rest (getf *show* :cards)))
              (setf *show* nil))
          (show-card)
          (unless *show* (funcall then))))
      (setf *show-down* down))))

(defun end-intro ()
  (setf *story-stage* :owing)
  (save-debt)
  (log-event :intro-seen)
  (say (format nil "Долг отца: ~d р. Гасят его в сберкассе — она в городе ~a" *debt* (town-name 0))))

;;; ------------------------------------------------------------------ the savings bank

(defun debt-share ()
  "Return the number of rubles the next payment takes."
  (min *debt-chunk* *debt*))

(defun bank-label ()
  (case *story-stage*
    (:free "E — купить билет и уехать к отцу, за границу. Это конец истории")
    (:done "Сберкасса: долгов за вами нет")
    (t (format nil "E — внести ~d р. в счёт долга · осталось ~d р." (debt-share) *debt*))))

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
        ((eq *story-stage* :free) (start-ending))
        (t (let ((sum (debt-share)))
             (when (pay sum)
               (log-event :debt :paid sum :left (pay-debt sum) :money *money*)
               (if (zerop *debt*)
                   (say-all "Долг закрыт! Кассирша ставит штамп «ПОГАШЕНО». Теперь в этом окне продают билет к отцу — E, когда будете готовы")
                   (say (format nil "Внесли ~d р. Осталось ~d р." sum *debt*))))))))

;; the savings bank is a booth like the others (DEFBOOTH, prikol.lisp)
(defbooth 14 :label #'bank-label :use #'bank-use)

;;; ------------------------------------------------------------------ the ending

(defparameter *ending-time* 64.0 "The length of the closing credits, in seconds.")
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

(defun start-ending ()
  "Leave for the country father went to: drive into the sunset under the credits and the final tune."
  (unless *ending-ui* (make-ending))
  (destructuring-bind (&key root text music) *ending-ui*
    (object-set text "text" (format nil "~{~a~^~%~}" (mapcar #'tr *credits*)))
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
                  (vector2 120 (- 360.0 (* k (+ 185.0 (* 17.0 (length *credits*))))))))
    (when (> *ending* (+ *ending-time* 6.0)) (stop-ending))))

(defun step-story (delta)
  "Advance the story by DELTA seconds: open the intro once the lobby is closed, turn the cards, roll the credits."
  (unless (net-guest-p)
    (when (and (eq *story-stage* :intro) (not *show*) (not *lobby-open*))
      (if (or *dev* (null *intro-cards*))
          (setf *story-stage* :owing)                                        ; nothing to show: straight to the debt
          (open-show *intro-cards* #'end-intro))))
  (step-show)
  (step-ending delta))
