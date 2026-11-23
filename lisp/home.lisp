;;;; Home: what gives the wagon a day and a year of its own.
;;;;
;;;; The tear-off calendar by the door has a leaf for every game day: the day of the trip, the weekday, the
;;;; season and how long it lasts, and on the back a note and an omen. The omens are true: tomorrow's weather
;;;; is decided from the day's number, the leaf foretells it and at dawn the driver's wagon makes it so.
;;;; The trip diary hangs under the calendar and writes itself: it hears the events of the game (LOG-EVENT)
;;;; and keeps a page per day. Before winter the window is sealed with paper and a quilt is hung on the
;;;; door: the wagon cools more slowly. Jars are filled and closed by paw at the crate under the washstand:
;;;; a good, boiling water from the kettle, and turns of the seaming key; what is in a jar never goes bad.
;;;; The words of the calendar come from mods (*CALENDAR-NOTES*, *OMENS*); the numbers are in economy.lisp.

(in-package #:kiosk-trip)

(defvar *calendar-notes* '() "The notes on the backs of the calendar leaves: a list of strings, one a day in turn.")
(defvar *omens* '() "The omens by tomorrow's weather: a plist of weather and a list of strings.")
(defvar *weekdays* '() "The names of the days of the week, from mods: seven strings, Monday first.")

(defvar *calendar-day* nil "The day whose leaf is on top of the calendar, or NIL before the first look.")
(defvar *said-saved* 0 "The value of *SAID-N* when the notebook last went into the save.")
(defvar *trip-day0* 0 "The game day on which this trip began: the calendar and the diary count the days from it.")
(defvar *weather-day* nil "The day for which the foretold weather has already been made.")
(defvar *diary* '() "The trip diary: entries of (day . plist of what happened), the newest first.")
(defvar *insulation* '() "What is done for the winter: a list that may hold :WINDOW and :DOOR.")
(defvar *jars* 4 "The number of empty jars in the crate.")
(defvar *home* nil "The nodes of this file, a plist: :leaf :strips :quilt :jars :can.")

(defparameter *diary-counts* '(:sold :income :picked :gnomes :debt :fuel :spoiled :papers :recycled) "The diary keys that hold a number.")
(defparameter *diary-lists* '(:towns :dishes) "The diary keys that hold a list: of town names, of dish kinds.")

(defun save-home ()
  "Write the home state. The diary is copied: its pages are changed in place, and a save that shares them
with the last one written would look unchanged."
  (unless (net-guest-p)
    (put-save :home (list *calendar-day* (copy-tree (subseq *diary* 0 (min 14 (length *diary*)))) *insulation* *jars* *trip-day0*
                          (loop for (day . text) in *said-log* collect (list day text))))))        ; what we were told: the diary's other half

(defun clean-page (page)
  "Return the diary page PAGE with only what a page may hold, or NIL if it is not a page at all."
  (when (and (fits page '(integer &rest t)) (evenp (length (rest page))))
    (cons (first page)
          (loop for (key value) on (rest page) by #'cddr
                when (or (and (member key *diary-counts*) (integerp value))
                         (and (eq key :towns) (fits value '(:each string)))
                         (and (eq key :dishes) (fits value '(:each keyword))))
                  collect key and collect value))))

(defun load-home ()
  (let ((f (get-save :home)))
    (when (fits f '((:or integer null) (:each t) (:each t) integer &optional integer (:each t)))
      (destructuring-bind (day diary insulation jars &optional (day0 0) said) f
        (setf *said-log* (loop for e in said when (fits e '(integer string)) collect (cons (first e) (second e)))
              *said-n* (length *said-log*) *said-seen* (length *said-log*)        ; what was there at the last launch has been read
              *said-saved* (length *said-log*))
        (setf *calendar-day* day
              *diary* (remove nil (mapcar #'clean-page diary))
              *insulation* (intersection '(:window :door) insulation)
              *jars* (max 0 (min 99 jars))
              *trip-day0* day0)))))

(defstate :home
  :clear (lambda ()
           (setf *said-log* '() *said-n* 0 *said-seen* 0)                      ; a new trip: an empty notebook
           (setf *calendar-day* nil *weather-day* nil *diary* '() *insulation* '() *jars* *start-jars* *can* nil
                 *trip-day0* (today)))                                     ; a new trip starts its count of days today
  :load #'load-home :save #'save-home :show (lambda () (show-home)))

(defun trip-day (&optional (day (today)))
  "Return which day of the trip the game day DAY is, counting from one."
  (1+ (max 0 (- day *trip-day0*))))

;;; ------------------------------------------------------------------ the calendar

(defun season-in (days)
  "Return the season it will be DAYS game days from now."
  (let ((left (- (* *season-days* *day-len*) *season-clock*)))
    (nth (mod (+ (position *season* *seasons*) (if (> (* days *day-len*) left) 1 0)) 4) *seasons*)))

(defun season-days-left ()
  "Return the number of whole game days until the season changes."
  (max 0 (floor (- (* *season-days* *day-len*) *season-clock*) *day-len*)))

(defun day-weather (day &optional (season *season*))
  "Return the weather foretold for the game day DAY in SEASON. The same day always gives the same answer."
  (let ((r (hash2 (* day 7.31) 4.2)))
    (if (eq season :winter)
        (cond ((< r .4) :snow) ((< r .5) :fog) ((< r .7) :cloudy) (t :clear))
        (cond ((< r .3) :rain) ((< r .42) :fog) ((< r .62) :cloudy) (t :clear)))))

(defun leaf-cards (day)
  "Return the two sides of the calendar leaf for DAY: the front and the back."
  (let* ((tomorrow (day-weather (1+ day) (season-in 1)))
         (omens (getf *omens* tomorrow))
         (left (season-days-left)))
    (list (format nil "~a~@[~%~%~a~]"
                  (format nil "~d-й день пути~%~%~a~%~%~a~a"
                          (trip-day day) (tr (nth (mod (1- (trip-day day)) 7) *weekdays*)) (tr (getf *season-names* *season*))
                          (if (zerop left) (tr " — последний день") (format nil (tr " · до смены сезона дней: ~d") left)))
                  (and (holiday) (tr (fourth (holiday)))))            ; a red-letter day
          (format nil "~a~%~%~a"
                  (if *calendar-notes* (fill-text (nth (mod day (length *calendar-notes*)) *calendar-notes*)) "")
                  (if omens (tr (nth (mod day (length omens)) omens)) "")))))

(defun calendar-label ()
  (let ((today (today)))
    (if (and *calendar-day* (>= *calendar-day* today))
        "E — прочитать сегодняшний лист календаря"
        (format nil "E — оторвать лист: на календаре ~d-й день, а идёт ~d-й" (trip-day (or *calendar-day* *trip-day0*)) (trip-day today)))))

(defun use-calendar ()
  "Tear the calendar down to today's leaf and read it."
  (let ((today (today)))
    (unless (and *calendar-day* (>= *calendar-day* today))
      (setf *calendar-day* today)
      (save-home) (show-home))
    (open-show (leaf-cards today) (lambda ()))))

(defun step-weather-omen ()
  "At the start of a day make the weather the calendar foretold for it; the driver alone does it."
  (unless (net-guest-p)
    (let ((day (today)))
      (cond ((or (null *weather-day*) (< day *weather-day*)) (setf *weather-day* day))   ; the first look, or the clock was turned back
            ((> day *weather-day*)
             (setf *weather-day* day)
             (set-weather (day-weather day) 7))))))

;; the season's clock is the driver's: a guest's calendar then counts the same days to the turn of the season
(defcommon :season :value (list *season* (round *season-clock* 60)) :spec '(keyword integer) :driver t
  :accept (lambda (v) (when (member (first v) *seasons*)
                        (unless (eq (first v) *season*) (set-season (first v)))
                        (setf *season-clock* (* 60.0 (second v))))))

;;; ------------------------------------------------------------------ the diary

(defun diary-note (event plist)
  "Write down in today's page of the diary what the event EVENT with fields PLIST means for the trip."
  (when (and (not (net-guest-p)) (boundp '*day-t*)
             (member event '(:sold :arrived :cooked :foraged :gnome :debt :fuel :spoiled :canned :rail :license :recycled)))
    (let* ((day (today))
           (page (or (assoc day *diary*) (first (push (list day) *diary*)))))
      (flet ((add (key n) (setf (getf (cdr page) key) (+ n (getf (cdr page) key 0))))
             (note (key what) (unless (member what (getf (cdr page) key) :test #'equal)
                                (setf (getf (cdr page) key) (append (getf (cdr page) key) (list what))))))
        (case event
          (:sold (add :sold 1) (add :income (or (getf plist :price) 0)))
          ((:arrived :rail) (note :towns (town-name (trade-town))))
          (:cooked (note :dishes (getf plist :dish)))
          (:canned (note :dishes (getf plist :dish)))
          (:foraged (add :picked 1))
          (:gnome (add :gnomes 1))
          (:debt (add :debt (or (getf plist :paid) 0)))
          (:fuel (add :fuel (or (getf plist :pay) 0)))
          (:spoiled (add :spoiled (or (getf plist :n) 0)))
          (:license (add :papers 1))
          (:recycled (add :recycled 1))))
      (save-home))))

(defun diary-page (page)
  "Return the text of one page of the diary: PAGE is (day . plist)."
  (flet ((got (key) (getf (rest page) key)))
    (let ((day (first page)))
      (format nil "~d-й день пути, ~a~%~%~{~a~^~%~}"
              (trip-day day) (tr (nth (mod (1- (trip-day day)) 7) *weekdays*))
              (or (remove nil
                          (list (and (got :towns) (format nil "Были: ~{~a~^, ~}" (got :towns)))
                                (and (got :sold) (format nil "Продали: ~d, выручка ~/kiosk-trip::rub/" (got :sold) (or (got :income) 0)))
                                (and (got :dishes) (format nil "Приготовили: ~{~a~^, ~}"
                                                           (mapcar (lambda (k) (or (second (assoc k *kinds*)) "блюдо")) (got :dishes))))
                                (and (got :picked) (format nil "Собрали у дороги: ~d" (got :picked)))
                                (and (got :recycled) (format nil "Сдали бутылки и макулатуру"))
                                (and (got :fuel) (format nil "Залили бензина на ~/kiosk-trip::rub/" (got :fuel)))
                                (and (got :debt) (format nil "Внесли в счёт долга: ~/kiosk-trip::rub/" (got :debt)))
                                (and (got :papers) (format nil "Получили бумагу в горсовете"))
                                (and (got :gnomes) (format nil "Нашли гномиков: ~d" (got :gnomes)))
                                (and (got :spoiled) (format nil "Испортилось: ~d" (got :spoiled)))))
                  (list (tr "Тихий день: ехали и смотрели в окно")))))))

(defun use-diary ()
  "Open the diary: today's page first, then back through the last week."
  (open-diary))

(defun open-diary ()
  "Open the folder at the bookmark of the diary."
  (unless *docs-open* (open-docs))
  (funcall 'open-paper :diary))

;;; ------------------------------------------------------------------ for the winter: the window and the door

(defun cooling-factor ()
  "Return the share of the winter cooling that gets past what was done for the winter."
  (* (if (member :window *insulation*) *seal-factor* 1.0) (if (member :door *insulation*) *seal-factor* 1.0)))

(defshared toggle-insulation :insulate (what)
  "Seal the window with paper or hang the quilt on the door, or undo it; WHAT is :WINDOW or :DOOR.
The driver does it, on a guest's behalf too: the waste paper is the driver's, and so is the save."
  (cond ((not (member what '(:window :door))) nil)
        ((member what *insulation*)
         (setf *insulation* (remove what *insulation*))
         (say (if (eq what :window) "Отклеили бумагу с окна" "Сняли одеяло с двери")))
        ((and (eq what :window) (not *dev*) (< *papers* *seal-papers*))
         (say (format nil "Нужно ~d связки макулатуры — она лежит у остановок и заправок" *seal-papers*))
         (return-from toggle-insulation nil))
        (t (when (and (eq what :window) (not *dev*)) (decf *papers* *seal-papers*) (save-finds))
           (push what *insulation*)
           (say (if (eq what :window) "Окно заклеено на зиму: кибитка остывает медленнее" "Одеяло на двери: из тамбура не дует"))))
  (save-home) (show-home))

(defcommon :insulation :value (list (and (member :window *insulation*) t) (and (member :door *insulation*) t))
  :spec '((:member t nil) (:member t nil)) :driver t                       ; only the driver changes it: TOGGLE-INSULATION runs there
  :accept (lambda (v)
            (let ((now (remove nil (list (and (first v) :window) (and (second v) :door)))))
              (unless (equal (sort (copy-list now) #'string<) (sort (copy-list *insulation*) #'string<))
                (setf *insulation* now) (show-home)))))

;;; ------------------------------------------------------------------ jars

(defcommon :jars :value *jars* :spec 'integer                              ; the crate of empty jars is one for everyone
  :accept (lambda (n) (setf *jars* (max 0 (min 99 n))) (save-home) (show-home)))

(defvar *can* nil "The jar being closed: (good . turns of the key done), or NIL.")
(defvar *can-dip* 0.0 "The seconds left of the key's turn.")

(defun jar-of (kind)
  "Return what the good KIND becomes in a jar, or NIL: the recipes with the hearth :JAR."
  (second (find-if (lambda (r) (and (eq (first r) kind) (eq (fifth r) :jar))) *recipes*)))

(defun preserved-p (kind)
  "Return true if KIND is food put by for the winter: from a jar or from the dryer."
  (and (find-if (lambda (r) (and (eq (second r) kind) (member (fifth r) '(:jar :line)))) *recipes*) t))

(defun crate-point ()
  "Return where the crate of jars stands: x, height above the floor, z."
  (list (first *washstand-at*) 0.0 (- (second *washstand-at*) .88)))

(defun canning-p ()
  "Return true if what we carry is the good that the open jar waits for."
  (and *can* *carry* (null (cdr *carry*)) (eq (car *carry*) (car *can*))))

(defun jar-label ()
  (cond ((canning-p) (format nil "E — крутить закаточный ключ: ~d из ~d" (cdr *can*) *can-turns*))
        (*can* (format nil "Банка залита кипятком и ждёт: ~a" (good-name (car *can*))))
        ((and *carry* (null (cdr *carry*)) (jar-of (car *carry*)))
         (format nil "E — закатать в банку: ~a · пустых банок ~d" (good-name (jar-of (car *carry*))) *jars*))
        (t (format nil "Пустые банки: ~d. С грибами, яблоками или рыбой в лапах — закатать" *jars*))))

(defun use-jars ()
  "Answer E on the crate of jars: open a jar for what we carry, then a turn of the key per press.
The good stays in the paws until the last turn, when it becomes the closed jar: nothing is laid aside
that could be lost. A jar opened and left waits for the same good."
  (cond ((canning-p)
         (incf (cdr *can*))
         (setf *can-dip* .2)
         (when (audio :steps) (object-call (audio :steps) "play"))
         (when (>= (cdr *can*) *can-turns*)
           (let ((dish (jar-of (car *can*))))
             (setf *can* nil)
             (setf *carry* (cons dish nil) *carry-from* nil *carry-props* '())
             (show-carry) (show-home) (save-interior)
             (log-event :canned :dish dish)
             (say (format nil "Закатано: ~a. Не испортится, а зимой стоит дороже" (second (assoc dish *kinds*)))))))
        (*can* (say (format nil "Банка ждёт: нужно ~a в лапах" (good-name (car *can*)))))
        ((not (and *carry* (null (cdr *carry*)) (jar-of (car *carry*))))
         (say "В банки закатывают грибы, яблоки и рыбу: принеси в лапах"))
        ((<= *jars* 0) (say "Пустых банок нет: их продают в хозтоварах в Красном Куте"))
        ((or (<= *kettle-tea* 0) *kettle-brew*) (say "Нужен кипяток: вскипяти чайник с чистой водой"))
        (t (decf *jars*) (decf *kettle-tea*)
           (setf *can* (cons (car *carry*) 0))
           (show-kettle) (save-home) (show-home)
           (say (format nil "Банка залита кипятком. Крути ключ: E, всего ~d оборотов" *can-turns*)))))

;;; ------------------------------------------------------------------ what is seen and pressed

(defparameter *calendar-at* '(.68 1.5 2.76) "The calendar on the back wall by the door: x, height above the floor, z.")
(defparameter *diary-at* '(.68 1.12 2.76) "The diary on a nail under the calendar.")
(defparameter *roll-at* '(1.46 1.02 1.5) "The roll of paper for sealing the window, on the window wall.")
(defparameter *sheet-at* '(-1.46 1.42 .65) "The supply sheet: a clipboard on the carpet wall above the fridge.")
(defparameter *hook-at* '(-.66 1.72 2.76) "The hook for the door quilt, on the back wall to the right of the door.")

(defun show-home ()
  "Show the calendar leaf, the winter things and the jars as the state has them."
  (destructuring-bind (&key leaf strips quilt jars can) *home*
    (when leaf (godot:object-set leaf "text" (cl:format nil "~d" (trip-day (or *calendar-day* (today))))))
    (dolist (s strips) (object-set s "visible" (and (member :window *insulation*) t)))
    (when quilt (object-set quilt "visible" (and (member :door *insulation*) t)))
    (loop for jar in jars for i from 0 do (object-set jar "visible" (< i *jars*)))
    (when can (object-set can "visible" (and *can* t)))))

(defun jar-model (parent x y z &optional (rgb "#8a6a4a"))
  "Build a glass jar with a tin lid under PARENT with its bottom at (X Y Z); RGB is what is in it."
  (let ((g (group parent x y z)))
    (shape g :cyl '(.05 .05 .11 10) rgb 0 .06 0 :shadow nil)
    (shape g :cyl '(.058 .058 .14 10) "#d8e8f0" 0 .07 0 :alpha .3 :rough .1 :shadow nil)
    (shape g :cyl '(.05 .05 .02 10) "#d8a840" 0 .15 0 :metal .6 :rough .4 :shadow nil)
    g))

(defun make-home (c)
  "Build the calendar, the diary, the winter things and the crate of jars for the wagon C."
  (let ((f *floor*) (strips '()) (jars '()) leaf quilt can)
    ;; the calendar: a card with a picture and a block of leaves; the number on the top leaf is the day
    (destructuring-bind (x y z) *calendar-at*
      (setf leaf (sign c "" x (+ f y -.08) (- z .04) :h .07 :rgb "#2a2a2e" :yaw pi)))
    (let ((x (- *half-width* .085)))
      (dolist (z '(-1.33 1.33)) (push (shape c :box '(.012 1.2 .07) "#f0ece0" x (+ f 1.5) z :shadow nil) strips))
      (dolist (y '(.925 2.075)) (push (shape c :box '(.012 .07 2.66) "#f0ece0" x (+ f y) 0 :shadow nil) strips)))
    ;; the quilt, which hangs on the door itself and swings with it
    (when *door*
      (setf quilt (group *door* .55 -.1 -.07))
      (model quilt "quilt"))
    ;; the crate of jars under the washstand, and the jar that is being closed on top of it
    (destructuring-bind (x y z) (crate-point)
      (let ((g (group c x (+ f y) z)))
        (setf jars (loop for i below 6 collect (jar-model g (- (* (mod i 3) .15) .15) .04 (- (* (floor i 3) .15) .075) "#e8eef0")))
        (setf can (jar-model g 0 .26 0))
        (let ((key (group can 0 .17 0)))                                  ; the seaming key on the lid
          (shape key :cyl '(.065 .065 .02 10) "#8a8e92" 0 0 0 :metal .7 :rough .3 :shadow nil)
          (shape key :box '(.2 .015 .02) "#5a3e26" .08 .02 0 :shadow nil))))
    (setf *home* (list :leaf leaf :strips strips :quilt quilt :jars jars :can can)))
  (show-home))

(defun sheet-towns ()
  "Return the towns of the supply sheet: the one at hand first, then the two nearest by road."
  (let* ((here (trade-town))
         (near (sort (loop for tw in *towns* unless (= (town-index tw) here)
                           collect (cons (or (plan-way (town-stop-d tw)) 1e9) (town-index tw)))
                     #'< :key #'car)))
    (cons here (mapcar #'cdr (subseq near 0 (min 2 (length near)))))))

(defun use-sheet ()
  "Read the supply sheets as the co-operative was told them; a guest reads the same ones (the common thing :PLAN)."
  (log-event :sheet :town (trade-town))
  (open-show (mapcar #'supply-sheet (sheet-towns)) (lambda ())))

(defun home-usables ()
  "Return the things to press for the calendar, the diary, the winter things and the jars."
  (list (usable (first *calendar-at*) (second *calendar-at*) (third *calendar-at*) #'calendar-label #'use-calendar nil 2.6 .15)
        (usable (first *diary-at*) (second *diary-at*) (third *diary-at*) (lambda () "E — дневник поездки") #'use-diary nil 2.6 .13)
        (usable (first *sheet-at*) (second *sheet-at*) (third *sheet-at*)
                (lambda () (format nil "E — ведомость снабжения: ~a и соседи" (town-name (trade-town)))) #'use-sheet nil 2.6 .16)
        (usable (first *roll-at*) (second *roll-at*) (third *roll-at*)
                (lambda () (if (member :window *insulation*) "E — отклеить бумагу с окна"
                               (format nil "E — заклеить окно на зиму: ~d связки макулатуры · у нас ~d" *seal-papers* *papers*)))
                (lambda () (toggle-insulation :window)) nil 2.6 .12)
        (usable (first *hook-at*) (second *hook-at*) (third *hook-at*)
                (lambda () (if (member :door *insulation*) "E — снять одеяло с двери" "E — завесить дверь одеялом на зиму"))
                (lambda () (toggle-insulation :door)) nil 2.6 .12)
        (destructuring-bind (x y z) (crate-point) (usable x (+ y .3) z #'jar-label #'use-jars nil 2.6 .28))))

(defun step-home (delta)
  "Keep the calendar's promises about the weather and turn the seaming key."
  (step-weather-omen)
  (unless (= *said-n* *said-saved*)                                         ; something new was said: the notebook goes into the save
    (setf *said-saved* *said-n*)
    (save-home))
  (unless *calendar-day* (setf *calendar-day* (today)) (show-home))          ; a new trip: the calendar starts at today's leaf
  (when (and *can* (getf *home* :can))
    (setf *can-dip* (max 0.0 (- *can-dip* delta)))
    (node-3d-set-rotation (getf *home* :can) (v3 0 (+ (* (cdr *can*) 1.05) (* -5.0 *can-dip*)) 0))))
