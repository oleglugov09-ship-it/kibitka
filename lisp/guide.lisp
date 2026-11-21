;;;; The guide to the game: a booklet in the documents folder (G while the folder is open; ← → to leaf through).
;;;; The pages are plain text; the tables of goods and booths come from the same lists the game runs on,
;;;; so the booklet does not lie when the numbers and towns change.

(in-package #:kiosk-trip)

(defvar *guide-open* nil)
(defvar *guide-page* 0 "The index of the current spread.")
(defvar *guide-parts* nil)

(defun guide-trade-page ()
  "Return the page text on what is made where (cheap there) and what is short where (expensive there)."
  (format nil "~{~a~%~}"
          (loop for i from 0 below (length *town-trade*)
                collect (format nil "~a: делают ~{~a~^, ~}; ищут ~{~a~^, ~}." (town-name i)
                                (mapcar #'good-name (town-makes i)) (mapcar #'good-name (town-wants i))))))

(defvar *gimmick-notes* '()
  "What each booth is good for, as (index, line for the guide) entries.")

(defun guide-gimmick-page ()
  (format nil "~{~a~%~}"
          (append (loop for (i nil nil nil town) in *gimmick-plan*
                        collect (format nil "~a — ~a." (town-name (or town i)) (second (assoc i *gimmick-notes*))))
                  *guide-booth-extras*)))

(defvar *guide-pages* '()
  "The pages of the guide book, in order: entries of (title body).
The body is the page text, or a keyword for a page the game writes itself from the content tables:
:trade (what is made and wanted where), :booths, :gnomes, :gnomes-2.")

(defvar *guide-booth-extras* '()
  "Extra lines for the :booths page of the guide: places that are special but are not booths.")

(defun guide-pages ()
  "Return the pages of the booklet as (title . text) pairs.
A spread shows two of them."
  (loop for (title body) in *guide-pages*
        collect (cons title (case body
                              (:trade (guide-trade-page)) (:booths (guide-gimmick-page))
                              (:gnomes (guide-gnome-page)) (:gnomes-2 (guide-gnome-page-2))
                              (t body)))))

;;; The papers of the co-operative (bumagi.lisp) are the first pages of the booklet, one page each, and each
;;; has a bookmark along the top of the folder: a digit jumps to it, A and D leaf through as before. The text
;;; of a page is written anew every time it is shown: the numbers on it are the live ones.

(defparameter *paper-tabs*
  '((:waybill "ПУТЕВОЙ ЛИСТ" (.82 .76 .58)) (:loan "ССУДА" (.64 .78 .64)) (:order "НАРЯД" (.88 .66 .44)) (:account "СЧЁТ" (.62 .74 .86))
    (:iou "РАСПИСКА" (.78 .62 .78)) (:obhss "ОБХСС" (.88 .56 .52)) (:talons "ТАЛОНЫ" (.86 .82 .48)))
  "The bookmarks of the papers: the paper, the word on the bookmark, its colour.")
(defvar *tab-parts* '() "The bookmarks built over the folder: a list of (rectangle label).")

(defun paper-page (key)
  "Return the page of the paper KEY as (title . text), with today's numbers."
  (ecase key
    (:waybill
     (cons "ПУТЕВОЙ ЛИСТ"
           (format nil "Автобаза № 3 · кооператив «Кибитка». Грузовик в аренде, выезд разрешён.~%~%Посылок на борту: ~d~%~{~a~%~}~%Куда везти — смотри на посылке и на карте (M). В городе встань у рынка, раздвинь окно и позвони в колокольчик: получатель подойдёт сам.~%~%Бензин — на заправках, R."
                   (reduce #'+ (mapcar #'cdr (parcels-left)))
                   (loop for (town . n) in (parcels-left) collect (format nil "   ~a — ~d" (place-name town) n)))))
    (:loan
     (cons "ССУДНАЯ КНИЖКА"
           (if (net-guest-p)
               (format nil "Сберкасса, ~a.~%~%Ссуда записана на водителя: книжка у него." (town-name 0))
               (format nil "Сберкасса, ~a. Ссуда кооператива «Кибитка».~%~%Взято отцом: ~/kiosk-trip::rub/~%Внесено: ~/kiosk-trip::rub/~%Осталось: ~/kiosk-trip::rub/~%~%Взносы принимает окно сберкассы, сколько есть при себе, не больше ~/kiosk-trip::rub/ за раз. Доехать: карта (M), галочка «К сберкассе».~%~%Выплатишь всё — в том же окне продадут билет к отцу."
                       (town-name 0) *debt-total* (- *debt-total* *debt*) *debt* *debt-chunk*))))
    (:order
     (cons "НАРЯД"
           (format nil "РАДИОГРАММА. Автобаза № 3 — «Кибитке».~%~%Посылки возите исправно — допускаю к госгрузу.~%~%Ящик по наряду берут там, где товара лишнее, и везут туда, где его не хватает. Отправитель с ящиком приходит к окну с вывеской «НАРЯД».~%~%За ящик платят больше, чем за посылку. Довезёте, пока там ещё не хватает, — премия ~/kiosk-trip::rub/~%~%Где чего не хватает — в ведомости над холодильником.~%~%Диспетчер Рычагов"
                   *plan-premium*)))
    (:account
     (cons "СЧЁТ КООПЕРАТИВА"
           (format nil "ИЗВЕЩЕНИЕ сберкассы, ~a.~%~%Кооперативу «Кибитка» открыт расчётный счёт. За госгруз теперь платят не в руки, а переводом на счёт.~%~%На счёте: ~/kiosk-trip::rub/~%~%Бензин и взносы по ссуде списываются со счёта сами. У окна счёт не берут: там только наличные.~%~%Наличными счёт выдают в сберкассе, боковая дверь «ЗАРПЛАТА»: весь, раз в ~d дней. ~a"
                   (town-name 0) *credit* *wage-days*
                   (if (plusp (wage-wait)) (format nil "Следующая выдача — через ~d дн." (wage-wait)) (tr "Выдача — хоть сегодня.")))))
    (:iou
     (cons "РАСПИСКА"
           (if (<= *shark* 0)
               (format nil "Листок из тетради перечёркнут крест-накрест.~%~%«В расчёте. Отцу привет. А. С.»")
               (format nil "Листок из тетради, почерк мелкий.~%~%«Ваш батюшка взял у меня ~/kiosk-trip::rub/ на первый рейс — банк тогда кооперативам не давал. А. С.»~%~%За нами: ~/kiosk-trip::rub/~%Процент: рубль с сотни в день.~%Берёт только наличными: Первомайск, комиссионный.~%~%~a~%~%Три дня без платежа — ночью его люди заберут самый дорогой товар с полки.~@[~%~%~a~]"
                       *shark-total* *shark*
                       (if (net-guest-p) "" (format nil "Последний платёж: ~d дн. назад." (max 0 (- (today) *shark-paid-day*))))
                       (and (paper-p :account) (tr "У боковой двери комиссионного счёт меняют на наличные в любой день: за десять рублей дают семь."))))))
    (:obhss
     (cons "ПРЕДУПРЕЖДЕНИЕ ОБХСС"
           (format nil "Гражданин водитель!~%~%Товар, купленный в магазине по прейскуранту (на нём ценник), перепродавать дороже нельзя: это спекуляция.~%~%Своё — купленное у производителя сверх плана, собранное у дороги, сваренное, высушенное — продавайте по договорной цене.~%~%Сигналов на вас: ~d из ~d~%~%С ~d сигналов остановят на трассе: штраф ~/kiosk-trip::rub/ за каждую перепродажу."
                   *heat* *heat-limit* *heat-limit* *fine-resale*)))
    (:talons
     (cons "ТАЛОНЫ"
           (format nil "Лист с талонами горсовета.~%~%Талонов на руках: ~d~%~%Талон дают за ящик по наряду, довезённый туда, где его ждали.~%~%С талоном продавец магазина отпустит то, чего в городе мало: вывеска «ПО ТАЛОНАМ», один в руки, по прейскуранту. Талон при этом отрывают."
                   *talons*)))))

(defun held-papers () "Return the papers we hold, in the order of their bookmarks." (remove-if-not #'paper-p *folder-papers*))

;;; The diary is one bookmark for two things that used to live apart: the page the trip writes about each
;;; day (home.lisp) and the notebook of everything we were told (interact.lisp). A day is a page, a long day
;;; runs on to the next; the newest is first. What was said since the diary was last closed is new: it
;;; stands at the top of its page in a frame, so that one sees at once what has not been read.

(defparameter *diary-lines* 6 "How many things said fit on one page of the diary.")

(defun diary-pages ()
  "Return the pages of the diary, the newest day first, as plists of :title :body and, when something is unread, :new."
  (let* ((unread (max 0 (min (length *said-log*) (- *said-n* *said-seen*))))
         (marked (loop for e in *said-log* for i from 0 collect (list (car e) (cdr e) (< i unread))))
         (days (remove-duplicates (append (mapcar #'first marked) (mapcar (lambda (p) (trip-day (first p))) *diary*)
                                          (list (trip-day)))))
         (pages '()))
    (dolist (day (sort days #'>) (nreverse pages))
      (let* ((said (remove day marked :key #'first :test-not #'eql))
             (sheet (find day *diary* :key (lambda (p) (trip-day (first p)))))
             (chunks (or (loop for rest on said by (lambda (l) (nthcdr *diary-lines* l))
                               collect (subseq rest 0 (min *diary-lines* (length rest))))
                         (list '()))))
        (loop for chunk in chunks for n from 0
              do (flet ((lines (new) (format nil "~{· ~a~^~%~}" (mapcar #'second (remove new chunk :key #'third :test-not #'eq)))))
                   (let ((fresh (remove nil chunk :key #'third)) (old (remove nil chunk :key #'third :test-not #'eq)))
                     (push (list :title (if (zerop n) (format nil "ДНЕВНИК · день ~d" day) (format nil "ДНЕВНИК · день ~d, дальше" day))
                                 :new (and fresh (lines t))
                                 :body (format nil "~@[~a~%~%~]~@[~a~]"
                                               (and (zerop n) (if sheet (diary-page sheet) (tr "Тетрадь пуста: дорога только началась")))
                                               (and old (format nil "~a~%~a" (tr "Нам говорили:") (lines nil)))))
                           pages))))))))

(defun folder-pages ()
  "Return every page of the folder as a plist of :title :body, with :new (unread text, shown in a frame above the
body) or :frame (the whole page is new): the papers, the diary, then the guide."
  (append (loop for key in (held-papers)
                collect (let ((p (paper-page key))) (list :title (car p) :body (cdr p) :frame (and (member key *papers-new*) t) :paper key)))
          (diary-pages)
          (loop for p in (guide-pages) collect (list :title (car p) :body (cdr p)))))

(defun folder-tabs ()
  "Return the bookmarks of the folder as (key title rgb page): the papers we hold, the diary, the guide itself."
  (let ((papers (length (held-papers))))
    (append (loop for key in (held-papers) for page from 0
                  collect (append (assoc key *paper-tabs*) (list page)))
            (list (list :diary "ДНЕВНИК" '(.90 .88 .80) papers)
                  (list :guide "ПУТЕВОДИТЕЛЬ" '(.80 .78 .72) (+ papers (length (diary-pages))))))))

(defun make-tabs (bg)
  "Build the bookmarks along the top of the folder, on BG: more than are ever needed, shown as papers come."
  (setf *tab-parts*
        (loop for i below (+ 2 (length *paper-tabs*))
              collect (let ((r (hud-rect bg (+ 14 (* i 68)) 0 66 12 '(.8 .78 .72))) (l (hud-label bg "" (+ 16 (* i 68)) 1 6 '(.16 .12 .08))))
                        (list r l)))))

(defun show-tabs ()
  "Lay the bookmarks out: one for each paper we hold and one for the guide; the open one is brighter, a new one has a dot."
  (let ((tabs (folder-tabs)))
    (loop for (r l) in *tab-parts* for i from 0
          do (let ((tab (nth i tabs)))
               (object-set r "visible" (and tab t)) (object-set l "visible" (and tab t))
               (when tab
                 (destructuring-bind (key title rgb page) tab
                   (let ((open (and *guide-open*
                                    (let ((next (fourth (nth (1+ i) tabs))))          ; a bookmark covers its pages up to the next one
                                      (and (<= page (1+ (* 2 *guide-page*))) (or (null next) (> next (* 2 *guide-page*))))))))
                     (object-set r "color" (apply #'color (append (mapcar (lambda (v) (* v (if open 1.0 .78))) rgb) '(1.0))))
                     (object-set r "size" (vector2 66 (if open 14 12)))
                     (object-set l "text" (format nil "~d ~a~:[~; •~]" (1+ i) title (member key *papers-new*))))))))))

(defun open-paper (key)
  "Open the booklet at the bookmark KEY: a paper, :DIARY or :GUIDE."
  (let ((tab (assoc key (folder-tabs))))
    (when tab (setf *guide-page* (floor (fourth tab) 2) *guide-open* t) (show-guide))))

(defvar *papers-read* '() "The new papers whose pages were open since the folder was taken up.")
(defvar *diary-read* nil "True if unread lines of the diary were open since the folder was taken up.")

(defun folder-seen ()
  "The folder is put away: the new papers that were open are new no longer, and neither is what the diary showed."
  (setf *papers-new* (set-difference *papers-new* *papers-read*) *papers-read* '())
  (when *diary-read* (setf *said-seen* *said-n* *diary-read* nil))
  (show-folder-corner))

;;; A new paper is announced in the top right corner of the screen by its own bookmark, the very one to look
;;; for in the folder; it stays until the folder is opened, but not longer than *NOTE-TIME*.

(defparameter *note-time* 25.0 "The seconds the bookmark of a new paper stays in the corner of the screen.")
(defvar *paper-note* nil "The paper announced in the corner right now, as (key . seconds left), or NIL.")
(defvar *note-parts* nil "The nodes of the announcement: a plist of :layer :tab :name :text.")

(defun make-paper-note ()
  (let ((layer (gdext:make-object "CanvasLayer")))
    (object-set layer "layer" 5)
    (node-add-child *root* layer)
    (hud-rect layer 478 6 156 30 '(.06 .06 .07) .82)
    (setf *note-parts* (list :layer layer :tab (hud-rect layer 484 10 70 12 '(.8 .78 .72))
                             :name (hud-label layer "" 487 11 6 '(.16 .12 .08))
                             :text (hud-label layer "" 484 24 6 '(.92 .88 .76))))
    (hud-label layer "новая бумага" 560 11 6 '(.92 .88 .76))
    (object-set layer "visible" nil)))

(defun note-paper (key)
  "Announce the new paper KEY in the corner of the screen, or with NIL take the announcement away."
  (setf *paper-note* (and key (cons key *note-time*)))
  (when (and key *root*)
    (unless *note-parts* (make-paper-note))
    (let ((tab (assoc key (folder-tabs))))
      (when tab
        (object-set (getf *note-parts* :tab) "color" (apply #'color (append (third tab) '(1.0))))
        (object-set (getf *note-parts* :name) "text" (format nil "~d ~a" (1+ (position tab (folder-tabs) :test #'equal)) (second tab)))
        (object-set (getf *note-parts* :text) "text" "в папке под картой: E по папке"))))
  (when *note-parts* (object-set (getf *note-parts* :layer) "visible" (and *paper-note* t))))

(defun step-paper-note (delta)
  (when (and *paper-note* (<= (decf (cdr *paper-note*) delta) 0)) (note-paper nil)))

(defun make-guide (bg)
  "Build the booklet over the desk with papers, on BG: a spread of two pages."
  (let ((box (gdext:make-object "Control")) (ink '(.2 .16 .1)))
    (node-add-child bg box)
    (doc-card box "guide" 20 10 600 340)
    (flet ((page (x) (let* ((frame (loop repeat 4 collect (hud-rect box 0 0 1 1 '(.66 .16 .12))))      ; drawn before the text: under it
                            (mark (hud-label box "НОВОЕ" x 0 6 '(.66 .16 .12)))
                            (title (hud-label box "" x 26 9 '(.42 .12 .1))) (body (hud-label box "" x 46 7 ink))
                            (fresh (hud-label box "" (+ x 4) 50 7 ink)))
                       (dolist (l (list body fresh)) (object-set l "autowrap_mode" 3))
                       (object-set body "size" (vector2 264 280)) (object-set fresh "size" (vector2 256 20))
                       (list title body fresh frame mark x))))
      (setf *guide-parts* (list :box box :left (page 40) :right (page 336)
                                :foot (hud-label box "" 150 330 7 '(.42 .36 .28)))))
    (object-set box "visible" nil)
    (make-tabs bg)))

(defun show-guide ()
  (let* ((pages (folder-pages)) (spreads (ceiling (length pages) 2)))
    (setf *guide-page* (max 0 (min (1- spreads) *guide-page*)))
    (loop for key in '(:left :right) for k from 0
          do (let ((p (nth (+ (* 2 *guide-page*) k) pages)))
               (destructuring-bind (title body fresh frame mark x) (getf *guide-parts* key)
                 (let* ((new (getf p :new)) (whole (getf p :frame)) (top 46) (h 0))
                   (object-set title "text" (or (getf p :title) ""))
                   (object-set fresh "text" (or new "")) (object-set fresh "visible" (and new t))
                   (when new                                                  ; the unread lines: on top, in a frame
                     (object-set fresh "size" (vector2 256 10))
                     (setf h (+ 6 (* (max 1 (object-call fresh "get_line_count")) (+ 3 (object-call fresh "get_line_height" -1)))))
                     (setf top (+ 46 h 12)))
                   (object-set body "position" (vector2 x top))
                   (object-set body "text" (or (getf p :body) ""))
                   (destructuring-bind (x0 y0 w hh) (cond (whole (list (- x 8) 22 280 304)) (new (list (- x 2) 46 268 (+ h 4))) (t '(0 0 0 0)))
                     (loop for r in frame
                           for (rx ry rw rh) in (list (list x0 y0 w 1) (list x0 (+ y0 hh) w 1) (list x0 y0 1 hh) (list (+ x0 w -1) y0 1 hh))
                           do (object-set r "visible" (and (or whole new) t))
                              (object-set r "position" (vector2 rx ry)) (object-set r "size" (vector2 rw rh)))
                     (object-set mark "visible" (and (or whole new) t))
                     (object-set mark "position" (vector2 (+ x0 w -30) (if whole (+ y0 3) (- y0 9)))))
                   (when *guide-open*                                         ; what is open is being read
                     (when (getf p :paper) (pushnew (getf p :paper) *papers-read*))
                     (when new (setf *diary-read* t)))))))
    (object-set (getf *guide-parts* :foot) "text" (format nil "~d из ~d · A и D или стрелки — листать · цифры — закладки · G — закрыть" (1+ *guide-page*) spreads))
    (object-set (getf *guide-parts* :box) "visible" *guide-open*)
    (show-tabs)))

(defun guide-key (code)
  "Handle the key CODE while the folder is open.
Return T when the booklet consumed the key."
  (cond ((and (<= 49 code 57) (nth (- code 49) (folder-tabs)))                                  ; a digit: to that bookmark
         (setf *guide-page* (floor (fourth (nth (- code 49) (folder-tabs))) 2) *guide-open* t) (show-guide) t)
        ((= code 71) (setf *guide-open* (not *guide-open*)) (show-guide) t)                     ; G
        ((not *guide-open*) nil)
        ((member code '(4194319 65)) (decf *guide-page*) (show-guide) t)                        ; ← A
        ((member code '(4194321 68 32)) (incf *guide-page*) (show-guide) t)                     ; → D Space
        (t (setf *guide-open* nil) (show-guide) nil)))                                          ; anything else closes both the booklet and the folder
