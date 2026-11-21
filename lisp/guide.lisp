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

(defun folder-tabs ()
  "Return the bookmarks of the folder as (key title rgb page): the papers we hold, then the guide itself."
  (append (loop for key in (held-papers) for page from 0
                collect (append (assoc key *paper-tabs*) (list page)))
          (list (list :guide "ПУТЕВОДИТЕЛЬ" '(.80 .78 .72) (length (held-papers))))))

(defun make-tabs (bg)
  "Build the bookmarks along the top of the folder, on BG: more than are ever needed, shown as papers come."
  (setf *tab-parts*
        (loop for i below (1+ (length *paper-tabs*))
              collect (let ((r (hud-rect bg (+ 22 (* i 75)) 0 73 12 '(.8 .78 .72))) (l (hud-label bg "" (+ 25 (* i 75)) 1 6 '(.16 .12 .08))))
                        (list r l)))))

(defun show-tabs ()
  "Lay the bookmarks out: one for each paper we hold and one for the guide; the open one is brighter, a new one has a dot."
  (let ((tabs (folder-tabs)))
    (loop for (r l) in *tab-parts* for i from 0
          do (let ((tab (nth i tabs)))
               (object-set r "visible" (and tab t)) (object-set l "visible" (and tab t))
               (when tab
                 (destructuring-bind (key title rgb page) tab
                   (let ((open (and *guide-open* (if (eq key :guide) (>= *guide-page* (floor page 2)) (= (floor page 2) *guide-page*)))))
                     (object-set r "color" (apply #'color (append (mapcar (lambda (v) (* v (if open 1.0 .78))) rgb) '(1.0))))
                     (object-set r "size" (vector2 73 (if open 14 12)))
                     (object-set l "text" (format nil "~d ~a~:[~; •~]" (1+ i) title (member key *papers-new*))))))))))

(defun open-paper (key)
  "Open the booklet at the page of the paper KEY."
  (let ((page (position key (held-papers))))
    (when page (setf *guide-page* (floor page 2) *guide-open* t) (show-guide))))

(defun make-guide (bg)
  "Build the booklet over the desk with papers, on BG: a spread of two pages."
  (let ((box (gdext:make-object "Control")) (ink '(.2 .16 .1)))
    (node-add-child bg box)
    (doc-card box "guide" 20 10 600 340)
    (flet ((page (x) (let ((title (hud-label box "" x 26 9 '(.42 .12 .1))) (body (hud-label box "" x 46 7 ink)))
                       (object-set body "autowrap_mode" 3)
                       (object-set body "size" (vector2 264 280))
                       (list title body))))
      (setf *guide-parts* (list :box box :left (page 40) :right (page 336)
                                :foot (hud-label box "" 150 330 7 '(.42 .36 .28)))))
    (object-set box "visible" nil)
    (make-tabs bg)))

(defun show-guide ()
  (let* ((papers (held-papers)) (pages (append (mapcar #'paper-page papers) (guide-pages))) (spreads (ceiling (length pages) 2)))
    (setf *guide-page* (max 0 (min (1- spreads) *guide-page*)))
    (when *guide-open*                                                       ; the papers on this spread have been seen
      (dolist (key (subseq papers (min (length papers) (* 2 *guide-page*)) (min (length papers) (+ 2 (* 2 *guide-page*)))))
        (setf *papers-new* (remove key *papers-new*)))
      (show-folder-corner))
    (loop for key in '(:left :right) for k from 0
          do (let ((p (nth (+ (* 2 *guide-page*) k) pages)))
               (destructuring-bind (title body) (getf *guide-parts* key)
                 (object-set title "text" (if p (car p) ""))
                 (object-set body "text" (if p (cdr p) "")))))
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
