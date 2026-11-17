;;;; Quick checks of the rules — without Godot. Plain Lisp loads the game's "pure" files (layout, map,
;;;; terrain, goods, the plan of places, gimmicks, documents), lays out the world with the same seed and
;;;; checks what must hold after any edit: the loop is closed, there is a route from any town to any other,
;;;; fuel lasts from gas station to gas station, the road has no steps, the ground under the lots is level,
;;;; trade makes sense.
;;;; This checks the MODEL: what is drawn and how it looks is for tools/smoke.py and the eyes.
;;;; Usage: ecl --norc --shell tools/unit.lisp   (two or three seconds)

(defpackage #:godot (:use))
(defpackage #:godot-clos (:use))
(defpackage #:gdext (:use) (:export #:make-object #:setf-reference #:add-reference #:release-reference #:singleton))

(defparameter *here* (merge-pathnames "../lisp/" (or *load-truename* *default-pathname-defaults*)))
(load (merge-pathnames "packages.lisp" *here*))
(in-package #:kiosk-trip)

;;; What in the game comes from the files that use the engine: stubs here
(defvar *kinds* '()) (defvar *dalny* 7) (defvar *land* nil) (defvar *time* 0.0) (defvar *dist* 0.0)
(defvar *find-nodes* '()) (defvar *stop-ds* '()) (defvar *observatory* nil) (defvar *depot-d* nil)
(defvar *airmail-d* nil) (defvar *plane* nil) (defvar *client* nil) (defvar *stamp* nil) (defvar *items* (quote ())) (defvar *carry* nil) (defvar *gnome-nodes* (quote ())) (defvar *gnome-told* nil)
(defun reset-vista ()) (defun clear-land ()) (defun clear-roads ())
(defun net-guest-p () nil) (defun put-save (key form) (declare (ignore key)) form) (defun get-save (key) (declare (ignore key)) nil)
(defun say-all (text) text)

(dolist (f '("delo" "tverd" "random" "economy" "land" "karta" "razmetka" "goods" "istorii" "places" "prikol" "bumagi" "debt"))
  (load (merge-pathnames (concatenate 'string f ".lisp") cl-user::*here*)))

;; the content lives in mods: the core ships none. INCLUDE here is plain LOAD from the mod's folder
(defvar *mod-dir* nil)
(defun include (file) (load (merge-pathnames file *mod-dir*)))
(dolist (name '("base" "stories"))
  (let ((*mod-dir* (merge-pathnames (concatenate 'string "../project/mods/" name "/") cl-user::*here*)))
    (include "mod.lisp")))

(defvar *checks* 0) (defvar *fails* '())
(defmacro check (what test)
  `(progn (incf *checks*) (unless (ignore-errors ,test) (push ,what *fails*) (format t "  ПРОВАЛ ~a~%" ,what))))

;;; The world as in the game: seed 1, twelve towns, the plan of places
(setf *trip-seed* 1 *towns* '() *town-upto* 0.0 *town-n* 0)
(plan-towns 1e9)
(plan-places)

(format t "Разметка и карта~%")
(check "двенадцать городов" (= (length *towns*) 12))
(check "города не налезают друг на друга" (loop for (a b) on *towns* while b always (< (town-end a) (town-start b))))
(let ((r (ring)))
  (check "кольцо замкнуто: конец в начале"
         (< (+ (abs (- (aref (edge-xs r) 0) (aref (edge-xs r) (1- (length (edge-xs r))))))
               (abs (- (aref (edge-zs r) 0) (aref (edge-zs r) (1- (length (edge-zs r))))))) .01))
  (check "все города на кольце" (every (lambda (tw) (< (edge-d0 r) (town-start tw) (town-end tw) (edge-d1 r))) *towns*)))
(check "шесть дорог через перекрёсток" (and (= (length *exits*) 3) (= (length *outs*) 3) *hub*))
(check "дороги к перекрёстку сходятся в одной точке"
       (every (lambda (x) (let ((e (cdr x)) ) (< (+ (abs (- (aref (edge-xs e) (1- (length (edge-xs e)))) (first *hub*)))
                                                    (abs (- (aref (edge-zs e) (1- (length (edge-zs e)))) (second *hub*)))) 3.0)))
              *exits*))

(format t "Маршруты~%")
(dolist (a *towns*)
  (setf *dist* (town-stop-d a))
  (dolist (b *towns*)
    (unless (eq a b)
      (let ((len (plan-way (town-stop-d b))))
        (check (format nil "путь ~a → ~a есть и не длиннее круга" (town-index a) (town-index b))
               (and (realp len) (< 0 len (ring-length))))))))
(dolist (a *towns*)                                    ; the roads are two-way: with a U-turn it's no farther than half the loop
  (setf *dist* (town-stop-d a))
  (dolist (b *towns*)
    (unless (eq a b)
      (multiple-value-bind (len hops dir) (plan-way (town-stop-d b))
        (check (format nil "путь ~a → ~a в любую сторону — не дальше полукруга" (town-index a) (town-index b))
               (<= len (+ (/ (ring-length) 2) 50.0)))
        (let ((*dir* dir) (*way* hops))                  ; the autopilot will arrive at the same length following these turns
          (check (format nil "путь ~a → ~a: повороты ведут к цели" (town-index a) (town-index b))
                 (let ((on (way-on (town-stop-d b)))) (and on (< (abs (- on len)) 1.0)))))))))
(setf *dist* (town-stop-d (first *towns*)))
(check "без разворота назад не поедем" (eql 1 (nth-value 2 (plan-way (town-stop-d (car (last *towns*))) nil))))
(check "с разворотом в соседний сзади город — назад" (eql -1 (nth-value 2 (plan-way (town-stop-d (car (last *towns*))) t))))
(let ((*dir* -1))
  (check "едучи назад, с кольца сворачивают только на выезды" (every (lambda (tn) (= (third tn) -1)) (turns-from (ring) -1))))
(setf *dist* 0.0)
(check "улицы в городах почти прямые: на сто метров дорога отклоняется меньше чем на метр"
       (every (lambda (tw)
                (loop for d from (+ (town-start tw) 10.0) below (- (town-end tw) 110.0) by 50.0
                      always (< (abs (- (nth-value 2 (road-point (+ d 100.0))) (nth-value 2 (road-point d)))) .04)))
              *towns*))
(check "срезка через перекрёсток где-то короче кольца"
       (some (lambda (a) (setf *dist* (town-stop-d a))
               (some (lambda (tw) (nth-value 1 (plan-way (town-stop-d tw)))) *towns*))
             *towns*))
(setf *dist* 0.0)

(let ((*dist* 9000.0) (ok t))                           ; wagon ↔ map: there and back, in any lane and on any heading
  (dolist (pose '((1 0.0 0.0) (-1 3.14159 -4.5) (1 1.2 -2.0)))
    (destructuring-bind (*dir* *yaw-now* *lane-now*) pose
      (dolist (p '((0.0 0.0) (7.5 -30.0) (-12.0 44.0) (150.0 -90.0)))
        (multiple-value-bind (wx wz) (cart-world (first p) (second p))
          (multiple-value-bind (lx lz) (cart-local wx wz)
            (unless (and (< (abs (- lx (first p))) .01) (< (abs (- lz (second p))) .01)) (setf ok nil)))))))
  (check "точка у кибитки → карта → обратно даёт ту же точку" ok))
(let ((*dist* 9000.0) (*dir* 1) (*yaw-now* 0.0) (*lane-now* 0.0))
  (destructuring-bind (wx wy wz) (road-pos 9010.0 0.0)
    (declare (ignore wy))
    (multiple-value-bind (lx lz) (cart-local wx wz)
      (check "десять метров вперёд по дороге — это десять метров перед кибиткой" (and (< (abs lx) 1.0) (< (abs (+ lz 10.0)) .5))))))
(format t "Твёрдое~%")
(clear-solids)
(add-solid :дом 100.0 50.0 5.0 2.0 0.0)
(add-solid :сарай 0.0 0.0 4.0 1.0 (/ pi 2))                    ; turned a quarter: stretched along z
(check "в стену не войти" (solid-at-p 103.0 51.0))
(check "рядом со стеной — свободно" (not (solid-at-p 106.0 50.0)))
(check "вплотную к стене не прижаться (запас на плечи)" (solid-at-p 105.2 50.0))
(check "повёрнутый сарай твёрд вдоль z, а не вдоль x" (and (solid-at-p 0.0 3.5) (not (solid-at-p 3.5 0.0))))
(check "большой дом твёрд и вдали от своей середины" (solid-at-p 96.0 49.0))
(drop-solids :дом)
(check "дом убрали — пройти можно, сарай остался" (and (not (solid-at-p 103.0 51.0)) (solid-at-p 0.0 0.0)))
(clear-solids)
(let ((sea (find :port *towns* :key #'town-theme)))            ; water and the cliff
  (when sea
    (let* ((mid (/ (+ (town-start sea) (town-end sea)) 2)) (*dist* mid))
      (destructuring-bind (x y z) (road-pos mid 400.0) (declare (ignore y))
        (check "в море у порта пешком не зайти" (deep-at-p x z)))
      (destructuring-bind (x y z) (road-pos mid 2.0) (declare (ignore y))
        (check "по улице порта ходить можно" (not (deep-at-p x z)))))))
(multiple-value-bind (x z) (road-point *chasm*)
  (check "по настилу моста над ущельем ходить можно" (not (deep-at-p x z))))
(format t "Бензин~%")
(let* ((tank (* *tank-km* 1000)) (stations (sort (loop for tw in *towns* append (town-stations tw)) #'<)))
  (check "по кольцу от заправки до заправки хватает бака"
         (loop for (a b) on (append stations (list (+ (first stations) (ring-length)))) while b always (< (- b a) tank)))
  (check "через перекрёсток хватает бака: заправка перед ним есть на каждой дороге"
         (every (lambda (x) (find-if (lambda (s) (< (edge-d0 (cdr x)) s (edge-d1 (cdr x)))) *spoke-stations*)) *exits*))
  (check "от заправки у перекрёстка до заправки на кольце хватает бака"
         (every (lambda (o) (let ((next (find-if (lambda (s) (> s (edge-next o))) stations)))
                              (< (+ 150 (edge-len o) (- (or next (+ (first stations) (ring-length))) (edge-next o))) tank)))
                *outs*)))

(format t "Дорога и земля~%")
(check "дорога без ступенек: не круче метра на два метра пути"
       (loop for d from 0 below 25000 by 2 always (< (abs (- (road-y (+ d 2.0)) (road-y d))) 1.0)))
(check "под заправками, будками и закусочными дорога ровная"
       (every (lambda (d) (< (abs (- (road-y (+ d 10)) (road-y (- d 10)))) .02))
              (append (loop for tw in *towns* append (town-stations tw)) (mapcar #'first *gimmicks*)
                      (loop for (i f) in *diner-plan* collect (gap-d i f)))))
(check "в городах улица ровная"
       (every (lambda (tw) (< (abs (- (road-y (+ (town-start tw) 50)) (road-y (- (town-end tw) 50)))) .02)) *towns*))
(check "съезды и выезды на высоте кольца"
       (and (every (lambda (x) (< (abs (- (road-y (car x)) (road-y (+ (edge-d0 (cdr x)) 1.0)))) .3)) *exits*)
            (every (lambda (o) (< (abs (- (road-y (edge-next o)) (road-y (- (edge-d1 o) 1.0)))) .3)) *outs*)))
(check "все дороги у перекрёстка на одной высоте"
       (let ((ys (append (mapcar (lambda (x) (road-y (- (edge-d1 (cdr x)) 1.0))) *exits*) (mapcar (lambda (o) (road-y (+ (edge-d0 o) 1.0))) *outs*))))
         (< (- (reduce #'max ys) (reduce #'min ys)) .05)))
(check "земля без разрывов: в метре рядом высота почти та же (вне моста и туннеля)"
       (loop for i below 600
             always (let* ((d (* 41.0 i)) (lat (- (* 37 (mod i 17)) 300)))
                      (or (bare-at d) (bare-at (+ d 60)) (bare-at (- d 60))
                          (multiple-value-bind (px pz th) (road-point d)
                            (let ((x (+ px (* lat (cos th)))) (z (+ pz (* lat (sin th)))))
                              (< (abs (- (land-height (+ x 1.0) z) (land-height x z))) 3.6)))))))      ; 3.1 m — the embankment wall in the river town
(check "под полотном земля не выше дороги"
       (loop for d from 100 below 25000 by 53
             always (or (deck-at d) (multiple-value-bind (px pz) (road-point d) (<= (land-height px pz) (+ (road-y d) .02))))))
(check "на остановках и заправках нет ни моста, ни туннеля" (notany #'bare-at (append *stop-ds* (loop for tw in *towns* append (town-stations tw)))))

(format t "Торговля~%")
(let ((made (remove-duplicates (loop for i below 12 append (town-makes i))))
      (wanted (remove-duplicates (loop for i below 12 append (town-wants i)))))
  (check "всё, что ищут, где-то делают" (subsetp wanted made))
  (check "всё, что делают, где-то ищут" (subsetp made wanted))
  (check "город не ищет то, что сам делает" (loop for i below 12 never (intersection (town-makes i) (town-wants i))))
  (check "у каждого товара есть цена и имя" (every (lambda (k) (and (second (assoc k *goods*)) (plusp (good-base k)))) made)))
(check "везти выгодно: покупатель всегда даёт больше, чем просит продавец" (> (first *buy-range*) (second *sell-range*)))
(check "насыщение не роняет цену ниже пола"
       (let ((*glut* (list (cons (list 3 :milk :sold) 50.0)))) (>= (glut-factor :milk :buyer 3) *glut-floor*)))
(check "насыщенный рынок всё ещё не убыточен для первой сделки" (> (* (first *buy-range*) *glut-floor*) .5))

(format t "Форма данных~%")
(check "fits: кортеж нужной длины и типов" (and (fits '(1 2.5 "a" :k) '(integer real string keyword)) (not (fits '(1 2) '(integer real string)))))
(check "fits: не тот тип, не список, точечная пара — нет" (and (not (fits '(1 "x") '(integer integer))) (not (fits 5 '(integer))) (not (fits '(1 . 2) '(integer integer)))))
(check "fits: необязательный хвост" (and (fits '(1 2) '(t t &optional t t)) (fits '(1 2 3 4) '(t t &optional t t)) (not (fits '(1) '(t t &optional t t))) (not (fits '(1 2 3 4 5) '(t t &optional t t)))))
(check "fits: каждый элемент и вложенность" (and (fits '(3.0 "x" ((1 2 3) (4 5 6))) '(real string (:each (real real real)))) (fits '() '(:each integer)) (not (fits '(1 :a) '(:each integer)))))
(check "fits: одно из значений, в том числе NIL" (and (fits '(1 nil) '((:member 1 -1) (:member nil :fire))) (not (fits '(2 nil) '((:member 1 -1) t)))))
(check "fits: хвост любой длины" (and (fits '(1 :a :b) '(integer &rest keyword)) (not (fits '(1 :a 2) '(integer &rest keyword)))))

(format t "Истории~%")
(check "у каждой истории каждый шаг записан целиком, города настоящие"
       (every (lambda (story)
                (every (lambda (s) (and (= (length s) 8) (<= 0 (first s) 11) (<= 0 (second s) 11) (/= (first s) (second s))
                                        (every #'stringp (list (third s) (fourth s) (fifth s) (sixth s) (seventh s)))
                                        (= (length (eighth s)) 3) (<= 0 (first (eighth s)) 11)))
                       (rest story)))
              *stories*))
(check "реплики годятся для сообщений по сети: без кавычек-лапок и обратной черты"
       (every (lambda (story) (every (lambda (s) (notany (lambda (text) (and (stringp text) (find-if (lambda (c) (find c "\"\\")) text)))
                                                         (append s (eighth s))))
                                     (rest story)))
              *stories*))
(let ((*story-at* '()) (*story-transit* '()) (*story-after* '()) (*news* '()))
  (let ((s (story-sender 4)))
    (check "в Берёзовке ждёт отправитель истории — посылка в Ленинск" (and s (= (first s) 3) (eq (fourth s) :jam)))
    (check "в городе без историй отправитель обычный" (null (story-sender 10)))
    (story-taken (fourth s) (fifth s))
    (check "посылка в пути — второй такой же не дают" (null (story-sender 4)))
    (check "обычная посылка в тот же город — история молчит" (null (story-delivered 3)))
    (check "адрес посылки истории помнит город, отправителя и получателя"
           (let ((a (parcel-addr 3 :jam 0))) (and (= (parcel-town a) 3) (search "Зина" (parcel-from a)) (search "Гриша" (parcel-to a)))))
    (check "вручили ту самую посылку — получатель отвечает" (search "Гриша" (story-delivered (parcel-addr 3 :jam 0))))
    (check "после вручения есть сводка для радио и знакомый в Берёзовке" (and (story-news) (search "Зина" (story-regular 4))))
    (check "знакомый спрашивает один раз" (null (story-regular 4)))
    (check "история идёт дальше: теперь посылку шлют из Ленинска" (let ((n (story-sender 3))) (and n (= (first n) 4))))))
(format t "Приколюхи и документы~%")
(check "части состояния объявлены и у каждой есть очистка, чтение и запись"
       (and *state-parts* (every (lambda (p) (and (second p) (third p) (fourth p))) *state-parts*)))
(check "имена общих дел годятся для сообщений: ключевое слово до 23 знаков из букв и дефиса"
       (and *shared-ops*
            (every (lambda (op) (let ((name (symbol-name (car op))))
                                  (and (keywordp (car op)) (<= 1 (length name) 23) (fboundp (cdr op))
                                       (every (lambda (c) (or (alpha-char-p c) (char= c #\-))) name))))
                   *shared-ops*)))
(let ((*items* '()) (*carry* nil) (*licenses* '()))
  (flet ((coal (n) (setf *items* (loop repeat n collect (cons (list :coal 0 0 0 0) nil)))))
    (coal *cargo-free*) (check "пара ящиков угля — не опт, штрафа нет" (and (null (bulk-cargo)) (null (violations))))
    (coal (1+ *cargo-free*)) (check "уголь оптом без лицензии — штраф" (= (length (violations)) 1))
    (let ((*carry* (cons :coal nil))) (coal *cargo-free*) (check "ящик в руках тоже считается" (= (length (violations)) 1)))
    (coal 5) (let ((*licenses* '(:coal))) (check "уголь оптом с лицензией — без штрафа" (and (bulk-cargo) (null (violations)))))))
(check "будка стоит в своём городе"
       (every (lambda (g) (some (lambda (tw) (< (town-start tw) (first g) (town-end tw))) *towns*)) *gimmicks*))
(check "будки не стоят вплотную" (loop for (a b) on (sort (mapcar #'first *gimmicks*) #'<) while b always (> (- b a) 20)))
(check "сберкасса стоит в первом городе" (let ((g (find 14 *gimmicks* :key #'second))) (and g (eq (town-at (first g)) (first *towns*)))))
(let ((*debt* *debt-total*) (*story-stage* :owing))
  (check "взнос не больше остатка долга" (and (= (debt-share) *debt-chunk*) (let ((*debt* 7)) (= (debt-share) 7))))
  (pay-debt *debt-chunk*)
  (check "после взноса долг меньше, история та же" (and (= *debt* (- *debt-total* *debt-chunk*)) (eq *story-stage* :owing)))
  (pay-debt *debt*)
  (check "долг выплачен — открыт выезд" (and (zerop *debt*) (eq *story-stage* :free))))
(let ((*commons* '()) (lamp nil) (count 0))
  (defcommon :lamp :value lamp :spec '(:member t nil) :accept (lambda (v) (setf lamp v)))
  (defcommon :count :value count :spec 'integer :driver t :accept (lambda (v) (setf count v)))
  (check "общие вещи уходят списком по порядку объявления" (equal (common-state) '(nil 0)))
  (accept-common '(t 5))
  (check "гость принимает всё" (and (eq lamp t) (= count 5)))
  (accept-common '(nil 9) t)
  (check "водитель не берёт у гостя то, что решает сам" (and (null lamp) (= count 5)))
  (accept-common '(:junk 7))
  (check "значение не той формы отброшено, соседнее принято" (and (null lamp) (= count 7)))
  (accept-common '(t))
  (check "список не той длины отброшен целиком" (null lamp))
  (defcommon :lamp :value lamp :spec '(:member t nil) :accept (lambda (v) (setf lamp v)))
  (check "повторное объявление не плодит записи" (= (length *commons*) 2)))
(check "у вступления и титров есть текст" (and *intro-cards* *credits* (every #'stringp *intro-cards*) (every #'stringp *credits*)))
(check "на каждый запрещённый товар есть задание с лицензией"
       (every (lambda (r) (find (second r) *quests* :key #'second)) *restricted*))
(check "запрещённые товары существуют" (every (lambda (r) (assoc (first r) *goods*)) *restricted*))

(format t "~%~d проверок, ~:[всё сошлось~;провалов: ~:*~d~]~%" *checks* (and *fails* (length *fails*)))
(ext:quit (if *fails* 1 0))
