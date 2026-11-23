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
(defvar *calendar-notes* '()) (defvar *omens* '()) (defvar *jars* 0)
(defvar *lid* nil) (defvar *papers* 0) (defvar *dev* nil)
(defun reset-vista ()) (defun drop-mega ()) (defun forget-find-nodes ()) (defun forget-gnome-nodes ()) (defun clear-land ()) (defun clear-roads ())
(defun net-guest-p () nil) (defun put-save (key form) (declare (ignore key)) form) (defun get-save (key) (declare (ignore key)) nil)
(defun say-all (text) text) (defun log-event (&rest r) (declare (ignore r)))

(dolist (f '("delo" "tverd" "random" "economy" "kassa" "land" "karta" "razmetka" "goods" "plan" "istorii" "places" "prikol" "bumagi" "debt" "people" "zagranitsa"))
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
  (check "всё, что ищут, где-то делают или оно растёт у дороги" (subsetp wanted (append made (mapcar #'first *forage*))))
  (check "всё, что растёт, имеет цену и имя" (every (lambda (e) (and (second (assoc (first e) *goods*)) (plusp (good-base (first e))))) *forage*))
  (check "всё съедобное, что растёт, где-то ищут или оно идёт в чайник"
         (every (lambda (e) (or (eq (first e) :toadstool) (member (first e) wanted) (assoc (first e) *brews*))) *forage*))
  (check "у каждого блюда из рецептов есть сырьё с ценой" (every (lambda (r) (assoc (first r) *goods*)) *recipes*))
  (check "второй продукт рецепта — тоже товар" (every (lambda (r) (or (null (fourth r)) (assoc (fourth r) *goods*))) *recipes*))
  (check "заварки для чайника — товары или блюда" (every (lambda (b) (or (assoc (first b) *goods*) (dish-p (first b)))) *brews*))
  (check "всё, что делают, где-то ищут" (subsetp made wanted))
  (check "город не ищет то, что сам делает" (loop for i below 12 never (intersection (town-makes i) (town-wants i))))
  (check "у каждого товара есть цена и имя" (every (lambda (k) (and (second (assoc k *goods*)) (plusp (good-base k)))) made)))


(format t "План~%")
(reset-plan 0)
(let ((short-by-day '()) (surplus-by-day '()) (cargo-by-day '()) (first-short (loop for tw below 12 collect (short-goods tw))))
  (loop for day from 0 below 30
        do (run-plan-to (1+ day))
           (push (loop for tw below 12 sum (length (short-goods tw))) short-by-day)
           (push (loop for tw below 12 sum (length (surplus-goods tw))) surplus-by-day)
           (push (loop for tw below 12 count (plan-cargo tw)) cargo-by-day))
  (format t "  нехватки по дням: ~{~d~^ ~}~%  излишки по дням:  ~{~d~^ ~}~%  городов с нарядом: ~{~d~^ ~}~%"
          (reverse short-by-day) (reverse surplus-by-day) (reverse cargo-by-day))
  (dotimes (tw 3) (format t "  ~a~%" (substitute #\Space #\Newline (supply-sheet tw))))
  (check "каждый день где-то чего-то не хватает" (every (lambda (n) (>= n 8)) short-by-day))
  (check "каждый день где-то есть излишки" (every (lambda (n) (>= n 6)) surplus-by-day))
  (check "наряды есть хотя бы в трёх городах в любой день" (every (lambda (n) (>= n 3)) cargo-by-day))
  (check "нехватка не всеобщая: план в целом почти сходится" (every (lambda (n) (<= n 70)) short-by-day))
  (check "за месяц расклад нехваток меняется больше чем в половине городов"
         (>= (loop for tw below 12 for was in first-short count (not (equal was (short-goods tw)))) 7))
  (check "договорная цена не ниже прейскуранта и не выше тройной"
         (loop for tw below 12 always (every (lambda (g) (<= (list-price g) (agreed-price tw g) (* 3 (list-price g)))) (plan-goods))))
  (check "ящик по наряду закрывает дыру и даёт премию"
         (let* ((tw (position-if #'plan-cargo (loop for i below 12 collect i))) (cargo (plan-cargo tw))
                (before (stock (first cargo) (second cargo))))
           (and (= (deliver-cargo (plan-addr (first cargo) (second cargo))) *plan-premium*)
                (= (stock (first cargo) (second cargo)) (min *stock-cap* (+ before *crate-units*)))
                (eq (parcel-good (plan-addr 3 :bread)) :bread) (null (parcel-good 3)) (null (parcel-good 1203))))))

(let ((*talons* 0) (*deeds* '()))
  (reset-plan 0)
  (let* ((tw (position-if #'plan-cargo (loop for i below 12 collect i))) (cargo (plan-cargo tw)))
    (deliver-cargo (plan-addr (first cargo) (second cargo)))
    (check "за ящик, закрывший дыру, горсовет даёт талон; своим два, в ссоре ни одного"
           (and (= *talons* 1) (= (let ((*deeds* (list (cons 3 *deeds-friend*)))) (crate-talons 3)) 2)
                (= (let ((*deeds* (list (cons 3 *deeds-feud*)))) (crate-talons 3)) 0))))
  (check "по талонам — то, чего мало, но что ещё есть на складе"
         (loop for tw below 12 always (every (lambda (g) (and (>= (shortage tw g) .25) (>= (stock tw g) 1) (state-good-p g))) (rationed-goods tw))))
  (format t "  городов, где что-то отпускают по талонам: ~d из 12~%" (loop for tw below 12 count (rationed-goods tw))))

(check "у каждого сезона свой расход, и товары в таблице настоящие"
       (and (every (lambda (s) (getf *season-use* s)) '(:winter :spring :summer :autumn))
            (loop for (nil entries) on *season-use* by #'cddr always (every (lambda (e) (and (assoc (first e) *goods*) (plusp (second e)))) entries))))
(let ((*season* :winter))
  (check "зимой угля уходит больше нормы, воды меньше, рыбы — по норме"
         (and (> (day-use 3 :coal) (norm 3 :coal)) (< (day-use 3 :water) (norm 3 :water)) (= (day-use 3 :fish) (norm 3 :fish)))))
(flet ((coal-short (season)
         (let ((*season* season)) (reset-plan 0) (loop for d from 1 to 6 do (run-plan-day d))
           (loop for tw below 12 sum (shortage tw :coal)))))
  (let ((winter (coal-short :winter)) (summer (coal-short :summer)))
    (format t "  нехватка угля по всем городам за шесть дней: зимой ~,1f, летом ~,1f~%" winter summer)
    (check "зимой угля не хватает сильнее, чем летом" (> winter summer))))
(let ((by-year '()))
  (reset-plan 0)
  (dotimes (year 3)
    (push (loop for season in '(:spring :summer :autumn :winter) for q from 0
                collect (let ((*season* season))
                          (loop for d from 1 to 2 do (run-plan-day (+ d (* 2 q) (* 8 year))))
                          (loop for tw below 12 sum (length (short-goods tw)))))
          by-year))
  (format t "  нехваток к концу сезона (весна лето осень зима), три года подряд: ~{~{~d~^ ~}~^ · ~}~%" (reverse by-year))
  (check "сезонный расход не копится из года в год: на третий год нехваток не больше, чем в первый, плюс десять"
         (every (lambda (first third) (<= third (+ first 10))) (third by-year) (first by-year))))
(check "праздники: сезон и день есть у каждого, товары настоящие, ключи разные"
       (and (>= (length *holidays*) 4)
            (every (lambda (h) (and (member (first h) '(:winter :spring :summer :autumn)) (member (second h) '(0 1)) (keywordp (third h))
                                    (stringp (fourth h)) (every (lambda (g) (assoc g *goods*)) (fifth h))))
                   *holidays*)
            (= (length *holidays*) (length (remove-duplicates *holidays* :key #'third)))))
(let ((*season* :winter) (*holidays* '((:winter 0 :test "Проверка" (:honey)))))
  (check "в праздник за праздничный товар дают больше"
         (> (trade-price :honey :buyer 3) (let ((*holidays* '())) (trade-price :honey :buyer 3)))))
(reset-plan 0)

(check "свойства товара читаются из его записи в моде, свойства блюд — из своей таблицы"
       (and (trait :milk :spoils) (not (trait :bread :spoils)) (= (trait :cans :stack) 3) (= (trait :jam :stack) 2)
            (stringp (trait :fish :cut)) (stringp (trait :fried :cut)) (null (trait :coal :cut))
            (cuttable-p :bread) (not (cuttable-p :fried)) (stringp (cut-look :fried))))
(let ((*goods* (append *goods* '((:soap "мыло" 6 :stack 4 :size (.1 .05 .07) :rgb "#e8d8c0" :title "мыло")))))
  (check "товар из мода без модели и без правок ядра: цена, свойства, размер"
         (and (= (good-base :soap) 6) (= (trait :soap :stack) 4) (equal (trait :size :soap) nil) (equal (trait :soap :size) '(.1 .05 .07)))))
(check "ящик по наряду помнит товар по имени: для каждого товара адрес читается обратно, и номера не совпадают"
       (and (every (lambda (g) (eq (parcel-good (plan-addr 7 (first g))) (first g))) *goods*)
            (every (lambda (g) (= (mod (plan-addr 7 (first g)) 100) 7)) *goods*)
            (null (number-clashes))))
(let* ((addr (plan-addr 3 :honey))
       (*goods* (append (list '(:soap "мыло" 6)) (subseq *goods* 0 4) (list '(:nails "гвозди" 3)) (subseq *goods* 4))))
  (check "товары мода вставлены в начало и в середину списка — ящик на дороге всё ещё с мёдом"
         (and (eq (parcel-good addr) :honey) (eq (parcel-good (plan-addr 3 :soap)) :soap))))
(check "ящик, собранный по-старому (по месту в списке), всё ещё открывается; простая посылка — не груз"
       (and (eq (parcel-good (+ 3 (* 100 (+ 900 2)))) (first (nth 2 *goods*))) (null (parcel-good 3)) (null (parcel-good 1203))))
(check "посылка истории помнит историю по имени: адрес читается обратно, старые адреса тоже"
       (and (every (lambda (s) (let ((got (parcel-story (parcel-addr 5 (first s) 0)))) (and got (eq (first got) (first s)) (eql (second got) 0))))
                   *stories*)
            (let ((old (parcel-story (+ 5 (* 100 (+ 1 (* 10 1) 0)))))) (and old (eq (first old) (first (second *stories*)))))
            (let* ((addr (parcel-addr 5 (first (third *stories*)) 1)) (*stories* (cons '(:new-story) *stories*)))
              (eq (first (parcel-story addr)) (first (fourth *stories*))))
            (= (parcel-town (parcel-addr 5 (first (first *stories*)) 2)) 5)))
(check "таблицы, переехавшие в мод, на месте"
       (and (= (length *weekdays*) 7) *shop-names* *slogans* *start-interior* *restricted* *camp-food* *shift-themes* *shift-hours*
            (= (length *plank-price*) 2) (= (length *jar-deal*) 2) (= (length *carpenter-deal*) 2) *quest-money*
            (< .99 (loop for (nil . share) in *client-mix* sum share) 1.01)))
(check "у каждого города есть сила, и соперники названы взаимно"
       (and (= (length *forces*) 12)
            (every (lambda (f) (or (null (fourth f)) (eql (fourth (assoc (fourth f) *forces*)) (first f)))) *forces*)))
(let ((*deeds* '()))
  (check "чужой, пока дел мало" (eq (standing 2) :stranger))
  (dotimes (i *deeds-friend*) (add-deeds 2 1))
  (check "восемь дел — и свой; сопернику это записано в минус"
         (and (eq (standing 2) :friend) (= (deeds 4) (- *rival-cost*)) (eq (standing 4) :stranger)))
  (add-deeds 4 -3)
  (check "дальше в минус — ссора" (eq (standing 4) :feud))
  (check "своим продают дешевле и берут дороже, поссорившимся наоборот"
         (and (< (standing-factor 2 :seller) 1 (standing-factor 2 :buyer)) (> (standing-factor 4 :seller) 1 (standing-factor 4 :buyer))
              (zerop (standing-factor 4 :premium)))))
(check "свойства пишутся в одном порядке, лишнее и ложное выпадает"
       (and (equal (clean-props '(:shop t :junk 1 :cut t)) '(:cut t :shop t)) (null (clean-props '(:cut nil)))
            (props-ok-p '(:cut t)) (props-ok-p '()) (not (props-ok-p '(:shop t :cut t))) (not (props-ok-p '(:cut))) (not (props-ok-p 5))))
(check "запись вещи: адрес и свойства только когда есть"
       (and (equal (make-spec :bread 1 0 2 0) '(:bread 1 0 2 0)) (equal (make-spec :crate 1 0 2 1 7) '(:crate 1 0 2 1 7))
            (equal (make-spec :bread 1 0 2 0 nil '(:shop t)) '(:bread 1 0 2 0 nil (:shop t)))
            (spec-cut-p (make-spec :fish 1 0 2 0 nil '(:cut t))) (not (spec-cut-p '(:fish 1 0 2 0)))
            (spec-prop '(:bread 1 0 2 0 nil (:shop t)) :shop)))
(check "старая пометка «нарезано» поворотом читается как свойство, мебель с тем же поворотом — нет"
       (and (equal (old-cut-spec '(:fish 1 0 2 2)) '(:fish 1 0 2 0 nil (:cut t))) (equal (old-cut-spec '(:crate 1 0 2 2)) '(:crate 1 0 2 2))
            (equal (old-cut-spec '(:bread 1 0 2 0)) '(:bread 1 0 2 0))))
(let ((*shark* 10000) (*shark-day* 0) (*shark-paid-day* 0))
  (grow-shark 10)
  (check "частный долг растёт на процент в день" (< 11000 *shark* 11100))
  (check "три дня без платежа — просрочка" (and (shark-overdue-p 3) (not (shark-overdue-p 2)))))

(format t "Форма данных~%")
(check "длинный текст из байтов читается: 4192 знака и больше, русский и латиница"
       (loop for n in '(0 1 4191 4192 4193 5000 20000)
             always (loop for sample in (list "a" "ж" "aж—«1 €")
                          always (let ((text (make-string n)))
                                   (dotimes (i n) (setf (char text i) (char sample (mod i (length sample)))))
                                   (equal text (utf8-text (ext:string-to-octets text :external-format :utf-8)))))))
(check "латиница из байтов остаётся простой строкой" (typep (utf8-text (ext:string-to-octets "save.sexp" :external-format :utf-8)) 'simple-base-string))
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
  (check "взнос не больше остатка долга и не больше кошелька"
         (and (let ((*money* 1000000)) (= (debt-share) *debt-chunk*))
              (let ((*money* 1000000) (*debt* 7)) (= (debt-share) 7))
              (let ((*money* 30)) (= (debt-share) 30))))
  (check "деньги пишутся рублями и копейками"
         (and (equal (money-text 5) "25 коп.") (equal (money-text 48) "2 р. 40 коп.") (equal (money-text 200000) "10000 р.")
              (equal (format nil "за ~/kiosk-trip::rub/" 20) "за 1 р.")))
  (pay-debt *debt-chunk*)
  (check "после взноса долг меньше, история та же" (and (= *debt* (- *debt-total* *debt-chunk*)) (eq *story-stage* :owing)))
  (pay-debt *debt*)
  (check "долг выплачен — открыт выезд" (and (zerop *debt*) (eq *story-stage* :free))))
(flet ((saved (form) (let ((*debt* 0) (*story-stage* :intro))
                       (setf (symbol-function 'get-save) (lambda (key) (declare (ignore key)) form))
                       (load-debt)
                       (setf (symbol-function 'get-save) (lambda (key) (declare (ignore key)) nil))
                       (list *debt* *story-stage*))))
  (check "старое сохранение с долгом 300: уплаченное засчитано в новый долг"
         (equal (saved '(275 :owing)) (list (- *debt-total* 25) :owing)))
  (check "сохранение с нынешним долгом читается как есть"
         (equal (saved (list 4200 :owing *debt-total*)) '(4200 :owing)))
  (check "кто уже расплатился, тому новый долг не вешают" (equal (saved '(0 :done)) '(0 :done))))
(let ((*commons* '()) (*common-clocks* (make-hash-table)) (lamp nil) (count 0))
  (defcommon :lamp :value lamp :spec '(:member t nil) :accept (lambda (v) (setf lamp v)))
  (defcommon :count :value count :spec 'integer :driver t :accept (lambda (v) (setf count v)))
  (check "общие вещи уходят по порядку объявления, каждая со своими часами" (equal (common-state) '((0 nil) (0 0))))
  (accept-common '((1 t) (0 5)))
  (check "гость принимает то, что впереди, и всё, что решает водитель" (and (eq lamp t) (= count 5)))
  (accept-common '((0 nil) (9 9)) t)
  (check "старое сообщение не возвращает прежнее значение" (eq lamp t))
  (check "водитель не берёт у гостя то, что решает сам" (= count 5))
  (setf lamp nil)
  (check "своя правка двигает часы" (equal (first (common-state)) '(2 nil)))
  (accept-common '((2 t) (0 5)) t)
  (check "при равных часах водитель остаётся при своём" (null lamp))
  (accept-common '((2 t) (0 5)))
  (check "при равных часах гость берёт значение водителя" (eq lamp t))
  (accept-common '((7 :junk) (0 7)))
  (check "значение не той формы отброшено, соседнее принято" (and (eq lamp t) (= count 7)))
  (accept-common '((9 nil)))
  (check "список не той длины отброшен целиком" (eq lamp t))
  (defcommon :lamp :value lamp :spec '(:member t nil) :accept (lambda (v) (setf lamp v)))
  (check "повторное объявление не плодит записи" (= (length *commons*) 2))
  (reset-commons)
  (accept-common '((0 nil) (0 7)))
  (check "после сброса часов гость берёт всё как есть" (null lamp)))
(check "у каждой будки из плана есть объявление: подпись и действие"
       (every (lambda (plan) (and (booth (first plan) :label) (booth (first plan) :use))) *gimmick-plan*))
(check "у каждой будки из плана есть заметка в путеводителе"
       (every (lambda (plan) (assoc (first plan) *gimmick-notes*)) *gimmick-plan*))
(check "подпись будки — строка" (every (lambda (plan) (stringp (gimmick-label (first plan)))) *gimmick-plan*))
(check "того, что растёт, не больше десяти видов и не больше десяти штук: иначе слипнутся номера"
       (and (<= (length *forage*) 10) (every (lambda (e) (<= (fifth e) 10)) *forage*)))
(check "каждое блюдо делает один рецепт: от него зависит, где блюдо берут"
       (let ((dishes (mapcar #'second *recipes*))) (= (length dishes) (length (remove-duplicates dishes)))))
(check "у календаря есть заметки и приметы на каждую погоду"
       (and (> (length *calendar-notes*) 7) (every (lambda (w) (getf *omens* w)) '(:rain :fog :cloudy :clear :snow))))
(check "числа в текстах берутся из параметров: {долг} — это ссуда, незнакомое имя остаётся как есть"
       (and (string= (fill-text "Ссуда — {долг}.") (format nil "Ссуда — ~a." (money-text *debt-total*)))
            (string= (fill-text "{нет такого} и {обрыв") "{нет такого} и {обрыв") (string= (fill-text "просто") "просто")
            (notany (lambda (card) (search "тысяч" card)) *intro-cards*)))
(check "у вступления и титров есть текст" (and *intro-cards* *credits* (every #'stringp *intro-cards*) (every #'stringp *credits*)))
(check "на каждый запрещённый товар есть задание с лицензией"
       (every (lambda (r) (find (second r) *quests* :key #'second)) *restricted*))
(check "запрещённые товары существуют" (every (lambda (r) (assoc (first r) *goods*)) *restricted*))

(format t "Люди~%")
(check "людей не меньше двадцати, у каждого имя, место, реплики о мире и своя история"
       (and (>= (length *people*) 20)
            (every (lambda (e) (let ((p (rest e)))
                                 (and (keywordp (first e)) (stringp (getf p :who)) (getf p :world) (>= (length (getf p :life)) 2)
                                      (or (null (getf p :town)) (< -1 (getf p :town) 12))
                                      (let ((at (getf p :at)))
                                        (or (member at '(:fuel :camp))
                                            (and (eq (first at) :window) (rest at) (subsetp (rest at) '(:buyer :seller :sender :ration :recipient)))
                                            (and (eq (first at) :booth) (assoc (second at) *gimmick-plan*)))))))
                   *people*)))
(check "условия реплик понятны игре в любом городе, и запасная реплика есть у каждого"
       (every (lambda (e)
                (let ((p (rest e)))
                  (and (find :always (getf p :world) :key (lambda (w) (first (first w))))
                       (loop for tw below 12
                             always (every (lambda (w) (progn (person-cond-p (first w) tw) (stringp (second w)))) (getf p :world)))
                       (or (null (getf p :when)) (progn (person-cond-p (getf p :when) 0) t)))))
              *people*))
(check "сидящие в будках живут в городе своей будки"
       (every (lambda (e) (let ((at (getf (rest e) :at)))
                            (or (not (and (consp at) (eq (first at) :booth)))
                                (eql (getf (rest e) :town) (fifth (assoc (second at) *gimmick-plan*))))))
              *people*))
(let ((*people-met* '()) (*season* :summer) (*heat* 0) (*deeds* '()))
  (reset-plan 0)
  (let* ((first-line (person-line :granny 3)) (again (person-line :granny 3)))
    (check "человек говорит раз в день" (and (stringp first-line) (null again))))
  (let ((*story-stage* :owing) (*debt* *debt-total*))
    (setf (fourth (person-record :storekeeper)) -1)
    (person-line :storekeeper 10) (setf (fourth (person-record :storekeeper)) -1) (person-line :storekeeper 10)
    (setf (fourth (person-record :storekeeper)) -1) (person-line :storekeeper 10)
    (check "пока ссуда не тронута, история человека дальше первого шага не идёт"
           (= (third (person-record :storekeeper)) 1))
    (setf *people-met* (remove :storekeeper *people-met* :key #'first)))
  (let ((heard '()) (*story-stage* :owing) (*debt* 0))
    (dotimes (day 12)
      (setf (fourth (person-record :storekeeper)) -1)                    ; a new day
      (let ((line (person-line :storekeeper 10))) (when line (push line heard))))
    (check "за несколько встреч человек рассказывает всю свою историю, не повторяясь, а потом говорит дежурное"
           (and (every (lambda (life) (find-if (lambda (h) (search life h)) heard)) (getf (rest (assoc :storekeeper *people*)) :life))
                (= (length heard) 12)
                (<= (length (remove-duplicates heard :test #'string=)) 8))))
  (check "инспектор приходит, только когда за нами числится перепродажа"
         (and (null (person-speaks '(:window :buyer) 7))                   ; nobody tied to Dalny; granny spoke today, the inspector stays away
              (let ((*heat* 2) (*people-met* '())) (search "Кривцов" (or (person-speaks '(:window :buyer) 7) ""))))))
(let ((*people-met* '()) (*people-fate* '()) (*people-score* '()) (*quarter-done* 0) (*story-stage* :owing) (*debt* *debt-total*)
      (*cargo-day* 0) (*loan-seen* nil) (*heat* 0) (*deeds* '()))
  (reset-plan 0)
  (check "до первого платежа кварталов нет" (zerop (debt-quarter)))
  (dotimes (d 5) (people-day))                                             ; five days without a crate and without a payment
  (check "кто отвечает за наряды, за пять дней без единого ящика набирает пять плохих дней"
         (= (cdr (assoc :pusher *people-score*)) -5))
  (setf *debt* (round (* .7 *debt-total*)))
  (check "выплачена четверть — пора отчёт" (= (debt-quarter) 1))
  (let* ((cards (hold-report)) (fired (find :fired *people-fate* :key #'second)) (up (find :promoted *people-fate* :key #'second)))
    (format t "  первый отчёт без наших нарядов: снят ~a, в столицу ~a~%" (first fired) (first up))
    (check "на отчёте худшего снимают, лучшего забирают, счёт начинается заново"
           (and fired up (not (eq (first fired) (first up))) (>= (length cards) 3) (null *people-score*) (= *quarter-done* 1)))
    (check "снятый больше не на посту, вместо него говорит исполняющий обязанности"
           (let* ((p (rest (assoc (first fired) *people*))) (at (getf p :at))
                  (line (person-speaks (if (consp at) (list (first at) (second at)) at) (getf p :town))))
             (and (not (person-in-post-p (first fired))) (or (null line) (not (search (getf p :who) line))))))
    (check "снятого можно встретить ещё один раз, и только один"
           (let ((once (exile-line (first fired))) (twice (exile-line (first fired)))) (and (stringp once) (null twice))))
    (check "в титрах про каждого сказано, где он оказался; про ушедших — своё"
           (let ((lines (people-epilogue)))
             (and (= (length lines) (+ 4 (length *people*)))
                  (find-if (lambda (l) (search (getf (rest (assoc (first fired) *people*)) :fired-end) l)) lines)
                  (find-if (lambda (l) (search (getf (rest (assoc (first up) *people*)) :promoted-end) l)) lines)))))
  (setf *debt* 0)
  (loop while (< *quarter-done* (debt-quarter)) do (dotimes (d 4) (people-day)) (hold-report))
  (format t "  за четыре отчёта ушли: ~{~a~^, ~}; остались ~d из ~d~%"
          (mapcar (lambda (f) (format nil "~(~a~) (~(~a~))" (first f) (second f))) (reverse *people-fate*))
          (count-if #'person-in-post-p *people* :key #'first) (length *people*))
  (check "за всю игру уходит не больше восьми, никто — дважды"
         (and (<= (length *people-fate*) 8) (= (length *people-fate*) (length (remove-duplicates *people-fate* :key #'first))))))
(check "у каждого есть строка для титров; у тех, кто может уйти, — и прощание, и обе концовки, и замена"
       (every (lambda (e) (let ((p (rest e)))
                            (and (stringp (getf p :stayed))
                                 (or (null (getf p :answers))
                                     (and (every (lambda (k) (stringp (getf p k))) '(:acting :exile :fired-end :promoted :promoted-end))
                                          (member (person-mark (getf p :answers)) '(-1 0 1)))))))
              *people*))
(check "у каждого, кто подходит к окну, своя внешность, и двух одинаковых нет"
       (let ((faces (loop for e in *people*
                          when (and (consp (getf (rest e) :at)) (eq (first (getf (rest e) :at)) :window)) collect (getf (rest e) :look))))
         (and (every (lambda (f) (and (integerp (first f)) (<= 0 (second f) 4) (listp (third f)))) faces)
              (= (length faces) (length (remove-duplicates faces :test #'equal))))))
(check "слова отчёта и запасные реплики замещающих на месте"
       (and (>= (length *acting-lines*) 4) (every (lambda (k) (stringp (getf *report-lines* k))) '(:title :fired :promoted :quiet :stays))))
(format t "  по местам: окно ~d, будки ~d, заправка ~d, лагерь ~d~%"
        (count-if (lambda (e) (and (consp (getf (rest e) :at)) (eq (first (getf (rest e) :at)) :window))) *people*)
        (count-if (lambda (e) (and (consp (getf (rest e) :at)) (eq (first (getf (rest e) :at)) :booth))) *people*)
        (count :fuel *people* :key (lambda (e) (getf (rest e) :at))) (count :camp *people* :key (lambda (e) (getf (rest e) :at))))

;;; A player drives the ring for two months and lives by the plan's cargo alone: how long until the man is paid
;;; off, and the bank? It is a rough count, not the game: one crate offered at every stop that has cargo, six
;;; crates aboard at most, 7 m/s on the road and a minute and a half in a town; the man first (he grows), in
;;; cash turned from the account, then the bank. It tells whether the numbers are of the right order.
(format t "Баланс: месяц на нарядах~%")
(unless (boundp '*day-len*) (defparameter *day-len* 1200.0))   ; sky.lisp needs the engine; the length of a game day, in seconds
(defun ride-on-cargo (days)
  "Return (day-the-man-is-paid day-the-bank-is-paid earned crates) for DAYS of driving the ring on the plan's cargo."
  (reset-plan 0)
  (let* ((*deeds* '()) (ring (ring-length))
         (stops (sort (loop for tw in *towns* collect (cons (town-stop-d tw) (town-index tw))) #'< :key #'car))
         (at 0) (clock 0.0) (day 0) (aboard '()) (credit 0) (earned 0) (crates 0)
         (shark *shark-total*) (debt *debt-total*) (shark-day nil) (debt-day nil))
    (loop while (and (< day days) (not debt-day))
          do (let* ((here (nth at stops)) (next (nth (mod (1+ at) (length stops)) stops)) (town (cdr here))
                    (way (mod (- (car next) (car here)) ring)))
               (dolist (addr (remove town aboard :key (lambda (a) (mod a 100)) :test-not #'eql))   ; hand over what came here
                 (let ((pay (+ (delivery-pay) (deliver-cargo addr)))) (incf credit pay) (incf earned pay) (incf crates)))
               (setf aboard (remove town aboard :key (lambda (a) (mod a 100))))
               (let ((cargo (plan-cargo town)))                                            ; take the cargo waiting here
                 (when (and cargo (< (length aboard) 6))
                   (let ((pay (ceiling (cargo-tariff (mod (- (car (find (first cargo) stops :key #'cdr)) (car here)) ring)) 2)))
                     (incf credit pay) (incf earned pay))
                   (set-stock town (second cargo) (- (stock town (second cargo)) *crate-units*))
                   (push (plan-addr (first cargo) (second cargo)) aboard)))
               (decf credit (min credit (ceiling (* *tank-price* (/ way 1000 *tank-km*)))))   ; petrol for the next leg
               (cond ((plusp shark) (let ((cash (floor (* *cash-share* credit)))) (decf shark (min shark cash)) (setf credit 0)
                                      (when (and (<= shark 0) (not shark-day)) (setf shark-day day))))
                     (t (decf debt (min debt credit)) (setf credit 0)
                        (when (<= debt 0) (setf debt-day day))))
               (incf clock (+ 90.0 (/ way 7.0)))
               (loop while (>= clock *day-len*)
                     do (decf clock *day-len*) (incf day) (run-plan-day day)
                        (when (plusp shark) (setf shark (ceiling (* shark (+ 1 *shark-rate*))))))
               (setf at (mod (1+ at) (length stops)))))
    (list shark-day debt-day earned crates day)))
(destructuring-bind (shark-day debt-day earned crates days) (ride-on-cargo 240)
  (format t "  кольцо ~d м, игровой день ~d мин · за ~d дн. довезено ящиков: ~d, заработано ~a~%  цеховик закрыт на день ~a, ссуда — на день ~a (~,1f ч игры)~%"
          (round (ring-length)) (round *day-len* 60) days crates (money-text earned) shark-day debt-day
          (and debt-day (/ (* debt-day *day-len*) 3600.0)))
  (check "на одних нарядах цеховик закрывается за первые игровые дни, но не сразу" (and shark-day (<= 1 shark-day 15)))
  (check "ссуду можно выплатить, и это не час и не сто часов игры" (and debt-day (<= 20 debt-day 200))))

(format t "Заграница~%")
(check "у каждой страны есть товары из списка и строка для радио"
       (and *abroad* (every (lambda (c) (and (some (lambda (g) (assoc g *goods*)) (getf (cdr c) :goods)) (stringp (getf (cdr c) :news)))) *abroad*)))
(check "страны не действуют в один день, и каждая за месяц хоть раз"
       (let ((days (loop for d below 30 collect (count-if (lambda (c) (= (mod d (getf (cdr c) :every 7)) (getf (cdr c) :at 0))) *abroad*))))
         (and (<= (reduce #'max days) 1) (every (lambda (c) (<= (getf (cdr c) :every) 30)) *abroad*))))
(let ((brother (cdr (find :brother *abroad* :key (lambda (c) (getf (cdr c) :side)))))
      (west (cdr (find :west *abroad* :key (lambda (c) (getf (cdr c) :side))))))
  (let ((*goods* *goods*))
    (let ((saved (make-hash-table :test #'equal)))
      (maphash (lambda (k v) (setf (gethash k saved) v)) *stocks*)
      (let ((line (let ((*random-state* (make-random-state t))) (abroad-event brother))))
        (check "поставка братьям опустошает полки во всех городах"
               (and (stringp line) (some (lambda (g) (loop for tw below (plan-size) always (<= (stock tw g) (* .31 (max 1e-6 (gethash (cons tw g) saved 0.0))))))
                                         (getf brother :goods)))))
      (let ((line (abroad-event west)))
        (check "пароход с Запада даёт порту избыток, который план повезёт дальше"
               (and (stringp line) (some (lambda (g) (and (= (stock (getf west :town) g) *stock-cap*) (surplus-p (getf west :town) g))) (getf west :goods)))))
      (clrhash *stocks*) (maphash (lambda (k v) (setf (gethash k *stocks*) v)) saved))))

(format t "~%~d проверок, ~:[всё сошлось~;провалов: ~:*~d~]~%" *checks* (and *fails* (length *fails*)))
(ext:quit (if *fails* 1 0))
