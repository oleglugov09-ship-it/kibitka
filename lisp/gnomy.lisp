;;;; Gnomes are trophies. In every town and at six special places by the road hides a gnome in the spirit of
;;;; that place; a found one goes on the trophy shelf by the wagon's entrance: towns on the left, special places
;;;; on the right. The driver collects them; guests are told what has been found already (sync.lisp).

(in-package #:kiosk-trip)


;;; A figure 24 cm tall, facing -z. Its props are a list of shapes: (kind dims color x y z [rotation [glow]]).
(defparameter *gnome-plan*
  '((0 "Рыбак" "#3f6f73" "#55708a"
     ((:box (.006 .22 .006) "#6a5038" .075 .16 -.03 (.5 0 0)) (:box (.05 .018 .012) "#9ab0b8" .075 .05 -.11)))
    (1 "Часовой" "#4a5a38" "#5a6a44"
     ((:box (.022 .022 .006) "#c0241c" 0 .215 -.036) (:box (.008 .2 .008) "#3a3028" -.075 .12 0)))
    (2 "Кузнец" "#8a4038" "#5a5a5e"
     ((:box (.008 .13 .008) "#6a5038" .075 .09 -.02) (:box (.05 .028 .028) "#34363a" .075 .16 -.02)))
    (3 "Студент" "#2c4a7a" "#8a8070"
     ((:box (.055 .07 .016) "#a3262c" 0 .075 -.07) (:box (.07 .012 .004) "#1a1a1e" 0 .14 -.046)))
    (4 "Молочник" "#e0dcd0" "#6a8a5a"
     ((:cyl (.016 .024 .06 8) "#d8dce0" .08 .035 -.02) (:cyl (.01 .016 .012 8) "#d8dce0" .08 .07 -.02)))
    (5 "Боцман" "#1e3a6a" "#e8e4d8"
     ((:cyl (.062 .066 .012 10) "#1e3a6a" 0 .05 0) (:cyl (.052 .056 .012 10) "#1e3a6a" 0 .085 0)
      (:box (.008 .07 .008) "#3a3a40" .08 .06 -.02) (:box (.05 .008 .008) "#3a3a40" .08 .03 -.02)))
    (6 "Писарь" "#3a3a40" "#55554f"
     ((:box (.07 .05 .02) "#6a4a2a" .085 .045 0) (:box (.03 .04 .004) "#f0ece0" -.03 .09 -.068)))
    (7 "Полярник" "#7a5a3a" "#b8ac94"
     ((:box (.022 .05 .05) "#7a5a3a" .05 .125 0) (:box (.022 .05 .05) "#7a5a3a" -.05 .125 0)
      (:ball (.012 6 4) "#a3262c" .07 .04 -.05) (:ball (.012 6 4) "#a3262c" .09 .03 -.04)))
    (8 "Шахтёр" "#c9a13a" "#3a3838"
     ((:ball (.016 6 4) "#ffe9b0" 0 .2 -.045 nil "#ffd890") (:box (.008 .15 .008) "#6a5038" .075 .1 -.02)
      (:box (.07 .012 .012) "#34363a" .075 .17 -.02 (0 0 .3))))
    (9 "Курортник" "#e8d8a0" "#d88a6a"
     ((:cyl (.014 .011 .04 8) "#a8d0e0" .075 .1 -.03) (:box (.07 .012 .004) "#1a1a1e" 0 .14 -.046)))
    (10 "Стрелочник" "#a3262c" "#2a3a5a"
     ((:box (.03 .04 .03) "#ff3020" .08 .07 -.03 nil "#ff3020") (:box (.006 .03 .006) "#3a3a40" .08 .1 -.03)))
    (11 "Пасечник" "#c9a13a" "#e8e4d8"
     ((:cyl (.028 .028 .05 10) "#c8862a" .085 .03 -.01) (:cyl (.03 .03 .008 10) "#6a4a2a" .085 .058 -.01)))
    (12 "Звездочёт" "#1e2a6a" "#2c3a7a"
     ((:ball (.008 4 3) "#ffe070" .02 .22 -.03 nil "#ffe070") (:ball (.007 4 3) "#ffe070" -.022 .2 -.036 nil "#ffe070")
      (:cyl (.01 .014 .12 8) "#b08a3a" .07 .14 -.04 (1.0 0 .3))))
    (13 "Верхолаз" "#d86a2a" "#5a6a44"
     ((:cyl (.034 .034 .014 10) "#d8c04a" .08 .07 0 (1.57 0 0)) (:box (.006 .1 .006) "#d8c04a" .08 .1 0)))
    (14 "Проходчик" "#6e6e6a" "#5a4a3a"
     ((:box (.028 .036 .028) "#ffd080" .08 .06 -.03 nil "#ffb040") (:box (.036 .006 .036) "#3a3a40" .08 .082 -.03)))
    (15 "Болотник" "#3a6a3a" "#4a5a38"
     ((:ball (.022 8 6) "#4a8a3a" .07 .02 -.06) (:ball (.008 4 3) "#f0ece0" .078 .034 -.072) (:ball (.008 4 3) "#f0ece0" .062 .034 -.072)))
    (16 "Регулировщик" "#e8e4d8" "#2c4a7a"
     ((:box (.012 .05 .012) "#1a1a1e" .075 .1 -.03) (:box (.013 .05 .013) "#f0ece0" .075 .15 -.03) (:box (.012 .05 .012) "#1a1a1e" .075 .2 -.03)))
    (17 "Повар" "#f0ece0" "#e8e4d8"
     ((:cyl (.05 .04 .05 10) "#f0ece0" 0 .22 0) (:box (.006 .12 .006) "#8a8a84" .075 .1 -.02) (:ball (.016 6 4) "#8a8a84" .075 .16 -.02))))
  "The gnomes: entries (number name hat-color coat-color props).
Numbers 0-11 are towns, 12-17 are special places.")


(defvar *gnomes* 0 "The found gnomes, one bit per gnome.")
(defvar *gnome-nodes* '() "The gnomes by the road that are currently built: entries (number node path-coordinate).")
(defvar *trophies* '() "The figures on the wagon's shelves, keyed by number.")
(defvar *gnome-told* nil "The number of the gnome the players were already told is nearby.")

(defun gnome-found-p (i) (logbitp i *gnomes*))
(defun gnome-name (i) (second (assoc i *gnome-plan*)))
(defparameter *gnome-spots* '("обсерватория" "мост над ущельем" "туннель" "болото" "Перекрёсток" "закусочная")
  "The special places of gnomes 12-17, in the nominative case, for lists.")
(defun gnome-home (i) (if (< i 12) (town-name i) (nth (- i 12) *gnome-spots*)))
(defun gnome-count () (logcount *gnomes*))

(defun save-gnomes () (unless (net-guest-p) (put-save :gnomes *gnomes*)))
(defun load-gnomes () (let ((f (get-save :gnomes))) (setf *gnomes* (if (and (integerp f) (<= 0 f (1- (ash 1 18)))) f 0))))
(defstate :gnomes                                            ; a collection: a new game does not reset it
  :clear (lambda () (setf *gnomes* 0)) :load #'load-gnomes :save #'save-gnomes :show (lambda () (show-trophies)) :keep t)

;;; ------------------------------------------------------------------ where they hide

(defun open-road-d (d step)
  "Return the path coordinate nearest to D, stepping by STEP, where the road has land beside it.
That means neither a bridge nor a tunnel."
  (loop repeat 60 while (bare-at d) do (incf d step))
  d)

(defun gnome-spot (i)
  "Return where gnome I hides as (path-coordinate sideways-offset).
Return NIL if the map has no such place."
  (flet ((at (d x) (and d (list (float d 1.0) x))))
    (case i
      (12 (at (and *observatory* (+ *observatory* 7.0)) 9.5))
      (13 (at (and *chasm* (open-road-d (- *chasm* 60.0) -12.0)) 5.6))
      (14 (at (and *tunnel* (open-road-d (- (first *tunnel*) 44.0) -12.0)) 6.2))
      (15 (at (and *swamp* (/ (+ (first *swamp*) (second *swamp*)) 2)) 6.6))
      (16 (at (and *exits* (- (edge-d1 (cdr (first *exits*))) 48.0)) 7.0))
      (17 (at (and *diner-plan* (+ (gap-d (first (first *diner-plan*)) (second (first *diner-plan*))) 11.0)) 5.8))
      (t (let ((tw (nth i *towns*)))
           (and tw (at (+ (town-stop-d tw) (nth i '(16 -14 18 -16 15 -18 17 -13 19 -15 14 -17))) (+ 5.2 (* .5 (mod i 3))))))))))

;;; ------------------------------------------------------------------ the figure

(defun gnome-figure (parent i)
  "Build gnome I under PARENT: coat, beard, hat and the props of its place.
Return the group node."
  (destructuring-bind (n name hat coat props) (assoc i *gnome-plan*)
    (declare (ignore n name))
    (let ((g (group parent)))
      (shape g :cyl '(.045 .07 .1 10) coat 0 .05 0)
      (dolist (sx '(-.03 .03)) (shape g :box '(.035 .02 .05) "#3a2a1e" sx .01 -.03))          ; shoes
      (shape g :ball '(.045 10 8) "#e8c0a0" 0 .135 0)
      (shape g :cyl '(.04 .006 .075 8) "#f0ece0" 0 .092 -.034 :rot '(-.25 0 0))                 ; beard
      (shape g :ball '(.013 6 4) "#d88a8a" 0 .132 -.046)
      (dolist (sx '(-.018 .018)) (shape g :box '(.008 .01 .004) "#1a1a1e" sx .148 -.042 :shadow nil))
      (unless (= i 17) (shape g :cyl '(.003 .05 .13 10) hat 0 .225 0))                          ; hat (the cook has his own)
      (dolist (p props)
        (destructuring-bind (kind dims rgb x y z &optional rot emit) p
          (shape g kind dims rgb x y z :rot rot :emit emit :shadow (not emit))))
      g)))

;;; ------------------------------------------------------------------ trophy shelves in the wagon

(defun trophy-spot (i)
  "Return the spot of gnome I on the shelf as (x height-above-floor z rotation).
Towns take the three shelves to the left of the entrance, special places the shelf on the right."
  (if (< i 12)
      (list (- .15 *half-width*) (nth (floor i 4) '(.75 1.2 1.65)) (+ 1.6 (* .3 (mod i 4))) (- (/ pi 2)))
      (list (- *half-width* .15) 1.85 (+ 1.55 (* .2 (- i 12))) (/ pi 2))))

(defun show-trophies ()
  (loop for (i . node) in *trophies* do (object-set node "visible" (gnome-found-p i))))

(defun make-trophies (c)
  "Build the trophy shelves by the entrance under C and all eighteen figures on them.
Figures not yet found are hidden."
  (let ((f *floor*) (x (- *half-width* .13)))
    (dolist (h '(.75 1.2 1.65))
      (shape c :box '(.22 .03 1.3) "#8a6440" (- x) (+ f h -.015) 2.05)
      (dolist (z '(1.45 2.65)) (shape c :box '(.2 .12 .03) "#5a3e26" (- x) (+ f h -.09) z)))
    (shape c :box '(.22 .03 1.3) "#8a6440" x (+ f 1.835) 2.05)
    (dolist (z '(1.45 2.65)) (shape c :box '(.2 .12 .03) "#5a3e26" x (+ f 1.76) z))
    (setf *trophies*
          (loop for (i) in *gnome-plan*
                collect (destructuring-bind (tx ty tz yaw) (trophy-spot i)
                          (let ((g (gnome-figure c i)))
                            (node-3d-set-position g (v3 tx (+ f ty) tz))
                            (node-3d-set-rotation g (v3 0 yaw 0))
                            (cons i g)))))
    (show-trophies)))

;;; ------------------------------------------------------------------ by the road

(defshared pick-gnome :gnome (i)
  (cond ((or (not (assoc i *gnome-plan*)) (gnome-found-p i)) (say "Этот гномик уже на полке"))
        (t (setf *gnomes* (logior *gnomes* (ash 1 i)))
           (save-gnomes)
           (show-trophies)
           (log-event :gnome :n i :found (gnome-count))
           (say-all (format nil "Гномик-~a (~a)! Встал на полку у входа · найдено ~d из ~d"
                        (string-downcase (gnome-name i)) (gnome-home i) (gnome-count) (length *gnome-plan*))))))

(defun step-gnomes ()
  "Update the roadside gnomes for the current position.
Build the gnomes the wagon has reached, remove those it has left behind, and hint that one
is nearby."
  (loop for (i) in *gnome-plan*
        do (let ((spot (and (not (gnome-found-p i)) (gnome-spot i))) (i i))
             (when spot
               (destructuring-bind (d x) spot
                 (listed ((list :gnome i) d)
                   (destructuring-bind (wx wy wz) (side-point x d 0.0)
                     (let ((g (gnome-figure *world* i)))
                       (node-3d-set-position g (v3 wx wy wz))
                       (node-3d-set-rotation g (v3 0 (- (/ pi 2) (road-frame d)) 0))       ; facing the road
                       (node-3d-set-scale g (v3 2.4 2.4 2.4))
                       (push (list i g d) *gnome-nodes*))))
                 (when (and (< (abs (- d *dist*)) 45.0) (< *speed* .4) (not (eql *gnome-told* i)) (not (net-guest-p)))
                   (setf *gnome-told* i)
                   (say-all "Где-то рядом спрятался гномик. Выйди и поищи у обочины"))))))
  (setf *gnome-nodes* (remove-if (lambda (e) (when (or (out-of-window-p (third e)) (gnome-found-p (first e)))     ; drove away, or it was found
                                               (node-queue-free (second e)) t))
                                 *gnome-nodes*)))

(defun gnome-usables ()
  "Return the usables for gnomes: those by the road (E takes one) and labels for those on the shelf."
  (append
   (loop for (i nil d) in *gnome-nodes*
         collect (let ((i i))
                   (road-usable (second (gnome-spot i)) d .3
                                (lambda () (format nil "E — взять гномика: ~a" (string-downcase (gnome-name i))))
                                (lambda () (pick-gnome i)) 2.4)))
   (loop for (i) in *gnome-plan*
         when (gnome-found-p i)
           collect (let ((i i))
                     (destructuring-bind (x y z yaw) (trophy-spot i)
                       (declare (ignore yaw))
                       (usable x (+ y .12) z (lambda () (format nil "Гномик-~a · ~a" (string-downcase (gnome-name i)) (gnome-home i)))
                               (lambda () (say (format nil "Найдено гномиков: ~d из ~d" (gnome-count) (length *gnome-plan*))))
                               nil 1.6))))))

(defun gnome-lines (from to)
  "Return the gnome list lines for numbers FROM through TO.
A found gnome is shown with its name, an unfound one only with where it is from."
  (loop for (i name) in *gnome-plan*
        when (<= from i to)
          collect (format nil "~:[?~*~;+ ~a~] — ~a" (gnome-found-p i) name (gnome-home i))))

(defun guide-gnome-page ()
  (format nil "В каждом городе и в шести особых местах у дороги прячется гномик. Встань рядом, выйди и поищи у обочины; найденный встаёт на полку у входа.~%~%Найдено ~d из ~d. В городах:~%~{~a~%~}"
          (gnome-count) (length *gnome-plan*) (gnome-lines 0 11)))

(defun guide-gnome-page-2 ()
  (format nil "В особых местах:~%~{~a~%~}~%Плюс стоит у тех, кто уже на полке. Знак вопроса — этого ещё искать." (gnome-lines 12 17)))
