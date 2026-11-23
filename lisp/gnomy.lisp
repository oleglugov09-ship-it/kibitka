;;;; Gnomes are trophies. In every town and at six special places by the road hides a gnome in the spirit of
;;;; that place; a found one goes on the trophy shelf by the wagon's entrance: towns on the left, special places
;;;; on the right. The driver collects them; guests are told what has been found already (sync.lisp).

(in-package #:kiosk-trip)


;;; A figure 24 cm tall, facing -z. Its props are a list of shapes: (kind dims color x y z [rotation [glow]]).
(defvar *gnome-plan* '()
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
The gnomes stand on two shelves above the door, nine to a shelf, facing the room."
  (list (+ -1.0 (* .25 (mod i 9))) (if (< i 9) 2.07 2.47) (- *half-length* .2) 0.0))

(defun show-trophies ()
  (loop for (i . node) in *trophies* do (object-set node "visible" (gnome-found-p i))))

(defun make-trophies (c)
  "Build the trophy shelves by the entrance under C and all eighteen figures on them.
Figures not yet found are hidden."
  (let ((f *floor*))
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
                               nil 2.6))))))

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
