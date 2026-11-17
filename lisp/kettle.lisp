;;;; The kettle and the frying pan: two things of the kitchen corner that are carried about by hand.
;;;;
;;;; Tea. The kettle stands on the tea table. Take it (E), fill it at the washstand, put it on the ring of
;;;; the stove, to the right of the firebox. While the fire burns it heats up; once it has boiled the tea is
;;;; brewed right in it: carry it back to the table and pour, a cup at a time (the cups warm you, byt.lisp).
;;;;
;;;; The frying pan hangs on a nail by the stove. Put it on the stove top and everything in the recipes
;;;; fries on it, not only milk for blini (RAW-ON-STOVE in uyut.lisp). The lid from the hardware shop hangs
;;;; above it once bought: under it things fry twice as fast.
;;;;
;;;; Both are common to everyone in the wagon (DEFCOMMON at the end of the file): one kettle, one pan. While
;;;; someone carries the kettle, the others see it gone (:AWAY). The driver decides when it has boiled.
;;;; Neither is saved: after a restart the kettle is empty on the table and the pan is on its nail.

(in-package #:kiosk-trip)

(defparameter *boil-time* 20.0 "The number of seconds the kettle takes to boil on a burning stove.")
(defparameter *kettle-cups* 4 "The number of cups of tea in one boiled kettle.")
(defparameter *kettle-ring* '(0.0 .93 -.49) "The ring for the kettle, relative to the stove: x, height above the floor, z.")
(defparameter *pan-nail* '(.28 1.5 .385) "The nail the frying pan hangs on, relative to the stove: x, height above the floor, z.")

(defvar *kettle-at* :table "Where the kettle is: :TABLE, :STOVE, :PAW (in our paw) or :AWAY (in another cat's paw).")
(defvar *kettle-water* nil "True when there is cold water in the kettle.")
(defvar *kettle-heat* 0.0 "The number of seconds the water has been heated.")
(defvar *kettle-tea* 0 "The number of cups of tea left in the kettle.")
(defvar *kettle-nodes* nil "The kettle's nodes, a plist: :table :stove :paw :steam.")
(defvar *pan-on* nil "True when the frying pan is on the stove.")
(defvar *pan-nodes* nil "The frying pan's nodes, a plist: :wall :stove :lid.")

(defun kettle-model (parent x y z)
  "Build an enamel kettle under PARENT with its bottom at (X Y Z); return its group."
  (let ((g (group parent x y z)) (enamel "#3f6f73"))
    (shape g :cyl '(.085 .11 .14 12) enamel 0 .07 0 :rough .4)
    (shape g :cyl '(.06 .085 .03 12) enamel 0 .155 0 :rough .4 :shadow nil)
    (shape g :ball '(.02 6 4) "#2a2a2e" 0 .185 0 :shadow nil)
    (shape g :box '(.12 .03 .03) enamel .13 .1 0 :rot '(0 0 .6) :shadow nil)                       ; the spout
    (dolist (sx '(-.07 .07)) (shape g :box '(.015 .09 .015) "#2a2a2e" sx .2 0 :shadow nil))        ; the handle over the lid
    (shape g :box '(.16 .015 .02) "#2a2a2e" 0 .245 0 :shadow nil)
    g))

(defun pan-model (parent x y z &key hanging)
  "Build a cast-iron frying pan under PARENT at (X Y Z): lying on a stove, or HANGING flat against the back wall."
  (let ((g (group parent x y z)) (iron "#1e1e22"))
    (cond (hanging (shape g :cyl '(.16 .16 .03 14) iron 0 0 0 :rot (list (/ pi 2) 0 0) :metal .4 :rough .7)
                   (shape g :box '(.035 .22 .02) iron 0 -.26 0 :metal .4 :rough .7))
          (t (shape g :cyl '(.19 .17 .035 14) iron 0 .018 0 :metal .4 :rough .7)
             (shape g :cyl '(.17 .17 .004 14) "#2c2c32" 0 .037 0 :metal .3 :rough .5 :shadow nil)
             (shape g :box '(.24 .02 .035) iron .3 .03 0 :metal .4 :rough .7)))
    g))

(defun show-kettle ()
  "Show the kettle where it is and the frying pan where it is."
  (loop for key in '(:table :stove :paw)
        do (let ((node (getf *kettle-nodes* key))) (when node (object-set node "visible" (eq *kettle-at* key)))))
  (let ((steam (getf *kettle-nodes* :steam)))
    (when steam (object-set steam "visible" (and (eq *kettle-at* :stove) (plusp *kettle-tea*)))))
  (when *pan-nodes*
    (object-set (getf *pan-nodes* :wall) "visible" (not *pan-on*))
    (object-set (getf *pan-nodes* :stove) "visible" (and *pan-on* t))
    (object-set (getf *pan-nodes* :lid) "visible" (and *lid* t))))

(defun show-lid () "Show the lid above the frying pan once it is bought." (show-kettle))

(defun make-kettle (c)
  "Build the kettle (on the table, on the stove, in the paw) and the frying pan (on its nail, on the stove) for the wagon C."
  (let ((old (getf *kettle-nodes* :paw))) (when old (node-queue-free old)))   ; it hangs under the camera, not under the old wagon
  (destructuring-bind (sx sz) *stove-at*
    (let* ((f *floor*)
           (stove (destructuring-bind (rx ry rz) *kettle-ring* (kettle-model c (+ sx rx) (+ f ry) (+ sz rz))))
           (steam (group stove .19 .2 0)))
      (dotimes (i 3) (shape steam :ball '(.035 6 4) "#f0f0f0" 0 (* i .07) 0 :alpha .5 :shadow nil :unshaded t))
      (setf *kettle-nodes*
            (list :table (and *table* (kettle-model *table* (- (first *table-at*) .16) (+ *table-top* .035) (second *table-at*)))
                  :stove stove :steam steam
                  :paw (and *camera* (kettle-model *camera* .24 -.3 -.5)))
            *pan-nodes*
            (destructuring-bind (nx ny nz) *pan-nail*
              (let ((lid (group c (+ sx nx) (+ f ny .36) (+ sz nz))))                ; the lid on its own nail, above the pan
                (shape lid :cyl '(.15 .15 .02 14) "#2a2a30" 0 0 0 :rot (list (/ pi 2) 0 0) :metal .4 :rough .6)
                (shape lid :ball '(.03 6 4) "#1e1e22" 0 0 -.03 :shadow nil)
                (list :wall (pan-model c (+ sx nx) (+ f ny) (+ sz nz) :hanging t)
                      :stove (pan-model c sx (+ f *stove-top*) sz)
                      :lid lid))))))
  (show-kettle))

;;; ------------------------------------------------------------------ what E does

(defun move-kettle (to text)
  (setf *kettle-at* to *kettle-heat* 0.0)
  (show-kettle)
  (say text))

(defun use-table-kettle ()
  "Answer E on the kettle's place on the table: pour a cup, take the kettle, or put it back."
  (cond ((eq *kettle-at* :away) (say "Чайник у кого-то в лапе"))
        ((eq *kettle-at* :paw) (move-kettle :table (if (plusp *kettle-tea*) "Чайник на столе. E по нему — налить чаю" "Чайник на столе")))
        ((plusp *kettle-tea*) (when (pour-tea) (decf *kettle-tea*)))
        (t (move-kettle :paw "Чайник в лапе"))))

(defun use-washstand ()
  (cond ((not (eq *kettle-at* :paw)) (say "Умылся. Свежо"))
        (t (setf *kettle-water* t *kettle-heat* 0.0 *kettle-tea* 0)
           (say "Набрал воды. Теперь на печку: справа на ней конфорка для чайника"))))

(defun use-ring ()
  "Answer E on the ring of the stove: put the kettle on it or take it off."
  (case *kettle-at*
    (:paw (move-kettle :stove (cond ((plusp *kettle-tea*) "Чайник на печке")
                                    ((not *kettle-water*) "В чайнике пусто. Набери воды в умывальнике и вскипяти на печке")
                                    ((> *fire* 0) "Чайник на огне. Скоро закипит")
                                    (t "Печка холодная — подбрось доску, и чайник закипит"))))
    (:stove (move-kettle :paw "Чайник в лапе"))))

(defun ring-label ()
  (case *kettle-at*
    (:paw "E — поставить чайник на печку")
    (:stove (cond ((plusp *kettle-tea*) "E — снять чайник: чай готов")
                  ((not *kettle-water*) "E — снять чайник: он пустой")
                  ((> *fire* 0) (format nil "Чайник греется: ~d% · E — снять" (min 99 (round (* 100 (/ *kettle-heat* *boil-time*))))))
                  (t "Чайник на холодной печке: подбрось доску · E — снять")))
    (t "Конфорка для чайника")))

(defun food-on-stove-p ()
  "Return true if anything stands on the stove top."
  (destructuring-bind (cx cz w d top) (stove-surface)
    (some (lambda (it) (destructuring-bind (kind x y z &rest more) (car it)
                         (declare (ignore kind more))
                         (and (< (abs (- x cx)) (+ (/ w 2) .03)) (< (abs (- z cz)) (+ (/ d 2) .03)) (< (abs (- y top)) .08))))
          *items*)))

(defun toggle-pan ()
  (cond ((and *pan-on* (food-on-stove-p)) (say "Сначала сними еду со сковородки"))
        (t (setf *pan-on* (not *pan-on*))
           (show-kettle)
           (say (if *pan-on* "Сковородка на печке: клади на неё рыбу, хлеб, клюкву или тушёнку — пожарится"
                    "Сковородка висит на гвозде")))))

(defun kettle-usables ()
  "Return the things to press for the kettle and the frying pan."
  (destructuring-bind (sx sz) *stove-at*
    (remove nil
            (list
             (when (and (table-shown-p) (member *kettle-at* '(:table :paw :away)))
               (usable (- (first *table-at*) .16) (+ *table-top* .14) (second *table-at*)
                       (lambda () (cond ((eq *kettle-at* :away) "Чайник у кого-то в лапе")
                                        ((eq *kettle-at* :paw) "E — поставить чайник на стол")
                                        ((plusp *kettle-tea*) (format nil "E — налить чаю · осталось стаканов: ~d" *kettle-tea*))
                                        (*kettle-water* "E — взять чайник: вода в нём холодная")
                                        (t "E — взять чайник: он пустой")))
                       #'use-table-kettle nil 2.6 .16))
             (usable (+ (first *washstand-at*) .1) (+ *washstand-top* .25) (second *washstand-at*)
                     (lambda () (if (eq *kettle-at* :paw) "E — набрать воды в чайник" "Умывальник. С чайником в лапе — набрать воды"))
                     #'use-washstand nil 2.6 .3)
             (destructuring-bind (rx ry rz) *kettle-ring*
               (usable (+ sx rx) (+ ry .1) (+ sz rz) #'ring-label #'use-ring nil 2.6 .2))
             (if *pan-on*
                 (usable (+ sx .3) (+ *stove-top* .05) sz (lambda () "E — повесить сковородку на гвоздь") #'toggle-pan nil 2.6 .1)
                 (destructuring-bind (nx ny nz) *pan-nail*
                   (usable (+ sx nx) ny (+ sz nz) (lambda () "E — поставить сковородку на печку") #'toggle-pan nil 2.6 .2)))))))

(defun step-kettle (delta)
  "Heat the kettle on the burning stove by DELTA seconds and let the steam rise once it has boiled.
Everyone counts the heat for their own hint; the driver alone decides that it has boiled."
  (when (and (eq *kettle-at* :away) (null *net-others*))                   ; whoever carried it has left: it is back on the table
    (setf *kettle-at* :table) (show-kettle))
  (when (and (eq *kettle-at* :stove) *kettle-water* (> *fire* 0))
    (when (and (>= (incf *kettle-heat* delta) *boil-time*) (not (net-guest-p)))
      (setf *kettle-water* nil *kettle-heat* 0.0 *kettle-tea* *kettle-cups*)
      (show-kettle)
      (say-all "Чайник закипел! Заварили прямо в нём. Неси на стол и разливай по стаканам")))
  (let ((steam (getf *kettle-nodes* :steam)))
    (when (and steam (eq *kettle-at* :stove) (plusp *kettle-tea*))
      (node-3d-set-position steam (v3 (+ .19 (* .015 (sin (* *time* 3.0)))) (+ .2 (* .03 (mod (* *time* .8) 1.0))) 0)))))

;;; ------------------------------------------------------------------ shared over the network

(defcommon :kettle
  :value (list (if (eq *kettle-at* :away) :paw *kettle-at*) (and *kettle-water* t) *kettle-tea*)
  :spec '((:member :table :stove :paw) (:member t nil) integer)
  :accept (lambda (state)
            (destructuring-bind (at water tea) state
              ;; 'in a paw' means ours if we hold it and somebody else's otherwise
              (let ((at (cond ((not (eq at :paw)) at) ((eq *kettle-at* :paw) :paw) (t :away)))
                    (tea (max 0 (min *kettle-cups* tea))))
                (unless (and (eq at *kettle-at*) (eq water (and *kettle-water* t)) (= tea *kettle-tea*))
                  (when (or (not (eq at *kettle-at*)) (and water (not *kettle-water*))) (setf *kettle-heat* 0.0))
                  (setf *kettle-at* at *kettle-water* water *kettle-tea* tea)
                  (show-kettle))))))

(defcommon :pan :value (and *pan-on* t) :spec '(:member t nil)
  :accept (lambda (on) (unless (eq on (and *pan-on* t)) (setf *pan-on* on) (show-kettle))))
