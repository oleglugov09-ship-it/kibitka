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
;;;; Both are common to everyone in the wagon: one kettle, one pan. The kettle is an aktor (aktor.lisp): where
;;;; it is, who carries it and what is in it is one state, kept by the driver and changed only by telling the
;;;; kettle something (DEFTELL below). While someone carries it, the others see it gone (:AWAY); when that cat
;;;; leaves, the kettle is back on the table. The driver decides when it has boiled. The pan is an aktor too: a
;;;; switch, on the stove or on its nail.
;;;; Both are saved with the world (DEFSTATE :KITCHEN below): where the kettle stands, the water or the tea in
;;;; it, whether the pan is on the stove. A kettle that was in a paw is back on the table, and water that was
;;;; heating starts from cold. A new game empties the kettle and hangs the pan on its nail.

(in-package #:kiosk-trip)

(defparameter *boil-time* 20.0 "The number of seconds the kettle takes to boil on a burning stove.")
(defparameter *kettle-cups* 4 "The number of cups of tea in one boiled kettle.")
(defparameter *kettle-ring* '(0.0 .795 -.49) "The ring for the kettle, relative to the stove: x, height above the floor, z.")
(defparameter *pan-nail* '(.28 1.5 .385) "The nail the frying pan hangs on, relative to the stove: x, height above the floor, z.")

(defvar *kettle-heat* 0.0 "The number of seconds the water has been heated: everyone counts it for their own hint.")

;;; The kettle's state: :AT is where it is (:TABLE, :STOVE or :PAW), :HOLDER the number of the player who
;;; carries it (0 is the driver) or NIL, :WATER true when there is cold water in it, :TEA the cups of tea left,
;;; :BREW what was put into the water (NIL for plain tea, or a brew key from *BREWS*).
(defaktor :kettle
  :state '(:at :table :holder nil :water nil :tea 0 :brew nil)
  :spec '((:member :at) (:member :table :stove :paw) (:member :holder) (:or integer null)
          (:member :water) (:member t nil) (:member :tea) integer (:member :brew) symbol)
  :show (lambda ()
          (unless (and (eq (peek :kettle :at) :stove) (peek :kettle :water)) (setf *kettle-heat* 0.0))   ; taken off, or the water is gone
          (show-kettle)))

(defun kettle (key) "Return the field KEY of the kettle's state." (peek :kettle key))
(defvar *kettle-nodes* nil "The kettle's nodes, a plist: :table :stove :paw :steam.")
(defvar *pan-nodes* nil "The frying pan's nodes, a plist: :wall :stove :lid.")

(defun kettle-model (parent x y z)
  "Build an enamel kettle under PARENT with its bottom at (X Y Z); return its group."
  (let ((g (group parent x y z)))
    (model g "kettle")
    g))

(defun pan-model (parent x y z &key hanging)
  "Build a cast-iron frying pan under PARENT at (X Y Z): lying on a stove, or HANGING flat against the back wall."
  (let ((g (group parent x y z)))
    (cond (hanging (model g "pan-hanging"))
          (t (model g "pan")))
    g))

(defun show-kettle ()
  "Show the kettle where it is and the frying pan where it is."
  (when (and *camera* *kettle-nodes* (null (getf *kettle-nodes* :paw)))        ; the camera is made after the wagon: the paw kettle waits for it
    (setf (getf *kettle-nodes* :paw) (kettle-model *camera* 0 -.29 -.44)))
  (loop for key in '(:table :stove :paw)
        do (let ((node (getf *kettle-nodes* key))) (when node (object-set node "visible" (eq (kettle-at) key)))))
  (let ((steam (getf *kettle-nodes* :steam)))
    (when steam (object-set steam "visible" (and (eq (kettle-at) :stove) (plusp (kettle :tea))))))
  (when *pan-nodes*
    (object-set (getf *pan-nodes* :wall) "visible" (not (on-p :pan)))
    (object-set (getf *pan-nodes* :stove) "visible" (and (on-p :pan) t))
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
                  :paw nil)                                                ; made by SHOW-KETTLE once there is a camera
            *pan-nodes*
            (destructuring-bind (nx ny nz) *pan-nail*
              (let ((lid (group c (+ sx nx) (+ f ny .36) (+ sz nz))))                ; the lid on its own nail, above the pan
                (model lid "pan-lid")
                (list :wall (pan-model c (+ sx nx) (+ f ny) (+ sz nz) :hanging t)
                      :stove (pan-model c sx (+ f *stove-top*) sz)
                      :lid lid))))))
  (let ((knife (group c 0 0 0)))                                             ; the knife of the cutting board
    (model knife "knife")
    (setf *chop-nodes* (list :knife knife :slices '())))
  (show-chop)
  (show-kettle))

;;; ------------------------------------------------------------------ what E does

(defun my-number () "Return our number among the players: 0 for the driver and for a solo game." (if (net-guest-p) *net-id* 0))

(defun kettle-at ()
  "Return where the kettle is for us: :TABLE, :STOVE, :PAW (in our paw) or :AWAY (in another cat's paw)."
  (let ((at (kettle :at)))
    (if (and (eq at :paw) (not (eql (kettle :holder) (my-number)))) :away at)))

(defun brew-of (kind) "Return the *BREWS* entry for the good KIND, or NIL." (assoc kind *brews*))

(defun brew-name ()
  "Return the name of what is in the kettle."
  (or (third (find (kettle :brew) *brews* :key #'second)) "чай"))

(defun can-brew-p ()
  "Return true if what we carry can go into the kettle now: it has cold water and nothing in it yet."
  (and *carry* (brew-of (car *carry*)) (kettle :water) (null (kettle :brew)) (member (kettle-at) '(:table :stove))))

;;; What the kettle can be told. WHO is whoever tells; the paws are theirs too.

(deftell :kettle :take ()
  (cond ((eq (st :at) :paw) (say "Чайник у кого-то в лапе") nil)
        (t (say "Чайник в лапе")
           (become :at :paw :holder who))))

(deftell :kettle :put (place)
  (when (and (eq (st :at) :paw) (eql (st :holder) who) (member place '(:table :stove)))
    (say (cond ((eq place :table) (if (plusp (st :tea)) "Чайник на столе. E по нему — налить чаю" "Чайник на столе"))
               ((plusp (st :tea)) "Чайник на печке")
               ((not (st :water)) "В чайнике пусто. Набери воды в умывальнике и вскипяти на печке")
               ((> *fire* 0) "Чайник на огне. Скоро закипит")
               (t "Печка холодная — подбрось доску, и чайник закипит")))
    (become :at place :holder nil)))

(deftell :kettle :fill ()
  (when (and (eq (st :at) :paw) (eql (st :holder) who))
    (say "Набрал воды. Теперь на печку: справа на ней конфорка для чайника")
    (become :water t :tea 0 :brew nil)))

(deftell :kettle :brew ()
  ;; the good goes from the paws of whoever tells into the water: both at once, here, so neither can happen alone
  (let ((entry (and *carry* (brew-of (car *carry*)))))
    (when (and entry (st :water) (null (st :brew)) (member (st :at) '(:table :stove)))
      (empty-paws) (show-carry) (save-interior)
      (say (format nil "В чайник: будет ~a. Теперь на огонь" (third entry)))
      (become :brew (second entry)))))

(deftell :kettle :pour ()                                  ; a cup of tea, or of boiling water for a jar
  (when (plusp (st :tea)) (become :tea (1- (st :tea)))))

(deftell :kettle :boiled ()                                ; the driver's stove decides
  (when (and (eql who 0) (st :water)) (become :water nil :tea *kettle-cups*)))

(deftell :kettle :dropped (number)                         ; the player NUMBER has left: what it carried is back on the table
  (when (and (eql who 0) (eq (st :at) :paw) (eql (st :holder) number))
    (become :at :table :holder nil)))

(defun use-table-kettle ()
  "Answer E on the kettle's place on the table: pour a cup, take the kettle, or put it back."
  (cond ((can-brew-p) (tell :kettle :brew))
        ((eq (kettle-at) :away) (say "Чайник у кого-то в лапе"))
        ((eq (kettle-at) :paw) (tell :kettle :put :table))
        ((plusp (kettle :tea)) (when (pour-tea) (tell :kettle :pour)))
        (t (tell :kettle :take))))

(defun use-washstand ()
  (if (eq (kettle-at) :paw) (tell :kettle :fill) (say "Умылся. Свежо")))

(defun use-ring ()
  "Answer E on the ring of the stove: put the kettle on it or take it off."
  (cond ((can-brew-p) (tell :kettle :brew))
        ((eq (kettle-at) :paw) (tell :kettle :put :stove))
        ((eq (kettle-at) :stove) (tell :kettle :take))))

(defun ring-label ()
  (when (can-brew-p) (return-from ring-label (format nil "E — положить ~a в чайник" (good-name (car *carry*)))))
  (case (kettle-at)
    (:paw "E — поставить чайник на печку")
    (:stove (cond ((plusp (kettle :tea)) "E — снять чайник: чай готов")
                  ((not (kettle :water)) "E — снять чайник: он пустой")
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

(defaktor :pan :state '(:on nil) :spec '((:member :on) (:member t nil)) :show (lambda () (show-kettle)))   ; on the stove, or on its nail

(deftell :pan :flip ()
  (cond ((and (st :on) (food-on-stove-p)) (say "Сначала сними еду со сковородки") nil)
        (t (say (if (st :on) "Сковородка висит на гвозде"
                    "Сковородка на печке: клади на неё рыбу, хлеб, клюкву или тушёнку — пожарится"))
           (become :on (not (st :on))))))

(defun toggle-pan () (tell :pan :flip))

;;; The cutting board. Cutting is done by paw: with a good in the paws every E on the board is one stroke of
;;; the knife and one more slice, and after *CHOPS* strokes the good in the paws is cut. The good never leaves
;;; the paws, so nothing can be lost half way: walk off, and only the count of strokes is forgotten. Cut, it
;;; looks cut (slices on a little board, BUILD-CUT in goods.lisp) and cooks faster.

(defparameter *chops* 5 "The number of knife strokes it takes to cut a good.")
(defvar *chop* nil "The cutting in hand: (good . strokes done), or NIL.")
(defvar *chop-nodes* nil "The nodes of the cutting, a plist: :knife :slices.")
(defvar *chop-dip* 0.0 "The seconds left of the knife's stroke.")

(defun board-point ()
  "Return where the middle of the board is: x, height above the floor, z."
  (list (- (first *washstand-at*) .02) (+ *washstand-top* .045) (- (second *washstand-at*) .6)))

(defun chopping-p ()
  "Return true if what we carry is the good whose cutting is in hand."
  (and *chop* *carry* (null (cdr *carry*)) (eq (car *carry*) (car *chop*)) (not (carry-prop :cut))))

(defun show-chop ()
  "Show on the board as many slices as there were strokes."
  (dolist (s (getf *chop-nodes* :slices)) (node-queue-free s))
  (setf (getf *chop-nodes* :slices)
        (when *chop*
          (destructuring-bind (x y z) (board-point)
            (loop for i below (cdr *chop*)
                  collect (shape *cart* :box '(.018 .07 .1) (trait (car *chop*) :cut)
                                 (+ x -.1 (* i .03)) (+ *floor* y .04) (- z .12) :rot (list 0 0 -.35) :shadow nil))))))

(defun use-board ()
  "Answer E on the cutting board: one stroke of the knife at what we carry."
  (cond ((null *carry*) (say "Разделочная доска. Принеси продукт и режь: E — удар ножа"))
        ((or (cdr *carry*) (not (cuttable-p (car *carry*)))) (say "Это не режут: режут рыбу, хлеб, грибы, яблоки, тушёнку"))
        ((carry-prop :cut) (say "Уже нарезано"))
        (t (unless (chopping-p) (setf *chop* (cons (car *carry*) 0)))             ; another good, or the first stroke
           (incf (cdr *chop*))
           (setf *chop-dip* .18)
           (cond ((< (cdr *chop*) *chops*) (show-chop))
                 (t (setf *chop* nil (carry-prop :cut) t)
                    (show-chop) (show-carry) (save-interior)
                    (say "Нарезано. На сковородке приготовится вдвое быстрее"))))))

(defun board-label ()
  (cond ((chopping-p) (format nil "E — резать: ~d из ~d" (cdr *chop*) *chops*))
        ((and *carry* (null (cdr *carry*)) (cuttable-p (car *carry*)) (not (carry-prop :cut))) (format nil "E — резать: 0 из ~d" *chops*))
        (t "Разделочная доска")))

(defun step-chop (delta)
  "Move the knife: it rests by the board and comes down with every stroke; a cutting left behind is forgotten."
  (when (and *chop* (not (chopping-p)) (<= *chop-dip* 0.0)) (setf *chop* nil) (show-chop))
  (let ((knife (getf *chop-nodes* :knife)))
    (when knife
      (setf *chop-dip* (max 0.0 (- *chop-dip* delta)))
      (destructuring-bind (x y z) (board-point)
        (let ((k (/ *chop-dip* .18)))
          (if *chop*
              (progn (node-3d-set-position knife (v3 (+ x -.1 (* .03 (cdr *chop*))) (+ *floor* y .1 (* -.07 k)) (- z .02)))
                     (node-3d-set-rotation knife (v3 (* .5 (- 1 k)) 0 0)))
              (progn (node-3d-set-position knife (v3 (+ x .13) (+ *floor* y .012) z))
                     (node-3d-set-rotation knife (v3 0 0 0)))))))))

;;; The toadstool brew. Whoever drinks it sees the wagon swim for a minute: the picture sways, the colours
;;; go round (the film shader in doroga.lisp has a knob for it) and the gnomes on their shelves dance.

(defparameter *trip-time* 60.0 "The number of seconds the toadstool brew lasts.")
(defvar *trip-until* 0.0 "The time until which the toadstool brew is at work.")
(defvar *trip-now* 0.0 "The strength of the brew as the picture shows it now, 0..1.")

(defun step-trippy ()
  "Sway the picture and the gnomes while the toadstool brew is at work; put everything back afterwards."
  (let* ((left (- *trip-until* *time*))
         (k (cond ((<= left 0) 0.0) (t (min 1.0 (/ left 6.0) (/ (- *trip-time* left -0.01) 3.0))))))   ; it comes on in three seconds and fades in six
    (when (or (plusp k) (plusp *trip-now*))
      (setf *trip-now* k)
      (when *grade* (object-call *grade* "set_shader_parameter" "trip" k))
      (loop for (i . node) in *trophies*
            do (destructuring-bind (x y z yaw) (trophy-spot i)
                 (node-3d-set-position node (v3 x (+ *floor* y (* k .05 (abs (sin (+ (* *time* 5.0) i))))) z))
                 (node-3d-set-rotation node (v3 0 (+ yaw (* k .6 (sin (+ (* *time* 3.0) (* i 1.7))))) (* k .2 (sin (+ (* *time* 4.0) i))))))))))

(defun kettle-usables ()
  "Return the things to press for the kettle and the frying pan."
  (destructuring-bind (sx sz) *stove-at*
    (remove nil
            (list
             (when (and (table-shown-p) (member (kettle-at) '(:table :paw :away)))
               (usable (- (first *table-at*) .16) (+ *table-top* .14) (second *table-at*)
                       (lambda () (cond ((can-brew-p) (format nil "E — положить ~a в чайник" (good-name (car *carry*))))
                                        ((eq (kettle-at) :away) "Чайник у кого-то в лапе")
                                        ((eq (kettle-at) :paw) "E — поставить чайник на стол")
                                        ((plusp (kettle :tea)) (format nil "E — налить: ~a · осталось стаканов: ~d" (brew-name) (kettle :tea)))
                                        ((kettle :water) "E — взять чайник: вода в нём холодная")
                                        (t "E — взять чайник: он пустой")))
                       #'use-table-kettle :radius .16))
             (usable (+ (first *washstand-at*) .1) (+ *washstand-top* .25) (second *washstand-at*)
                     (lambda () (if (eq (kettle-at) :paw) "E — набрать воды в чайник" "Умывальник. С чайником в лапе — набрать воды"))
                     #'use-washstand :radius .3)
             (destructuring-bind (rx ry rz) *kettle-ring*
               (usable (+ sx rx) (+ ry .1) (+ sz rz) #'ring-label #'use-ring :radius .2))
             (destructuring-bind (x y z) (board-point) (usable x (+ y .02) z #'board-label #'use-board :radius .24))
             (if (on-p :pan)
                 (usable (+ sx .3) (+ *stove-top* .05) sz (lambda () "E — повесить сковородку на гвоздь") #'toggle-pan :radius .1)
                 (destructuring-bind (nx ny nz) *pan-nail*
                   (usable (+ sx nx) ny (+ sz nz) (lambda () "E — поставить сковородку на печку") #'toggle-pan :radius .2)))))))

(defun step-kettle (delta)
  "Heat the kettle on the burning stove by DELTA seconds and let the steam rise once it has boiled.
Everyone counts the heat for their own hint; the driver alone decides that it has boiled."
  (save-kitchen)                                                            ; it writes only what has changed
  (when (and (eq (kettle :at) :stove) (kettle :water) (> *fire* 0))
    (when (and (>= (incf *kettle-heat* delta) *boil-time*) (not (net-guest-p)))
      (tell :kettle :boiled)
      (say-all (format nil "Чайник закипел: готов ~a. Неси на стол и разливай по стаканам" (brew-name)))))
  (step-chop delta)
  (step-trippy)
  (let ((steam (getf *kettle-nodes* :steam)))
    (when (and steam (eq (kettle :at) :stove) (plusp (kettle :tea)))
      (node-3d-set-position steam (v3 (+ .19 (* .015 (sin (* *time* 3.0)))) (+ .2 (* .03 (mod (* *time* .8) 1.0))) 0)))))

;;; ------------------------------------------------------------------ shared over the network

(defun save-kitchen ()
  "Write the kettle and the pan into the save. PUT-SAVE leaves the file alone while nothing has changed."
  (put-save :kitchen (list (if (eq (kettle :at) :paw) :table (kettle :at))                  ; carried: it will be found on the table
                             (and (kettle :water) t) (kettle :tea) (kettle :brew) (and (on-p :pan) t))))

(defun load-kitchen ()
  (let ((f (get-save :kitchen)))
    (when (fits f '((:member :table :stove) (:member t nil) integer symbol (:member t nil)))
      (destructuring-bind (at water tea brew pan) f
        (poke :kettle :at at :holder nil :water water :tea (max 0 (min *kettle-cups* tea))
                      :brew (and (find brew *brews* :key #'second) brew))
        (poke :pan :on pan)))))

(defstate :kitchen
  :clear (lambda ()
           (aktor-reset :kettle) (aktor-reset :pan)
           (setf *kettle-heat* 0.0 *chop* nil *chop-dip* 0.0 *trip-until* 0.0))
  :load #'load-kitchen :save #'save-kitchen
  :show (lambda () (show-kettle) (when *chop-nodes* (show-chop))))
