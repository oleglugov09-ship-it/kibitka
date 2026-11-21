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
;;;; someone carries the kettle, the others see it gone (:AWAY) and know who has it, so when that cat leaves
;;;; the kettle is back on the table. The driver decides when it has boiled.
;;;; Both are saved with the world (DEFSTATE :KITCHEN below): where the kettle stands, the water or the tea in
;;;; it, whether the pan is on the stove. A kettle that was in a paw is back on the table, and water that was
;;;; heating starts from cold. A new game empties the kettle and hangs the pan on its nail.

(in-package #:kiosk-trip)

(defparameter *boil-time* 20.0 "The number of seconds the kettle takes to boil on a burning stove.")
(defparameter *kettle-cups* 4 "The number of cups of tea in one boiled kettle.")
(defparameter *kettle-ring* '(0.0 .795 -.49) "The ring for the kettle, relative to the stove: x, height above the floor, z.")
(defparameter *pan-nail* '(.28 1.5 .385) "The nail the frying pan hangs on, relative to the stove: x, height above the floor, z.")

(defvar *kettle-at* :table "Where the kettle is: :TABLE, :STOVE, :PAW (in our paw) or :AWAY (in another cat's paw).")
(defvar *kettle-holder* nil "The number of the player who carries the kettle (0 is the driver), or NIL.")
(defvar *kettle-water* nil "True when there is cold water in the kettle.")
(defvar *kettle-heat* 0.0 "The number of seconds the water has been heated.")
(defvar *kettle-tea* 0 "The number of cups of tea left in the kettle.")
(defvar *kettle-brew* nil "What was put into the water: NIL for plain tea, or a brew key from *BREWS* (:COMPOTE, :HERBAL).")
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
             (shape g :cyl '(.17 .17 .012 14) "#2c2c32" 0 .04 0 :metal .3 :rough .5 :shadow nil)        ; a centimetre proud of the pan under it
             (shape g :box '(.24 .02 .035) iron .3 .03 0 :metal .4 :rough .7)))
    g))

(defun show-kettle ()
  "Show the kettle where it is and the frying pan where it is."
  (when (and *camera* *kettle-nodes* (null (getf *kettle-nodes* :paw)))        ; the camera is made after the wagon: the paw kettle waits for it
    (setf (getf *kettle-nodes* :paw) (kettle-model *camera* 0 -.29 -.44)))
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
                  :paw nil)                                                ; made by SHOW-KETTLE once there is a camera
            *pan-nodes*
            (destructuring-bind (nx ny nz) *pan-nail*
              (let ((lid (group c (+ sx nx) (+ f ny .36) (+ sz nz))))                ; the lid on its own nail, above the pan
                (shape lid :cyl '(.15 .15 .02 14) "#2a2a30" 0 0 0 :rot (list (/ pi 2) 0 0) :metal .4 :rough .6)
                (shape lid :ball '(.03 6 4) "#1e1e22" 0 0 -.03 :shadow nil)
                (list :wall (pan-model c (+ sx nx) (+ f ny) (+ sz nz) :hanging t)
                      :stove (pan-model c sx (+ f *stove-top*) sz)
                      :lid lid))))))
  (let ((knife (group c 0 0 0)))                                             ; the knife of the cutting board
    (shape knife :box '(.03 .004 .18) "#b8bcc0" 0 0 -.05 :metal .7 :rough .3 :shadow nil)
    (shape knife :box '(.03 .015 .1) "#3a2a1e" 0 .004 .09 :shadow nil)
    (setf *chop-nodes* (list :knife knife :slices '())))
  (show-chop)
  (show-kettle))

;;; ------------------------------------------------------------------ what E does

(defun my-number () "Return our number among the players: 0 for the driver and for a solo game." (if (net-guest-p) *net-id* 0))

(defun player-here-p (number)
  "Return true if the player NUMBER is in the game: ourselves or one of the others."
  (or (eql number (my-number)) (and (member number *net-others* :key #'first) t)))

(defun move-kettle (to text)
  (setf *kettle-at* to *kettle-heat* 0.0 *kettle-holder* (and (eq to :paw) (my-number)))
  (show-kettle)
  (say text))

(defun brew-of (kind) "Return the *BREWS* entry for the good KIND, or NIL." (assoc kind *brews*))

(defun brew-name ()
  "Return the name of what is in the kettle."
  (or (third (find *kettle-brew* *brews* :key #'second)) "чай"))

(defun can-brew-p ()
  "Return true if what we carry can go into the kettle now: it has cold water and nothing in it yet."
  (and *carry* (brew-of (car *carry*)) *kettle-water* (null *kettle-brew*) (member *kettle-at* '(:table :stove))))

(defun add-brew ()
  "Put the good from the paws into the kettle."
  (let ((entry (brew-of (car *carry*))))
    (setf *kettle-brew* (second entry) *carry* nil *carry-from* nil)
    (show-carry) (save-interior)
    (say (format nil "В чайник: будет ~a. Теперь на огонь" (third entry)))))

(defun use-table-kettle ()
  "Answer E on the kettle's place on the table: pour a cup, take the kettle, or put it back."
  (cond ((can-brew-p) (add-brew))
        ((eq *kettle-at* :away) (say "Чайник у кого-то в лапе"))
        ((eq *kettle-at* :paw) (move-kettle :table (if (plusp *kettle-tea*) "Чайник на столе. E по нему — налить чаю" "Чайник на столе")))
        ((plusp *kettle-tea*) (when (pour-tea) (decf *kettle-tea*)))
        (t (move-kettle :paw "Чайник в лапе"))))

(defun use-washstand ()
  (cond ((not (eq *kettle-at* :paw)) (say "Умылся. Свежо"))
        (t (setf *kettle-water* t *kettle-heat* 0.0 *kettle-tea* 0 *kettle-brew* nil)
           (say "Набрал воды. Теперь на печку: справа на ней конфорка для чайника"))))

(defun use-ring ()
  "Answer E on the ring of the stove: put the kettle on it or take it off."
  (when (can-brew-p) (return-from use-ring (add-brew)))
  (case *kettle-at*
    (:paw (move-kettle :stove (cond ((plusp *kettle-tea*) "Чайник на печке")
                                    ((not *kettle-water*) "В чайнике пусто. Набери воды в умывальнике и вскипяти на печке")
                                    ((> *fire* 0) "Чайник на огне. Скоро закипит")
                                    (t "Печка холодная — подбрось доску, и чайник закипит"))))
    (:stove (move-kettle :paw "Чайник в лапе"))))

(defun ring-label ()
  (when (can-brew-p) (return-from ring-label (format nil "E — положить ~a в чайник" (good-name (car *carry*)))))
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
           (when (audio :steps) (object-call (audio :steps) "play"))            ; a knock on the board
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
             (when (and (table-shown-p) (member *kettle-at* '(:table :paw :away)))
               (usable (- (first *table-at*) .16) (+ *table-top* .14) (second *table-at*)
                       (lambda () (cond ((can-brew-p) (format nil "E — положить ~a в чайник" (good-name (car *carry*))))
                                        ((eq *kettle-at* :away) "Чайник у кого-то в лапе")
                                        ((eq *kettle-at* :paw) "E — поставить чайник на стол")
                                        ((plusp *kettle-tea*) (format nil "E — налить: ~a · осталось стаканов: ~d" (brew-name) *kettle-tea*))
                                        (*kettle-water* "E — взять чайник: вода в нём холодная")
                                        (t "E — взять чайник: он пустой")))
                       #'use-table-kettle nil 2.6 .16))
             (usable (+ (first *washstand-at*) .1) (+ *washstand-top* .25) (second *washstand-at*)
                     (lambda () (if (eq *kettle-at* :paw) "E — набрать воды в чайник" "Умывальник. С чайником в лапе — набрать воды"))
                     #'use-washstand nil 2.6 .3)
             (destructuring-bind (rx ry rz) *kettle-ring*
               (usable (+ sx rx) (+ ry .1) (+ sz rz) #'ring-label #'use-ring nil 2.6 .2))
             (destructuring-bind (x y z) (board-point) (usable x (+ y .02) z #'board-label #'use-board nil 2.6 .24))
             (if *pan-on*
                 (usable (+ sx .3) (+ *stove-top* .05) sz (lambda () "E — повесить сковородку на гвоздь") #'toggle-pan nil 2.6 .1)
                 (destructuring-bind (nx ny nz) *pan-nail*
                   (usable (+ sx nx) ny (+ sz nz) (lambda () "E — поставить сковородку на печку") #'toggle-pan nil 2.6 .2)))))))

(defun step-kettle (delta)
  "Heat the kettle on the burning stove by DELTA seconds and let the steam rise once it has boiled.
Everyone counts the heat for their own hint; the driver alone decides that it has boiled."
  (when (and (eq *kettle-at* :away) (not (player-here-p *kettle-holder*)))   ; whoever carried it has left: it is back on the table
    (setf *kettle-at* :table *kettle-holder* nil) (show-kettle))
  (save-kitchen)                                                            ; it writes only what has changed
  (when (and (eq *kettle-at* :stove) *kettle-water* (> *fire* 0))
    (when (and (>= (incf *kettle-heat* delta) *boil-time*) (not (net-guest-p)))
      (setf *kettle-water* nil *kettle-heat* 0.0 *kettle-tea* *kettle-cups*)
      (show-kettle)
      (say-all (format nil "Чайник закипел: готов ~a. Неси на стол и разливай по стаканам" (brew-name)))))
  (step-chop delta)
  (step-trippy)
  (let ((steam (getf *kettle-nodes* :steam)))
    (when (and steam (eq *kettle-at* :stove) (plusp *kettle-tea*))
      (node-3d-set-position steam (v3 (+ .19 (* .015 (sin (* *time* 3.0)))) (+ .2 (* .03 (mod (* *time* .8) 1.0))) 0)))))

;;; ------------------------------------------------------------------ shared over the network

(defcommon :kettle
  :value (list (if (eq *kettle-at* :away) :paw *kettle-at*) (and *kettle-water* t) *kettle-tea* *kettle-brew* *kettle-holder*)
  :spec '((:member :table :stove :paw) (:member t nil) integer symbol (:or integer null))
  :accept (lambda (state)
            (destructuring-bind (at water tea brew holder) state
              ;; 'in a paw' is ours if the holder is us and somebody else's otherwise
              (let ((at (cond ((not (eq at :paw)) at) ((eql holder (my-number)) :paw) (t :away)))
                    (tea (max 0 (min *kettle-cups* tea))))
                (setf *kettle-holder* (and (member at '(:paw :away)) holder))
                (unless (and (eq at *kettle-at*) (eq water (and *kettle-water* t)) (= tea *kettle-tea*) (eq brew *kettle-brew*))
                  (when (or (not (eq at *kettle-at*)) (and water (not *kettle-water*))) (setf *kettle-heat* 0.0))
                  (setf *kettle-at* at *kettle-water* water *kettle-tea* tea
                        *kettle-brew* (and (find brew *brews* :key #'second) brew))
                  (show-kettle))))))

(defcommon :pan :value (and *pan-on* t) :spec '(:member t nil)
  :accept (lambda (on) (unless (eq on (and *pan-on* t)) (setf *pan-on* on) (show-kettle))))

(defun save-kitchen ()
  "Write the kettle and the pan into the save. PUT-SAVE leaves the file alone while nothing has changed."
  (unless (net-guest-p)
    (put-save :kitchen (list (if (member *kettle-at* '(:paw :away)) :table *kettle-at*)       ; carried: it will be found on the table
                             (and *kettle-water* t) *kettle-tea* *kettle-brew* (and *pan-on* t)))))

(defun load-kitchen ()
  (let ((f (get-save :kitchen)))
    (when (fits f '((:member :table :stove) (:member t nil) integer symbol (:member t nil)))
      (destructuring-bind (at water tea brew pan) f
        (setf *kettle-at* at *kettle-water* water *kettle-tea* (max 0 (min *kettle-cups* tea))
              *kettle-brew* (and (find brew *brews* :key #'second) brew) *pan-on* pan)))))

(defstate :kitchen
  :clear (lambda ()
           (setf *kettle-at* :table *kettle-holder* nil *kettle-water* nil *kettle-heat* 0.0 *kettle-tea* 0 *kettle-brew* nil
                 *pan-on* nil *chop* nil *chop-dip* 0.0 *trip-until* 0.0))
  :load #'load-kitchen :save #'save-kitchen
  :show (lambda () (show-kettle) (when *chop-nodes* (show-chop))))
