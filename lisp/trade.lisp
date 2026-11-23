;;;; Trade and parcels: everything goes through the wagon's slid-open window.
;;;;
;;;; Milk and parcels are carried in hand: LMB on an item takes it, LMB while looking at a shelf, the
;;;; windowsill or the floor puts it down. A bell hangs in the window: E on it calls a client. Four kinds of clients:
;;;;   buyer     - takes a carton of milk from the windowsill and pays;
;;;;   seller    - puts milk on the windowsill, LMB on a carton buys it;
;;;;   sender    - puts one parcel for another town on the windowsill and pays at once, when
;;;;               we take it; the farther the town, the more;
;;;;   recipient - waits at the window by himself when we park at the market of his town with a parcel for him;
;;;;               takes it from our windowsill (the money for it has already been received).
;;;; Clients come at the market in the middle of a town (stalls, the 'MARKET' sign) and at bus stops where
;;;; cats are waiting (buyers only there). Few come per stay. A window with a client at it cannot be closed.
;;;;
;;;; No money at all? Park by a bus stop or a market and turn on the music: listeners toss coins.
;;;;
;;;; Start of the game (onboarding): three parcels, four milks and just enough gas to reach the first town's market;
;;;; the gas station is right there. The step-by-step hints are in STEP-ONBOARD.
;;;;
;;;; Clients and money are counted by the driver (or the single-player game). Guests carry and arrange items.

(in-package #:kiosk-trip)


(defvar *carry* nil "What we carry in hand: (KIND . TOWN-INDEX-OR-NIL), or NIL.")
(defvar *carry-node* nil)
(defvar *carry-from* nil "The spec of the item picked up from its place (not from a client).
A guest uses it to reconcile with the driver.")
(defvar *last-put* nil "The last put-down: (SPEC TIME TAKEN-FROM).
It says what was just put down and when, and where it stood before that.")
(defvar *client* nil "The client at the window, a plist: :kind :node :price :left (seconds) :reach
:stall ((NODE Z KIND ADDR) ...) :stop :town.")
(defvar *visits* '() "The visits left per place: (PLACE . CLIENTS-STILL-TO-COME).
PLACE is the rounded path coordinate of the stay.")
(defvar *met* '() "The places where the recipient has already come up by himself (the second time, only by the bell).")
(defvar *busked* '() "The coins tossed per place: (PLACE . COINS-ALREADY-TOSSED).")
(defvar *busk-timer* 0.0)
(defvar *bell-sound* nil)
(defvar *onboard* 0 "The step of the intro hints; a large number means everything has been shown.")

;;; Flights: an item does not pop up in place, it flies over: from hand to shelf, from the windowsill to the client

(defvar *tweens* '() "The running flights: (NODE FROM TO ELAPSED DURATION FREE-AT-END).")

(defun fly (node to &key from (dur .28) free)
  "Move NODE smoothly to point TO (along an arc); with FREE, remove the node when it arrives."
  (when from (node-3d-set-position node from))
  ;; the node is already flying (put down and taken at once): cancel the earlier flight, it would touch a freed node
  (setf *tweens* (remove node *tweens* :key #'first))
  (push (list node (or from (node-3d-get-position node)) to 0.0 dur free) *tweens*))

(defun stop-flying (nodes)
  "Stop moving NODES: they are about to be removed."
  (setf *tweens* (remove-if (lambda (tw) (member (first tw) nodes)) *tweens*)))

(defun step-tweens (delta)
  (setf *tweens*
        (remove-if (lambda (tw)
                     (handler-case (step-tween tw delta)
                       (error () t)))                       ; the node vanished from under us: just drop the flight
                   *tweens*)))

(defun step-tween (tw delta)
  "Advance one flight TW by DELTA; return T when it is finished."
  (destructuring-bind (node from to tt dur free) tw
    (let* ((tt (+ tt delta)) (k (min 1.0 (/ tt dur))) (e (* k k (- 3 (* 2 k)))))
      (setf (fourth tw) tt)
      (flet ((mix (a b) (+ a (* e (- b a)))))
        (node-3d-set-position node (v3 (mix (vector3-x from) (vector3-x to))
                                       (+ (mix (vector3-y from) (vector3-y to)) (* .1 (sin (* pi k))))
                                       (mix (vector3-z from) (vector3-z to)))))
      (when (>= k 1.0) (when free (node-queue-free node)) t))))

(defun hand-point ()
  "Return where our hands are in wagon coordinates: a little ahead of and below the eyes."
  (v3 (- (vector3-x *pos*) (* .35 (sin *yaw*)))
      (+ (if (outdoors-p (vector3-x *pos*) (vector3-z *pos*)) 0.0 *floor*) (eye-height) -.3)
      (- (vector3-z *pos*) (* .35 (cos *yaw*)))))

(defun paws-point (&optional (z 0.0))
  "Return where the paws of the client standing on the step are, at offset Z."
  (v3 (+ *half-width* .5) (+ *step-y* .55) z))

;;; In hand

(defun carry-name (carry)
  (cond ((and (cdr carry) (parcel-good (cdr carry)))                       ; the plan's crate: it says what is in it
         (format nil "ящик по наряду (~a) в ~a" (good-name (parcel-good (cdr carry))) (place-name (parcel-town (cdr carry)))))
        ((cdr carry) (format nil "посылку в ~a~@[ (от: ~a)~]" (place-name (parcel-town (cdr carry))) (parcel-from (cdr carry))))
        (t (good-name (car carry)))))

(defvar *carry-props* '() "The properties of the thing in the paws, as in its record: see *PROP-KEYS* in goods.lisp.")

(defun set-carry (carry &optional props from)
  "Take CARRY, (KIND . TOWN-INDEX-OR-NIL), into the paws with its properties PROPS; FROM is the record of the thing
if it was picked up from its place. With CARRY NIL the paws are empty. What is carried, its properties and where it
came from change together, and only here. SHOW-CARRY afterwards shows it."
  (setf *carry* carry *carry-from* (and carry from) *carry-props* (and carry props)))

(defun empty-paws () "Nothing is carried any more." (set-carry nil))

(defun carry-prop (key) "Return the property KEY of the thing in the paws." (getf *carry-props* key))
(defun (setf carry-prop) (value key) (setf *carry-props* (clean-props (list* key value *carry-props*))) value)

(defun carry-spec ()
  "Return the record of the thing in the paws as it would stand: where it was taken from, or by the window."
  (destructuring-bind (x y z spin) (if *carry-from* (subseq *carry-from* 1 5) '(.6 0.0 .8 0))
    (make-spec (car *carry*) x y z spin (cdr *carry*) *carry-props*)))

(defun show-carry ()
  (unless *carry* (setf *carry-props* '()))
  (when *actor* (return-from show-carry nil))          ; an action on behalf of a guest: nothing changes in our own paws
  (when *carry-node* (node-queue-free *carry-node*) (setf *carry-node* nil))
  (when (and *carry* *camera*)
    (let ((g (group *camera* 0 -.27 -.42)))                                ; in the middle of the view, below the crosshair
      (node-3d-set-scale (build-item g (car *carry*) 0 (- *floor*) 0 0 (cdr *carry*) *carry-props*)
                         (if (cdr *carry*) (v3 .45 .45 .45) (v3 1 1 1)))
      (dolist (sx '(-.07 .07))                                              ; our own paws under it
        (shape g :ball '(.04 6 4) (nth (mod *fur* (length *cat-furs*)) *cat-furs*) sx -.02 .05 :scale '(1 .7 1.5) :shadow nil))
      (setf *carry-node* g))))

(defun portable-p (it)
  "Return true if IT is an item that is carried in hand: milk and parcels."
  (or (good-p (spec-kind (car it))) (spec-addr (car it))))

(defun item-in-sight ()
  "Return the milk or parcel we are looking at, or NIL."
  (find-if (lambda (it) (and (portable-p it)
                             (or *fridge-open* (not (in-fridge-p (car it))))       ; not through the shut fridge door
                             (multiple-value-bind (w h) (item-size (spec-kind (car it)) (spec-spin (car it)))
                               (looking-at (spec-x (car it)) (+ (spec-y (car it)) (/ h 2)) (spec-z (car it)) 2.3 (min .3 (+ .08 (* .25 w)))))))
           *items*))

(defun shelf-spot ()
  "Return where to put an item on the shelf we look at, at eye level or above: (X Z HEIGHT) or NIL.
A shelf is scanned lengthwise: the rack along x, the upper shelves and windowsills along z."
  (flet ((free-p (x z top)
           (and (looking-at x (+ top .15) z 2.4 .1)
                (notany (lambda (it) (and (< (abs (- (spec-x (car it)) x)) .3) (< (abs (- (spec-y (car it)) top)) .2)
                                          (< (abs (- (spec-z (car it)) z)) .3)))
                        *items*))))
    (loop for (cx cz w d top) in (fixture-surfaces)
          do (if (> d w)
                 (loop for z from (- cz (/ d 2) -.3) to (+ cz (/ d 2) -.3) by .2
                       do (when (free-p cx z top) (return-from shelf-spot (list cx z top))))
                 (loop for x from (- cx (/ w 2) -.3) to (+ cx (/ w 2) -.3) by .2
                       do (when (free-p x cz top) (return-from shelf-spot (list x cz top))))))))

;;; The rack. A level has three sections between the uprights. A big thing (a parcel, a crate) takes one of the
;;; two places of a section; goods are small and stand eight to a section, in four columns and two rows. Both
;;; kinds share a shelf: a place is free when nothing standing on the level reaches into it.

(defparameter *shelf-slots-x* '(-1.18 -.71 -.24 .24 .71 1.18)
  "The places for big things on one level of the rack: three sections, two places in each.")
(defparameter *shelf-sections* '(-.945 0.0 .945) "The middles of the three sections of the rack.")
(defparameter *small-slots* '((-.33 -.11 .11 .33) (-.13 .13))
  "The places for goods within a section: the offsets across it and the offsets in depth, 4 × 2 = 8.")
(defparameter *small-half* '(.1 .12) "The half-width and half-depth a good is counted to take, whatever its real size.")
(defparameter *shelf-levels* '(0.0 .55 1.1 1.65) "The levels of the rack: the floor under it and three shelves.")

(defun shelf-level (top)
  "Return the level of the rack that the height TOP belongs to: the shelf itself or the top of what stands on it."
  (find-if (lambda (lv) (< -.06 (- top lv) *level-room*)) (reverse *shelf-levels*)))

(defun shelf-half (kind spin)
  "Return the half-width and half-depth a thing of KIND takes on the rack."
  (if (good-p kind)
      (values-list *small-half*)
      (multiple-value-bind (w h d) (item-size kind spin) (declare (ignore h)) (values (/ w 2) (/ d 2)))))

(defun shelf-places (kind)
  "Return every place of one level where a thing of KIND may stand, as (X Z) entries."
  (if (good-p kind)
      (loop for cx in *shelf-sections*
            append (loop for dx in (first *small-slots*)
                         append (loop for dz in (second *small-slots*) collect (list (+ cx dx) (+ *shelf-z* dz)))))
      (loop for x in *shelf-slots-x* collect (list x *shelf-z*))))

(defparameter *rack-front* -2.2 "The z nearer than which a point belongs to the rack on the front wall.")
(defparameter *level-room* .5 "The height above a shelf up to which what stands there belongs to that shelf.")

(defun stack-here (kind x z level &optional cut)
  "Return how many things of KIND stand on one another at the place (X Z) of the shelf LEVEL.
With CUT, count the cut ones (slices on a board) instead of the whole ones: the two do not stack together."
  (count-if (lambda (it) (destructuring-bind (ikind ix iy iz &rest more) (car it)
                           (declare (ignore more))
                           (and (eq ikind kind) (eq (and cut t) (and (spec-cut-p (car it)) t))
                                (< (abs (- ix x)) .03) (< (abs (- iz z)) .03) (< -.02 (- iy level) *level-room*))))
            *items*))

(defun stack-room-p (kind x z level)
  "Return true if the whole thing of KIND that we carry may go on top of the stack of its own kind at (X Z) on LEVEL.
Cut goods do not stack, and nothing is stacked on them."
  (let ((high (trait kind :stack)))
    (and high (not (carry-prop :cut)) (zerop (stack-here kind x z level t)) (< 0 (stack-here kind x z level) high))))

(defun stack-rise (kind x z level)
  "Return how far above LEVEL the next thing of KIND stands at (X Z): the height of the stack there."
  (* (stack-here kind x z level) (nth-value 1 (item-size kind 0))))

(defun slot-taken-p (x z level kind)
  "Return true if a thing of KIND cannot stand at (X Z) on LEVEL of the rack: something there is in the way.
A stack of the same kind that is not yet full is not in the way: the thing goes on top of it."
  (multiple-value-bind (hw hd) (shelf-half kind 0)
    (or (some (lambda (it)
                (destructuring-bind (ikind ix iy iz spin &rest more) (car it)
                  (declare (ignore more))
                  (and (< -.05 (- iy level) *level-room*)                 ; whatever reaches into the place, wherever its middle is
                       (not (and (eq ikind kind) (< (abs (- ix x)) .03) (< (abs (- iz z)) .03) (stack-room-p kind x z level)))
                       (multiple-value-bind (iw id) (shelf-half ikind spin)
                         (and (< (abs (- ix x)) (- (+ hw iw) .04)) (< (abs (- iz z)) (- (+ hd id) .02)))))))
              *items*))))

(defun shelf-slot (x z level kind)
  "Return the free place on LEVEL nearest to (X Z) for a thing of KIND, as (X Z), or NIL if the level is full.
A stack of the same kind with room on top counts as a little nearer than an empty place beside it."
  (first (sort (remove-if (lambda (place) (slot-taken-p (first place) (second place) level kind)) (shelf-places kind))
               #'< :key (lambda (place) (- (+ (expt (- (first place) x) 2) (expt (- (second place) z) 2))
                                           (if (stack-room-p kind (first place) (second place) level) .01 0))))))   ; a small pull: the next place stays reachable

(defun surface-at (x z top)
  "Return the shelf or windowsill that point (X Z TOP) falls on, as (X Z WIDTH DEPTH HEIGHT).
Return NIL for the floor or the top of an item."
  (find-if (lambda (f) (destructuring-bind (cx cz w d ftop) f
                         (and (< (abs (- top ftop)) .03) (< (abs (- x cx)) (/ w 2)) (< (abs (- z cz)) (/ d 2)))))
           (fixture-surfaces)))

(defun spot-problem (spot)
  "Return why what is in hand cannot be put at SPOT: text for the player, or NIL if it can."
  (when (fourth spot)
    (return-from spot-problem
      (case (fourth spot)
        (:full "На этом этаже стеллажа мест нет: в секции восемь товаров или две посылки")
        (:fridge-shut "Холодильник закрыт: E по дверце")
        (:fridge-big "В холодильник влезают только продукты")
        (:fridge-full "На этой полке холодильника мест нет: по четыре на полку")
        (t "Сюда не встаёт — выбери свободное место на полке"))))
  (destructuring-bind (x z top) spot
    (multiple-value-bind (w h d) (item-size (car *carry*) 0)
      (declare (ignore h))
      (let ((surface (surface-at x z top)))
        (cond ((>= (length *items*) *max-items*) "Больше в кибитку не влезет")
              ((and *client* (< (abs (- top .95)) .05)                  ; on the shared windowsill: not onto the client's goods
                    (some (lambda (entry) (and (< (abs (- z (offer-z entry))) .22) (< (abs (- x *half-width* .29)) .3)))
                          (getf *client* :stall)))
               "Тут лежит товар клиента — поставь рядом")
              ((and (< z *rack-front*) (shelf-level top)) nil)                ; the rack: the place has already been chosen by slots
              ((fridge-shelf x z top) nil)                            ; the fridge: likewise
              ((some (lambda (it)                                   ; no putting one item inside another
                       (destructuring-bind (kind ix iy iz spin &rest more) (car it)
                         (declare (ignore more))
                         (multiple-value-bind (iw ih id) (item-size kind spin)
                           (and (< (abs (- iy top)) (max .04 (* .5 ih)))
                                (< (abs (- ix x)) (- (/ (+ w iw) 2) .02)) (< (abs (- iz z)) (- (/ (+ d id) 2) .02))))))
                     *items*)
               "Тут уже что-то стоит — поставь рядом")
              ((and surface (< (third surface) .6) (< (fifth surface) 1.5)   ; a windowsill (the rack is counted by slots, the upper shelves are not counted)
                    (>= (count-if (lambda (it) (equal (surface-at (spec-x (car it)) (spec-z (car it)) (spec-y (car it))) surface)) *items*)
                        *sill-limit*))
               "На подоконнике больше нет места"))))))

(defun put-spot ()
  "Return where what is in hand will land: (X Z HEIGHT) or NIL."
  (let* ((a (aim-point))
         (raw (cond (a (list (snap (first a)) (snap (second a)) (third a)))
                    (t (shelf-spot)))))
    (cond ((null raw) nil)
          ((fridge-shelf (first raw) (second raw) (third raw))        ; the fridge: the nearest free place of the shelf
           (let ((level (fridge-shelf (first raw) (second raw) (third raw))))
             (cond ((not *fridge-open*) (append raw (list :fridge-shut)))
                   ((not (good-p (car *carry*))) (append raw (list :fridge-big)))
                   (t (let ((place (fridge-slot (first raw) (second raw) level (car *carry*))))
                        (if place
                            (list (first place) (second place) (+ level (stack-rise (car *carry*) (first place) (second place) level)))
                            (append raw (list :fridge-full))))))))
          ((< (second raw) *rack-front*)                                   ; the rack: the item goes to the nearest free slot of the level
           (let ((level (shelf-level (third raw))))
             (cond ((null level) (append raw (list :stack)))
                   (t (let ((place (shelf-slot (first raw) (second raw) level (car *carry*))))
                        (if place
                            (list (first place) (second place) (+ level (stack-rise (car *carry*) (first place) (second place) level)))
                            (append raw (list :full))))))))
          (t raw))))

(defun carry-use ()
  "Take milk or a parcel, or put down what we carry (left mouse button).
Return T if something was done."
  (cond (*carry*
         (let* ((spot (put-spot)) (problem (and spot (spot-problem spot))))
           (cond (problem (say problem))
                 (spot
                  (let ((spec (make-spec (car *carry*) (float (first spot) 1.0) (float (third spot) 1.0) (float (second spot) 1.0) 0
                                         (cdr *carry*) *carry-props*)))
                    (let ((node (stand-item spec)))
                      (fly node (node-3d-get-position node) :from (hand-point)))      ; from hand to its place
                    (setf *last-put* (list spec *time* *carry-from*)))
                  (empty-paws) (show-carry)
                  (things-changed))
                 (t (say "Посмотри, куда поставить: полка, подоконник, пол")))
           t))
        (t (let ((it (item-in-sight)))
             (when it
               (fly (cdr it) (hand-point) :dur .2 :free t)                           ; from its place into hand
               (lift-item it) (setf *took* (car it))
               (set-carry (cons (spec-kind (car it)) (spec-addr (car it))) (spec-props (car it)) (car it))
               (show-carry) (things-changed t)
               t)))))

(defun reconcile-carry (specs)
  "Reconcile what a guest holds after the driver sent a layout SPECS that differs from the guest's own.
That means the guest's edit was rejected (someone got there first). An item just taken still stands at
the driver's, so the hands go empty, or there would be a copy; an item just put down is missing at the
driver's, so it returns to the hands, or it would be lost."
  (cond ((and *carry* *carry-from* (member *carry-from* specs :test #'equal))
         (empty-paws) (show-carry)
         (say "Эту вещь взял кто-то другой"))
        ((and (not *carry*) *last-put* (< (- *time* (second *last-put*)) 3.0)
              (not (member (first *last-put*) specs :test #'equal))
              (not (and (third *last-put*) (member (third *last-put*) specs :test #'equal))))   ; unless it has returned to its old place
         (set-carry (cons (spec-kind (first *last-put*)) (spec-addr (first *last-put*))) (spec-props (first *last-put*)))
         (setf *last-put* nil)
         (show-carry)
         (say "Поставить не вышло — вещь снова в руках"))))

(defun stow-carry ()
  "Return what is in hand to where it was taken from (or to the window) before joining another game."
  (when *carry*
    (stand-item (carry-spec))
    (empty-paws)
    (show-carry) (things-changed)))

(defun carry-hint ()
  (cond (*carry* (if (and (put-spot) (not (spot-problem (put-spot))))
                     (format nil "ЛКМ — поставить ~a" (carry-name *carry*))
                     (format nil "В руках ~a · посмотри, куда поставить" (carry-name *carry*))))
        (t (let ((it (item-in-sight)))
             (when it (format nil "ЛКМ — взять ~a" (carry-name (cons (spec-kind (car it)) (spec-addr (car it))))))))))

;;; Music for coins

(defun step-busk (delta)
  (let ((place (and (not (net-guest-p)) (or *playing* *guitar-on*) (place-here))))
    (when (and place (<= (decf *busk-timer* (* delta (if *guitar-on* *busk-live* 1))) 0))        ; live music: twice as often
      (setf *busk-timer* *busk-every*)
      (let* ((key (place-key place))
             (pot (if (market-p place) 14 (+ 4 (* 3 (length (stop-cats (place-stop place)))))))
             (got (or (assoc key *busked*) (first (push (cons key 0) *busked*)))))
        (when (< (cdr got) pot)
          (incf (cdr got)) (add-money 1)
          (when (= (cdr got) 1) (log-event :busk :dist (round *dist*) :money *money*))
          (say (if (< (cdr got) pot) "Котам нравится музыка: +5 коп." "Котам нравится музыка: +5 коп. Больше тут не накидают")))))))

;;; Intro hints: one step at a time, once the previous message has faded

(defun step-onboard ()
  (unless (or (net-guest-p) *lobby-open* (>= *onboard* 9) (and *toast* (> (cdr *toast*) 0))
              *show*)                 ; a card is on the screen: a hint under it would be lost
    (let ((market (let ((p (place-here))) (and p (market-p p) (zerop (place-town p))))))
      (case *onboard*
        (0 (when (> *time* 2)
             (say "В кузове три посылки и молоко. Бензина — только до рынка в Заречном") (setf *onboard* 1)))
        (1 (say "M — карта: выбери город, грузовик поедет сам. S — остановить. V — вид изнутри") (setf *onboard* 2))
        (2 (when market (setf *onboard* 3)))                               ; the gas station at the market speaks for itself (city.lisp)
        (3 (when market
             (say "V — внутрь. Раздвинь окно: E по стеклу. Колокольчик в проёме зовёт клиентов") (setf *onboard* 4)))
        (4 (when (and market (> *window-k* .9))
             (say "ЛКМ — взять вещь с полки и поставить на подоконник. E — всё остальное") (setf *onboard* 5)))
        (5 (cond ((and market (< *money* 5))
                  (say "Денег мало — включи музыку (диск в проигрыватель): слушатели накидают монет") (setf *onboard* 6))
                 ((not market) (setf *onboard* 6))))                        ; never short of money here: on to the next hint all the same
        (6 (when (> *dist* (+ (town-stop-d (first *towns*)) 60))
             (say "Куда везти посылки — на карте у двери. Бумаги кооператива — в папке под картой. N — блокнот: всё, что тебе говорили") (setf *onboard* 9)))))))

;;; Every frame, and the things within reach

(defun step-trade (&optional (delta .016))
  (step-tweens delta)
  (forget-far-places)
  (step-client-walk delta)
  (unless (net-guest-p)
    (step-client delta)
    (step-busk delta)
    (step-onboard)))

(defun trade-usables ()
  "Return the trade usables.
These are the bell in the slid-open window (E) and what the client put on the windowsill (LMB)."
  (when (> *window-k* .9)
    (append
     (list (usable (+ *half-width* .02) 1.8 0.0 (lambda () "E — позвонить в колокольчик") #'ring-bell))
     (when *client*
       (loop for entry in (getf *client* :stall)
             collect (let ((entry entry) (sender (eq (getf *client* :kind) :sender)))
                       (usable (+ *half-width* .29) (if sender 1.17 1.07) (offer-z entry)
                             (lambda ()
                               (cond ((eq (offer-kind entry) :coins)
                                      (if (and *carry* (null (cdr *carry*)) (wanted-p (car *carry*)))
                                          (format nil "ЛКМ — отдать за ~/kiosk-trip::rub/" (good-price (car *carry*) (getf *client* :price)))
                                          (format nil "Покупатель ищет ~a, даёт ~/kiosk-trip::rub/: возьми с полки (ЛКМ)" (good-name (client-good)) (getf *client* :price))))
                                     (t (if sender
                                   (if (parcel-good (offer-addr entry))
                                       (format nil (if (paper-p :account) "ЛКМ — взять ящик по наряду: ~a в ~a · платит ~/kiosk-trip::rub/ на счёт" "ЛКМ — взять ящик по наряду: ~a в ~a · платит ~/kiosk-trip::rub/") (good-name (parcel-good (offer-addr entry))) (place-name (parcel-town (offer-addr entry))) (getf *client* :price))
                                       (format nil "ЛКМ — взять посылку в ~a · платит ~/kiosk-trip::rub/" (place-name (parcel-town (offer-addr entry))) (getf *client* :price)))
                                   (format nil "ЛКМ — купить ~a за ~/kiosk-trip::rub/ · у тебя ~/kiosk-trip::rub/" (good-name (offer-kind entry)) (getf *client* :price) *money*)))))
                             (lambda () (take-from-stall entry))
                             :hand)))))))

(defun make-bell-sound (root)
  "Build the bell sound under ROOT: a decaying chord of two sines, no file needed."
  (let* ((rate 22050) (n (round (* rate .7))) (data (make-array (* 2 n) :element-type '(unsigned-byte 8)))
         (s (gdext:make-object "AudioStreamWAV")))
    (dotimes (i n)
      (let* ((tt (/ i rate 1.0))
             (v (round (* 9000 (exp (* -6.0 tt)) (+ (sin (* 2 pi 1568 tt)) (* .5 (sin (* 2 pi 3136 tt)))))))
             (u (if (< v 0) (+ v 65536) v)))
        (setf (aref data (* 2 i)) (logand u 255) (aref data (1+ (* 2 i))) (ash u -8))))
    (object-set s "format" 1) (object-set s "mix_rate" rate) (object-set s "data" data)
    (setf *bell-sound* (player root s :db -8.0 :bus "Кибитка" :play nil))))

(defun window-busy-p ()
  "Return true if the window cannot be closed now: a client is at it or items stand on the windowsill."
  (or *client*
      (find-if (lambda (it) (and (> (spec-x (car it)) 1.15) (< (abs (- (spec-y (car it)) .95)) .12))) *items*)))

(defun forget-far-places ()
  "Forget the places we have driven far away from.
The count of clients and coins at a place is reset only once we are more than a kilometre and a half
away from it: coming back with a 'U-turn' for another round costs gas."
  (flet ((near-p (entry) (< (abs (- (car entry) *dist*)) 1500)))
    (setf *visits* (remove-if-not #'near-p *visits*)
          *busked* (remove-if-not #'near-p *busked*)
          *met* (remove-if-not (lambda (k) (< (abs (- k *dist*)) 1500)) *met*))))

(defun reset-trade (&optional new-game)
  "Remove the client and everything in flight.
Their nodes are children of the wagon and will not vanish by themselves. With NEW-GAME, also reset
the per-place counts."
  (when *client* (client-off *client*) (setf *client* nil))
  (dolist (l *leavers*) (node-queue-free (first l)))
  (dolist (tw *tweens*) (when (sixth tw) (node-queue-free (first tw))))
  (setf *leavers* '() *tweens* '() *client-seen* nil)
  (unless new-game (return-from reset-trade))
  (setf *visits* '() *busked* '() *met* '()))
