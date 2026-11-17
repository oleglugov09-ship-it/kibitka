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
      (+ (if (outdoors-p (vector3-x *pos*) (vector3-z *pos*)) 0.0 *floor*) (if *seat* (fifth *seat*) *eye*) -.3)
      (- (vector3-z *pos*) (* .35 (cos *yaw*)))))

(defun paws-point (&optional (z 0.0))
  "Return where the paws of the client standing on the step are, at offset Z."
  (v3 (+ *half-width* .5) (+ *step-y* .55) z))

;;; In hand

(defun carry-name (carry)
  (if (cdr carry)
      (format nil "посылку в ~a~@[ (от: ~a)~]" (town-name (parcel-town (cdr carry))) (parcel-from (cdr carry)))
      (good-name (car carry))))

(defun show-carry ()
  (when *actor* (return-from show-carry nil))          ; an action on behalf of a guest: nothing changes in our own paws
  (when *carry-node* (node-queue-free *carry-node*) (setf *carry-node* nil))
  (when (and *carry* *camera*)
    (let ((g (group *camera* .22 -.3 -.5)))
      (node-3d-set-scale (build-item g (car *carry*) 0 (- *floor*) 0 0 (cdr *carry*))
                         (if (cdr *carry*) (v3 .45 .45 .45) (v3 1 1 1)))
      (setf *carry-node* g))))

(defun portable-p (it)
  "Return true if IT is an item that is carried in hand: milk and parcels."
  (or (good-p (first (car it))) (sixth (car it))))

(defun item-in-sight ()
  "Return the milk or parcel we are looking at, or NIL."
  (find-if (lambda (it) (and (portable-p it)
                             (multiple-value-bind (w h) (item-size (first (car it)) (fifth (car it)))
                               (looking-at (second (car it)) (+ (third (car it)) (/ h 2)) (fourth (car it)) 2.3 (min .3 (+ .08 (* .25 w)))))))
           *items*))

(defun shelf-spot ()
  "Return where to put an item on the shelf we look at, at eye level or above: (X Z HEIGHT) or NIL.
A shelf is scanned lengthwise: the rack along x, the upper shelves and windowsills along z."
  (flet ((free-p (x z top)
           (and (looking-at x (+ top .15) z 2.4 .1)
                (notany (lambda (it) (and (< (abs (- (second (car it)) x)) .3) (< (abs (- (third (car it)) top)) .2)
                                          (< (abs (- (fourth (car it)) z)) .3)))
                        *items*))))
    (loop for (cx cz w d top) in (fixture-surfaces)
          do (if (> d w)
                 (loop for z from (- cz (/ d 2) -.3) to (+ cz (/ d 2) -.3) by .2
                       do (when (free-p cx z top) (return-from shelf-spot (list cx z top))))
                 (loop for x from (- cx (/ w 2) -.3) to (+ cx (/ w 2) -.3) by .2
                       do (when (free-p x cz top) (return-from shelf-spot (list x cz top))))))))

(defparameter *shelf-slots-x* '(-1.18 -.71 -.24 .24 .71 1.18)
  "The slots on one level of the rack: three sections between the uprights, two slots in each.")
(defparameter *shelf-levels* '(0.0 .55 1.1 1.65) "The levels of the rack: the floor under it and three shelves; 4 × 6 = 24 slots in all.")

(defun shelf-level (top) (find-if (lambda (lv) (< (abs (- lv top)) .06)) *shelf-levels*))

(defun slot-taken-p (x level)
  (or (and (= level *jukebox-y*) (< x -.4))                              ; the record player and the box of discs stand here
      (some (lambda (it) (and (< (fourth (car it)) -2.2) (< (abs (- (second (car it)) x)) .22)
                              (< (abs (- (third (car it)) level)) .1)))
            *items*)))

(defun shelf-slot (x level)
  "Return the free slot on LEVEL nearest to X, or NIL if the level is full."
  (first (sort (remove-if (lambda (sx) (slot-taken-p sx level)) (copy-list *shelf-slots-x*))
               #'< :key (lambda (sx) (abs (- sx x))))))

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
      (if (eq (fourth spot) :full) "На этом этаже стеллажа мест нет: по два в секции" "На стеллаже вещи стоят в один ряд — выбери свободное место")))
  (destructuring-bind (x z top) spot
    (multiple-value-bind (w h d) (item-size (car *carry*) 0)
      (declare (ignore h))
      (let ((surface (surface-at x z top)))
        (cond ((>= (length *items*) *max-items*) "Больше в кибитку не влезет")
              ((and *client* (< (abs (- top .95)) .05)                  ; on the shared windowsill: not onto the client's goods
                    (some (lambda (entry) (and (< (abs (- z (second entry))) .22) (< (abs (- x *half-width* .29)) .3)))
                          (getf *client* :stall)))
               "Тут лежит товар клиента — поставь рядом")
              ((and (< z -2.2) (shelf-level top)) nil)                ; the rack: the place has already been chosen by slots
              ((some (lambda (it)                                   ; no putting one item inside another
                       (destructuring-bind (kind ix iy iz spin &rest more) (car it)
                         (declare (ignore more))
                         (multiple-value-bind (iw ih id) (item-size kind spin)
                           (and (< (abs (- iy top)) (max .04 (* .5 ih)))
                                (< (abs (- ix x)) (- (/ (+ w iw) 2) .02)) (< (abs (- iz z)) (- (/ (+ d id) 2) .02))))))
                     *items*)
               "Тут уже что-то стоит — поставь рядом")
              ((and surface (< (third surface) .6) (< (fifth surface) 1.5)   ; a windowsill (the rack is counted by slots, the upper shelves are not counted)
                    (>= (count-if (lambda (it) (equal (surface-at (second (car it)) (fourth (car it)) (third (car it))) surface)) *items*)
                        *sill-limit*))
               "На подоконнике больше нет места"))))))

(defun put-spot ()
  "Return where what is in hand will land: (X Z HEIGHT) or NIL."
  (let* ((a (aim-point))
         (raw (cond (a (list (snap (first a)) (snap (second a)) (third a)))
                    (t (shelf-spot)))))
    (cond ((null raw) nil)
          ((< (second raw) -2.2)                                   ; the rack: the item goes to the nearest free slot of the level
           (let ((level (shelf-level (third raw))))
             (cond ((null level) (append raw (list :stack)))
                   (t (let ((sx (shelf-slot (first raw) level)))
                        (if sx (list sx *shelf-z* level) (append raw (list :full))))))))
          (t raw))))

(defun carry-use ()
  "Take milk or a parcel, or put down what we carry (left mouse button).
Return T if something was done."
  (cond (*carry*
         (let* ((spot (put-spot)) (problem (and spot (spot-problem spot))))
           (cond (problem (say problem))
                 (spot
                  (let ((spec (append (list (car *carry*) (float (first spot) 1.0) (float (third spot) 1.0) (float (second spot) 1.0) 0)
                                      (when (cdr *carry*) (list (cdr *carry*))))))
                    (let ((node (apply #'build-item *cart* spec)))
                      (push (cons spec node) *items*)
                      (fly node (node-3d-get-position node) :from (hand-point)))      ; from hand to its place
                    (setf *last-put* (list spec *time* *carry-from*)))
                  (setf *carry* nil *carry-from* nil) (show-carry)
                  (refresh-seats) (save-interior))
                 (t (say "Посмотри, куда поставить: полка, подоконник, пол")))
           t))
        (t (let ((it (item-in-sight)))
             (when it
               (fly (cdr it) (hand-point) :dur .2 :free t)                           ; from its place into hand
               (setf *items* (remove it *items*) *carry* (cons (first (car it)) (sixth (car it))) *carry-from* (car it)
                     *took* (car it))
               (show-carry) (settle-items) (refresh-seats) (save-interior)
               t)))))

(defun client-hint ()
  "Return the hint on what to do with the client at the window.
It is shown while we are not looking at anything specific."
  (when (and *client* (> (getf *client* :arrive 0.0) 0)) (return-from client-hint "Клиент идёт к окну"))
  (when *client*
    (let ((price (getf *client* :price)))
      (ecase (getf *client* :kind)
        (:buyer (if *carry*
                    (format nil "Покупатель ищет ~a: ЛКМ по монетам на подоконнике — отдать" (good-name (client-good)))
                    (format nil "Покупатель ищет ~a, даёт ~d р.: возьми с полки (ЛКМ) и принеси к окну" (good-name (client-good)) price)))
        (:seller (format nil "Продавец: ~a по ~d р. — ЛКМ по товару на подоконнике" (good-name (client-good)) price))
        (:sender (format nil "Отправитель платит ~d р.: ЛКМ по посылке на подоконнике" price))
        (:recipient (format nil "Получатель ждёт посылку в ~a: возьми её с полки (ЛКМ) и поставь на подоконник"
                            (town-name (getf *client* :town))))))))

(defun reconcile-carry (specs)
  "Reconcile what a guest holds after the driver sent a layout SPECS that differs from the guest's own.
That means the guest's edit was rejected (someone got there first). An item just taken still stands at
the driver's, so the hands go empty, or there would be a copy; an item just put down is missing at the
driver's, so it returns to the hands, or it would be lost."
  (cond ((and *carry* *carry-from* (member *carry-from* specs :test #'equal))
         (setf *carry* nil *carry-from* nil) (show-carry)
         (say "Эту вещь взял кто-то другой"))
        ((and (not *carry*) *last-put* (< (- *time* (second *last-put*)) 3.0)
              (not (member (first *last-put*) specs :test #'equal))
              (not (and (third *last-put*) (member (third *last-put*) specs :test #'equal))))   ; unless it has returned to its old place
         (setf *carry* (cons (first (first *last-put*)) (sixth (first *last-put*))) *carry-from* nil *last-put* nil)
         (show-carry)
         (say "Поставить не вышло — вещь снова в руках"))))

(defun stow-carry ()
  "Return what is in hand to where it was taken from (or to the window) before joining another game."
  (when *carry*
    (let ((spec (or *carry-from* (append (list (car *carry*) .6 0.0 .8 0) (when (cdr *carry*) (list (cdr *carry*)))))))
      (push (cons spec (apply #'build-item *cart* spec)) *items*))
    (setf *carry* nil *carry-from* nil)
    (show-carry) (refresh-seats) (save-interior)))

(defun carry-hint ()
  (cond (*carry* (if (and (put-spot) (not (spot-problem (put-spot))))
                     (format nil "ЛКМ — поставить ~a" (carry-name *carry*))
                     (format nil "В руках ~a · посмотри, куда поставить" (carry-name *carry*))))
        (t (let ((it (item-in-sight)))
             (when it (format nil "ЛКМ — взять ~a" (carry-name (cons (first (car it)) (sixth (car it))))))))))

;;; Where we are parked

(defun place-here ()
  "Return where we have parked, or NIL.
It is (:market PATH TOWN-INDEX) at a market or (:stop STOP) at a bus stop with cats."
  (unless (> *speed* .4)
    (let ((drop (find-if (lambda (d) (< (abs (- (car d) *dist*)) 14)) *drops*))
          (stop (find-if (lambda (s) (and (second s) (< (abs (- (first s) *dist*)) 7))) *stops*)))
      (cond (drop (list :market (car drop) (cdr drop)))
            (stop (list :stop stop))))))

(defun place-key (place) (round (if (eq (first place) :market) (second place) (first (second place)))))

(defun parcels-for (town)
  "Return how many parcels we have for town TOWN (on the shelves and in hand)."
  (+ (count town *items* :key (lambda (it) (and (sixth (car it)) (parcel-town (sixth (car it))))))
     (if (and *carry* (cdr *carry*) (eql (parcel-town (cdr *carry*)) town)) 1 0)))

(defun parcel-pay (town)
  "Return what is paid for a parcel from here to town TOWN: the farther, the more."
  (let ((tw (find town *towns* :key #'town-index)))
    (round (* (if *stamp* *stamp-factor* 1)                         ; with the town council stamp it is doubled
              (+ *parcel-base* (* *parcel-per-km* (/ (plan-way (town-stop-d tw)) 1000)))))))   ; by the real road, not by the path coordinate

;;; Clients

(defparameter *walk-time* 2.6 "The seconds a client takes to walk from the bus stop to the window and back.")
(defvar *leavers* '() "The clients walking away from the window: (NODE ELAPSED FROM).")

(defun client-spot (u from)
  "Return where the client is along his walk.
U = 1 is where he came from (FROM = (X Z) on the ground), 0 is on the step by the window.
He climbs the steps over the last third of the walk."
  (let ((x (+ *half-width* .68)) (h (+ *step-y* .04)))
    (v3 (+ x (* u (- (first from) x)))
        (+ (* h (- 1 (min 1.0 (/ u .35)))) (* (if (< 0 u 1) .05 0.0) (abs (sin (* *time* 10.0)))))
        (* u (second from)))))

(defun client-off (client)
  "Remove CLIENT from the window: his goods vanish from the windowsill and he walks away on foot."
  (let ((nodes (mapcar #'first (getf client :stall))))
    (stop-flying nodes)
    (dolist (n nodes) (node-queue-free n)))
  (when (getf client :node)
    (node-3d-set-rotation (getf client :node) (v3 0 (/ pi 2) 0))            ; back to us, face to the bus stop
    (push (list (getf client :node) (max 0.0 (- *walk-time* (max 0.0 (getf client :arrive 0.0))) 0) (getf client :from))
          *leavers*)))

(defun client-leave (&optional words)
  (when *client*
    (client-off *client*)
    (setf *client* nil)
    (when words (say words))))

(defun step-client-walk (delta)
  "Walk the clients for DELTA seconds, on every player: the client to the window, the departed back."
  (when (and *client* (> (getf *client* :arrive 0.0) 0))
    (decf (getf *client* :arrive) delta)
    (node-3d-set-position (getf *client* :node)
                          (client-spot (max 0.0 (/ (getf *client* :arrive) *walk-time*)) (getf *client* :from)))
    (when (and (<= (getf *client* :arrive) 0) (not (net-guest-p))) (client-arrived)))
  (setf *leavers*
        (remove-if (lambda (l)
                     (destructuring-bind (node tt from) l
                       (let ((tt (+ tt delta)))
                         (setf (second l) tt)
                         (cond ((>= tt *walk-time*) (node-queue-free node) t)
                               (t (node-3d-set-position node (client-spot (/ tt *walk-time*) from)) nil)))))
                   *leavers*)))

(defun client-arrived ()
  "Handle the client reaching the window: he lays his goods on the windowsill and says why he came."
  (setf (getf *client* :stall)
        (loop for (z k addr) in (getf *client* :goods)
              collect (let ((node (stall-node k addr z)))
                        (fly node (node-3d-get-position node) :from (paws-point z) :dur .4)
                        (list node z k addr))))
  (say (client-words (getf *client* :kind) (getf *client* :price) (getf *client* :town) (getf *client* :dest))))

(defun client-figure (kind price town dest look &optional good)
  "Build a client cat on the step by the window, facing us, with a sign overhead saying what he wants."
  (let ((g (group *cart* (+ *half-width* .68) (+ *step-y* .04) 0)))
    (node-3d-set-rotation g (v3 0 (- (/ pi 2)) 0))
    (node-3d-set-scale (first (make-cat g look)) (v3 .72 .72 .72))
    (sign g (ecase kind
              (:buyer (format nil "КУПЛЮ ~a · ~d р." (string-upcase (good-name (or good :milk))) price))
              (:seller (format nil "ПРОДАЮ ~a · ~d р." (string-upcase (good-name (or good :milk))) price))
              (:sender (format nil "ПОСЫЛКА В ~a · ПЛАЧУ ~d р." (string-upcase (town-name dest)) price))
              (:recipient (format nil "ЖДУ ПОСЫЛКУ В ~a" (string-upcase (town-name town)))))
          0 1.42 0 :h .13 :yaw 0.0)
    g))

(defun stall-node (kind addr z)
  "Build what the client put on the windowsill: a buyer's coins, a seller's milk or a sender's parcel."
  (let ((x (+ *half-width* .29)))
    (if (eq kind :coins)
        (let ((g (group *cart* x (+ *floor* .975) z)))
          (dotimes (i 3) (shape g :cyl '(.06 .06 .016 10) "#d8a840" (* i .05) (* i .016) (* i .03) :metal .7 :rough .3 :shadow nil))
          g)
        (build-item *cart* kind x .95 z 0 addr))))

(defun stall-z (z taken)
  "Return where on the shared windowsill the client puts his item.
It is near Z, but not on an item of ours and not on one of his own (TAKEN)."
  (flet ((busy (c) (or (member c taken :test (lambda (a b) (< (abs (- a b)) .3)))
                       (some (lambda (it) (and (> (second (car it)) 1.5) (< (abs (- (third (car it)) .95)) .1)
                                               (< (abs (- (fourth (car it)) c)) .3)))
                             *items*))))
    (or (find-if-not #'busy (sort (loop for c from -1.0 to 1.0 by .25 collect c) #'< :key (lambda (c) (abs (- c z)))))
        z)))

(defun client-come (kind &key stop town)
  "Bring a client of KIND to the window.
The driver keeps count; guests are sent a description, see CLIENT-VIEW."
  (let* ((story (and (eq kind :sender) (story-sender town)))   ; a sender from a story (istorii.lisp): he has his own addressee
         (regular (and (member kind '(:buyer :seller)) (story-regular town)))     ; a regular: he recognised us and asks
         (dest (when (eq kind :sender)                       ; where the parcel goes: by the story, or to any other town
                 (if story
                     (first story)
                     (let ((others (remove town (loop for tw in *towns* collect (town-index tw)))))
                       (nth (random (length others)) others)))))
         (good (trade-good kind town))                       ; what he sells or looks for depends on the town (goods.lisp)
         (price (ecase kind
                  (:buyer (trade-price good :buyer town))
                  (:seller (trade-price good :seller town))
                  (:sender (parcel-pay dest))
                  (:recipient 0)))
         (look (random 36))
         (goods (ecase kind                                   ; (Z KIND ADDR): what he puts on the windowsill
                  (:buyer (list (list 0.0 :coins good)))              ; for coins the address field holds which goods he wants
                  (:seller (loop for z in (subseq '(-.5 0.0 .5) 0 (1+ (random 3))) collect (list z good nil)))
                  (:sender (list (list 0.0 :box (if story (parcel-addr dest (fourth story) (fifth story)) dest))))   ; a story parcel has its own address
                  (:recipient '()))))
    ;; at a bus stop one cat leaves the queue: he is the one standing at the window
    (when (and stop (second stop))
      (node-queue-free (first (second stop)))
      (setf (second stop) (rest (second stop))))
    (let ((taken '()))                                            ; the windowsill is shared: he puts things where ours are not in the way
      (setf goods (loop for (z k addr) in goods
                        collect (let ((free (stall-z z taken))) (push free taken) (list free k addr)))))
    (let ((from (if stop (list 5.2 1.0) (list 7.4 -6.0))))        ; from the bus stop or from the market stalls
      (setf *client* (list :kind kind :node (client-figure kind price town dest look good) :good good :stop nil :town town :dest dest
                           ;; what lay on the windowsill before the buyer came is not for him: he will not take it himself (see SILL-ITEM)
                           :ignore (and (eq kind :buyer) (mapcar #'car (remove-if-not #'on-sill-p *items*)))
                           :story (and story (cdddr story))
                           :price price :look look :left (if (eq kind :recipient) 70.0 *patience*) :reach 1.2
                           :arrive *walk-time* :from from :goods goods :stall '()))
      (node-3d-set-position (getf *client* :node) (client-spot 1.0 from)))
    (log-event :client :kind kind :price price :town town :dist (round *dist*))
    (cond (story (say-all (format nil "~a: «~a»" (second story) (third story))))
          (regular (say-all regular))
          (t (say (ecase kind
                    (:buyer "Идёт покупатель") (:seller "Идёт продавец") (:sender "Идёт отправитель с посылкой")
                    (:recipient (let ((who (some (lambda (it) (let ((a (sixth (car it))))      ; a story parcel is awaited: its addressee comes
                                                                (and a (eql (parcel-town a) town) (parcel-to a))))
                                                 *items*)))
                                  (if who (format nil "Идёт получатель: ~a" who) "Идёт получатель")))))))))

(defun client-words (kind price town dest)
  (ecase kind
    (:buyer (format nil "Покупатель ищет ~a: даст ~d р. — возьми с полки и принеси к окну" (good-name (client-good)) price))
    (:seller (format nil "Продавец: ~a по ~d р. — бери с подоконника (ЛКМ)" (good-name (client-good)) price))
    (:sender (format nil "Отправитель: посылка в ~a, платит ~d р. — возьми её с подоконника (ЛКМ)" (town-name dest) price))
    (:recipient (format nil "Получатель ждёт посылку в ~a — поставь её на подоконник" (town-name town)))))

;;; The client over the network: the driver sends guests a description, guests draw the same one and send their actions

(defun client-view ()
  "Return the description of the client for guests, or NIL.
It is (KIND PRICE TOWN DEST LOOK ((Z KIND ADDR) ...))."
  (when *client*
    (list (getf *client* :kind) (getf *client* :price) (getf *client* :town) (getf *client* :dest) (getf *client* :look)
          (loop for (nil z k addr) in (getf *client* :stall) collect (list z k addr))
          (getf *client* :good))))                               ; what he sells or looks for, for the sign

(defvar *client-seen* nil "The last client description a guest received from the driver.")

(defun guest-client (view)
  "Show the client on a guest exactly as he stands at the driver's, from description VIEW."
  (unless (equal view *client-seen*)
    (let ((new (not (equal (and view (subseq view 0 5)) (and *client-seen* (subseq *client-seen* 0 5))))))
      (setf *client-seen* view)
      (when *client*
        (let ((nodes (mapcar #'first (getf *client* :stall))))
          (stop-flying nodes)
          (dolist (n nodes) (node-queue-free n)))
        (setf (getf *client* :stall) '())
        (when (or new (null view)) (client-off *client*)))
      (cond ((null view) (setf *client* nil))
            (t (destructuring-bind (kind price town dest look goods &optional good) view
                 (when (and (member kind '(:buyer :seller :sender :recipient)) (integerp price) (integerp look) (listp goods)
                            (or (null good) (keywordp good)))
                   (setf *client* (list :kind kind :price price :town town :dest dest :look look :left 99.0 :reach 9.0
                                        :from (list 7.4 -6.0)
                                        :arrive (if (and *client* (not new)) (getf *client* :arrive 0.0) *walk-time*)
                                        :node (if (and *client* (not new)) (getf *client* :node) (client-figure kind price town dest look good))
                                        :stall (loop for (z k addr) in goods
                                                     when (and (realp z) (or (member k '(:coins :box)) (good-p k)))
                                                       collect (list (stall-node k addr z) z k addr))))
                   (when new
                     (node-3d-set-position (getf *client* :node) (client-spot 1.0 (getf *client* :from)))
                     (say (client-words kind price town dest))))))))))

(defun pick-kind ()
  (let ((r (random 1.0)))
    (loop for (kind . share) in *client-mix*
          do (when (< (decf r share) 0) (return kind))
          finally (return :buyer))))

(defun ring-bell ()
  "Ring the bell to call a client; if one is already at the window, dismiss him and call the next.
A guest asks the driver to do it."
  (when *bell-sound* (object-call *bell-sound* "play"))
  (when (net-guest-p)
    (guest-send (list :a :bell))
    (return-from ring-bell))
  ;; a client at the window: the bell dismisses him and calls the next one; wrong goods, wrong price - we do not wait
  (let ((place (place-here))
        (was (when *client* (prog1 (getf *client* :kind) (client-leave)))))
    (cond ((null place) (say "Динь! Никого: зови на рынке в городе или на остановке, где ждут"))
          ((and (eq (first place) :market) (plusp (parcels-for (third place)))
                (not (eq was :recipient)))                          ; the recipient was just dismissed: call someone else
           (client-come :recipient :town (third place)))           ; first the one who is waiting for a parcel
          (t (let* ((key (place-key place))
                    (visit (or (assoc key *visits*)
                               (first (push (cons key (if (eq (first place) :market) *market-clients* (length (second (second place)))))
                                            *visits*)))))
               (cond ((<= (cdr visit) 0) (say "Динь! Все уже разошлись. Езжай дальше"))
                     (t (decf (cdr visit))
                        (if (eq (first place) :market)
                            (client-come (pick-kind) :town (third place))
                            (client-come :buyer :stop (second place))))))))))

(defun sill-item (test)
  "Return an item on the windowsill that satisfies TEST, or NIL.
The windowsill is shared with the client, so anywhere on it counts. What lay here before the buyer came
(:IGNORE) he does not take himself: it was not put there for him. To give him such an item, click his
coins with the left mouse button."
  (find-if (lambda (it) (and (on-sill-p it) (funcall test (car it))
                             (not (member (car it) (getf *client* :ignore) :test #'equal))))
           *items*))

(defun on-sill-p (it)
  (and (< 1.15 (second (car it)) 2.3) (< (abs (- (third (car it)) .95)) .1)))

(defun stall-deal (entry holding)
  "Make a deal at the windowsill over stall ENTRY (the driver keeps count).
HOLDING is what the player who clicked has in hand: (KIND . ADDR) or NIL. Return three values: what
he now has in hand, whether that changed, and what to say."
  (destructuring-bind (node z kind addr) entry
    (declare (ignore z))
    (let ((price (getf *client* :price)) (sender (eq (getf *client* :kind) :sender)))
      (cond ((eq kind :coins)
             (cond ((and holding (null (cdr holding)) (wanted-p (car holding)))
                    (let ((price (good-price (car holding) price)))
                      (note-trade (car holding) :sold)
                      (incf *money* price)
                      (log-event :sold :price price :money *money* :dist (round *dist*))
                      (client-leave)
                      (values nil t (format nil "Продали за ~d р." price))))
                   ((and (null holding) (getf *client* :ignore)                 ; what he wants already lies on the windowsill: hand it over on our word
                         (some (lambda (spec) (wanted-p (first spec))) (getf *client* :ignore)))
                    (setf (getf *client* :ignore) nil)
                    (values nil nil "Отдаём с подоконника"))
                   (t (values holding nil (format nil "Он ищет ~a: возьми с полки (ЛКМ) и принеси к окну" (good-name (client-good)))))))
            (holding (values holding nil "Руки заняты — сначала поставь то, что несёшь"))
            ((and (not sender) (< *money* price)) (values nil nil "Денег не хватает. Включи музыку — слушатели накидают"))
            (t (if sender (incf *money* (ceiling price 2)) (decf *money* price))   ; the sender pays a deposit, half; the rest on delivery
               (unless sender (note-trade kind :bought))
               (when (and sender (getf *client* :story)) (apply #'story-taken (getf *client* :story)))   ; the story is on its way
               (stop-flying (list node))
               (fly node (v3 (- *half-width* .3) (+ *floor* 1.0) (second entry)) :dur .22 :free t)   ; through the window, to us
               (setf (getf *client* :stall) (remove entry (getf *client* :stall)))
               (log-event (if sender :parcel-taken :bought) :price price :to addr :money *money* :dist (round *dist*))
               (unless (getf *client* :stall) (client-leave))
               (values (cons kind addr) t
                       (if sender
                           (format nil "Взяли посылку в ~a: задаток ~d р., при вручении ещё ~d. Поставь её на полку" (town-name (parcel-town addr)) (ceiling price 2) (delivery-pay))
                           (format nil "Купили ~a за ~d р." (good-name kind) price))))))))

(defun take-from-stall (entry)
  "Handle a left click on stall ENTRY, something the client put on the windowsill.
For a guest the deal is made by the driver."
  (cond ((net-guest-p)
         (guest-send (list :a :take (second entry) (car *carry*) (cdr *carry*))))
        (t (multiple-value-bind (carry changed words) (stall-deal entry *carry*)
             (when changed (setf *carry* carry *carry-from* nil) (show-carry))
             (say words)))))

(defun host-deal (p z kind addr)
  "Handle, on the driver, guest P clicking the windowsill item at position Z while holding (KIND . ADDR)."
  (let ((entry (and *client* (find z (getf *client* :stall) :key #'second :test (lambda (a b) (< (abs (- a b)) .05))))))
    (when entry
      (multiple-value-bind (carry changed words) (stall-deal entry (and kind (cons kind addr)))
        (peer-send p (list :g (and changed t) (car carry) (cdr carry) words))))))

(defun guest-dealt (changed kind addr words)
  "Handle, on a guest, the driver's answer to our deal."
  (when changed
    (setf *carry* (and kind (cons kind addr)) *carry-from* nil)
    (show-carry))
  (when (stringp words) (say words)))

(defun step-client (delta)
  ;; parked at a market with a parcel for this town: the recipient comes up by himself
  (let ((place (place-here)))
    (when (and place (not *client*) (eq (first place) :market) (plusp (parcels-for (third place)))
               (> *window-k* .9)
               (not (member (place-key place) *met*)))
      (push (place-key place) *met*)
      (client-come :recipient :town (third place))))
  (when (and *client* (<= (getf *client* :arrive 0.0) 0))
    (let ((kind (getf *client* :kind)))
      (cond ((> *speed* .6) (client-leave "Тронулись — клиент остался"))
            ((<= (decf (getf *client* :left) delta) 0)
             (client-leave (ecase kind
                             (:buyer "Покупатель не дождался и ушёл")
                             (:seller "Продавец собрал товар и ушёл")
                             (:sender "Отправитель унёс посылку")
                             (:recipient "Получатель отошёл. Позвони в колокольчик — вернётся"))))
            ((member kind '(:buyer :recipient))
             (let* ((town (getf *client* :town))
                    (it (and (> *window-k* .9) *client*
                             (sill-item (if (eq kind :buyer)
                                            (lambda (spec) (wanted-p (first spec)))
                                            (lambda (spec) (and (sixth spec) (eql (parcel-town (sixth spec)) town))))))))
               (if it
                   (when (<= (decf (getf *client* :reach) delta) 0)       ; reaches out a paw for a second and takes it
                     (let ((price (if (eq kind :buyer) (good-price (first (car it)) (getf *client* :price)) (delivery-pay))))   ; the recipient pays for the delivery
                       (when (eq kind :buyer) (note-trade (first (car it)) :sold))
                       (when (eq kind :recipient)
                         (award-pennant town) (note-quest :post)
                         (let ((answer (story-delivered (sixth (car it))))) (when answer (say-later answer))))   ; they reply to this very parcel
                       (fly (cdr it) (paws-point (fourth (car it))) :dur .4 :free t)   ; reaches out a paw and takes it
                       (setf *items* (remove it *items*))
                       (incf *money* price)
                       (settle-items) (refresh-seats) (save-interior)
                       (log-event (if (eq kind :buyer) :sold :delivered) :price price :town town :money *money* :dist (round *dist*))
                       (cond ((eq kind :buyer) (client-leave (format nil "Продали за ~d р." price)))
                             ((plusp (parcels-for town))
                              (setf (getf *client* :left) 70.0 (getf *client* :reach) 1.2)
                              (say (format nil "Посылка вручена: +~d р. В ~a есть ещё — ставь следующую" price (town-name town))))
                             (t (client-leave (format nil "Посылка вручена: +~d р. Спасибо!" price))))))
                   (setf (getf *client* :reach) 1.2))))))))

;;; Music for coins

(defun step-busk (delta)
  (let ((place (and (not (net-guest-p)) (or *playing* *guitar-on*) (place-here))))
    (when (and place (<= (decf *busk-timer* (* delta (if *guitar-on* *busk-live* 1))) 0))        ; live music: twice as often
      (setf *busk-timer* *busk-every*)
      (let* ((key (place-key place))
             (pot (if (eq (first place) :market) 14 (+ 4 (* 3 (length (second (second place)))))))
             (got (or (assoc key *busked*) (first (push (cons key 0) *busked*)))))
        (when (< (cdr got) pot)
          (incf (cdr got)) (incf *money*)
          (when (= (cdr got) 1) (log-event :busk :dist (round *dist*) :money *money*))
          (say (if (< (cdr got) pot) "Котам нравится музыка: +1 р." "Котам нравится музыка: +1 р. Больше тут не накидают")))))))

;;; Intro hints: one step at a time, once the previous message has faded

(defun step-onboard ()
  (unless (or (net-guest-p) *lobby-open* (>= *onboard* 9) (and *toast* (> (cdr *toast*) 0)))
    (let ((market (let ((p (place-here))) (and p (eq (first p) :market) (zerop (third p))))))
      (case *onboard*
        (0 (when (> *time* 2)
             (say "В кузове три посылки и молоко. Бензина — только до рынка в Заречном") (setf *onboard* 1)))
        (1 (say "M — карта: выбери город, грузовик поедет сам. S — остановить. V — вид изнутри") (setf *onboard* 2))
        (2 (when market
             (say "Заглохли у рынка. Здесь же заправка: R — бензин на 10 р.") (setf *onboard* 3)))
        (3 (when market
             (say "V — внутрь. Раздвинь окно: E по стеклу. Колокольчик в проёме зовёт клиентов") (setf *onboard* 4)))
        (4 (when (and market (> *window-k* .9))
             (say "ЛКМ — взять вещь с полки и поставить на подоконник. E — всё остальное") (setf *onboard* 5)))
        (5 (when (and market (< *money* 5))
             (say "Денег мало — включи музыку (диск в проигрыватель): слушатели накидают монет") (setf *onboard* 6)))
        (6 (when (> *dist* (+ (town-stop-d (first *towns*)) 60))
             (say "Куда везти посылки и сколько до городов — на карте у двери") (setf *onboard* 9)))))))

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
                       (usable (+ *half-width* .29) (if sender 1.17 1.07) (second entry)
                             (lambda ()
                               (cond ((eq (third entry) :coins)
                                      (if (and *carry* (null (cdr *carry*)) (wanted-p (car *carry*)))
                                          (format nil "ЛКМ — отдать за ~d р." (good-price (car *carry*) (getf *client* :price)))
                                          (format nil "Покупатель ищет ~a, даёт ~d р.: возьми с полки (ЛКМ)" (good-name (client-good)) (getf *client* :price))))
                                     (t (if sender
                                   (format nil "ЛКМ — взять посылку в ~a · платит ~d р." (town-name (fourth entry)) (getf *client* :price))
                                   (format nil "ЛКМ — купить ~a за ~d р. · у тебя ~d р." (good-name (third entry)) (getf *client* :price) *money*)))))
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
      (find-if (lambda (it) (and (> (second (car it)) 1.15) (< (abs (- (third (car it)) .95)) .12))) *items*)))

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
