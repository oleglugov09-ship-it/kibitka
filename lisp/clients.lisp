;;;; The clients at the big window: who comes, what he shows and says, the deal over the windowsill, and the
;;;; same client drawn for guests. The paws, the shelves and the rack are in trade.lisp; what a client sells
;;;; or wants and for how much is in goods.lisp and plan.lisp.

(in-package #:kiosk-trip)

;;; The kinds of client at the window. What a kind says and shows is declared here in one place; a new kind
;;; needs a DEFCLIENT, its price and what it puts on the windowsill in CLIENT-COME, and, if the deal itself
;;; is new, a branch in STALL-DEAL. The functions take the price, the good, the town and the destination.

(defvar *client-kinds* '() "The kinds of client: entries of (kind . plist), see DEFCLIENT.")

(defmacro defclient (kind &rest plist)
  "Declare the client KIND: :sign over his head, :hint while he waits, :words when he has come up, :coming
(a text, or a function of the town), :gone when he gave up, :wait - his own patience in seconds."
  `(setf *client-kinds* (cons (list ,kind ,@plist) (remove ,kind *client-kinds* :key #'car))))

(defun client-kind (kind key) "Return what is declared under KEY for the client KIND." (getf (rest (assoc kind *client-kinds*)) key))

(defclient :buyer
  :sign (lambda (price good town dest) (declare (ignore town dest))
          (format nil "КУПЛЮ ~a · ~/kiosk-trip::rub/" (string-upcase (good-name (or good :milk))) price))
  :hint (lambda (price good town dest) (declare (ignore town dest))
          (if *carry*
              (format nil "Покупатель ищет ~a: ЛКМ по монетам на подоконнике — отдать" (good-name good))
              (format nil "Покупатель ищет ~a, даёт ~/kiosk-trip::rub/: возьми с полки (ЛКМ) и принеси к окну" (good-name good) price)))
  :words (lambda (price good town dest) (declare (ignore town dest))
           (format nil "Покупатель ищет ~a: даст ~/kiosk-trip::rub/ — возьми с полки и принеси к окну" (good-name good) price))
  :coming "Идёт покупатель" :gone "Покупатель не дождался и ушёл")

(defclient :seller
  :sign (lambda (price good town dest) (declare (ignore town dest))
          (format nil (if (> price (list-price (or good :milk))) "СВЕРХ ПЛАНА: ~a · ~/kiosk-trip::rub/" "МАГАЗИН: ~a · ~/kiosk-trip::rub/")
                  (string-upcase (plan-name (or good :milk))) price))
  :hint (lambda (price good town dest) (declare (ignore town dest))
          (format nil "Продавец: ~a по ~/kiosk-trip::rub/ — ЛКМ по товару на подоконнике" (good-name good) price))
  :words (lambda (price good town dest) (declare (ignore town dest))
           (format nil "Продавец: ~a по ~/kiosk-trip::rub/ — бери с подоконника (ЛКМ)" (good-name good) price))
  :coming "Идёт продавец" :gone "Продавец собрал товар и ушёл")

(defclient :ration                              ; the shop again, with a good the town is short of: a coupon and the list price
  :sign (lambda (price good town dest) (declare (ignore town dest))
          (format nil "ПО ТАЛОНАМ: ~a · ~/kiosk-trip::rub/" (string-upcase (plan-name (or good :milk))) price))
  :hint (lambda (price good town dest) (declare (ignore town dest))
          (format nil "По талонам: ~a за ~/kiosk-trip::rub/ и один талон — ЛКМ по товару на подоконнике · талонов у нас: ~d" (good-name good) price *talons*))
  :words (lambda (price good town dest) (declare (ignore town dest))
           (format nil "Магазин: ~a здесь по талонам. ~/kiosk-trip::rub/ и талон — бери с подоконника (ЛКМ)" (good-name good) price))
  :coming "Идёт продавец" :gone "Продавец собрал товар и ушёл")

(defclient :sender
  :sign (lambda (price good town dest) (declare (ignore town))
          (if good
              (format nil "НАРЯД: ~a В ~a · ТАРИФ ~/kiosk-trip::rub/" (string-upcase (plan-name good)) (string-upcase (place-name dest)) price)
              (format nil "ПОСЫЛКА В ~a · ПЛАЧУ ~/kiosk-trip::rub/" (string-upcase (place-name dest)) price)))
  :hint (lambda (price good town dest) (declare (ignore good town dest))
          (format nil "Отправитель платит ~/kiosk-trip::rub/: ЛКМ по посылке на подоконнике" price))
  :words (lambda (price good town dest) (declare (ignore good town))
           (format nil "Отправитель: посылка в ~a, платит ~/kiosk-trip::rub/ — возьми её с подоконника (ЛКМ)" (place-name dest) price))
  :coming "Идёт отправитель с посылкой" :gone "Отправитель унёс посылку")

(defclient :recipient
  :sign (lambda (price good town dest) (declare (ignore price good dest))
          (format nil "ЖДУ ПОСЫЛКУ В ~a" (string-upcase (town-name town))))
  :hint (lambda (price good town dest) (declare (ignore price good dest))
          (format nil "Получатель ждёт посылку в ~a: возьми её с полки (ЛКМ) и поставь на подоконник" (town-name town)))
  :words (lambda (price good town dest) (declare (ignore price good dest))
           (format nil "Получатель ждёт посылку в ~a — поставь её на подоконник" (town-name town)))
  :coming (lambda (town)
            (let ((who (some (lambda (it) (let ((a (sixth (car it))))      ; a story parcel is awaited: its addressee comes
                                            (and a (eql (parcel-town a) town) (parcel-to a))))
                             *items*)))
              (if who (format nil "Идёт получатель: ~a" who) "Идёт получатель")))
  :gone "Получатель отошёл. Позвони в колокольчик — вернётся" :wait 70.0)

(defun client-hint ()
  "Return the hint on what to do with the client at the window.
It is shown while we are not looking at anything specific."
  (when (and *client* (> (getf *client* :arrive 0.0) 0)) (return-from client-hint "Клиент идёт к окну"))
  (when *client*
    (funcall (client-kind (getf *client* :kind) :hint) (getf *client* :price) (client-good) (getf *client* :town) (getf *client* :dest))))

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
    (* (if *stamp* *stamp-factor* 1)                                ; with the town council stamp it is doubled
       (way-pay (plan-way (if tw (town-stop-d tw) (or (camp-d) *dist*)))))))   ; by the real road; or to the camp

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

(defun person-dress (g head marks)
  "Put the MARKS of a person we know by name on a cat: G is its body group, HEAD its head.
They are what tells the director from the fixer at a glance: a tie, a hat, a kerchief, spectacles."
  (dolist (mark marks)
    (case mark
      (:tie (model g "person-tie"))
      (:hat (model head "person-hat"))
      (:folder (model g "person-folder"))
      (:glasses (model head "person-glasses"))
      (:shades (model head "person-shades"))
      (:bun (model head "person-bun"))
      (:bow (model head "person-bow"))
      (:white (model g "person-white")
              (model head "person-white-cap"))
      ((:kerchief :kerchief-white)
       (let ((rgb (if (eq mark :kerchief) "#8a2a3a" "#eef0ec")))
         (model head "person-kerchief" 0.0 0.0 0.0 :paint (list rgb))
         (when (eq mark :kerchief)
           (model head "person-kerchief-dots"))))
      (:cap (model head "person-cap"))
      (:book (model g "person-book"))
      (:officer (model head "person-officer-cap")
                (model g "person-officer")))))

(defun client-figure (kind price town dest look &optional good who)
  "Build a client cat on the step by the window, facing us, with a sign overhead saying what he wants.
WHO is the key of a person we know by name (people.lisp): he comes as himself, not as one of the crowd."
  (let* ((g (group *cart* (+ *half-width* .68) (+ *step-y* .04) 0))
         (face (and who (getf (rest (assoc who *people*)) :look)))
         (cat (if face (make-cat g (first face) (second face)) (make-cat g look))))
    (node-3d-set-rotation g (v3 0 (- (/ pi 2)) 0))
    (node-3d-set-scale (first cat) (v3 .72 .72 .72))
    (when face (person-dress (first cat) (second cat) (third face)))
    (sign g (funcall (client-kind kind :sign) price good town dest)
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
  (when (and (eq kind :seller) (or *dev* (plusp *talons*))          ; with no coupon in the purse the sign would only puzzle
             (rationed-goods (trade-town town)) (< (random 1.0) *ration-chance*))
    (setf kind :ration))                                      ; the shop brings what is short here: against a coupon
  (let* ((story (and (eq kind :sender)
                     (not (and *first-crate* (plan-cargo (trade-town town))))   ; the order has just come: its first crate goes before a story
                     (story-sender town)))                     ; a sender from a story (istorii.lisp): he has his own addressee
         (regular (and (member kind '(:buyer :seller)) (story-regular town)))     ; a regular: he recognised us and asks
         (camp (and (eq kind :sender) (not story) (camp-open-p) (camp-d) (< (random 1.0) *camp-parcel-chance*)))   ; parents send to the pioneer camp
         (cargo (and (eq kind :sender) (not story) (not camp) (paper-p :order) (plan-cargo (trade-town town))))   ; the plan's cargo: a surplus here that is short elsewhere
         (person (and (not story) (not regular) (or cargo (not (eq kind :sender)))      ; someone we know by name (people.lisp): a line a day
                      (person-speaks (list :window kind) (trade-town town))))
         (who (and person *person-spoke*))
         (dest (when (eq kind :sender)                       ; where the parcel goes: by the story, by the plan, or to any other town
                 (cond (story (first story))
                       (cargo (first cargo))
                       (camp *camp-town*)
                       (t (let ((others (remove town (loop for tw in *towns* collect (town-index tw)))))
                            (nth (random (length others)) others))))))
         (good (cond (cargo (second cargo))
                     ((eq kind :ration) (let ((list (rationed-goods (trade-town town)))) (nth (random (length list)) list)))
                     (t (trade-good kind town))))   ; what he sells or looks for depends on the town's plan (goods.lisp)
         (maker (and (eq kind :seller) (member good (town-makes (trade-town town))) (< (random 1.0) .5)))   ; the maker itself, above the plan
         (price (ecase kind
                  (:buyer (trade-price good :buyer town))
                  (:seller (trade-price good :seller town maker))
                  (:ration (trade-price good :seller town))
                  (:sender (if cargo (* *transfer-scale* (parcel-pay dest)) (parcel-pay dest)))   ; the plan pays by transfer, on its own scale
                  (:recipient 0)))
         (look (random 36))
         (goods (ecase kind                                   ; (Z KIND ADDR): what he puts on the windowsill
                  (:buyer (list (list 0.0 :coins good)))              ; for coins the address field holds which goods he wants
                  (:seller (loop for z in (subseq '(-.5 0.0 .5) 0 (1+ (random 3))) collect (list z good nil)))
                  (:ration (list (list 0.0 good nil)))                ; one to a customer
                  (:sender (list (list 0.0 :box (cond (story (parcel-addr dest (fourth story) (fifth story)))
                                                      (cargo (plan-addr dest (second cargo)))    ; the crate remembers what is in it
                                                      (t dest)))))   ; a story parcel has its own address
                  (:recipient '()))))
    ;; at a bus stop one cat leaves the queue: he is the one standing at the window
    (when (and stop (second stop))
      (node-queue-free (first (second stop)))
      (setf (second stop) (rest (second stop))))
    (let ((taken '()))                                            ; the windowsill is shared: he puts things where ours are not in the way
      (setf goods (loop for (z k addr) in goods
                        collect (let ((free (stall-z z taken))) (push free taken) (list free k addr)))))
    (let ((from (if stop (list 5.2 1.0) (list 7.4 -6.0))))        ; from the bus stop or from the market stalls
      (setf *client* (list :kind kind :node (client-figure kind price town dest look good who) :who who :good good :stop nil :town town :dest dest
                           ;; what lay on the windowsill before the buyer came is not for him: he will not take it himself (see SILL-ITEM)
                           :ignore (and (eq kind :buyer) (mapcar #'car (remove-if-not #'on-sill-p *items*)))
                           :story (and story (cdddr story))
                           :price price :look look :left (or (client-kind kind :wait) *patience*) :reach 1.2
                           :arrive *walk-time* :from from :goods goods :stall '()))
      (node-3d-set-position (getf *client* :node) (client-spot 1.0 from)))
    (log-event :client :kind kind :price price :town town :dist (round *dist*))
    (cond (story (say-all (format nil "~a: «~a»" (second story) (third story))))
          ((eql dest *camp-town*)
           (say-all (if (eql (trade-town town) *camp-intl-town*)
                        "Проводница: «Поездом из братской Озерии — детям интернациональной смены, в лагерь «Костёр»»"
                        "Мама пионера: «В лагерь «Костёр», сыну в третий отряд. Там варенье — не разбейте»")))
          (regular (say-all regular))
          (person (say-all person))
          (t (let ((coming (client-kind kind :coming))) (say (if (functionp coming) (funcall coming town) coming)))))))

(defun client-words (kind price town dest)
  (funcall (client-kind kind :words) price (client-good) town dest))

;;; The client over the network: the driver sends guests a description, guests draw the same one and send their actions

(defun client-view ()
  "Return the description of the client for guests, or NIL.
It is (KIND PRICE TOWN DEST LOOK ((Z KIND ADDR) ...))."
  (when *client*
    (list (getf *client* :kind) (getf *client* :price) (getf *client* :town) (getf *client* :dest) (getf *client* :look)
          (loop for (nil z k addr) in (getf *client* :stall) collect (list z k addr))
          (getf *client* :good)                                  ; what he sells or looks for, for the sign
          (getf *client* :who))))                                ; someone we know by name: he looks like himself

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
            (t (destructuring-bind (kind price town dest look goods &optional good who) view
                 (when (and (assoc kind *client-kinds*) (integerp price) (integerp look) (listp goods)
                            (or (null good) (keywordp good)) (or (null who) (keywordp who)))
                   (setf *client* (list :kind kind :price price :town town :dest dest :look look :left 99.0 :reach 9.0
                                        :from (list 7.4 -6.0)
                                        :arrive (if (and *client* (not new)) (getf *client* :arrive 0.0) *walk-time*)
                                        :node (if (and *client* (not new)) (getf *client* :node) (client-figure kind price town dest look good who))
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

(defun stall-deal (entry holding &optional props)
  "Make a deal at the windowsill over stall ENTRY (the driver keeps count).
HOLDING is what the player who clicked has in hand: (KIND . ADDR) or NIL, and PROPS its properties.
Return four values: what he now has in hand, whether that changed, what to say, and the properties of the new thing."
  (destructuring-bind (node z kind addr) entry
    (declare (ignore z))
    (let ((price (getf *client* :price)) (sender (eq (getf *client* :kind) :sender)) (ration (eq (getf *client* :kind) :ration)))
      (cond ((eq kind :coins)
             (cond ((and holding (null (cdr holding)) (wanted-p (car holding)))
                    (let ((price (good-price (car holding) price)))
                      (note-trade (car holding) :sold (getf props :shop))
                      (add-money price)
                      (log-event :sold :price price :money *money* :dist (round *dist*))
                      (client-leave)
                      (values nil t (format nil "Продали за ~/kiosk-trip::rub/" price))))
                   ((and (null holding) (getf *client* :ignore)                 ; what he wants already lies on the windowsill: hand it over on our word
                         (some (lambda (spec) (wanted-p (first spec))) (getf *client* :ignore)))
                    (setf (getf *client* :ignore) nil)
                    (values nil nil "Отдаём с подоконника"))
                   (t (values holding nil (format nil "Он ищет ~a: возьми с полки (ЛКМ) и принеси к окну" (good-name (client-good)))))))
            (holding (values holding nil "Руки заняты — сначала поставь то, что несёшь"))
            ((and (not sender) (< *money* price) (plusp *credit*))            ; money there is, but on the account
             (log-event :no-cash :price price :money *money* :credit *credit*)
             (values nil nil (format nil "Наличных не хватает. Счёт кооператива у окна не берут — наличными его выдают в сберкассе: ~a, дверь «ЗАРПЛАТА»" (town-name 0))))
            ((and (not sender) (< *money* price)) (values nil nil "Денег не хватает. Включи музыку — слушатели накидают"))
            ((and ration (< *talons* 1) (not *dev*))
             (values nil nil "Без талона не отпустят. Талоны даёт горсовет за ящик по наряду, довезённый туда, где его ждут"))
            (t (cond ((and sender (parcel-good addr)) (coop-pay (ceiling price 2)) (setf *first-crate* nil))   ; the plan pays to the account, once there is one
                     (sender (add-money (ceiling price 2)))                           ; a sender pays a deposit, half; the rest on delivery
                     (t (take-money price) (when ration (use-talon))))
               (unless sender (note-trade kind :bought))
               (when (and sender (getf *client* :story)) (apply #'story-taken (getf *client* :story)))   ; the story is on its way
               (stop-flying (list node))
               (fly node (v3 (- *half-width* .3) (+ *floor* 1.0) (second entry)) :dur .22 :free t)   ; through the window, to us
               (setf (getf *client* :stall) (remove entry (getf *client* :stall)))
               (log-event (if sender :parcel-taken :bought) :price price :to addr :money *money* :dist (round *dist*))
               (unless (getf *client* :stall) (client-leave))
               (values (cons kind addr) t
                       (cond
                         ((and sender (parcel-good addr))
                          (format nil (if (paper-p :account)
                                          "Взяли ящик по наряду: ~a, везти в ~a. Задаток ~/kiosk-trip::rub/ — на счёт кооператива. Довезём, пока там не хватает, — премия ~/kiosk-trip::rub/ Поставь его на полку"
                                          "Взяли ящик по наряду: ~a, везти в ~a. Задаток ~/kiosk-trip::rub/ Довезём, пока там не хватает, — премия ~/kiosk-trip::rub/ Поставь его на полку")
                                  (good-name (parcel-good addr)) (place-name (parcel-town addr)) (ceiling price 2)
                                  (* *plan-premium* (standing-factor (parcel-town addr) :premium))))
                         (sender
                           (format nil "Взяли посылку в ~a: задаток ~/kiosk-trip::rub/, при вручении ещё ~/kiosk-trip::rub/ Поставь её на полку" (place-name (parcel-town addr)) (ceiling price 2) (delivery-pay)))
                         (t (format nil "Купили ~a за ~/kiosk-trip::rub/" (good-name kind) price)))
                       (and (not sender) (or ration (<= price (list-price kind))) '(:shop t))))))))   ; at the list price it is the shop's: not for resale

(defun take-from-stall (entry)
  "Handle a left click on stall ENTRY, something the client put on the windowsill.
For a guest the deal is made by the driver."
  (cond ((net-guest-p)
         (guest-send (list :a :take (second entry) (car *carry*) (cdr *carry*) *carry-props*)))
        (t (multiple-value-bind (carry changed words props) (stall-deal entry *carry* *carry-props*)
             (when changed (setf *carry* carry *carry-from* nil *carry-props* props) (show-carry))
             (say words)))))

(defun host-deal (p z kind addr props)
  "Handle, on the driver, guest P clicking the windowsill item at position Z while holding (KIND . ADDR) with PROPS."
  (let ((entry (and *client* (find z (getf *client* :stall) :key #'second :test (lambda (a b) (< (abs (- a b)) .05))))))
    (when entry
      (multiple-value-bind (carry changed words new-props) (stall-deal entry (and kind (cons kind addr)) props)
        (peer-send p (list :g (and changed t) (car carry) (cdr carry) words new-props))))))

(defun guest-dealt (changed kind addr words props)
  "Handle, on a guest, the driver's answer to our deal."
  (when changed
    (setf *carry* (and kind (cons kind addr)) *carry-from* nil *carry-props* (and (props-ok-p props) props))
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
             (client-leave (client-kind kind :gone)))
            ((member kind '(:buyer :recipient))
             (let* ((town (getf *client* :town))
                    (it (and (> *window-k* .9) *client*
                             (sill-item (if (eq kind :buyer)
                                            (lambda (spec) (wanted-p (first spec)))
                                            (lambda (spec) (and (sixth spec) (eql (parcel-town (sixth spec)) town))))))))
               (if it
                   (when (<= (decf (getf *client* :reach) delta) 0)       ; reaches out a paw for a second and takes it
                     (let ((price (if (eq kind :buyer) (good-price (first (car it)) (getf *client* :price)) (delivery-pay)))   ; the recipient pays for the delivery
                           (done nil))                                                       ; what to say about a parcel handed over
                       (when (eq kind :buyer) (note-trade (first (car it)) :sold (spec-prop (car it) :shop)))
                       (when (eq kind :recipient)
                         (award-pennant town) (note-quest :post)
                         (let ((answer (story-delivered (sixth (car it))))) (when answer (say-later answer))))   ; they reply to this very parcel
                       (fly (cdr it) (paws-point (fourth (car it))) :dur .4 :free t)   ; reaches out a paw and takes it
                       (setf *items* (remove it *items*))
                       (if (and (eq kind :recipient) (parcel-good (sixth (car it))))       ; the plan's crate: into the shop, pay to the account
                           (let ((had *talons*) (premium (deliver-cargo (sixth (car it)))))
                             (setf price (+ price premium))
                             ;; the fourth town on the wall and a crate handed over: from now on the state pays by transfer
                             (when (and (>= (logcount *pennants*) 4) (not (paper-p :account)))
                               (grant-paper :account
                                            "Вместе с накладной получатель протягивает конверт. ИЗВЕЩЕНИЕ сберкассы: кооперативу «Кибитка» открыт расчётный счёт. За госгруз теперь платят не в руки, а переводом. Бензин и ссуда спишутся со счёта сами; наличными его выдают в сберкассе, дверь «ЗАРПЛАТА», раз в неделю."
                                            "Кооперативу открыли счёт: за ящики по наряду теперь платят переводом. Подробности — в папке, закладка «СЧЁТ»"))
                             (coop-pay price) (save-plan)
                             (setf done (cond ((and (plusp premium) (paper-p :account))
                                               (format nil "Наряд сдан: ~/kiosk-trip::rub/ на счёт кооператива, из них премия ~/kiosk-trip::rub/" price premium))
                                              ((plusp premium) (format nil "Наряд сдан: +~/kiosk-trip::rub/, из них премия ~/kiosk-trip::rub/" price premium))
                                              (t (format nil "Наряд сдан: +~/kiosk-trip::rub/ В ~a этого уже хватает — премии нет. Где не хватает — в ведомости над холодильником" price (town-name town)))))
                             (when (> *talons* had)
                               (or (grant-paper :talons
                                                "Секретарь горсовета отрывает от листа талон: «За снабжение. С талоном в магазине отпустят то, чего в городе мало: один в руки, по прейскуранту»."
                                                "Горсовет выдал талон за ящик, которого ждали. Что с ним делать — в папке, закладка «ТАЛОНЫ»")
                                   (say-later (format nil "Горсовет выдал талоны: ~d. Теперь у нас ~d" (- *talons* had) *talons*)))))
                           (progn (add-money price)
                                  (setf done (format nil "Посылка вручена: +~/kiosk-trip::rub/" price))))
                       (settle-items) (refresh-seats) (save-interior)
                       (log-event (if (eq kind :buyer) :sold :delivered) :price price :town town :money *money* :dist (round *dist*))
                       (cond ((eq kind :buyer) (client-leave (format nil "Продали за ~/kiosk-trip::rub/" price)))
                             ((plusp (parcels-for town))
                              (setf (getf *client* :left) 70.0 (getf *client* :reach) 1.2)
                              (say (format nil "~a В ~a есть ещё — ставь следующую" done (town-name town))))
                             (t (client-leave done)))))
                   (setf (getf *client* :reach) 1.2))))))))
