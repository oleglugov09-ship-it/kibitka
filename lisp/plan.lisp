;;;; The plan. It is 1988: every town has a shop with fixed prices, a daily need and whatever the allocation
;;;; from above has sent it. The allocation never quite fits: one town is sent three times the bread it eats,
;;;; another is forgotten. That, and not a haggling cat, is where trade comes from now.
;;;;
;;;; For every town and good there are three numbers: the output (what the town makes a day), the norm (what
;;;; it uses a day) and the stock (what lies in its shop). Once a game day the stock changes: the makers
;;;; make, the allocation moves their output to the other towns in shares that are skewed anew every few
;;;; days, and every town uses its norm. Where the stock runs low there is a shortage, and a buyer at our
;;;; window pays an agreed price above the list: the lower the stock, the more. Where it piles up, the shop
;;;; sells at the list price. What grows by the road is made nowhere, so it is always short where it is wanted.
;;;;
;;;; The wagon is a co-operative: besides its own trade it carries the plan's own cargo, a crate of a good
;;;; from a town with a surplus to a town with a shortage, for a tariff. The tariff is paid by transfer
;;;; (*CREDIT*), which buys only what the state sells us: petrol and the bank's debt.
;;;;
;;;; This file needs no engine: tools/unit.lisp loads it and runs the plan for a month to see what it does.

(in-package #:kiosk-trip)

(defparameter *norm-wanted* 3 "The daily norm of a good in a town that is short of it by its nature.")
(defparameter *norm-own* 2 "The daily norm of a good in the town that makes it.")
(defparameter *norm-other* 1 "The daily norm of a state good in any other town.")
(defparameter *plan-cover* 1.04 "The share of all the norms that the makers' output covers: on paper the plan just about meets; the skewed shares do the rest.")
(defparameter *plan-week* 4 "The number of days an allocation holds before the shares are dealt anew.")
(defparameter *stock-cap* 40.0 "The most a shop holds of one good.")
(defparameter *reserve-days* 4 "The days of norm in stock below which there is a shortage.")
(defparameter *surplus-days* 7 "The days of norm in stock above which the shop has a surplus to sell.")
(defparameter *deal-markup* 2.0 "How far above the list price a buyer goes when the shop is empty: list × (1 + this).")
(defparameter *crate-units* 5 "The number of units of a good in one crate of the plan's cargo.")
(defparameter *plan-premium* 10 "The premium, in units of money, for a crate that reaches a town while it is short.")

(defvar *stocks* (make-hash-table :test 'equal) "The table from (town . good) to the stock in that town's shop.")
(defvar *plan-day* nil "The game day up to which the plan has been run, or NIL before the first run.")
(defvar *credit* 0 "The money on the co-operative's account, in units: paid by transfer, spent on petrol and the bank's debt.")

(defun state-good-p (kind) "Return true if KIND is a good the plan makes and allocates." (and (some (lambda (tr) (member kind (first tr))) *town-trade*) t))
(defun plan-goods () "Return every good some town has a norm for: the state goods and what is wanted of the road's growth."
  (remove-if-not (lambda (kind) (or (state-good-p kind) (some (lambda (tr) (member kind (second tr))) *town-trade*)))
                 (mapcar #'first *goods*)))
(defun plan-size () "Return the number of towns the plan covers." (length *town-trade*))

(defun norm (town good)
  "Return what TOWN uses of GOOD a day."
  (cond ((member good (town-wants town)) *norm-wanted*)
        ((member good (town-makes town)) *norm-own*)
        ((state-good-p good) *norm-other*)
        (t 0)))

(defun output (town good)
  "Return what TOWN makes of GOOD a day: the makers together cover *PLAN-COVER* of all the norms."
  (if (member good (town-makes town))
      (let ((makers (count-if (lambda (tr) (member good (first tr))) *town-trade*))
            (need (loop for tw below (plan-size) sum (norm tw good))))
        (/ (* *plan-cover* need) makers))
      0))

(defun stock (town good) (or (gethash (cons town good) *stocks*) 0.0))
(defun set-stock (town good value) (setf (gethash (cons town good) *stocks*) (max 0.0 (min *stock-cap* (float value 1.0)))))

(defparameter *written-off* .3 "The share of a shop's stock above ten days of norm that is written off each day: it spoils, or goes out by the back door.")

(defun write-off (town good)
  "Take from the shop in TOWN the part of its GOOD that will never be sold there."
  (let ((keep (* 10.0 (max 1 (norm town good)))))
    (when (> (stock town good) keep)
      (set-stock town good (- (stock town good) (* *written-off* (- (stock town good) keep)))))))

(defun share (town good day)
  "Return the weight of TOWN in the allocation of GOOD on DAY: its norm, skewed for the allocation's week."
  (* (norm town good) (+ .2 (* 1.6 (hash2 (+ (* 31 town) (* 7 (position good (mapcar #'first *goods*)))) (floor day *plan-week*))))))

(defun run-plan-day (day)
  "Run the plan for one DAY: make, allocate, use."
  (dolist (good (plan-goods))
    (let* ((towns (loop for tw below (plan-size) collect tw))
           (makers (remove-if-not (lambda (tw) (member good (town-makes tw))) towns))
           (others (remove-if (lambda (tw) (member tw makers)) towns))
           (pool 0.0))
      (dolist (tw makers)                                                ; a maker keeps its own norm and a little over, the rest is shipped
        (let ((ship (max 0.0 (- (output tw good) (norm tw good) .5))))
          (incf pool ship)
          (set-stock tw good (+ (stock tw good) (- (output tw good) ship (norm tw good))))))
      (let ((weights (loop for tw in others sum (share tw good day))))
        (dolist (tw others)
          (set-stock tw good (+ (stock tw good)
                                (if (plusp weights) (* pool (/ (share tw good day) weights)) 0.0)
                                (- (norm tw good))))))
      (dolist (tw towns) (write-off tw good)))))

(defun reset-plan (&optional (day 0))
  "Start the plan afresh: the shops are stocked, then eight days of allocation pull the towns apart."
  (clrhash *stocks*)
  (dolist (good (plan-goods))
    (dotimes (tw (plan-size))                                    ; a maker starts with its shop full, the others with their reserve
      (set-stock tw good (* (norm tw good) (if (member good (town-makes tw)) 10 *reserve-days*)))))
  (loop for d from (- day 8) below day do (run-plan-day d))
  (setf *plan-day* day))

(defun run-plan-to (day)
  "Bring the plan up to DAY, a day at a time (a month at the most: after a long absence the rest is not replayed)."
  (cond ((or (null *plan-day*) (< day *plan-day*) (zerop (hash-table-count *stocks*))) (reset-plan day))
        (t (loop for d from (max *plan-day* (- day 30)) below day do (run-plan-day d))
           (setf *plan-day* day))))

(defun shortage (town good)
  "Return how short TOWN is of GOOD: 0 with the reserve in stock, 1 with the shop empty."
  (let ((n (norm town good)))
    (if (zerop n) 0.0 (max 0.0 (min 1.0 (- 1.0 (/ (stock town good) (* n *reserve-days*))))))))

(defun surplus-p (town good)
  "Return true if the shop in TOWN has more of GOOD than it will use in a week."
  (and (state-good-p good) (>= (stock town good) (max 3.0 (* (norm town good) *surplus-days*)))))

(defun short-goods (town) "Return the goods TOWN is short of, the shortest first."
  (sort (remove-if (lambda (g) (< (shortage town g) .25)) (plan-goods)) #'> :key (lambda (g) (shortage town g))))
(defun surplus-goods (town) "Return the goods the shop in TOWN has a surplus of."
  (remove-if-not (lambda (g) (surplus-p town g)) (plan-goods)))

(defun list-price (good) "Return the list price of GOOD, the same in every shop." (good-base good))

(defun agreed-price (town good)
  "Return what a buyer in TOWN gives for GOOD: the list price and a markup that grows as the shop empties."
  (max 1 (round (* (list-price good) (+ 1.0 (* *deal-markup* (max .15 (shortage town good))))))))

(defun plan-cargo (town)
  "Return the plan's cargo waiting in TOWN as (destination good), or NIL: a surplus here that is short elsewhere."
  (let ((best nil) (worst .3))
    (dolist (good (surplus-goods town) best)
      (dotimes (tw (plan-size))
        (when (and (/= tw town) (> (shortage tw good) worst))
          (setf worst (shortage tw good) best (list tw good)))))))

;;; A crate of the plan's cargo is a parcel whose address says what is in it: the town, plus hundreds from
;;; 900 up that name the good (story parcels use the hundreds below, istorii.lisp).

(defun plan-addr (town good) (+ town (* 100 (+ 900 (position good (mapcar #'first *goods*))))))
(defun parcel-good (addr)
  "Return the good in the crate with address ADDR if it is the plan's cargo, else NIL."
  (let ((id (and (integerp addr) (floor addr 100))))
    (and id (>= id 900) (first (nth (- id 900) *goods*)))))

(defun deliver-cargo (addr)
  "Put the plan's crate ADDR into the shop of its town; return the premium earned, in units of money."
  (let ((town (mod addr 100)) (good (parcel-good addr)))
    (if good
        (let ((short (>= (shortage town good) .25)))
          (set-stock town good (+ (stock town good) *crate-units*))
          (if short *plan-premium* 0))
        0)))

(defun plan-name (good) "Return the name of GOOD for the supply sheet." (or (second (assoc good *kinds*)) (good-name good)))

(defun supply-sheet (town)
  "Return the supply sheet of TOWN as text: every good with its stock, its norm and how things stand."
  (format nil "~a · ведомость снабжения~%~%~{~a~^~%~}"
          (town-name town)
          (loop for good in (sort (copy-list (plan-goods)) #'> :key (lambda (g) (shortage town g)))
                when (plusp (norm town good))
                  collect (let ((have (round (stock town good))) (n (norm town good)))
                            (cond ((>= (shortage town good) .25)
                                   (format nil "~a — на складе ~d, норма ~d в день: НЕ ХВАТАЕТ, дают ~/kiosk-trip::rub/" (plan-name good) have n (agreed-price town good)))
                                  ((surplus-p town good)
                                   (format nil "~a — на складе ~d, норма ~d в день: ИЗЛИШЕК, по прейскуранту ~/kiosk-trip::rub/" (plan-name good) have n (list-price good)))
                                  (t (format nil "~a — на складе ~d, норма ~d в день" (plan-name good) have n)))))))

(defun pay-by-transfer (sum)
  "Pay SUM to the state: from the account first, the rest in cash. Return true if there was enough; on the dev polygon it is free."
  (cond (*dev* t)
        ((< (+ *credit* *money*) sum) nil)
        (t (let ((from-account (min *credit* sum)))
             (decf *credit* from-account)
             (decf *money* (- sum from-account))
             t))))

;;; ------------------------------------------------------------------ in the game: the day, the save, the guests

(defun save-plan ()
  (unless (net-guest-p)
    (put-save :plan (list *plan-day* *credit*
                          (loop for key being the hash-keys of *stocks* using (hash-value v)
                                collect (list (car key) (cdr key) (round v)))))))

(defun load-plan ()
  (let ((f (get-save :plan)))
    (when (fits f '((:or integer null) integer (:each (integer keyword real))))
      (destructuring-bind (day credit stocks) f
        (clrhash *stocks*)
        (loop for (town good value) in stocks
              do (when (and (< -1 town (plan-size)) (assoc good *goods*)) (set-stock town good value)))
        (setf *plan-day* day *credit* (max 0 credit))))))

(defstate :plan
  :clear (lambda () (clrhash *stocks*) (setf *plan-day* nil *credit* 0))
  :load #'load-plan :save #'save-plan)

(defun step-plan ()
  "Bring the plan up to today; the driver runs it."
  (let ((day (today)))
    (unless (eql day *plan-day*)
      (run-plan-to day)
      (save-plan))))

;; a guest sees the driver's books for the town at hand, and the co-operative's account
(defcommon :plan
  :value (let ((town (trade-town))) (list town *credit* (mapcar (lambda (g) (round (stock town g))) (plan-goods))))
  :spec '(integer integer (:each integer)) :driver t
  :accept (lambda (v)
            (destructuring-bind (town credit stocks) v
              (setf *credit* (max 0 credit))
              (when (and (< -1 town (plan-size)) (= (length stocks) (length (plan-goods))))
                (loop for good in (plan-goods) for n in stocks do (set-stock town good n))))))
