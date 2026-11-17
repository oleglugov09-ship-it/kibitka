;;;; Goods and trade between towns. Every town makes something, which is sold cheap there, and is short
;;;; of something, which fetches a high price there (the tables are in economy.lisp). A seller at the window
;;;; offers only local goods, a buyer looks only for what is short: you cannot buy and resell the same thing
;;;; on the spot, the goods have to be hauled. Between towns, clients at bus stops follow the nearest town.

(in-package #:kiosk-trip)

(defun dish-p (kind) "Return the recipe entry if KIND is a cooked dish." (find kind *recipes* :key #'second))
(defun dish-of (raw) "Return the dish cooked from the raw good RAW, or NIL." (second (assoc raw *recipes*)))
(defun raw-of (kind) "Return the raw good the dish KIND is cooked from; for a non-dish return KIND itself." (let ((r (dish-p kind))) (if r (first r) kind)))
(defun good-name (kind) (cond ((dish-p kind) (third (dish-p kind))) (t (or (second (assoc kind *goods*)) "товар"))))
(defun good-base (kind) (or (third (assoc kind *goods*)) 5))
(defun town-makes (i) (first (nth i *town-trade*)))
(defun town-wants (i) (second (nth i *town-trade*)))

(defun trade-town (&optional town)
  "Return the number of the town whose rules trade follows here.
That is TOWN if given, otherwise the town nearest to the wagon."
  (or town
      (let ((best nil))
        (multiple-value-bind (x z) (road-point *dist*)             ; nearest on the map, not by path coordinate
          (flet ((far (tw) (multiple-value-bind (tx tz) (road-point (town-stop-d tw)) (+ (expt (- tx x) 2) (expt (- tz z) 2)))))
            (dolist (tw *towns* (and best (town-index best)))
              (when (or (null best) (< (far tw) (far best))) (setf best tw))))))
      0))

(defun trade-good (kind town)
  "Return the good a client of KIND brings (a seller) or wants (a buyer) in TOWN."
  (let ((list (if (eq kind :seller) (town-makes (trade-town town)) (town-wants (trade-town town)))))
    (when (member kind '(:buyer :seller)) (nth (random (length list)) list))))

(defun rush-p (&optional (town (trade-town)))
  "Return true during the factory whistle in Krasny Kut.
The shift is leaving the factory: buyers are many and they pay more."
  (and (eql town *rush-town*) (< (first *rush-hours*) (day-phase *day-t*) (second *rush-hours*))))

(defun trade-price (good kind town)
  "Return the price of GOOD for a client of KIND in TOWN.
A seller's price is low (the good is made here), a buyer's is high (the good is missing here)."
  (let* ((range (if (eq kind :seller) *sell-range* *buy-range*))
         (k (+ (first range) (random (- (second range) (first range))))))
    (max 1 (round (* (good-base good) k
                     (glut-factor good kind (trade-town town))               ; the market is glutted, so the price has sagged
                     (if (and (eq kind :buyer) (rush-p (trade-town town))) *rush-bonus* 1))))))

;;; Market glut: the more of a good we have sold in a town, the less it fetches there; the more we have
;;; bought up, the more they ask. With time the market settles. One and the same run stops being golden.

(defvar *glut* '() "The recent deals: entries of ((town good side) . count), sold (:sold) or bought (:bought).")

(defun glut-factor (good kind town)
  (let ((n (or (cdr (assoc (list town good (if (eq kind :buyer) :sold :bought)) *glut* :test #'equal)) 0)))
    (if (eq kind :buyer) (max *glut-floor* (- 1 (* *glut-step* n))) (+ 1 (* *glut-step* n)))))

(defun note-trade (good side)
  "Record a deal: SIDE :sold means we sold GOOD in this town, :bought means we bought it."
  (let* ((key (list (trade-town (and *client* (getf *client* :town))) (raw-of good) side))
         (cell (or (assoc key *glut* :test #'equal) (first (push (cons key 0.0) *glut*)))))
    (incf (cdr cell) 1.0)))

(defun step-glut (delta)
  (dolist (c *glut*) (setf (cdr c) (max 0.0 (- (cdr c) (/ delta *glut-rest*)))))
  (setf *glut* (remove-if (lambda (c) (<= (cdr c) 0.0)) *glut*)))

(defun delivery-pay ()
  "Return the pay from the recipient for a delivered parcel (more with the town council stamp)."
  (* *parcel-delivery* (if *stamp* *stamp-factor* 1)))

(defun client-good ()
  "Return the good of the current client: what a seller brings or what a buyer looks for.
While the client walks to the window the goods are in :GOODS as (z kind address), at the window they
are in :STALL as (node z kind address)."
  (when *client*
    (let ((items (or (mapcar #'rest (getf *client* :stall)) (getf *client* :goods))))
      (case (getf *client* :kind)
        (:buyer (let ((coins (find :coins items :key #'second))) (and coins (third coins))))
        (:seller (second (first items)))))))

(defun wanted-p (kind)
  "Return true if the current buyer will take a good of KIND.
That is the good sought or a dish made from it (blini will do for milk)."
  (let ((want (client-good)))
    (and want (or (eq kind want) (eq (raw-of kind) want)))))

(defun market-note (i)
  "Return one line saying what town I sells and what it looks for."
  (format nil "~a: продают ~{~a~^, ~}; ищут ~{~a~^, ~}" (town-name i)
          (mapcar #'good-name (town-makes i)) (mapcar #'good-name (town-wants i))))

(defvar *market-told* nil "The market that has already been announced.")

(defun step-market-note ()
  "Announce what is traded here once the wagon stops at a market (once per arrival)."
  (let ((place (place-here)))
    (cond ((and place (eq (first place) :market))
           (unless (or (eql *market-told* (third place)) (and *toast* (> (cdr *toast*) 0)))
             (setf *market-told* (third place))
             (say-all (format nil "Рынок. ~a~:[~;. Гудок: смена идёт с завода!~]" (market-note (third place)) (rush-p (third place))))))
          ((> *speed* 2.0) (setf *market-told* nil)))))

(defun build-good (g kind)
  "Build the good KIND as a thing in G: it stands on a shelf, is carried in paws, goes on the windowsill."
  (ecase kind
    (:fried (shape g :cyl '(.1 .1 .012 12) "#e4e0d4" 0 .006 0)                          ; a plate with a browned fish on it
            (shape g :ball '(.085 8 5) "#b8782c" 0 .04 0 :scale '(.6 .4 1.3))
            (shape g :cone '(.05 .08 4) "#9a6424" 0 .04 .14 :rot (list (/ pi 2) 0 0) :scale '(.3 1 1)))
    (:jam (shape g :cyl '(.05 .05 .13 10) "#a3262c" 0 .065 0 :rough .2)                 ; a jar with a lid
          (shape g :cyl '(.054 .054 .025 10) "#d8a840" 0 .142 0 :metal .6 :rough .4)
          (shape g :box '(.07 .05 .004) "#f0ece0" 0 .07 -.05 :shadow nil))
    (:toast (shape g :cyl '(.1 .1 .012 12) "#e4e0d4" 0 .006 0)
            (dotimes (i 3) (shape g :box '(.09 .018 .07) "#c8843c" (* .02 (- i 1)) (+ .022 (* i .02)) (* .015 (- i 1)) :rot (list 0 (* i .4) 0))))
    (:stew (shape g :cyl '(.09 .07 .08 12) "#d8d2c0" 0 .04 0)                           ; a bowl with stew in it
           (shape g :cyl '(.082 .082 .012 12) "#7a4a2c" 0 .078 0 :shadow nil)
           (dotimes (i 3) (shape g :ball '(.016 5 4) "#9a5a34" (* .03 (cos (* i 2.1))) .09 (* .03 (sin (* i 2.1))) :shadow nil)))
    (:fish (shape g :ball '(.1 8 5) "#8aa0b0" 0 .06 0 :scale '(.6 .5 1.4) :metal .4 :rough .4)
           (shape g :cone '(.06 .1 4) "#6a8090" 0 .06 .17 :rot (list (/ pi 2) 0 0) :scale '(.3 1 1)))
    (:bread (shape g :ball '(.11 8 5) "#c8843c" 0 .07 0 :scale '(1.5 .7 .9))
            (dolist (z '(-.05 0 .05)) (shape g :box '(.02 .01 .1) "#e8c080" z .145 0 :rot '(0 .5 0) :shadow nil)))
    (:coal (shape g :box '(.2 .14 .18) "#5a4636" 0 .07 0)
           (dotimes (i 5) (shape g :rock '(.05) "#1a1a1e" (- (* (mod i 3) .06) .06) .17 (- (* (floor i 3) .07) .03))))
    (:tools (shape g :box '(.24 .1 .12) "#a3262c" 0 .05 0) (shape g :box '(.14 .02 .02) "#2a2c30" 0 .13 0)
            (dolist (x '(-.06 .06)) (shape g :box '(.02 .04 .02) "#2a2c30" x .11 0)))
    (:books (dotimes (i 3) (shape g :box '(.16 .035 .22) (nth i '("#6a3a2e" "#2c4a7a" "#3c5a48")) (* .01 (- i 1)) (+ .018 (* i .037)) 0 :rot (list 0 (* .08 (- i 1)) 0)))
            (shape g :box '(.01 .12 .012) "#c9a13a" 0 .06 .115 :shadow nil))
    (:cans (dolist (x '(-.045 .045)) (shape g :cyl '(.04 .04 .1 10) "#b0aa98" x .05 0 :metal .7 :rough .3)
                                     (shape g :cyl '(.041 .041 .05 10) "#a3262c" x .05 0 :shadow nil)))
    (:water (shape g :cyl '(.04 .045 .2 8) "#7ab0c8" 0 .1 0 :alpha .7 :rough .2) (shape g :cyl '(.02 .035 .07 6) "#7ab0c8" 0 .235 0 :alpha .7)
            (shape g :cyl '(.046 .046 .06 8) "#e8e0d0" 0 .1 0 :shadow nil))
    (:honey (shape g :cyl '(.07 .06 .13 10) "#d89a20" 0 .065 0 :rough .3) (shape g :cyl '(.075 .075 .03 10) "#e8e0d0" 0 .145 0)
            (shape g :box '(.08 .05 .005) "#f0ece0" 0 .07 .068 :shadow nil))
    (:berry (shape g :cyl '(.09 .07 .1 8) "#8a6440" 0 .05 0)
            (dotimes (i 7) (shape g :ball '(.022 5 3) "#a3202c" (* .05 (cos i)) .11 (* .05 (sin i)) :shadow nil)))))

(setf *kinds* (append *kinds*
                      '((:fish "рыба" (.16 .12 .3)) (:bread "хлеб" (.3 .15 .2)) (:coal "уголь" (.2 .2 .18)) (:tools "инструмент" (.24 .15 .12))
                        (:books "книги" (.18 .12 .24)) (:cans "тушёнка" (.2 .1 .1)) (:water "минеральная вода" (.1 .27 .1))
                        (:honey "мёд" (.15 .16 .15)) (:berry "клюква" (.18 .14 .18))
                        ;; dishes off the cooktop (uyut.lisp): blini are listed in interior.lisp
                        (:fried "жареная рыба" (.22 .08 .22)) (:jam "варенье" (.12 .17 .12))
                        (:toast "гренки" (.22 .08 .22)) (:stew "горячая тушёнка" (.2 .12 .2)))))
