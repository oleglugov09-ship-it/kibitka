;;;; The tank and the gas stations as the game plays them: fuel burns as we drive, R at a station fills up, P
;;;; buys planks, and stopping at a station, a booth or the observatory lowers the ladder and says where we are.
;;;; The stations themselves, as buildings, are in city.lisp.

(in-package #:kiosk-trip)

;;; Gas stations and the tank

(defvar *fuel* 1.0 "The fuel tank level; 1 means full.")
(defvar *at-station* nil "True while we stand at a gas station.")

(defshared pour-fuel :fuel ()
  "Fill up for *FUEL-CHUNK* roubles, or for as much as the money and the room in the tank allow.
The wallet is shared, so anyone can refuel."
  (when *at-station*
    (let* ((room (ceiling (* *tank-price* (- 1 *fuel*)))) (pay (min *fuel-chunk* (+ *money* *credit*) room))
           (from-account nil))                                             ; true if the account paid, wholly or in part
      (cond ((<= room 0) (say "Бак полный"))
            ((<= pay 0) (say "Денег нет. Развези посылки"))
            (t (setf from-account (and (not *dev*) (plusp (min *credit* pay))))
               (pay-by-transfer pay) (setf *fuel* (min 1.0 (+ *fuel* (/ pay *tank-price*))) *fuel-told* nil)
               (log-event :fuel :pay pay :fuel *fuel* :money *money* :dist (round *dist*))
               (say-all (if from-account
                            (format nil "Залили на ~/kiosk-trip::rub/ — со счёта кооператива. Хватит ещё на ~,1f км" pay (* *fuel* *tank-km*))
                            (format nil "Залили на ~/kiosk-trip::rub/ Хватит ещё на ~,1f км" pay (* *fuel* *tank-km*))))
               (let ((line (person-speaks :fuel))) (when line (say-later line 5.0))))))))                       ; the dispatcher on the air, a driver at the next pump

(defun buy-fuel () "Buy fuel on R at a gas station; return T if we are at one." (when *at-station* (pour-fuel) t))

(defshared take-planks :planks ()
  "Buy a bundle of planks for the stove."
  (when *at-station*
    (destructuring-bind (n price) *plank-price*
      (cond ((not (or *dev* (take-money price))) (say "На доски не хватает"))   ; on the dev polygon everything is free
            (t (incf *planks* n) (say-all (format nil "Купили досок: ~d за ~/kiosk-trip::rub/" n price)))))))

(defun buy-planks () "Buy planks on P at a gas station; return T if we are at one." (when *at-station* (take-planks) t))
(defvar *fuel-told* nil)
(defvar *fuel-out* nil "True while the tank is empty, so that this is written to the log only once.")

(defshared cheat-fuel :tank ()
  "Fill the tank for free; the F9 cheat."
  (setf *fuel* 1.0 *fuel-told* nil)
  (log-event :cheat :what :fuel :money *money* :dist (round *dist*))
  (say-all "Чит: бак полный"))

(defshared cheat-money :money ()
  "Add a hundred roubles to the shared wallet for free; the F8 cheat."
  (add-money 100)
  (log-event :cheat :what :money :money *money* :dist (round *dist*))
  (say-all (format nil "Чит: +5 р. В кошельке ~/kiosk-trip::rub/" *money*)))

(defun step-fuel (delta)
  (setf *fuel* (max 0.0 (- *fuel* (/ (* *speed* delta) (* 1000 *tank-km*)))))
  (when (<= *fuel* 0.0)
    (unless *fuel-out* (setf *fuel-out* t) (log-event :fuel-out :money *money* :dist (round *dist*) :next-station (next-station *dist*)))
    (slow-to 2.5))
  (when (> *fuel* .05) (setf *fuel-out* nil))                       ; empty tank: we coast along, barely moving
  (when (and (< *fuel* .15) (not *fuel-told*)) (setf *fuel-told* t) (say "Бак почти пуст. Ищи заправку")))

(defun step-station ()
  "Track whether we stand at a gas station.
Once we stop there the ladder lowers by itself: only at such places can one get out and walk around."
  (let ((at (and (find-if (lambda (d) (< (abs (- d *dist*)) 30)) *stations*) (< *speed* .4))))
    (when (and at (not *at-station*))
      (unless (net-guest-p) (setf *ladder-down* t))
      (say (if (net-guest-p)
               "Заправка. Лесенка опущена — можно выйти"
               (format nil "Заправка. R — бензин на ~/kiosk-trip::rub/ · P — ~d доски за ~/kiosk-trip::rub/ Лесенка опущена — можно выйти"
                       *fuel-chunk* (first *plank-price*) (second *plank-price*)))))
    (setf *at-station* at)
    ;; one can get out not only at a gas station: bus stop, observatory, drop-off point (finds.lisp)
    (let ((place (walk-place)))
      (when (and place (not *walk-here*) (not (eq (first place) :station)))
        (unless (net-guest-p) (when (member (first place) '(:observatory :depot :airmail :gimmick)) (setf *ladder-down* t)))
        (say (ecase (first place)
               (:stop "Остановка. E по лесенке за дверью — опустить её и выйти")
               (:observatory "Обсерватория. Лесенка опущена — выходи: на террасе большой телескоп")
               (:depot "Пункт приёма стеклотары и макулатуры. Лесенка опущена — сдавать у окошка")
               (:airmail "Авиапочта. Лесенка опущена — неси посылку к окошку, улетит самолётом")
               (:gimmick (format nil "~a. Лесенка опущена — подойди к окошку" (third (assoc (third place) *gimmick-plan*)))))))
      (setf *walk-here* (and place t)))))

(defun reset-tank ()
  "Start a trip: the tank as at the depot. START-TRIP calls it through RESET-ROADSIDE, together with the pocket."
  (setf *at-station* nil *fuel* *start-fuel* *fuel-told* nil))

(defun onboard-fuel ()
  "Set the starting fuel to exactly enough to reach the first town's market.
There we stall by the gas station and work out how to get by."
  (when *towns*
    (setf *fuel* (/ (+ (town-stop-d (first *towns*)) 4.0) (* 1000 *tank-km*)))))
