;;;; The pocket: the roubles we carry. They are counted in units of *KOPECKS* (delo.lisp).
;;;; Every change of the pocket goes through this file: ADD-MONEY and TAKE-MONEY for what happens in the game,
;;;; PUT-MONEY when the whole sum comes from outside (a save, a new game). The pocket is an aktor (aktor.lisp):
;;;; the driver's, and a guest's *MONEY* mirrors it.
;;;; A rule about money - a limit, a record, a refund - then has one place to live in.
;;;; The co-operative's account is another purse and lives with the plan (plan.lisp: COOP-PAY, PAY-BY-TRANSFER,
;;;; DRAW-ACCOUNT).

(in-package #:kiosk-trip)

(defvar *money* 10 "The number of roubles in the pocket: the mirror of the pocket's state, for reading.")

(defaktor :kassa :state '(:money 10) :spec '((:member :money) integer) :vars '(:money *money*))   ; one pocket for the whole wagon, the driver's

(defun add-money (sum) "Put SUM into the pocket." (poke :kassa :money (+ (peek :kassa :money) sum)))

(defun take-money (sum)
  "Take SUM out of the pocket. Return NIL, taking nothing, if there is less than SUM there."
  (when (>= (peek :kassa :money) sum) (poke :kassa :money (- (peek :kassa :money) sum)) t))

(defun put-money (sum) "Set the pocket to SUM: a save being read, a new game." (poke :kassa :money sum))

(defun pay (sum)
  "Pay SUM roubles.
Return NIL and show a hint if there is not enough money."
  (cond (*dev* t)                                                        ; on the dev polygon everything is free
        ((take-money sum) t)
        (t (say (format nil "Нужно ~/kiosk-trip::rub/, а у нас ~/kiosk-trip::rub/" sum *money*)) nil)))
