;;;; Abroad. Two countries beyond the ring, and we see both only through the shops. A fraternal republic
;;;; is shipped goods out of the plan: once in a while a good is taken from the shelves of every town, the
;;;; shops go short and buyers pay more for it. The West sends goods in by sea through one town now and
;;;; then: there the shop overflows, and the plan pays to haul the surplus where it is short. The driver
;;;; runs it once a game day (STEP-PLAN); everyone hears of it at once and the radio repeats it.

(in-package #:kiosk-trip)

(defvar *abroad* '()
  "The countries beyond the border: entries of (key :side side :every days :at day :goods (good ...) :town n :news text).
SIDE :brother takes *EXPORT-SHARE* of every town's stock of one of GOODS; :west fills the shop of TOWN with
one of GOODS. It happens on the days when the day number mod EVERY is AT. NEWS is a format string that
takes the name of the good (accusative) and, for :west, the town; it starts with a lowercase letter: it is
read after the word for 'Radio:'.")

(defparameter *export-share* .7 "The share of the stocks a shipment to a fraternal country takes.")
(defvar *abroad-news* '() "The radio lines about abroad still to be read, the oldest first.")

(defun abroad-event (entry)
  "Let the country ENTRY (its plist) do its business today; return the line for everyone."
  (let* ((goods (remove-if-not (lambda (g) (assoc g *goods*)) (getf entry :goods)))
         (good (and goods (nth (random (length goods)) goods))))
    (when good
      (ecase (getf entry :side)
        (:brother (dotimes (tw (plan-size)) (set-stock tw good (* (- 1 *export-share*) (stock tw good))))
                  (format nil (getf entry :news) (good-name good)))
        (:west (let ((town (getf entry :town 0)))
                 (set-stock town good *stock-cap*)
                 (format nil (getf entry :news) (good-name good) (town-name town))))))))

(defun abroad-day (day)
  "Run what the countries abroad do on DAY; the driver calls it on a new day, before the plan is saved."
  (loop for (nil . entry) in *abroad*
        do (when (= (mod day (getf entry :every 7)) (getf entry :at 0))
             (let ((line (abroad-event entry)))
               (when line
                 (setf *abroad-news* (last (append *abroad-news* (list line)) 4))
                 (unless (inside-mega-p) (say-all (format nil "Радио: ~a" line)))
                 (log-event :abroad :line line))))))

(defun abroad-news () "Return the next radio line about abroad, or NIL." (pop *abroad-news*))
