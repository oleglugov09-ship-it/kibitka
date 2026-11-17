;;;; Layout: where along the path the towns, bus stops and gas stations are. Pure calculation, no engine
;;;; needed, so the file also loads in plain Lisp for the quick rule checks (tools/unit.lisp). The buildings
;;;; are placed from this layout by doroga.lisp and city.lisp; karta.lisp assembles the road map from it.

(in-package #:kiosk-trip)

(defvar *town-names* '())

(defun town-name (i)
  "Return the name of town number I.
When the names run out they go round again with a number: 'Zarechny-2'."
  (let ((n (length *town-names*)))
    (if (< i n) (nth i *town-names*) (format nil "~a-~d" (nth (mod i n) *town-names*) (1+ (floor i n))))))

(defparameter *stop-gap* '(260.0 520.0) "The range of distances between bus stops, m.")

(defun between (range) (+ (first range) (random (- (second range) (first range)))))

(defparameter *town-gap* '(900.0 1600.0) "The range of distances between towns, m.")

(defparameter *town-len* '(760.0 1100.0) "The range of town lengths, m.")

(defstruct town
  "A town along the road.
FROM is where the previous town ended (the start of the stretch leading to this one)."
  from start end index title
  memory                       ; it has the street you remember
  stele centre plaza)

(defvar *towns* '() "The towns (TOWN structures) in ascending path coordinate; those left far behind are dropped.")

(defvar *town-upto* 0.0 "The path coordinate up to which towns are already laid out.")

(defvar *town-n* 0)

(defparameter *town-count* 12 "The total number of towns on the road.
The world is permanent: the same seed gives the same road and the same towns.")

(defun plan-towns (upto)
  "Lay out the towns up to path coordinate UPTO and return the list of towns.
The first one comes soon and has the street you remember; after that every third one has it."
  (loop while (and (< *town-upto* upto) (< *town-n* *town-count*))
        do (let* ((*seed* (rs-for (+ 500 *town-n*)))
                  (start (+ *town-upto* (if (zerop *town-n*) 330.0 (between *town-gap*))))
                  (end (+ start (between *town-len*))))
             (setf *towns* (append *towns* (list (make-town :from *town-upto* :start start :end end :index *town-n*
                                                            :title (town-name *town-n*) :memory (zerop (mod *town-n* 3)))))
                   *town-upto* end)
             (incf *town-n*)))
  *towns*)

(defun town-stop-d (tw)
  "Return the path coordinate of the bus stop in the middle of town TW.
Parcels are handed over there and the wholesaler trades there. It is the first lamp post past the
middle plus 9 m, the same way STEP-ROADSIDE computes it."
  (let ((mid (/ (+ (town-start tw) (town-end tw)) 2)))
    (+ 9.0 (* 26.0 (1+ (floor mid 26.0))))))

(defvar *extra-stations* '() "The gas stations placed by the scene plan (vista.lisp), replacing the halfway one.")

(defun town-stations (tw)
  "Return the path coordinates of the gas stations belonging to town TW.
They stand on the stretch before it (if it is long) and at the exit."
  (append (cond ((find-if (lambda (s) (< (town-from tw) s (town-start tw))) *extra-stations*)   ; a special one, from the scene plan
                 (list (find-if (lambda (s) (< (town-from tw) s (town-start tw))) *extra-stations*)))
                ((> (- (town-start tw) (town-from tw)) *mid-station-gap*)
                 (list (/ (+ (town-from tw) (town-start tw)) 2))))
          ;; in the first town the gas station stands right by the market: you arrive here on the last drops
          (when (zerop (town-index tw)) (list (- (town-stop-d tw) 24.0)))
          (list (+ (town-start tw) (* .8 (- (town-end tw) (town-start tw)))))))

(defun next-station (d)
  "Return the path coordinate of the nearest gas station beyond D, or NIL."
  (if (ring-d-p d)
      (loop for tw in *towns* thereis (find-if (lambda (s) (> s d)) (town-stations tw)))
      (find-if (lambda (s) (and (> s d) (eq (edge-of s) (edge-of d)))) (sort (copy-list *spoke-stations*) #'<))))

(defun town-at (d) (find-if (lambda (tw) (<= (town-start tw) d (town-end tw))) *towns*))

(defun stop-after (d)
  "Return the path coordinate of the next bus stop after the one at path coordinate D.
It depends only on the seed and D."
  (let ((*seed* (rs-for d))) (+ d (between *stop-gap*))))
