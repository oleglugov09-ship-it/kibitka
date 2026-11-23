;;;; Where the wagon is. The game has places: the ring - the road with its towns, where everything of the game
;;;; goes on - and the worlds one reaches through a door (mega.lisp), each laid out around the wagon as it drives.
;;;; What the rest of the game needs to know of the place it asks here, not of the worlds themselves: whether
;;;; cars drive, whether the radio is heard, whether the map of the region shows it, whether the clock runs.

(in-package #:kiosk-trip)

(defvar *place* :ring "Where the wagon is: :RING, or the key of the world behind the door it went through.")
(defvar *mega-s* 0.0 "How far we have driven in the place behind the door, in metres, since the ring went out of sight.")

;;; The rules of a place say what goes on there as it does on the ring; what a place does not name does not go on.
;;;   :TRAFFIC      other cars drive;
;;;   :RADIO        the stations are heard, and the news comes;
;;;   :MAP          the map of the region shows the place;
;;;   :DESTINATION  the autopilot has somewhere to go, and the corner of the screen says where;
;;;   :CLOCK        the clock of the game runs, and the calendar turns;
;;;   :PUMPS        there is somewhere to fill the tank.

(defvar *place-rules* (list :ring '(:traffic t :radio t :map t :destination t :clock t :pumps t))
  "The rules of each place, a plist from the place to a plist of rules: see above.")

(defun place-rule (rule) "Return true if RULE holds where the wagon is." (getf (getf *place-rules* *place*) rule))
(defun off-ring-p () "Return true if the wagon is in a place behind a door, not on the ring." (not (eq *place* :ring)))
