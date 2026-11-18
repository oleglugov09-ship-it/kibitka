;;;; The people of the road: one or two for every link in how this country feeds and clothes itself - the
;;;; factory, the supply, the shop, the transport, the village, the inspectors, the bank, the shadow, the
;;;; council, and those who stand in the queue. They are who we already meet: the cat at the window, the
;;;; clerk in a booth, the driver at the pump. Each says one line per game day at the most.
;;;;
;;;; A person has two kinds of lines (mods, *PEOPLE*). The lines about the world each carry a condition
;;;; on what the plan knows - a shortage, the season, a holiday, how the town stands to us - so the same
;;;; shopkeeper says different things in winter and after our crate has come. The lines of a life are
;;;; told in order, one a meeting: a small story of their own that moves on as we keep coming back.
;;;; When the world has something new to say, it goes first; otherwise the life moves on a step.
;;;; This file needs no engine.

(in-package #:kiosk-trip)

(defvar *people* '()
  "The people, from mods: entries of (key . plist). :who is the name with the trade; :town is the town
number, or NIL for someone met anywhere; :at is where: (:window kind ...), (:booth number), :fuel or :camp;
:when is a condition for turning up at all; :world is a list of (condition line); :life is a list of lines.")

(defvar *people-met* '()
  "What we know of each person: entries of (key meetings life-lines-heard day-last-met world-lines-heard).
A line about the world is heard once while its condition holds; when it stops holding, the line is forgotten
and will be news again the next time.")

(defun person-day () "Return the game day, or 0 without the engine."
  (if (and (boundp '*day-t*) (boundp '*day-len*)) (floor (symbol-value '*day-t*) (symbol-value '*day-len*)) 0))

(defun person-cond-p (cond town)
  "Return true if the condition COND holds in TOWN now.
The forms: (:always), (:short [good]), (:plenty [good]), (:rationed), (:season s), (:holiday [key]),
(:friend), (:feud), (:heat), (:shark), (:shark-paid), (:debt-half), (:talons), (:no-talons), (:rich)."
  (destructuring-bind (what &optional arg) cond
    (ecase what
      (:always t)
      (:short (if arg (>= (shortage town arg) .25) (and (short-goods town) t)))
      (:plenty (if arg (surplus-p town arg) (and (surplus-goods town) t)))
      (:rationed (and (rationed-goods town) t))
      (:season (eq *season* arg))
      (:holiday (if arg (eq (holiday-key) arg) (and (holiday) t)))
      (:friend (eq (standing town) :friend))
      (:feud (eq (standing town) :feud))
      (:heat (plusp *heat*))
      (:shark (plusp *shark*))
      (:shark-paid (zerop *shark*))
      (:debt-half (<= *debt* (/ *debt-total* 2)))
      (:talons (plusp *talons*))
      (:no-talons (zerop *talons*))
      (:rich (>= *credit* 20000)))))

(defun person-record (key)
  (or (assoc key *people-met*) (first (push (list key 0 0 -1 nil) *people-met*))))

(defun person-line (key town)
  "Return what the person KEY says to us in TOWN now, as 'who: «words»', and remember the meeting.
Return NIL if we have already spoken today or there is nothing left to say."
  (let* ((p (rest (assoc key *people*))) (rec (person-record key)) (day (person-day)))
    (when (and p (/= (fourth rec) day))
      (setf (fifth rec)                                                ; what is no longer true may be said again one day
            (remove-if-not (lambda (heard) (let ((w (find heard (getf p :world) :key #'second :test #'equal)))
                                             (and w (person-cond-p (first w) town))))
                           (fifth rec)))
      (let* ((news (find-if (lambda (w) (and (not (eq (first (first w)) :always)) (person-cond-p (first w) town)
                                             (not (member (second w) (fifth rec) :test #'equal))))
                            (getf p :world)))
             (life (nth (third rec) (getf p :life)))
             (idle (find-if (lambda (w) (eq (first (first w)) :always)) (getf p :world)))
             (line (cond (news (push (second news) (fifth rec)) (second news))
                         (life (incf (third rec)) life)
                         (idle (second idle)))))
        (when line
          (incf (second rec))
          (setf (fourth rec) day)
          (save-people)
          (format nil "~a: «~a»" (tr (getf p :who)) (tr line)))))))

(defun person-here-p (p place town)
  "Return true if the person with the plist P is met at PLACE (a list like (:window :buyer), or a keyword) in TOWN."
  (let ((at (getf p :at)))
    (and (or (null (getf p :town)) (eql (getf p :town) town))
         (if (consp place)
             (and (consp at) (eq (first at) (first place)) (member (second place) (rest at)))
             (eq at place))
         (or (null (getf p :when)) (person-cond-p (getf p :when) (or town 0))))))

(defun person-speaks (place &optional town)
  "Return the line of someone met at PLACE in TOWN who has not spoken today, or NIL.
The one tied to this very town goes before the ones met anywhere."
  (let ((here (remove-if-not (lambda (e) (and (person-here-p (rest e) place town) (/= (fourth (person-record (first e))) (person-day))))
                             *people*)))
    (loop for e in (stable-sort (copy-list here) (lambda (a b) (and (getf (rest a) :town) (not (getf (rest b) :town)))))
          do (let ((line (person-line (first e) (or town 0)))) (when line (return line))))))

(defun save-people ()
  (when (and (fboundp 'net-guest-p) (not (net-guest-p))) (put-save :people *people-met*)))

(defun load-people ()
  (let ((f (get-save :people)))
    (when (fits f '(:each (keyword integer integer integer (:each string))))
      (setf *people-met* (remove-if-not (lambda (r) (assoc (first r) *people*)) f)))))

(defstate :people
  :clear (lambda () (setf *people-met* '()))
  :load #'load-people :save #'save-people)
