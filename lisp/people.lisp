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

;;; ------------------------------------------------------------------ the quarterly report
;;; Our own story has a scale: the loan, paid from nothing to all of it. A quarter of it is a quarter of the
;;; year's plan, and at the end of each the region sums up. Between the reports the people who answer for
;;; something gather good and bad days (*PEOPLE-SCORE*); at a report the worst is dismissed and the best is
;;; taken up to the capital - for good, there is no way back - and the scores start anew. The rest move on
;;; one step in their own stories: a line of a life is not told before its quarter. So nobody's story ends
;;; before ours, and the world waits for us: no payments, no reports.

(defvar *people-fate* '() "Who has left their post: entries of (key what quarter), what being :fired or :promoted.")
(defvar *people-score* '() "The good days minus the bad ones since the last report: entries of (key . number).")
(defvar *quarter-done* 0 "The number of quarterly reports already held: 0 to 4.")
(defvar *cargo-day* 0 "The crates of the plan's cargo delivered since the last day's count.")
(defvar *loan-seen* nil "The loan as it stood at the last day's count, and the days since it last went down: (left . days).")
(defvar *acting-lines* '() "What an acting official says, whoever he stands in for: a list of lines, from mods.")
(defvar *report-lines* '() "The words of the quarterly report, from mods: a plist of :title :fired :promoted :quiet :stays.")

(defun debt-quarter ()
  "Return how many quarters of the loan are paid: 0 to 4. Before the story begins it is 0."
  (if (and (boundp '*story-stage*) (not (eq (symbol-value '*story-stage*) :intro)) (plusp *debt-total*))
      (max 0 (min 4 (floor (* 4 (- *debt-total* *debt*)) *debt-total*)))
      0))

(defun person-fate (key) (second (assoc key *people-fate*)))
(defun person-in-post-p (key) (null (person-fate key)))

(defun person-mark (answers)
  "Return how the day went for someone who ANSWERS for a thing: 1 for a good day, -1 for a bad one, 0 for neither.
The forms: (:supply town) - the town's shortages; (:stock town) - goods piling up in its store; (:cargo) -
the plan's crates we delivered; (:order) - resales the inspectors have not caught; (:loan) - how we pay the
bank; (:deficit town) - shortages again, for one who lives off them."
  (destructuring-bind (what &optional town) answers
    (flet ((by (n good bad) (cond ((funcall good n) 1) ((funcall bad n) -1) (t 0))))
      (ecase what
        (:supply (by (length (short-goods town)) (lambda (n) (<= n 3)) (lambda (n) (>= n 5))))
        (:stock (by (length (surplus-goods town)) (lambda (n) (<= n 2)) (lambda (n) (>= n 4))))
        (:cargo (if (plusp *cargo-day*) 1 -1))
        (:order (cond ((zerop *heat*) 1) ((>= *heat* *heat-limit*) -1) (t 0)))
        (:loan (cond ((and *loan-seen* (zerop (cdr *loan-seen*))) 1) ((and *loan-seen* (>= (cdr *loan-seen*) 3)) -1) (t 0)))
        (:deficit (by (length (short-goods town)) (lambda (n) (>= n 5)) (lambda (n) (<= n 3))))))))

(defun people-day ()
  "Count one more day for everyone who answers for something and is still at the post; the driver runs it."
  (setf *loan-seen* (cond ((null *loan-seen*) (cons *debt* 1))
                          ((< *debt* (car *loan-seen*)) (cons *debt* 0))
                          (t (cons *debt* (1+ (cdr *loan-seen*))))))
  (loop for (key . p) in *people*
        do (when (and (getf p :answers) (person-in-post-p key))
             (let ((cell (or (assoc key *people-score*) (first (push (cons key 0) *people-score*)))))
               (incf (cdr cell) (person-mark (getf p :answers))))))
  (setf *cargo-day* 0)
  (save-people))

(defun hold-report ()
  "Hold the next quarterly report: dismiss the worst, send the best up, start the count anew.
Return the cards to show: the title, then a line for everyone who left, or a line that all stay."
  (let* ((quarter (incf *quarter-done*))
         (ranked (sort (loop for (key . score) in *people-score*
                             when (and (assoc key *people*) (person-in-post-p key)) collect (cons key score))
                       #'< :key #'cdr))
         (worst (and ranked (minusp (cdr (first ranked))) (car (first ranked))))
         (best (let ((top (first (last ranked)))) (and top (plusp (cdr top)) (not (eq (car top) worst)) (car top))))
         (cards (list (format nil (tr (getf *report-lines* :title)) quarter))))
    (flet ((leave (key what line)
             (push (list key what quarter) *people-fate*)
             (let ((p (rest (assoc key *people*))))
               (push (format nil "~a~%~%~a" (format nil (tr (getf *report-lines* what)) (tr (getf p :who)))
                             (if line (format nil "«~a»" (tr line)) ""))
                     cards))))
      (when worst (leave worst :fired nil))
      (when best (leave best :promoted (getf (rest (assoc best *people*)) :promoted))))
    (unless (or worst best) (push (tr (getf *report-lines* :quiet)) cards))
    (setf *people-score* '())
    (save-people)
    (nreverse cards)))

(defun acting-key (key) (intern (format nil "~a-ACTING" (symbol-name key)) :keyword))

(defun person-epilogue (key)
  "Return the line of the closing credits for the person KEY: where they ended up."
  (let* ((p (rest (assoc key *people*)))
         (line (getf p (ecase (person-fate key) ((nil) :stayed) (:fired :fired-end) (:promoted :promoted-end)))))
    (and line (format nil "~a — ~a" (tr (getf p :who)) (tr line)))))

(defun people-epilogue ()
  "Return the lines of the closing credits about the people: a heading, then everyone in turn."
  (let ((lines (remove nil (mapcar (lambda (e) (person-epilogue (first e))) *people*))))
    (and lines (append (list (tr (getf *report-lines* :stays)) "") lines (list "" "")))))

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
      (:shark (and (shark-met-p) (plusp *shark*)))
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
             (lives (getf p :life))
             (life (and (< (third rec) (length lives))                               ; a step of the life waits for its quarter of our story
                        (<= (floor (* 4 (third rec)) (length lives)) (debt-quarter))
                        (nth (third rec) lives)))
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
  "Return true if the post of the person with the plist P is at PLACE (a list like (:window :buyer), or a keyword) in TOWN."
  (let ((at (getf p :at)))
    (and (or (null (getf p :town)) (eql (getf p :town) town))
         (if (consp place)
             (and (consp at) (eq (first at) (first place)) (member (second place) (rest at)))
             (eq at place))
         (or (null (getf p :when)) (person-cond-p (getf p :when) (or town 0))))))

(defvar *person-spoke* nil "The key of the person who spoke last through PERSON-SPEAKS, or NIL for an acting official or nobody.")

(defun exile-line (key)
  "Return what the dismissed person KEY says when met away from the post, once; then they are gone for good."
  (let ((p (rest (assoc key *people*))) (rec (person-record key)))
    (when (and (getf p :exile) (/= (fourth rec) -2))
      (setf (fourth rec) -2)                                                       ; said: will not turn up again
      (save-people)
      (format nil "~a: «~a»" (tr (getf p :who)) (tr (getf p :exile))))))

(defun acting-line (key)
  "Return the line of whoever stands in for the person KEY who has left, once a day."
  (let* ((p (rest (assoc key *people*))) (rec (person-record (acting-key key))) (day (person-day)))
    (when (and (getf p :acting) *acting-lines* (/= (fourth rec) day))
      (setf (fourth rec) day)
      (incf (second rec))
      (save-people)
      (format nil "~a: «~a»" (tr (getf p :acting)) (tr (nth (mod (+ (second rec) (sxhash key)) (length *acting-lines*)) *acting-lines*))))))

(defun person-speaks (place &optional town)
  "Return the line of someone met at PLACE in TOWN who has not spoken today, or NIL.
The one tied to this very town goes before the ones met anywhere. Where the post is empty, the one acting
speaks instead. A dismissed person may be met once more among the buyers or on the fishing stage."
  (let* ((posts (remove-if-not (lambda (e) (person-here-p (rest e) place town)) *people*))
         (here (remove-if-not (lambda (e) (and (person-in-post-p (first e)) (/= (fourth (person-record (first e))) (person-day)))) posts)))
    (setf *person-spoke* nil)
    (or (and (or (equal place '(:window :buyer)) (equal place '(:booth 0)))
             (some (lambda (f) (and (eq (second f) :fired)
                                    (let ((line (exile-line (first f)))) (when line (setf *person-spoke* (first f)) line))))
                   *people-fate*))
        (loop for e in (stable-sort (copy-list here) (lambda (a b) (and (getf (rest a) :town) (not (getf (rest b) :town)))))
              do (let ((line (person-line (first e) (or town 0)))) (when line (setf *person-spoke* (first e)) (return line))))
        (some (lambda (e) (and (not (person-in-post-p (first e))) (acting-line (first e)))) posts))))

(defun save-people ()
  (when (and (fboundp 'net-guest-p) (not (net-guest-p)))
    (put-save :people (list :v2 *people-met* *people-fate* *people-score* *quarter-done* *cargo-day* *loan-seen*))))

(defun load-people ()
  (let ((f (get-save :people)) (met '(:each (keyword integer integer integer (:each string)))))
    (cond ((fits f (list '(:member :v2) met '(:each (keyword (:member :fired :promoted) integer)) '(:each cons) 'integer 'integer '(:or null cons)))
           (destructuring-bind (met fate score done cargo loan) (rest f)
             (setf *people-met* met
                   *people-fate* (remove-if-not (lambda (r) (assoc (first r) *people*)) fate)
                   *people-score* (remove-if-not (lambda (c) (and (assoc (car c) *people*) (integerp (cdr c)))) score)
                   *quarter-done* (max 0 (min 4 done)) *cargo-day* (max 0 cargo)
                   *loan-seen* (and loan (integerp (car loan)) (integerp (cdr loan)) loan))))
          ((fits f met) (setf *people-met* f)))))                                    ; a save from before the reports

(defstate :people
  :clear (lambda () (setf *people-met* '() *people-fate* '() *people-score* '() *quarter-done* 0 *cargo-day* 0 *loan-seen* nil))
  :load #'load-people :save #'save-people)
