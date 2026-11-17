;;;; Town stories. No cutscenes: a story travels in parcels. The sender has a name and a few words about
;;;; what is being sent and to whom; the recipient answers; a town or two later one of them recognises you
;;;; at the window and asks how things are over there; the radio retells what you delivered. A chain is
;;;; several parcels between the same cats; the next parcel appears once the previous one is handed over.
;;;;
;;;; There is no engine here: a table of stories and the bookkeeping of who owes what to whom. It is hooked
;;;; into trade at four points of trade.lisp (the sender, parcel taken, parcel delivered, the acquaintance
;;;; at the window) and into the radio (uyut.lisp).

(in-package #:kiosk-trip)

(defvar *stories* '()
  "The stories: entries of (key step...).
A step is (from-town to-town sender sender-words recipient recipient-words radio-summary (town who words));
the final triple says who later recognises us at the window, where, and what they ask. The summary
starts with a lowercase letter: the radio reads it after the word for 'Radio:'.")

(defmacro define-story (key &body steps)
  "Define the story KEY as the chain of STEPS; a story with the same key is replaced.
This is how a mod adds a story: see mods/stories/mod.lisp for the shape of a step."
  `(progn (setf *stories* (append (remove ,key *stories* :key #'first) (list (list ,key ,@(mapcar (lambda (s) `',s) steps)))))
          ,key))

(defvar *story-at* '() "The progress of each story: entries of (key . number of the step now waiting to be sent).")
(defvar *story-transit* '() "The story parcels in transit: entries of (recipient-town key step-number).")
(defvar *story-after* '() "The cats who will recognise us at the next meeting: entries of (town who words).")
(defvar *news* '() "The radio summaries of what we have delivered; the first is the oldest.")
(defvar *story-late* nil "The line to be said a little later, as (when . text): after the line about money.")

(defun story-step (key) (or (cdr (assoc key *story-at*)) 0))
(defun story-record (key n) (nth n (rest (assoc key *stories*))))

;;; A story parcel has an address of its own: the town number plus hundreds encoding the story and step. It
;;; shows whose parcel it is: the crate says who it is from, and the recipient answers that very parcel
;;; rather than the first one that happens to come to this town.

(defun parcel-addr (town &optional key n)
  "Return the address of a parcel to town TOWN.
With KEY and N it is a parcel of story KEY, step N."
  (+ town (if key (* 100 (+ 1 (* 10 (position key *stories* :key #'first)) n)) 0)))
(defun parcel-town (addr) "Return the town the parcel with address ADDR goes to." (mod addr 100))
(defun parcel-story (addr)
  "Return whose parcel ADDR is: (key step step-record) for a story parcel, NIL for an ordinary one."
  (let ((id (floor addr 100)))
    (when (plusp id)
      (let* ((story (nth (floor (1- id) 10) *stories*)) (n (mod (1- id) 10)) (step (and story (nth n (rest story)))))
        (and step (list (first story) n step))))))
(defun parcel-from (addr) "Return the sender of the story parcel ADDR, or NIL." (let ((s (parcel-story addr))) (and s (third (third s)))))
(defun parcel-to (addr) "Return the recipient of the story parcel ADDR, or NIL." (let ((s (parcel-story addr))) (and s (fifth (third s)))))

(defun story-sender (town)
  "Return the story sender in town TOWN if one of the stories is waiting to be sent from there.
The result is (recipient-town who words key step-number), or NIL."
  (loop for (key . steps) in *stories*
        do (let* ((n (story-step key)) (step (nth n steps)))
             (when (and step (= (first step) town) (notany (lambda (tr) (eq (second tr) key)) *story-transit*))
               (return (list (second step) (third step) (fourth step) key n))))))

(defun story-taken (key n)
  "Note that the parcel of story KEY (step N) was taken into the wagon.
From now on it is awaited in the recipient town."
  (let ((step (story-record key n)))
    (when step (push (list (second step) key n) *story-transit*) (save-stories))))

(defun story-delivered (addr &optional by-air)
  "Handle the delivery of the parcel with address ADDR.
If it is a story parcel and its step is the current one, return the recipient's answer (a line for the
players; with BY-AIR an airmail notice instead), queue the radio summary and let one of the two
recognise us at the window later; the story moves on. Otherwise return NIL."
  (let ((story (parcel-story addr)))
    (when story
      (destructuring-bind (key n step) story
        (when (= n (story-step key))                                   ; that very step, not a parcel left from a previous game
          (destructuring-bind (from dest sender words who answer news after) step
            (declare (ignore from dest sender words))
            (setf *story-transit* (remove-if (lambda (tr) (eq (second tr) key)) *story-transit*)
                  *story-at* (cons (cons key (1+ n)) (remove key *story-at* :key #'car))
                  *story-after* (append *story-after* (list after))
                  *news* (last (append *news* (list news)) 6))
            (save-stories)
            (if by-air
                (format nil "Авиапочта: ~a получил посылку" who)
                (format nil "~a: «~a»" who answer))))))))

(defun story-regular (town)
  "Return the line of the acquaintance at the window in town TOWN, as 'who: words'.
It is said once, when there is something to ask about; otherwise return NIL."
  (let ((line (find town *story-after* :key #'first)))
    (when line
      (setf *story-after* (remove line *story-after*))
      (save-stories)
      (format nil "~a: «~a»" (second line) (third line)))))

(defun story-news ()
  "Return the next radio summary of our doings, or NIL."
  (when *news* (prog1 (pop *news*) (save-stories))))

(defun say-later (text &optional (after 3.2))
  "Say TEXT to everyone a little later, after AFTER seconds: once they have read the line about money."
  (setf *story-late* (cons (+ *time* after) text)))

(defun step-stories ()
  (when (and *story-late* (> *time* (car *story-late*)))
    (say-all (cdr *story-late*))
    (setf *story-late* nil)))

(defun save-stories () (unless (net-guest-p) (put-save :stories (list *story-at* *story-transit* *story-after* *news*))))
(defun load-stories ()
  (let ((f (get-save :stories)))
    (when (and (listp f) (= (length f) 4) (every #'listp f))
      (setf *story-at* (remove-if-not (lambda (c) (and (consp c) (assoc (car c) *stories*) (integerp (cdr c)))) (first f))
            *story-transit* (remove-if-not (lambda (tr) (and (listp tr) (= (length tr) 3) (story-record (second tr) (third tr)))) (second f))
            *story-after* (remove-if-not (lambda (a) (and (listp a) (= (length a) 3) (integerp (first a)))) (third f))
            *news* (remove-if-not #'stringp (fourth f))))))
(defstate :stories
  :clear (lambda () (setf *story-at* '() *story-transit* '() *story-after* '() *news* '() *story-late* nil))
  :load #'load-stories :save #'save-stories)
