;;;; Aktors: a piece of the world that keeps its own state and changes it only when told to.
;;;;
;;;; An aktor has a name (a keyword), a state (a plist with a fixed set of keys) and things it can be told
;;;; (DEFTELL). Telling it something is the only way its state changes:
;;;;
;;;;   (defaktor :kettle :state '(:at :table :tea 0) :spec '(...) :show (lambda () (show-kettle)))
;;;;   (deftell :kettle :pour () (when (plusp (st :tea)) (become :tea (1- (st :tea)))))
;;;;   (tell :kettle :pour)
;;;;
;;;;  • ONE OWNER. An aktor lives with the driver. A guest that tells it something asks the driver (TELL is a
;;;;    shared action, sync.lisp), who runs the same handler with the guest's number as WHO and the guest's paws.
;;;;  • A TURN IS WHOLE OR NOT AT ALL. A handler does not change the state, it answers with the new one. If it
;;;;    fails, or the new state has the wrong shape, the state is as it was.
;;;;  • NO POLLING. Every change passes through one place (AKTOR-BECOME): there the aktor is shown anew and its
;;;;    new state is sent to the guests, once. Nothing is compared every frame, and no clocks are needed to tell
;;;;    whose value is newer: there is one value, the driver's.
;;;;  • FROM THE REPL: (peek :kettle) reads the state, (poke :kettle :tea 4) sets a field as the driver - through
;;;;    the same one place, so everyone sees it. A plain SETF has nothing to reach.
;;;;
;;;; What is counted every frame (the path, the speed, how long the kettle has been heating) is not an aktor's
;;;; business: an aktor is for what changes when somebody does something.

(in-package #:kiosk-trip)

(defvar *aktors* '() "The aktors, in the order they were declared: entries of (name state version spec show handlers start).")
(define-fields aktor name state ver spec show handlers start)

(defun aktor (name) "Return the aktor called NAME." (or (assoc name *aktors*) (error "Нет актора ~s" name)))

(defmacro defaktor (name &key state spec show)
  "Declare the aktor NAME. STATE is its state at the start of a new game: a plist, whose keys are all the keys it
will ever have. SPEC is the shape of the state for FITS: a state of another shape is not taken, neither from a
handler nor from the network. SHOW is called after every change, to show the thing anew. Declared again while the
game runs, an aktor keeps the state it has."
  `(let ((old (assoc ,name *aktors*)))
     (if old
         (setf (aktor-spec old) ,spec (aktor-show old) ,show (aktor-start old) ,state)
         (setf *aktors* (append *aktors* (list (list ,name (copy-list ,state) 0 ,spec ,show '() ,state)))))
     ,name))

(defun state-with (state changes)
  "Return STATE, a plist, with CHANGES (key value ...) made in it. Only keys the state has can be changed."
  (let ((new (copy-list state)))
    (loop for (key value) on changes by #'cddr
          do (unless (loop for k in new by #'cddr thereis (eq k key)) (error "В состоянии нет поля ~s" key))
             (setf (getf new key) value))
    new))

(defmacro deftell (name message (&optional (arg 'arg)) &body body)
  "Declare what the aktor NAME does when told MESSAGE with ARG (an integer or a keyword; 0 when nothing is given).
In BODY, (ST key) reads the state, WHO is the number of the player who tells (0 is the driver) and
(BECOME key value ...) is the state with those fields changed. BODY answers with the new state, or with NIL
when nothing changes. What BODY says (SAY) goes to whoever told."
  `(progn
     (setf (getf (aktor-handlers (aktor ,name)) ,message)
           (lambda (state who ,arg)
             (declare (ignorable who ,arg))
             (flet ((st (key) (getf state key))
                    (become (&rest changes) (state-with state changes)))
               (declare (ignorable #'st #'become))
               ,@body)))
     ,message))

(defun aktor-announce (a peers)
  "Send the state of the aktor A to the guests PEERS."
  (dolist (p peers)
    (when (getf p :ok) (peer-send p (list :o (aktor-name a) (aktor-ver a) (aktor-state a))))))

(defun aktors-for (p)
  "Send every aktor's state to the guest P, who has just come in."
  (dolist (a *aktors*) (aktor-announce a (list p))))

(defun aktor-become (a new)
  "Make NEW the state of the aktor A, if it differs: check its shape, show the thing anew, tell the guests."
  (unless (equal new (aktor-state a))
    (unless (fits new (aktor-spec a)) (error "Актор ~s: состояние не той формы: ~s" (aktor-name a) new))
    (setf (aktor-state a) new)
    (incf (aktor-ver a))
    (when (aktor-show a) (funcall (aktor-show a)))
    (aktor-announce a *net-peers*)))

(defshared deliver :tell (name message arg)
  "Tell the aktor NAME the MESSAGE with ARG, as the driver does it: for ourselves, or for the guest who asked."
  (let* ((a (assoc name *aktors*)) (handler (and a (getf (aktor-handlers a) message))))
    (when handler
      (let ((new (funcall handler (aktor-state a) (if *actor* (getf *actor* :id) 0) arg)))
        (when new (aktor-become a new))))))

(defun tell (name message &optional (arg 0))
  "Tell the aktor NAME the MESSAGE, with ARG if it takes one."
  (deliver name message arg))

(defun aktor-accept (fields)
  "Take, as a guest, what the driver says an aktor's state is now: FIELDS is (name version state)."
  (when (fits fields '(keyword integer list))
    (destructuring-bind (name ver state) fields
      (let ((a (assoc name *aktors*)))
        (when (and a (fits state (aktor-spec a)))
          (setf (aktor-state a) state (aktor-ver a) ver)
          (when (aktor-show a) (funcall (aktor-show a))))))))

(defun peek (name &optional key)
  "Return the state of the aktor NAME, or its field KEY."
  (let ((state (aktor-state (aktor name)))) (if key (getf state key) state)))

(defun poke (name &rest changes)
  "Change fields of the aktor NAME by hand, as the driver: (poke :kettle :tea 4). Return the new state."
  (let ((a (aktor name)))
    (aktor-become a (state-with (aktor-state a) changes))
    (aktor-state a)))

(defun aktor-reset (name)
  "Put the aktor NAME back into the state it has at the start of a new game."
  (let ((a (aktor name))) (aktor-become a (copy-list (aktor-start a)))))
