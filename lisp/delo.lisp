;;;; Shared actions and boundaries. Only what every file needs at once and that does not require the engine:
;;;; who is acting right now (*ACTOR*), the declaration of a shared action (DEFSHARED) and error logging at
;;;; boundaries (NOTING). How actions travel over the network is in sync.lisp.

(in-package #:kiosk-trip)

;;; Language. All game text is written in Russian right in the code; English is a table 'Russian string →
;;; English string' (en.lisp, built by tools/make-en.py). Translation happens in the two places the text
;;; passes through anyway: FORMAT (the template and string arguments) and setting a node's 'text' property
;;; (OBJECT-SET, shapes.lisp). So the rest of the code does not need to know about language.

(defvar *dev* nil "Dev polygon flag: when true, all booths stand in a row at the start of the road (dev.lisp).")
(defvar *lang* :ru "The game language: :ru or :en.")
(defvar *en* (make-hash-table :test #'equal) "The table mapping a Russian string to its English string.")

(defun tr (text)
  "Return TEXT in the game language.
Anything missing from the table (and any non-string) is returned as is."
  (if (and (eq *lang* :en) (stringp text)) (or (gethash text *en*) text) text))

(defun translate (&rest pairs)
  "Add English text: PAIRS alternate a Russian string from the code and its English translation.
Return the number of pairs. Mods call this for their own strings."
  (loop for (ru en) on pairs by #'cddr do (setf (gethash ru *en*) en))
  (floor (length pairs) 2))

(defun tr-arg (arg)
  "Translate the template argument ARG.
A string is translated; in a list (an enumeration of goods or towns) every element is."
  (cond ((stringp arg) (tr arg))
        ((and (consp arg) (ignore-errors (list-length arg))) (mapcar #'tr-arg arg))
        (t arg)))

(defun format (destination control &rest args)
  (if (and (eq *lang* :en) (stringp control))
      (apply #'cl:format destination (tr control) (mapcar #'tr-arg args))
      (apply #'cl:format destination control args)))

(defvar *untranslated* '() "The strings that had no translation and were shown in Russian in English mode (inspect from the REPL).")

(defvar *actor* nil "The guest on whose behalf the driver is performing a shared action right now (sync.lisp).
NIL means we are acting for ourselves.")
(defvar *said* '() "The lines said to the guest during this action; they are sent to the guest in the reply.")

(defvar *shared-ops* '() "The shared actions as (key . function-name) pairs, collected by DEFSHARED.")

(defmacro defshared (name key (&rest args) &body body)
  "Define the shared action NAME.
The driver runs BODY; a guest asks the driver (ASK-HOST, sync.lisp), who runs the same BODY on the guest's
behalf: with the guest's paws and with the reply going to the guest. KEY is the action's name in the
message: a keyword of at most 23 characters. Over the network ARGS can only be integers and keywords."
  (multiple-value-bind (doc body) (if (stringp (first body)) (values (list (first body)) (rest body)) (values nil body))
    `(progn (defun ,name ,args ,@doc (if (net-guest-p) (ask-host ,key ,@args) (progn ,@body)))
            (setf *shared-ops* (cons (cons ,key ',name) (remove ,key *shared-ops* :key #'car)))
            ',name)))

(defmacro noting (where &body body)
  "Run BODY, logging any error (NOTE-ERROR) instead of swallowing it silently, and carry on.
WHERE names the place in the log. Use it at boundaries where foreign data must not crash the game:
network messages, save files. Return NIL after an error."
  `(handler-case (progn ,@body) (error (c) (note-error c ,where) nil)))

;;; Parts of the world state. Each subsystem declares its part once (how to clear it, read it from the save,
;;; write it and show it), and 'new game' and 'came back from someone else's room to my own' walk over all
;;; the parts at once (RESET-WORLD, LOAD-WORLD). A subsystem can no longer forget to reset its own state.

(defvar *state-parts* '() "The state parts as (name clear load save show keep-in-new-game) lists.")

(defmacro defstate (name &key clear load save show keep)
  "Declare the world state part NAME.
CLEAR resets it to initial values (without saving), LOAD reads it from the save, SAVE writes it to the
save, SHOW refreshes what is visible; with KEEP a new game leaves this part alone (a collection)."
  `(setf *state-parts* (append (remove ',name *state-parts* :key #'first)
                               (list (list ',name ,clear ,load ,save ,show ,keep)))))

(defun load-world ()
  "Load our own world state from the save.
Each part is cleared first and then filled with what was saved, so nothing foreign (left after playing as
a guest) remains."
  (loop for (nil clear load nil show) in *state-parts*
        do (when clear (funcall clear)) (when load (funcall load)) (when show (funcall show))))

(defun reset-world ()
  "Start a new game: reset every part to its initial state and write it to the save.
Collections (KEEP) are left alone."
  (loop for (nil clear nil save show keep) in *state-parts*
        do (unless keep (when clear (funcall clear)) (when save (funcall save)) (when show (funcall show)))))

(defmacro stages (&body forms)
  "Run FORMS in order, timing each one.
Print one line to the log: which startup stages took more than 30 ms. It shows what exactly slows the
launch down, in the browser too."
  (let ((log (gensym "LOG")) (t0 (gensym "T0")))
    `(let ((,log '()))
       ,@(loop for form in forms
               collect `(let ((,t0 (get-internal-real-time)))
                          ,form
                          (push (cons ,(if (consp form) (string-downcase (princ-to-string (first form))) "?")
                                      (round (* 1000 (- (get-internal-real-time) ,t0)) internal-time-units-per-second))
                                ,log)))
       (format t "~&Старт, мс: ~{~a~^ · ~}~%"
               (loop for (name . ms) in (sort (remove-if (lambda (e) (< (cdr e) 30)) ,log) #'> :key #'cdr)
                     collect (format nil "~a ~d" name ms))))))
