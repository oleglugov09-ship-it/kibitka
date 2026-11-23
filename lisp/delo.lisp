;;;; Shared actions and boundaries. Only what every file needs at once and that does not require the engine:
;;;; who is acting right now (*ACTOR*), the declaration of a shared action (DEFSHARED) and error logging at
;;;; boundaries (NOTING). How actions travel over the network is in sync.lisp.

(in-package #:kiosk-trip)

;;; Language. All game text is written in Russian right in the code; English is a table 'Russian string →
;;; English string' (en.lisp, built by tools/make-en.py). Translation happens in the two places the text
;;; passes through anyway: FORMAT (the template and string arguments) and setting a node's 'text' property
;;; (OBJECT-SET, shapes.lisp). So the rest of the code does not need to know about language.

(defvar *dev-flags* '() "Switches for hunting a bug, from the page address: strings such as nostart or nopond.")
(defvar *dev* nil "Dev polygon flag: when true, all booths stand in a row at the start of the road (dev.lisp).")
(defvar *lang* :ru "The game language: :ru or :en.")
(defvar *en* (make-hash-table :test #'equal) "The table mapping a Russian string to its English string.")

(defun tr (text &optional context)
  "Return TEXT in the game language.
Anything missing from the table (and any non-string) is returned as is. The same words may need another translation
in another place: with CONTEXT, a string, the table is first asked for the key \"CONTEXT|TEXT\"."
  (if (and (eq *lang* :en) (stringp text))
      (or (and context (gethash (concatenate 'string context "|" text) *en*)) (gethash text *en*) text)
      text))

;;; EXT:OCTETS-TO-STRING of this ECL (26.5.5) must not be called: on 4192 characters or more (the size of
;;; its buffer) it adds a position where it means a count, takes its buffer for full for ever, and grows it
;;; until the memory is gone. The game froze on it wherever a long text came in as bytes: a save file read
;;; back, a message from another player. READ-SEQUENCE on the same bytes counts correctly.

(defun utf8-text (octets)
  "Return the string that the UTF-8 bytes OCTETS, a vector, spell; of any length."
  (let* ((octets (coerce octets '(simple-array (unsigned-byte 8) (*))))
         (text (make-string (length octets)))                             ; there are never more characters than bytes
         (count (read-sequence text (ext:make-sequence-input-stream octets :external-format :utf-8))))
    (let ((text (if (= count (length text)) text (subseq text 0 count))))
      (if (every (lambda (c) (typep c 'base-char)) text) (coerce text 'simple-base-string) text))))   ; as the old one did

(defun name-number (symbol range)
  "Return a number below RANGE that depends on the name of SYMBOL alone: the same on every machine, in every
build, wherever the symbol stands in any list. Things that travel or are saved name a good or a story by it."
  (let ((h 2166136261))
    (loop for c across (symbol-name symbol)
          do (setf h (logand (* (logxor h (char-code c)) 16777619) #xffffffff)))
    (mod h range)))

(defun fits (x spec)
  "Return true if X has the shape SPEC; use it on data from the network and from save files.
SPEC is T for anything; a type name (INTEGER, REAL, STRING, KEYWORD, LIST, CONS); (:each S) for a
list whose every element fits S; (:member A B ...) for one of the given values; (:or S ...); or a
list of specs for a list of exactly that many elements, where &OPTIONAL makes the rest optional and
&REST S lets any number of elements fitting S follow."
  (cond ((eq spec t) t)
        ((symbolp spec) (typep x spec))
        ((eq (first spec) :each) (and (listp x) (ignore-errors (list-length x)) (every (lambda (e) (fits e (second spec))) x)))
        ((eq (first spec) :member) (and (member x (rest spec)) t))
        ((eq (first spec) :or) (some (lambda (s) (fits x s)) (rest spec)))
        (t (and (listp x) (ignore-errors (list-length x))
                (loop with optional = nil
                      for (s . more) on spec
                      do (cond ((eq s '&optional) (setf optional t))
                               ((eq s '&rest) (return (every (lambda (e) (fits e (first more))) x)))
                               ((null x) (return optional))
                               ((not (fits (pop x) s)) (return nil)))
                      finally (return (null x)))))))

(defvar *lines* '()
  "What the named characters say: a plist of key and line. A line may be a FORMAT template.")

(defun line (key)
  "Return the line of a named character for KEY, or an empty string when no mod provides it."
  (or (getf *lines* key) ""))

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

;;; Money. The game counts in one whole unit, and the unit is *KOPECKS* kopecks: prices read as in a Soviet
;;; price list (milk 25 kop., a tank of petrol 2 rub. 40 kop.). Every sum shown to the player goes through
;;; RUB, the format directive ~/kiosk-trip::rub/, so the tables and the arithmetic stay in whole units.

(defparameter *kopecks* 5 "The number of kopecks in one unit of the game's money.")

(defun money-text (units)
  "Return the sum of UNITS as text: rubles and kopecks, in the game language."
  (multiple-value-bind (rub kop) (floor (round (* units *kopecks*)) 100)
    (let ((en (eq *lang* :en)))
      (cond ((zerop kop) (cl:format nil (if en "~d rub." "~d р.") rub))
            ((zerop rub) (cl:format nil (if en "~d kop." "~d коп.") kop))
            (t (cl:format nil (if en "~d rub. ~d kop." "~d р. ~d коп.") rub kop))))))

(defvar *text-values* '()
  "The numbers a written text may name: entries of (name . function). A card or a note says {name}, and
FILL-TEXT puts there what the function returns now, so the text follows the parameter and not the other way.")

(defun fill-text (text)
  "Return TEXT in the game language with every {name} replaced by its value from *TEXT-VALUES*; an unknown name stays."
  (let ((text (tr text)))
    (if (not (stringp text))
        text
        (with-output-to-string (out)
          (loop with at = 0
                do (let* ((open (position #\{ text :start at)) (close (and open (position #\} text :start open)))
                          (value (and close (cdr (assoc (subseq text (1+ open) close) *text-values* :test #'string=)))))
                     (cond ((null close) (write-string text out :start at) (return))
                           (value (write-string text out :start at :end open) (princ (funcall value) out) (setf at (1+ close)))
                           (t (write-string text out :start at :end (1+ close)) (setf at (1+ close))))))))))

(defun rub (stream units &optional colon at)
  "Print the sum of UNITS as rubles and kopecks: the format directive ~/kiosk-trip::rub/."
  (declare (ignore colon at))
  (write-string (if (realp units) (money-text units) (princ-to-string units)) stream))

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
        do (when clear (funcall clear))
           (when load (noting :load-world (funcall load)))                 ; one damaged section must not stop the launch
           (when show (funcall show))))

(defun reset-world ()
  "Start a new game: reset every part to its initial state and write it to the save.
Collections (KEEP) are left alone."
  (loop for (nil clear nil save show keep) in *state-parts*
        do (unless keep (when clear (funcall clear)) (when save (funcall save)) (when show (funcall show)))))

(defmacro stages (&body forms)
  "Run FORMS in order, timing each one; an error in one is logged and the next one runs.
Print one line to the log: which startup stages took more than 30 ms. It shows what exactly slows the
launch down, in the browser too."
  (let ((log (gensym "LOG")) (t0 (gensym "T0")))
    `(let ((,log '()))
       ,@(loop for form in forms
               collect `(let ((,t0 (get-internal-real-time)))
                          ;; one stage that fails must not stop the launch: in the browser an error here used to
                          ;; end in a prompt for the debugger's input, and what came after (the save!) never ran
                          (noting ,(if (consp form) (cl:format nil "startup: ~(~a~)" (first form)) "startup") ,form)
                          (push (cons ,(if (consp form) (string-downcase (princ-to-string (first form))) "?")
                                      (round (* 1000 (- (get-internal-real-time) ,t0)) internal-time-units-per-second))
                                ,log)))
       (format t "~&Старт, мс: ~{~a~^ · ~}~%"
               (loop for (name . ms) in (sort (remove-if (lambda (e) (< (cdr e) 30)) ,log) #'> :key #'cdr)
                     collect (format nil "~a ~d" name ms))))))

;;; Common things of the wagon. Whatever several cats share inside it (the radio, the dice, the kettle, the
;;; frying pan, the fridge door) is declared once with DEFCOMMON: how to read its value, what shape the value
;;; has and what to do when another player's value arrives. The values travel together with the switches
;;; (net.lisp), in the order of declaration.
;;;
;;; Every thing has a clock: a number that grows by one each time its value changes here. A value that arrives
;;; is taken only if its clock is ahead of ours; with equal clocks the driver's value stands. So a message that
;;; was already on its way when somebody else changed the thing (a guest's whole state is sent on any edit of
;;; theirs) cannot put the old value back: its clock is behind.

(defvar *commons* '() "The common things, in the order they were declared: entries of (name spec driver-owned value-function accept-function).")
(defvar *common-clocks* (make-hash-table) "The table from the name of a common thing to (clock . value as last seen here).")

(defmacro defcommon (name &key value (spec t) accept driver)
  "Declare the common thing NAME.
VALUE is a form giving what we send: something made of numbers, keywords, strings and lists of them.
SPEC is its shape for FITS; a value of another shape is dropped. ACCEPT is a function of one argument,
called with a value that arrived: it compares, stores, shows and, where the thing is saved, saves. With
DRIVER the thing is the driver's to decide: what a guest sends is ignored, what the driver sends is taken."
  `(let ((entry (list ,name ,spec ,driver (lambda () ,value) ,accept)) (old (assoc ,name *commons*)))
     (if old (setf (cdr old) (cdr entry)) (setf *commons* (append *commons* (list entry))))
     ,name))

(defun reset-commons ()
  "Forget the clocks: called when we join somebody's wagon or come back to our own, so that what is there is taken as it is."
  (clrhash *common-clocks*))

(defun common-clock (name value)
  "Return the clock cell of the common thing NAME, first noticing whether its VALUE has changed here."
  (let ((cell (gethash name *common-clocks*)))
    (cond ((null cell) (setf (gethash name *common-clocks*) (cons 0 value)))
          ((equal value (cdr cell)) cell)
          (t (incf (car cell)) (setf (cdr cell) value) cell))))

(defun common-state ()
  "Return the common things as (clock value) entries, in the order of declaration."
  (loop for (name nil driver value) in *commons*
        collect (let ((cell (gethash name *common-clocks*)))
                  (if (and driver cell (net-guest-p))
                      (list (car cell) (cdr cell))                            ; the driver's thing: a guest sends back what it was given
                      (let ((now (funcall value))) (list (car (common-clock name now)) now))))))

(defun accept-common (entries &optional driver-side)
  "Take in the common things ENTRIES sent by another player: (clock value) each.
DRIVER-SIDE is true when we are the driver receiving from a guest."
  (when (and (listp entries) (ignore-errors (list-length entries)) (= (length entries) (length *commons*)))
    (loop for (name spec driver value accept) in *commons*
          for entry in entries
          do (when (fits entry (list 'integer spec))
               (destructuring-bind (clock v) entry
                 (let ((cell (common-clock name (funcall value))))
                   (when (cond (driver (not driver-side))                                  ; the driver's thing: guests take it as it is
                               ((> clock (car cell)) t)
                               ((= clock (car cell)) (and (not driver-side) (not (equal v (cdr cell)))))
                               (t nil))                                                    ; behind ours: an old message
                     (noting name (funcall accept v))
                     (setf (car cell) (max clock (car cell)) (cdr cell) (funcall value)))))))))
