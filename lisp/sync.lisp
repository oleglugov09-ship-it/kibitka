;;;; One wavelength: everything that concerns the world and the wagon is the same for all players.
;;;;  • WORLD. Several times a second the driver sends the guests a WORLD-STATE block: road events, papers,
;;;;    booths, finds, gnomes, the route. A guest computes none of this itself: APPLY-WORLD.
;;;;  • ACTIONS. A guest asks the driver for any shared action (pick up a bottle, take a gnome, milk the goat,
;;;;    pay a fine): ASK-HOST. The driver runs the very same function 'on the guest's behalf' (RUN-AS: with
;;;;    the guest's paws and with the reply going to the guest), so actions have no separate 'guest' code.
;;;;    An action is declared with DEFSHARED (interact.lisp); nothing else needs to be registered.
;;;;  • WORDS. SAY-ALL is a message to everyone in the wagon. A plain SAY inside an action done for a guest
;;;;    goes to that guest.
;;;;  • RECONCILIATION. WORLD-DIGEST must match between the driver and a guest; tools/smoke.py pair checks it.
;;;; The wheel stays with the driver: the route and the new game.
;;;;
;;;; Shared events for playing as two or more. The train, cars, the plane, bird flocks, the mosquito: the
;;;; driver computes all of it and several times a second sends it to the guests along with the switches
;;;; (HOST-SWITCHES); a guest starts none of it itself, it adjusts its own to what was sent (APPLY-EVENTS).
;;;; Between packets everyone moves their own things, so there are no jerks. Passers-by in towns walk by the
;;;; shared clock of the day cycle.

(in-package #:kiosk-trip)

(defun f1 (x) (float x 1.0))

(defun world-events ()
  "Return what is happening in the driver's world right now, in a compact form to send to the guests."
  (list (loop for c in *cars* collect (list (getf c :n) (f1 (getf c :d)) (f1 (getf c :v)) (getf c :dir) (getf c :kind)))
        (loop for tr in *trains*
              collect (list (position (first tr) *tracks* :test #'equal) (f1 (second tr)) (f1 (fourth tr))))
        (and *plane* (cons (f1 (- *time* (second *plane*))) (mapcar #'f1 (append (third *plane*) (fourth *plane*)))))
        (loop for f in *flocks*
              collect (let ((p (node-3d-get-position (first f))))
                        (list (sixth f) (f1 (- *time* (third f))) (f1 (vector3-x p)) (f1 (vector3-y p)) (f1 (vector3-z p))
                              (f1 (fourth f)) (f1 (fifth f)))))
        (and *mosquito* t)
        (and *sleep* t)
        *dir*
        *gnomes*))

(defvar *sleep-seen* nil "True once the guest already knows that the night is being fast-forwarded.")

(defun apply-events (ev)
  "Bring the guest's own events in line with EV, the events sent by the driver."
  (when (and (listp ev) (= (length ev) 8))                    ; EV is the first part of WORLD-STATE
    (destructuring-bind (cars trains plane flocks mosquito sleeping dir gnomes) ev
      (when (and (integerp gnomes) (<= 0 gnomes (1- (ash 1 18))) (/= gnomes *gnomes*))       ; the driver found a gnome
        (setf *gnomes* gnomes) (show-trophies))
      (when (and (member dir '(1 -1)) (/= dir *dir*)) (setf *dir* dir) (swap-sides))      ; the driver turned around
      (when (listp cars)
        (setf *cars* (remove-if (lambda (c) (unless (find (getf c :n) cars :key (lambda (e) (and (listp e) (first e))))
                                               (node-queue-free (getf c :node)) t))
                                *cars*))
        (dolist (e cars)
          (when (fits e '(integer real real (:member 1 -1) (:member nil :fire :ambulance :police :runaway)))
            (let ((c (find (first e) *cars* :key (lambda (c) (getf c :n)))))
              (cond (c (when (> (abs (- (getf c :d) (second e))) 6.0) (setf (getf c :d) (second e))))   ; drifted apart: correct it
                    (t (multiple-value-bind (node blink) (make-car (first e) (fifth e))
                         (push (list :node node :d (second e) :v (third e) :dir (fourth e) :blink blink
                                     :n (first e) :kind (fifth e) :out nil :x nil)
                               *cars*))))))))
      (when (listp trains)
        (dolist (e trains)
          (when (and (fits e '(integer real real)) (nth (first e) *tracks*))
            (let* ((track (nth (first e) *tracks*)) (tr (assoc track *trains* :test #'equal)))
              (unless tr (setf tr (list track (second e) (make-train track) (third e))) (push tr *trains*))
              (when (> (abs (- (second tr) (second e))) 12.0) (setf (second tr) (second e)))
              (setf (fourth tr) (third e))))))
      (when (and plane (null *plane*) (listp plane) (= (length plane) 7) (every #'realp plane))
        (let ((g (group *world*)))
          (biplane g)
          (setf *plane* (list g (- *time* (first plane)) (subseq plane 1 4) (subseq plane 4 7)))))
      (when (listp flocks)
        (dolist (e flocks)
          (when (and (fits e '(integer real real real real real real))
                     (not (find (first e) *flocks* :key #'sixth)))
            (destructuring-bind (n age x y z vx vz) e
              (push (make-flock n (list x y z) age vx vz) *flocks*)))))
      (cond ((and mosquito (not *mosquito*)) (start-mosquito))
            ((and (not mosquito) *mosquito*) (end-mosquito nil)))
      (cond ((and sleeping (not *sleep-seen*)) (setf *sleep-seen* t) (say "Все уснули — ночь летит"))
            ((not sleeping) (setf *sleep-seen* nil))))))

(defun all-abed-p ()
  "Return true when everyone in the wagon is lying or sitting.
The driver is on the bunk, the guests are on any seat (their eyes are lower than when standing)."
  (every (lambda (p) (or (getf p :wait) (not (getf p :placed)) (< (or (getf p :eye) *eye*) (- *eye* .1)) (look-lying-p (getf p :look 0)))) *net-peers*))

;;; ------------------------------------------------------------------ words

(defun clean-text (text) (remove-if (lambda (c) (find c "\"\\")) text))      ; the message parser will not let a quote or a backslash through

(defun say-all (text)
  "Say TEXT to everyone in the wagon.
The driver calls it; if a guest was performing the action, the guest's name is prepended."
  (let ((text (if *actor* (format nil "~a: ~a" (getf *actor* :name) text) text)))
    (let ((*actor* nil)) (say text))
    (unless (net-guest-p)
      (dolist (p *net-peers*) (when (getf p :ok) (peer-send p (list :t (clean-text text))))))))

;;; ------------------------------------------------------------------ actions

(defvar *remote-wait* nil "True on a guest while the driver is still busy with our action (fishing, the mine): the paws are left alone.")
(defvar *pending* nil "True when the action done for a guest is not finished: we wait (START-WAIT) and the reply will come again.")

(defun ask-host (key &rest args)
  "Ask the driver, as a guest, to perform the shared action KEY with ARGS.
The request carries what we hold in our paws: the action may take it or hand something over. Return T."
  (if *remote-wait*
      (say "Погоди, ещё не закончил")
      (guest-send (list :a :do key args (car *carry*) (cdr *carry*) *carry-props*)))
  t)

(defun carry-ok-p (kind addr &optional props)
  (and (or (null kind) (good-p kind) (member kind '(:box :crate))) (or (null addr) (integerp addr) (keywordp addr))
       (props-ok-p props)))

(defun run-as (p carry thunk &optional props)
  "Run THUNK as the driver on behalf of guest P, with the guest's paws holding CARRY with PROPS.
Reply to the guest with what is now in its paws and what was said to it."
  (when (member p *net-peers*)
    (let ((*actor* p) (*carry* (and (car carry) carry)) (*carry-from* nil) (*carry-props* props) (*said* '()) (*pending* nil))   ; the guest's paws, not ours
      (funcall thunk)
      (peer-send p (list :r (car *carry*) (cdr *carry*) (mapcar #'clean-text (reverse *said*)) *pending* (and *carry* *carry-props*))))))

(defun host-do (p request)
  "Handle, as the driver, the REQUEST of guest P for an action: (key args kind-in-paws address properties)."
  (when (and (listp request) (= (length request) 5))
    (destructuring-bind (key args kind addr props) request
      (let ((op (assoc key *shared-ops*)))
        (when (and op (listp args) (<= (length args) 3) (every (lambda (a) (or (integerp a) (keywordp a))) args)
                   (carry-ok-p kind addr props))
          (run-as p (cons kind addr) (lambda () (apply (cdr op) args)) props))))))

(defun guest-done (reply)
  "Handle, as a guest, the REPLY to our finished action: (kind address said still-waiting properties).
The same thing still in the paws keeps the properties we know: a long job answers without them."
  (when (and (listp reply) (= (length reply) 5))
    (destructuring-bind (kind addr said pending props) reply
      (when (and (carry-ok-p kind addr props) (listp said) (every #'stringp said))
        (unless (and *carry* (eq kind (car *carry*)) (eql addr (cdr *carry*)) (null props)) (setf *carry-props* props))
        (setf *carry* (and kind (cons kind addr)) *carry-from* nil *remote-wait* (and pending t))
        (show-carry)
        (dolist (text said) (say text))))))

;;; ------------------------------------------------------------------ world

(defun world-slow ()
  "Return the part of the world that changes rarely: papers, booths, finds, the route."
  (list (list *licenses* *fines* (and *quest* (list (car *quest*) (cdr *quest*))) (first *check*))
        (list (and *stamp* t) (and *badge* t) (loop for (i . day) in *gimmick-spent* collect (list i day)) (and *lid* t))
        (list *bottles* *papers* (and *spyglass* t)
              (remove-if (lambda (id) (> (abs (- (first id) *dist*)) 200.0)) *picked*))      ; picked-up things: only the nearby ones
        (and *auto* (list (f1 (car *auto*)) (cdr *auto*) *way*))))

(defun world-state ()
  "Return everything shared that the driver knows as one block for the guests.
It is the road events followed by the rarely changing part."
  (cons (world-events) (world-slow)))

(defvar *world-sent* (make-hash-table) "The table from a guest id to (rare world part . time sent): it is not resent until it changes.")

(defun world-for (p)
  "Return the world block for guest P.
Events travel every time; the rare part travels when it has changed and, to be safe, once every two
seconds (a message may have been lost); otherwise :SAME stands in for it."
  (let* ((slow (world-slow)) (sent (gethash (getf p :id) *world-sent*)))
    (cond ((and sent (equal (car sent) slow) (< (- *time* (cdr sent)) 2.0)) (list (world-events) :same))
          (t (setf (gethash (getf p :id) *world-sent*) (cons slow *time*))
             (cons (world-events) slow)))))

(defun apply-world (w)
  "Bring the guest's own state in line with the driver's world block W."
  (when (and (listp w) (= (length w) 2) (eq (second w) :same))       ; the rare part did not change
    (apply-events (first w)))
  (when (and (listp w) (= (length w) 5))
    (destructuring-bind (events papers prikol finds auto) w
      (apply-events events)
      (when (fits papers '(t t t t))
        (destructuring-bind (licenses fines quest check) papers
          (when (and (fits licenses '(:each keyword)) (fits fines '(:each integer)))
            (setf *licenses* licenses *fines* fines
                  *quest* (and (fits quest '(keyword integer))
                               (cons (first quest) (second quest))))
            (let ((phase (and (member check '(:stopping :waiting)) check)))
              (unless (eq phase (first *check*))                       ; the policeman at the window is there for everyone
                (setf *check* (and phase (list phase nil nil 0.0)))
                (show-officer (eq phase :waiting)))))))
      (when (fits prikol '(t t list t))
        (unless (eq (and (fourth prikol) t) (and *lid* t)) (setf *lid* (and (fourth prikol) t)) (show-lid))
        (setf *stamp* (and (first prikol) t) *badge* (and (second prikol) t)
              *gimmick-spent* (loop for e in (third prikol)
                                    when (fits e '(integer integer)) collect (cons (first e) (second e)))))
      (when (fits finds '(integer integer t list))
        (setf *bottles* (first finds) *papers* (second finds))
        (unless (eq (and (third finds) t) (and *spyglass* t)) (setf *spyglass* (and (third finds) t)) (show-spyglass))
        (dolist (id (fourth finds))
          (when (fits id '(integer keyword integer))
            (pushnew id *picked* :test #'equal))))
      (if (fits auto '(real string (:each (real real real))))
          (setf *auto* (cons (first auto) (second auto)) *way* (third auto))      ; where we are heading is visible on the guest's map too
          (setf *auto* nil *way* '())))))

(defun world-digest ()
  "Return the shared state as one list.
It must match between the driver and a guest (tools/smoke.py compares it)."
  (list *money* (round (* 100 *fuel*)) *planks* *licenses* *fines* *quest* (first *check*) (and *stamp* t) (and *badge* t)
        *bottles* *papers* (and *spyglass* t) *gnomes* *pennants* *dir* (and *auto* (round (car *auto*))) (length *way*)
        *season* *weather* (length *items*)))

;;; ------------------------------------------------------------------ live patching for everyone
;;; A mode for developing with friends: the driver sends Lisp text, every guest evaluates it (EVAL works in
;;; the browser too, there it goes through the built-in interpreter) and answers the driver with the result
;;; or the error.
;;; From the REPL: (live:everyone '(setf kiosk-trip::*tank-price* 64)); the guests' answers: (live:answers).
;;; A patch lives until the game restarts; if we like it, we put it into the sources. Latecomers do not get it.
;;; The text travels as a hex string: messages may not contain quotes or backslashes.

(defvar *answers* '() "On the driver, the guests' answers to the last patch, as (name . text) pairs.")

(defun text-hex (text)
  (with-output-to-string (out)
    (loop for b across (ext:string-to-octets text :external-format :utf-8) do (format out "~(~2,'0x~)" b))))

(defun hex-text (hex)
  "Return the text decoded from the hex string HEX, or NIL if HEX is not one."
  (when (and (stringp hex) (evenp (length hex)) (every (lambda (c) (digit-char-p c 16)) hex))
    (ext:octets-to-string (coerce (loop for i from 0 below (length hex) by 2
                                        collect (parse-integer hex :start i :end (+ i 2) :radix 16))
                                  '(vector (unsigned-byte 8)))
                          :external-format :utf-8)))

(defun run-text (text)
  "Evaluate the forms in TEXT one after another (in the game package).
Return a string: the value of the last form, or the error."
  (handler-case
      (let ((*package* (find-package '#:kiosk-trip)) (value nil) (start 0))
        (loop (multiple-value-bind (form next) (read-from-string text nil :eof :start start)
                (when (eq form :eof) (return))
                (setf value (eval form) start next)))
        (let ((shown (format nil "=> ~s" value))) (subseq shown 0 (min 400 (length shown)))))
    (error (c) (let ((shown (format nil "ошибка: ~a" c))) (subseq shown 0 (min 400 (length shown)))))))

(defun everyone (text)
  "Evaluate TEXT here and on all guests.
Return our own result; the guests answer later (*ANSWERS*)."
  (let ((hex (text-hex text)))
    (cond ((> (length hex) 18000) "слишком длинно для одного сообщения: раздели на части")
          (t (setf *answers* '())
             (unless (net-guest-p)
               (dolist (p *net-peers*) (when (getf p :ok) (peer-send p (list :c hex)))))
             (run-text text)))))

(defun guest-run (hex)
  "Evaluate, as a guest, the text in HEX sent by the driver and answer with what came out."
  (let ((text (hex-text hex)))
    (when text (guest-send (list :a :ran (text-hex (run-text text)))))))

(defun host-ran (p hex)
  "Record, as the driver, the answer HEX of guest P to a patch."
  (let ((text (hex-text hex)))
    (when text
      (push (cons (getf p :name) text) *answers*)
      (format t "~&~a: ~a~%" (getf p :name) text)
      (say (format nil "~a: ~a" (getf p :name) (subseq text 0 (min 80 (length text))))))))
