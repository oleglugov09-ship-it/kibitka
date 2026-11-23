;;;; Papers. A folder hangs by the door in the wagon: driving licence, trading permit, licences, fines, quests.
;;;; From the very start you may drive and trade everything except coal and air cargo (cranberries from
;;;; Dalny): those need licences. Once in a while the police stop you on the road and you show the folder
;;;; (E on it); carrying restricted goods without a licence gets you a fine. Fines are paid at the police
;;;; station in Oktyabrsky. Licences are issued by the town council there too, for quests: take a quest
;;;; from the board, complete it, come back for the paper.
;;;; The driver keeps count of all this; guests see the same in the folder, but checks and fines apply to the whole wagon.

(in-package #:kiosk-trip)

(defvar *restricted* '() "The goods that need a licence, from mods: entries of (goods-kind licence display-name).")
(defvar *quests* '()
  "The town council quests: (KEY GRANTS TITLE TASK COUNT REWARD-NAME).")
(defun quest-key (q) "Return the key of the quest record Q." (first q))
(defun quest-grants (q) "Return the licence the quest record Q grants." (second q))
(defun quest-title (q) "Return the title of the quest record Q." (third q))
(defun quest-task (q) "Return the words of the task of the quest record Q." (fourth q))
(defun quest-need (q) "Return how many units the quest record Q asks for." (fifth q))
(defun quest-reward (q) "Return the name of the reward of the quest record Q." (sixth q))
(defun quest-now () "Return the record of the quest taken, or NIL." (and *quest* (assoc (car *quest*) *quests*)))

(defvar *licenses* '() "The licences obtained: :coal, :air, :honor.")
(defvar *fines* '() "The unpaid fines, in roubles.")
(defvar *quest* nil "The quest taken: (KEY . DONE).")
(defvar *check* nil "The police check: (:stopping AUTOPILOT TURNS), then (:waiting ... SECONDS-LEFT).")
(defvar *check-in* 420.0 "The seconds of driving left until the next check.")
(defvar *docs-open* nil)
(defvar *docs-panel* nil)
(defvar *officer* nil "The police officer at the window.")
(defparameter *folder-at* '(1.45 .78 -1.77) "The position of the folder, which hangs under the map to the left of the window.")

;;; The papers of the co-operative. The game starts as a plain game of its kind: parcels, the window, petrol,
;;; one purse. What is peculiar to this world comes later, a portion at a time, and each portion comes as a
;;; paper somebody hands us; it then lies in the folder under its own bookmark (guide.lisp), with the rule in
;;; a few words and the live numbers. The papers we hold are also the state of the game: no order - no crates
;;; of the plan; no account - everything is paid in cash; no note of hand - the man's debt keeps quiet.
;;; They are kept in *LICENSES*, which is saved and already travels to the guests.

(defparameter *folder-papers* '(:waybill :loan :order :account :iou :obhss :talons)
  "The papers of the co-operative, in the order of their bookmarks in the folder.")
(defvar *papers-new* '() "The papers that came and have not been opened yet: a white corner sticks out of the folder.")
(defvar *folder-corner* nil "The white corner of a new paper sticking out of the folder on the wall, a node.")
(defvar *first-crate* nil "True from the day the order came until the first crate is taken: a story's sender does not come before it.")

(defun paper-p (key)
  "Return true if we hold the paper KEY. On the dev polygon every paper is held."
  (and (or *dev* (member key *licenses*)) t))

(defun show-folder-corner ()
  (when *folder-corner* (object-set *folder-corner* "visible" (and *papers-new* t))))

(defun grant-paper (key &optional card line)
  "Hand the co-operative the paper KEY, once: the driver reads CARD over the screen, everyone on board hears LINE.
Return true if the paper is new."
  (unless (or (net-guest-p) (member key *licenses*))
    (push key *licenses*) (save-papers)
    (pushnew key *papers-new*) (show-folder-corner)
    (funcall 'note-paper key)                              ; a bookmark in the corner of the screen: where to look
    (log-event :paper :key key)
    (when (and card (boundp '*show*) (not (symbol-value '*show*)))
      (funcall 'open-show (list (format nil "~a~%~%~a" card (tr "Бумага легла в папку под картой: E по папке."))) (lambda ())))
    (when line (let ((*actor* nil)) (say-all line)))
    t))

(defun save-papers () (put-save :papers (list *licenses* *fines* *quest*)))
(defun load-papers ()
  (let ((f (get-save :papers)) (old t))                          ; OLD: the save knows nothing of the co-operative's papers
    (when (fits f '(list list t))
      (setf old (not (member :waybill (first f))))
      (setf *licenses* (remove-if-not #'keywordp (first f)) *fines* (remove-if-not #'integerp (second f))
            *quest* (and (consp (third f)) (keywordp (car (third f))) (integerp (cdr (third f))) (third f))))
    ;; A world from before the papers has no waybill in its save, or no papers saved at all. If the man's clock
    ;; already runs there (it used to start on the first day), that world has seen everything and holds every
    ;; paper. A world begun with the papers saves them, the waybill among them, the day it meets the man.
    (when old
      (setf *licenses* (union *licenses* (if *shark-day* *folder-papers* '(:waybill :loan)))))))
(defstate :papers
  :clear (lambda () (setf *licenses* (list :waybill :loan) *papers-new* '() *first-crate* nil       ; a new trip starts with two papers
                          *fines* '() *quest* nil *check* nil *check-in* 420.0)
           (show-folder-corner))
  :load #'load-papers :save #'save-papers :show (lambda () (show-officer nil)))

;;; ------------------------------------------------------------------ the folder

(defvar *doc-tex* (make-hash-table :test 'equal) "A table from document name to texture (drawn by tools/make-docs.py).")

(defun doc-tex (name)
  (or (gethash name *doc-tex*)
      (let ((path (cl:format nil "res://textures/docs~a/~a.png" (if (eq *lang* :en) "-en" "") name)))
        (when (picture-here-p path)
          (let ((img (gdext:make-object "Image")) (tx (gdext:make-object "ImageTexture")))
            (read-picture img path)
            (object-call tx "set_image" img)
            (gdext:setf-reference (gethash name *doc-tex*) tx)
            tx)))))

(defun doc-card (parent name x y w h)
  "Build a document on the desk under PARENT: picture NAME in the rectangle X Y W H.
Return the TextureRect."
  (let ((r (gdext:make-object "TextureRect")))
    (node-add-child parent r)
    (object-set r "texture" (doc-tex name))
    (object-set r "expand_mode" 1) (object-set r "stretch_mode" 0) (object-set r "texture_filter" 1)   ; the picture is drawn pixel for pixel: no smoothing
    (object-set r "position" (vector2 x y)) (object-set r "size" (vector2 w h))
    r))

(defun make-docs (root)
  "Build the desk with the papers under ROOT.
Each paper is its own card; a missing one is an empty slot with a dotted outline."
  (let ((layer (gdext:make-object "CanvasLayer")) (bg (gdext:make-object "Control")) (ink '(.2 .16 .1)))
    (object-set layer "layer" 6)
    (node-add-child root layer) (node-add-child layer bg)
    (doc-card bg "desk" 0 0 640 360)
    (doc-card bg "driver" 14 14 186 118)
    (doc-card bg "trade" 14 140 186 118)
    (let ((parts (list :layer layer
                       :name (hud-label bg "" 118 40 7 ink)
                       :stamp (doc-card bg "stamp" 36 262 136 93)
                       :coal (doc-card bg "coal" 214 14 136 180) :coal-slot (doc-card bg "slot" 214 14 136 180)
                       :air (doc-card bg "air" 358 14 136 180) :air-slot (doc-card bg "slot" 358 14 136 180)
                       :honor (doc-card bg "honor" 500 14 136 180) :honor-slot (doc-card bg "slot" 500 14 136 180)
                       :quest (doc-card bg "quest" 214 200 200 136)
                       :fine (doc-card bg "fine" 430 210 160 96))))
      (setf (getf parts :quest-text) (hud-label bg "" 232 240 7 ink)
            (getf parts :fine-text) (hud-label bg "" 482 240 9 '(.5 .1 .08))
            (getf parts :badge) (hud-label bg "" 430 318 7 '(.92 .88 .76)))
      (hud-label bg "Цифры — закладки · G — книжка · E или Esc — убрать" 414 344 7 '(.8 .76 .64))
      (setf *docs-panel* parts)
      (make-guide bg))
    (object-set layer "visible" nil)))

(defun open-docs ()
  "Lay out the papers: show those we have and fill in the name, the fines and the quest progress."
  (setf *docs-open* t)
  (log-event :docs)
  (flet ((show (key on) (object-set (getf *docs-panel* key) "visible" (and on t)))
         (text (key s) (object-set (getf *docs-panel* key) "text" s)))
    (text :name *net-name*)
    (show :stamp *stamp*)
    (dolist (k '((:coal :coal-slot :coal) (:air :air-slot :air) (:honor :honor-slot :honor)))
      (show (first k) (member (third k) *licenses*)) (show (second k) (not (member (third k) *licenses*))))
    (show :fine *fines*)
    (text :fine-text (if *fines* (format nil "~/kiosk-trip::rub/~:[~; (~d шт.)~]" (reduce #'+ *fines*) (> (length *fines*) 1) (length *fines*)) ""))
    (text :quest-text
          (if *quest*
              (destructuring-bind (key license title what need reward) (assoc (car *quest*) *quests*)
                (declare (ignore key license))
                (format nil "«~a»~%~a~%сделано: ~d из ~d~%награда: ~a" title what (min need (cdr *quest*)) need reward))
              (format nil "Заданий нет.~%Доска — у горсовета~%в Октябрьском.")))
    (text :badge (if *badge* "К папке приколот значок «Меткий стрелок»" "")))
  (object-set (getf *docs-panel* :layer) "visible" t)
  (funcall 'show-tabs)
  (funcall 'note-paper nil)
  (when *papers-new* (funcall 'open-paper (first *papers-new*))))             ; a new paper: straight to it

(defun close-docs ()
  (funcall 'folder-seen)                                   ; what was open has been read
  (setf *docs-open* nil)
  (object-set (getf *docs-panel* :layer) "visible" nil))

(defun docs-key (event)
  (when (and (object-call event "is_class" "InputEventKey") (object-get event "pressed") (not (object-get event "echo"))
             (or (guide-key (key-code event))                                         ; the guide booklet (guide.lisp)
                 (member (key-code event) '(69 4194305 73))))
    (unless *guide-open* (close-docs))))

(defun make-folder (c)
  (destructuring-bind (x y z) *folder-at*
    (let ((g (group c x (+ *floor* y) z)))
      (model g "folder")
      (setf *folder-corner* (shape g :box '(.03 .1 .12) "#f4f0e4" 0 .2 .04 :shadow nil :rot '(.25 0 0)))   ; a new paper sticks out
      (show-folder-corner))))

;;; ------------------------------------------------------------------ the police

(defun cargo-count (kind)
  "Return how many goods of KIND are in the wagon right now: on the shelves and in hand."
  (+ (count kind *items* :key (lambda (it) (spec-kind (car it)))) (if (and *carry* (eq (car *carry*) kind)) 1 0)))

(defun bulk-cargo ()
  "Return what we carry in bulk, more than *CARGO-FREE* pieces: rows of *RESTRICTED*.
A couple of crates is for personal use, but a loaded wagon is what the police look at: by this sign
they stop us and ask for a licence."
  (remove-if-not (lambda (r) (> (cargo-count (first r)) *cargo-free*)) *restricted*))

(defun violations ()
  "Return what we will be fined for.
The result is (NAME . AMOUNT) for each kind of goods carried in bulk without a licence."
  (append (loop for (nil license name) in (bulk-cargo)
                unless (member license *licenses*)
                  collect (cons (format nil "~a без лицензии" name) *fine-cargo*))
          (when (plusp *heat*)                                           ; resales of shop goods: speculation (plan.lisp)
            (list (cons (format nil "спекуляция, перепродаж: ~d" *heat*) (* *heat* *fine-resale*))))))

(defun show-officer (on)
  (when *officer* (node-queue-free *officer*) (setf *officer* nil))
  (when (and on *cart*)
    (let ((g (group *cart* 2.7 0 .2)))
      (node-3d-set-rotation g (v3 0 (- (/ pi 2)) 0))
      (node-3d-set-scale (first (make-cat g 2 24)) (v3 .8 .8 .8))             ; a grey cat in a leather jacket and a peaked cap
      (shape g :box '(.05 .5 .05) "#e8e0d0" .3 .9 .2 :rot '(0 0 -.5))             ; the baton
      (shape g :box '(.06 .16 .06) "#16161a" .42 1.08 .2 :rot '(0 0 -.5) :shadow nil)
      (setf *officer* g))))

(defun end-check (text)
  (let ((auto (second *check*)))
    (setf *check* nil *check-in* (+ *check-every* (random *check-every*)))
    (show-officer nil)
    (when auto (set-course (car auto) (cdr auto)))                      ; drive on to where we were heading (the route is planned anew: we may have turned around)
    (save-papers)
    (say-all text)))

(defshared show-papers :papers ()
  "Show the papers to the police officer.
If all is clean he wishes a good trip; if we carry restricted goods he writes a fine."
  (unless (and *check* (eq (first *check*) :waiting)) (return-from show-papers nil))
  (let ((bad (violations)) (owed (length *fines*)))
    (dolist (b bad) (push (cdr b) *fines*))
    (when (plusp *heat*) (setf *heat* 0) (add-deeds 6 -2) (save-plan))      ; fined for speculation: the count is cleared, the town council remembers
    (log-event :check :bad (length bad) :fines (length *fines*))
    (end-check (cond (bad (format nil "Проверка: ~{~a~^, ~}. Штраф ~/kiosk-trip::rub/ — оплатить в участке, Октябрьский"
                                  (mapcar #'car bad) (reduce #'+ (mapcar #'cdr bad))))
                     ((plusp owed) "Милиция: документы в порядке, но за вами неоплаченный штраф. Не тяните")
                     (t "Милиция: документы в порядке. Счастливого пути!")))))

(defun use-folder ()
  (if (and *check* (eq (first *check*) :waiting)) (show-papers) (open-docs)))        ; anyone may show the papers to the police

(defun step-check (delta)
  "Run the police check on the driver's side for DELTA seconds.
When we carry coal or air cargo in bulk, once in a while on the road, outside towns, the police stop us
and ask for a licence. Travelling light, we are never stopped."
  (unless (net-guest-p)
    (cond ((null *check*)
           (when (and *auto* (not *dev*) (> *speed* 4.0) (not (town-at *dist*)) (not (bare-at *dist*)) (not *meta*)
                      (or (bulk-cargo) (>= *heat* *heat-limit*))           ; those loaded with coal or air cargo, and those the inspectors have counted
                      (< (decf *check-in* delta) 0))
             (setf *check* (list :stopping *auto* *way*) *auto* nil)       ; autopilot is off: the wagon brakes by itself
             (say-all "Впереди милиция: машут жезлом. Останавливаемся")))
          ((eq (first *check*) :stopping)
           (when (< *speed* .2)
             (setf *check* (list :waiting (second *check*) (third *check*) *check-wait*))
             (show-officer t)
             (say-all (tr (line :sergeant-stop)))))
          ((eq (first *check*) :waiting)
           (when (< (decf (fourth *check*) delta) 0)
             (push *fine-ignore* *fines*)
             (end-check (format nil "Милиция не дождалась документов: штраф ~/kiosk-trip::rub/ за неподчинение" *fine-ignore*)))))))

;;; ------------------------------------------------------------------ the police station and the town council quests

(defshared pay-fines :fines ()
  (cond ((null *fines*) (say "Участок: штрафов за вами нет"))
        (t (let ((sum (reduce #'+ *fines*)))
             (when (pay sum)
               (setf *fines* '()) (save-papers) (log-event :fines-paid :sum sum :money *money*)
               (say-all (format nil "Штрафы оплачены: −~/kiosk-trip::rub/ Квитанция — в папке" sum)))))))

(defun quest-label ()
  (cond ((and *quest* (>= (cdr *quest*) (quest-need (quest-now))))
         (format nil "E — задание выполнено: получить ~a" (quest-reward (quest-now))))
        (*quest* (destructuring-bind (key license title what need reward) (assoc (car *quest*) *quests*)
                   (declare (ignore key license reward))
                   (format nil "Задание «~a»: ~a — ~d из ~d" title what (cdr *quest*) need)))
        (t (let ((next (find-if-not (lambda (q) (member (quest-grants q) *licenses*)) *quests*)))
             (if next (format nil "E — взять задание «~a»: ~a. Награда: ~a" (quest-title next) (quest-task next) (quest-reward next))
                 "Доска заданий пуста: все бумаги у тебя есть")))))

(defshared use-quests :quest ()
  (cond ((and *quest* (>= (cdr *quest*) (quest-need (quest-now))))
         (cond (*fines* (say "Горсовет: сначала оплатите штрафы в участке — он рядом"))
               (t (let ((q (quest-now)))
                    (push (quest-grants q) *licenses*) (setf *quest* nil)
                    (add-money (or (cdr (assoc (quest-grants q) *quest-money*)) 0))
                    (save-papers) (log-event :license :kind (quest-grants q))
                    (say-all (format nil (line :chairman-gives) (quest-reward q)))))))
        (*quest* (say (quest-label)))
        (t (let ((next (find-if-not (lambda (q) (member (quest-grants q) *licenses*)) *quests*)))
             (cond (next (setf *quest* (cons (quest-key next) 0)) (save-papers)
                         (say-all (format nil (line :chairman-asks) (quest-title next) (quest-task next))))
                   (t (say "Заданий больше нет")))))))

(defun note-quest (key &optional (n 1))
  "Count N units done towards the quest taken; KEY is :post, :clean or :album."
  (when (and *quest* (eq (car *quest*) key) (not (net-guest-p)))
    (let ((need (quest-need (assoc key *quests*))) (was (cdr *quest*)))
      (incf (cdr *quest*) n)
      (save-papers)
      (when (and (< was need) (>= (cdr *quest*) need))
        (say "Задание горсовета выполнено! Возвращайся в Октябрьский за бумагой")))))

(defun bumagi-usables ()
  (list (usable (first *folder-at*) (second *folder-at*) (third *folder-at*)
                (lambda () (if (and *check* (eq (first *check*) :waiting)) "E — показать документы милиции" "E — папка с документами"))
                #'use-folder)))

;;; ------------------------------------------------------------------ the booths of the papers

;; the town council: quests for licences
(defbooth 12 :label #'quest-label :use #'use-quests :shelf '("#a3262c" "#b8ac94" "#6a3a2e"))

;; the militia station: fines are paid here
(defbooth 13
  :shelf '("#2c4a7a" "#d8d0b8" "#2a2c30")
  :label (lambda () (if *fines* (format nil "E — оплатить штрафы: ~/kiosk-trip::rub/" (reduce #'+ *fines*)) "Участок: штрафов за вами нет"))
  :use #'pay-fines)
