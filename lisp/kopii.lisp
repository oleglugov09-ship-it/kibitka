;;;; Copies of the world by hand. In the pause menu (Esc) whoever owns the world, the driver or a solo
;;;; player, sees a panel at the left: «Make a copy» and the list of copies made. A click on a copy, and a
;;;; second click to confirm, puts it back: the world on the disk becomes the copy and the game reads it
;;;; anew. Before that the world as it is now is copied too, so a restore can be undone. The automatic
;;;; guards (save.lisp) save a file that does not read; these save a world that reads well but has gone
;;;; wrong, and the player decides which one is right.

(in-package #:kiosk-trip)

(defparameter *copy-slots* 8 "How many copies by hand are kept: a new one takes the place of the oldest.")
(defparameter *auto-slots* 3
  "How many automatic copies are kept, in slots after the hand-made ones: one a game day and one at launch,
so a world gone wrong loses at most a day, and the oldest automatic copy is three days back.")
(defvar *auto-copy-day* nil "The game day of the last automatic copy.")
(defvar *copy-ui* nil "The panel: (:layer :new button :rows (button ...)).")
(defvar *copy-armed* nil "The copy clicked once and waiting for the second click: (slot . time).")
(defvar *copy-list* '() "The copies on disk as (slot unix-time label), the newest first.")
(defvar *copies-read* nil "True once *COPY-LIST* has been read from the disk in this launch: after that it is kept by hand.")
(defparameter *copy-index* "user://copies.sexp" "The list of the copies, a small file: so that nobody has to read the copies to list them.")

(defun copy-path (slot) (cl:format nil "user://copy-~d.sexp" slot))

(defun copy-info (slot)
  "Return (slot unix-time label) for the copy in SLOT, or NIL if there is none or it does not read."
  (let ((f (load-form (copy-path slot))))
    (and (fits f '(t real string t)) (eq (first f) :copy) (list slot (second f) (third f)))))

(defun read-copies ()
  "Learn which copies there are, once a launch: from the index, or, where there is none yet, from the copies themselves.
A copy is a whole world; reading eleven of them to list them took its time in the middle of a frame."
  (unless *copies-read*
    (let ((index (load-form *copy-index*)))
      (setf *copy-list* (sort (if (fits index '(:each (integer real string)))
                                  (remove-if-not (lambda (c) (< -1 (first c) (+ *copy-slots* *auto-slots*))) index)
                                  (loop for i below (+ *copy-slots* *auto-slots*) for c = (copy-info i) when c collect c))
                              #'> :key #'second)
            *copies-read* t)
      (unless index (save-form *copy-index* *copy-list*)))))

(defun note-copy (slot time label)
  "Remember that SLOT now holds the copy made at TIME and called LABEL, in the list and in the index."
  (setf *copy-list* (sort (cons (list slot time label) (remove slot *copy-list* :key #'first)) #'> :key #'second))
  (save-form *copy-index* *copy-list*))

(defun copy-label ()
  "Return what a copy of the world now says about itself: when, which day of the game, money, debt."
  (let ((dt (object-call (gdext:singleton "Time") "get_datetime_string_from_system" nil t)))   ; 2026-11-18 16:20:05
    (format nil "~a · день ~d · ~/kiosk-trip::rub/ · долг ~/kiosk-trip::rub/"
            (subseq dt 5 (min 16 (length dt))) (today) *money* *debt*)))

(defun make-copy (&optional auto)
  "Copy the world as it is now into the free or the oldest slot; return T on success.
With AUTO it goes to the slots of automatic copies, and the hand-made ones are left alone."
  (flush-saves)
  (read-copies)
  (let* ((file (assoc :world *save-files*))
         (sections (loop for (key) in (cddr file) when (gethash key *saves*) collect key and collect (gethash key *saves*)))
         (from (if auto *copy-slots* 0)) (to (+ from (if auto *auto-slots* *copy-slots*)))
         (mine (remove-if-not (lambda (c) (<= from (first c) (1- to))) *copy-list*))
         (slot (or (loop for i from from below to unless (find i mine :key #'first) return i) (first (first (last mine)))))
         (time (object-call (gdext:singleton "Time") "get_unix_time_from_system"))
         (label (if auto (format nil "авто · ~a" (copy-label)) (copy-label)))
         (form (list :copy time label (list :save *save-version* sections))))
    (when (and (form-reads-back-p form) (save-form (copy-path slot) form))
      (note-copy slot time label)
      (log-event :copy :slot slot)
      t)))

(defun restore-copy (slot)
  "Put the copy in SLOT back as the world and read it anew; the world as it was is copied first."
  (let* ((f (load-form (copy-path slot))) (save (fourth f)))
    (cond ((not (and (fits f '(t real string t)) (fits save '(t integer list)))) (say "Копия не читается"))
          (*net-peers* (say "Сначала отпусти попутчиков: при них мир не подменить"))
          (t (make-copy)                                                  ; the world as it is now: so this can be undone
             (replace-saves (upgrade-save (second save) (third save)))
             (own-world-again)
             (log-event :restore :slot slot)
             (say (format nil "Мир восстановлен из копии: ~a. Прежний мир тоже сохранён копией" (third f)))))))

(defun make-copy-panel ()
  (multiple-value-bind (bg layer) (screen-panel 30 8 24 176 (+ 52 (* 22 (+ *copy-slots* *auto-slots*))) '(.08 .09 .1 .9))   ; layer 30: above the pause menu
    (object-set layer "visible" nil)                                   ; STEP-COPIES opens it and fills the list
    (hud-label bg "Копии мира" 8 4 10)
    (let ((new (lobby-control "Button" bg 8 22 160 20 "Сделать копию"))
          (rows (loop for i below (+ *copy-slots* *auto-slots*) collect (lobby-control "Button" bg 8 (+ 48 (* 22 i)) 160 20 ""))))
      (dolist (b (cons new rows)) (object-set b "toggle_mode" t))
      (dolist (b rows) (object-call b "add_theme_font_size_override" "font_size" 8))
      (setf *copy-ui* (list :layer layer :new new :rows rows)))))

(defun show-copy-rows ()
  (loop for b in (getf *copy-ui* :rows) for i from 0
        do (let ((c (nth i *copy-list*)))
             (object-set b "visible" (and c t))
             (when c (object-set b "text" (if (eql (first c) (car *copy-armed*)) "Точно? Нажми ещё раз" (third c)))))))

(defun step-auto-copy ()
  "Copy the world by itself once a game day, and once soon after launch, for whoever owns it."
  (when (and (not (net-guest-p)) *cart* (> *time* 20.0) (not *lobby-open*)
             (not (eql *auto-copy-day* (today))))
    (setf *auto-copy-day* (today))
    (make-copy t)))


(defun step-copies ()
  "Show the panel while the pause menu is open, for whoever owns the world, and answer its buttons."
  (noting :gfx (step-gfx))
  (noting :auto-copy (step-auto-copy))
  (let ((open (and *paused* (not (net-guest-p)) (not *lobby-open*))))
    (when (and open (null *copy-ui*)) (make-copy-panel))
    (when *copy-ui*
      (unless (eq open (object-get (getf *copy-ui* :layer) "visible"))
        (object-set (getf *copy-ui* :layer) "visible" open)
        (when open (setf *copy-armed* nil) (read-copies) (show-copy-rows)))
      (when open
        (let ((new (getf *copy-ui* :new)))
          (when (object-get new "button_pressed")
            (object-set new "button_pressed" nil)
            (say (if (make-copy) "Копия мира сделана" "Копию сделать не вышло"))
            (show-copy-rows)))
        (when (and *copy-armed* (> (- *time* (cdr *copy-armed*)) 4.0)) (setf *copy-armed* nil) (show-copy-rows))
        (loop for b in (getf *copy-ui* :rows) for i from 0
              do (when (object-get b "button_pressed")
                   (object-set b "button_pressed" nil)
                   (let ((c (nth i *copy-list*)))
                     (when c
                       (if (eql (first c) (car *copy-armed*))
                           (progn (setf *copy-armed* nil) (restore-copy (first c)))
                           (setf *copy-armed* (cons (first c) *time*)))
                       (show-copy-rows)))))))))
