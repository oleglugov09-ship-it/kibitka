;;;; Copies of the world by hand. In the pause menu (Esc) whoever owns the world, the driver or a solo
;;;; player, sees a panel at the left: «Make a copy» and the list of copies made. A click on a copy, and a
;;;; second click to confirm, puts it back: the world on the disk becomes the copy and the game reads it
;;;; anew. Before that the world as it is now is copied too, so a restore can be undone. The automatic
;;;; guards (save.lisp) save a file that does not read; these save a world that reads well but has gone
;;;; wrong, and the player decides which one is right.

(in-package #:kiosk-trip)

(defparameter *copy-slots* 8 "How many copies are kept: a new one takes the place of the oldest.")
(defvar *copy-ui* nil "The panel: (:layer :new button :rows (button ...)).")
(defvar *copy-armed* nil "The copy clicked once and waiting for the second click: (slot . time).")
(defvar *copy-list* '() "The copies on disk as (slot unix-time label), the newest first; read when the panel opens.")

(defun copy-path (slot) (cl:format nil "user://copy-~d.sexp" slot))

(defun copy-info (slot)
  "Return (slot unix-time label) for the copy in SLOT, or NIL if there is none or it does not read."
  (let ((f (load-form (copy-path slot))))
    (and (fits f '(t real string t)) (eq (first f) :copy) (list slot (second f) (third f)))))

(defun read-copies ()
  (setf *copy-list* (sort (loop for i below *copy-slots* for c = (copy-info i) when c collect c) #'> :key #'second)))

(defun copy-label ()
  "Return what a copy of the world now says about itself: when, which day of the game, money, debt."
  (let ((dt (object-call (gdext:singleton "Time") "get_datetime_string_from_system" nil t)))   ; 2026-11-18 16:20:05
    (format nil "~a · день ~d · ~/kiosk-trip::rub/ · долг ~/kiosk-trip::rub/"
            (subseq dt 5 (min 16 (length dt))) (today) *money* *debt*)))

(defun make-copy ()
  "Copy the world as it is now into the free or the oldest slot; return T on success."
  (flush-saves)
  (let* ((file (assoc :world *save-files*))
         (sections (loop for (key) in (cddr file) when (gethash key *saves*) collect key and collect (gethash key *saves*)))
         (used (mapcar #'first *copy-list*))
         (slot (or (loop for i below *copy-slots* unless (member i used) return i) (first (first (last *copy-list*)))))
         (form (list :copy (object-call (gdext:singleton "Time") "get_unix_time_from_system") (copy-label)
                     (list :save *save-version* sections))))
    (when (and (form-reads-back-p form) (save-form (copy-path slot) form))
      (read-copies)
      (log-event :copy :slot slot)
      t)))

(defun restore-copy (slot)
  "Put the copy in SLOT back as the world and read it anew; the world as it was is copied first."
  (let* ((f (load-form (copy-path slot))) (save (fourth f)))
    (cond ((not (and (fits f '(t real string t)) (fits save '(t integer list)))) (say "Копия не читается"))
          (*net-peers* (say "Сначала отпусти попутчиков: при них мир не подменить"))
          (t (make-copy)                                                  ; the world as it is now: so this can be undone
             (let ((sections (upgrade-save (second save) (third save))))
               (loop for (key form) on sections by #'cddr do (setf (gethash key *saves*) form))
               (pushnew :world *saves-dirty*)
               (flush-saves))
             (setf *carry* nil *carry-from* nil *auto* nil)
             (show-carry)
             (start-trip)
             (set-items (load-interior))
             (unless (load-game) (setf *onboard* 0))
             (load-world)
             (log-event :restore :slot slot)
             (say (format nil "Мир восстановлен из копии: ~a. Прежний мир тоже сохранён копией" (third f)))))))

(defun make-copy-panel ()
  (let ((layer (gdext:make-object "CanvasLayer")) (bg (gdext:make-object "ColorRect")))
    (node-add-child *root* layer) (node-add-child layer bg)
    (object-set layer "layer" 30)                                      ; above the pause menu
    (object-set layer "visible" nil)                                   ; STEP-COPIES opens it and fills the list
    (object-set bg "position" (vector2 8 24)) (object-set bg "size" (vector2 176 (+ 52 (* 22 *copy-slots*))))
    (object-set bg "color" (color .08 .09 .1 .9))
    (hud-label bg "Копии мира" 8 4 10)
    (let ((new (lobby-control "Button" bg 8 22 160 20 "Сделать копию"))
          (rows (loop for i below *copy-slots* collect (lobby-control "Button" bg 8 (+ 48 (* 22 i)) 160 20 ""))))
      (dolist (b (cons new rows)) (object-set b "toggle_mode" t))
      (dolist (b rows) (object-call b "add_theme_font_size_override" "font_size" 8))
      (setf *copy-ui* (list :layer layer :new new :rows rows)))))

(defun show-copy-rows ()
  (loop for b in (getf *copy-ui* :rows) for i from 0
        do (let ((c (nth i *copy-list*)))
             (object-set b "visible" (and c t))
             (when c (object-set b "text" (if (eql (first c) (car *copy-armed*)) "Точно? Нажми ещё раз" (third c)))))))

(defun step-copies ()
  "Show the panel while the pause menu is open, for whoever owns the world, and answer its buttons."
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
