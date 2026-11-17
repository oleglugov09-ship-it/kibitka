;;;; Saves. Two files instead of ten: the world (user://save.sexp — the game, items, the wall, finds, booths,
;;;; papers, gnomes) and the player (user://me.sexp — where they stand, what they wear, the last lobby). The
;;;; world is written only by whoever owns the game (the driver or a solo player); players always write their own.
;;;;
;;;; Subsystems keep their sections through PUT-SAVE and GET-SAVE; how a section is laid out is their business.
;;;; The file carries a version number: when the layout changes, a step is added to *SAVE-STEPS*, and an old
;;;; file is brought up to date on reading. We write through a temporary file, and the previous one stays as a
;;;; backup copy (.bak): if a write was cut short, we read the copy. Old-style files (one per subsystem) are
;;;; left untouched: sections are taken from them once, while the new file does not exist yet.

(in-package #:kiosk-trip)

(defparameter *save-version* 1)

(defparameter *save-files*
  '((:world "user://save.sexp"
     (:game "user://game.sexp") (:interior "user://interior6.sexp") (:wall "user://wall.sexp") (:finds "user://finds.sexp")
     (:prikol "user://prikol.sexp") (:papers "user://bumagi.sexp") (:gnomes "user://gnomy.sexp")
     (:stories "user://stories.sexp"))
    (:me "user://me.sexp"
     (:place "user://player.sexp") (:look "user://look.sexp") (:lobby "user://lobby.sexp")
     (:lang "user://lang.sexp")))
  "The save files: (name path (section old-style-file-path) ...).")

(defparameter *save-steps* '()
  "The transitions between file versions: (from-version . function).
The function receives the list of sections (key form ...) and returns it in the layout of the next version.")

(defvar *saves* (make-hash-table) "The table mapping a section to its form as it lies (or will lie) in the file.")
(defvar *saves-read* '() "The names of the files that have already been read.")
(defvar *saves-dirty* '() "The names of the files that are due to be written.")
(defvar *save-timer* 0.0)

(defun save-file-of (key) (find-if (lambda (f) (assoc key (cddr f))) *save-files*))

(defun upgrade-save (version sections)
  "Take SECTIONS through every step from VERSION to the current one."
  (loop while (< version *save-version*)
        do (let ((step (cdr (assoc version *save-steps*))))
             (when step (setf sections (funcall step sections)))
             (incf version)))
  sections)

(defun read-save (path)
  "Return the sections from the file PATH as (key form ...) of the current version.
Return :NONE if the file is missing or unreadable."
  (let ((form (load-form path)))
    (if (and (listp form) (eq (first form) :save) (integerp (second form)) (listp (third form)) (evenp (length (third form))))
        (upgrade-save (second form) (third form))
        :none)))

(defun ensure-save (file)
  "Read the file FILE once.
Read the file itself, its backup copy or, when neither exists, the old-style files."
  (destructuring-bind (name path &rest sections) file
    (unless (member name *saves-read*)
      (push name *saves-read*)
      (let ((got (read-save path)))
        (when (eq got :none) (setf got (read-save (concatenate 'string path ".bak"))))
        (cond ((eq got :none)                                      ; no new file — take the old ones one by one
               (loop for (key old) in sections
                     do (let ((form (and (file-here-p old) (load-form old))))
                          (when form (setf (gethash key *saves*) form) (pushnew name *saves-dirty*)))))
              (t (loop for (key form) on got by #'cddr
                       do (when (assoc key sections) (setf (gethash key *saves*) form)))))))))

(defun get-save (key)
  "Return what is saved in the section KEY, or NIL."
  (let ((file (save-file-of key)))
    (when file (ensure-save file) (values (gethash key *saves*)))))

(defun put-save (key form)
  "Put FORM into the section KEY.
It reaches the disk within the next second (STEP-SAVES)."
  (let ((file (save-file-of key)))
    (when file
      (ensure-save file)
      (unless (equal form (gethash key *saves*))
        (setf (gethash key *saves*) form)
        (pushnew (first file) *saves-dirty*)))))

(defun write-save (file)
  (destructuring-bind (name path &rest sections) file
    (declare (ignore name))
    (let ((tmp (concatenate 'string path ".tmp")) (bak (concatenate 'string path ".bak"))
          (form (list :save *save-version*
                      (loop for (key) in sections
                            when (gethash key *saves*) collect key and collect (gethash key *saves*)))))
      (when (save-form tmp form)
        (when (file-here-p path) (dir-access-rename-absolute path bak))        ; the previous one becomes the backup
        (dir-access-rename-absolute tmp path)))))

(defun flush-saves ()
  "Write everything that has changed right now."
  (dolist (name (shiftf *saves-dirty* '()))
    (noting :save (write-save (assoc name *save-files*)))))

(defun step-saves (delta)
  (when (and *saves-dirty* (<= (decf *save-timer* delta) 0))
    (setf *save-timer* 1.0)
    (flush-saves)))
