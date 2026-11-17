;;;; Mods. A mod is a folder with a mod.lisp file: plain Lisp, read and evaluated at startup in the game
;;;; package, before the world is planned and the save is loaded. Mods come from two places: res://mods
;;;; (shipped with the game, project/mods/ in the sources) and user://mods (the player's own folder, no
;;;; rebuild needed). Within each place they load in alphabetical order, the player's after the shipped
;;;; ones, so a player's mod can replace a shipped story by defining one with the same key.
;;;;
;;;; A broken mod must not take the game down: its error is recorded and the next mod loads. What mods
;;;; are meant to call is listed in MODDING.md; nothing stops a mod from calling anything else.

(in-package #:kiosk-trip)

(defvar *mods* '() "The mods found at startup, in load order: entries of (name place status), status :ok or the error text.")
(defvar *mod* nil "The name of the mod being loaded, or NIL.")
(defvar *mod-dir* nil "The folder of the mod being loaded, or NIL.")

(defun text-file (path)
  "Return the text of the game file at PATH, decoded as UTF-8."
  ;; read through a stream: EXT:OCTETS-TO-STRING sizes its result by the byte count, so text with
  ;; multi-byte letters comes back with garbage after the real end
  (let ((in (ext:make-sequence-input-stream (file-bytes path) :external-format :utf-8)))
    (with-output-to-string (out)
      (loop for c = (read-char in nil) while c do (write-char c out)))))

(defun eval-text (text)
  "Read the forms in TEXT one after another and evaluate them in the game package."
  (let ((*package* (find-package '#:kiosk-trip)) (start 0))
    (loop (multiple-value-bind (form next) (read-from-string text nil :eof :start start)
            (when (eq form :eof) (return))
            (eval form)
            (setf start next)))))

(defun include (file)
  "Read and evaluate FILE from the folder of the mod being loaded; a mod.lisp calls this to split itself up."
  (eval-text (text-file (cl:format nil "~a/~a" *mod-dir* file))))

(defun mod-names (place)
  "Return the names of the mod folders in PLACE that have a mod.lisp, sorted."
  (sort (remove-if-not (lambda (name) (file-here-p (cl:format nil "~a/~a/mod.lisp" place name)))
                       (or (ignore-errors (coerce (dir-access-get-directories-at place) 'list)) '()))
        #'string<))

(defun load-mods ()
  "Load every mod from res://mods and then from user://mods, and record how each went in *MODS*."
  (setf *mods* '())
  (dolist (place '("res://mods" "user://mods"))
    (dolist (name (mod-names place))
      (let ((*mod* name) (*mod-dir* (cl:format nil "~a/~a" place name)))
        (push (list name place
                    (handler-case (progn (eval-text (text-file (cl:format nil "~a/~a/mod.lisp" place name))) :ok)
                      (error (c) (note-error c (cl:format nil "mod ~a" name)) (princ-to-string c))))
              *mods*))))
  (setf *mods* (nreverse *mods*))
  (cl:format t "~&Mods: ~:[none~;~:*~{~a~^, ~}~]~%"
             (mapcar (lambda (m) (if (eq (third m) :ok) (first m) (cl:format nil "~a (FAILED)" (first m)))) *mods*)))
