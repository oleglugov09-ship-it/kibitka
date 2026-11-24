;;;; Files of the game, read through the engine: bytes, pictures, saved forms. (The game has no sound: the author
;;;; asked to take all of it out, 24.11.2026.)

(in-package #:kiosk-trip)

;;; Files

(defun file-bytes (path)
  "Return the bytes of the game file at PATH (res://...).
Reading goes through the engine, which works both from disk and from the game package."
  (coerce (file-access-get-file-as-bytes path) '(simple-array (unsigned-byte 8) (*))))

(defun file-here-p (path) (file-access-file-exists path))

;;; Pictures are drawn and kept as PNG, but next to a .png there may lie a .webp of it, several times smaller
;;; (tools/pack-assets.py), and the browser build packs only that one: a new player downloads the whole game
;;; before the first frame. So a picture is asked for by its .png path and read from whichever copy is there.

(defun packed-path (path)
  "Return the path the picture PATH (a .png) is really kept at: its .webp where there is one, else PATH itself."
  (let ((webp (and (> (length path) 4) (string-equal ".png" path :start2 (- (length path) 4))
                   (concatenate 'string (subseq path 0 (- (length path) 4)) ".webp"))))
    (if (and webp (file-access-file-exists webp)) webp path)))

(defun picture-here-p (path) "Return true if the picture PATH (a .png) is there, as it is or as its smaller copy." (file-access-file-exists (packed-path path)))

(defun read-picture (image path)
  "Load the picture PATH (a .png) into the Image IMAGE, from its smaller copy where there is one."
  (let ((real (packed-path path)))
    (object-call image (if (eq real path) "load_png_from_buffer" "load_webp_from_buffer") (file-bytes real))))

(defun save-form (path form)
  "Write FORM as text to the file PATH (user://...) and return T on success.
Writing goes through the engine: in the browser that puts the data into persistent storage,
while Lisp's ordinary file output does not."
  (let ((file (file-access-open path 2)))              ; FileAccess.WRITE
    (when file
      (object-call file "store_string" (with-standard-io-syntax (prin1-to-string form)))
      (object-call file "close")
      t)))

(defun load-form (path)
  "Read what SAVE-FORM wrote to PATH.
Return NIL if the file is missing or damaged."
  (when (file-here-p path)
    (ignore-errors
     (with-standard-io-syntax
       (let ((*read-eval* nil)) (values (read-from-string (file-access-get-file-as-string path))))))))
