;;;; What is under the crosshair and what to do with it. E is interaction: an item from USABLES (byt.lisp), the door,
;;;; and with nothing to look at, sit down on the nearest seat or stand up. LMB is the hand: take or put down
;;;; a thing (trade.lisp), the record player's discs. The hint under the crosshair says what will happen.

(in-package #:kiosk-trip)

(defparameter *reach* 2.4 "The reach to the record player, the crate or the door, in meters.")

(defvar *hint* nil "The Label that shows the hint.")
(defvar *toast* nil "The current toast message: (text . seconds left to show).")

(defun look-dir ()
  (v3 (* (- (sin *yaw*)) (cos *pitch*)) (sin *pitch*) (* (- (cos *yaw*)) (cos *pitch*))))

(defun gaze-on-door-p ()
  "Return true if the crosshair is on the door leaf itself, wherever it stands - shut across the doorway or swung
open onto the porch: the leaf is a board 1.1 m wide and 2 m high on its hinge. Aiming at the middle of the doorway
alone made an open door one could look straight at and not close (the author, 24.11)."
  (let* ((dir (look-dir)) (a (* 1.55 *door-k*))
         (ux (cos a)) (uz (sin a))                                     ; along the leaf, from the hinge
         (hx (- -.55 (vector3-x *pos*))) (hz (- (+ *half-length* .1) (vector3-z *pos*)))
         (dx (vector3-x dir)) (dz (vector3-z dir))
         (det (- (* ux dz) (* dx uz))))
    (when (> (abs det) 1e-4)
      (let* ((along (/ (- (* ux hz) (* hx uz)) det))                   ; metres along the gaze to the leaf's plane
             (across (/ (- (* dx hz) (* dz hx)) det))                  ; metres from the hinge along the leaf
             (y (+ *eye-now* (* along (vector3-y dir)))))
        (and (< 0.0 along *reach*) (< -.05 across 1.15) (< (- *floor* .05) y (+ *floor* 2.1)))))))

(defun target ()
  "Return what the player is looking at: :jukebox, :crate, :door or NIL."
  (let* ((eye (v3 (vector3-x *pos*) *eye-now* (vector3-z *pos*)))
         (dir (look-dir)) (best nil) (best-a .32))
    (loop for (what (x z) y) in (if *meta*
                                    (list (list :jukebox *jukebox-at* .35))
                                    (list (list :jukebox *jukebox-at* (+ *jukebox-y* .2)) (list :crate *crate-at* (+ *jukebox-y* .1)) (list :door *door-at* 1.0)))
          do (let* ((dx (- x (vector3-x eye))) (dy (- (+ *floor* y) (vector3-y eye))) (dz (- z (vector3-z eye)))
                    (len (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
                    (cosa (/ (+ (* dx (vector3-x dir)) (* dy (vector3-y dir)) (* dz (vector3-z dir))) (max len .01)))
                    (a (acos (max -1 (min 1 cosa)))))
               ;; the door counts only when the gaze is really on it: its half-width as seen from here, not a wide cone
               (when (and (< len *reach*) (< a best-a) (or (not (eq what :door)) (< a (+ .06 (atan .45 (max len .3))))))
                 (setf best what best-a a))))
    (or best (and (not *meta*) (gaze-on-door-p) :door))))

(defun interact ()
  "Handle a left click while the mouse is captured."
  (case (target)
    (:crate
     ;; with a disc in hand, put it back (at the end of the crate); with an empty hand, take the first one.
     ;; This frees the hands, and to go through all the discs you put and take in turn
     (if *held*
         (setf *crate* (append *crate* (list *held*)) *held* nil)
         (setf *held* (pop *crate*)))
     (show-discs))
    (:door (toggle-door))
    (:jukebox
     (let ((out *playing*))
       (play-disc *held*)
       (setf *held* out)
       (show-discs)))))

(defun seat-near ()
  "Return the nearest seat within reach, or NIL."
  (loop for seat in (seats-here)
        for (nil x z) = seat
        for dd = (sqrt (+ (expt (- x (vector3-x *pos*)) 2) (expt (- z (vector3-z *pos*)) 2)))
        when (< dd *sit-reach*) minimize dd into m and collect (cons dd seat) into all
        finally (return (cdr (find m all :key #'car)))))

(defun use-thing ()
  "Handle E: use what the player is looking at.
With nothing to look at, sit down on the nearest seat or stand up."
  (cond ((outside-p) (toggle-sit))
        ((use-usable))
        ((eq (target) :door) (interact))
        ((and *meta* (not *seat*) (not (seat-near))) (toggle-meta-curtains))
        ((or *seat* *meta*) (toggle-sit))))                       ; in the wagon you sit by looking at a seat (SEAT-USABLES); E at nothing just stands up

(defun in-cart-p ()
  "Return true when the player is inside the wagon and may touch its things.
That means not outside, not in the meta room and not waiting at a bus stop."
  (and (eq *view* :inside) (not *meta*)))

(defun hand-thing ()
  "Handle a left click: take by hand what the player is looking at, or put down what is carried.
Discs are handled by hand too."
  (cond ((not (in-cart-p)) (when (and *meta* (member (target) '(:crate :jukebox))) (interact)))   ; in the meta room, discs only
        ((use-usable :hand))
        ((carry-use))
        ((member (target) '(:crate :jukebox)) (interact))))

(defun hint-text ()
  (let ((u (and (in-cart-p) (or (usable-here :hand) (usable-here)))))
    (cond ((and (/= (input-get-mouse-mode) 2) (not *lobby-open*) (not *map-open*) (not *look-open*) (not *docs-open*)
                (not *cheats-open*) (not *paused*))
           "Щёлкни мышью по игре, чтобы снова оглядываться")              ; the browser let the mouse go (Esc, another window)
          (u (if (usable-label u) (funcall (usable-label u)) ""))
          ((and (in-cart-p) *build*) *build-hint*)
          ((and (in-cart-p) (carry-hint)))
          ((and (in-cart-p) (client-hint)))
          (t (hint-text-rest)))))

(defun hint-text-rest ()
  (case (target)
    (:crate (cond ((and (null *crate*) (null *held*)) "Ящик с дисками пуст")
                  (*held* (format nil "ЛКМ — положить «~a» на место" (disc-title *held*)))
                  (t (format nil "ЛКМ — взять диск «~a»" (disc-title (first *crate*))))))
    (:jukebox (cond ((and *held* *playing*) (format nil "ЛКМ — поменять на «~a»" (disc-title *held*)))
                    (*held* (format nil "ЛКМ — поставить «~a»" (disc-title *held*)))
                    (*playing* (format nil "Играет «~a» · ЛКМ — вынуть" (disc-title *playing*)))
                    (t "Проигрыватель · возьми диск из ящика")))
    (:door (cond ((and *door-open* (not (inside-cart-p))) "")
                 (*door-open* "E — закрыть дверь")
                 (t "E — открыть дверь наружу")))
    (t (cond (*spell* "")
             (*meta* (if *meta-curtains-shut* "E — открыть шторы · Q — назад в кибитку" "Q — назад в кибитку"))
             (*seat* "E — встать")
             ((and *meta* (seat-near)) "E — сесть")
             (t "")))))

;;; What is said goes three ways. A message about what we did is a toast in the middle of the screen: it
;;; stays for as long as it takes to read, and the next one waits its turn instead of wiping it out. What a
;;; cat says - anything with «words» after a name - goes into a framed panel lower down, with a queue of
;;; its own, so a customer's line and a hint no longer fight for one place. And everything said is written
;;; into the notebook (N), newest first, for whoever did not have the time to read it.

(defvar *toasts* '() "The messages waiting for their turn in the middle of the screen.")
(defvar *toast-shown* 0.0 "The seconds the current message has been on the screen.")
(defvar *speech* nil "The line in the panel right now: (text . seconds left).")
(defvar *speeches* '() "The lines waiting for the panel.")
(defvar *speech-label* nil)
(defvar *said-log* '() "The notebook: entries of (day . text), the newest first.")
(defparameter *said-keep* 90 "The number of entries the notebook keeps.")
(defvar *said-n* 0 "How many entries the notebook has taken in all.")
(defvar *said-seen* 0 "The value of *SAID-N* when the diary was last closed: what came after it has not been read.")
(defparameter *toast-least* 1.2 "The seconds a message stays at the least before the next one may take its place.")

(defun read-time (text) "Return the seconds TEXT stays on the screen: longer for a longer one." (max 2.5 (min 10.0 (/ (length text) 12.0))))
(defun speech-p (text) "Return true if TEXT is somebody's words: a name, a colon and a quotation." (and (search ": «" text) t))

(defun note-said (text)
  (unless (equal text (cdr (first *said-log*)))
    (push (cons (trip-day) text) *said-log*)
    (incf *said-n*)
    (when (> (length *said-log*) *said-keep*) (setf *said-log* (subseq *said-log* 0 *said-keep*)))))

(defun say (text)
  "Show TEXT on the screen and write it into the notebook.
When the driver performs an action on behalf of a guest, the hint goes to the guest,
not to the driver."
  (cond (*actor* (push text *said*))
        ((not (stringp text)))
        (t (note-said text)
           (cond ((speech-p text)
                  (cond ((null *speech*) (setf *speech* (cons text (read-time text))))
                        ((or (equal text (car *speech*)) (member text *speeches* :test #'equal)))
                        (t (setf *speeches* (last (append *speeches* (list text)) 4)))))
                 ((and *toast* (> (cdr *toast*) .45) (< *toast-shown* *toast-least*) (not (equal text (car *toast*))))   ; something is still being read: wait
                  (unless (member text *toasts* :test #'equal)
                    (setf *toasts* (last (append *toasts* (list text)) 4))))
                 (t (setf *toast* (cons text (read-time text)) *toast-shown* 0.0))))))

(defun press-notebook ()
  "Open the notebook: everything that was said, the newest first."
  (cond (*show* nil)
        (t (log-event :notebook :n (length *said-log*))
           (open-diary) t)))                                       ; the notebook is the diary's bookmark in the folder

(defun make-hint (root)
  (let ((layer (gdext:make-object "CanvasLayer")) (l (gdext:make-object "Label")))
    (node-add-child root layer)
    (node-add-child layer l)
    (object-set l "anchor_left" .5) (object-set l "anchor_right" .5)
    (object-set l "anchor_top" .62) (object-set l "anchor_bottom" .62)
    (object-set l "offset_left" -300.0) (object-set l "offset_right" 300.0)
    (object-set l "horizontal_alignment" 1)
    (object-call l "add_theme_font_size_override" "font_size" 10)   ; the picture is 640x360: the font is half the size
    (object-call l "add_theme_constant_override" "outline_size" 3)
    (object-call l "add_theme_color_override" "font_outline_color" (color 0 0 0 .8))
    (setf *hint* l)
    ;; the panel for what cats say: lower down, framed, wrapped to a column
    (let ((s (gdext:make-object "Label")) (box (gdext:make-object "StyleBoxFlat")))
      (node-add-child layer s)
      (object-set box "bg_color" (color .07 .06 .05 .82))
      (object-set box "border_color" (color .85 .66 .25 .9))
      (dolist (side '("left" "top" "right" "bottom"))
        (object-set box (format nil "border_width_~a" side) 1)
        (object-set box (format nil "content_margin_~a" side) 6.0))
      (object-call s "add_theme_stylebox_override" "normal" box)
      (object-set s "anchor_left" .5) (object-set s "anchor_right" .5)
      (object-set s "anchor_top" .97) (object-set s "anchor_bottom" .97)
      (object-set s "offset_left" -210.0) (object-set s "offset_right" 210.0)
      (object-set s "grow_vertical" 0)                                   ; grows upwards from the bottom edge
      (object-set s "horizontal_alignment" 1)
      (object-set s "autowrap_mode" 3)
      (object-call s "add_theme_font_size_override" "font_size" 10)
      (object-set s "visible" nil)
      (setf *speech-label* s))))

(defun step-hint (delta)
  "Update the hint each frame, DELTA seconds after the previous one.
Show the SAY message until it expires, otherwise what is under the crosshair."
  (when *toast*
    (incf *toast-shown* delta)
    (when (or (<= (decf (cdr *toast*) delta) 0) (and *toasts* (> *toast-shown* *toast-least*) (> (cdr *toast*) .45)))
      (setf *toast* nil)))
  (when (and (null *toast*) *toasts*)                                    ; the next one in the queue
    (let ((text (pop *toasts*))) (setf *toast* (cons text (read-time text)) *toast-shown* 0.0)))
  (when *speech* (when (<= (decf (cdr *speech*) delta) 0) (setf *speech* nil)))
  (when (and (null *speech*) *speeches*)
    (let ((text (pop *speeches*))) (setf *speech* (cons text (read-time text)))))
  (when *speech-label*
    (let ((text (if (and *speech* (not *hud-hidden*)) (car *speech*) "")))
      (unless (string= text (object-get *speech-label* "text"))
        (object-set *speech-label* "text" text)
        (object-set *speech-label* "visible" (plusp (length text))))))
  (when *hint*
    (let ((text (cond (*toast* (car *toast*)) ((= (input-get-mouse-mode) 2) (hint-text)) (t ""))))
      (unless (string= text (object-get *hint* "text")) (object-set *hint* "text" text)))))
