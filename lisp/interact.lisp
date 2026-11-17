;;;; What is under the crosshair and what to do with it. E is interaction: an item from USABLES (byt.lisp), the door,
;;;; and with nothing to look at, sit down on the nearest seat or stand up. LMB is the hand: take or put down
;;;; a thing (trade.lisp), the record player's discs. The hint under the crosshair says what will happen.

(in-package #:kiosk-trip)

(defparameter *reach* 2.4 "The reach to the record player, the crate or the door, in meters.")

(defvar *hint* nil "The Label that shows the hint.")
(defvar *toast* nil "The current toast message: (text . seconds left to show).")

(defun look-dir ()
  (v3 (* (- (sin *yaw*)) (cos *pitch*)) (sin *pitch*) (* (- (cos *yaw*)) (cos *pitch*))))

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
    best))

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
                (not *play*) (not *cheats-open*) (not *paused*))
           "Щёлкни мышью по игре, чтобы снова оглядываться")              ; the browser let the mouse go (Esc, another window)
          (u (if (usable-label u) (funcall (usable-label u)) ""))
          ((and (in-cart-p) *build*) *build-hint*)
          ((and (in-cart-p) (carry-hint)))
          ((and (in-cart-p) (client-hint)))
          (t (hint-text-rest)))))

(defun hint-text-rest ()
  (case (target)
    (:crate (cond ((and (null *crate*) (null *held*)) "Ящик с дисками пуст")
                  (*held* (format nil "ЛКМ — положить «~a» на место" (disc-name *held*)))
                  (t (format nil "ЛКМ — взять диск «~a»" (disc-name (first *crate*))))))
    (:jukebox (cond ((and *held* *playing*) (format nil "ЛКМ — поменять на «~a»" (disc-name *held*)))
                    (*held* (format nil "ЛКМ — поставить «~a»" (disc-name *held*)))
                    (*playing* (format nil "Играет «~a» · ЛКМ — вынуть" (disc-name *playing*)))
                    (t "Проигрыватель · возьми диск из ящика")))
    (:door (cond ((and *door-open* (not (inside-cart-p))) "")
                 (*door-open* "E — закрыть дверь")
                 (t "E — открыть дверь наружу")))
    (t (cond (*spell* "")
             (*meta* (if *meta-curtains-shut* "E — открыть шторы · Q — назад в кибитку" "Q — назад в кибитку"))
             (*seat* "E — встать")
             ((and *meta* (seat-near)) "E — сесть")
             ((spell-ready-p) "Q — заклинание")
             (t "")))))

(defun say (text)
  "Show TEXT as a hint on the screen.
When the driver performs an action on behalf of a guest, the hint goes to the guest,
not to the driver."
  (if *actor* (push text *said*) (setf *toast* (cons text 2.5))))

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
    (setf *hint* l)))

(defun step-hint (delta)
  "Update the hint each frame, DELTA seconds after the previous one.
Show the SAY message until it expires, otherwise what is under the crosshair."
  (when *toast* (when (<= (decf (cdr *toast*) delta) 0) (setf *toast* nil)))
  (when *hint*
    (let ((text (cond (*toast* (car *toast*)) ((= (input-get-mouse-mode) 2) (hint-text)) (t ""))))
      (unless (string= text (object-get *hint* "text")) (object-set *hint* "text" text)))))
