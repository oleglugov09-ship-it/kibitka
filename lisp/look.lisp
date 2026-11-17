;;;; Wardrobe: what your cat looks like. I opens the profile: outfit and fur colour, with a fitting doll
;;;; next to it. The look is a single number LOOK: units are the outfit, tens are the fur (0 means
;;;; 'whichever you got'); that is how MAKE-CAT (doroga.lisp) reads it and how it travels over the
;;;; network together with the player's position (net.lisp).

(in-package #:kiosk-trip)

(defvar *outfits* '())
(defparameter *fur-names* '("Чёрный" "Рыжий" "Серый" "Белый" "Бурый" "Угольный"))

(defvar *outfit* 0 "The outfit, as an index into *OUTFITS*.")
(defvar *fur* 1 "The fur colour, as an index into *CAT-FURS*.")
(defvar *look-open* nil)
(defvar *look-panel* nil)
(defvar *look-buttons* '() "The profile buttons, as a list of (KIND NUMBER BUTTON) entries.")
(defvar *look-doll* nil "The fitting cat shown in front of the camera.")

(defun my-look () (+ *outfit* (* 10 (1+ *fur*)) (case *torch* (:beam 100) (:chin 200) (t 0))))        ; hundreds: torch, as a beam or under the chin (uyut.lisp)

(defun load-look ()
  (let ((form (get-save :look)))
    (when (and (listp form) (= (length form) 2) (integerp (first form)) (integerp (second form)))
      (setf *outfit* (mod (first form) (length *outfits*)) *fur* (mod (second form) (length *fur-names*))))))

(defun show-doll ()
  "Place a cat in the chosen outfit a metre and a half in front of the player, facing the player."
  (when *look-doll* (node-queue-free *look-doll*))
  (let* ((x (+ (vector3-x *pos*) (* -1.5 (sin *yaw*)))) (z (+ (vector3-z *pos*) (* -1.5 (cos *yaw*))))
         (g (group *cart* (max (- .3 *half-width*) (min (- *half-width* .3) x)) *floor* (max -2.2 (min 2.5 z)))))
    (node-3d-set-rotation g (v3 0 *yaw* 0))
    (make-cat g 0 (my-look))
    (setf *look-doll* g)))

(defun mark-look ()
  (loop for (kind n button) in *look-buttons*
        do (object-set button "text" (format nil "~:[   ~;● ~]~a" (= n (if (eq kind :outfit) *outfit* *fur*))
                                             (nth n (if (eq kind :outfit) *outfits* *fur-names*))))))

(defun make-look (root)
  (let ((layer (gdext:make-object "CanvasLayer")) (bg (gdext:make-object "Control")))
    (object-set layer "layer" 5)
    (node-add-child root layer)
    (node-add-child layer bg)
    (hud-rect bg 20 40 210 280 '(.13 .14 .15) .94)
    (hud-rect bg 20 40 210 2 '(.79 .63 .23))
    (hud-label bg "ПРОФИЛЬ" 34 50 14 '(.92 .88 .76))
    (hud-label bg "Одежда" 34 78 8)
    (hud-label bg "Шерсть" 34 208 8)
    (hud-label bg "I — закрыть · кот справа — это ты" 34 300 7 '(.6 .6 .56))
    (setf *look-buttons*
          (append (loop for name in *outfits* for i from 0
                        collect (list :outfit i (lobby-control "Button" bg 34 (+ 92 (* i 22)) 182 20 name)))
                  (loop for name in *fur-names* for i from 0
                        collect (list :fur i (lobby-control "Button" bg (+ 34 (* 93 (mod i 2))) (+ 222 (* 24 (floor i 2))) 89 20 name)))))
    (dolist (b *look-buttons*) (object-set (third b) "toggle_mode" t) (object-set (third b) "alignment" 0))
    (object-set layer "visible" nil)
    (setf *look-panel* layer)
    (load-look)))

(defun open-look ()
  (unless (or *lobby-open* *map-open* *meta*)
    (setf *look-open* t)
    (object-set *look-panel* "visible" t)
    (mark-look) (show-doll)
    (input-set-mouse-mode 0)))

(defun close-look ()
  (setf *look-open* nil)
  (object-set *look-panel* "visible" nil)
  (when *look-doll* (node-queue-free *look-doll*) (setf *look-doll* nil))
  (put-save :look (list *outfit* *fur*))
  (input-set-mouse-mode 2))

(defun look-key (event)
  "Handle the input EVENT while the profile is open.
Game keys are muted meanwhile; I or Esc closes the profile."
  (when (and (object-call event "is_class" "InputEventKey") (object-get event "pressed") (not (object-get event "echo"))
             (member (key-code event) '(73 4194305)))
    (close-look)))

(defun step-look ()
  (when *look-open*
    (loop for (kind n button) in *look-buttons*
          do (when (object-get button "button_pressed")
               (object-set button "button_pressed" nil)
               (if (eq kind :outfit) (setf *outfit* n) (setf *fur* n))
               (mark-look) (show-doll)))))
