;;;; Booth activities played in the world itself, not on a flat screen: at the shooting range you take
;;;; the rifle and shoot the plates that ride along the backboard; at the fishing booth you swing the
;;;; rod, cast the float into the pond, wait for the bite and reel the fish in.
;;;;
;;;; Whoever walked up plays, with their own eyes and paws (a guest too); the driver only admits them
;;;; and hands out the prize by score (START-PLAY and PLAY-RESULT in igry.lisp). Aiming is done by the
;;;; engine: a world point is projected to the screen and compared with the crosshair, so nothing here
;;;; depends on which way the road or the booth is turned. The plates and the pond are built with the
;;;; booth (prikol.lisp). The state is *ACT*.

(in-package #:kiosk-trip)

(defvar *act* nil "The activity in progress: a plist with :kind (:tir :fish) and its state, or NIL.")
(defvar *act-node* nil "What the paw holds during the activity, under the camera: the rifle or the rod.")
(defvar *act-held* nil "True if the left mouse button was down last frame.")
(defvar *bobber* nil "The fishing float: a node in the world, hidden when nobody fishes.")

(defmacro with-act (slots &body body)
  "Run BODY with each name in SLOTS standing for that field of the activity in progress.
Reading, SETF, INCF and DECF all work on the names. Do not rebind a slot name inside BODY."
  `(symbol-macrolet ,(loop for s in slots collect `(,s (getf *act* ,(intern (symbol-name s) '#:keyword)))) ,@body))

(defun hint (text) "Keep TEXT on the screen for as long as it is being set every frame." (setf *toast* (cons text .4)))

(defun eye-point () "Return the world point of the eyes." (object-get *camera* "global_position"))

(defun eye-ahead ()
  "Return the horizontal direction of the gaze as two values, x and z, of unit length."
  (let* ((n (object-call *camera* "project_ray_normal" (vector2 320 180)))
         (x (vector3-x n)) (z (vector3-z n)) (len (max 1e-4 (sqrt (+ (* x x) (* z z))))))
    (values (/ x len) (/ z len))))

(defun aimed-at-p (point radius)
  "Return true if the crosshair is on a ball of RADIUS metres around the world POINT.
The ball counts as at least 10 screen pixels wide, so a far plate is still fair game."
  (and (not (object-call *camera* "is_position_behind" point))
       (let* ((at (object-call *camera* "unproject_position" point)) (eye (eye-point))
              (far (sqrt (+ (expt (- (vector3-x point) (vector3-x eye)) 2) (expt (- (vector3-y point) (vector3-y eye)) 2)
                            (expt (- (vector3-z point) (vector3-z eye)) 2))))
              (size (max 10.0 (* 1.5 248.0 (/ radius (max .5 far))))))        ; 248 px: half the screen height over tan of half the view angle
         (< (+ (expt (- (vector2-x at) 320) 2) (expt (- (vector2-y at) 180) 2)) (* size size)))))

(defun hold-model (kind)
  "Put the tool of activity KIND into the paw, or take it away when KIND is NIL."
  (when *act-node* (node-queue-free *act-node*) (setf *act-node* nil))
  (case kind
    (:tir (let ((g (group *camera* .17 -.15 -.3)))
            (shape g :box '(.04 .06 .5) "#5a3e26" 0 -.01 0 :shadow nil)
            (shape g :cyl '(.012 .012 .55 8) "#2a2a30" 0 .03 -.45 :rot (list (/ pi 2) 0 0) :metal .6 :rough .4 :shadow nil)
            (shape g :box '(.01 .03 .01) "#16161a" 0 .055 -.7 :shadow nil)
            (setf *act-node* g)))
    (:fish (let ((g (group *camera* .24 -.22 -.3)))
             (node-3d-set-rotation g (v3 -1.0 0 -.12))
             (shape g :cyl '(.006 .014 1.7 6) "#6a5038" 0 .8 0 :shadow nil)
             (setf *act-node* g)))))

(defun start-act (kind)
  "Begin the activity KIND where the player stands."
  (setf *act* (list :kind kind :time 0.0 :from (list (vector3-x *pos*) (vector3-z *pos*))) *act-held* t)
  (hold-model kind)
  (ecase kind
    (:tir (with-act (shots hits) (setf shots 5 hits 0))
          (dolist (p *tir-plates*) (setf (seventh p) t) (object-set (first p) "visible" t)))
    (:fish (with-act (phase tries) (setf phase :aim tries 3)))))

(defun end-act (score)
  "Put the tool away and, when SCORE is not NIL, report it to the driver."
  (let ((kind (getf *act* :kind)))
    (setf *act* nil)
    (hold-model nil)
    (when *bobber* (object-set *bobber* "visible" nil))
    (when score (play-result kind score))))

;;; ------------------------------------------------------------------ shooting range

(defun step-plates (time)
  "Ride the plates back and forth along the backboard."
  (loop for (node x y z swing speed) in *tir-plates* for i from 0
        do (node-3d-set-position node (v3 (+ x (* swing (sin (+ (* time speed) i)))) y z))))

(defun step-range (click)
  (with-act (time shots hits done)
    (step-plates time)
    (cond (done (when (> time done) (end-act hits)))
          (t (hint (format nil "ТИР · выстрелов: ~d · попаданий: ~d · ЛКМ — стрелять по тарелочкам" shots hits))
             (when (and click (plusp shots))
               (decf shots)
               (let ((hit (find-if (lambda (p) (and (seventh p) (aimed-at-p (object-get (first p) "global_position") .3))) *tir-plates*)))
                 (when hit (setf (seventh hit) nil) (object-set (first hit) "visible" nil) (incf hits)))
               (setf *pitch* (min 1.3 (+ *pitch* .03)))                            ; the rifle kicks
               (when (zerop shots) (setf done (+ time .8))))))))

;;; ------------------------------------------------------------------ fishing

(defun bobber-at (x y z)
  (unless *bobber*
    (setf *bobber* (group *world*))
    (shape *bobber* :cyl '(.05 .05 .07 8) "#d8322a" 0 .035 0 :shadow nil)
    (shape *bobber* :cyl '(.05 .05 .07 8) "#f0ece0" 0 -.035 0 :shadow nil))
  (object-set *bobber* "visible" t)
  (object-set *bobber* "global_position" (v3 x y z)))

(defun retry-fishing (text)
  "Lose one try with the note TEXT: back to swinging, or home without a fish when no tries are left."
  (with-act (tries phase note time done)
    (if (plusp (decf tries))
        (setf phase :aim note text)
        (setf phase :over note (format nil "~a Рыба ушла совсем" text) done (+ time 1.6)))
    (when *bobber* (object-set *bobber* "visible" nil))))

(defun step-fishing (delta click down release)
  (with-act (time phase tries note swing fly land until pull strain done)
    (let ((eye (eye-point)) (pond (and *pond* (object-get *pond* "global_position"))))
      (ecase phase
        (:over (hint note) (when (> time done) (end-act 0)))
        (:aim
         (hint (format nil "РЫБАЛКА · ~@[~a · ~]зажми ЛКМ — замах, отпусти — заброс в воду. Попыток: ~d" note tries))
         (when click (setf phase :swing swing 0.0)))
        (:swing                                                                ; the longer the hold, the farther the float flies
         (setf swing (min 1.0 (+ swing (* delta .9))))
         (hint (format nil "Замах: ~d%" (round (* 100 swing))))
         (when release
           (multiple-value-bind (ax az) (eye-ahead)
             (let ((far (+ 2.5 (* 7.5 swing))))
               (setf phase :fly fly 0.0
                     land (list (+ (vector3-x eye) (* ax far)) (if pond (+ (vector3-y pond) .08) (- (vector3-y eye) 1.2))
                                (+ (vector3-z eye) (* az far))))))))
        (:fly
         (incf fly (* delta 1.4))
         (let ((k (min 1.0 fly)))
           (bobber-at (+ (vector3-x eye) (* k (- (first land) (vector3-x eye))))
                      (+ (vector3-y eye) (* k (- (second land) (vector3-y eye))) (* 1.6 (sin (* pi k))))
                      (+ (vector3-z eye) (* k (- (third land) (vector3-z eye))))))
         (when (>= fly 1.0)
           (if (and pond (< (+ (expt (- (first land) (vector3-x pond)) 2) (expt (- (third land) (vector3-z pond)) 2)) (* 3.8 3.8)))
               (setf phase :wait until (+ time 2.0 (random 4.0)) note nil)
               (progn (setf phase :aim note "Мимо воды — забрось в пруд") (object-set *bobber* "visible" nil)))))
        (:wait
         (bobber-at (first land) (+ (second land) (* .015 (sin (* time 3.0)))) (third land))
         (hint (format nil "Ждём… нырнёт поплавок — ЛКМ! Попыток: ~d" tries))
         (cond (click (retry-fishing "Рано дёрнул — спугнул."))
               ((> time until) (setf phase :bite until (+ time .8)))))
        (:bite
         (bobber-at (first land) (- (second land) .11) (third land))
         (hint "КЛЮЁТ! ЛКМ!")
         (cond (click (setf phase :reel pull 0.0 strain 0.0))
               ((> time until) (retry-fishing "Проспал поклёвку."))))
        (:reel                                                                 ; hold to pull, let go when the line strains: the fish jerks
         (let ((jerk (+ 1.0 (* .7 (sin (* time 2.6))))))
           (if down
               (setf pull (+ pull (* delta .34)) strain (+ strain (* delta .75 jerk)))
               (setf pull (max 0.0 (- pull (* delta .07))) strain (max 0.0 (- strain (* delta 1.1)))))
           (let ((k (min 1.0 pull)) (shake (* .04 strain (sin (* time 31.0)))))
             (bobber-at (+ (first land) shake (* k (- (vector3-x eye) (first land)))) (second land)
                        (+ (third land) (* k (- (vector3-z eye) (third land))))))
           (hint (format nil "ТЯНИ! Зажми ЛКМ — тянуть, отпусти — дать слабину · леска: ~a"
                         (let ((n (round (* 8 (min 1.0 strain))))) (concatenate 'string (make-string n :initial-element #\#) (make-string (- 8 n) :initial-element #\.)))))
           (cond ((>= strain 1.0) (retry-fishing "Леска лопнула."))
                 ((>= pull 1.0) (hint "Вытащил!") (end-act 1)))))))))

;;; ------------------------------------------------------------------ frame step

(defun step-act (delta)
  (when *act*
    (with-act (time kind from)
      (incf time delta)
      (let* ((down (and (= (input-get-mouse-mode) 2) (mouse-held-p))) (click (and down (not *act-held*))) (release (and *act-held* (not down))))
        (setf *act-held* down)
        (cond ((> (+ (expt (- (vector3-x *pos*) (first from)) 2) (expt (- (vector3-z *pos*) (second from)) 2)) (* 14.0 14.0))
               (say "Ушёл далеко — занятие окончено") (end-act nil))
              (t (ecase kind (:tir (step-range click)) (:fish (step-fishing delta click down release)))))))))
