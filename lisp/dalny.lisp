;;;; Dalny is a shift-workers' settlement by the marshes, the northernmost on the ring road. Its three features:
;;;;  • northern lights: at night in clear weather over the settlement and on the approach; a photo goes into the album;
;;;;  • airmail: a parcel in hand can go by plane; it gets there by itself, but part of the pay goes to forwarding;
;;;;  • the plane: from time to time a crop-duster takes off and passes over the road. You cannot board it:
;;;;    the game is about the wagon and the view from the window.

(in-package #:kiosk-trip)

(defparameter *dalny* 7 "The index of the town Dalny.")
(defvar *airmail-d* nil "The path coordinate of the airmail hut.")
(defvar *aurora* nil "The aurora node.")
(defvar *aurora-mat* nil)
(defvar *aurora-k* 0.0 "The current strength of the aurora, 0..1.")
(defvar *plane* nil "The plane in flight: (NODE TAKEOFF-TIME FROM TO).")
(defvar *next-plane* 60.0)

(defun dalny-mid ()
  (let ((tw (nth *dalny* *towns*))) (and tw (/ (+ (town-start tw) (town-end tw)) 2))))

;;; ------------------------------------------------------------------ northern lights

(defun make-aurora (root)
  "Build the aurora under ROOT: four curtains high in the sky on the window side.
They are green at the bottom and fade into purple towards the top."
  (let ((st (gdext:make-object "SurfaceTool")) (mi (gdext:make-object "MeshInstance3D")) (uv (vector2 0 0)))
    (object-call st "begin" 3)
    (dotimes (c 4)
      (flet ((pt (i top)
               (let* ((a (+ -1.1 (* c .22) (* i .085))) (r (+ 950 (* c 130) (* 120 (sin (+ (* i .7) c)))))
                      (y (+ 240 (* c 50) (* 60 (sin (+ (* i .9) (* 2 c)))))))
                 (v3 (* r (cos a)) (if top (+ y 260 (* 90 (sin (+ (* i 1.7) c)))) y) (* r (sin a)))))
             (col (i top)
               (let ((edge (sin (* pi (/ i 26.0)))))
                 (if top (color .55 .3 .9 0.0) (color .3 1.0 (+ .5 (* .12 c)) (* .7 edge))))))
        (dotimes (i 26)
          (object-call st "add_triangle_fan" (vector (pt i nil) (pt (1+ i) nil) (pt (1+ i) t) (pt i t)) (vector uv uv uv uv)
                       (vector (col i nil) (col (1+ i) nil) (col (1+ i) t) (col i t))))))
    (object-call st "generate_normals")
    (object-set mi "mesh" (object-call st "commit"))
    (gdext:setf-reference *aurora-mat* (sky-material))
    (object-set mi "material_override" *aurora-mat*)
    (object-set mi "cast_shadow" 0)
    (node-add-child root mi)
    (setf *aurora* mi)))

(defun step-aurora ()
  "Light the aurora at night in clear weather near Dalny.
It is at full strength in the settlement and fades out a kilometre and a half away."
  (when *aurora*
    (let* ((mid (dalny-mid))
           (near (if mid (- 1 (sstep 500 1700 (abs (- *dist* mid)))) 0.0))
           (night (max 0.0 (min 1.0 (/ (- .3 (daylight *day-t*)) .2)))))
      (setf *aurora-k* (if *meta* 0.0 (* near night (- 1 (wx :cloud)))))
      (object-set *aurora* "visible" (> *aurora-k* .02))
      (when (> *aurora-k* .02)
        (destructuring-bind (x y z) *rig-at* (node-3d-set-position *aurora* (v3 x y z)))
        (node-3d-set-rotation *aurora* (v3 0 (* .12 (sin (* *time* .09))) (* .03 (sin (* *time* .23)))))
        (object-set *aurora-mat* "albedo_color" (color 1 1 1 (* *aurora-k* (+ .75 (* .25 (sin (* *time* .8)))))))))))

;;; ------------------------------------------------------------------ the plane

(defun biplane (g)
  "Build a crop-duster biplane in G (nose towards -z); its propeller spins by itself."
  (shape g :cyl '(.7 .5 7.5 10) "#e8e0d0" 0 0 0 :rot (list (/ pi 2) 0 0))
  (shape g :box '(11.0 .14 1.6) "#3c5a48" 0 -.5 -1.2) (shape g :box '(12.0 .14 1.6) "#3c5a48" 0 1.5 -1.4)
  (dolist (x '(-4 4)) (shape g :box '(.1 2.0 .1) "#3a3a40" x .5 -1.3))
  (shape g :box '(3.6 .12 1.2) "#3c5a48" 0 .3 3.6) (shape g :box '(.12 1.6 1.2) "#3c5a48" 0 1.0 3.6)
  (shape g :cyl '(1.3 1.3 .06 10) "#8a8e92" 0 0 -3.9 :rot (list (/ pi 2) 0 0) :alpha .35 :shadow nil)
  (shape g :ball '(.14 6 4) "#ff3020" -5.4 1.5 -1.4 :emit "#ff2010" :energy 4 :shadow nil)
  (shape g :ball '(.14 6 4) "#30ff60" 5.4 1.5 -1.4 :emit "#20ff40" :energy 4 :shadow nil))

(defun launch-plane ()
  "Launch the plane: it takes off ahead on the right, climbs and leaves to the left over the road."
  (unless *plane*
    (let ((g (group *world*)) (from (side-point 260 (+ *dist* 330) 35.0)) (to (side-point -520 (+ *dist* 40) 170.0)))
      (biplane g)
      (setf *plane* (list g *time* from to)))))

(defun step-plane ()
  (let ((mid (dalny-mid)))
    (when (and mid (not *meta*) (not (net-guest-p)) (null *plane*) (< (abs (- *dist* mid)) 2200) (> *time* *next-plane*))
      (launch-plane)))
  (when *plane*
    (destructuring-bind (g t0 from to) *plane*
      (let ((u (/ (- *time* t0) 34.0)))
        (cond ((> u 1.0) (node-queue-free g) (setf *plane* nil *next-plane* (+ *time* 150 (* 140 (vr *time* 3)))))
              (t (flet ((mix (a b) (+ a (* u (- b a)))))
                   (node-3d-set-position g (v3 (mix (first from) (first to)) (mix (second from) (second to)) (mix (third from) (third to))))
                   (node-3d-set-rotation g (v3 .12 (atan (- (first from) (first to)) (- (third from) (third to))) (* .15 (sin (* *time* .6))))))))))))

;;; ------------------------------------------------------------------ airmail

(defun airmail-fee (addr)
  "Return what airmail charges for a parcel to town ADDR: a share of what the recipient would pay."
  (declare (ignorable addr))
  (max *airmail-min* (round (* *airmail-share* (delivery-pay)))))

(defun build-airmail (d)
  "Build the airmail hut by the road at path coordinate D and return its node.
It has a drop-off window, sacks and a little-plane weathervane."
  (let ((g (apply #'group *world* (road-pos d 3.2 -.35))))
    (node-3d-set-rotation g (v3 0 (- (road-frame d)) 0))
    (shape g :box '(12.0 .12 16.0) "#5a5a58" 8.0 .06 0 :shadow nil)
    (shape g :box '(5.0 3.0 6.0) "#55708a" 11.5 1.5 0) (shape g :box '(5.6 .3 6.6) "#e8e0d0" 11.5 3.15 0)
    (shape g :box '(.14 1.0 1.6) "#e8c070" 8.95 1.6 0 :emit "#ffc060" :energy 1.0 :shadow nil)
    (shape g :box '(.6 .08 1.8) "#8a6440" 8.7 1.05 0)
    (shape g :box '(.2 .8 5.4) "#2c4a7a" 8.9 3.75 0 :shadow nil)
    (sign g "АВИАПОЧТА" 8.78 3.75 0 :h .55 :rgb "#f0ece0")
    (loop for i below 4 do (shape g :ball '(.4 8 5) "#a08a5a" 7.2 (+ .25 (* .1 (mod i 2))) (+ -3.4 (* i .75)) :scale '(1 .7 .9)))
    (shape g :box '(.1 5.0 .1) "#8a8a84" 11.5 5.6 0)
    (let ((vane (group g 11.5 8.2 0)))
      (shape vane :box '(1.6 .08 .16) "#a3262c" 0 0 0) (shape vane :box '(.3 .06 1.1) "#a3262c" .1 0 0)
      (moving vane :spin :y .4))
    (flood g 7.5 3.0 0 :range 16.0)
    g))

(defshared send-airmail :airmail ()
  "Hand the parcel in the paws in at the airmail window.
The recipient pays as for a hand-delivered parcel and the post office keeps its fee."
  (if (not (and *carry* (cdr *carry*)))
      (say "Авиапочта: принеси сюда посылку в руках (ЛКМ по ней на полке)")
      (let* ((addr (cdr *carry*)) (fee (airmail-fee addr)) (pay (- (delivery-pay) fee)))
        (incf *money* pay)
        (setf *carry* nil *carry-from* nil)
        (show-carry) (save-interior)
        (award-pennant (parcel-town addr)) (note-quest :post)
        (let ((answer (story-delivered addr t))) (when answer (say-later answer)))
        (log-event :airmail :to addr :fee fee :money *money*)
        (setf *next-plane* 0.0)                                        ; the plane takes off with it right away
        (say-all (format nil "Посылка в ~a улетела самолётом: +~d р. (почта удержала ~d)" (town-name (parcel-town addr)) pay fee)))))

(defun dalny-usables ()
  (let ((place (walk-place)))
    (when (and place (eq (first place) :airmail))
      (list (road-usable 11.9 (second place) 1.3
                         (lambda () (if (and *carry* (cdr *carry*))
                                        (format nil "E — отправить посылку в ~a самолётом: получишь ~d р. вместо ~d" (town-name (parcel-town (cdr *carry*))) (- (delivery-pay) (airmail-fee (cdr *carry*))) (delivery-pay))
                                        "Авиапочта: принеси посылку в руках — улетит сама"))
                         #'send-airmail 3.2 .7)))))
