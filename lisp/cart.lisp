;;;; The wagon body: walls, roof, floor, the carpet on the left wall, the lamp and the fairy lights. It sits
;;;; at the origin: x+ is the wall with the big window, z− is the front (the truck cab is behind it, doroga.lisp),
;;;; z+ is the back wall with the door (door.lisp). Everything that is used inside lives in byt.lisp.
;;;; Sizes and colours are parameters: change them in the REPL, then (live:rebuild-cart).

(in-package #:kiosk-trip)

(defparameter *floor* .95 "The height of the wagon floor.")
(defparameter *half-width* 1.55)
(defparameter *half-length* 2.85)
(defparameter *wall-height* 2.3)
(defparameter *kc*
  '(:body "#7a2a24" :trim "#d8a840" :roof "#2a4a3e" :roof2 "#2e5244" :wood "#8a6440" :dark "#4a3020"
    :floor "#a07a50" :cloth "#b84a34" :inside "#c89a6a" :carpet "#8a1a22" :carpet2 "#e0b050" :carpet3 "#2a3a7a")
  "The wagon colours.")
(defvar *cam-course* 0.0 "The course the outside camera looks along; it catches up with the road course smoothly.")
(defparameter *sway* 1.0 "The strength of the wagon sway (0 means no sway).")
(defparameter *fairy* '("#ff6060" "#ffd060" "#60ff90" "#60c0ff" "#d070ff"))

(defvar *cart* nil "The Node3D of the wagon.")
(defvar *bulbs* '())
(defvar *glow* '() "The lamp and its light; they are switched off together with the fairy lights (L).")
(defvar *lights-on* t)
(defvar *meta* nil "True while we are in the meta room (meta.lisp): the wagon and the world are hidden and nothing sways.")

(defun kc (key) (getf *kc* key))

(defun wall (cart side along from to holes)
  "Build a wall of CART with the openings HOLES, a list of (a0 a1 y0 y1).
ALONG is :z for a side wall and :x for an end wall."
  ;; the walls go up under the roof without a gap: the roof is a half-ellipse wider than the walls, and above
  ;; a side wall it is ~0.5 m higher than the wall top — side walls are stretched up to it, ends are closed by panels
  (let* ((f *floor*) (rx (+ *half-width* .25))
         (wh (+ *wall-height* (if (eq along :z) (+ (* 1.05 (sqrt (max 0 (- 1 (expt (/ (+ *half-width* .08) rx) 2))))) .08) .14))))
    (flet ((put (a0 a1 y0 y1)
             (when (and (> (- a1 a0) .01) (> (- y1 y0) .01))
               (let ((len (- a1 a0)) (mid (/ (+ a0 a1) 2)) (y (+ f (/ (+ y0 y1) 2))))
                 (if (eq along :z)
                     (shape cart :box (list .16 (- y1 y0) (+ len .02)) (kc :body) side y mid)
                     (shape cart :box (list (+ len .02) (- y1 y0) .16) (kc :body) mid y side))))))
      (let ((a from))
        (dolist (h (sort (copy-list holes) #'< :key #'first))
          (destructuring-bind (h0 h1 y0 y1) h
            (put a h0 0 wh) (put h0 h1 0 y0) (put h0 h1 y1 wh) (setf a h1)))
        (put a to 0 wh)))))

;;; The carpet on the wall is a real one, pixelated (tools/make-carpet.py): one pixel is one knot.

(defvar *carpet-mat* nil "The material with the picture of the wall carpet.")

(defun wall-carpet (parent x y z height)
  "Hang the pictured carpet of height HEIGHT under PARENT, centred at (X Y Z) and facing x+.
Its width follows the picture. Return the mesh instance, or NIL when the picture is missing."
  (let ((path "res://textures/murals/carpet.png"))
    (when (picture-here-p path)
      (let ((img (gdext:make-object "Image")) (mi (gdext:make-object "MeshInstance3D")) (mesh (gdext:make-object "QuadMesh")))
        (read-picture img path)
        (unless *carpet-mat*
          (let ((tx (gdext:make-object "ImageTexture")) (m (gdext:make-object "StandardMaterial3D")))
            (object-call tx "set_image" img)
            (object-set m "albedo_texture" tx)
            (object-set m "texture_filter" 0)                    ; unfiltered: the pixels stay square
            (object-set m "roughness" 1.0)
            (object-set m "metallic_specular" .05)
            (gdext:setf-reference *carpet-mat* m)))
        (object-set mesh "size" (vector2 (/ (* height (object-call img "get_width")) (object-call img "get_height")) height))
        (object-set mi "mesh" mesh)
        (object-set mi "material_override" *carpet-mat*)
        (object-set mi "cast_shadow" 0)
        (node-add-child parent mi)
        (node-3d-set-position mi (v3 x y z))
        (node-3d-set-rotation mi (v3 0 (/ pi 2) 0))
        mi))))

(defun make-cart (root)
  (when *cart* (node-queue-free *cart*))
  (setf *cart* (group root) *bulbs* '() *glow* '())
  (let* ((c *cart*) (f *floor*) (x *half-width*) (z *half-length*) (wh *wall-height*)
         (win '(-1.3 1.3 .95 2.05)))                             ; the big window opening: z from and to, height from and to
    ;; the underbody (the wheels belong to the truck, doroga.lisp)
    (shape c :box (list 3.3 .25 6) (kc :dark) 0 (- f .14) 0)
    ;; walls: the big window (x+), blank with the carpet (x−), blank with the shelving (z−), with the door (z+)
    (wall c x :z (- z) z (list win))
    (wall c (- x) :z (- z) z '())
    (wall c (- z) :x (- x) x '())
    (wall c z :x (- x) x '((-.5 .5 0 1.95)))
    ;; golden battens
    (dolist (sx '(-1 1)) (dolist (sz '(-1 1)) (shape c :box (list .14 (+ wh .1) .14) (kc :trim) (* sx x) (+ f (/ wh 2)) (* sz z))))
    (dolist (sx '(-1 1))
      (shape c :box (list .06 .1 (* 2 z)) (kc :trim) (* sx (+ x .05)) (+ f .05) 0)
      (shape c :box (list .06 .1 (* 2 z)) (kc :trim) (* sx (+ x .05)) (+ f wh) 0))
    ;; the wagon roof: planks along a half-ellipse, green outside, light wood inside
    (let* ((rx (+ x .25)) (ry 1.05) (n 14))
      (flet ((arc (a) (list (* (cos a) rx) (+ f wh (* (sin a) ry)))))
        (dotimes (i n)
          (destructuring-bind (x0 y0) (arc (* i (/ pi n)))
            (destructuring-bind (x1 y1) (arc (* (1+ i) (/ pi n)))
              (let ((len (+ (sqrt (+ (expt (- x1 x0) 2) (expt (- y1 y0) 2))) .02))
                    (mx (/ (+ x0 x1) 2)) (my (/ (+ y0 y1) 2)) (ang (atan (- y1 y0) (- x1 x0))))
                ;; the roof planks are thick and have a lining that casts a shadow too: a thin roof lets the sun through
                (shape c :box (list (+ len .04) .2 (+ (* 2 z) .5)) (if (oddp i) (kc :roof) (kc :roof2)) mx my 0 :rot (list 0 0 ang))
                (shape c :box (list (+ len .03) .04 (- (* 2 z) .1)) (kc :inside) (* mx .95) (- my .14) 0 :rot (list 0 0 ang)))))))
      ;; the end panels under the roof
      (dolist (sz '(-1 1))
        (dolist (layer (list (list (kc :body) (* sz (+ z .06))) (list (kc :inside) (* sz (- z .06)))))   ; paint outside, lining inside
          (let ((mi (gdext:make-object "MeshInstance3D")))
            (object-set mi "mesh" (half-ellipse-mesh (+ rx .02) (+ ry .02)))
            (object-set mi "material_override" (material (first layer) :double t))
            (node-add-child c mi)
            (node-3d-set-position mi (v3 0 (+ f wh -.02) (second layer)))))))
    (shape c :ball '(.28 12 6) "#ffd890" 0 (+ f wh .45) (- (- z) .05) :emit "#ffb050" :energy 2 :scale '(1 1 .2))
    ;; inside: the floor, the carpet on the floor, the lining
    (shape c :box (list (- (* 2 x) .1) .04 (- (* 2 z) .1)) (kc :floor) 0 (+ f .02) 0)
    (shape c :box '(1.8 .02 2.8) "#2a3a6a" -.15 (+ f .05) 0)
    (shape c :box '(1.5 .025 2.5) "#d8a840" -.15 (+ f .055) 0)
    (shape c :box '(1.3 .03 2.3) "#2a3a6a" -.15 (+ f .06) 0)
    ;; the carpet wall: the layers go strictly one after another from the wall into the room, with no
    ;; coinciding planes (otherwise they flicker): the wall's inner face at −x+0.08 → lining → carpet → border → centre
    (let ((w0 (+ (- x) .08)))
      (flet ((layer (n) (+ w0 (* n .012))))
        (shape c :box (list .01 (- wh .1) (- (* 2 z) .2)) (kc :inside) (layer .5) (+ f (/ wh 2)) 0 :shadow nil)
        ;; behind the bunks, hung low so the photo cards above it stay in sight; without the picture, the plain carpet as before
        (unless (wall-carpet c (layer 2.5) (+ f 1.0) -1.2 1.7)
          (shape c :box '(.01 1.7 2.6) (kc :carpet) (layer 1.5) (+ f 1.25) 0)
          (shape c :box '(.01 1.5 2.4) (kc :carpet2) (layer 2.5) (+ f 1.25) 0)
          (shape c :box '(.01 1.36 2.26) (kc :carpet) (layer 3.5) (+ f 1.25) 0))))
    ;; the lamp, the light, the fairy lights
    (shape c :cyl '(.008 .008 .7 4) "#2a2a2e" 0 (+ f wh .7) 0 :shadow nil)                        ; the cord from the roof
    (shape c :cyl '(.07 .27 .2 14) "#c8643a" 0 (+ f wh .26) 0 :shadow nil)                        ; a cloth shade
    (shape c :cyl '(.28 .28 .03 14) "#e0b050" 0 (+ f wh .165) 0 :shadow nil)                      ; its trim
    (push (shape c :ball '(.09 10 6) "#ffe8b0" 0 (+ f wh .11) 0 :emit "#ffc070" :energy 3 :shadow nil) *glow*)
    (push (light c "#ffb070" 4.2 3.7 0 (+ f wh .1) 0) *glow*)       ; a short reach: lights cast no shadows here, and a longer one lit the ground under the wagon through its floor
    (push (light c "#ff8060" 1.2 4 -.6 (+ f 1.4) -.2) *glow*)
    (dolist (sx '(-1 1))
      (dotimes (i 22)
        (let ((col (nth (mod i 5) *fairy*)))
          (push (shape c :ball '(.035 6 4) col (* sx (- x .15)) (- (+ f wh) .08 (* .06 (sin (* (/ i 21) pi 3)))) (+ (- z) .2 (* i .25))
                       :emit col :energy 4 :shadow nil)
                *bulbs*))))
    (make-door c)
    (apply-lights)
    c))

(defun half-ellipse-mesh (rx ry &optional (n 24))
  "Return a half-ellipse mesh (a triangle fan) in the XY plane: the panel under the roof."
  (let ((st (gdext:make-object "SurfaceTool")))
    (object-call st "begin" 3)
    (dotimes (i n)
      (let ((a0 (* i (/ pi n))) (a1 (* (1+ i) (/ pi n))))
        (object-call st "add_vertex" (v3 0 0 0))
        (object-call st "add_vertex" (v3 (* rx (cos a0)) (* ry (sin a0)) 0))
        (object-call st "add_vertex" (v3 (* rx (cos a1)) (* ry (sin a1)) 0))))
    (object-call st "generate_normals")
    (object-call st "commit")))

(defun apply-lights ()
  "Show or hide the light inside (the lamp, the glow, the fairy lights) according to *LIGHTS-ON*."
  (dolist (n (append *glow* *bulbs*)) (object-set n "visible" *lights-on*)))

(defun step-cart (delta time)
  "Put the Rig (the wagon with the camera) on the road.
From the inside it sways together with the camera, slowly: fast shaking makes the window edges jerk against
the landscape. From the outside the camera neither sways nor tilts: the horizon stays level, it follows a
turn softly, and the slope and the turn are played out by the truck itself; otherwise it causes motion
sickness. In the meta room we do not drive: no sway, no slope, no turn."
  (let* ((out (outside-p)) (afoot (anyone-afoot-p))
         (sway (if (or *meta* out afoot (eq *view* :wait)) 0.0 *sway*)))
    ;; the course is now a real azimuth on the map: we approach it by the shortest turn, not through a full circle
    (incf *cam-course* (* (- (mod (+ (- *course* *cam-course*) pi) (* 2 pi)) pi) (if out (min 1.0 (* delta 1.6)) 1.0)))
    (destructuring-bind (x y z) *rig-at*
      (node-3d-set-position *rig* (v3 x (+ y (* sway .015 (sin (* time .9)))) z)))
    (cond (*meta* (node-3d-set-rotation *rig* (v3 0 0 0)))
          (out (node-3d-set-rotation *rig* (v3 0 (- *cam-course*) 0))
               (node-3d-set-rotation *cart* (v3 *slope* (- *cam-course* *course*) 0)))
          ;; someone is walking outside: the wagon stands level, with no sway and no tilt — otherwise a hundred
          ;; metres away from it a hundredth of a degree becomes a metre, and the world 'drifts'
          (afoot (node-3d-set-rotation *rig* (v3 0 (- *course*) 0))
                 (node-3d-set-rotation *cart* (v3 0 0 0)))
          (t (node-3d-set-rotation *rig* (v3 *slope* (- *course*) (* sway .01 (sin (* time 1.3)))))
             (unless (eq *view* :wait) (node-3d-set-rotation *cart* (v3 0 0 0)))))))
