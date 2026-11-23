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
  (let* ((c *cart*) (f *floor*) (x *half-width*) (z *half-length*) (wh *wall-height*))
    ;; the body is drawn in Blender (models/kibitka-body.blend): the underbody, the walls with the window (x+) and
    ;; the door (z+) openings, the golden battens, the roof with its lining and end panels, the floor.
    ;; The wheels belong to the truck (doroga.lisp); what moves or glows is built below and in the other files
    (model c "kibitka-body")
    (shape c :ball '(.28 12 6) "#ffd890" 0 (+ f wh .45) (- (- z) .05) :emit "#ffb050" :energy 2 :scale '(1 1 .2))
    ;; inside: the carpet on the floor, the lining
    (model c "small-things")
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
