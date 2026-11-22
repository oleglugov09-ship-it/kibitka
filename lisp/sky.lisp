;;;; A day cycle like in Minecraft, 20 minutes: dawn 1.5, day 10, dusk 1.5, night 7.
;;;; *DAY-T* is seconds since the start of dawn. The sky is a ProceduralSkyMaterial (top
;;;; and horizon colors by keys), the sun is a DirectionalLight3D (the sky draws its
;;;; disc), at night the moon shines: a second directional light and a ball in the sky. Fog and
;;;; ambient light follow the sky. Weather (climate.lisp) greys the sky, dims the
;;;; sun and thickens the fog.

(in-package #:kiosk-trip)

(defparameter *day-len* 1200.0)
(defparameter *dusk* '(690.0 780.0) "The dusk: its start and end, in seconds of the day cycle.")
(defparameter *fog-base* .0025 "The haze density in clear weather.")
(defparameter *time-scale* 1.0 "The factor by which the day cycle runs faster (live:time-scale).")
(defparameter *sky-keys*
  '((0 "#1c1a34" "#3a2a40") (45 "#5a7ab0" "#f0a070") (110 "#3a78d8" "#a8d0f0") (660 "#3a78d8" "#a8d0f0")
    (720 "#4a5aa0" "#f08a50") (760 "#2a2a5a" "#a04a50") (810 "#0a1028" "#1a2040") (1140 "#080c20" "#141a34")
    (1200 "#1c1a34" "#3a2a40"))
  "The sky keys: a list of (second top-color horizon-color).")

(defvar *day-t* 650.0)
(defvar *env* nil "The Environment resource of the scene.")
(defvar *sky-mat* nil "The ProceduralSkyMaterial of the sky.")
(defvar *sun* nil)
(defvar *moon-light* nil)
(defvar *moon* nil)

(defun day-phase (tt) (mod tt *day-len*))

(defparameter *sun-step* .004 "The step of the sun and moon across the sky, in radians.")
(defun sun-step (a) (* *sun-step* (round a *sun-step*)))

(defvar *gloom* 0.0 "How deep inside we are, from 0 in the open to 1 in the tunnel and the megastructure beyond it:
there the day does not reach, whatever the hour (marks.lisp sets it every frame).")

(defun daylight (tt)
  "Return the amount of daylight at time TT: 1 is day, 0 is night. Inside the megastructure there is none."
  (let ((tt (day-phase tt)))
    (* (- 1.0 *gloom*)
       (if (< tt (second *dusk*)) (* (sstep 0 80 tt) (- 1 (sstep (first *dusk*) (second *dusk*) tt))) 0))))

(defun sky-colors (tt)
  (let* ((tt (day-phase tt))
         (i (position-if (lambda (k) (> (first k) tt)) *sky-keys*))
         (a (nth (1- i) *sky-keys*)) (b (nth i *sky-keys*))
         (f (/ (- tt (first a)) (- (first b) (first a)))))
    (values (mix-color (hex (second a)) (hex (second b)) f) (mix-color (hex (third a)) (hex (third b)) f))))

(defun make-sky (root)
  (let ((we (gdext:make-object "WorldEnvironment"))
        (env (gdext:make-object "Environment"))
        (sky (gdext:make-object "Sky"))
        (mat (gdext:make-object "ProceduralSkyMaterial")))
    (object-set sky "sky_material" mat)
    (object-set env "background_mode" 2)          ; BG_SKY
    (object-set env "sky" sky)
    (object-set env "ambient_light_source" 3)     ; AMBIENT_SOURCE_SKY
    (object-set env "tonemap_mode" 2)             ; filmic
    (object-set env "fog_enabled" t)
    (object-set env "fog_density" .0025)
    (object-set env "fog_sky_affect" .25)
    (object-set env "glow_enabled" t)
    (object-set env "glow_hdr_threshold" 2.5)     ; lamps and garlands glow, not lit snow and
                                                  ; slopes: their halos jitter along the horizon in motion
    (object-set we "environment" env)
    (node-add-child root we)
    (gdext:setf-reference *env* env)
    (gdext:setf-reference *sky-mat* mat))
  (setf *sun* (gdext:make-object "DirectionalLight3D"))
  (object-set *sun* "shadow_enabled" t)
  ;; shadows: one cascade and only up close: far shadows are invisible in the haze, and drawing them
  ;; costs three times as many draw calls
  (object-set *sun* "directional_shadow_mode" 0)
  (object-set *sun* "directional_shadow_max_distance" 70.0)
  (object-set *sun* "shadow_bias" .1)           ; lower and shadow ripple flickers on far slopes;
  (object-set *sun* "shadow_normal_bias" 1.0)   ; the wagon's gaps are closed by geometry, no light leaks
  (node-add-child (or *turn* root) *sun*)                ; lights go under Turn: on bends the shadows and sun shift
  (setf *moon-light* (gdext:make-object "DirectionalLight3D"))
  (object-set *moon-light* "light_color" (hex "#a8b4d8"))
  (object-set *moon-light* "sky_mode" 1)          ; light only, no disc in the sky
  (node-add-child (or *turn* root) *moon-light*)
  (setf *moon* (shape (or *turn* root) :ball '(6 16 10) "#f0ecdc" 0 0 0 :emit "#e8e4d0" :energy 1.6 :shadow nil :unshaded t)))

(defun step-sky (delta)
  (incf *day-t* (* delta *time-scale*))
  (let* ((day (daylight *day-t*)) (ph (day-phase *day-t*))
         ;; the lights move in steps of ~0.2 degrees, not every frame: rotating a light shifts the shadow map,
         ;; and the shadow edges ripple
         (sun-a (sun-step (* pi (/ ph (second *dusk*))))) (sun-up (<= sun-a pi))
         (moon-a (sun-step (* pi (/ (mod (- ph (second *dusk*)) *day-len*) (- *day-len* (second *dusk*)))))))
    (multiple-value-bind (top horizon) (sky-colors *day-t*)
      (let ((gray (mix-color (hex "#8a9098") (hex "#202428") (- 1 day))) (overcast (* .6 (wx :cloud))))
        (setf top (mix-color top gray overcast) horizon (mix-color horizon gray (* .8 overcast))))
      (when (plusp *gloom*)                                    ; inside: no sky at all
        (setf top (mix-color top (hex "#000000") *gloom*) horizon (mix-color horizon (hex "#000000") *gloom*)))
      (object-set *sky-mat* "sky_top_color" top)
      (object-set *sky-mat* "sky_horizon_color" horizon)
      (object-set *sky-mat* "ground_horizon_color" horizon)
      (object-set *sky-mat* "ground_bottom_color" (mix-color horizon (hex "#101010") .6))
      (object-set *env* "fog_light_color" horizon))
    ;; the sun rises on the left (x-) and sets on the window side (x+). A light shines along the node's -Z:
    ;; the downward tilt is the light's elevation, the turn goes from -pi/2 (shining toward x+) to pi/2
    (flet ((elev (a) (if (< a (/ pi 2)) a (- pi a))))
      (node-3d-set-rotation *sun* (v3 (- (max .03 (elev (min sun-a pi)))) (- (min sun-a pi) (/ pi 2)) 0))
      (node-3d-set-rotation *moon-light* (v3 (- (max .15 (elev moon-a))) (- moon-a (/ pi 2)) 0)))
    (object-set *sun* "light_energy" (if sun-up (* (- 1.0 *gloom*) (+ .15 (* 1.4 day)) (- 1 (* .7 (wx :cloud)))) 0.0))
    (object-set *env* "fog_density" (+ *fog-base* (* .012 (wx :fog)) (* .004 (wx :rain)) (* .006 (wx :snow)) (* .003 *gloom*)))
    (object-set *sun* "light_color" (mix-color (hex "#ff9a60") (hex "#fff4e0") day))
    (object-set *sun* "visible" sun-up)
    (object-set *moon-light* "light_energy" (if sun-up 0.0 (* (- 1.0 *gloom*) .35 (- 1 (* .7 (wx :cloud))))))
    (object-set *moon* "visible" (and (not sun-up) (< (wx :cloud) .6) (< *gloom* .3)))
    (destructuring-bind (x y z) *rig-at*                   ; the moon circles the wagon wherever it has driven
      (node-3d-set-position *moon* (v3 (+ x (* -140 (cos moon-a))) (+ y (- (* 80 (sin moon-a)) 4)) (- z 50))))
    ;; lights off (L): almost no ambient light, so you can see where the street light breaks in
    (object-set *env* "ambient_light_energy" (if *lights-on* (* (- 1.0 (* .97 *gloom*)) (+ .35 (* .65 day))) .0))
    (object-set *env* "reflected_light_source" (if *lights-on* 0 1))))   ; REFLECTION_SOURCE_DISABLED: no sky glint
