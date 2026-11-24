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

(defparameter *summer-sky-keys*
  '((0 "#16204a" "#5a4a70") (45 "#4f9ae6" "#ffcdb4") (70 "#4f9ae6" "#ffd8c2") (120 "#2a82ea" "#b6e2fb") (350 "#1560d8" "#b6e2fb")
    (660 "#1c68e0" "#c2e2f6") (722 "#2a5cc4" "#ffb04a") (765 "#22367f" "#e8703a") (815 "#081238" "#27508c") (960 "#050c26" "#1c3a6a")
    (1140 "#060d28" "#1a2c5a") (1200 "#16204a" "#5a4a70"))
  "The sky keys of the summer behind the tunnel: the same hours, but a clean sky - deep blue by day, a bright band at dusk.")

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

(defvar *fog-inside* nil "How thick the air is in the megastructure where we are, or NIL outside it: a great hall is seen
from end to end only in clear air (mega.lisp sets it by the zone).")
(defvar *sky-hour* nil "The second of the day cycle the sky is to show whatever the clock says, or NIL: in the summer
behind the tunnel the hour belongs to the place, and the clock of the game stands still (leto.lisp).")
(defvar *sun-turn* 0.0 "How far the sun and the moon are turned about the wagon, in radians: behind a door the way bends
and the place is turned about us, and the lights of the sky turn with it (mega.lisp).")
(defun sky-now () "Return the second of the day cycle the sky shows." (or *sky-hour* *day-t*))

(defvar *haze* 0.0 "How much pale haze stands in the megastructure where we are, from 0 (pitch dark) to 1: the cold
sourceless light in which far things show as dark shapes (marks.lisp sets it by the zone).")

(defun daylight (tt)
  "Return the amount of daylight at time TT: 1 is day, 0 is night. Inside the megastructure there is none."
  (let ((tt (day-phase tt)))
    (* (- 1.0 *gloom*)
       (if (< tt (second *dusk*)) (* (sstep 0 80 tt) (- 1 (sstep (first *dusk*) (second *dusk*) tt))) 0))))

(defun sky-colors (tt)
  (let* ((tt (day-phase tt))
         (keys (if *sky-hour* *summer-sky-keys* *sky-keys*))
         (i (position-if (lambda (k) (> (first k) tt)) keys))
         (a (nth (1- i) keys)) (b (nth i keys))
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
  ;; 110 m on a 4096 map: a point of the shadow is 5 cm (at 70 m on 2048 it was 7, and the shadow of the moving wagon
  ;; jumped from point to point). At 50 m the shadows of the houses ended in a line across the street right ahead
  (object-set *sun* "directional_shadow_max_distance" 110.0)
  (object-set *sun* "directional_shadow_fade_start" .55)                ; shadows thin out from 60 m on, not cut off
  (object-set *sun* "shadow_blur" 1.6)                                  ; with the soft edges of project.godot: the edge of a shadow does not crawl
  (object-set *sun* "shadow_bias" .1)           ; lower and shadow ripple flickers on far slopes;
  (object-set *sun* "shadow_normal_bias" 1.0)   ; the wagon's gaps are closed by geometry, no light leaks
  (node-add-child (or *turn* root) *sun*)                ; lights go under Turn: on bends the shadows and sun shift
  (setf *moon-light* (gdext:make-object "DirectionalLight3D"))
  (object-set *moon-light* "light_color" (hex "#a8b4d8"))
  (object-set *moon-light* "sky_mode" 1)          ; light only, no disc in the sky
  (node-add-child (or *turn* root) *moon-light*)
  ;; the moon hangs beyond the mountains, not over the road: 1.8 km away, just inside the star sphere (stars.lisp), so hills, houses and
  ;; the tunnel mouth hide it as they would; the haze does not touch it, or it would drown at that distance
  (setf *moon* (shape (or *turn* root) :ball '(40 20 12) "#f0ecdc" 0 0 0 :emit "#e8e4d0" :energy 1.6 :shadow nil :unshaded t))
  (object-set (object-get *moon* "material_override") "disable_fog" t))

(defun step-sky (delta)
  (when (place-rule :clock) (incf *day-t* (* delta *time-scale*)))      ; where the clock stands, the calendar does not turn
  (let* ((day (daylight (sky-now))) (ph (day-phase (sky-now)))
         ;; the lights move in steps of ~0.2 degrees, not every frame: rotating a light shifts the shadow map,
         ;; and the shadow edges ripple
         (sun-a (sun-step (* pi (/ ph (second *dusk*))))) (sun-up (<= sun-a pi))
         (moon-a (sun-step (* pi (/ (mod (- ph (second *dusk*)) *day-len*) (- *day-len* (second *dusk*)))))))
    (multiple-value-bind (top horizon) (sky-colors (sky-now))
      (let ((gray (if *sky-hour* (hex "#2e2b4c") (mix-color (hex "#8a9098") (hex "#202428") (- 1 day))))      ; the storm of the summer is lilac-black
            (overcast (* (if *sky-hour* .85 .6) (wx :cloud))))
        (setf top (mix-color top gray overcast) horizon (mix-color horizon gray (* .8 overcast))))
      (when (plusp *gloom*)                                    ; inside: no sky at all, only the haze, the same above and around
        (let ((haze (mix-color (hex "#000000") (hex "#5d7984") (* .8 *haze*))))
          (setf top (mix-color top haze *gloom*) horizon (mix-color horizon haze *gloom*))))
      (object-set *sky-mat* "sky_top_color" top)
      (object-set *sky-mat* "sky_horizon_color" horizon)
      (object-set *sky-mat* "ground_horizon_color" horizon)
      (object-set *sky-mat* "ground_bottom_color" (mix-color horizon (hex "#101010") .6))
      (object-set *env* "fog_light_color" horizon))
    ;; the sun rises on the left (x-) and sets on the window side (x+). A light shines along the node's -Z:
    ;; the downward tilt is the light's elevation, the turn goes from -pi/2 (shining toward x+) to pi/2
    (flet ((elev (a) (if (< a (/ pi 2)) a (- pi a))))
      (node-3d-set-rotation *sun* (v3 (- (max .03 (elev (min sun-a pi)))) (+ (- (min sun-a pi) (/ pi 2)) (sun-step *sun-turn*)) 0))
      (node-3d-set-rotation *moon-light* (v3 (- (max .15 (elev moon-a))) (+ (- moon-a (/ pi 2)) (sun-step *sun-turn*)) 0)))
    (object-set *sun* "light_energy" (if sun-up (* (- 1.0 *gloom*) (+ .15 (* 1.4 day)) (- 1 (* .7 (wx :cloud)))) 0.0))
    (object-set *env* "fog_sky_affect" (if *sky-hour* 0.0 .25))            ; in the summer the haze lies on the land and leaves the sky clean
    (let ((fog (+ (if *sky-hour* .0007 *fog-base*) (* .012 (wx :fog)) (* .004 (wx :rain)) (* .006 (wx :snow)) (* .0022 *gloom*))))
      (object-set *env* "fog_density" (if *fog-inside* (+ (* (- 1.0 *gloom*) fog) (* *gloom* *fog-inside*)) fog)))
    (object-set *sun* "light_color" (mix-color (hex "#ff9a60") (hex "#fff4e0") day))
    (object-set *sun* "visible" sun-up)
    (object-set *moon-light* "light_energy" (if sun-up 0.0 (* (- 1.0 *gloom*) .35 (- 1 (* .7 (wx :cloud))))))
    (object-set *moon* "visible" (and (not sun-up) (< (wx :cloud) .6) (< *gloom* .3)))
    (destructuring-bind (x y z) *rig-at*                   ; the moon circles the wagon wherever it has driven
      (node-3d-set-position *moon* (v3 (+ x (* -1700 (cos moon-a))) (+ y (- (* 970 (sin moon-a)) 50)) (- z 600))))
    ;; lights off (L): almost no ambient light, so you can see where the street light breaks in
    (object-set *env* "ambient_light_energy" (if *lights-on*
                                                    (+ (* (- 1.0 *gloom*) (+ .35 (* .65 day))) (* *gloom* (+ .02 (* .2 *haze*))))
                                                    .0))
    (object-set *env* "reflected_light_source" (if *lights-on* 0 1))))   ; REFLECTION_SOURCE_DISABLED: no sky glint
