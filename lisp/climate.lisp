;;;; Seasons and weather: they change rarely, but they do.
;;;;
;;;; A season lasts *SEASON-DAYS* day cycles and they follow each other in a circle; a new trip
;;;; starts with a random one. It colours whatever gets built ahead: grass, tree crowns
;;;; (blossom in spring, yellow in autumn, bare in winter), flowers, water (ice in winter). The change
;;;; arrives gradually: what is already built rolls away behind, the new is built the new way.
;;;;
;;;; Weather (clear, cloudy, rain, fog, snow in winter instead of rain) holds for
;;;; a random *WEATHER-MINUTES* minutes and blends smoothly into the next one: clouds in
;;;; the sky, a dimmer sun, thicker fog, rain or snow around the wagon, the sound of rain.

(in-package #:kiosk-trip)

(defparameter *seasons* '(:spring :summer :autumn :winter))
(defparameter *season-names* '(:spring "весна" :summer "лето" :autumn "осень" :winter "зима"))
(defparameter *season-days* 2 "The number of day cycles a season lasts.")
(defparameter *weather-minutes* '(4 12) "The range of minutes a weather state holds, as (MIN MAX).")
(defparameter *weather-change* 30.0 "The number of seconds it takes the weather to blend into the next one.")
;; overcast and foggy more often than clear
(defparameter *weathers*
  '((:clear 2 .25 0 0 .1) (:cloudy 5 .85 0 0 .25) (:rain 2 .95 1 0 .4) (:fog 3 .5 0 0 1) (:snow 2 .85 0 1 .45))
  "The weather table: entries of (kind weight clouds rain snow fog).")
(defparameter *weather-names* '(:clear "ясно" :cloudy "облачно" :rain "дождь" :fog "туман" :snow "снег"))

(defvar *season-clock* 0.0 "The seconds elapsed since the season began.")
(defvar *weather-left* 300.0 "The seconds the current weather will still hold.")
(defvar *rain-node* nil)
(defvar *wx-force* nil "The weather to aim at whatever the forecast, as a plist like *WX*, or NIL: the summer behind the tunnel has its own.")
(defvar *precip-on* t "Whether rain and snow are drawn: a player's own setting (the pause menu), for a slow machine.")
(defvar *snow-node* nil)

;;; Season: colours

(defun season-grass (c)
  "Return the grass colour C tinted for the current season."
  (case *season*
    (:spring (mix-color c (hex "#6aa03a") .25))
    (:autumn (mix-color c (hex "#9a6a2a") .45))
    (:winter (mix-color c (hex "#e8eef4") .85))
    (t c)))

(defun season-crown (rgb)
  "Return the crown colour of a deciduous tree with base colour RGB for the current season.
Return NIL when the tree is bare (winter)."
  (case *season*
    (:spring (if (< (random 1.0) .35) (nth (random 2) '("#f0b8c8" "#f4f0e8")) "#7ac050"))
    (:autumn (nth (random 4) '("#d86a20" "#c84020" "#e0a030" "#b89030")))
    (:winter nil)
    (t rgb)))

(defun season-conifer (rgb)
  "Return the conifer needle colour RGB for the current season: under snow in winter."
  (if (eq *season* :winter) (mix-hex rgb "#e8eef4" .55) rgb))

(defun season-prop (kind)
  "Return the roadside prop KIND adjusted for the current season.
In winter flowers, mushrooms and pumpkins become snowdrifts; pumpkins appear only in autumn."
  (case *season*
    (:winter (if (member kind '(:flowers :mush :pumpkin)) :drift kind))
    ((:spring :summer) (if (eq kind :pumpkin) :hay kind))
    (t kind)))

(defun mix-hex (a b tt)
  "Return the blend of the two '#rrggbb' colours A and B at ratio TT as a '#rrggbb' string.
The string form is for materials, which are cached by colour."
  (let ((c (mix-color (hex a) (hex b) tt)))
    (format nil "#~2,'0x~2,'0x~2,'0x" (round (* 255 (color-r c))) (round (* 255 (color-g c))) (round (* 255 (color-b c))))))

(defun apply-season ()
  "Apply the current season to the water: in winter it is ice."
  (let ((m (water-material)) (ice (eq *season* :winter)))
    (object-set m "albedo_color" (hex (if ice "#b8d0e0" *water-rgb*)))
    (object-set m "roughness" (if ice .5 .3))
    (object-set m "emission_enabled" (not ice))))

(defun set-season (season)
  (setf *season* season *season-clock* 0.0)
  (apply-season)
  season)

;;; Weather

(defun weather-target (k)
  (let ((spec (assoc *weather* *weathers*)))
    (nth (position k '(:cloud :rain :snow :fog)) (cddr spec))))

(defun pick-weather ()
  "Return a random weather kind picked by weight.
In winter snow replaces rain; in the other seasons there is no snow."
  (let* ((ok (remove-if (lambda (w) (member (first w) (if (eq *season* :winter) '(:rain) '(:snow)))) *weathers*))
         (x (random (float (reduce #'+ ok :key #'second)))))
    (loop for w in ok do (decf x (second w)) (when (<= x 0) (return (first w))) finally (return :clear))))

(defun set-weather (weather &optional minutes)
  (setf *weather* weather
        *weather-left* (* 60 (or minutes (+ (first *weather-minutes*) (random (float (- (second *weather-minutes*) (first *weather-minutes*))))))))
  weather)

(defun particles (parent size rgb billboard amount lifetime speed spread &key turbulence)
  "Build the precipitation emitter around the wagon as a child of PARENT and return it.
It emits in a ring from 3.6 to 30 m, falling straight down, so nothing lands inside the wagon.
A particle is a flat quad of SIZE (x y) in colour RGB turned to the camera (BILLBOARD is the
StandardMaterial mode); AMOUNT, LIFETIME, SPEED, SPREAD and TURBULENCE set the count and motion."
  (let ((p (gdext:make-object "GPUParticles3D")) (pm (gdext:make-object "ParticleProcessMaterial"))
        (mesh (gdext:make-object "QuadMesh")) (mat (gdext:make-object "StandardMaterial3D")))
    (object-set pm "emission_shape" 6)                         ; EMISSION_SHAPE_RING
    (object-set pm "emission_ring_axis" (v3 0 1 0))
    (object-set pm "emission_ring_height" 1.0)
    (object-set pm "emission_ring_radius" 30.0)
    (object-set pm "emission_ring_inner_radius" 3.6)
    (object-set pm "direction" (v3 0 -1 0))
    (object-set pm "spread" spread)
    (object-set pm "initial_velocity_min" (* speed .85))
    (object-set pm "initial_velocity_max" speed)
    (object-set pm "gravity" (v3 0 0 0))
    (when turbulence
      (object-set pm "turbulence_enabled" t)
      (object-set pm "turbulence_noise_strength" turbulence))
    (object-set p "process_material" pm)
    (object-set mesh "size" (vector2 (first size) (second size)))
    (object-set mat "albedo_color" (hex rgb .55))
    (object-set mat "transparency" 1)
    (object-set mat "shading_mode" 0)
    (object-set mat "billboard_mode" billboard)
    (object-set mesh "material" mat)
    (object-set p "draw_pass_1" mesh)
    (object-set p "amount" amount)
    (object-set p "lifetime" lifetime)
    (object-set p "preprocess" lifetime)
    (object-set p "visibility_aabb" (aabb (v3 -32 -20 -32) (v3 64 40 64)))
    (object-set p "local_coords" t)              ; drops ride along with the wagon, so they never fly inside
    (object-set p "amount_ratio" 0.0)
    (object-set p "emitting" nil)
    (node-add-child parent p)
    (node-3d-set-position p (v3 0 13 0))
    p))

(defvar *cloud-tex* nil "The cloud image; the reference is kept so that the engine does not discard it.")

(defun make-climate (rig)
  "Build the precipitation under the wagon RIG and the clouds in the sky.
Start with a random season and clear weather."
  (setf *rain-node* (particles rig '(.02 .55) "#b0c4dc" 2 1600 1.1 14.0 2.0)           ; BILLBOARD_FIXED_Y: the streaks stay upright
        *snow-node* (particles rig '(.06 .06) "#ffffff" 1 1400 9.0 1.6 10.0 :turbulence 1.5))
  ;; The clouds are a ready-made image (tools/make-sky.py): the noise is computed on a sphere, so overhead the
  ;; sky does not pinch into a star the way flat noise stretched over the sky dome does. No image: flat noise.
  (let ((path "res://textures/sky/clouds.png"))
    (if (picture-here-p path)
        (let ((img (gdext:make-object "Image")) (tex (gdext:make-object "ImageTexture")))
          (read-picture img path)
          (object-call tex "set_image" img)
          (gdext:setf-reference *cloud-tex* tex)
          (object-set *sky-mat* "sky_cover" tex))
        (let ((noise (gdext:make-object "FastNoiseLite")) (tex (gdext:make-object "NoiseTexture2D")))
          (object-set noise "frequency" .006)
          (object-set noise "fractal_octaves" 4)
          (object-set tex "noise" noise)
          (object-set tex "seamless" t)
          (object-set tex "width" 1024)
          (object-set tex "height" 512)
          (object-set *sky-mat* "sky_cover" tex))))
  (set-season (nth (random 4) *seasons*))
  (setf *wx* (list :cloud 0.0 :rain 0.0 :snow 0.0 :fog 0.0))
  (set-weather :clear))

(defun step-climate (delta)
  ;; season
  (when (> (incf *season-clock* (* delta *time-scale*)) (* *season-days* *day-len*))
    (let ((next (nth (mod (1+ (position *season* *seasons*)) 4) *seasons*)))
      (set-season next)
      (say (format nil "Пришла ~a" (getf *season-names* next)))))
  ;; weather
  (when (<= (decf *weather-left* delta) 0) (set-weather (pick-weather)))
  (let ((k (min 1.0 (/ delta (if *wx-force* 3.0 *weather-change*)))))
    (loop for (key) on *wx* by #'cddr
          do (incf (getf *wx* key) (* k (- (if *wx-force* (getf *wx-force* key 0.0) (weather-target key)) (getf *wx* key))))))
  (let ((outside (or (null *world*) *wx-force* (object-get *world* "visible"))))   ; no precipitation in the pocket dimension
    (loop for (node k) in (list (list *rain-node* :rain) (list *snow-node* :snow))
          do (let ((v (if (and outside *precip-on*) (wx k) 0.0)))
               (object-set node "amount_ratio" v)
               (unless (eq (> v .01) (object-get node "emitting")) (object-set node "emitting" (> v .01))))))
  (object-set *sky-mat* "sky_cover_modulate" (if *sky-hour* (color .3 .28 .44 (* .9 (wx :cloud))) (color 1 1 1 (* .9 (wx :cloud) (- 1.0 *gloom*))))))   ; no clouds inside the megastructure
