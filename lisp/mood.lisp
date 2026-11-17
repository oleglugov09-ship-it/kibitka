;;;; Mood: where the world and the wagon show in each other.
;;;;
;;;; Out of the wagon: smoke rises from the stove pipe while the fire burns, and at night the big window
;;;; glows warm for whoever looks from the road. Into the wagon: at night the street lamps of a town pass
;;;; over the walls one by one as we drive, now and then the headlights of an oncoming car sweep through,
;;;; rain runs down the panes and drums on the roof. Nothing here is saved or sent: everyone's wagon shows
;;;; it from the fire, the weather and the road they already share.

(in-package #:kiosk-trip)

(defparameter *lamp-gap* 34.0 "The distance between street lamps in a town, in metres.")
(defparameter *car-gap* '(9.0 26.0) "The shortest and the longest pause between oncoming headlights at night, in seconds.")
(defparameter *car-sweep* 1.3 "The number of seconds the headlights of one car take to sweep through the wagon.")

(defvar *mood* nil "The nodes of the mood, a plist: :smoke :glow :lamp :car :roof.")
(defvar *car-timer* 12.0 "The seconds until the next oncoming car.")
(defvar *car-t* nil "The seconds since the headlights of the passing car came in, or NIL.")
(defvar *rain-mat* nil "The material of the rain on the panes.")
(defvar *rain-now* -1.0 "The rain strength the panes show now.")

(defparameter *rain-code* "
shader_type spatial;
render_mode unshaded, cull_disabled, shadows_disabled;
// Капли на стекле: стекло разбито на столбцы, в каждом капли бегут вниз со своей скоростью,
// за каплей тянется след. Чем сильнее дождь, тем больше столбцов занято.
uniform float rain = 0.0;
float h(vec2 p) { return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453); }
void fragment() {
  vec2 uv = UV * vec2(26.0, 5.0);
  float col = floor(uv.x);
  float y = uv.y - TIME * (0.25 + 0.5 * h(vec2(col, 1.0))) + h(vec2(col, 7.0)) * 10.0;
  float on = step(1.0 - rain * 0.75, h(vec2(col, floor(y))));
  vec2 f = vec2(fract(uv.x), fract(y));
  float head = smoothstep(0.32, 0.0, length((f - vec2(0.5, 0.85)) * vec2(1.0, 0.35)));
  float tail = smoothstep(0.12, 0.0, abs(f.x - 0.5)) * smoothstep(0.0, 0.85, f.y) * 0.35;
  ALBEDO = vec3(0.80, 0.88, 0.95);
  ALPHA = on * max(head, tail) * 0.55 * step(0.02, rain);
}" "The shader of the rain on a pane.")

(defun rain-pane (g)
  "Put running raindrops on the outside of the sash G."
  (unless *rain-mat*
    (let ((sh (gdext:make-object "Shader")) (mat (gdext:make-object "ShaderMaterial")))
      (object-set sh "code" *rain-code*)
      (object-set mat "shader" sh)
      (gdext:setf-reference *rain-mat* mat)
      (setf *rain-now* -1.0)))
  (let ((mi (gdext:make-object "MeshInstance3D")) (q (gdext:make-object "QuadMesh")))
    (object-set q "size" (vector2 1.3 1.1))
    (object-set mi "mesh" q)
    (object-set mi "material_override" *rain-mat*)
    (object-set mi "cast_shadow" 0)
    (node-add-child g mi)
    (node-3d-set-position mi (v3 .035 0 0))
    (node-3d-set-rotation mi (v3 0 (/ pi 2) 0))))

;;; The curtains of the big window. Drawn, they shut the street out: no lamps, no headlights, no eyes from
;;; the road. They are a common thing: whoever draws them, they are drawn for everyone.

(defvar *curtains-shut* nil "True when the curtains of the big window are drawn.")
(defvar *curtain-k* 0.0 "The position of the curtains: 0 is gathered at the sides, 1 is drawn.")
(defvar *curtains* '() "The two curtains, each as (node . side).")

(defun apply-curtains ()
  (loop for (node . side) in *curtains*
        do (let ((k *curtain-k*))
             (node-3d-set-scale node (v3 1 1 (+ .16 (* .84 k))))
             (node-3d-set-position node (v3 (- *half-width* .17) (+ *floor* 1.45) (* side (- 1.3 (* .6 k))))))))

(defun make-curtains (c)
  "Hang under C a rod over the big window and two curtains on it."
  (shape c :cyl '(.012 .012 2.9 6) "#5a3e26" (- *half-width* .17) (+ *floor* 2.07) 0 :rot (list (/ pi 2) 0 0) :shadow nil)
  (setf *curtains*
        (loop for side in '(-1 1)
              collect (let ((g (group c 0 0 0)))
                        (shape g :box '(.03 1.22 1.42) "#b84a34" 0 0 0 :shadow nil)
                        (dolist (dz '(-.5 -.25 0.0 .25 .5)) (shape g :box '(.04 1.2 .03) "#9a3a2a" 0 0 dz :shadow nil))   ; folds
                        (shape g :box '(.036 .08 1.42) "#d8a840" 0 -.56 0 :shadow nil)                                  ; a hem
                        (cons g side))))
  (apply-curtains))

(defun toggle-curtains ()
  (setf *curtains-shut* (not *curtains-shut*)))

(defun curtain-usables ()
  "Return the things to press for the curtains: the gathered cloth at the sides, or the drawn cloth anywhere."
  (let ((x (- *half-width* .2)))
    (if *curtains-shut*
        (loop for z in '(-.9 0.0 .9) collect (usable x 1.45 z (lambda () "E — раздвинуть шторы") #'toggle-curtains nil 2.6 .5))
        (loop for z in '(-1.3 1.3) collect (usable x 1.45 z (lambda () "E — задёрнуть шторы") #'toggle-curtains nil 2.6 .2)))))

(defcommon :curtains :value (and *curtains-shut* t) :spec '(:member t nil)
  :accept (lambda (shut) (setf *curtains-shut* shut)))

(defstate :curtains :clear (lambda () (setf *curtains-shut* nil)))

(defun make-mood (c)
  "Build for the wagon C the smoke over the pipe, the glow of the window, the lights that pass and the rain on the roof."
  (let ((old (getf *mood* :roof))) (when old (node-queue-free old)))            ; the sound hangs under the root, not under the old wagon
  (make-curtains c)
  (destructuring-bind (sx sz) *stove-at*
    (let* ((f *floor*)
           (glow (gdext:make-object "MeshInstance3D")) (quad (gdext:make-object "QuadMesh")))
      ;; the lit window as seen from the road: a pane that faces outward only, so from inside it is not there
      (object-set quad "size" (vector2 2.6 1.1))
      (object-set glow "mesh" quad)
      (object-set glow "material_override" (material "#ffc070" :emit "#ffb050" :energy 1.4 :alpha .4 :unshaded t))
      (object-set glow "cast_shadow" 0)
      (node-add-child c glow)
      (node-3d-set-position glow (v3 (+ *half-width* .06) (+ f 1.5) 0))
      (node-3d-set-rotation glow (v3 0 (/ pi 2) 0))
      (object-set glow "visible" nil)
      (setf *mood* (list :smoke (chimney-smoke c (- sx .2) (+ f 4.4) (+ sz .17) :size .55 :amount 10 :life 3.5)
                         :glow glow
                         :lamp (light c "#ffb060" 0.0 4.5 (+ *half-width* .5) (+ f 2.3) 0)
                         :car (light c "#dfe8ff" 0.0 5.0 (- -.4 *half-width*) (+ f 1.6) 0)
                         :roof (player *root* (wav-stream (sound-path "rain") :loop t) :db -80.0 :bus "Кибитка"))
            *car-t* nil)
      (object-set (getf *mood* :roof) "pitch_scale" .62))))                    ; lower and duller than the rain outside: it is the roof

(defun step-mood (delta)
  "Show by DELTA seconds what the fire, the night, the road and the weather do to the wagon."
  (let ((want (if *curtains-shut* 1.0 0.0)))                                    ; the curtains slide in a third of a second
    (unless (= want *curtain-k*)
      (setf *curtain-k* (if (< (abs (- want *curtain-k*)) (* 3.0 delta)) want (+ *curtain-k* (* 3.0 delta (signum (- want *curtain-k*))))))
      (apply-curtains)))
  (destructuring-bind (&key smoke glow lamp car roof) *mood*
    (when smoke
      (let* ((dark (- 1.0 (min 1.0 (* 3.0 (daylight *day-t*)))))                ; 0 by day, 1 in the dark
             (night (* dark (- 1.0 (* .9 *curtain-k*))))                        ; what of the street's light gets in: drawn curtains keep it out
             (moving (> (abs *speed*) .5)) (rain (wx :rain)))
        (object-set smoke "emitting" (and (> *fire* 0) (not *meta*)))
        (object-set glow "visible" (and *lights-on* (> dark .3) (< *curtain-k* .5) (not *meta*)))
        ;; a street lamp every *LAMP-GAP* metres of a town: it comes from the front and leaves behind us
        (let* ((p (mod (/ *dist* *lamp-gap*) 1.0)) (p (if (> *dir* 0) p (- 1.0 p))))
          (node-3d-set-position lamp (v3 (+ *half-width* .5) (+ *floor* 2.3) (- (* 9.0 p) 4.5)))
          (object-set lamp "light_energy" (if (and (town-at *dist*) (not *meta*)) (* 2.4 night (expt (sin (* pi p)) 2)) 0.0)))
        ;; an oncoming car: its headlights sweep the wagon from front to back
        (cond (*car-t*
               (let ((k (/ (incf *car-t* delta) *car-sweep*)))
                 (cond ((>= k 1.0) (setf *car-t* nil) (object-set car "light_energy" 0.0))
                       (t (node-3d-set-position car (v3 (- -.4 *half-width*) (+ *floor* 1.6) (- (* 10.0 k) 5.0)))
                          (object-set car "light_energy" (* 3.2 night (sin (* pi k))))))))
              ((and moving (> night .5) (not *meta*) (<= (decf *car-timer* delta) 0))
               (setf *car-timer* (+ (first *car-gap*) (random (- (second *car-gap*) (first *car-gap*)))) *car-t* 0.0)))
        ;; rain: on the panes and on the roof
        (when (and *rain-mat* (> (abs (- rain *rain-now*)) .01))
          (setf *rain-now* rain)
          (object-call *rain-mat* "set_shader_parameter" "rain" rain))
        (object-set roof "volume_db" (if (or (< rain .02) *meta*) -80.0 (+ (if (outside-p) -22 -13) (* 20 (log rain 10)))))))))
