;;;; Meta room. Turn off the light (L), close the shutters (F), keep the door shut, then Q casts
;;;; the spell: a circle spins under your paws, sparks rise, everything floods with lilac, a white
;;;; flash, and we are in another room. Checkered floor, dark wallpaper with gold, a grandfather
;;;; clock, a gramophone (where the record player stands; it plays the same disc), a small table
;;;; with a tablecloth for two and a wall-wide window with velvet curtains. The room hangs in
;;;; space: open the curtains (C or F) and there is a nebula and a planet outside, with golden
;;;; butterflies flying around. The wagon does not sway: we are not driving. Q takes us back the
;;;; same way.
;;;;
;;;; Space, the floor and the butterflies are shaders without textures. Nothing is brighter than 1:
;;;; glow leaves them alone, and there are no highlights or flicker.

(in-package #:kiosk-trip)

(defparameter *meta-room* '(-3.2 3.2 -3.6 3.6) "The bounds of the meta room as (X0 X1 Z0 Z1); the wagon is inside it.")
(defparameter *meta-height* 3.4)
(defparameter *meta-window* '(-2.4 2.4 .7 2.8) "The window in the x+ wall, as (Z0 Z1 BOTTOM TOP), with heights above the floor.")
(defparameter *meta-colors*
  '(:wall "#2a1c3e" :wainscot "#1c1230" :trim "#c8a050" :ceiling "#140c20" :tile1 "#4c3468" :tile2 "#1c1228"
    :velvet "#6e1a48" :velvet2 "#8a2458" :cloth "#9a2030" :wood "#4a2a22" :wood2 "#6a3c2a" :ambient "#6a5a90"))
(defparameter *meta-table* '(1.75 0.0) "The position of the table by the window, as (X Z).")
(defparameter *meta-seats* '((:meta 1.75 -.85 3.14 1.2) (:meta 1.75 .85 0.0 1.2))
  "The chairs at the table, facing each other.
Each entry is (KIND X Z FACING EYE-HEIGHT).")
(defparameter *spell-time* 1.8 "The duration of the spell before the flash, in seconds.")
(defparameter *flash-fade* .9 "The time the flash takes to fade, in seconds.")

(defvar *meta-node* nil)
(defvar *meta-curtains* '() "The two curtains, each as (NODE . SIDE).")
(defvar *meta-curtains-shut* t)
(defvar *meta-curtain-k* 1.0 "The curtain position: 0 means the curtains are open, 1 means drawn.")
(defvar *window-light* nil)
(defvar *pendulum* nil)
(defvar *pendulum-t* 0.0)
(defvar *space-mat* nil "The outer-space sky material (a ShaderMaterial).")
(defvar *spell* nil "The number of seconds since the spell started, or NIL.")
(defvar *spell-node* nil)
(defvar *spell-ring* nil)
(defvar *spell-sparks* nil)
(defvar *spell-light* nil)
(defvar *flash* nil "The ColorRect of the flash.")

(defun mc (key) (getf *meta-colors* key))

;;; Shaders

(defparameter *space-shader* "shader_type sky;
// Космос: тёмный фон, туманность из шума, звёзды — мягкие точки в 2–3 пикселя,
// неподвижные: в кадре ничего не мерцает.
float hash(vec3 p) { p = fract(p * 0.3183099 + 0.1); p *= 17.0; return fract(p.x * p.y * p.z * (p.x + p.y + p.z)); }
float noise(vec3 x) {
  vec3 i = floor(x); vec3 f = fract(x); f = f * f * (3.0 - 2.0 * f);
  return mix(mix(mix(hash(i), hash(i + vec3(1,0,0)), f.x), mix(hash(i + vec3(0,1,0)), hash(i + vec3(1,1,0)), f.x), f.y),
             mix(mix(hash(i + vec3(0,0,1)), hash(i + vec3(1,0,1)), f.x), mix(hash(i + vec3(0,1,1)), hash(i + vec3(1,1,1)), f.x), f.y), f.z);
}
float fbm(vec3 p) { float v = 0.0, a = 0.5; for (int i = 0; i < 5; i++) { v += a * noise(p); p *= 2.03; a *= 0.5; } return v; }
float stars(vec3 d, float scale, float keep) {
  vec3 p = d * scale; vec3 c = floor(p); float h = hash(c);
  if (h < keep) return 0.0;
  vec3 o = vec3(hash(c + 1.7), hash(c + 3.1), hash(c + 5.3)) * 0.5 + 0.25;
  return smoothstep(0.28, 0.0, length(fract(p) - o)) * (h - keep) / (1.0 - keep);
}
void sky() {
  vec3 d = EYEDIR;
  float n = fbm(d * 2.2 + vec3(4.0, 1.0, 2.0)), n2 = fbm(d * 4.5 + 9.0);
  vec3 col = mix(vec3(0.015, 0.01, 0.04), vec3(0.05, 0.02, 0.09), d.y * 0.5 + 0.5);
  float neb = smoothstep(0.42, 0.8, n);
  col += vec3(0.38, 0.12, 0.48) * neb * 0.7 + vec3(0.6, 0.25, 0.45) * smoothstep(0.55, 0.85, n2) * neb * 0.6;
  col += vec3(1.0, 0.95, 0.88) * (stars(d, 70.0, 0.9) + 0.6 * stars(d, 130.0, 0.92));
  COLOR = min(col, vec3(1.0));
}")

(defparameter *checker-shader* "shader_type spatial;
render_mode diffuse_burley;
// Шахматка по своим координатам пола; края клеток сглажены — вдали не рябит.
uniform vec4 a : source_color; uniform vec4 b : source_color; uniform float tile = 0.6;
varying vec2 p;
void vertex() { p = VERTEX.xz / tile; }
void fragment() {
  vec2 w = fwidth(p) + 0.001;
  vec2 i = 2.0 * (abs(fract((p - 0.5 * w) * 0.5) - 0.5) - abs(fract((p + 0.5 * w) * 0.5) - 0.5)) / w;
  ALBEDO = mix(a.rgb, b.rgb, 0.5 - 0.5 * i.x * i.y);
  ROUGHNESS = 0.55; SPECULAR = 0.2;
}")

(defparameter *planet-shader* "shader_type spatial;
render_mode unshaded;
// Планета серпом: освещена с одного бока, по диску — тихие полосы.
varying vec3 n;
void vertex() { n = (MODEL_MATRIX * vec4(NORMAL, 0.0)).xyz; }
void fragment() {
  vec3 nn = normalize(n);
  float lit = smoothstep(-0.1, 0.35, dot(nn, normalize(vec3(0.15, 0.45, 0.9))));
  vec3 base = mix(vec3(0.62, 0.52, 0.72), vec3(0.74, 0.62, 0.76), 0.5 + 0.5 * sin(nn.y * 14.0 + nn.x * 3.0));
  ALBEDO = mix(vec3(0.05, 0.03, 0.08), base, lit);
}")

(defparameter *butterfly-shader* "shader_type spatial;
render_mode unshaded, cull_disabled;
// Золотая бабочка: к камере лицом, крылья машут, каждая — в своём ритме.
uniform vec4 wing : source_color = vec4(1.0, 0.78, 0.32, 1.0);
uniform vec4 edge : source_color = vec4(0.55, 0.3, 0.08, 1.0);
void vertex() {
  MODELVIEW_MATRIX = VIEW_MATRIX * mat4(INV_VIEW_MATRIX[0], INV_VIEW_MATRIX[1], INV_VIEW_MATRIX[2], MODEL_MATRIX[3]);
  MODELVIEW_NORMAL_MATRIX = mat3(MODELVIEW_MATRIX);
  VERTEX.x *= 0.2 + 0.8 * abs(sin(TIME * 7.0 + float(INSTANCE_ID) * 1.37));
}
void fragment() {
  vec2 u = UV * 2.0 - 1.0; u.y = -u.y;
  vec2 q = vec2(abs(u.x), u.y);
  float up = length((q - vec2(0.48, 0.25)) / vec2(0.5, 0.55));
  float lo = length((q - vec2(0.33, -0.45)) / vec2(0.32, 0.38));
  float body = step(q.x, 0.07) * step(abs(u.y), 0.7);
  float d = min(up, lo);
  ALPHA = max(step(d, 1.0), body);
  ALPHA_SCISSOR_THRESHOLD = 0.5;
  ALBEDO = mix(wing.rgb, edge.rgb, max(smoothstep(0.7, 0.95, d), body));
}")

(defun shader-material (code &rest params)
  (let ((sh (gdext:make-object "Shader")) (m (gdext:make-object "ShaderMaterial")))
    (object-set sh "code" code)
    (object-set m "shader" sh)
    (loop for (k v) on params by #'cddr do (object-call m "set_shader_parameter" k v))
    m))

(defun mesh-node (parent mesh mat x y z &key (shadow t))
  (let ((mi (gdext:make-object "MeshInstance3D")))
    (object-set mi "mesh" mesh)
    (object-set mi "material_override" mat)
    (unless shadow (object-set mi "cast_shadow" 0))
    (node-add-child parent mi)
    (node-3d-set-position mi (v3 x y z))
    mi))

(defun box-mesh (w h d)
  (let ((m (gdext:make-object "BoxMesh"))) (object-set m "size" (v3 w h d)) m))

;;; The room

(defun make-meta (rig)
  "Build the meta room under RIG, alongside the wagon and the camera, and return its node.
The room is visible only while we are in it."
  (when *meta-node* (node-queue-free *meta-node*))
  (unless *space-mat* (gdext:setf-reference *space-mat* (shader-material *space-shader*)))
  (let* ((r (group rig)) (f *floor*))
    (setf *meta-node* r *meta-curtains* '())
    (destructuring-bind (x0 x1 z0 z1) *meta-room*
      (let ((w (- x1 x0)) (l (- z1 z0)) (zc (/ (+ z0 z1) 2)) (xc (/ (+ x0 x1) 2)))
        ;; the ceiling, the walls with the window frame, the pelmet, the wainscot, the table with its chairs and candles,
        ;; the clock case and the gramophone are drawn in Blender (models/meta-room.blend); the checkered floor is a shader
        (mesh-node r (box-mesh w .1 l) (shader-material *checker-shader* "a" (hex (mc :tile1)) "b" (hex (mc :tile2)) "tile" .6)
                   xc (- f .05) zc)
        (model r "meta-room")
        ;; curtains: two, made of folds in two shades; they slide
        (destructuring-bind (wz0 wz1 wy0 wy1) *meta-window*
          (let ((half (/ (- wz1 wz0) 2)) (xs (- x1 .16)))
            (dolist (side '(-1 1))
              (let ((g (group r xs (+ f (/ (+ wy0 wy1 .3) 2)) (if (< side 0) (- wz0 .1) (+ wz1 .1)))) (n 8))
                (dotimes (i n)
                  (shape g :box (list (if (evenp i) .07 .1) (+ (- wy1 wy0) .3) (/ (+ half .12) n))
                         (if (evenp i) (mc :velvet) (mc :velvet2)) 0 0 (* (- side) (+ .5 i) (/ (+ half .12) n))))
                (push (cons g side) *meta-curtains*)))))
        ;; light: candles on the table, a cold one from the window (follows the curtains), lilac ambient
        (destructuring-bind (tx tz) *meta-table*
          (light r "#ffc890" 1.6 5.5 tx (+ f 1.2) tz)
          (setf *window-light* (light r "#a090ff" 1.2 7.0 (- x1 .8) (+ f 1.8) zc)))))
    (make-meta-clock r f)
    ;; the planet outside the window, and butterflies both in the room and outside
    (mesh-node r (let ((m (gdext:make-object "SphereMesh")))
                   (object-set m "radius" 11.0) (object-set m "height" 22.0)
                   (object-set m "radial_segments" 48) (object-set m "rings" 24) m)
               (shader-material *planet-shader*) 90 22 -22 :shadow nil)
    (butterflies r (v3 0 (+ f 1.6) 0) (v3 2.6 1.0 3.0) 14)
    (butterflies r (v3 14 (+ f 3) 0) (v3 9 5 12) 30)
    (make-spell rig)
    (apply-meta-curtains *meta-curtain-k*)
    (apply-meta)
    r))

(defun make-meta-clock (r f)
  "Build under R, at floor height F, the grandfather clock by the blank wall.
Its pendulum is swung by STEP-META."
  (destructuring-bind (x0 x1 z0 z1) *meta-room*
    (declare (ignore x1 z1))
    (let ((g (group r (+ x0 .32) f (+ z0 1.1))))
      (let ((p (group g .23 1.32 0)))
        (model p "meta-pendulum")
        (setf *pendulum* p)))))

(defun butterflies (parent at extents amount)
  "Add AMOUNT golden butterflies under PARENT and return their particle node.
They fly slowly and out of step inside the box EXTENTS around AT."
  (let ((p (gdext:make-object "GPUParticles3D")) (pm (gdext:make-object "ParticleProcessMaterial"))
        (mesh (gdext:make-object "QuadMesh")))
    (object-set pm "emission_shape" 3)                            ; EMISSION_SHAPE_BOX
    (object-set pm "emission_box_extents" extents)
    (object-set pm "direction" (v3 1 .3 0))
    (object-set pm "spread" 180.0)
    (object-set pm "initial_velocity_min" .15)
    (object-set pm "initial_velocity_max" .4)
    (object-set pm "gravity" (v3 0 0 0))
    (object-set pm "turbulence_enabled" t)
    (object-set pm "turbulence_noise_strength" .6)
    (object-set pm "turbulence_noise_speed_random" .4)
    (object-set p "process_material" pm)
    (object-set mesh "size" (vector2 .16 .12))
    (object-set mesh "material" (shader-material *butterfly-shader*))
    (object-set p "draw_pass_1" mesh)
    (object-set p "amount" amount)
    (object-set p "lifetime" 12.0)
    (object-set p "preprocess" 12.0)
    (object-set p "local_coords" t)
    (object-set p "visibility_aabb" (aabb (v3 -30 -20 -30) (v3 60 40 60)))
    (node-add-child parent p)
    (node-3d-set-position p at)
    p))

;;; Spell and flash

(defun spark-material (rgb)
  (let ((m (gdext:make-object "StandardMaterial3D")))
    (object-set m "albedo_color" (hex rgb))
    (object-set m "shading_mode" 0)
    (object-set m "billboard_mode" 3)                             ; BILLBOARD_PARTICLES
    m))

(defun make-spell (rig)
  "Build under RIG the circle under the paws, the sparks, the lilac light and the full-screen flash.
They all stay hidden until Q is pressed."
  (when *spell-node* (node-queue-free *spell-node*))
  (let* ((g (group rig)) (ring (gdext:make-object "TorusMesh"))
         (p (gdext:make-object "GPUParticles3D")) (pm (gdext:make-object "ParticleProcessMaterial"))
         (q (gdext:make-object "QuadMesh")))
    (object-set ring "inner_radius" .95) (object-set ring "outer_radius" 1.05)
    (object-set ring "rings" 64) (object-set ring "ring_segments" 6)
    (setf *spell-node* g
          *spell-ring* (mesh-node g ring (material "#c8a0ff" :unshaded t) 0 .09 0 :shadow nil))
    (dotimes (i 3)                                               ; a star of three rays inside the circle
      (shape *spell-ring* :box '(1.9 .01 .03) "#ffd890" 0 0 0 :unshaded t :shadow nil :rot (list 0 (* i (/ pi 3)) 0)))
    (object-set pm "emission_shape" 6)                            ; a ring along the edge of the circle
    (object-set pm "emission_ring_axis" (v3 0 1 0))
    (object-set pm "emission_ring_height" .05)
    (object-set pm "emission_ring_radius" 1.0)
    (object-set pm "emission_ring_inner_radius" .6)
    (object-set pm "direction" (v3 0 1 0))
    (object-set pm "spread" 12.0)
    (object-set pm "initial_velocity_min" .5)
    (object-set pm "initial_velocity_max" 1.1)
    (object-set pm "gravity" (v3 0 0 0))
    (object-set pm "scale_min" .6)
    (object-set p "process_material" pm)
    (object-set q "size" (vector2 .018 .018))
    (object-set q "material" (spark-material "#f0d8ff"))
    (object-set p "draw_pass_1" q)
    (object-set p "amount" 90)
    (object-set p "lifetime" 1.4)
    (object-set p "local_coords" t)
    (object-set p "emitting" nil)
    (node-add-child g p)
    (setf *spell-sparks* p
          *spell-light* (light g "#b080ff" 0.0 5.0 0 .5 0))
    (object-set g "visible" nil))
  (unless *flash*
    (let ((layer (gdext:make-object "CanvasLayer")) (rect (gdext:make-object "ColorRect")))
      (object-set layer "layer" 5)
      (node-add-child *root* layer)
      (node-add-child layer rect)
      (object-set rect "anchor_right" 1.0)
      (object-set rect "anchor_bottom" 1.0)
      (object-set rect "mouse_filter" 2)                         ; MOUSE_FILTER_IGNORE
      (object-set rect "color" (color .96 .92 1 0))
      (setf *flash* rect))))

(defun spell-ready-p ()
  "Return true if the spell may be cast.
In the meta room it always may (to go back); in the wagon only in the dark with everything shut."
  (or *meta* (and (not *lights-on*) (all-shut-p) (not *door-open*) (< *door-k* .01))))

(defun cast-spell ()
  "Start the spell on Q, which takes us to the meta room or back."
  (cond (*spell*)
        ((spell-ready-p)
         (setf *spell* 0.0 *seat* nil)
         (object-set *spell-node* "visible" t)
         (object-set *spell-sparks* "emitting" t))
        (t (say "Погаси свет, закрой окно и дверь — тогда Q"))))

(defun step-spell (delta)
  (when *spell*
    (let* ((tt (incf *spell* delta)) (k (min 1.0 (/ tt *spell-time*))))
      (node-3d-set-position *spell-node* (v3 (vector3-x *pos*) *floor* (vector3-z *pos*)))
      (node-3d-set-scale *spell-ring* (v3 (+ .3 (* .7 k)) 1 (+ .3 (* .7 k))))
      (node-3d-set-rotation *spell-ring* (v3 0 (* tt 2.5) 0))
      (object-set *spell-light* "light_energy" (* 3.0 k))
      (let ((flash (cond ((< tt (- *spell-time* .45)) 0.0)
                         ((< tt *spell-time*) (/ (- tt (- *spell-time* .45)) .45))
                         (t (max 0.0 (- 1 (/ (- tt *spell-time*) *flash-fade*)))))))
        (object-set *flash* "color" (color .96 .92 1 flash)))
      (when (and (>= tt *spell-time*) (object-get *spell-node* "visible"))   ; swap the scene at the peak of the flash
        (setf *meta* (not *meta*))
        (object-set *spell-sparks* "emitting" nil)
        (object-set *spell-node* "visible" nil)
        (apply-meta))
      (when (> tt (+ *spell-time* *flash-fade*))
        (setf *spell* nil)))))

;;; There and back

(defun apply-meta ()
  "Show either the meta room or the wagon with the world.
The sky, fog and ambient light are set to match."
  (when *meta-node*
    (object-set *meta-node* "visible" *meta*)
    (object-set *cart* "visible" (not *meta*))
    (when *world* (object-set *world* "visible" (not *meta*)))
    (object-set (object-get *env* "sky") "sky_material" (if *meta* *space-mat* *sky-mat*))
    (object-set *env* "fog_enabled" (not *meta*))
    (object-set *env* "ambient_light_source" (if *meta* 2 3))   ; AMBIENT_SOURCE_COLOR / SKY
    (object-set *env* "ambient_light_color" (hex (mc :ambient)))
    (unless *meta*                                               ; we are back: make sure we stand inside the wagon
      (destructuring-bind (x0 x1 z0 z1) *bounds*
        (setf *pos* (v3 (max x0 (min x1 (vector3-x *pos*))) 0 (max z0 (min z1 (vector3-z *pos*)))))))))

(defun toggle-meta-curtains () (setf *meta-curtains-shut* (not *meta-curtains-shut*)))

(defun apply-meta-curtains (k)
  "Set the curtains to position K: 0 gathers them at the window edges, 1 draws them shut."
  (dolist (c *meta-curtains*)
    (node-3d-set-scale (car c) (v3 1 1 (+ .14 (* .86 k)))))
  (when *window-light* (object-set *window-light* "light_energy" (* 1.4 (- 1 k)))))

(defun seats-here () (if *meta* *meta-seats* *seats*))

(defun meta-walkable-p (x z)
  (destructuring-bind (x0 x1 z0 z1) *meta-room*
    (and (< (+ x0 .35) x (- x1 .35)) (< (+ z0 .35) z (- z1 .35)))))

(defun step-meta (delta)
  (step-spell delta)
  (when *meta*
    ;; the wagon's sky and its sun and moon are not needed here, yet STEP-SKY turns them on every frame
    (object-set *sun* "visible" nil)
    (object-set *moon* "visible" nil)
    (object-set *moon-light* "light_energy" 0.0)
    (object-set *env* "ambient_light_energy" (+ .45 (* .35 (- 1 *meta-curtain-k*))))
    (let ((target (if *meta-curtains-shut* 1.0 0.0)))
      (unless (< (abs (- target *meta-curtain-k*)) .001)
        (incf *meta-curtain-k* (* (- target *meta-curtain-k*) (min 1 (* delta 2.5))))
        (apply-meta-curtains *meta-curtain-k*)))
    (when *pendulum*
      (node-3d-set-rotation *pendulum* (v3 (* .22 (sin (* (incf *pendulum-t* delta) pi))) 0 0)))))
