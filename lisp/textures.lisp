;;;; Textures - a trial of styles (tools/make-textures.py puts the sets into
;;;; project/textures/<style>/). All are nearly gray, mean ~0.5: the game supplies the color.
;;;; Land has its own shader: grass, meadow, dry grass by vegetation (UV2), road and
;;;; silt by offset from the road and by height (UV), rock by steepness, snow by world height;
;;;; against repetition - two scales and coarse noise. Wood, fabric, bark, roadside
;;;; rocks - a StandardMaterial with the same texture in three projections, tinted by the
;;;; material color. *STYLE* NIL - no textures, as before. To change it - (live:style :painted).

(in-package #:kiosk-trip)

(defparameter *style* :pixel "The current texture style: NIL, :painted, :pixel or :pbr.")

(defparameter *tex-of*
  (append '(("#7a2a24" :planks_wall 1.3) ("#c89a6a" :planks_wall 1.3) ("#8a6440" :planks_wall .9) ("#4a3020" :planks_wall .9)
    ("#a07a50" :planks_floor 1.6) ("#2a4a3e" :planks_roof 1.4) ("#2e5244" :planks_roof 1.4)
    ("#8a1a22" :carpet 1.2) ("#2a3a6a" :carpet 1.0) ("#e0b050" :fabric .5) ("#b84a34" :fabric .6)
    ("#5a2a5a" :fabric .6) ("#c04050" :fabric .4) ("#4070c0" :fabric .4) ("#40a060" :fabric .4) ("#f0e8d8" :fabric .5)
    ("#3a2a1e" :bark 1.2) ("#4a3424" :bark 1.2) ("#3a2a1a" :bark 1.2) ("#e0dcd0" :bark 1.0)
    ("#4a4648" :rock 1.5) ("#6a6670" :rock 1.5) ("#46424e" :rock 3.0)
    ("#7a5a3a" :planks_floor 1.6) ("#6a4a2e" :planks_floor 1.6) ("#5a3e26" :planks_wall 1.2) ("#4a3420" :planks_wall 1.2)
    ("#d8b050" :dry 1.0))
                       '(("#4a4038" :bark 1.0) ("#85857f" :panel 1.6) ("#6e6e6a" :panel 1.6) ("#5a5a58" :panel 2.0)   ; concrete of bus stops and pedestals
                         ;; houses - panels with seams (tools/make-panel.py); the rock texture on a wall read as a crumpled wall
                         ("#7b7d82" :panel 2.8) ("#84807a" :panel 2.8) ("#6f757c" :panel 2.8) ("#8a7d6e" :panel 2.8)
                         ("#8e8a82" :panel 2.8) ("#8a8478" :panel 2.8) ("#7f8a8c" :rock 2.0)
                         ("#3a4034" :fabric .5) ("#7a3a34" :fabric .5) ("#d8d2c0" :fabric .5)))
  "A table from a material color to (texture, meters per repeat).")

(defparameter *water-rgb* "#2a5a80")

(defun water-material ()
  (material *water-rgb* :rough .3 :emit "#0c2238" :energy .6))   ; not a mirror: sharp highlights flicker
(defparameter *ground-tile* 4.0 "The number of meters per repeat of the land texture.")

(defvar *textures* (make-hash-table :test 'equal) "A table from (style . name) to an ImageTexture.")
(defvar *ground-mat* nil)
(defvar *ground-shader-style* :none)
(defvar *water-t* 0.0)

(defun texture-path (style name)
  (format nil "res://textures/~(~a~)/~(~a~).png" style name))

(defun tex (style name)
  "Return the ImageTexture NAME of STYLE, loaded from its file without Godot's import.
Return NIL if there is no such file."
  (let ((key (cons style name)))
    (multiple-value-bind (tx found) (gethash key *textures*)
      (if found tx
          (let ((path (texture-path style name)))
            (setf (gethash key *textures*) nil)
            (let ((tx (picture-texture path :mipmaps t)))
              (when tx (gdext:setf-reference (gethash key *textures*) tx) tx)))))))

;;; Land

(defparameter *ground-names* '(:grass :grass2 :meadow :dry :road :rock :snow :sand :macro))

(defun ground-shader-code (filter)
  (format nil "shader_type spatial;
render_mode cull_back, diffuse_burley;
~{uniform sampler2D t_~(~a~) : source_color, ~a, repeat_enable;~%~}
uniform float tile = 0.25;
uniform float amount = 1.0;
varying vec3 lp;
varying vec3 ln;
void vertex() {
  // битая нормаль (вырожденный треугольник вдали) даёт в освещении бесконечность — свечение
  // раздувает такой пиксель в белый шар; берём «вверх»
  if (any(isnan(NORMAL)) || any(isinf(NORMAL)) || length(NORMAL) < 0.5) NORMAL = vec3(0.0, 1.0, 0.0);
  lp = VERTEX; ln = NORMAL;
}
vec3 two(sampler2D t, vec2 p) { return mix(texture(t, p).rgb, texture(t, p * 0.37 + vec2(0.31, 0.57)).rgb, 0.4); }
vec3 tri(sampler2D t, vec3 p, vec3 w) {
  return texture(t, p.zy * tile).rgb * w.x + texture(t, p.xz * tile).rgb * w.y + texture(t, p.xy * tile).rgb * w.z;
}
void fragment() {
  vec3 tint = pow(COLOR.rgb, vec3(2.2));
  vec2 top = lp.xz * tile;
  vec3 w = pow(abs(ln), vec3(4.0)); w /= max(w.x + w.y + w.z, 1e-4);
  float m1 = texture(t_macro, lp.xz * 0.006).r;
  float m2 = texture(t_macro, lp.xz * 0.023 + vec2(0.37)).r;
  vec3 g = mix(two(t_grass, top), two(t_grass2, top * 0.8), smoothstep(0.35, 0.65, m1));
  vec4 md = texture(t_meadow, top);
  g = mix(g, md.rgb, UV2.x);
  g = mix(g, two(t_dry, top), UV2.y);
  float own = (1.0 - md.a) * UV2.x;
  float sand = smoothstep(-0.2, -1.0, UV.y);
  g = mix(g, two(t_sand, top), sand);
  float road = smoothstep(2.8, 1.8, abs(UV.x)) * step(-0.3, UV.y);
  g = mix(g, two(t_road, top), road);
  float rock = max(smoothstep(0.3, 0.6, 1.0 - ln.y), smoothstep(4.0, 10.0, UV.y)) * (1.0 - road);
  g = mix(g, tri(t_rock, lp, w), rock);
  float snow = smoothstep(32.0, 40.0, lp.y);
  g = mix(g, two(t_snow, top), snow);
  vec3 detail = mix(vec3(0.5), g, amount) * 2.0 * mix(0.88, 1.12, m2);
  own *= (1.0 - road) * (1.0 - rock) * (1.0 - snow) * (1.0 - sand) * amount;
  vec3 albedo = mix(tint * detail, md.rgb, own);
  ALBEDO = (any(isnan(albedo)) || any(isinf(albedo))) ? tint : clamp(albedo, 0.0, 1.0);
  ROUGHNESS = 1.0;
  SPECULAR = 0.0;   // без отблеска: дальние грани под скользящим углом иначе искрят
}
" (loop for n in *ground-names* append (list n filter))))

(defun ground-material ()
  "Return the land ShaderMaterial set up for the current style.
The shader is rebuilt if the filter has changed."
  (let ((filter (if (eq *style* :pixel) "filter_nearest_mipmap" "filter_linear_mipmap_anisotropic")))
    (unless *ground-mat*
      (gdext:setf-reference *ground-mat* (gdext:make-object "ShaderMaterial")))
    (unless (equal filter *ground-shader-style*)
      (let ((sh (gdext:make-object "Shader")))
        (object-set sh "code" (ground-shader-code filter))
        (object-set *ground-mat* "shader" sh)
        (setf *ground-shader-style* filter)))
    (object-call *ground-mat* "set_shader_parameter" "tile" (/ 1.0 *ground-tile*))
    (object-call *ground-mat* "set_shader_parameter" "amount" (if *style* 1.0 0.0))
    (when *style*
      (dolist (n *ground-names*)
        (let ((tx (tex *style* n)))
          (when tx (object-call *ground-mat* "set_shader_parameter" (format nil "t_~(~a~)" n) tx)))))
    *ground-mat*))

;;; Things: wood, fabric, bark, rocks, water

(defun texture-material (key mat)
  "Put the texture of the current style on material MAT, or take it off.
KEY is the key of MAT in *MATERIALS*."
  (let* ((rgb (first key)) (spec (assoc rgb *tex-of* :test #'string=)))
    (cond
      ((string= rgb *water-rgb*)
       (let ((n (and *style* (tex *style* :water_n))))
         (object-set mat "normal_enabled" (and n t))
         (when n
           (object-set mat "normal_texture" n)
           (object-set mat "normal_scale" .6)
           (object-set mat "uv1_triplanar" t)
           (object-set mat "uv1_scale" (v3 .12 .12 .12)))))
      (spec
       (destructuring-bind (name meters) (rest spec)
         (let ((tx (and *style* (tex *style* name))) (n (and (eq *style* :pbr) (tex *style* (format nil "~(~a~)_n" name)))))
           (if tx
               (let ((c (hex rgb)) (k (/ 1.0 meters)))
                 (object-set mat "albedo_texture" tx)
                 (object-set mat "albedo_color" (color (* 2 (color-r c)) (* 2 (color-g c)) (* 2 (color-b c)) 1.0))
                 (object-set mat "uv1_triplanar" t)
                 (object-set mat "uv1_scale" (v3 k k k))
                 (object-set mat "texture_filter" (if (eq *style* :pixel) 2 5))   ; NEAREST_WITH_MIPMAPS / LINEAR_WITH_MIPMAPS_ANISOTROPIC
                 (object-set mat "normal_enabled" (and n t))
                 (when n (object-set mat "normal_texture" n) (object-set mat "normal_scale" .7)))
               (progn
                 (object-set mat "albedo_texture" nil)
                 (object-set mat "albedo_color" (hex rgb))
                 (object-set mat "uv1_triplanar" nil)
                 (object-set mat "normal_enabled" nil)))))))))

(defun apply-style ()
  (maphash #'texture-material *materials*)
  (ground-material)
  *style*)

(defun step-style (delta)
  "Advance the ripples on the water by DELTA seconds."
  (when *style*
    (incf *water-t* delta)
    (object-set (water-material) "uv1_offset" (v3 (* *water-t* .02) 0 (* *water-t* .013)))))
