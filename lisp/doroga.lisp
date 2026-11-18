;;;; The road and the roadside. The roadway over the land, the truck under the wagon, poles with wires,
;;;; bus stops with cats, the layout of the twelve towns and gas stations, road signs, the cats themselves
;;;; (MAKE-CAT). And the mood: everything but the wagon is faded - grayer sky, drier grass, thicker haze.
;;;; The wagon is the only warm spot. The buildings of the towns are in city.lisp.

(in-package #:kiosk-trip)

;;; Mood

(setf *grass* '(:pine "#2c3a30" :meadow "#4a5238" :autumn "#5a4e34" :birch "#444c38" :rocky "#484846"
                :steppe "#5e583e" :marsh "#363c2e"))
(setf *sky-keys*
      '((0 "#16161f" "#2a2630") (45 "#566078" "#b08a78") (110 "#6f7f93" "#b9c0c4") (660 "#6f7f93" "#b9c0c4")
        (720 "#4a4f6a" "#b87a5a") (760 "#262638" "#6a4448") (810 "#0a0c16" "#161a26") (1140 "#07090f" "#10131c")
        (1200 "#16161f" "#2a2630")))
(setf *fog-base* .0018)        ; thin haze: beyond the detailed land there is now a far plane (vista.lisp)
;; overcast and foggy more often than clear
(setf *weathers* '((:clear 2 .25 0 0 .1) (:cloudy 5 .85 0 0 .25) (:rain 2 .95 1 0 .4) (:fog 3 .5 0 0 1) (:snow 2 .85 0 1 .45)))
;; no fairy-tale mushrooms and pumpkins; fewer things by the road - poles and bus stops replace them
(setf *veg-props*
      '(:pine (:pine :pine :pine :rock :snag)
        :meadow (:bush :hay :fence :round :flowers)
        :autumn (:autumn :autumn :snag :hay :bush)
        :birch (:birch :birch :birch :round :rock)
        :rocky (:rock :rock :cairn :pine)
        :steppe (:bush :bush :hay :rock :fence)
        :marsh (:reeds :reeds :snag :snag :bush)))
(setf *prop-step* 2.4)

;;; Style: pixel textures from the first frame and a shared 'film' over the picture -
;;; less saturation, few shades, a grainy transition between them. It is also what hides
;;; the 'default look' of the simple models.

(setf *style* :pixel)
(setf *tex-of* (append *tex-of*
                       '(("#4a4038" :bark 1.0) ("#85857f" :panel 1.6) ("#6e6e6a" :panel 1.6) ("#5a5a58" :panel 2.0)   ; concrete of bus stops and pedestals
                         ;; houses - panels with seams (tools/make-panel.py); the rock texture on a wall read as a crumpled wall
                         ("#7b7d82" :panel 2.8) ("#84807a" :panel 2.8) ("#6f757c" :panel 2.8) ("#8a7d6e" :panel 2.8)
                         ("#8e8a82" :panel 2.8) ("#8a8478" :panel 2.8) ("#7f8a8c" :rock 2.0)
                         ("#3a4034" :fabric .5) ("#7a3a34" :fabric .5) ("#d8d2c0" :fabric .5))))

(defparameter *grade-code* "
shader_type canvas_item;
uniform sampler2D screen : hint_screen_texture, filter_nearest;
uniform float levels = 20.0;
uniform float sat = 0.74;
uniform float gain = 0.94;
uniform float trip = 0.0;                                     // мухоморный отвар: картинка плывёт, цвета уходят по кругу
const float B[16] = float[](0.0, 8.0, 2.0, 10.0, 12.0, 4.0, 14.0, 6.0, 3.0, 11.0, 1.0, 9.0, 15.0, 7.0, 13.0, 5.0);
void fragment() {
  vec2 uv = SCREEN_UV + vec2(sin(SCREEN_UV.y * 14.0 + TIME * 2.3), cos(SCREEN_UV.x * 11.0 + TIME * 1.7)) * 0.012 * trip;
  vec3 c = texture(screen, uv).rgb;
  float turn = sin(TIME * 0.6) * 2.6 * trip;
  vec3 axis = vec3(0.57735);
  c = c * cos(turn) + cross(axis, c) * sin(turn) + axis * dot(axis, c) * (1.0 - cos(turn));
  float g = dot(c, vec3(0.299, 0.587, 0.114));
  c = mix(vec3(g), c, sat) * gain;
  c = mix(c, c * vec3(0.94, 0.98, 1.06), 1.0 - g);          // тени холоднее
  vec2 v = SCREEN_UV - 0.5;
  c *= 1.0 - 0.35 * dot(v, v);                                // края темнее
  ivec2 p = ivec2(FRAGCOORD.xy) % 4;
  c = floor(c * levels + B[p.x + p.y * 4] / 16.0) / levels;  // мало оттенков, переход зерном
  COLOR = vec4(c, 1.0);
}")
(defvar *grade* nil "The ShaderMaterial of the film, with parameters levels, sat and gain.")

(defun make-grade (root)
  (let ((layer (gdext:make-object "CanvasLayer")) (rect (gdext:make-object "ColorRect"))
        (sh (gdext:make-object "Shader")) (mat (gdext:make-object "ShaderMaterial")))
    (object-set sh "code" *grade-code*)
    (object-set mat "shader" sh)
    (object-set layer "layer" -1)                              ; under the hints and the menu
    (object-set rect "anchor_right" 1.0) (object-set rect "anchor_bottom" 1.0)
    (object-set rect "mouse_filter" 2)
    (object-set rect "material" mat)
    (node-add-child root layer)
    (node-add-child layer rect)
    (gdext:setf-reference *grade* mat)))

(defvar *styled* 0 "The number of materials that already have textures.")
(defun step-style-new ()
  "Give textures to new materials too.
Things by the road are built on the go, so new materials keep appearing."
  (unless (= *styled* (hash-table-count *materials*))
    (setf *styled* (hash-table-count *materials*))
    (apply-style)))

;;; The roadway: a ribbon over the land in every piece, old asphalt with a center line

(defparameter *road-lift* .07 "The height in meters of the roadway above the land.")
(defparameter *road-strips*
  '((-6.3 -5.6 "#3a393b") (-5.6 -4.0 "#4b4947") (-4.0 -2.8 "#55524f") (-2.8 -2.6 :line)
    (-2.6 -.95 "#55524f") (-.95 .95 "#55524f") (.95 1.8 "#4b4947") (1.8 2.7 "#3a393b"))
  "The strips across the road, each as from, to (meters from the wagon's axis) and color.
The :line strip is the marking between our lane (+-2.7 m, the wagon drives along its middle)
and the oncoming one on the left; it is white in every other piece.")

(defparameter *road-piece* 40.0 "The length of one road ribbon, in meters.")
(defparameter *road-range* 900.0 "The distance on the map within which road ribbons are kept.")
(defvar *road-pieces* (make-hash-table) "A table from a ribbon number (path coordinate / *ROAD-PIECE*) to its node.")
(defvar *roads-at* nil)

(defun build-road-piece (k)
  "Build the road ribbon that starts at path coordinate K times *ROAD-PIECE* and return its node.
It has the strips across and, in a town, a curb and sidewalks. It lies at road height, the land
under it is slightly lower (STRIP-HEIGHT); over the chasm the ribbon runs alone as the bridge deck."
  (let* ((d0 (* k *road-piece*)) (n 10) (step (/ *road-piece* n)) (base (road-pos d0))
         (snow (if (eq *season* :winter) .6 0.0)) (st (gdext:make-object "SurfaceTool")) (uv (vector2 0 0))
         (mi (gdext:make-object "MeshInstance3D")))
    (flet ((pt (s j) (destructuring-bind (x y z) (road-pos (+ d0 (* j step)) s *road-lift*)
                       (v3 (- x (first base)) (- y (second base)) (- z (third base)))))
           (tint (rgb) (mix-color (hex rgb) (hex "#c9ced6") snow)))
      (object-call st "begin" 3)
      (dotimes (j n)
        (when (town-at (+ d0 (* (+ j .5) step)))                               ; in a town - curb and sidewalks
          (loop for (s0 s1 rgb) in '((-9.4 -6.6 "#67655f") (-6.6 -6.3 "#9a988f") (2.7 3.0 "#9a988f") (3.0 5.8 "#67655f"))
                do (let ((c (tint rgb)))
                     (object-call st "add_triangle_fan" (vector (pt s0 (1+ j)) (pt s1 (1+ j)) (pt s1 j) (pt s0 j))
                                  (vector uv uv uv uv) (vector c c c c)))))
        (loop for (s0 s1 rgb) in *road-strips*
              do (let ((c (tint (if (eq rgb :line) (if (evenp (+ j (* k n))) "#b9b6ac" "#55524f") rgb))))
                   (object-call st "add_triangle_fan" (vector (pt s0 (1+ j)) (pt s1 (1+ j)) (pt s1 j) (pt s0 j))
                                (vector uv uv uv uv) (vector c c c c)))))
      (object-call st "generate_normals")
      (object-set mi "mesh" (object-call st "commit"))
      (object-set mi "material_override" (material "#ffffff" :vertex t :rough 1.0))
      (object-set mi "cast_shadow" 0)
      (node-add-child *land* mi)
      (node-3d-set-position mi (v3 (first base) (second base) (third base)))
      mi)))

(defun step-roads ()
  "Keep the ribbons of all roads nearby on the map, not only of the one we drive on.
At a fork one can thus see where the other road goes. At most two new ribbons are built per frame."
  (multiple-value-bind (x z) (road-point *dist*)
    (when (and *roads-at* (not (eq (fourth *roads-at*) *season*))) (clear-roads))   ; in winter the road is under snow - repaint
    (when (or (null *roads-at*) (> (max (abs (- x (first *roads-at*))) (abs (- z (second *roads-at*)))) 30.0)
              (plusp (third *roads-at*)))
      (let ((want '()) (r (ceiling *road-range* *grid-cell*)) (made 0))
        (loop for ix from (- (floor x *grid-cell*) r) to (+ (floor x *grid-cell*) r)
              do (loop for iz from (- (floor z *grid-cell*) r) to (+ (floor z *grid-cell*) r)
                       do (dolist (s (gethash (+ (* 100003 ix) iz) *road-grid*))
                            (pushnew (floor (+ (edge-d0 (car s)) (* (cdr s) *map-step*)) *road-piece*) want))))
        (maphash (lambda (k node) (unless (member k want) (node-queue-free node) (remhash k *road-pieces*))) *road-pieces*)
        (dolist (k want)
          (unless (gethash k *road-pieces*)
            (if (< made 2)
                (progn (setf (gethash k *road-pieces*) (build-road-piece k)) (incf made))
                (incf made))))
        (setf *roads-at* (list x z (max 0 (- made 2)) *season*))))))        ; some are left unbuilt - we will come again

(defun clear-roads ()
  (maphash (lambda (k node) (declare (ignore k)) (node-queue-free node)) *road-pieces*)
  (clrhash *road-pieces*)
  (setf *roads-at* nil))

;;; The truck: a house on wheels. The body is a small passenger rail car: rail-car green,
;;; a yellow stripe under the windows, a gray iron roof with mushroom vents, ribs along the sides.
;;; In front (z-) are the cab and the hood.

(setf *kc* '(:body "#2f4a3c" :trim "#b9a24a" :roof "#6a6e70" :roof2 "#5e6264" :wood "#8a6440" :dark "#4a3020"
             :floor "#a07a50" :cloth "#8a5a44" :inside "#c4b894" :carpet "#8a1a22" :carpet2 "#e0b050" :carpet3 "#2a3a7a"))

(defparameter *cab-rgb* "#6f9cb4" "The color of the cab: the pale blue of a Soviet lorry.")
(defvar *wheels* '() "The wheel nodes, which turn by the distance traveled.")
(defvar *wheel-spin* 0.0)
(defvar *headlight* nil "The headlight, which shines in the dark.")
(defvar *lamps* '() "The lenses of the headlights and tail lights.")
(defvar *beams* '() "The two spotlights of the headlights.")
(defvar *beam-cones* '() "The faint cones of light in front of the headlights.")
(defparameter *wheel-r* .45)   ; the top of the wheel (0.9 m) is below the wagon floor (0.95), otherwise it sticks inside

(defun truck-wheel (c x z)
  (let ((w (group c x *wheel-r* z)))
    (shape w :cyl (list *wheel-r* *wheel-r* .42 14) "#141416" 0 0 0 :rot (list 0 0 (/ pi 2)))
    (shape w :cyl (list (* .5 *wheel-r*) (* .5 *wheel-r*) .44 10) "#8a8a84" 0 0 0 :rot (list 0 0 (/ pi 2)))
    (shape w :box (list .46 .1 (* 1.5 *wheel-r*)) "#55554f" 0 0 0)          ; a spoke - so that the turning is visible
    (push w *wheels*)))

(defun make-team (c)
  "Build the truck under the wagon C.
It has a frame, wheels, a cab with a hood, headlights and crates on the cab roof."
  (setf *wheels* '() *lamps* '())
  (let* ((f *floor*) (zf (- *half-length*)) (cab *cab-rgb*) (dark "#22242a") (glass "#1a2026"))
    (shape c :box '(1.9 .24 9.4) dark 0 (- f .2) -1.75)                      ; frame
    (dolist (sx '(-1 1))
      (truck-wheel c (* sx 1.28) (- zf 2.75))                                 ; front axle
      (truck-wheel c (* sx 1.3) 1.15) (truck-wheel c (* sx 1.3) 2.35)         ; rear ones
      (shape c :box '(.5 .1 2.7) dark (* sx 1.3) (min (+ (* 2 *wheel-r*) .1) (- f .12)) 1.75))
    ;; cab
    (shape c :box '(2.7 1.2 1.9) cab 0 (+ f .6) (- zf 1.05))
    (shape c :box '(2.6 .95 1.6) cab 0 (+ f 1.68) (- zf .9))
    (shape c :box '(2.72 .12 1.85) cab 0 (+ f 2.2) (- zf .95))
    (shape c :box '(2.3 .7 .06) glass 0 (+ f 1.7) (- zf 1.73) :rough .2)       ; windshield
    (shape c :box '(.07 .72 .08) cab 0 (+ f 1.7) (- zf 1.74) :shadow nil)       ; in two halves, with a post between them
    (shape c :cyl '(1.36 1.36 1.6 12) cab 0 (+ f 2.02) (- zf .9) :rot (list (/ pi 2) 0 0) :scale '(1 .2 1))   ; the rounded roof
    (shape c :box '(2.74 .08 1.9) "#e8e4d8" 0 (+ f 1.2) (- zf 1.05) :shadow nil)                              ; the white belt under the windows
    (dolist (sx '(-1 1)) (shape c :box '(.06 .62 1.0) glass (* sx 1.31) (+ f 1.72) (- zf 1.0) :rough .2))
    (shape c :box '(.5 .75 .3) "#2c2e34" -.6 (+ f 1.5) (- zf 1.0))            ; the driver - a silhouette behind the glass
    (shape c :ball '(.19 8 5) "#2c2e34" -.6 (+ f 2.0) (- zf 1.05))
    (dolist (sx '(-1 1)) (shape c :box '(.08 .3 .2) dark (* sx 1.5) (+ f 1.75) (- zf 1.7)))   ; mirrors
    ;; hood, grille, bumper, headlights
    (shape c :box '(2.3 1.0 1.5) cab 0 (+ f .5) (- zf 2.75))
    (shape c :cyl '(1.15 1.15 1.5 12) cab 0 (+ f .98) (- zf 2.75) :rot (list (/ pi 2) 0 0) :scale '(1 .22 1))   ; the hood is rounded on top
    ;; the face of the lorry: a wide white grille with bars, the lamps set into its ends, a bright bumper
    (shape c :box '(2.34 .62 .08) "#e8e4d8" 0 (+ f .5) (- zf 3.52))
    (dolist (y '(-.2 -.07 .06 .19)) (shape c :box '(1.4 .05 .09) dark 0 (+ f .5 y) (- zf 3.53) :shadow nil))
    (shape c :box '(.5 .09 .1) "#b03a2c" 0 (+ f .9) (- zf 3.53) :shadow nil)                                  ; the maker's plate
    (dolist (sx '(-1 1))
      ;; a wing over each front wheel: an arch of five plates a hand above the tyre. It used to be a whole
      ;; disc, which swallowed the upper half of the wheel and hung in front of it
      (let ((r (+ *wheel-r* .13)))
        (dolist (a '(-1.25 -.62 0.0 .62 1.25))
          (shape c :box (list .56 .07 (* r .72)) cab (* sx 1.3) (+ *wheel-r* (* r (cos a))) (+ (- zf 2.75) (* r (sin a)))
                 :rot (list a 0 0))))
      (shape c :cyl '(.07 .07 .06 8) "#e8a030" (* sx 1.2) (+ f .86) (- zf 3.5) :rot (list (/ pi 2) 0 0) :emit "#e08020" :energy .8 :shadow nil))
    (shape c :box '(2.8 .24 .22) "#b8b4a8" 0 (- f .05) (- zf 3.62) :metal .4 :rough .5)
    (dolist (sx '(-1 1))
      (push (shape c :cyl '(.19 .19 .08 10) "#ffe9b0" (* sx .92) (+ f .55) (- zf 3.54) :rot (list (/ pi 2) 0 0)
                   :emit "#ffd890" :energy 2.5 :shadow nil)
            *lamps*)
      (push (shape c :box '(.22 .16 .06) "#a02020" (* sx 1.25) (+ f .25) (+ *half-length* .1) :emit "#ff2010" :energy 1.2 :shadow nil)
            *lamps*))
    (setf *headlight* (light c "#ffe2a8" 0.0 22.0 0 (+ f 1.2) (- zf 7.5)))
    ;; two beams from the lenses: they reach far down the road and pick out signs and walls, and in the dark
    ;; each is seen as a faint cone
    (setf *beams*
          (loop for sx in '(-1 1)
                collect (let ((spot (gdext:make-object "SpotLight3D")))
                          (object-set spot "light_color" (hex "#ffe6b4"))
                          (object-set spot "light_energy" 0.0)
                          (object-set spot "spot_range" 46.0)
                          (object-set spot "spot_angle" 26.0)
                          (object-set spot "shadow_enabled" nil)
                          (node-add-child c spot)
                          (node-3d-set-position spot (v3 (* sx .92) (+ f .55) (- zf 3.6)))
                          (node-3d-set-rotation spot (v3 -.07 (* sx -.03) 0))
                          spot))
          *beam-cones*
          (loop for sx in '(-1 1)
                collect (shape c :cone '(1.5 9.0 10) "#ffe6b4" (* sx .92) (+ f .45) (- zf 3.6 4.5) :rot (list (/ pi 2) 0 0)
                               :alpha .045 :unshaded t :shadow nil)))
    ;; crates on the cab roof: we carry things in crates
    (loop for (bx bz w) in '((-.55 -.75 .9) (.6 -1.0 .8) (.5 -.3 .6))
          do (shape c :box (list w .45 .6) "#8a6440" bx (+ f 2.49) (+ zf bz))
             (shape c :box (list (+ w .03) .06 .63) "#5a3e26" bx (+ f 2.6) (+ zf bz)))
    (shape c :box '(.1 1.3 .1) dark 1.2 (+ f .9) (- zf .05))                  ; exhaust pipe
    ;; rail-car details: rib posts along the sides, the yellow stripe, underfloor boxes, mushroom vents on the roof
    (let ((x (+ *half-width* .1)) (wh *wall-height*))
      (dolist (sx '(-1 1))
        (loop for z from -2.6 to 2.61 by .65
              do (unless (and (> sx 0) (< (abs z) 1.4))            ; no ribs across the big window - it is not a grille
                   (shape c :box (list .05 (+ wh .1) .09) "#263c31" (* sx x) (+ f (/ wh 2)) z :shadow nil)))
        (shape c :box (list .03 .13 (* 2 *half-length*)) (kc :trim) (* sx (+ x .03)) (+ f .8) 0 :shadow nil)
        (shape c :box '(.5 .45 1.3) "#2a2c30" (* sx 1.15) (- f .55) -.4)
        (shape c :box '(.5 .3 .7) "#34363a" (* sx 1.15) (- f .48) -1.6))
      (loop for z in '(-1.9 0.0 1.9)
            do (shape c :cyl '(.16 .2 .22 8) "#4c5052" 0 (+ f wh 1.12) z)
               (shape c :cyl '(.26 .26 .05 8) "#4c5052" 0 (+ f wh 1.25) z)))))

(defun step-team (delta)
  "Turn the wheels by the distance traveled in DELTA seconds and light the headlights in the dark."
  (incf *wheel-spin* (/ (* *speed* delta) *wheel-r*))
  (dolist (w *wheels*) (node-3d-set-rotation w (v3 (- *wheel-spin*) 0 0)))
  (when *headlight*
    (object-set *headlight* "light_energy" (* 1.2 (- 1 (daylight *day-t*)))))
  (let ((dark (- 1 (min 1.0 (* 2.5 (daylight *day-t*))))))                ; the headlights come on at dusk
    (dolist (spot *beams*) (object-set spot "light_energy" (* 7.0 dark)))
    (dolist (cone *beam-cones*) (object-set cone "visible" (> dark .4)))))

;;; The roadside: poles with wires, bus stops, panel blocks

(defparameter *pole-step* 32.0)
(defparameter *pole-x* -8.0 "The sideways position of the poles, which run on the left beyond the oncoming lane.")
(defparameter *stop-x* 6.8 "The sideways position of the bus stops, which are on the right.")
(defvar *roadside* '() "Everything by the roadside that is removed once behind, each as (node . d).")
(defvar *stops* '() "The bus stops ahead, each as (d people told), where people is a list of nodes.")

(defun side-point (x d &optional (up 0.0))
  "Return the point in World coordinates X sideways from the road at path coordinate D, UP above the land."
  (multiple-value-bind (px pz th) (road-point d)
    (let ((wx (+ px (* x (cos th)))) (wz (+ pz (* x (sin th)))))
      (list wx (+ (land-height wx wz) up) wz))))

(defun turned (g d)
  "Turn the group G along the road at path coordinate D and return it.
Its x then points sideways to the right and its z backwards along the path."
  (node-3d-set-rotation g (v3 0 (- (road-frame d)) 0))
  g)

(defvar *keep-into* nil "The collector cell into which KEEP puts nodes while it is set.
That is how a town is built as a whole.")

(defun keep (node d)
  "Remember the roadside node NODE and return it.
It is removed when path coordinate D leaves the window, or together with its whole town."
  (if *keep-into*
      (push node (cdr *keep-into*))
      (push (list node d *keep-reach*) *roadside*))
  node)

(defvar *pole-wires* (make-hash-table) "A table from the path coordinate of a pole to the ends of its crossarm.
Wires are strung to the neighboring poles.")

(defun wire (a b d)
  "Add a sagging wire between points A and B, kept at path coordinate D."
  (let ((mid (list (/ (+ (first a) (first b)) 2) (- (/ (+ (second a) (second b)) 2) .55) (/ (+ (third a) (third b)) 2))))
    (loop for (p q) in (list (list a mid) (list mid b))
          do (let* ((dx (- (first q) (first p))) (dy (- (second q) (second p))) (dz (- (third q) (third p)))
                    (flat (sqrt (+ (* dx dx) (* dz dz)))) (len (sqrt (+ (* flat flat) (* dy dy)))))
               (keep (shape *world* :box (list .035 .035 len) "#16161a"
                            (/ (+ (first p) (first q)) 2) (/ (+ (second p) (second q)) 2) (/ (+ (third p) (third q)) 2)
                            :rot (list (- (atan dy flat)) (atan dx dz) 0) :shadow nil)
                     d)))))

(defun add-pole (d)
  (when (bare-at d) (return-from add-pole nil))                         ; no poles on the bridge or in the tunnel
  (destructuring-bind (x y z) (side-point *pole-x* d -.3)
    (when (on-road-p x z .3) (return-from add-pole nil))                 ; at a fork - not in the middle of another road
    (let ((g (keep (group *world* x y z) d)) (lean (- (random .1) .05)) (th (road-frame d)))
      (node-3d-set-rotation g (v3 (- (random .06) .03) (- th) lean))
      (shape g :box '(.18 7.4 .18) "#4a4038" 0 3.7 0)
      (shape g :box '(1.5 .1 .1) "#4a4038" 0 6.9 0)
      (dolist (sx '(-1 1)) (shape g :box '(.08 .16 .08) "#b8b4a8" (* sx .65) 7.02 0 :shadow nil))
      (let ((tops (loop for s in '(-.65 .65) collect (list (+ x (* s (cos th))) (+ y 7.05) (+ z (* s (sin th)))))))
        (setf (gethash (round d) *pole-wires*) tops)                    ; wires go to those neighbors that already stand
        (dolist (n (list (- d *pole-step*) (+ d *pole-step*)))
          (let ((other (gethash (round n) *pole-wires*)))
            (when other (loop for a in other for b in tops do (wire a b (/ (+ d n) 2))))))))))

;;; The residents are cats: a big head with ears, a bell-shaped body in a coat, a scarf, a tail. No legs: they
;;; stand, and walk by hopping (STEP-AVATARS in net.lisp). Our own, made of boxes and balls - ready-made
;;; models of people are no longer needed.

(defparameter *cat-furs* '("#3a3a40" "#b06a30" "#8a8a8e" "#d8d2c4" "#5a4636" "#22222a") "The fur colors of cats.")
(defparameter *cat-coats* '("#3c4638" "#54424a" "#2e4460" "#6a4a2a" "#4a4038" "#6a3030") "The coat colors of cats.")
(defparameter *cat-head-y* 1.3 "The height of the center of a cat's head.
The head floats above the shoulders with a gap of a couple of centimeters.")
(defparameter *cat-eye-y* 1.33 "The height of a cat's eyes in its own units, slightly above the center of the head.")

(defun cat-outfit (g head outfit coat)
  "Dress a cat: the body goes into the group G, the hat onto the head HEAD.
OUTFIT is 0 for a coat of color COAT and a scarf, 1 for a sheepskin coat and an ushanka,
2 for a coat and a flat cap, 3 for overalls and a hard hat, 4 for a leather jacket and a peaked cap."
  (flet ((scarf ()
           (shape g :cyl '(.2 .22 .11 10) "#a3262c" 0 .95 0)
           (shape g :ball '(.07 8 5) "#8a1c22" .13 .93 .17)
           (shape g :box '(.09 .34 .04) "#a3262c" .14 .76 .2 :rot '(.12 0 -.08))
           (shape g :box '(.08 .26 .04) "#b83038" .07 .8 .22 :rot '(.16 0 .1))
           (dolist (dy '(.0 .1)) (shape g :box '(.092 .025 .042) "#e8e0d0" .14 (+ .64 dy) .21 :rot '(.12 0 -.08) :shadow nil))))
    (case outfit
      (1 (shape g :cyl '(.19 .34 .92 10) "#b08a58" 0 .46 0)                    ; sheepskin coat: sheepskin, white trim
         (shape g :cyl '(.23 .25 .13 10) "#f0ece0" 0 .95 0) (shape g :cyl '(.35 .355 .1 10) "#f0ece0" 0 .06 0)
         (shape g :box '(.05 .8 .03) "#f0ece0" 0 .5 .27 :rot '(-.17 0 0) :shadow nil)
         (shape head :ball '(.3 10 6) "#6a4a30" 0 .2 -.02 :scale '(1.22 .55 1.08))   ; ushanka
         (shape head :box '(.42 .11 .06) "#8a6a48" 0 .16 .27 :shadow nil)
         (dolist (sx '(-1 1)) (shape head :box '(.07 .3 .24) "#6a4a30" (* sx .37) -.02 0)))
      (2 (shape g :cyl '(.15 .3 .92 10) "#4a4a52" 0 .46 0) (scarf)              ; coat with buttons
         (dolist (y '(.4 .56 .72)) (shape g :ball '(.025 6 4) "#c8b890" 0 y (+ .2 (* .16 (- .92 y))) :shadow nil))
         (shape head :cyl '(.3 .32 .08 12) "#5a5048" 0 .26 -.02 :rot '(-.1 0 0))  ; flat cap
         (shape head :box '(.26 .03 .17) "#4a4038" 0 .22 .3 :rot '(.2 0 0)))
      (3 (shape g :cyl '(.15 .3 .92 10) "#2c5a8a" 0 .46 0)                      ; overalls with straps
         (dolist (sx '(-.09 .09)) (shape g :box '(.045 .34 .03) "#244a72" sx .8 .18 :rot '(-.16 0 0) :shadow nil))
         (shape g :box '(.2 .16 .03) "#244a72" 0 .6 .225 :rot '(-.16 0 0) :shadow nil)
         (shape g :cyl '(.2 .21 .08 10) "#d8d4c4" 0 .94 0)
         (shape head :ball '(.33 12 6) "#e0a020" 0 .14 0 :scale '(1.12 .62 1.06))  ; hard hat
         (shape head :cyl '(.4 .4 .03 14) "#e0a020" 0 .13 .03))
      (4 (shape g :cyl '(.24 .3 .46 10) "#3a3a40" 0 .23 0)                      ; leather jacket, trousers, belt
         (shape g :cyl '(.16 .25 .5 10) "#241e1a" 0 .69 0 :rough .45)
         (shape g :cyl '(.255 .255 .05 10) "#16120e" 0 .47 0 :shadow nil) (shape g :box '(.06 .06 .02) "#c9a13a" 0 .47 .255 :shadow nil)
         (shape g :cyl '(.2 .21 .07 10) "#3a2e26" 0 .95 0)
         (shape head :cyl '(.3 .27 .1 12) "#2a2a30" 0 .27 -.02)                    ; peaked cap
         (shape head :cyl '(.275 .275 .05 12) "#a3262c" 0 .22 -.02 :shadow nil)
         (shape head :box '(.24 .025 .15) "#101014" 0 .19 .29 :rot '(.25 0 0))
         (shape head :ball '(.03 6 4) "#c9a13a" 0 .26 .26 :metal .6 :rough .3))
      (t (shape g :cyl '(.15 .3 .92 10) coat 0 .46 0) (scarf)))))

;;; A player's look is one number: units - the clothing set, tens - the fur, hundreds - the flashlight (0 - none,
;;; 1 - beam forward, 2 - upright under the muzzle), thousands - what the cat holds in its paws (0 - nothing,
;;; 99 - the kettle, otherwise the place of the thing in *KINDS* plus one), the hundred thousand - the cat lies
;;; on a bunk. MY-LOOK (look.lisp) assembles it, these take it apart.
(defun look-ok-p (look) (and (integerp look) (<= 0 look 199299)))
(defun look-lying-p (look) (>= look 100000))
(defun look-torch (look) (mod (floor look 100) 10))
(defun look-clothes (look) (mod look 100))
(defun look-hold (look)
  "Return the kind of thing the cat with LOOK holds: a kind from *KINDS*, :KETTLE or NIL."
  (let ((n (mod (floor look 1000) 100)))
    (cond ((= n 99) :kettle) ((plusp n) (first (nth (1- n) *kinds*))))))

(defun make-cat (parent index &optional (look 0))
  "Build a cat under the node PARENT; INDEX picks the fur and the coat.
LOOK is the look chosen by the player (look.lisp): units - the clothing set, tens - the fur
(0 - by INDEX). The cat faces +z. Return (node head): the head is a separate node that floats
above the body with a gap and can be rocked on its own."
  (setf look (look-clothes look))
  (let* ((g (group parent 0 0 0))
         (head (group g 0 *cat-head-y* 0))
         (fur (nth (mod (if (>= look 10) (1- (floor look 10)) index) (length *cat-furs*)) *cat-furs*))
         (coat (nth (mod (floor index 2) (length *cat-coats*)) *cat-coats*))
         (eye (nth (mod index 3) '("#e8c840" "#7ac060" "#6ab0d8"))))
    ;; body and hat - by the clothing set
    (cat-outfit g head (mod look 10) coat)
    (dolist (sx '(-1 1)) (shape g :ball '(.075 8 5) fur (* sx .26) .66 .1))
    ;; the head - in its own coordinates, centered at zero
    (node-3d-set-scale (shape head :ball '(.31 10 7) fur 0 0 0) (v3 1.15 .92 1.0))
    (dolist (sx '(-1 1))
      ;; ear: a flat shell, pink middle, dark edge at the back, a tuft on the tip
      (let ((ear (group head (* sx .2) .27 0)))
        (node-3d-set-rotation ear (v3 0 0 (* sx -.22)))
        (shape ear :cone '(.13 .3 6) fur 0 .1 0 :scale '(1 1 .45))
        (shape ear :cone '(.085 .2 6) "#d89a9a" 0 .08 .035 :scale '(1 1 .3) :shadow nil)
        (shape ear :cone '(.135 .28 6) "#1c1c22" 0 .09 -.03 :scale '(1 1 .3) :shadow nil)
        (shape ear :cone '(.03 .09 4) "#e8e4d8" 0 .28 0 :shadow nil)
        (shape ear :box '(.012 .09 .012) "#e8e4d8" (* sx .03) .05 .06 :rot (list 0 0 (* sx .5)) :shadow nil)
        (shape ear :box '(.012 .07 .012) "#e8e4d8" (* sx -.02) .04 .06 :rot (list 0 0 (* sx -.3)) :shadow nil))
      (shape head :ball '(.065 8 5) eye (* sx .125) .03 .265 :emit eye :energy .5 :shadow nil)
      (shape head :box '(.022 .1 .02) "#101014" (* sx .125) .03 .325 :shadow nil)
      (dolist (dy '(-.02 .02))
        (shape head :box '(.2 .008 .008) "#e8e4d8" (* sx .22) (+ -.08 dy) .25 :rot (list 0 (* sx -.3) (* sx dy 4)) :shadow nil)))
    (shape head :box '(.05 .035 .03) "#d88a8a" 0 -.06 .305 :shadow nil)
    (list g head)))

(defun place-person (g d x &optional (turn .45))
  "Put the cat G by the road at path coordinate D, X sideways from the axis, turned by TURN.
Cats face the road: they wait for whoever drives up. Return G."
  (destructuring-bind (px py pz) (side-point x d -.05)
    (node-3d-set-position g (v3 px (max py (- (road-y d) .03)) pz))       ; the roadside and the pads lie flush with the road - do not sink
    (node-3d-set-rotation g (v3 0 (- turn (/ pi 2) .45 (road-frame d) (if (< x (/ *lane-x* 2)) pi 0)) 0))
    g))

(defun add-person (d x)
  "Add a cat by the road at path coordinate D, X sideways, and return its node."
  (without-solids                                                              ; a cat is alive - one can pass through it, and it can walk away too
    (let* ((g (group *world*)) (k (+ .55 (random .25))))                       ; a cat is waist-high to a human
      (place-person g d x (random .9))
      (node-3d-set-scale (first (make-cat g (random 36))) (v3 k k k))
      g)))

(defun stop-spot (d i)
  "Return where the I-th waiting cat stands at bus stop D, as (path-coordinate sideways).
On the highway that is the side we are driving on; in a town there is one bus stop, on the right:
there the wagon itself pulls up side-on to it (PARK-RIGHT)."
  (let ((*dir* (if (town-at d) 1 *dir*)))
    (list (+ d (* *dir* (+ -1.0 (* i 1.1)))) (side-x (- *stop-x* 2.0 (* i .25))))))

(defvar *zebra-ds* '() "The path coordinates of the pedestrian crossings.
They are at bus stops, gas stations and everywhere one gets out of the wagon.")

(defun service-ds ()
  "Return the path coordinates of the places where the wagon stops on business.
All the gameplay there stands on the right: gas stations, booths, the observatory, the scrap
depot, the airmail."
  (append *stations* (mapcar #'first *gimmicks*) (remove nil (list *observatory* *depot-d* *airmail-d*))))

(defun near-p (ds &optional (within 40.0))
  "Return true if the wagon stands no farther than WITHIN meters from one of the path coordinates DS."
  (some (lambda (d) (< (abs (- d *dist*)) within)) ds))

(defun plan-zebras ()
  "Plan a crossing 13 m past every place where the wagon stops.
That is in front of its nose when we drive forward."
  (setf *zebra-ds* '())
  (dolist (d (append *stop-ds* (service-ds)))
    (when (and d (notany (lambda (z) (< (abs (- z d 13.0)) 40.0)) *zebra-ds*) (not (bare-at (+ d 13.0))))
      (push (+ d 13.0) *zebra-ds*))))

(defun add-zebra (d)
  "Add a zebra crossing over both lanes at path coordinate D, with a sign on each side."
  (destructuring-bind (x y z) (road-pos d (/ *lane-x* 2) .04)
    (let ((g (keep (turned (group *world* x y z) d) d)))
      (loop for i from -4 to 4 do (shape g :box '(.5 .02 3.0) "#e4e0d2" (* i 1.0) 0 0 :shadow nil))
      (dolist (sx '(-5.4 5.4))
        (shape g :box '(.07 2.4 .07) "#6a6e70" sx 1.2 (* (signum sx) 1.9))
        (shape g :box '(.05 .6 .6) "#2c5a9a" sx 2.5 (* (signum sx) 1.9) :shadow nil)
        (shape g :box '(.06 .3 .3) "#e8e4d8" sx 2.5 (* (signum sx) 1.9) :shadow nil :rot (list (/ pi 4) 0 0))))))


(defun stop-bench (g)
  (shape g :box '(.4 .08 2.4) "#6a5038" .5 .55 0)
  (dolist (sz '(-1 1)) (shape g :box '(.36 .5 .1) "#55554f" .5 .28 (* sz 1.0))))

(defun stop-box (g d)
  "Build under G a concrete box with a mosaic of faded tiles, varied by path coordinate D."
  (let ((wall "#85857f"))
    (shape g :box '(.2 2.5 3.6) wall .9 1.25 0)
    (dolist (sz '(-1 1)) (shape g :box '(1.5 2.5 .2) wall .15 1.25 (* sz 1.7)))
    (shape g :box '(2.3 .16 4.2) "#6e6e6a" .1 2.58 0)
    (loop for i below 6 for c = (nth (mod (+ i (round d)) 5) '("#3f6f73" "#a8843c" "#8a4038" "#55708a" "#b0aa98"))
          do (shape g :box '(.04 .5 .5) c .78 (+ .8 (* .52 (mod i 2)) (* .1 (mod i 3))) (- (* i .56) 1.4) :shadow nil))))

(defun stop-wing (g)
  "Build under G a wing: a slanted canopy on two leaning supports, with a rayed sun on the wall."
  (shape g :box '(2.8 .16 4.6) "#7f8a8c" -.1 2.75 0 :rot '(0 0 -.2))
  (dolist (sz '(-1 1)) (shape g :box '(.22 3.0 .3) "#6e6e6a" .75 1.45 (* sz 1.8) :rot '(0 0 .22)))
  (shape g :box '(.14 1.7 3.2) "#3f6f73" 1.0 1.2 0)
  (shape g :cyl '(.5 .5 .05 12) "#c9a13a" .92 1.45 0 :rot (list 0 0 (/ pi 2)) :shadow nil)
  (loop for i below 8 for a = (* i (/ pi 4))
        do (shape g :box '(.04 .08 .5) "#c9a13a" .92 (+ 1.45 (* .8 (sin a))) (* .8 (cos a)) :rot (list a 0 0) :shadow nil)))

(defun stop-arch (g)
  "Build under G a shell arch: a vault of slabs, every other one painted."
  (loop for i below 7 for a = (* (+ i .5) (/ pi 7))
        do (shape g :box '(.16 .95 3.4) (if (evenp i) "#55708a" "#b0aa98") (+ .2 (* 1.9 (cos a) -1) 1.0) (* 2.1 (sin a)) 0
                  :rot (list 0 0 (+ a (/ pi 2))))))

(defun add-stop (d)
  "Add a bus stop at path coordinate D; each is different, BUILD-STOP (stops.lisp) picks the look.
On the highway there is an identical one opposite, across the road, for those driving back.
Sometimes cats wait at it (STOP-SPOT says where)."
  (dolist (side (if (town-at d) '(1) '(1 -1)))                 ; in a town there are houses on the left - no paired bus stop there
    (destructuring-bind (x y z) (side-point (if (> side 0) *stop-x* (- *lane-x* *stop-x*)) d 0.0)
      (let ((g (keep (group *world* x y z) d)))
        (node-3d-set-rotation g (v3 0 (- (if (> side 0) 0 pi) (road-frame d)) 0))
        (build-stop g d)                                                       ; seventeen looks - stops.lisp
        (shape g :box '(2.4 .12 4.2) "#5a5a58" -.2 .06 0)                      ; floor slab
        (stop-bench g)
        (shape g :box '(.08 2.6 .08) "#5a5a58" -1.2 1.3 2.4)                   ; bus stop sign on a post
        (shape g :box '(.06 .5 .6) "#c9a13a" -1.2 2.4 2.4 :shadow nil))))
  (let ((people (loop for i below (nth (random 6) '(0 1 1 1 2 2))
                      collect (apply #'add-person (stop-spot d i)))))
    (push (list d people nil) *stops*)))

;;; Towns. Every now and then the road becomes a street: panel blocks stand in rows on both
;;; sides, street lamps and a few trees along the street, a stele at the entrance. No houses between towns.

          ; what is already built: the stele at the entrance, the bus stop in the middle, the square
;;; Gas stations stand by plan, not 'wherever the buildings fall': at the exit of every town and
;;; halfway along a long stretch. The road signs are written from the same plan - they cannot disagree.

   ; a spoke has its own

(defun town-prop (d side)
  "Add roadside decoration in a town at path coordinate D on side SIDE: now and then a tree by the street."
  (when (zerop (random 6))
    (let* ((x (* side (+ (if (< side 0) 12.6 9) (random 3.0))))            ; on the left - beyond the oncoming lane
           (g (turned (apply #'group *world* (side-point x d -.3)) d)))
      (build-prop g (if (zerop (random 2)) :birch :round) side)
      (setf *props* (nconc *props* (list (cons g d)))))))

(defun add-lamp (d)
  "Add a street lamp on the right at path coordinate D: a post, an arm over the road, a shade."
  (destructuring-bind (x y z) (side-point 5.2 d -.2)
    (when (on-road-p x z .3) (return-from add-lamp nil))
    (let ((g (keep (turned (group *world* x y z) d) d)))
      (shape g :box '(.13 6.2 .13) "#5a5e60" 0 3.1 0)
      (shape g :box '(1.7 .08 .08) "#5a5e60" -.85 6.1 0 :shadow nil)
      (shape g :box '(.55 .12 .26) "#ffe2a0" -1.6 6.02 0 :emit "#ffd080" :energy 1.6 :shadow nil))))

(defun add-stele (d name &optional back)
  "Add the stele at a town entrance at path coordinate D: a concrete slab with an emblem and NAME.
BACK is for those entering from the other end: it stands on their right side, facing them."
  (destructuring-bind (x y z) (side-point (if back (- *lane-x* 8.5) 8.5) d -.2)
    (let ((g (keep (group *world* x y z) d)))
      (node-3d-set-rotation g (v3 0 (- (if back pi 0) (road-frame d)) 0))
      (sign g name -.3 1.6 .9 :h .7 :rgb "#d8d4c4")
      (shape g :box '(1.2 .5 3.4) "#6e6e6a" 0 .25 0)
      (shape g :box '(.5 4.6 1.5) "#85857f" 0 2.7 -.6 :rot '(0 0 -.1))
      (shape g :box '(.5 2.6 1.1) "#85857f" 0 1.7 .9 :rot '(0 0 -.1))
      (shape g :cyl '(.55 .55 .08 12) "#8a4038" -.3 3.9 -.6 :rot (list 0 0 (/ pi 2)) :shadow nil)
      (shape g :box '(.08 .22 1.3) "#c9a13a" -.3 2.9 -.6 :shadow nil))))

(defun add-km-sign (d &optional back)
  "Add the blue sign on the right at path coordinate D: distances to the next town and gas station.
BACK is for those driving back."
  (let* ((next (if back
                   (find-if (lambda (tw) (< (town-end tw) (- d 30))) *towns* :from-end t)
                   (find-if (lambda (tw) (> (town-start tw) (+ d 30))) *towns*)))
         (ring-stations (remove-if-not #'ring-d-p *stations*))
         (station (if back
                      (let ((before (remove-if (lambda (s) (>= s d)) ring-stations))) (and before (reduce #'max before)))
                      (next-station d))))
    (when next
      (destructuring-bind (x y z) (side-point (if back (- *lane-x* 7.0) 7.0) d -.2)
        (let ((g (keep (group *world* x y z) d)))
          (node-3d-set-rotation g (v3 0 (- (if back pi 0) (road-frame d)) 0))
          (dolist (sx '(-1.1 1.1)) (shape g :box '(.1 3.2 .1) "#8a8a84" sx 1.6 0))
          (shape g :box '(2.9 1.1 .08) "#2c4a7a" 0 2.9 0 :shadow nil)                  ; the board across the road, facing those driving up
          (sign g (format nil "~a  ~,1f" (town-title next)
                          (/ (abs (- (if back (town-end next) (town-start next)) d)) 1000))
                0 3.13 .06 :h .36 :yaw 0.0)
          (when station
            (sign g (format nil "АЗС  ~,1f" (/ (abs (- station d)) 1000)) 0 2.68 .06 :h .3 :rgb "#c9a13a" :yaw 0.0)))))))

(defun add-bus (d)
  "Add a city bus at the bus stop at path coordinate D: yellow, with a white roof."
  (destructuring-bind (x y z) (side-point 4.4 d 0.0)
    (let ((g (keep (turned (group *world* x y z) d) d)))
      (shape g :box '(2.5 2.3 9.0) "#c9a13a" 0 1.75 0)
      (shape g :box '(2.54 .5 9.04) "#d8d4c4" 0 3.0 0 :shadow nil)
      (shape g :box '(2.56 .8 8.2) "#1a2026" 0 2.2 0 :rough .2 :shadow nil)
      (shape g :box '(2.3 .9 .06) "#1a2026" 0 2.15 -4.52 :rough .2 :shadow nil)
      (dolist (sx '(-1 1))
        (dolist (wz '(-2.9 2.6)) (shape g :cyl '(.5 .5 .3 12) "#141416" (* sx 1.15) .5 wz :rot (list 0 0 (/ pi 2))))
        (shape g :cyl '(.14 .14 .06 8) "#ffe9b0" (* sx .85) 1.0 -4.53 :rot (list (/ pi 2) 0 0) :emit "#ffd890" :energy 1.5 :shadow nil)))))

;;; A street you remember: in the middle of such a town a bus stands and music quietly starts
;;; to play. The file is project/discs/street.mp3 or street.wav; if it is missing - the first disc.

(defvar *street-player* nil)
(defvar *street-vol* 0.0 "The volume of the street music, 0..1.")
(defvar *street-told* nil "The start of the town that has already been announced.")

(defun street-file ()
  (or (find-if #'file-here-p '("res://discs/street.mp3" "res://discs/street.wav"))
      (second (first *discs*))))

(defun on-memory-street-p ()
  (let ((tw (town-at *dist*)))
    (and tw (town-memory tw)
         (let ((len (- (town-end tw) (town-start tw))))
           (< (+ (town-start tw) (* .3 len)) *dist* (+ (town-start tw) (* .8 len)))))))

(defun step-street (delta)
  (let ((on (and (on-memory-street-p) (not *meta*))))
    (when (and on (null *street-player*))
      (let ((file (street-file)))
        (when file
          (setf *street-player* (player *root* (audio-stream file :loop t) :db -40.0 :bus "Музыка" :play nil)))))
    (setf *street-vol* (max 0.0 (min 1.0 (+ *street-vol* (* delta (if on .3 -.2))))))
    (when *street-player*
      (cond ((and (> *street-vol* 0) (not (object-get *street-player* "playing"))) (object-call *street-player* "play"))
            ((and (<= *street-vol* 0) (object-get *street-player* "playing")) (object-call *street-player* "stop")))
      (object-set *street-player* "volume_db" (- (* 34.0 *street-vol*) 40.0)))))

(defun reset-roadside ()
  "Start the roadside from scratch for a new trip.
The nodes are already gone together with the previous World."
  (setf *roadside* '() *stops* '() *towns* '() *town-upto* 0.0 *town-n* 0 *street-told* nil
        *speed* 0.0 *departed* nil *dir* 1 *yaw-now* 0.0 *lane-now* 0.0)
  (clrhash *pole-wires*) (clrhash *built-towns*)
  (stages
    (reset-city)
    (plan-towns 1e9)
    (plan-places)                 ; pads for everything that stands by plan - before the land
    (setf *drops* (loop for tw in *towns* collect (cons (town-stop-d tw) (town-index tw)))        ; markets - in the middle of every town
          *stations* (append (loop for tw in *towns* append (town-stations tw)) *spoke-stations*))
    (plan-zebras)
    (onboard-fuel)))                 ; the number of towns is finite - lay them all out at once: the map needs them

(defun step-stops ()
  "Step the bus stops where cats wait.
From afar there is a hint; once we stop nearby, a passenger gets in."
  (dolist (s *stops*)
    (destructuring-bind (d people told) s
      (when people
        (let ((gap (ahead-of d)))
          (when (and (not told) (< 0 gap 70) (> *speed* 1.0))
            (setf (third s) t))))))
  ;; drove past - those not picked up stay behind
  (setf *stops* (remove-if (lambda (s)
                             (when (out-of-window-p (first s))
                               (dolist (p (second s)) (node-queue-free p))
                               t))
                           *stops*)))

(defun sweep-roadside ()
  "Remove the roadside things that left the window and forget them.
When we come back they are built again."
  (setf *roadside* (remove-if (lambda (r) (destructuring-bind (node d reach) r
                                            (when (out-of-window-p d reach) (node-queue-free node) t)))
                              *roadside*))
  (sweep-listed)
  (maphash (lambda (d tops) (declare (ignore tops)) (when (out-of-window-p d) (remhash d *pole-wires*))) *pole-wires*))

(defun step-roadside (delta)
  (step-team delta)
  (step-style-new)
  (cover :poles *pole-step* (lambda (d i) (declare (ignore i)) (add-pole d)))
  (dolist (d *stop-ds*)
    (listed ((list :stop d) d) (let ((*seed* (rs-for (+ d 1)))) (add-stop d))))
  (listed (:depot 16.0 440.0) (let ((*seed* (rs-for 16))) (depot (lot 16.0 1))))        ; the depot we set out from
  (listed (:km0 60.0) (add-km-sign 60.0))
  (dolist (z *zebra-ds*) (listed ((list :zebra z) z) (add-zebra z)))
  (step-city)
  (unless (net-guest-p) (step-fuel delta))
  (step-station)
  (cover :lamps 26.0 (lambda (d i) (declare (ignore i)) (when (town-at d) (add-lamp d))))
  (dolist (tw *towns*)                                                    ; stele at the entrance, sign at the exit, market in the middle
    (let ((i (town-index tw)) (in (- (town-start tw) 12)) (out (+ (town-end tw) 12)) (mid (town-stop-d tw)))
      (listed ((list :stele i) in) (add-stele in (town-title tw)))
      (listed ((list :km i) (+ out 13)) (add-km-sign (+ out 13)))
      (listed ((list :stele-back i) out) (add-stele out (town-title tw) t))
      (listed ((list :km-back i) (- in 13)) (add-km-sign (- in 13) t))
      (listed ((list :market i) mid) (add-market mid) (when (town-memory tw) (add-bus (- mid 15))))))
  (step-street delta)
  (step-stops)
  (sweep-roadside))
