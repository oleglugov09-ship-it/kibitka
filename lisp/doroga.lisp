;;;; The road and the roadside. The roadway over the land, the truck under the wagon, poles with wires,
;;;; bus stops with cats, the layout of the twelve towns and gas stations, road signs, the cats themselves
;;;; (MAKE-CAT). And the mood: everything but the wagon is faded - grayer sky, drier grass, thicker haze.
;;;; The wagon is the only warm spot. The buildings of the towns are in city.lisp.

(in-package #:kiosk-trip)

;;; Mood


;;; Style: pixel textures from the first frame and a shared 'film' over the picture -
;;; less saturation, few shades, a grainy transition between them. It is also what hides
;;; the 'default look' of the simple models.


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
  // зерна нет: точки по всей картинке утомляли глаза (автор, 22.11). Таблица B и levels остались на случай возврата
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

;;; The roadway: a ribbon over the land, old asphalt with a center line. The ribbons are baked with the map
;;; (BAKE-RIBBONS in bake.lisp, tools/bake-map.sh --roads): a mesh for every *ROAD-PIECE* metres of every road in
;;; project/terrain/roads, which the game only loads. Built on the go, a ribbon cost the browser 20 ms and more
;;; every 40 m of the way.

(defparameter *road-lift* .07 "The height in meters of the roadway above the land.")
(defparameter *road-strips*
  '((-6.3 -5.6 "#3a393b") (-5.6 -4.0 "#4b4947") (-4.0 -2.8 "#55524f") (-2.8 -2.6 :line)
    (-2.6 -.95 "#55524f") (-.95 .95 "#55524f") (.95 1.8 "#4b4947") (1.8 2.7 "#3a393b"))
  "The strips across the road, each as from, to (meters from the wagon's axis) and color.
The :line strip is the marking between our lane (+-2.7 m, the wagon drives along its middle)
and the oncoming one on the left; it is white in every other piece.")

(defparameter *road-piece* 200.0 "The length of one road ribbon, in meters.")
(defparameter *road-range* 900.0 "The distance on the map within which road ribbons are kept.")
(defparameter *ribbons-dir* "res://terrain/roads" "Where the baked ribbons lie.")
(defvar *road-pieces* (make-hash-table) "A table from a ribbon's number (RIBBON-KEY) to its nodes: the ribbon and the roadside.")
(defvar *roads-at* nil "Where the ribbons were last chosen: (x z).")
(defvar *roads-want* '() "The ribbons still to load around *ROADS-AT*, each as (key . start), the nearest first.")

(defun ribbon-start (e d)
  "Return the path coordinate where the ribbon of road E that holds path coordinate D starts."
  (+ (edge-d0 e) (* *road-piece* (floor (- d (edge-d0 e)) *road-piece*))))

(defun ribbon-key (start) "Return the number of the ribbon that starts at path coordinate START: its file is named so." (round start))

(defun ribbon-path (start) (format nil "~a/~d.res" *ribbons-dir* (ribbon-key start)))

(defparameter *ribbon-code* "
shader_type spatial;
// The roadway: its colour comes with the mesh, per vertex, as in \"#rrggbb\"; in winter it lies under snow.
uniform float snow = 0.0;
void fragment() {
  // taken as it is, without turning sRGB into linear: so the road was drawn when it was built on the go
  ALBEDO = mix(COLOR.rgb, vec3(0.788, 0.808, 0.839), snow);
  ROUGHNESS = 1.0;
  SPECULAR = 0.15;
}")
(defvar *ribbon-mat* nil "The ShaderMaterial of the roadway of all the ribbons, with the parameter snow.")
(defvar *ribbon-edge-mat* nil "The ShaderMaterial of the soft edges of all the ribbons: the same, and clear where the mesh says so.")

(defun ribbon-material (&optional edge)
  (or (if edge *ribbon-edge-mat* *ribbon-mat*)
      (let ((sh (gdext:make-object "Shader")) (mat (gdext:make-object "ShaderMaterial")))
        (object-set sh "code" (if edge
                                  (let ((at (search "ROUGHNESS" *ribbon-code*)))
                                    (concatenate 'string (subseq *ribbon-code* 0 at) "ALPHA = COLOR.a;
  " (subseq *ribbon-code* at)))
                                  *ribbon-code*))
        (object-set mat "shader" sh)
        (if edge (gdext:setf-reference *ribbon-edge-mat* mat) (gdext:setf-reference *ribbon-mat* mat)))))

(defvar *ribbons-missing* nil "True once a ribbon was asked for and was not among the game's files: told once.")

(defun load-ribbon (start)
  "Put the baked ribbon that starts at path coordinate START on the map and return its node; NIL (and an error in the
log) when it is not baked."
  (let* ((path (ribbon-path start))
         (mesh (and (object-call (gdext:singleton "ResourceLoader") "exists" path)
                    (present (object-call (gdext:singleton "ResourceLoader") "load" path)))))
    (cond ((null mesh)
           ;; said aloud, once: a road that is not there is seen at once, and why it is not there is not
           (unless *ribbons-missing*
             (setf *ribbons-missing* t)
             (note-error (format nil "лента дороги ~a не запечена (tools/bake-map.sh --roads)" path) "roads")
             (when (fboundp 'say) (funcall 'say "Полотна дороги нет в файлах игры. Обнови страницу без кеша: Ctrl+Shift+R")))
           nil)
          (t (let ((mi (gdext:make-object "MeshInstance3D")) (base (road-pos start)))
               (object-set mi "mesh" mesh)
               (object-call mi "set_surface_override_material" 0 (ribbon-material))
               (when (> (object-call mesh "get_surface_count") 1)
                 (object-call mi "set_surface_override_material" 1 (ribbon-material t)))
               (object-set mi "cast_shadow" 0)
               (node-add-child *land* mi)
               (node-3d-set-position mi (v3 (first base) (second base) (third base)))
               mi)))))

(defun step-roads ()
  "Keep the ribbons of all roads nearby on the map, not only of the one we drive on.
At a fork one can thus see where the other road goes. At most one ribbon is loaded per frame; which ribbons
are wanted is worked out once per 30 m driven."
  (let ((snow (if (eq *season* :winter) .6 0.0)))
    (object-call (ribbon-material) "set_shader_parameter" "snow" snow)
    (object-call (ribbon-material t) "set_shader_parameter" "snow" snow))
  (multiple-value-bind (x z) (road-point *dist*)
    (when (or (null *roads-at*) (> (max (abs (- x (first *roads-at*))) (abs (- z (second *roads-at*)))) 30.0))
      (let ((want (make-hash-table)) (r (ceiling *road-range* *grid-cell*)))
        (loop for ix from (- (floor x *grid-cell*) r) to (+ (floor x *grid-cell*) r)
              do (loop for iz from (- (floor z *grid-cell*) r) to (+ (floor z *grid-cell*) r)
                       do (dolist (s (gethash (+ (* 100003 ix) iz) *road-grid*))
                            (let ((start (ribbon-start (car s) (+ (edge-d0 (car s)) (* (cdr s) *map-step*)))))
                              (setf (gethash (ribbon-key start) want) (cons start (car s)))))))
        (maphash (lambda (k nodes) (unless (gethash k want) (dolist (n nodes) (when n (node-queue-free n))) (remhash k *road-pieces*)))
                 *road-pieces*)
        (setf *roads-want*                                                    ; the nearest first: the road under the wheels, then the others
              (mapcar #'cdr (sort (loop for k being the hash-keys of want using (hash-value v)
                                        unless (nth-value 1 (gethash k *road-pieces*))
                                          collect (destructuring-bind (start . e) v
                                                    (multiple-value-bind (px pz) (road-point (min (edge-d1 e) (+ start (/ *road-piece* 2))))
                                                      (list (+ (abs (- px x)) (abs (- pz z))) k start))))
                                  #'< :key #'car))
              *roads-at* (list x z))))
    (when *roads-want*
      (destructuring-bind (k start) (pop *roads-want*)
        (unless (nth-value 1 (gethash k *road-pieces*))
          (setf (gethash k *road-pieces*)                                 ; the ribbon and what stands by it; NIL for what is not baked
                (list (load-ribbon start) (values (load-thing (side-key start) *land*)))))))))

(defun clear-roads ()
  (maphash (lambda (k nodes) (declare (ignore k)) (dolist (n nodes) (when n (node-queue-free n)))) *road-pieces*)
  (clrhash *road-pieces*)
  (setf *roads-at* nil *roads-want* '()))

(defun roadside-builder (e a b)
  "Return a function that puts up the roadside of road E from path coordinate A to B - poles with their wires, street
lamps, steles and signs at the towns, zebras, bus stops, the depot - under one root node and returns it, or NIL when
nothing stands there. It is called when the map is baked (BAKE-ROADSIDE): the game loads the result with the ribbon
of the same stretch (STEP-ROADS)."
  (declare (ignore e))
  (lambda ()
    (let ((*keep-into* (list nil)))
      (declare (special *keep-into*))                                        ; its DEFVAR is further down this file
      (flet ((here (d) (and (<= a d) (< d b))))
        (loop for i from (ceiling a *pole-step*) while (< (* i *pole-step*) b)
              do (with-solids (add-pole (* i *pole-step*))))
        (loop for i from (ceiling a 26.0) while (< (* i 26.0) b)
              do (when (town-at (* i 26.0)) (with-solids (add-lamp (* i 26.0)))))
        (dolist (d *stop-ds*) (when (here d) (let ((*seed* (rs-for (+ d 1)))) (with-solids (add-stop-shelters d)))))
        (when (here 16.0) (let ((*seed* (rs-for 16))) (with-solids (depot (lot 16.0 1)))))          ; the depot we set out from
        (when (here 60.0) (with-solids (add-km-sign 60.0)))
        (dolist (z *zebra-ds*) (when (here z) (with-solids (add-zebra z))))
        (dolist (tw *towns*)                                                  ; stele at the entrance, sign at the exit
          (let ((in (- (town-start tw) 12)) (out (+ (town-end tw) 12)))
            (when (here in) (with-solids (add-stele in (town-title tw))))
            (when (here (+ out 13)) (with-solids (add-km-sign (+ out 13))))
            (when (here out) (with-solids (add-stele out (town-title tw) t)))
            (when (here (- in 13)) (with-solids (add-km-sign (- in 13) t))))))
      (let ((nodes (cdr *keep-into*)))
        (when nodes
          (let ((root (group *world*)))
            (dolist (n nodes) (object-call n "reparent" root t))
            root))))))

(defun side-key (start) "Return the name the roadside of the stretch that starts at path coordinate START is baked under." (format nil "side-~d" (ribbon-key start)))

;;; The truck: a house on wheels. The body is a small passenger rail car: rail-car green,
;;; a yellow stripe under the windows, a gray iron roof with mushroom vents, ribs along the sides.
;;; In front (z-) are the cab and the hood.


(defvar *wheels* '() "The wheel nodes, which turn by the distance traveled.")
(defvar *wheel-spin* 0.0)
(defvar *headlight* nil "The headlight, which shines in the dark.")
(defvar *lamps* '() "The lenses of the headlights and tail lights.")
(defvar *beams* '() "The two spotlights of the headlights.")
(defvar *beam-cones* '() "The faint cones of light in front of the headlights.")
(defparameter *wheel-r* .45)   ; the top of the wheel (0.9 m) is below the wagon floor (0.95), otherwise it sticks inside

(defun truck-wheel (c x z)
  (let ((w (group c x *wheel-r* z)))
    (model w "wheel")
    (push w *wheels*)))

(defun make-team (c)
  "Build the truck under the wagon C.
It has a frame, wheels, a cab with a hood, headlights and crates on the cab roof."
  (setf *wheels* '() *lamps* '())
  (let* ((f *floor*) (zf (- *half-length*)))
    (dolist (sx '(-1 1))
      (truck-wheel c (* sx 1.28) (- zf 2.75))                                 ; front axle
      (truck-wheel c (* sx 1.3) 1.15) (truck-wheel c (* sx 1.3) 2.35))        ; rear ones
    ;; everything of the truck that stands still; the wheels, the lamps and the beams are built around it
    (model c "truck")
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
                               :alpha .045 :unshaded t :shadow nil)))))

(defun step-team (delta)
  "Turn the wheels by the distance traveled in DELTA seconds and light the headlights in the dark."
  (incf *wheel-spin* (/ (* *speed* delta) *wheel-r*))
  (dolist (w *wheels*) (node-3d-set-rotation w (v3 (- *wheel-spin*) 0 0)))
  (when *headlight*
    ;; it has no shadow, so it lights the ground under the wagon too: on a road at night that passes for the
    ;; glow of the lamps, on a bare floor in the megastructure it is a patch of light from nowhere
    (object-set *headlight* "light_energy" (* 1.2 (- 1 (daylight (sky-now))) (- 1.0 *gloom*))))
  (let ((dark (- 1 (min 1.0 (* 2.5 (daylight (sky-now)))))))                ; the headlights come on at dusk
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
               (keep (shape *world* :box '(.035 .035 1.0) "#16161a"           ; one mesh stretched: all the wires of a stretch are one draw
                            (/ (+ (first p) (first q)) 2) (/ (+ (second p) (second q)) 2) (/ (+ (third p) (third q)) 2)
                            :rot (list (- (atan dy flat)) (atan dx dz) 0) :scale (list 1.0 1.0 len) :shadow nil)
                     d)))))

(defun add-pole (d)
  (when (bare-at d) (return-from add-pole nil))                         ; no poles on the bridge or in the tunnel
  (destructuring-bind (x y z) (side-point *pole-x* d -.3)
    (when (on-road-p x z .3) (return-from add-pole nil))                 ; at a fork - not in the middle of another road
    (let ((g (keep (group *world* x y z) d)) (lean (- (random .1) .05)) (th (road-frame d)))
      (node-3d-set-rotation g (v3 (- (random .06) .03) (- th) lean))
      (model g "pole")
      (let ((tops (loop for s in '(-.65 .65) collect (list (+ x (* s (cos th))) (+ y 7.05) (+ z (* s (sin th)))))))
        (setf (gethash (round d) *pole-wires*) tops)                    ; wires go to those neighbors that already stand
        (dolist (n (list (- d *pole-step*) (+ d *pole-step*)))
          (let ((other (gethash (round n) *pole-wires*)))
            ;; the neighbour by number may stand on another road, kilometres away (the roads are numbered on end):
            ;; a wire to it was a black beam across the whole sky
            (when (and other (< (+ (abs (- (first (first other)) (first (first tops))))
                                   (abs (- (third (first other)) (third (first tops)))))
                                (* 3 *pole-step*)))
              (loop for a in other for b in tops do (wire a b (/ (+ d n) 2))))))))))

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
2 for a coat and a flat cap, 3 for overalls and a hard hat, 4 for a leather jacket and a peaked cap.
The clothes are drawn in Blender (models/zhiteli/cat-coat-N.blend, cat-hat-N.blend)."
  (cond ((<= 1 outfit 4)
         (model g (format nil "cat-coat-~d" outfit))
         (model head (format nil "cat-hat-~d" outfit)))
        (t (model g "cat-coat-0" 0.0 0.0 0.0 :paint (list coat)))))

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
    (model g "cat-paws" 0.0 0.0 0.0 :paint (list fur))
    ;; the head - in its own coordinates, centered at zero
    (model head "cat-head" 0.0 0.0 0.0 :paint (list fur eye))
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
      (model g "zebra"))))


(defparameter *stop-tiles* '("#3f6f73" "#a8843c" "#8a4038" "#55708a" "#b0aa98") "The faded colours of the tiles on the box bus stop.")

(defun stop-box (g d)
  "Build under G a concrete box with a mosaic of faded tiles, varied by path coordinate D."
  (let ((tiles (loop for j below 5 collect (nth (mod (+ j (round d)) 5) *stop-tiles*))))
    (model g "stop-0" 0.0 0.0 0.0 :paint tiles)))

(defun add-stop-shelters (d)
  "Put up the bus stop at path coordinate D; each is different, STOP-LOOK (stops.lisp) picks the look.
On the highway there is an identical one opposite, across the road, for those driving back.
This is baked with the roadside (BAKE-ROADSIDE in bake.lisp)."
  (dolist (side (if (town-at d) '(1) '(1 -1)))                 ; in a town there are houses on the left - no paired bus stop there
    (destructuring-bind (x y z) (side-point (if (> side 0) *stop-x* (- *lane-x* *stop-x*)) d 0.0)
      (unless (on-road-p x z 2.5)                                                ; at a fork - not on the other road: the wagon drove through it
       (let ((g (keep (group *world* x y z) d)))
        (node-3d-set-rotation g (v3 0 (- (if (> side 0) 0 pi) (road-frame d)) 0))
        (stop-look g d)                                                        ; eighteen looks - stops.lisp
        (model g "stop-base"))))))

(defun add-stop (d)
  "Let cats wait at the bus stop at path coordinate D, now and then (STOP-SPOT says where): the stop itself stands
there with the roadside, baked."
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
      (model g "street-lamp"))))

(defun add-stele (d name &optional back)
  "Add the stele at a town entrance at path coordinate D: a concrete pylon with a gear and a star, and a wall with NAME in letters.
BACK is for those entering from the other end: it stands on their right side, facing them."
  (destructuring-bind (x y z) (side-point (if back (- *lane-x* 8.5) 8.5) d -.2)
    (let ((g (keep (group *world* x y z) d)))
      (node-3d-set-rotation g (v3 0 (- (if back pi 0) (road-frame d)) 0))
      (destructuring-bind (px py pz) (model-point "stele" "name") (sign g name px py pz :h .8 :rgb "#8a4038"))
      (model g "stele"))))

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
          (model g "km-sign")
          (sign g (format nil "~a  ~,1f" (town-title next)
                          (/ (abs (- (if back (town-end next) (town-start next)) d)) 1000))
                0 3.13 .06 :h .36 :yaw 0.0)
          (when station
            (sign g (format nil "АЗС  ~,1f" (/ (abs (- station d)) 1000)) 0 2.68 .06 :h .3 :rgb "#c9a13a" :yaw 0.0)))))))

(defun add-bus (d)
  "Add a bus at the bus stop at path coordinate D: a LAZ, white over a skirt of red, blue or green."
  (destructuring-bind (x y z) (side-point 4.4 d 0.0)
    (let ((g (keep (turned (group *world* x y z) d) d)))
      (model g "bus" 0.0 0.0 0.0 :paint (list (elt '("#a3262c" "#2c5a8a" "#3f6f5a") (mod (floor d 100) 3)))))))

(defvar *street-told* nil "The start of the town that has already been announced.")

(defun on-memory-street-p ()
  (let ((tw (town-at *dist*)))
    (and tw (town-memory tw)
         (let ((len (- (town-end tw) (town-start tw))))
           (< (+ (town-start tw) (* .3 len)) *dist* (+ (town-start tw) (* .8 len)))))))


(defun reset-roadside ()
  "Start the roadside from scratch for a new trip.
The nodes are already gone together with the previous World."
  (setf *roadside* '() *stops* '() *towns* '() *town-upto* 0.0 *town-n* 0
        *departed* nil)
  (reset-heading)
  (halt)
  (clrhash *pole-wires*) (clrhash *built-towns*)
  (stages
    (reset-city)
    (reset-tank) (put-money *start-money*)   ; a trip starts with the depot's tank and pocket; LOAD-GAME, after it, puts back what was saved
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
  (timed :rs-team (step-team delta))
  (timed :rs-style (step-style-new))
  ;; poles and wires, lamps, steles, signs, zebras, the bus stops themselves and the depot stand with the roadside,
  ;; baked (ROADSIDE-BUILDER); here is only who waits at the stops
  (timed :rs-listed
  (dolist (d *stop-ds*)
    (listed ((list :stop d) d) (let ((*seed* (rs-for (+ d 1)))) (add-stop d)))))
  (timed :rs-city (step-city))
  (timed :rs-fuel (unless (net-guest-p) (step-fuel delta))
  (step-station))
  (timed :rs-rest
  (dolist (tw *towns*)                                                    ; the market in the middle
    (let ((i (town-index tw)) (mid (town-stop-d tw)))
      (listed ((list :market i) mid) (add-market mid) (when (town-memory tw) (add-bus (- mid 15))))))
  (step-stops)
  (sweep-roadside)))
