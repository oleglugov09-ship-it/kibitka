;;;; Towns. The road runs straight through a town, and along it the town is laid out like a real
;;;; Soviet one: private houses behind fences at the edges, then garages and a factory behind a
;;;; concrete fence, then five-storey blocks end-on to the street with courtyards between them, in
;;;; the middle a square with the house of culture, shops and the bus stop, after it nine-storey
;;;; blocks and tower blocks, and then back again: garages, a gas station, private houses. Houses
;;;; carry signs, and the stele carries the town's name.
;;;;
;;;; A lot is a group at the roadside: its x+ points away from the road, z runs along it. A house
;;;; occupies x from the setback onwards; the facade towards the road is the face with the smaller x.

(in-package #:kiosk-trip)

(defvar *shop-names* '() "The signs over the shops, from mods: a list of strings.")
(defvar *slogans* '() "The slogans on the roofs of tall houses, from mods: a list of strings.")
(defparameter *city-ahead* 430.0 "The distance ahead at which a town is built, far enough for it to rise out of the haze.")
(defparameter *zones*
  '((.00 :village) (.13 :garages) (.24 :blocks5) (.43 :centre) (.57 :blocks9) (.76 :garages) (.87 :village))
  "The kinds of buildings a town has along the road.
Each entry is (FRACTION KIND): from which fraction of the town's length, and what is built.")

(defvar *stations* '() "The path coordinates at which gas stations stand.")

;;; Signs

(defun sign (parent text x y z &key (h .6) (rgb "#f0ece0") (yaw (- (/ pi 2))) (pitch 0.0))
  "Add under PARENT at (X, Y, Z) a sign reading TEXT with letter height H, and return it.
By default it faces the road (towards -x of the lot)."
  (let ((l (gdext:make-object "Label3D")))
    (object-set l "text" text)
    (object-set l "font_size" 48)
    (object-set l "pixel_size" (/ h 48.0))
    (object-set l "modulate" (hex rgb))
    (object-set l "outline_size" 0)
    (object-set l "texture_filter" 0)
    (object-set l "alpha_cut" 1)
    (node-add-child parent l)
    (node-3d-set-position l (v3 x y z))
    (node-3d-set-rotation l (v3 pitch yaw 0))
    l))

;;; Mosaics on the end walls. Every house end has one of its own: the cells of one atlas
;;; (tools/make-mosaics.py), a block of them per town, handed out in the order the town is built.

(defparameter *mosaic-grid* '(24 20 48 64 40)
  "The mosaic atlas: columns and rows of cells, tiles across and down a cell, cells per town.")
(defvar *mosaic-town* 0 "The index of the town being built.")
(defvar *mosaic-count* 0 "The number of mosaics already placed in the town being built.")
(defvar *mosaic-atlas* nil "The atlas texture.")
(defvar *mosaic-shader* nil)
(defvar *mosaic-mats* (make-hash-table) "The table mapping a cell number to its material.")

(defparameter *mosaic-code* "shader_type spatial;
// One cell of the atlas, one texel per tile. Each tile gets its own shade and a dark seam;
// both fade out with distance, so far walls do not flicker.
uniform sampler2D atlas : source_color, filter_nearest, repeat_disable;
uniform vec2 cell; uniform vec2 grid; uniform vec2 tiles;
void fragment() {
  vec2 p = UV * tiles; vec2 t = floor(p); vec2 f = fract(p);
  vec3 c = textureLod(atlas, (cell + (t + 0.5) / tiles) / grid, 0.0).rgb;
  float near = 1.0 - clamp(max(fwidth(p.x), fwidth(p.y)) * 2.5, 0.0, 1.0);
  float shade = 0.88 + 0.22 * fract(sin(dot(t + cell * 7.0, vec2(12.9898, 78.233))) * 43758.5453);
  float seam = step(0.86, max(f.x, f.y));
  c *= mix(1.0, shade, near) * mix(1.0, 0.55, seam * near);
  ALBEDO = c * 0.82; ROUGHNESS = 1.0; SPECULAR = 0.1;
}" "The shader of a mosaic.")

(defun mosaic-material (n)
  "Return the material showing cell N of the mosaic atlas, or NIL when the atlas is missing."
  (destructuring-bind (cols rows w h per-town) *mosaic-grid*
    (declare (ignore per-town))
    (or (gethash n *mosaic-mats*)
        (let ((path "res://textures/murals/atlas.png"))
          (unless *mosaic-atlas*
            (when (file-here-p path)
              (let ((img (gdext:make-object "Image")) (tx (gdext:make-object "ImageTexture")) (sh (gdext:make-object "Shader")))
                (object-call img "load_png_from_buffer" (file-bytes path))
                (object-call tx "set_image" img)
                (object-set sh "code" *mosaic-code*)
                (gdext:setf-reference *mosaic-atlas* tx)
                (gdext:setf-reference *mosaic-shader* sh))))
          (when *mosaic-atlas*
            (let ((m (gdext:make-object "ShaderMaterial")))
              (object-set m "shader" *mosaic-shader*)
              (object-call m "set_shader_parameter" "atlas" *mosaic-atlas*)
              (object-call m "set_shader_parameter" "cell" (vector2 (mod n cols) (floor n cols)))
              (object-call m "set_shader_parameter" "grid" (vector2 cols rows))
              (object-call m "set_shader_parameter" "tiles" (vector2 w h))
              (gdext:setf-reference (gethash n *mosaic-mats*) m)
              m))))))

(defun end-mosaic (g x z yaw wide high)
  "Add to G the next mosaic of the town being built, on a blank wall WIDE by HIGH whose foot is at (X, Z).
YAW turns it: 0 faces z+, pi faces z-, -pi/2 faces the street."
  (let* ((per-town (fifth *mosaic-grid*))
         (mat (mosaic-material (+ (* per-town (mod *mosaic-town* 12)) (mod *mosaic-count* per-town))))
         (mh (min (- high 3.0) (/ (- wide 2.0) .75) 14.0)))
    (incf *mosaic-count*)
    (when mat
      (let ((mi (gdext:make-object "MeshInstance3D")) (mesh (gdext:make-object "QuadMesh")))
        (object-set mesh "size" (vector2 (* mh .75) mh))
        (object-set mi "mesh" mesh)
        (object-set mi "material_override" mat)
        (object-set mi "cast_shadow" 0)
        (node-add-child g mi)
        (node-3d-set-position mi (v3 x (max (+ 1.8 (/ mh 2)) (- (* high .62) 1.0)) z))
        (node-3d-set-rotation mi (v3 0 yaw 0))
        mi))))

;;; Building blocks of houses

(defun body (g w h d rgb x z)
  "Add to G the box of a house with its facade at X, depth W away from the road and length D along it.
The foundation goes 4 m into the ground, so on a slope there is no gap under the house."
  (shape g :box (list w (+ h 4) d) rgb (+ x (/ w 2)) (- (/ h 2) 2) z))

(defun window-rows (g floors x z d &key (first 1.6) (step 2.8) (lit 3))
  "Add to G ribbon windows for each of FLOORS storeys on the facade at X, plus a few lit windows."
  (dotimes (f floors)
    (shape g :box (list .12 1.25 (* d .92)) "#2e333b" (- x .05) (+ first (* f step)) z :shadow nil))
  (loop repeat (random (1+ lit))
        do (shape g :box '(.14 1.1 1.3) "#e8c070" (- x .07) (+ first (* (random floors) step))
                  (+ z (* (- (random 1.0) .5) d .85)) :emit "#ffc060" :energy .9 :shadow nil)))

(defun panel-house (g floors w d x z &key slogan)
  "Add to G a panel apartment block of FLOORS storeys standing along the street.
It has ribbon windows, columns of balconies and entrances with canopies."
  (let* ((h (* floors 2.8)) (rgb (nth (random 4) '("#7b7d82" "#84807a" "#6f757c" "#8a7d6e"))))
    (body g w h d rgb x z)
    (shape g :box (list (+ w .4) .5 (+ d .4)) "#4c4e52" (+ x (/ w 2)) (+ h .25) z :shadow nil)
    (window-rows g floors x z d :lit (+ 2 floors))
    (end-mosaic g (+ x (/ w 2)) (+ z (/ d 2) .1) 0 w h)
    (end-mosaic g (+ x (/ w 2)) (- z (/ d 2) .1) pi w h)
    (let ((n (max 2 (round d 16))))
      (dotimes (i n)
        (let ((bz (+ z (* (- (/ (+ i .5) n) .5) d))))
          (shape g :box (list .9 (- h 3.2) 2.6) (nth (mod i 3) '("#9a9488" "#7f8a8c" "#a8a08c")) (- x .4) (+ 1.8 (/ h 2)) bz :shadow nil)
          (shape g :box '(1.6 .15 2.4) "#55554f" (- x .8) 2.6 (+ bz 4.5) :shadow nil)          ; entrance canopy
          (shape g :box '(.12 2.1 1.2) "#4a3020" (- x .06) 1.05 (+ bz 4.5) :shadow nil))))    ; door
    (when slogan
      (shape g :box (list .2 .2 (* d .7)) "#3a3a40" (+ x 1) (+ h .8) z :shadow nil)
      (sign g slogan (+ x 1) (+ h 2.0) z :h 2.4 :rgb "#a3262c"))))

(defun end-on-blocks (g floors n)
  "Add to G N blocks of FLOORS storeys standing end-on to the street, 34 m apart.
Between them are courtyards."
  (dotimes (i n)
    (let* ((z (* (- i (/ (1- n) 2)) 34.0)) (h (* floors 2.8)) (len 52.0)
           (rgb (nth (random 3) '("#84807a" "#8a7d6e" "#7b7d82"))))
      (shape g :box (list len (+ h 4) 12.0) rgb (+ 9 (/ len 2)) (- (/ h 2) 2) z)
      (shape g :box (list (+ len .4) .5 12.4) "#4c4e52" (+ 9 (/ len 2)) (+ h .25) z :shadow nil)
      ;; the end facing the street: a blank wall with a mosaic, windows on the sides
      (end-mosaic g 8.9 z (- (/ pi 2)) 12.0 h)
      (dolist (sz '(-1 1))
        (dotimes (f floors)
          (shape g :box (list (* len .9) 1.25 .12) "#2e333b" (+ 9 (/ len 2)) (+ 1.6 (* f 2.8)) (+ z (* sz 6.05)) :shadow nil))
        (loop repeat (random 4)
              do (shape g :box '(1.3 1.1 .14) "#e8c070" (+ 12 (random (- len 6))) (+ 1.6 (* (random floors) 2.8)) (+ z (* sz 6.07))
                        :emit "#ffc060" :energy .9 :shadow nil)))
      (when (< i (1- n))                                                        ; courtyard: swings and a sandbox
        (shape g :box '(.1 2.2 .1) "#3f6f73" 24 1.1 (+ z 15)) (shape g :box '(.1 2.2 .1) "#3f6f73" 24 1.1 (+ z 19))
        (shape g :box '(.1 .1 4.2) "#3f6f73" 24 2.2 (+ z 17))
        (shape g :box '(3.0 .3 3.0) "#a8843c" 34 .15 (+ z 17))))))

(defun izba (g x z)
  "Add to G at (X, Z) a private house.
It is a log house with a gable roof, windows with carved frames, a fence and a shed."
  (let ((wall (nth (random 4) '("#6a5038" "#4a6a5a" "#55708a" "#7a6a4a"))))
    (body g 7.0 3.2 8.0 wall x z)
    (dolist (s '(-1 1))
      (shape g :box '(4.6 .2 8.6) "#4c4e52" (+ x 3.5 (* s 1.75)) 4.3 z :rot (list 0 0 (* s -.62))))
    (dolist (wz '(-2 2))
      (shape g :box '(.1 1.5 1.3) "#d8d4c4" (- x .04) 1.8 (+ z wz) :shadow nil)
      (shape g :box '(.12 1.1 .9) "#2e333b" (- x .06) 1.8 (+ z wz) :shadow nil))
    (when (zerop (random 3)) (shape g :box '(.14 1.1 .9) "#e8c070" (- x .08) 1.8 (+ z 2) :emit "#ffc060" :energy .9 :shadow nil))
    (shape g :box '(.5 1.6 .5) "#5a4a44" (+ x 4.5) 5.0 (- z 1.5))                                  ; chimney
    (when (zerop (random 3)) (chimney-smoke g (+ x 4.5) 6.0 (- z 1.5) :size .9))                          ; the stove is lit
    (shape g :box '(.12 1.3 13.0) "#6a5a48" (- x 3.5) .65 z)                                     ; fence
    (shape g :box '(3.0 2.2 3.5) "#5a4a3a" (+ x 9.5) 1.1 (+ z 3))))                               ; shed

(defun garages (g len)
  "Add to G a row of garages of length LEN, with doors of different colours, rusty here and there."
  (shape g :box (list 6.0 6.6 len) "#6e6c66" 9.0 1.3 0)
  (shape g :box (list 6.4 .2 (+ len .3)) "#4c4e52" 9.0 2.75 0 :shadow nil)
  (loop for z from (+ (- (/ len 2)) 2.2) below (/ len 2) by 4.2
        do (shape g :box '(.12 2.1 3.3) (nth (random 5) '("#6a3a2e" "#3c4638" "#55708a" "#5a5a58" "#7a4a30")) 5.95 1.1 z :shadow nil)))

(defun factory (g len)
  "Add to G a factory of length LEN behind a concrete fence.
It has a main building, a striped chimney and a gatehouse."
  (loop for z from (+ (- (/ len 2)) 1.5) below (/ len 2) by 3.0                 ; fence of concrete slabs
        do (shape g :box '(.2 2.5 2.9) "#8a8a84" 5.0 1.25 z)
           (shape g :box '(.24 .5 .5) "#6e6e6a" 5.0 1.3 z :rot (list (/ pi 4) 0 0) :shadow nil))
  (body g 26.0 10.0 (* len .7) "#6f757c" 14.0 0)
  (shape g :box (list .14 2.2 (* len .6)) "#2e333b" 13.95 6.0 0 :shadow nil)
  (let ((cz (* len .28)))
    (shape g :cyl '(1.1 1.6 34 10) "#8a8580" 34 17 cz)
    (dotimes (i 3) (shape g :cyl '(1.2 1.25 2.2 10) "#a3262c" 34 (+ 24 (* i 4)) cz :shadow nil))
    (chimney-smoke g 34 35.5 cz :size 4.5 :amount 14 :life 11.0))
  (shape g :box '(4.0 3.0 5.0) "#84807a" 7.0 1.5 (- (/ len 2) 4))                ; gatehouse
  (sign g "ЗАВОД" 6.9 3.6 (- (/ len 2) 4) :h 1.0 :rgb "#d8d4c4"))

(defun shop (g name)
  "Add to G a single-storey shop with a glowing shop window and a sign reading NAME."
  (body g 10.0 4.2 22.0 "#8a8a84" 8.0 0)
  (shape g :box '(.14 2.2 18.0) "#d8c890" 7.95 1.5 0 :emit "#ffd890" :energy .6 :shadow nil)
  (shape g :box '(2.2 .2 22.4) "#55554f" 7.4 3.4 0 :shadow nil)
  (shape g :box '(.2 1.0 12.0) (nth (random 3) '("#8a4038" "#3f6f73" "#a8843c")) 7.9 4.4 0 :shadow nil)
  (sign g name 7.75 4.4 0 :h .8))

(defun culture-house (g)
  "Add to G the house of culture, with a columned portico, a pediment and steps."
  (body g 18.0 11.0 38.0 "#b0a57e" 16.0 0)
  (shape g :box '(5.0 .6 26.0) "#8a8a84" 13.5 .3 0)
  (dotimes (i 6) (shape g :cyl '(.55 .6 8.0 10) "#c8bf9c" 12.6 4.6 (- (* i 4.4) 11)))
  (shape g :box '(5.4 1.6 26.0) "#b0a57e" 13.5 9.4 0)
  (shape g :box '(.14 5.0 3.0) "#4a3020" 15.95 2.8 0 :shadow nil)
  (window-rows g 3 16.0 0 38.0 :first 2.6 :step 3.2)
  (sign g "ДОМ КУЛЬТУРЫ" 10.7 9.4 0 :h 1.2 :rgb "#6a3a2e"))

(defun plaza (g)
  "Add to G the square: paving slabs, a monument on a pedestal, a flower bed, benches and a flagpole."
  (shape g :box '(12.0 .2 48.0) "#6e6c68" 9.0 .1 0 :shadow nil)
  (shape g :box '(2.6 2.4 2.6) "#85857f" 8.0 1.3 0)
  (shape g :box '(.9 2.6 .7) "#4c5052" 8.0 3.8 0) (shape g :ball '(.4 8 5) "#4c5052" 8.0 5.4 0)
  (shape g :box '(1.6 .3 .3) "#4c5052" 7.2 4.7 .2 :rot '(0 0 .5))                 ; the arm points the way
  (shape g :cyl '(3.0 3.0 .3 14) "#5a4a3a" 8.0 .25 -15) (shape g :cyl '(2.6 2.6 .34 14) "#8a4038" 8.0 .27 -15 :shadow nil)
  (dolist (z '(-7 7)) (shape g :box '(.5 .45 2.2) "#6a5038" 5.0 .45 z))
  (shape g :box '(.1 9.0 .1) "#8a8a84" 6.0 4.5 18) (shape g :box '(.05 .9 1.5) "#a3262c" 6.0 8.4 18.8 :shadow nil))

(defun tower (g)
  "Add to G a tower block of twelve storeys."
  (let ((h (* 12 2.8)))
    (body g 16.0 h 16.0 "#7b7d82" 18.0 0)
    (window-rows g 12 18.0 0 16.0 :lit 8)
    (end-mosaic g 26.0 8.1 0 16.0 h)
    (end-mosaic g 26.0 -8.1 pi 16.0 h)
    (dolist (sz '(-1 1)) (shape g :box (list 1.0 (- h 3) 2.4) "#9a9488" 17.6 (+ 1.5 (/ h 2)) (* sz 5) :shadow nil))
    (shape g :box '(4.0 2.6 4.0) "#5c5e62" 26.0 (+ h 1.3) 0 :shadow nil)))

(defun station-kind (d)
  "Return the kind of gas station at path coordinate D.
It is :SWAMP (a hut on stilts) by a swamp, :TAVERN in a pine forest outside towns, otherwise :PLAIN."
  (cond ((in-swamp-p d) :swamp)
        ((and (not (town-at d)) (eq (dominant :veg d) :pine)) :tavern)
        (t :plain)))

(defun gas-station (g d)
  "Add to G the gas station that suits the location at path coordinate D (see STATION-KIND)."
  (ecase (station-kind d)
    (:plain (plain-station g))
    (:tavern (tavern-station g))
    (:swamp (swamp-station g))))

(defun old-pump (g x z)
  (shape g :box '(.6 1.5 .5) "#a3262c" x .75 z) (shape g :cyl '(.32 .32 .12 12) "#e8e0d0" x 1.75 z :rot (list (/ pi 2) 0 0))
  (shape g :box '(.62 .3 .3) "#d8d4c4" x 1.1 z :shadow nil))

(defun tavern-station (g)
  "Add to G a tavern in the deep forest.
It is a log house with a porch, a lantern and smoke from the chimney, with an old pump in the yard."
  (shape g :box '(16.0 .12 26.0) "#5a5048" 11.0 .06 0 :shadow nil)
  (shape g :box '(9.0 4.2 12.0) "#6a5038" 16.0 2.1 0)
  (dolist (s '(-1 1)) (shape g :box '(6.4 .3 13.0) "#3a2a1e" (+ 16.0 (* s 2.5)) 5.7 0 :rot (list 0 0 (* s -.62))))
  (loop for y from .5 to 4.0 by .7 do (shape g :box '(.1 .08 12.0) "#4a3424" 11.48 y 0 :shadow nil))
  (shape g :box '(3.4 .2 8.0) "#5a3e26" 9.8 3.3 0 :rot '(0 0 .12))
  (dolist (z '(-3.6 3.6)) (shape g :box '(.22 3.2 .22) "#5a3e26" 8.4 1.6 z))
  (shape g :box '(3.0 .3 8.0) "#7a5a3a" 9.9 .3 0)
  (dolist (z '(-2.6 2.6)) (shape g :box '(.14 1.5 1.3) "#e8c070" 11.45 2.3 z :emit "#ffb050" :energy 1.3 :shadow nil))
  (shape g :box '(.14 2.6 1.4) "#3a2a1e" 11.45 1.3 0 :shadow nil)
  (shape g :box '(.9 2.0 .9) "#5a4a44" 18.0 7.2 -3.0) (chimney-smoke g 18.0 8.4 -3.0 :size 1.4)
  (shape g :box '(.2 .2 .2) "#ffd080" 8.4 3.0 3.6 :emit "#ffa040" :energy 3 :shadow nil)
  (shape g :box '(.2 .9 5.0) "#3a2a1e" 8.3 4.3 0 :shadow nil)
  (sign g "ТРАКТИР · БЕНЗИН" 8.18 4.3 0 :h .5 :rgb "#e8c070")
  (old-pump g 6.5 -6.0)
  (loop for i below 7 do (shape g :cyl '(.16 .16 1.1 6) "#7a5a3a" (+ 12.0 (* .34 (mod i 4))) (+ .18 (* .3 (floor i 4))) 8.0 :rot (list (/ pi 2) 0 0)))
  (shape g :cyl '(.7 .7 .12 12) "#4a3424" 12.5 .8 -7.5 :rot (list 0 .3 (/ pi 2))))

(defun swamp-station (g)
  "Add to G the gas station by the swamp: a hut on stilts, a boardwalk and barrels.
Fly swatters are sold only here."
  (shape g :box '(12.0 .14 22.0) "#6a5a48" 9.0 .07 0 :shadow nil)
  (dolist (x '(12.5 18.5)) (dolist (z '(-3.5 3.5)) (shape g :box '(.35 3.4 .35) "#3a2a1e" x .7 z)))
  (shape g :box '(7.6 .3 9.0) "#5a4636" 15.5 2.4 0)
  (shape g :box '(6.4 3.0 7.4) "#5a6a4a" 15.5 4.0 0)
  (dolist (s '(-1 1)) (shape g :box '(4.6 .25 8.4) "#3a3a34" (+ 15.5 (* s 1.8)) 6.3 0 :rot (list 0 0 (* s -.55))))
  (shape g :box '(.12 1.2 1.6) "#e8c070" 12.25 4.2 0 :emit "#b0d060" :energy 1.2 :shadow nil)
  (beam-between g '(11.6 2.5 2.5) '(8.0 .1 2.5) :thick .9 :rgb "#5a4636")
  (sign g "БЕНЗИН У БОЛОТА" 12.2 5.9 0 :h .5 :rgb "#d8d4c4")
  (shape g :box '(.12 .8 3.2) "#3c4638" 4.4 2.6 9.0 :shadow nil) (shape g :box '(.14 2.6 .14) "#3a2a1e" 4.4 1.3 9.0)
  (sign g "МУХОБОЙКИ" 4.3 2.6 9.0 :h .42 :rgb "#e8e060")
  (old-pump g 6.5 -5.0) (old-pump g 6.5 -2.5)
  (dolist (z '(5.0 6.3)) (shape g :cyl '(.5 .5 1.3 10) "#3f6f73" 11.0 .65 z)))

(defun plain-station (g)
  "Add to G an ordinary gas station.
It has a canopy with a red stripe, three pumps, a booth and a price stele."
  (shape g :box '(16.0 .2 28.0) "#55554f" 11.0 .1 0 :shadow nil)
  (shape g :box '(9.0 .5 14.0) "#d8d4c4" 9.5 5.2 0)
  (shape g :box '(9.1 .22 14.1) "#a3262c" 9.5 5.05 0 :shadow nil)
  (dolist (sx '(6.0 13.0)) (dolist (sz '(-5.5 5.5)) (shape g :box '(.3 5.0 .3) "#8a8a84" sx 2.5 sz)))
  (dolist (sz '(-4 0 4))
    (shape g :box '(.7 1.7 .9) (if (zerop sz) "#a8843c" "#a3262c") 9.5 .95 sz)
    (shape g :box '(.72 .4 .6) "#d8d4c4" 9.5 1.45 sz :shadow nil))
  (body g 5.0 3.0 6.0 "#84807a" 17.0 -8)
  (shape g :box '(.12 1.3 3.4) "#d8c890" 16.95 1.8 -8 :emit "#ffd890" :energy .7 :shadow nil)
  (shape g :box '(.3 7.0 .3) "#8a8a84" 4.5 3.5 12)
  (shape g :box '(.3 2.6 2.2) "#2a2c30" 4.5 6.0 12)
  (sign g "АЗС" 4.3 6.6 12 :h .9 :rgb "#d8d4c4") (sign g "76   0.40" 4.3 5.6 12 :h .5 :rgb "#c9a13a")
  (sign g "АЗС" 5.0 5.06 0 :h .8 :rgb "#d8d4c4"))

(defun add-market (d)
  "Build the market by the bus stop in the middle of the town at path coordinate D and return its group.
It is a sign on posts, behind it a paved lot and two rows of stalls under striped awnings. The
stalls display what this town makes, with sellers behind them. This is where we park to trade:
buyers and sellers come up at the sound of the bell (trade.lisp)."
  (let* ((g (lot d 1)) (tw (town-at d)) (makes (and tw (town-makes (town-index tw)))))
    (dolist (sz '(-4.2 -18.6)) (shape g :box '(.14 3.6 .14) "#5a3e26" 3.6 1.8 sz))
    (shape g :box '(.1 .7 14.5) "#2c4a7a" 3.6 3.4 -11.4 :shadow nil)
    (sign g "РЫНОК" 3.54 3.4 -11.4 :h .5 :rgb "#e8e0d0")
    (shape g :box '(15.0 .1 16.0) "#8a8880" 12.0 .32 -11.4 :shadow nil)                 ; paved lot
    (loop for row from 0 below 2
          do (loop for k from 0 below 4
                   do (let* ((x (+ 7.5 (* row 6.5))) (z (+ -17.0 (* k 3.8))) (stall (group g x .37 z))
                             (stripe (nth (mod (+ k row) 3) '("#a3262c" "#2c4a7a" "#3c5a48"))))
                        (shape stall :box '(.9 .9 2.4) "#8a6440" 0 .45 0)                 ; stall counter
                        (shape stall :box '(1.0 .06 2.5) "#6a4a2e" 0 .93 0 :shadow nil)
                        (dolist (pz '(-1.2 1.2)) (shape stall :box '(.08 2.3 .08) "#5a3e26" .55 1.15 pz))
                        (shape stall :box '(1.9 .06 2.8) "#e8e0d0" .1 2.3 0 :rot '(0 0 .12))        ; awning
                        (dotimes (i 3) (shape stall :box '(1.92 .065 .46) stripe .1 2.3 (* (- i 1) .93) :rot '(0 0 .12) :shadow nil))
                        (when makes                                                         ; this town's goods
                          (dotimes (i 3)
                            ;; BUILD-ITEM places a thing 'above the wagon floor', so subtract the floor height
                            (node-3d-set-scale (build-item stall (nth (mod (+ i k) (length makes)) makes)
                                                           -.1 (- .96 *floor*) (* (- i 1) .7) 0)
                                               (v3 1.6 1.6 1.6))))
                        (when (evenp (+ k row))                                             ; seller behind the stall
                          (without-solids
                            (let ((cat (group stall .85 0 0)))
                              (node-3d-set-rotation cat (v3 0 (- (/ pi 2)) 0))
                              (node-3d-set-scale (first (make-cat cat (+ (* 7 k) (* 3 row) (round d)))) (v3 .62 .62 .62)))))
                        (shape stall :box '(.6 .4 .5) "#7a5a3a" 1.4 .2 (if (evenp k) .8 -.8)))))        ; crate by the stall
    (dolist (z '(-19.6 -3.2)) (shape g :box '(.5 .9 .5) "#55554f" 5.2 .8 z))                 ; litter bins at the entrance
    g))

(defun depot (g)
  "Add to G the depot we set out from, a long warehouse with a loading ramp and gates."
  (body g 16.0 7.0 46.0 "#6f757c" 9.0 0)
  (shape g :box '(16.6 .4 46.6) "#4c4e52" 17.0 7.2 0 :shadow nil)
  (shape g :box '(2.5 1.1 40.0) "#55554f" 7.8 .55 0)
  (dolist (z '(-12 0 12)) (shape g :box '(.14 3.6 5.0) (if (zerop z) "#3c4638" "#5a5a58") 8.95 2.9 z :shadow nil))
  (loop for (bx bz) in '((7.4 -16) (7.6 -14.6) (7.3 6) (7.5 17)) do (shape g :box '(.8 .5 .9) "#8a6440" bx 1.35 bz))
  (sign g "СКЛАД № 3" 8.85 5.6 0 :h 1.2 :rgb "#d8d4c4"))

(defun parked-car (g x z)
  "Add to G at (X, Z) a car parked at the kerb."
  (let ((rgb (nth (random 5) '("#8a4038" "#3f6f73" "#b0aa98" "#55708a" "#a8843c"))))
    (shape g :box '(1.6 .6 4.0) rgb x .65 z) (shape g :box '(1.5 .55 2.0) rgb x 1.2 (+ z .2))
    (shape g :box '(1.52 .4 1.9) "#1a2026" x 1.22 (+ z .2) :shadow nil)
    (dolist (sx '(-.8 .8)) (dolist (sz '(-1.3 1.3)) (shape g :cyl '(.32 .32 .2 8) "#141416" (+ x sx) .32 (+ z sz) :rot (list 0 0 (/ pi 2)))))))

(defun kvas (g x z)
  "Add to G at (X, Z) a kvass barrel on wheels."
  (shape g :cyl '(.9 .9 2.6 12) "#c9a13a" x 1.3 z :rot (list (/ pi 2) 0 0))
  (dolist (sz '(-.7 .7)) (shape g :cyl '(.4 .4 .2 8) "#141416" (+ x .9) .4 (+ z sz) :rot (list 0 0 (/ pi 2))))
  (sign g "КВАС" (- x .92) 1.4 z :h .6 :rgb "#6a3a2e"))

;;; Laying out the buildings

(defun zone-at (tw d)
  (let ((f (/ (- d (town-start tw)) (- (town-end tw) (town-start tw)))) (zone :village))
    (loop for (from what) in *zones* do (when (>= f from) (setf zone what)))
    (if (eq (town-theme tw) :village) :village zone)))          ; a village-like town is nothing but log houses

(defun lot (d side)
  "Return the group of a lot at the roadside at path coordinate D, on side SIDE.
Its x+ points away from the road."
  ;; the land here is a lot (vista.lisp): slightly below the road on the right; level with it on the
  ;; left, beyond the oncoming lane
  (destructuring-bind (x y z) (road-pos d (if (< side 0) -6.8 3.2) (if (< side 0) .02 -.35))
    (let ((g (keep (group *world* x y z) d)) (course (road-frame d)))
      (node-3d-set-rotation g (v3 0 (- (if (< side 0) pi 0) course) 0))
      g)))

(defun build-lot (tw d side)
  "Build up the lot of town TW that starts at path coordinate D on side SIDE, according to the town zone.
Return the length of the lot."
  (let ((zone (zone-at tw d)))
    (flet ((at (len) (lot (+ d (/ len 2)) side)))
      (ecase zone
        (:village (izba (at 17.0) 6.0 0) 17.0)
        (:garages
         (if (< side 0)
             (progn (factory (at 64.0) 64.0) 64.0)
             (progn (garages (at 30.0) 30.0) 34.0)))
        (:blocks5
         (if (zerop (random 2))
             (progn (end-on-blocks (at 104.0) 5 3) 104.0)
             (let ((g (at 70.0)))
               (panel-house g 5 12.0 62.0 12.0 0)
               (when (zerop (random 2)) (parked-car g 1.6 (- (random 40.0) 20)))
               (when (zerop (random 4)) (kvas g 3.0 26))
               70.0)))
        (:centre
         (let ((n (random 3)))
           (cond ((and (< side 0) (not (town-plaza tw)))               ; the square with the house of culture: one per town
                  (setf (town-plaza tw) t)
                  (let ((g (at 62.0))) (plaza g) (culture-house g) 62.0))
                 ((= n 0) (let ((g (at 30.0))) (shop g (nth (random (length *shop-names*)) *shop-names*)) (parked-car g 1.6 6) 30.0))
                 (t (let ((g (at 70.0))) (panel-house g 5 12.0 62.0 12.0 0) (shop-front g) 70.0)))))
        (:blocks9
         (case (random 4)
           (0 (tower (at 34.0)) 34.0)
           (1 (end-on-blocks (at 104.0) 9 3) 104.0)
           (t (let ((g (at 76.0)))
                (panel-house g 9 12.0 66.0 16.0 0 :slogan (when (zerop (random 3)) (nth (random (length *slogans*)) *slogans*)))
                (when (zerop (random 2)) (parked-car g 1.6 (- (random 40.0) 20)))
                76.0))))))))

(defun shop-front (g)
  "Add to G a shop on the ground floor of an apartment block."
  (shape g :box '(.16 2.0 24.0) "#d8c890" 11.9 1.4 0 :emit "#ffd890" :energy .6 :shadow nil)
  (shape g :box '(.2 .8 12.0) "#3f6f73" 11.85 2.9 0 :shadow nil)
  (sign g (nth (random (length *shop-names*)) *shop-names*) 11.7 2.9 0 :h .7))

(defvar *built-towns* (make-hash-table) "The map from a town index to the town's nodes.
A town is built as a whole and removed as a whole.")

(defun build-town (tw)
  "Build up the town TW on both sides of the road, from its start to its end, and return its nodes.
Gas stations are on the right, as planned: a house that would overlap a gas station is not built.
In towns with a character of their own a gap is left on the right (CITY-GAP)."
  (setf (town-plaza tw) nil)
  (with-solids
   (let ((*keep-into* (list nil)) (*mosaic-town* (town-index tw)) (*mosaic-count* 0))
    (dotimes (i 2)
      (let ((side (if (zerop i) -1 1)) (d (+ (town-start tw) 4.0)))
        (loop while (< (+ d 20) (town-end tw))
              do (let* ((*seed* (rs-for (+ (* 2 d) i)))
                        (station (and (> side 0) (let ((s (next-station (- d 18)))) (and s (< s (+ (town-end tw) 40)) s))))
                        (gap (and (> side 0) (city-gap tw d))))
                   (setf d (cond ((and station (<= (- station 17.5) d)) (+ station 23))         ; the gas station lot is 34 m
                                 (gap gap)                                                      ; shows what makes the town special
                                 ((and station (< (- station 17 d) 110)) (- station 17))        ; the longest lot is 104 m
                                 (t (+ d (build-lot tw d side) 6 (random 8.0)))))))))
    (or (cdr *keep-into*) (list nil)))))

(defun step-city ()
  "Build and remove gas stations and towns as the wagon moves.
Gas stations are built from the list as soon as they come into view; towns are built as a whole
when they are *CITY-AHEAD* away and removed as a whole."
  (dolist (s *stations*)
    (listed ((list :station s) s *city-ahead*) (gas-station (lot s 1) s)))
  (dolist (tw *towns*)
    (let* ((i (town-index tw)) (built (gethash i *built-towns*))
           (near (span-in-window-p (town-start tw) (town-end tw) *city-ahead*)))
      (cond ((and near (null built)) (setf (gethash i *built-towns*) (build-town tw)))
            ((and built (not near) (out-of-window-p (/ (+ (town-start tw) (town-end tw)) 2)
                                                    (+ *city-ahead* (/ (- (town-end tw) (town-start tw)) 2) 30)))
             (dolist (n built) (when n (node-queue-free n)))
             (remhash i *built-towns*))))))

;;; Gas stations and the tank

(defvar *fuel* 1.0 "The fuel tank level; 1 means full.")
(defvar *at-station* nil "True while we stand at a gas station.")
(defvar *money* 10 "The number of roubles in the pocket.")

(defshared pour-fuel :fuel ()
  "Fill up for *FUEL-CHUNK* roubles, or for as much as the money and the room in the tank allow.
The wallet is shared, so anyone can refuel."
  (when *at-station*
    (let* ((room (ceiling (* *tank-price* (- 1 *fuel*)))) (pay (min *fuel-chunk* (+ *money* *credit*) room))
           (from-account nil))                                             ; true if the account paid, wholly or in part
      (cond ((<= room 0) (say "Бак полный"))
            ((<= pay 0) (say "Денег нет. Развези посылки"))
            (t (setf from-account (and (not *dev*) (plusp (min *credit* pay))))
               (pay-by-transfer pay) (setf *fuel* (min 1.0 (+ *fuel* (/ pay *tank-price*))) *fuel-told* nil)
               (log-event :fuel :pay pay :fuel *fuel* :money *money* :dist (round *dist*))
               (say-all (if from-account
                            (format nil "Залили на ~/kiosk-trip::rub/ — со счёта кооператива. Хватит ещё на ~,1f км" pay (* *fuel* *tank-km*))
                            (format nil "Залили на ~/kiosk-trip::rub/ Хватит ещё на ~,1f км" pay (* *fuel* *tank-km*))))
               (let ((line (person-speaks :fuel))) (when line (say-later line 5.0))))))))                       ; the dispatcher on the air, a driver at the next pump

(defun buy-fuel () "Buy fuel on R at a gas station; return T if we are at one." (when *at-station* (pour-fuel) t))

(defshared take-planks :planks ()
  "Buy a bundle of planks for the stove."
  (when *at-station*
    (destructuring-bind (n price) *plank-price*
      (cond ((< *money* price) (say "На доски не хватает"))
            (t (decf *money* price) (incf *planks* n) (say-all (format nil "Купили досок: ~d за ~/kiosk-trip::rub/" n price)))))))

(defun buy-planks () "Buy planks on P at a gas station; return T if we are at one." (when *at-station* (take-planks) t))
(defvar *fuel-told* nil)
(defvar *fuel-out* nil "True while the tank is empty, so that this is written to the log only once.")

(defshared cheat-fuel :tank ()
  "Fill the tank for free; the F9 cheat."
  (setf *fuel* 1.0 *fuel-told* nil)
  (log-event :cheat :what :fuel :money *money* :dist (round *dist*))
  (say-all "Чит: бак полный"))

(defshared cheat-money :money ()
  "Add a hundred roubles to the shared wallet for free; the F8 cheat."
  (incf *money* 100)
  (log-event :cheat :what :money :money *money* :dist (round *dist*))
  (say-all (format nil "Чит: +5 р. В кошельке ~/kiosk-trip::rub/" *money*)))

(defun step-fuel (delta)
  (setf *fuel* (max 0.0 (- *fuel* (/ (* *speed* delta) (* 1000 *tank-km*)))))
  (when (<= *fuel* 0.0)
    (unless *fuel-out* (setf *fuel-out* t) (log-event :fuel-out :money *money* :dist (round *dist*) :next-station (next-station *dist*)))
    (setf *speed* (min *speed* 2.5)))
  (when (> *fuel* .05) (setf *fuel-out* nil))                       ; empty tank: we coast along, barely moving
  (when (and (< *fuel* .15) (not *fuel-told*)) (setf *fuel-told* t) (say "Бак почти пуст. Ищи заправку"))
  )

(defun step-station ()
  "Track whether we stand at a gas station.
Once we stop there the ladder lowers by itself: only at such places can one get out and walk around."
  (let ((at (and (find-if (lambda (d) (< (abs (- d *dist*)) 30)) *stations*) (< *speed* .4))))
    (when (and at (not *at-station*))
      (unless (net-guest-p) (setf *ladder-down* t))
      (say (if (net-guest-p)
               "Заправка. Лесенка опущена — можно выйти"
               (format nil "Заправка. R — бензин на ~/kiosk-trip::rub/ · P — ~d доски за ~/kiosk-trip::rub/ Лесенка опущена — можно выйти"
                       *fuel-chunk* (first *plank-price*) (second *plank-price*)))))
    (setf *at-station* at)
    ;; one can get out not only at a gas station: bus stop, observatory, drop-off point (finds.lisp)
    (let ((place (walk-place)))
      (when (and place (not *walk-here*) (not (eq (first place) :station)))
        (unless (net-guest-p) (when (member (first place) '(:observatory :depot :airmail :gimmick)) (setf *ladder-down* t)))
        (say (ecase (first place)
               (:stop "Остановка. E по лесенке за дверью — опустить её и выйти")
               (:observatory "Обсерватория. Лесенка опущена — выходи: на террасе большой телескоп")
               (:depot "Пункт приёма стеклотары и макулатуры. Лесенка опущена — сдавать у окошка")
               (:airmail "Авиапочта. Лесенка опущена — неси посылку к окошку, улетит самолётом")
               (:gimmick (format nil "~a. Лесенка опущена — подойди к окошку" (third (assoc (third place) *gimmick-plan*)))))))
      (setf *walk-here* (and place t)))))

(defun reset-city ()
  (setf *stations* '() *at-station* nil
        *fuel* *start-fuel* *money* *start-money* *fuel-told* nil))

(defun onboard-fuel ()
  "Set the starting fuel to exactly enough to reach the first town's market.
There we stall by the gas station and work out how to get by."
  (when *towns*
    (setf *fuel* (/ (+ (town-stop-d (first *towns*)) 4.0) (* 1000 *tank-km*)))))
