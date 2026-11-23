;;;; Night sky. The real bright stars of the northern sky (right ascension, declination, magnitude) and
;;;; the constellation lines, plus a scatter of faint stars, denser along the Milky Way. The sky is one
;;;; mesh on a sphere around the wagon: tilted for latitude 55° (the celestial pole is to the left of
;;;; the road, south is on the window side) and turning with the day cycle. The spyglass zooms in on
;;;; what you see and shows the constellation lines; the big telescope at the observatory shows planets
;;;; and nebulae close up.

(in-package #:kiosk-trip)

(defparameter *star-catalog*
  ;; name, right ascension (hours), declination (degrees), magnitude, colour
  '((dubhe 11.06 61.75 1.8 :y) (merak 11.03 56.38 2.4) (phecda 11.90 53.69 2.4) (megrez 12.26 57.03 3.3)
    (alioth 12.90 55.96 1.8) (mizar 13.40 54.93 2.2) (alkaid 13.79 49.31 1.9 :b)
    (polaris 2.53 89.26 2.0 :y) (kochab 14.85 74.16 2.1 :o) (pherkad 15.35 71.83 3.0) (yildun 17.54 86.59 4.4)
    (eps-umi 16.77 82.04 4.2) (zeta-umi 15.73 77.79 4.3) (eta-umi 16.29 75.76 5.0)
    (caph 0.15 59.15 2.3) (schedar 0.68 56.54 2.2 :o) (gamma-cas 0.95 60.72 2.2 :b) (ruchbah 1.43 60.24 2.7) (segin 1.91 63.67 3.4)
    (betelgeuse 5.92 7.41 0.5 :r) (rigel 5.24 -8.20 0.1 :b) (bellatrix 5.42 6.35 1.6 :b) (saiph 5.80 -9.67 2.1 :b)
    (alnitak 5.68 -1.94 1.8 :b) (alnilam 5.60 -1.20 1.7 :b) (mintaka 5.53 -0.30 2.2 :b) (meissa 5.59 9.93 3.4)
    (sirius 6.75 -16.72 -1.5 :b) (mirzam 6.38 -17.96 2.0 :b) (wezen 7.14 -26.39 1.8) (adhara 6.98 -28.97 1.5 :b) (aludra 7.40 -29.30 2.4)
    (procyon 7.66 5.22 0.4 :y) (gomeisa 7.45 8.29 2.9)
    (castor 7.58 31.89 1.6) (pollux 7.76 28.03 1.1 :o) (alhena 6.63 16.40 1.9) (tejat 6.38 22.51 2.9 :r) (mebsuta 6.73 25.13 3.0)
    (aldebaran 4.60 16.51 0.9 :o) (elnath 5.44 28.61 1.7 :b) (zeta-tau 5.63 21.14 3.0) (ain 4.48 19.18 3.5)
    (alcyone 3.79 24.11 2.9 :b) (atlas 3.82 24.05 3.6 :b) (electra 3.75 24.11 3.7 :b) (maia 3.76 24.37 3.9 :b) (merope 3.77 23.95 4.2 :b)
    (capella 5.28 46.00 0.1 :y) (menkalinan 5.99 44.95 1.9) (mahasim 5.99 37.21 2.6) (hassaleh 4.95 33.17 2.7 :o)
    (regulus 10.14 11.97 1.4 :b) (denebola 11.82 14.57 2.1) (algieba 10.33 19.84 2.0 :o) (zosma 11.24 20.52 2.6)
    (chertan 11.24 15.43 3.3) (adhafera 10.28 23.42 3.4) (rasalas 9.88 26.01 3.9) (algenubi 9.76 23.77 3.0)
    (arcturus 14.26 19.18 -0.05 :o) (izar 14.75 27.07 2.4 :o) (seginus 14.53 38.31 3.0) (nekkar 15.03 40.39 3.5)
    (muphrid 13.91 18.40 2.7) (delta-boo 15.26 33.31 3.5) (alphecca 15.58 26.71 2.2)
    (spica 13.42 -11.16 1.0 :b) (porrima 12.69 -1.45 2.7) (vindemiatrix 13.04 10.96 2.8)
    (vega 18.62 38.78 0.0 :b) (sheliak 18.83 33.36 3.5) (sulafat 18.98 32.69 3.2)
    (deneb 20.69 45.28 1.25) (sadr 20.37 40.26 2.2 :y) (albireo 19.51 27.96 3.1 :o) (gienah 20.77 33.97 2.5 :o) (delta-cyg 19.75 45.13 2.9)
    (altair 19.85 8.87 0.8) (tarazed 19.77 10.61 2.7 :o) (alshain 19.92 6.41 3.7)
    (markab 23.08 15.21 2.5) (scheat 23.06 28.08 2.4 :r) (algenib 0.22 15.18 2.8 :b) (alpheratz 0.14 29.09 2.1 :b)
    (mirach 1.16 35.62 2.1 :r) (almach 2.07 42.33 2.2 :o) (enif 21.74 9.88 2.4 :o)
    (mirfak 3.41 49.86 1.8 :y) (algol 3.14 40.96 2.1) (hamal 2.12 23.46 2.0 :o)
    (antares 16.49 -26.43 1.0 :r) (dschubba 16.01 -22.62 2.3 :b) (acrab 16.09 -19.81 2.6 :b)
    (alderamin 21.31 62.59 2.5) (eltanin 17.94 51.49 2.2 :o) (rastaban 17.51 52.30 2.8 :y) (thuban 14.07 64.38 3.7)
    (rasalhague 17.58 12.56 2.1) (kornephoros 16.50 21.49 2.8 :y) (zeta-her 16.69 31.60 2.8)
    (fomalhaut 22.96 -29.62 1.2) (diphda 0.73 -17.99 2.0 :o) (alphard 9.46 -8.66 2.0 :o)
    ;; planets sit on the ecliptic; we do not make them wander here
    (jupiter 4.25 21.2 -2.4 :y) (saturn 23.3 -6.5 0.6 :y) (mars 7.2 24.0 -0.6 :r))
  "The bright stars, the ones the sky is recognised by.")

(defparameter *star-lines*
  '((dubhe merak phecda megrez dubhe) (megrez alioth mizar alkaid)
    (polaris yildun eps-umi zeta-umi kochab pherkad eta-umi zeta-umi)
    (caph schedar gamma-cas ruchbah segin)
    (betelgeuse bellatrix mintaka alnilam alnitak betelgeuse) (alnitak saiph rigel mintaka) (betelgeuse meissa bellatrix)
    (mirzam sirius wezen adhara) (wezen aludra) (procyon gomeisa)
    (castor mebsuta tejat) (castor pollux alhena)
    (zeta-tau aldebaran ain elnath) (capella menkalinan mahasim elnath hassaleh capella)
    (regulus algieba adhafera rasalas algenubi) (regulus chertan denebola zosma algieba) (zosma chertan)
    (muphrid arcturus izar delta-boo nekkar seginus arcturus)
    (spica porrima vindemiatrix) (vega sheliak sulafat vega)
    (deneb sadr albireo) (gienah sadr delta-cyg) (tarazed altair alshain)
    (markab scheat alpheratz algenib markab) (alpheratz mirach almach) (markab enif)
    (mirfak algol) (acrab dschubba antares) (eltanin rastaban))
  "The constellation lines, as chains of names from the catalogue.")

(defparameter *sky-r* 1900.0 "The radius of the star sphere in metres, inside the camera's far plane.")
(defparameter *latitude* 55.0 "The latitude we observe from, which is how high the celestial pole stands above the horizon.")

(defvar *sky-tilt* nil "The node that tilts the celestial axis.")
(defvar *sky-spin* nil "The node under *SKY-TILT* that carries the daily rotation.")
(defvar *star-mat* nil)
(defvar *line-node* nil "The constellation lines node, visible through the spyglass.")
(defvar *zoom* nil "True while the player looks through the spyglass.")
(defvar *scope* nil "The index of the object shown while the big telescope is open, or NIL.")
(defvar *scope-parts* nil)

(defun star-dir (ra dec)
  "Return the unit vector of a star at right ascension RA and declination DEC.
The result is a list (X Y Z) in sky axes, with y pointing to the celestial pole."
  (let ((a (* ra (/ pi 12))) (d (* dec (/ pi 180))))
    (list (* (cos d) (cos a)) (sin d) (* (cos d) (sin a)))))

(defun star-quad (st dir size col)
  "Add to ST a star quad of SIZE and colour COL on the sphere along DIR, facing the centre."
  (destructuring-bind (x y z) dir
    (let* ((ux (- z)) (uz x) (ul (max 1e-4 (sqrt (+ (* ux ux) (* uz uz)))))        ; u is sideways, in the equatorial plane
           (ux (/ ux ul)) (uz (/ uz ul))
           (vx (- (* y uz))) (vy (- (* z ux) (* x uz))) (vz (* y ux))               ; v = dir × u
           (r *sky-r*) (uv (vector2 0 0)))
      (flet ((p (a b) (v3 (+ (* x r) (* a size ux) (* b size vx)) (+ (* y r) (* b size vy)) (+ (* z r) (* a size uz) (* b size vz)))))
        (object-call st "add_triangle_fan" (vector (p -1 1) (p 1 1) (p 1 -1) (p -1 -1)) (vector uv uv uv uv) (vector col col col col))))))

(defun star-line (st a b col)
  "Add to ST a thin strip of colour COL between the star directions A and B."
  (let* ((r (- *sky-r* 8)) (mx (+ (first a) (first b))) (my (+ (second a) (second b))) (mz (+ (third a) (third b)))
         (dx (- (first b) (first a))) (dy (- (second b) (second a))) (dz (- (third b) (third a)))
         (nx (- (* my dz) (* mz dy))) (ny (- (* mz dx) (* mx dz))) (nz (- (* mx dy) (* my dx)))   ; across the line, along the tangent
         (nl (max 1e-6 (sqrt (+ (* nx nx) (* ny ny) (* nz nz))))) (w (/ 1.6 nl)) (uv (vector2 0 0)))
    (flet ((p (s k) (v3 (+ (* r (first s)) (* k w nx)) (+ (* r (second s)) (* k w ny)) (+ (* r (third s)) (* k w nz)))))
      (object-call st "add_triangle_fan" (vector (p a -1) (p a 1) (p b 1) (p b -1)) (vector uv uv uv uv) (vector col col col col)))))

(defun sky-material ()
  (let ((m (gdext:make-object "StandardMaterial3D")))
    (object-set m "shading_mode" 0)
    (object-set m "vertex_color_use_as_albedo" t)
    (object-set m "transparency" 1)
    (object-set m "cull_mode" 2)
    (object-set m "disable_fog" t)
    m))

(defun make-stars (root)
  (let ((st (gdext:make-object "SurfaceTool")) (lt (gdext:make-object "SurfaceTool"))
        (mi (gdext:make-object "MeshInstance3D")) (li (gdext:make-object "MeshInstance3D")))
    (setf *sky-tilt* (group root) *sky-spin* (group *sky-tilt*))
    (node-3d-set-rotation *sky-tilt* (v3 0 0 (* (- 90 *latitude*) (/ pi 180))))      ; celestial pole to the left of the road
    (object-call st "begin" 3) (object-call lt "begin" 3)
    ;; the bright catalogue stars
    (loop for (nil ra dec mag tint) in *star-catalog*
          do (let ((c (hex (case tint (:b "#bcd0ff") (:y "#fff0c0") (:o "#ffd0a0") (:r "#ffa890") (t "#f4f4ff"))
                           (max .6 (min 1.0 (- 1.1 (* .12 mag)))))))
               (star-quad st (star-dir ra dec) (max 4.2 (- 10.5 (* 1.7 mag))) c)))
    ;; faint ones: all over the sky and denser along the Milky Way (a great circle tilted to the equator)
    (dotimes (i 900)
      (let* ((milky (< i 420)) (ra (* 24 (vr i 81)))
             (dec (if milky
                      (+ (* 62 (sin (* (- ra 18.8) (/ pi 12)))) (* 16 (- (+ (vr i 82) (vr i 83)) 1.0)))
                      (* (/ 180 pi) (asin (- (* 2 (vr i 84)) 1.0)))))
             (b (+ .3 (* .45 (vr i 85)))))
        (star-quad st (star-dir ra (max -89.0 (min 89.0 dec))) (+ 2.6 (* 1.5 (vr i 86))) (color .86 .9 1.0 (if milky (* b .8) b)))))
    (object-call st "generate_normals")
    (object-set mi "mesh" (object-call st "commit"))
    ;; constellation lines
    (dolist (chain *star-lines*)
      (loop for (a b) on chain while b
            do (let ((sa (assoc a *star-catalog*)) (sb (assoc b *star-catalog*)))
                 (when (and sa sb)
                   (star-line lt (star-dir (second sa) (third sa)) (star-dir (second sb) (third sb)) (color .5 .7 1.0 .55))))))
    (object-call lt "generate_normals")
    (object-set li "mesh" (object-call lt "commit"))
    (gdext:setf-reference *star-mat* (sky-material))
    (dolist (n (list mi li))
      (object-set n "material_override" *star-mat*)
      (object-set n "cast_shadow" 0)
      (node-add-child *sky-spin* n))
    (object-set li "visible" nil)
    (setf *line-node* li)))

(defun step-stars ()
  "Update the night sky for the current frame.
The sky stays over the wagon, turns with the day cycle and fades towards dawn and under clouds."
  (when *sky-tilt*
    (destructuring-bind (x y z) *rig-at* (node-3d-set-position *sky-tilt* (v3 x y z)))
    ;; at midnight Orion stands in the south (on the window side): right ascension 6 hours
    (node-3d-set-rotation *sky-spin* (v3 0 (+ (/ pi 2) (* 2 pi (/ (- (day-phase (sky-now)) (* .5 (+ (second *dusk*) *day-len*))) *day-len*))) 0))
    (let ((night (* (max 0.0 (min 1.0 (/ (- .42 (daylight (sky-now))) .3))) (- 1 (* .85 (wx :cloud))) (- 1.0 *gloom*) (if *meta* 0 1))))
      (object-set *sky-tilt* "visible" (> night .02))
      (object-set *star-mat* "albedo_color" (color 1 1 1 night)))
    (object-set *line-node* "visible" (and *zoom* t))))

;;; ------------------------------------------------------------------ spyglass

(defparameter *spyglass-at* '(.85 1.78 2.76) "The position of the spyglass, which hangs on the back wall by the door, above the light switch.")
(defvar *spyglass-node* nil)

(defun show-spyglass () (when *spyglass-node* (object-set *spyglass-node* "visible" (and *spyglass* (not (eq *tool* :spyglass))))))

(defun make-spyglass (c)
  (destructuring-bind (x y z) *spyglass-at*
    (let ((g (group c x (+ *floor* y) z)))
      (model g "spyglass")
      (setf *spyglass-node* g)
      (show-spyglass))))

;;; ------------------------------------------------------------------ big telescope

(defvar *scope-sights* '())

(defun make-scope (root)
  (multiple-value-bind (bg layer) (screen-panel 6 0 0 640 360 '(0 0 0 1) root)
   (let ((pic (gdext:make-object "TextureRect")))
    (node-add-child bg pic)
    (object-set pic "position" (vector2 170 20)) (object-set pic "size" (vector2 300 300))
    (object-set pic "expand_mode" 1) (object-set pic "stretch_mode" 5) (object-set pic "texture_filter" 1)
    (object-set layer "visible" nil)
    (setf *scope-parts* (list :layer layer :pic pic
                              :caption (hud-label bg "" 170 326 10 '(.92 .88 .76))
                              :hint (hud-label bg "ЛКМ или E — следующий объект · S или Esc — отойти от телескопа" 170 344 7 '(.6 .6 .56)))))))

(defun show-sight ()
  (destructuring-bind (file caption) (nth *scope* *scope-sights*)
    (let ((path (format nil "res://textures/scope/~a.png" file)))
      (let ((tx (picture-texture path)))
        (when tx (object-set (getf *scope-parts* :pic) "texture" tx))))
    (object-set (getf *scope-parts* :caption) "text" caption)))

(defun open-scope ()
  (cond ((>= (daylight (sky-now)) .3) (say "Днём звёзд не видно. Дождись ночи — или усни в кибитке до темноты"))
        ((> (wx :cloud) .6) (say "Небо в тучах: в телескоп ничего не видно"))
        (t (setf *scope* 0)
           (object-set (getf *scope-parts* :layer) "visible" t)
           (show-sight))))

(defun close-scope ()
  (setf *scope* nil)
  (object-set (getf *scope-parts* :layer) "visible" nil))

(defun scope-input (event)
  "Handle the input EVENT while the player looks through the telescope.
Left click and E step through the objects; S and Esc step away."
  (cond ((and (object-call event "is_class" "InputEventMouseButton") (object-get event "pressed"))
         (setf *scope* (mod (1+ *scope*) (length *scope-sights*))) (show-sight))
        ((and (key-press-p event))
         (let ((k (key-code event)))
           (cond ((member k '(83 4194305)) (close-scope))
                 ((member k '(69 32)) (setf *scope* (mod (1+ *scope*) (length *scope-sights*))) (show-sight)))))))

(defun stars-usables ()
  (when *spyglass*
    (list (usable (first *spyglass-at*) (second *spyglass-at*) (third *spyglass-at*)
                  (lambda () (if (eq *tool* :spyglass) "E — повесить подзорную трубу" "E — взять подзорную трубу"))
                  (lambda () (take-tool :spyglass))))))
