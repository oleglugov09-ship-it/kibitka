;;;; Towns from afar and in depth: the second rows of houses with murals (BUILD-SKYLINE) and what makes a town
;;;; unlike the others (BUILD-THEME by *TOWN-THEMES*). The first row right by the street is built by city.lisp.

(in-package #:kiosk-trip)

;;; ------------------------------------------------------------------ the second rows of a town

(defvar *mural-mats* (make-hash-table :test 'equal) "The table mapping (town variant) to the material with the mural.")

(defun mural-material (town variant)
  "Return the material with the mosaic of the town TOWN.
VARIANT is :a for a scene or :b for an ornament (drawn by tools/make-murals.py)."
  (let ((key (list town variant)))
    (or (gethash key *mural-mats*)
        (let ((path (format nil "res://textures/murals/~d-~(~a~).png" town variant)))
          (when (file-here-p path)
            (let ((img (gdext:make-object "Image")) (tx (gdext:make-object "ImageTexture")) (m (gdext:make-object "StandardMaterial3D")))
              (object-call img "load_png_from_buffer" (file-bytes path))
              (object-call img "generate_mipmaps")
              (object-call tx "set_image" img)
              (object-set m "albedo_texture" tx)
              (object-set m "texture_filter" 2)                  ; the tiles are point-sampled; at a distance they smooth out
              (object-set m "roughness" 1.0)
              (object-set m "metallic_specular" .1)
              (object-set m "albedo_color" (color .72 .7 .68 1.0))   ; faded: the mural must not shout
              (gdext:setf-reference (gethash key *mural-mats*) m)
              m))))))

(defun mural-quad (parent x y z facing height town variant)
  "Place a mural of height HEIGHT (3:4) centred at (X Y Z) under PARENT.
FACING is ±1, the direction along x that it faces. Return the mesh instance, or NIL if there is no material."
  (let ((mat (mural-material town variant)))
    (when mat
      (let ((mi (gdext:make-object "MeshInstance3D")) (mesh (gdext:make-object "QuadMesh")))
        (object-set mesh "size" (vector2 (* height .75) height))
        (object-set mi "mesh" mesh)
        (object-set mi "material_override" mat)
        (object-set mi "cast_shadow" 0)
        (node-add-child parent mi)
        (node-3d-set-position mi (v3 x y z))
        (node-3d-set-rotation mi (v3 0 (* facing (/ pi 2)) 0))
        mi))))

(defun mural (b face h len town variant)
  "Place a mural across the whole wall of the house B.
FACE is the x of its facade (signed); H and LEN are the height and the length of the house."
  (let ((mh (min (- h 2.5) (* len .85 (/ 4 3)) 10.0)) (out (if (< face 0) -1 1)))
    (mural-quad b (+ face (* .12 out)) (+ 1.6 (/ mh 2)) 0 out mh town variant)))

(defun far-house (g x d w h len rgb &optional mural)
  "Build a second-row house: a W×H×LEN box on the ground at (X, D) with a few window strips and lights.
MURAL is (town . variant) of a mural. Return the house group."
  (destructuring-bind (px py pz) (side-point x d 0.0)
    (let ((b (group g px py pz)) (face (if (> x 0) (- (/ w 2)) (/ w 2))))
      (node-3d-set-rotation b (v3 0 (- (road-frame d)) 0))
      (shape b :box (list w (+ h 8) len) rgb 0 (- (/ h 2) 4) 0)
      (shape b :box (list (+ w .4) .5 (+ len .4)) "#4c4e52" 0 (+ h .25) 0 :shadow nil)
      (loop for y from 4.0 below (- h 1) by 5.6
            do (unless mural (shape b :box (list .14 1.3 (* len .92)) "#2e333b" face y 0 :shadow nil)))
      (when mural (mural b face h len (car mural) (cdr mural)))
      (dotimes (i (if mural 0 (+ 2 (floor (* 4 (vr x d))))))
        (shape b :box '(.18 1.2 1.4) "#e8c070" (* face 1.01) (+ 1.6 (* 2.8 (floor (* (vr d i) (/ h 2.8)))))
               (* len .85 (- (vr i d) .5)) :emit "#ffc060" :energy 1.2 :shadow nil))
      b)))

(defun build-skyline (tw)
  "Build the second rows of the town TW: houses behind the first line, smoking chimneys, a tower.
They are visible from afar, so the town rises as a silhouette in advance instead of growing out of the haze.
Return the group."
  (let ((g (group *world*)) (n 0) (murals 0))                          ; murals — no more than two per town
    (loop for d from (+ (town-start tw) 50) below (- (town-end tw) 50) by 62
          do (dolist (side '(-1 1))
               (incf n)
               (let ((u (vr d side)) (x (* side (+ 84 (* 26 (vr side d))))))
                 (case (if (and (> side 0) (or (city-gap tw d) (eq (town-theme tw) :river))) :open (zone-at tw d))   ; no second rows in the gap — the town's main sight is there
                   (:village (when (< u .3) (far-house g x d 7.0 3.4 9.0 "#6a5a48")))
                   (:garages
                    (cond ((< u .22)                                   ; a boiler house with a chimney
                           (far-house g x d 14.0 9.0 22.0 "#7a5a50")
                           (destructuring-bind (px py pz) (side-point x (+ d 18) 0.0)
                             (let ((c (group g px py pz)))
                               (shape c :cyl '(1.0 1.7 44.0 10) "#8a8580" 0 20.0 0)
                               (dotimes (i 3) (shape c :cyl '(1.1 1.2 2.4 10) "#a3262c" 0 (+ 30 (* i 5)) 0 :shadow nil))
                               (red-light c 0 44.6 0)
                               (chimney-smoke c 0 45.5 0 :size 5.5 :amount 14 :life 12.0))))
                          ((< u .7) (far-house g x d 16.0 8.0 44.0 "#6f757c"))))
                   ((:blocks5 :centre)
                    (when (< u .8)
                      (far-house g x d 12.0 14.0 52.0 (nth (mod n 3) '("#84807a" "#8a7d6e" "#7b7d82"))
                                 (when (and (> side 0) (< murals 2) (< (vr d 41) .5)) (cons (town-index tw) (if (zerop (1- (incf murals))) :a :b))))))
                   (:blocks9
                    (cond ((< u .3) (far-house g x d 16.0 44.8 16.0 "#8e8a82"))
                          ((< u .9) (far-house g x d 12.0 25.2 52.0 (nth (mod n 3) '("#7b7d82" "#84807a" "#8a8478"))))))))))
    (let ((*seed* (rs-for (+ 7000 (town-index tw))))) (build-theme g tw))
    ;; a TV tower — in every second town, on the side away from the window
    (when (evenp (town-index tw))
      (let ((d (/ (+ (town-start tw) (town-end tw)) 2)))
        (destructuring-bind (px py pz) (side-point -118 d 0.0)
          (let ((c (group g px py pz)))
            (dolist (s '((1 1) (1 -1) (-1 1) (-1 -1)))
              (beam-between c (list (* 5 (first s)) -2.0 (* 5 (second s))) (list (* .6 (first s)) 40.0 (* .6 (second s))) :thick .3 :rgb "#8a8e90"))
            (shape c :box '(1.0 30.0 1.0) "#b0342c" 0 55.0 0 :shadow nil)
            (shape c :box '(.4 14.0 .4) "#e0dcd0" 0 77.0 0 :shadow nil)
            (shape c :cyl '(2.2 2.2 1.2 10) "#8a8e90" 0 40.6 0 :shadow nil)
            (red-light c 0 84.3 0 .5) (red-light c .9 41.5 0)))))
    g))

;;; ------------------------------------------------------------------ the character of a town

(defun spot (g x d &optional (up 0.0))
  "Return a group under G on the ground at (X sideways, path coordinate D), turned along the road.
Its x+ points away from the road."
  (destructuring-bind (px py pz) (side-point x d up)
    (let ((b (group g px py pz)))
      (node-3d-set-rotation b (v3 0 (- (road-frame d)) 0))
      b)))

(defvar *themes* '() "The list of what makes a town special: (character . builder-function). Filled by DEFTHEME.")

(defmacro deftheme (key (g tw d0 len mid) &body body)
  "Define the builder of the character KEY.
G is the town group, TW is the town, D0 is its start, LEN is its length and MID is its middle;
inside, (at fraction) is the path coordinate at that fraction of the town length."
  (let ((name (intern (format nil "THEME-~a" key))))
    `(progn (defun ,name (,g ,tw)
              (let* ((,d0 (town-start ,tw)) (,len (- (town-end ,tw) ,d0)) (,mid (+ ,d0 (/ ,len 2))))
                (declare (ignorable ,d0 ,len ,mid))
                (flet ((at (f) (+ ,d0 (* f ,len))))
                  (declare (ignorable #'at))
                  ,@body)))
            (setf *themes* (cons (cons ,key ',name) (remove ,key *themes* :key #'car))))))

(deftheme :military (g tw d0 len mid)
  (let ((b (spot g 62 mid)))                                      ; a military unit behind a fence, right by the street
       (shape b :box '(.3 2.6 220.0) "#3c4638" -22 1.3 0)
       (loop for z from -90 to 90 by 60 do (shape b :box '(12.0 5.0 40.0) "#5a6048" 0 2.5 z)
                                            (shape b :box '(12.6 .5 40.6) "#3c4638" 0 5.2 z :shadow nil))
       (dolist (z '(-105 105))                                       ; watchtowers
         (dolist (s '((-1 -1) (-1 1) (1 -1) (1 1)))
           (beam-between b (list (+ -20 (* 1.4 (first s))) -1 (+ z (* 1.4 (second s)))) (list (+ -20 (* .8 (first s))) 9 (+ z (* .8 (second s)))) :thick .2 :rgb "#4a4038"))
         (shape b :box '(2.6 2.2 2.6) "#4a5038" -20 10.1 z) (shape b :cone '(2.2 1.2 4) "#3c4638" -20 11.8 z :rot (list 0 (/ pi 4) 0)))
       (loop for i below 6 do (let ((z (- (* i 7.0) 17)))            ; vehicles in a row
                                (shape b :box '(2.6 2.2 6.0) "#4a5a3a" -12 1.5 z) (shape b :box '(2.4 1.5 1.8) "#3c4a30" -12 1.3 (- z 3.6))))
       (dolist (z '(-80 80)) (light-beam b -18 10.5 z 150.0 :sweep 1.1 :tilt .9 :speed (if (< z 0) .23 .31) :wide 7.0))   ; searchlights sweep the sky
       (shape b :box '(.6 9.0 .6) "#8a8a84" 14 4.5 0)
       (moving (shape b :cyl '(3.0 3.0 .25 12) "#8a9080" 14 9.6 0 :rot (list (- (/ pi 2) .3) 0 0)) :spin :y .7))
     (let ((b (spot g 24 (at .3))))                                  ; a tank on a pedestal
       (shape b :box '(5.0 2.0 8.0) "#85857f" 0 1.0 0)
       (shape b :box '(3.2 1.0 6.4) "#4a5a3a" 0 2.5 0) (shape b :cyl '(1.2 1.4 .9 10) "#4a5a3a" 0 3.4 .4)
       (shape b :cyl '(.14 .14 4.6 8) "#3c4a30" 0 3.5 -2.6 :rot (list (/ pi 2) 0 0))
       (shape b :box '(.14 .7 .7) "#a3262c" -1.62 2.6 0 :shadow nil)))

(deftheme :factory (g tw d0 len mid)
  (dolist (f '(.2 .46 .72))                                      ; cooling towers and chimneys
       (let ((b (spot g 150 (at f))))
         (shape b :cyl '(9.0 13.0 34.0 16) "#9a968a" 0 17.0 0) (shape b :cyl '(10.0 9.0 8.0 16) "#9a968a" 0 38.0 0)
         (chimney-smoke b 0 43.0 0 :size 9.0 :amount 12 :life 14.0 :rgb "#e4e4e8")
         (shape b :cyl '(1.2 2.0 60.0 10) "#8a8580" 34 30.0 20) (red-light b 34 60.6 20)
         (dotimes (i 3) (shape b :cyl '(1.3 1.4 3.0 10) "#a3262c" 34 (+ 40 (* i 7)) 20 :shadow nil))
         (chimney-smoke b 34 62.0 20 :size 6.0 :amount 14 :life 12.0)))
     (let ((b (spot g 96 (at .58))))
       (shape b :ball '(9.0 14 10) "#b8b4a8" 0 10.0 0) (dotimes (i 6) (shape b :box '(.4 10.0 .4) "#6e6e6a" (* 7 (cos i)) 4.0 (* 7 (sin i)))))
     (let* ((d (at .5)) (b (road-group g d)))   ; a pipeline arching over the street
       (dolist (x '(-11 7)) (shape b :box '(.5 9.0 .5) "#6e6e6a" x 4.5 0) (shape b :box '(.5 9.0 .5) "#6e6e6a" x 4.5 2.4))
       (dolist (z '(.4 1.2 2.0)) (shape b :cyl '(.32 .32 150.0 8) (if (= z 1.2) "#c9a13a" "#8a8e92") -2 9.0 z :rot (list 0 0 (/ pi 2))))))

(deftheme :university (g tw d0 len mid)
  (let ((b (spot g 112 mid)))                                     ; the main building: wings, a tower, a spire
       (shape b :box '(22.0 20.0 110.0) "#c8bca0" 0 10.0 0)
       (shape b :box '(26.0 34.0 34.0) "#d0c4a8" 0 17.0 0) (shape b :box '(18.0 16.0 20.0) "#d0c4a8" 0 42.0 0)
       (shape b :box '(10.0 10.0 10.0) "#d8ccb0" 0 55.0 0) (shape b :cone '(3.5 18.0 8) "#c9a13a" 0 69.0 0 :metal .5 :rough .4)
       (red-light b 0 78.4 0)
       (loop for z from -12 to 12 by 4 do (shape b :cyl '(.8 .8 12.0 10) "#e8e0d0" -14 6.0 z))
       (shape b :box '(4.0 1.5 30.0) "#d8ccb0" -14 12.8 0)
       (loop for y from 5 to 17 by 4 do (shape b :box '(.2 1.6 100.0) "#2e333b" -11.05 y 0 :shadow nil))
       (sign b "УНИВЕРСИТЕТ" -16.1 12.8 0 :h 1.6 :rgb "#6a3a2e")
       (flood b -24 4.0 0 :range 60.0 :energy 3.5))
     (let ((b (spot g 84 (at .28))))                                  ; the observatory
       (shape b :cyl '(7.0 7.0 9.0 14) "#c8bca0" 0 4.5 0) (shape b :ball '(7.0 14 8) "#e8e8e4" 0 9.0 0)
       (shape b :box '(1.6 5.0 .3) "#2e333b" -4.5 12.0 0 :rot '(0 0 .7) :shadow nil))
     (let ((b (spot g 86 (at .74))))                                  ; dormitories with a mural
       (shape b :box '(12.0 30.0 40.0) "#8e8a82" 0 15.0 0)))

(deftheme :port (g tw d0 len mid)
  (loop for d from (+ d0 20) below (town-end tw) by 60             ; the sea
           do (let ((b (apply #'group g (road-pos d 640 -2.2))))
                (node-3d-set-rotation b (v3 0 (- (road-frame d)) 0))
                (shape b :box '(1000.0 .2 64.0) *water-rgb* 0 0 0 :rough .3 :emit "#0c2238" :energy .6 :shadow nil)))
     (dolist (f '(.25 .45 .65))                                      ; gantry cranes
       (let ((b (spot g 122 (at f))))
         (dolist (s '((-4 -5) (-4 5) (4 -5) (4 5))) (shape b :box '(.7 22.0 .7) "#c9a13a" (first s) 11.0 (second s)))
         (shape b :box '(10.0 3.0 12.0) "#c9a13a" 0 23.5 0) (shape b :box '(3.0 3.0 3.0) "#e8e0d0" 2 26.5 0)
         (beam-between b '(0 25 0) '(34 40 0) :thick 1.0 :rgb "#c9a13a") (beam-between b '(0 33 0) '(34 40 0) :thick .25 :rgb "#3a3a40")
         (beam-between b '(34 40 0) '(34 14 0) :thick .1 :rgb "#16161a")))
     (dolist (ship '((.3 "#2c4a7a") (.62 "#8a3a2c")))                 ; ships at the quay
       (let ((b (spot g 168 (at (first ship)) 2.0)))
         (shape b :box '(16.0 7.0 90.0) (second ship) 0 0 0) (shape b :box '(16.2 1.0 90.2) "#a3262c" 0 -3.2 0 :shadow nil)
         (shape b :box '(12.0 9.0 14.0) "#e8e0d0" 0 8.0 34) (shape b :cyl '(1.6 1.6 5.0 10) "#2a2c30" 0 15.0 34)
         (chimney-smoke b 0 18.0 34 :size 3.0 :amount 8 :life 8.0)
         (loop for z from -36 to 20 by 7 for i from 0
               do (shape b :box (list 12.0 (* 2.6 (1+ (mod i 3))) 6.0) (nth (mod i 4) '("#a8843c" "#3f6f73" "#8a4038" "#55708a")) 0 (+ 3.5 (* 1.3 (1+ (mod i 3)))) z))))
     (loop for i below 14                                             ; containers in the yard
           do (let ((b (spot g (+ 100 (* 7 (mod i 3))) (at (+ .36 (* .012 i))))))
                (dotimes (k (1+ (mod i 3))) (shape b :box '(2.6 2.6 9.0) (nth (mod (+ i k) 5) '("#a8843c" "#3f6f73" "#8a4038" "#55708a" "#b0aa98")) 0 (+ 1.3 (* 2.6 k)) 0))))
     (let ((b (spot g 136 (at .88))))                                 ; the lighthouse
       (shape b :cyl '(2.0 3.4 26.0 12) "#e8e4d8" 0 13.0 0) (dotimes (i 2) (shape b :cyl '(2.75 2.95 4.0 12) "#a3262c" 0 (+ 6 (* i 10)) 0 :shadow nil))
       (shape b :cyl '(2.6 2.6 .5 12) "#3a3a40" 0 26.2 0) (shape b :cone '(2.0 2.4 12) "#a3262c" 0 30.4 0)
       (shape b :cyl '(1.4 1.4 2.6 10) "#fff0b0" 0 27.8 0 :emit "#ffe080" :energy 6 :shadow nil)
       (light-beam b 0 27.8 0 170.0 :spin .5 :tilt .06)))

               ; the lighthouse beam goes round in a circle
(deftheme :mine (g tw d0 len mid)
  (dolist (hill '((.24 150 44) (.6 190 60) (.8 130 34)))          ; spoil heaps
       (destructuring-bind (f x r) hill
         (shape (spot g x (at f)) :cone (list r (* r .95) 16) "#3a3838" 0 (* r .47) 0)))
     (let ((b (spot g 90 (at .42))))                                  ; the headframe with its wheels
       (dolist (s '((-3 -3) (-3 3) (3 -3) (3 3))) (shape b :box '(.6 30.0 .6) "#6a5a48" (first s) 15.0 (second s)))
       (loop for y from 6 to 26 by 5 do (shape b :box '(6.6 .4 6.6) "#6a5a48" 0 y 0 :shadow nil))
       (dolist (z '(-1.6 1.6)) (moving (shape b :cyl '(2.4 2.4 .4 14) "#2a2c30" 0 31.5 z :rot (list (/ pi 2) 0 0)) :spin :z 1.5))
       (beam-between b '(-3 30 0) '(-26 0 0) :thick .7 :rgb "#6a5a48")
       (shape b :box '(14.0 8.0 18.0) "#7a5a50" 12 4.0 0)
       (beam-between b '(12 8 8) '(46 20 60) :thick 2.6 :rgb "#6f757c")))

(deftheme :rail (g tw d0 len mid)
  (let* ((d mid) (b (road-group g d)))        ; a footbridge over the street
       (shape b :box '(30.0 .4 3.0) "#6e6e6a" -2 7.6 0)
       (dolist (z '(-1.45 1.45)) (shape b :box '(30.0 1.1 .1) "#3f6f73" -2 8.3 z :shadow nil))
       (dolist (x '(-16 12))
         (dolist (z '(-1.2 1.2)) (shape b :box '(.4 7.6 .4) "#6e6e6a" x 3.8 z))
         (beam-between b (list x 7.6 0) (list (+ x (if (< x 0) -1 1)) 0 (if (< x 0) -16 16)) :thick 2.4 :rgb "#6e6e6a")))
     (let ((b (spot g 78 (at .4))))                                   ; the depot and tracks with railcars
       (shape b :box '(26.0 9.0 60.0) "#7a5a50" 20 4.5 0)
       (loop for z from -24 to 24 by 12 do (shape b :box '(.3 6.0 7.0) "#2a2c30" 6.9 3.0 z :shadow nil))
       (dotimes (r 3)
         (shape b :box '(.3 .2 220.0) "#8a8e92" (- (* r 5.0) 14) .15 30 :shadow nil)
         (loop for i below 8 for z = (+ -60 (* i 15) (* r 9))
               do (shape b :box '(2.9 3.0 13.0) (nth (mod (+ i r) 4) '("#6a3a2e" "#4a4e52" "#5a4636" "#2f5a3c")) (- (* r 5.0) 14) 2.2 z)))
       (shape b :cyl '(1.2 1.2 14.0 10) "#8a5a4a" 44 7.0 -40) (shape b :cyl '(3.0 2.2 6.0 12) "#8a5a4a" 44 17.0 -40)))

(deftheme :river (g tw d0 len mid)
  (loop for d from (- d0 20) below (+ (town-end tw) 20) by 60      ; the river along the town and the embankment parapet
           do (let ((b (apply #'group g (road-pos d 112 -1.5))))
                (node-3d-set-rotation b (v3 0 (- (road-frame d)) 0))
                (shape b :box '(84.0 .2 64.0) *water-rgb* 0 0 0 :rough .3 :emit "#0c2238" :energy .6 :shadow nil)
                (shape b :box '(.5 1.0 62.0) "#9a9890" -47.5 1.5 0)
                (shape b :box '(.14 4.6 .14) "#5a5e60" -49.0 3.4 0)
                (shape b :box '(.4 .3 .4) "#ffe2a0" -49.0 5.8 0 :emit "#ffd080" :energy 1.6 :shadow nil)))
     (let ((b (spot g 70 (at .26))))                                  ; the pier, boats, anglers
       (shape b :box '(22.0 .3 5.0) "#7a5a3a" 8 -.6 0)
       (loop for x from 0 to 16 by 8 do (dolist (z '(-2.2 2.2)) (shape b :box '(.3 3.0 .3) "#4a3424" x -2.0 z)))
       (loop for (x z rgb) in '((6 5 "#3f6f73") (13 -5 "#a3262c") (17 5.5 "#c9a13a"))
             do (shape b :box '(1.5 .5 4.2) rgb x -1.1 z) (shape b :box '(1.2 .1 3.6) "#3a2a1e" x -.84 z :shadow nil))
       (shape b :box '(5.0 2.6 4.0) "#55708a" -6 1.3 6) (shape b :box '(5.6 .3 4.6) "#3a3a34" -6 2.75 6)
       (sign b "ПРИСТАНЬ" -8.55 2.0 6 :h .5 :rgb "#e8e0d0")
       (loop for (x z n) in '((-11.4 -8 4) (-11.4 -15 9) (-11.4 14 15))
             do (diner-cat b x z (/ pi 2) n)
                (beam-between b (list x 1.0 z) (list (+ x 4.5) 2.4 z) :thick .04 :rgb "#3a2a1e"))))

(deftheme :village (g tw d0 len mid)
  (dolist (f '(.14 .3 .46 .62 .78))                                ; behind the cottages: greenhouses, wells, laundry, a grove, goats
       (let ((b (spot g 40 (at f))))
         (shape b :box '(4.0 2.2 9.0) "#c8e0d8" 0 1.1 -8 :alpha .4 :rough .2 :shadow nil)
         (dolist (s '(-1 1)) (shape b :box '(2.5 .1 9.2) "#c8e0d8" (* s 1.0) 2.55 -8 :rot (list 0 0 (* s -.5)) :alpha .4 :shadow nil))
         (shape b :cyl '(.7 .7 1.0 10) "#85857f" 8 .5 2) (dolist (z '(1.3 2.7)) (shape b :box '(.14 2.2 .14) "#5a3e26" 8 1.6 z))
         (shape b :box '(1.6 .2 2.0) "#5a3e26" 8 2.8 2 :rot '(0 0 .2))
         (dolist (z '(8 15)) (shape b :box '(.12 2.2 .12) "#5a3e26" -4 1.1 z))
         (beam-between b '(-4 2.1 8) '(-4 2.1 15) :thick .03)
         (loop for i below 5 do (shape b :box '(.04 .9 .8) (nth (mod (+ i (floor (* 9 f))) 5) '("#e8e0d0" "#a3262c" "#3f6f73" "#c9a13a" "#e8e0d0")) -4 1.6 (+ 9 (* i 1.2)) :shadow nil))
         (loop for i below 3 do (shape b :cyl '(.9 .9 1.5 10) "#d8b050" (+ 14 (* i 2.2)) .9 12 :rot (list 0 0 (/ pi 2))))
         (loop for i below 7 do (build-prop (group b (+ 16 (random 34.0)) 0 (- (random 40.0) 20)) :birch 1))
         (loop for i below 2 do (let ((s (group b (+ 4 (* i 5)) 0 (+ -1 (* i 4)))))
                                  (node-3d-set-scale s (v3 .55 .55 .55))
                                  (mark-cow s 0 0 (* 2.0 i) (if (zerop i) "#e8e4d8" "#5a4636")))))))

(deftheme :admin (g tw d0 len mid)
  (let ((b (spot g 12 mid)))                                       ; the square: a rostrum, a fountain, a monument, flags
       (shape b :box '(46.0 .1 150.0) "#8a8880" 20 .05 0 :shadow nil)
       (loop for i below 4 do (shape b :box (list (- 5.0 (* i 1.1)) .6 16.0) "#9a9890" (- 4 (* i .5)) (+ .3 (* i .6)) -40))
       (shape b :box '(.3 1.0 16.0) "#a3262c" 1.4 2.7 -40 :shadow nil)
       (shape b :cyl '(5.0 5.0 .7 16) "#9a9890" 22 .35 22) (shape b :cyl '(4.6 4.6 .1 16) *water-rgb* 22 .72 22 :rough .3 :emit "#0c2238" :energy .6 :shadow nil)
       (shape b :cyl '(.3 .5 2.6 8) "#9a9890" 22 1.6 22) (chimney-smoke b 22 3.0 22 :size .8 :amount 12 :life 1.6 :rgb "#e8f0f8")
       (shape b :box '(3.0 4.0 3.0) "#6e6e6a" 22 2.0 -8)                ; the monument: a cat pointing the way
       (let ((cat (diner-cat b 22 -8 (- (/ pi 2)) 6))) (node-3d-set-position cat (v3 22 4.0 -8)) (node-3d-set-scale cat (v3 3 3 3)))
       (loop for z from -60 to 60 by 12
             do (shape b :box '(.1 9.0 .1) "#c8c8c4" 6 4.5 z)
                (moving (shape b :box '(.04 1.2 1.9) "#a3262c" 6 8.3 (+ z 1.0) :shadow nil) :swing .12 3.0 (* .3 z) :x))
       (loop for z in '(-70 -64 58) do (shape b :box '(1.7 .6 4.4) "#16161a" 8 .65 z) (shape b :box '(1.6 .55 2.2) "#16161a" 8 1.2 (+ z .2))))
     (let ((b (spot g 78 mid)))                                       ; a building with columns and a clock tower
       (shape b :box '(24.0 22.0 96.0) "#b8ac94" 0 11.0 0) (shape b :box '(26.0 1.2 98.0) "#8a8070" 0 22.6 0 :shadow nil)
       (loop for z from -40 to 40 by 8 do (shape b :cyl '(1.1 1.1 17.0 10) "#e8e0d0" -14 9.5 z))
       (shape b :box '(5.0 2.0 88.0) "#d8ccb0" -14 19.0 0) (shape b :box '(8.0 1.0 90.0) "#9a9890" -15 .5 0)
       (loop for y from 4 to 17 by 4.2 do (shape b :box '(.2 2.0 90.0) "#2e333b" -12.05 y 0 :shadow nil))
       (shape b :box '(9.0 16.0 9.0) "#b8ac94" 0 31.0 0) (shape b :cyl '(2.6 2.6 .3 16) "#e8e0d0" -4.6 33.0 0 :rot (list 0 0 (/ pi 2)) :shadow nil)
       (shape b :cone '(5.0 7.0 4) "#6a3a2e" 0 42.5 0 :rot (list 0 (/ pi 4) 0)) (shape b :box '(.12 6.0 .12) "#c8c8c4" 0 49.0 0 :shadow nil)
       (moving (shape b :box '(.06 2.0 3.2) "#a3262c" 0 50.6 1.7 :shadow nil) :swing .15 2.4 0.0 :x)
       (sign b "ГОРСОВЕТ" -17.6 19.0 0 :h 1.5 :rgb "#6a3a2e")
       (dolist (z '(-30 30)) (flood b -22 3.0 z :range 46.0))))

(deftheme :outpost (g tw d0 len mid)
  (let ((b (spot g 30 (at .4))))                                   ; shift workers' cabins and tanks
       (loop for i below 10 do (shape b :box '(2.6 2.5 7.0) (nth (mod i 4) '("#55708a" "#c9793a" "#3f6f73" "#b0aa98")) (* 6 (mod i 2)) 1.5 (- (* 5 i) 22))
                               (shape b :box '(.1 .9 1.2) "#e8c070" (- (* 6 (mod i 2)) 1.32) 1.8 (- (* 5 i) 22) :emit (when (evenp i) "#ffc060") :shadow nil))
       (dolist (z '(-36 -31)) (shape b :cyl '(1.6 1.6 8.0 12) "#8a8e92" 16 1.9 z :rot (list (/ pi 2) 0 0)))
       (shape b :box '(.2 8.0 .2) "#8a8a84" 20 4.0 30)
       (moving (shape b :cone '(.5 2.4 8) "#e86a20" 20 8.0 31.3 :rot (list (/ pi 2) 0 0) :shadow nil) :swing .35 1.3 0.0 :x 1.4))
     (let ((b (spot g 70 (at .5))))                                   ; the helipad
       (light-beam b 12 9.0 12 120.0 :spin .9 :tilt .1 :rgb "#c0ffd0")
       (shape b :cyl '(10.0 10.0 .2 20) "#55554f" 0 .1 0 :shadow nil) (shape b :box '(.6 .22 6.0) "#e8e0d0" -2 .12 0 :shadow nil)
       (shape b :box '(.6 .22 6.0) "#e8e0d0" 2 .12 0 :shadow nil) (shape b :box '(4.0 .22 .6) "#e8e0d0" 0 .12 0 :shadow nil)
       (shape b :ball '(1.7 10 8) "#c9793a" 0 2.6 -1 :scale '(1 1 1.7)) (shape b :box '(.5 .6 6.0) "#c9793a" 0 3.0 4)
       (shape b :box '(.12 1.6 .9) "#c9793a" 0 3.8 6.6) (dolist (x '(-1.2 1.2)) (shape b :box '(.12 .12 4.4) "#3a3a40" x .7 -1))
       (shape b :box '(.2 1.0 .2) "#3a3a40" 0 4.6 -1)
       (let ((rotor (group b 0 5.1 -1))) (shape rotor :box '(11.0 .06 .4) "#2a2c30" 0 0 0) (shape rotor :box '(.4 .06 11.0) "#2a2c30" 0 0 0)
         (moving rotor :spin :y 9.0)))
     (let ((b (spot g 104 (at .46))))                                 ; the airstrip and a crop-duster biplane
       (shape b :box '(18.0 .12 240.0) "#5a5a58" 0 .06 0 :shadow nil)
       (loop for z from -110 to 110 by 22 do (shape b :box '(.5 .14 9.0) "#e8e0d0" 0 .08 z :shadow nil))
       (let ((pl (group b -3 1.6 -30)))
         (node-3d-set-rotation pl (v3 .14 .5 0))
         (shape pl :cyl '(.7 .5 7.5 10) "#e8e0d0" 0 0 0 :rot (list (/ pi 2) 0 0)) (shape pl :box '(11.0 .14 1.6) "#3c5a48" 0 -.5 -1.2)
         (shape pl :box '(12.0 .14 1.6) "#3c5a48" 0 1.5 -1.4) (dolist (x '(-4 4)) (shape pl :box '(.1 2.0 .1) "#3a3a40" x .5 -1.3))
         (shape pl :box '(3.6 .12 1.2) "#3c5a48" 0 .3 3.6) (shape pl :box '(.12 1.6 1.2) "#3c5a48" 0 1.0 3.6)
         (let ((prop (group pl 0 0 -3.9))) (shape prop :box '(2.6 .2 .08) "#2a2c30" 0 0 0) (moving prop :spin :z 14.0))))
     (dolist (f '(.34 .58))                                           ; boarded-up houses
       (let ((b (spot g 26 (at f))))
         (shape b :box '(10.0 8.4 30.0) "#7a766c" 0 4.2 0)
         (loop for y in '(2.0 5.0) do (loop for z from -12 to 12 by 6 do (shape b :box '(.14 1.4 2.0) "#5a4636" -5.05 y z :shadow nil))))))

(deftheme :resort (g tw d0 len mid)
  (dolist (z '(-45 45))                                            ; the sanatorium buildings with balconies
       (let ((b (spot g 60 (+ (at .45) z))))
         (shape b :box '(14.0 22.0 60.0) "#e8e4d8" 0 11.0 0) (shape b :box '(14.6 .6 60.6) "#3f6f73" 0 22.3 0 :shadow nil)
         (loop for y from 3.4 to 20 by 3.1 do (shape b :box '(1.4 .9 58.0) "#55708a" -7.4 y 0 :shadow nil)
                                              (shape b :box '(.14 1.6 56.0) "#2e333b" -7.05 (+ y 1.2) 0 :shadow nil))))
     (let ((b (spot g 24 (at .45))))                                  ; the pump room and the lower station of the cable car
       (shape b :cyl '(5.0 5.0 .5 14) "#e8e4d8" 0 .25 -22)
       (dotimes (i 8) (shape b :cyl '(.3 .3 4.0 8) "#e8e4d8" (* 4.4 (cos (* i .785))) 2.3 (+ -22 (* 4.4 (sin (* i .785))))))
       (shape b :ball '(5.2 14 8) "#3f6f73" 0 4.3 -22 :scale '(1 .45 1))
       (sign b "МИНЕРАЛЬНЫЕ ВОДЫ" -5.3 3.9 -22 :h .45 :rgb "#3f6f73")
       (shape b :box '(8.0 5.0 8.0) "#c8bca0" 6 2.5 14) (shape b :box '(9.0 .5 10.0) "#a3262c" 6 5.25 14 :shadow nil)
       (sign b "КАНАТНАЯ ДОРОГА" 1.9 4.0 14 :h .5 :rgb "#a3262c")
       (loop for (x y) in '((70 18.6) (140 32.7) (210 46.9)) do (shape b :box (list .9 (+ y 12) .9) "#8a8e90" x (/ (- y 12) 2) 14)   ; pylons
                                                          (shape b :box '(.5 .5 4.0) "#8a8e90" x y 14 :shadow nil))
       (dolist (z '(12.6 15.4)) (beam-between b (list 8 6.0 z) (list 250 55.0 z) :thick .1 :sag 2.0))
       (dotimes (i 4)                                                   ; the cabins crawl up and down
         (let ((cab (group b)) (z (if (evenp i) 12.6 15.4)))
           (shape cab :box '(2.2 2.2 2.0) (if (evenp i) "#a3262c" "#c9a13a") 0 -2.2 0) (shape cab :box '(2.24 .8 1.6) "#16181c" 0 -1.9 0 :shadow nil)
           (shape cab :box '(.08 1.2 .08) "#3a3a40" 0 -.6 0 :shadow nil)
           (moving cab :path 10 5.4 z 248 53.6 z 40.0 (* i .5))))
       (shape b :box '(10.0 6.0 10.0) "#c8bca0" 256 51.0 14) (shape b :cone '(7.5 4.0 4) "#a3262c" 256 56.0 14 :rot (list 0 (/ pi 4) 0))))

(deftheme :sawmill (g tw d0 len mid)
  (let ((b (spot g 26 (at .42))))                                  ; log stacks, the sawmill, sawdust, logging trucks
       (loop for k below 5
             do (loop for r below 4
                      do (loop for i below (- 6 r)
                               do (shape b :cyl '(.45 .45 9.0 8) (if (evenp (+ i r)) "#8a6440" "#7a5a3a") (+ (* k 9) (* .45 r) (* .9 i)) (+ .45 (* .8 r)) -30 :rot (list (/ pi 2) 0 0)))))
       (dolist (x '(0 14)) (dolist (z '(4 22)) (shape b :box '(.5 6.0 .5) "#5a3e26" x 3.0 z)))
       (dolist (s '(-1 1)) (shape b :box '(9.0 .3 21.0) "#4c4e52" (+ 7 (* s 3.9)) 7.3 13 :rot (list 0 0 (* s -.45))))
       (shape b :box '(3.0 1.2 12.0) "#6a5038" 7 .9 13) (shape b :cyl '(1.0 1.0 .1 16) "#c8c8c4" 7 2.0 13 :rot (list 0 0 (/ pi 2)) :metal .6 :rough .3)
       (shape b :cone '(6.0 5.0 12) "#d8b050" 24 2.5 16) (chimney-smoke b 7 2.4 13 :size 1.0 :amount 10 :life 2.5 :rgb "#d8c890")
       (loop for (x z) in '((30 -8) (36 -14))                           ; logging trucks
             do (shape b :box '(2.2 2.0 2.2) "#3f6f73" x 1.7 (- z 6)) (shape b :box '(2.0 .5 12.0) "#1e2022" x .75 z)
                (dolist (dx '(-.6 0 .6)) (shape b :cyl '(.4 .4 10.0 8) "#8a6440" (+ x dx) 1.5 (+ z 1.5) :rot (list (/ pi 2) 0 0)))
                (dolist (dx '(-.3 .3)) (shape b :cyl '(.4 .4 10.0 8) "#7a5a3a" (+ x dx) 2.2 (+ z 1.5) :rot (list (/ pi 2) 0 0))))
       (dolist (s '((-1 -1) (-1 1) (1 -1) (1 1))) (shape b :box '(.4 12.0 .4) "#5a3e26" (+ 46 (* 2 (first s))) 6.0 (+ 30 (* 2 (second s)))))
       (shape b :cyl '(3.2 3.2 5.0 10) "#6a5038" 46 14.5 30) (shape b :cone '(3.6 2.4 10) "#3a2a1e" 46 18.2 30)
       (loop for i below 26 do (build-prop (group b (+ 50 (random 60.0)) 0 (- (random 150.0) 75)) :pine 1))))

(defun build-theme (g tw)
  "Build what makes the town TW unlike the others.
It stands in the second row and beyond, visible from the window and from the street."
  (let ((build (cdr (assoc (town-theme tw) *themes*))))
    (when build (funcall build g tw))))
