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
          (when (picture-here-p path)
            (let ((img (gdext:make-object "Image")) (tx (gdext:make-object "ImageTexture")) (m (gdext:make-object "StandardMaterial3D")))
              (read-picture img path)
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
       (model b "theme-military-1")
       (dolist (z '(-80 80)) (light-beam b -18 10.5 z 150.0 :sweep 1.1 :tilt .9 :speed (if (< z 0) .23 .31) :wide 7.0))   ; searchlights sweep the sky
       (moving (shape b :cyl '(3.0 3.0 .25 12) "#8a9080" 14 9.6 0 :rot (list (- (/ pi 2) .3) 0 0)) :spin :y .7))
     (let ((b (spot g 24 (at .3))))                                  ; a tank on a pedestal
       (model b "theme-military-2")))

(deftheme :factory (g tw d0 len mid)
  (dolist (f '(.2 .46 .72))                                      ; cooling towers and chimneys
       (let ((b (spot g 150 (at f))))
         (model b "theme-factory-1")
         (chimney-smoke b 0 43.0 0 :size 9.0 :amount 12 :life 14.0 :rgb "#e4e4e8")
          (red-light b 34 60.6 20)
         (chimney-smoke b 34 62.0 20 :size 6.0 :amount 14 :life 12.0)))
     (let ((b (spot g 96 (at .58))))
       (model b "theme-factory-2"))
     (let* ((d (at .5)) (b (road-group g d)))   ; a pipeline arching over the street
       (model b "theme-factory-3")
       (model b "theme-factory-3b")))

(deftheme :university (g tw d0 len mid)
  (let ((b (spot g 112 mid)))                                     ; the main building: wings, a tower, a spire
       (model b "theme-university-1")
       (red-light b 0 78.4 0)
       (sign b "УНИВЕРСИТЕТ" -16.1 12.8 0 :h 1.6 :rgb "#6a3a2e")
       (flood b -24 4.0 0 :range 60.0 :energy 3.5))
     (let ((b (spot g 84 (at .28))))                                  ; the observatory
       (model b "theme-university-2"))
     (let ((b (spot g 86 (at .74))))                                  ; dormitories with a mural
       (model b "theme-university-3")))

(deftheme :port (g tw d0 len mid)
  (loop for d from (+ d0 20) below (town-end tw) by 60             ; the sea
           do (let ((b (apply #'group g (road-pos d 640 -2.2))))
                (node-3d-set-rotation b (v3 0 (- (road-frame d)) 0))
                (model b "theme-port-1")))
     (dolist (f '(.25 .45 .65))                                      ; gantry cranes
       (let ((b (spot g 122 (at f))))
         (model b "theme-port-2")))
     (dolist (ship '((.3 "#2c4a7a") (.62 "#8a3a2c")))                 ; ships at the quay
       (let ((b (spot g 168 (at (first ship)) 2.0)))
         (shape b :box '(16.0 7.0 90.0) (second ship) 0 0 0) (model b "theme-port-3")
         (chimney-smoke b 0 18.0 34 :size 3.0 :amount 8 :life 8.0)
         ))
     (loop for i below 14                                             ; containers in the yard
           do (let ((b (spot g (+ 100 (* 7 (mod i 3))) (at (+ .36 (* .012 i))))))
                (dotimes (k (1+ (mod i 3))) (shape b :box '(2.6 2.6 9.0) (nth (mod (+ i k) 5) '("#a8843c" "#3f6f73" "#8a4038" "#55708a" "#b0aa98")) 0 (+ 1.3 (* 2.6 k)) 0))))
     (let ((b (spot g 136 (at .88))))                                 ; the lighthouse
       (model b "theme-port-5")
       (light-beam b 0 27.8 0 170.0 :spin .5 :tilt .06)))

               ; the lighthouse beam goes round in a circle
(deftheme :mine (g tw d0 len mid)
  (dolist (hill '((.24 150 44) (.6 190 60) (.8 130 34)))          ; spoil heaps
       (destructuring-bind (f x r) hill
         (shape (spot g x (at f)) :cone (list r (* r .95) 16) "#3a3838" 0 (* r .47) 0)))
     (let ((b (spot g 90 (at .42))))                                  ; the headframe with its wheels
       (model b "theme-mine-1")
       (dolist (z '(-1.6 1.6)) (moving (shape b :cyl '(2.4 2.4 .4 14) "#2a2c30" 0 31.5 z :rot (list (/ pi 2) 0 0)) :spin :z 1.5))
       ))

(deftheme :rail (g tw d0 len mid)
  (let* ((d mid) (b (road-group g d)))        ; a footbridge over the street
       (model b "theme-rail-1")
       (model b "theme-rail-1b"))
     (let ((b (spot g 78 (at .4))))                                   ; the depot and tracks with railcars
       (model b "theme-rail-2")))

(deftheme :river (g tw d0 len mid)
  (loop for d from (- d0 20) below (+ (town-end tw) 20) by 60      ; the river along the town and the embankment parapet
           do (let ((b (apply #'group g (road-pos d 112 -1.5))))
                (node-3d-set-rotation b (v3 0 (- (road-frame d)) 0))
                (model b "theme-river-1")))
     (let ((b (spot g 70 (at .26))))                                  ; the pier, boats, anglers
       (model b "theme-river-2")
       (sign b "ПРИСТАНЬ" -8.55 2.0 6 :h .5 :rgb "#e8e0d0")
       (loop for (x z n) in '((-11.4 -8 4) (-11.4 -15 9) (-11.4 14 15))
             do (diner-cat b x z (/ pi 2) n)
                (beam-between b (list x 1.0 z) (list (+ x 4.5) 2.4 z) :thick .04 :rgb "#3a2a1e"))))

(deftheme :village (g tw d0 len mid)
  (dolist (f '(.14 .3 .46 .62 .78))                                ; behind the cottages: greenhouses, wells, laundry, a grove, goats
       (let ((b (spot g 40 (at f))))
         (model b "theme-village-1")
         (loop for i below 5 do (shape b :box '(.04 .9 .8) (nth (mod (+ i (floor (* 9 f))) 5) '("#e8e0d0" "#a3262c" "#3f6f73" "#c9a13a" "#e8e0d0")) -4 1.6 (+ 9 (* i 1.2)) :shadow nil))
         (loop for i below 7 do (build-prop (group b (+ 16 (random 34.0)) 0 (- (random 40.0) 20)) :birch 1))
         (loop for i below 2 do (let ((s (group b (+ 4 (* i 5)) 0 (+ -1 (* i 4)))))
                                  (node-3d-set-scale s (v3 .55 .55 .55))
                                  (mark-cow s 0 0 (* 2.0 i) (if (zerop i) "#e8e4d8" "#5a4636")))))))

(deftheme :admin (g tw d0 len mid)
  (let ((b (spot g 12 mid)))                                       ; the square: a rostrum, a fountain, a monument, flags
       (model b "theme-admin-1") (chimney-smoke b 22 3.0 22 :size .8 :amount 12 :life 1.6 :rgb "#e8f0f8")
                       ; the monument: a cat pointing the way
       (let ((cat (diner-cat b 22 -8 (- (/ pi 2)) 6))) (node-3d-set-position cat (v3 22 4.0 -8)) (node-3d-set-scale cat (v3 3 3 3)))
       (loop for z from -60 to 60 by 12
             do (shape b :box '(.1 9.0 .1) "#c8c8c4" 6 4.5 z)
                (moving (shape b :box '(.04 1.2 1.9) "#a3262c" 6 8.3 (+ z 1.0) :shadow nil) :swing .12 3.0 (* .3 z) :x))
       )
     (let ((b (spot g 78 mid)))                                       ; a building with columns and a clock tower
       (model b "theme-admin-2")
       (moving (shape b :box '(.06 2.0 3.2) "#a3262c" 0 50.6 1.7 :shadow nil) :swing .15 2.4 0.0 :x)
       (sign b "ГОРСОВЕТ" -17.6 19.0 0 :h 1.5 :rgb "#6a3a2e")
       (dolist (z '(-30 30)) (flood b -22 3.0 z :range 46.0))))

(deftheme :outpost (g tw d0 len mid)
  (let ((b (spot g 30 (at .4))))                                   ; shift workers' cabins and tanks
       (model b "theme-outpost-1b")
       (model b "theme-outpost-1")
       (moving (shape b :cone '(.5 2.4 8) "#e86a20" 20 8.0 31.3 :rot (list (/ pi 2) 0 0) :shadow nil) :swing .35 1.3 0.0 :x 1.4))
     (let ((b (spot g 70 (at .5))))                                   ; the helipad
       (light-beam b 12 9.0 12 120.0 :spin .9 :tilt .1 :rgb "#c0ffd0")
       (model b "theme-outpost-2")
       (let ((rotor (group b 0 5.1 -1))) (shape rotor :box '(11.0 .06 .4) "#2a2c30" 0 0 0) (shape rotor :box '(.4 .06 11.0) "#2a2c30" 0 0 0)
         (moving rotor :spin :y 9.0)))
     (let ((b (spot g 104 (at .46))))                                 ; the airstrip and a crop-duster biplane
       (model b "theme-outpost-3")
       (let ((pl (group b -3 1.6 -30)))
         (node-3d-set-rotation pl (v3 .14 .5 0))
         (shape pl :cyl '(.7 .5 7.5 10) "#e8e0d0" 0 0 0 :rot (list (/ pi 2) 0 0)) (shape pl :box '(11.0 .14 1.6) "#3c5a48" 0 -.5 -1.2)
         (shape pl :box '(12.0 .14 1.6) "#3c5a48" 0 1.5 -1.4) (dolist (x '(-4 4)) (shape pl :box '(.1 2.0 .1) "#3a3a40" x .5 -1.3))
         (shape pl :box '(3.6 .12 1.2) "#3c5a48" 0 .3 3.6) (shape pl :box '(.12 1.6 1.2) "#3c5a48" 0 1.0 3.6)
         (let ((prop (group pl 0 0 -3.9))) (shape prop :box '(2.6 .2 .08) "#2a2c30" 0 0 0) (moving prop :spin :z 14.0))))
     (dolist (f '(.34 .58))                                           ; boarded-up houses
       (let ((b (spot g 26 (at f))))
         (model b "theme-outpost-4"))))

(deftheme :resort (g tw d0 len mid)
  (dolist (z '(-45 45))                                            ; the sanatorium buildings with balconies
       (let ((b (spot g 60 (+ (at .45) z))))
         (model b "theme-resort-1")))
     (let ((b (spot g 24 (at .45))))                                  ; the pump room and the lower station of the cable car
       (model b "theme-resort-2")
       (sign b "МИНЕРАЛЬНЫЕ ВОДЫ" -5.3 3.9 -22 :h .45 :rgb "#3f6f73")
       (sign b "КАНАТНАЯ ДОРОГА" 1.9 4.0 14 :h .5 :rgb "#a3262c")
       (dotimes (i 4)                                                   ; the cabins crawl up and down
         (let ((cab (group b)) (z (if (evenp i) 12.6 15.4)))
           (shape cab :box '(2.2 2.2 2.0) (if (evenp i) "#a3262c" "#c9a13a") 0 -2.2 0) (shape cab :box '(2.24 .8 1.6) "#16181c" 0 -1.9 0 :shadow nil)
           (shape cab :box '(.08 1.2 .08) "#3a3a40" 0 -.6 0 :shadow nil)
           (moving cab :path 10 5.4 z 248 53.6 z 40.0 (* i .5))))
       ))

(deftheme :sawmill (g tw d0 len mid)
  (let ((b (spot g 26 (at .42))))                                  ; log stacks, the sawmill, sawdust, logging trucks
       (model b "theme-sawmill-1b")
       (model b "theme-sawmill-1") (chimney-smoke b 7 2.4 13 :size 1.0 :amount 10 :life 2.5 :rgb "#d8c890")
       (loop for i below 26 do (build-prop (group b (+ 50 (random 60.0)) 0 (- (random 150.0) 75)) :pine 1))))

(defun build-theme (g tw)
  "Build what makes the town TW unlike the others.
It stands in the second row and beyond, visible from the window and from the street."
  (let ((build (cdr (assoc (town-theme tw) *themes*))))
    (when build (funcall build g tw))))
