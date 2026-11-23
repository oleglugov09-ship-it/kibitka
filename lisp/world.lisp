;;;; The world outside the window. Everything scenic is under the World node: a thing at path coordinate D
;;;; stands in its coordinates at the point (ROAD-X D + sideways offset, ROAD-Y D + height, −D). The world
;;;; stands still and the Rig moves — the wagon with the camera: it stands on a road point, turns to the
;;;; course and tilts with the slope. This way the shadows do not shimmer: Godot snaps the shadow map to a
;;;; grid relative to the camera, and if the world floated under a fixed camera, every shadow edge
;;;; would shift by fractions of a texel each frame. To keep the numbers from growing, every
;;;; *REBASE* m the origin is moved to the wagon (World is shifted). The land is chunks of *CHUNK-LEN* m
;;;; (SurfaceTool, flat faces, vertex colours), the water is ribbons in the same chunks wherever a hollow
;;;; lies below the water level. A chunk left behind is rebuilt ahead piece by piece, no more than
;;;; *CHUNK-BUDGET-MS* per frame. The roadside things (trees, lanterns, rocks…) and the bridges work the
;;;; same way: one is left behind — we build a new one ahead.

(in-package #:kiosk-trip)

(defparameter *speed* 7.0 "The truck speed in m/s.")
(defparameter *smooth-ground* t "True for smooth land lighting: flat faces far away are smaller than a pixel and shimmer.")
(defparameter *prop-step* 1.4 "The spacing of the roadside things, in metres.")
(defparameter *ahead* 170.0 "The distance ahead up to which we build.")
(defparameter *behind* 60.0 "The distance behind up to which we keep things.")

(defparameter *grass*
  '(:pine "#2a4a2c" :meadow "#4a6a30" :autumn "#6a5a2a" :birch "#3e5a32" :rocky "#4a4a44"
    :steppe "#7a7038" :marsh "#34442a"))
(defparameter *veg-props*
  '(:pine (:pine :pine :pine :lantern :mush :campfire :rock)
    :meadow (:flowers :flowers :flowers :hay :fence :round :lantern)
    :autumn (:autumn :autumn :autumn :mush :pumpkin :lantern :hay)
    :birch (:birch :birch :birch :round :lantern :rock :mush)
    :rocky (:rock :rock :cairn :cairn :pine)
    :steppe (:bush :bush :hay :rock :cairn :fence :lantern)
    :marsh (:reeds :reeds :snag :snag :bush :mush :lantern)))
(defparameter *small-far* 70.0 "The distance in metres beyond which small things fade out.
Flowers, mushrooms, reeds and lanterns are smaller than a pixel far away and flicker.")
(defparameter *small-fade* 1 "The fade mode of small things: 1 to fade smoothly (VISIBILITY_RANGE_FADE_SELF), 0 to vanish at once.")

(defvar *world* nil "The Node3D holding everything that floats.")
(defvar *turn* nil "The Node3D above World and the sun: the turn to the road course.")
(defvar *rig* nil "The Node3D of the wagon and the camera: the tilt with the road slope and the sway.")
(defvar *slope* 0.0 "The slope of the road under the wagon, in radians.")
(defvar *course* 0.0 "The course of the road under the wagon, in radians.")
(defvar *origin* '(0.0 0.0 0.0) "The scene origin in World coordinates (x y z).")
(defvar *rig-at* (list 0.0 0.0 0.0) "The position of the Rig in the scene (x y z).")
(defparameter *rebase* 128.0 "The number of metres after which the origin is moved.
It is done often: far from the origin the engine runs out of precision, and surfaces lying a millimetre
apart flicker.")
(defvar *dist* 0.0 "The distance travelled, in metres.")
(defvar *props* '() "The list of (node . d), in ascending order of d.")
(defvar *bridges* '())
(defvar *river-side* 0)

;;; The land

;;; land colours — from '#rrggbb' once
(defparameter *rock-rgb* "#58545e")
(defparameter *snow-rgb* "#dfe4ee")
(defparameter *silt-rgb* "#8a7a5a")
(defparameter *road-rgb* "#6a5640")

(defun veg-color (d)
  "Return the grass colour at D: a blend of the vegetation kinds by their weights."
  (let ((r 0.0) (g 0.0) (b 0.0))
    (loop for (k wt) on (layer-weights :veg d) by #'cddr
          do (let ((c (hex (getf *grass* k))))
               (incf r (* wt (color-r c))) (incf g (* wt (color-g c))) (incf b (* wt (color-b c)))))
    (season-grass (color r g b 1.0))))

(defun ground-color (x h y grass)
  "Return the land colour derived from the grass colour GRASS.
X is the sideways offset from the road, H is the height above the road and Y is the height above the
world level (rock and snow depend on it)."
  (let ((c grass) (rock (max (sstep 4 10 h) (sstep 14 26 y))))
    (when (> rock 0) (setf c (mix-color c (hex *rock-rgb*) rock)))
    (when (> y 32) (setf c (mix-color c (hex *snow-rgb*) (sstep 32 40 y))))
    (when (< h -.2) (setf c (mix-color c (hex *silt-rgb*) (sstep -.2 -1 h))))
    (when (and (< -6.4 x 2.8) (< -.3 h 3)) (setf c (mix-color c (hex *road-rgb*) (min (sstep 2.8 1.8 x) (sstep -6.4 -5.4 x)))))
    c))

;;; The map land: tiles of three levels of detail around the wagon. Level 0 has a 4 m cell and a 64 m tile;
;;; level 1 has a 16 m cell and a 256 m tile; level 2 has a 64 m cell and a 1024 m tile. The tiles are nested
;;; (16 fine ones = one coarse one): a coarse tile hides once all of its fine ones are ready, so there are no
;;; holes and no overlaps. The seams between levels are covered by 'skirts' — strips going down along the tile edges.

(defparameter *lod-cells* '(4.0 16.0 64.0) "The cell size per level, in metres; a tile has 16×16 cells.")
(defparameter *land-budget-ms* 3.0 "The amount of frame time given to building the land.")
(defvar *land* nil "The node of the land and the roads: it outlives the world of things and shifts together with the origin.")
(defvar *tiles* (make-hash-table :test 'equal) "The table mapping (level ix iz) to (node ready).")
(defvar *tile-jobs* '() "The build queue: (key . job).")
(defvar *land-at* nil "The map point around which the land was laid out last time.")
(defvar *chunks* '() "A leftover from the former strip land: empty.")

(defun tile-size (lod) (* 16 (nth lod *lod-cells*)))

(defun tile-job (key node)
  "Return a job that builds the tile KEY piece by piece into NODE.
Each call computes a few rows of heights and returns T once the mesh is ready."
  (destructuring-bind (lod ix iz) key
    (let* ((cell (nth lod *lod-cells*)) (x0 (* ix 16 cell)) (z0 (* iz 16 cell)) (n 16)
           (ys (make-array (list (1+ n) (1+ n)))) (cols (make-array (list (1+ n) (1+ n))))
           (uvs (make-array (list (1+ n) (1+ n)))) (uv2s (make-array (list (1+ n) (1+ n)))) (wls (make-array (list (1+ n) (1+ n))))
           (j 0) (wild-uv2 (vector2 0 0)))
      (lambda ()
        (cond
          ((<= j n)
           (dotimes (i (1+ n))
             (let ((x (+ x0 (* i cell))) (z (+ z0 (* j cell))))
               (multiple-value-bind (y near) (land-height x z)
                 (setf (aref ys i j) y)
                 (if near
                     (destructuring-bind (d lat &rest more) near           ; (path-coordinate sideways distance road)
                       (declare (ignore more))
                       (let* ((row (row-near d)) (h (- y (road-y d))) (wet (max (row-lake row) (row-river row))))
                         ;; coarse tiles have a cell wider than the road: a vertex next to the roadbed is lowered under
                         ;; it, otherwise a face between tall neighbours covers half the road until the fine ones load
                         (when (and (> lod 0) (< (abs (+ lat 2.25)) (+ 6.0 (* .8 cell))) (not (deck-at d)))
                           (setf y (min y (- (road-y d) .25)) (aref ys i j) y))
                         (setf (aref cols i j) (ground-color lat h y (veg-color (row-d row)))
                               (aref uvs i j) (vector2 lat h)
                               (aref uv2s i j) (vector2 (row-meadow row) (row-dry row))
                               (aref wls i j) (and (> wet .01) (< (abs lat) *road-near*) (+ (road-y d) *water-y*)))
                         ;; the bed under water is sunk by almost a metre: in the shallows it lay level with the water,
                         ;; and on coarse far tiles water and bed came in stripes. The shore (above water) is left alone
                         (let ((wl (aref wls i j)))
                           (when (and wl (< y (+ wl .15)))
                             (setf (aref ys i j) (- y (* 1.5 (min 1.0 (/ (- (+ wl .15) y) .25)))))))))
                     (setf (aref cols i j) (ground-color 999.0 0.0 (* .45 y) (hex "#3e5a32"))
                           (aref uvs i j) (vector2 999.0 0.0) (aref uv2s i j) wild-uv2 (aref wls i j) nil)))))
           (incf j) nil)
          (t
           (let ((st (gdext:make-object "SurfaceTool")) (wst (gdext:make-object "SurfaceTool")) (water 0) (drop (* 2 cell)))
             (object-call st "begin" 3) (object-call wst "begin" 3)
             (flet ((pt (i j &optional (down 0.0)) (v3 (* i cell) (- (aref ys i j) down) (* j cell))))
               (dotimes (j n)
                 (dotimes (i n)
                   (object-call st "add_triangle_fan" (vector (pt i j) (pt (1+ i) j) (pt (1+ i) (1+ j)) (pt i (1+ j)))
                                (vector (aref uvs i j) (aref uvs (1+ i) j) (aref uvs (1+ i) (1+ j)) (aref uvs i (1+ j)))
                                (vector (aref cols i j) (aref cols (1+ i) j) (aref cols (1+ i) (1+ j)) (aref cols i (1+ j)))
                                (vector (aref uv2s i j) (aref uv2s (1+ i) j) (aref uv2s (1+ i) (1+ j)) (aref uv2s i (1+ j))))
                   ;; The water level is one per vertex, shared by all the cells around it: the cells meet without
                   ;; gaps. A 'wet' vertex has its own (half a metre below its road — the road will not flood), a dry
                   ;; one on the shore takes the lowest of the levels of its wet neighbours.
                   (flet ((level (i j)
                            (or (aref wls i j)
                                (let ((low nil))
                                  (loop for di from -1 to 1
                                        do (loop for dj from -1 to 1
                                                 do (let ((a (+ i di)) (b (+ j dj)))
                                                      (when (and (<= 0 a n) (<= 0 b n) (aref wls a b)
                                                                 (or (null low) (< (aref wls a b) low)))
                                                        (setf low (aref wls a b))))))
                                  low))))
                     (let ((l00 (level i j)) (l10 (level (1+ i) j)) (l11 (level (1+ i) (1+ j))) (l01 (level i (1+ j))))
                       (when (and l00 l10 l11 l01
                                  (or (aref wls i j) (aref wls (1+ i) j) (aref wls i (1+ j)) (aref wls (1+ i) (1+ j)))
                                  (< (min (aref ys i j) (aref ys (1+ i) j) (aref ys i (1+ j)) (aref ys (1+ i) (1+ j)))
                                     (max l00 l10 l11 l01)))
                         (incf water)
                         (object-call wst "add_triangle_fan"
                                      (vector (v3 (* i cell) l00 (* j cell)) (v3 (* (1+ i) cell) l10 (* j cell))
                                              (v3 (* (1+ i) cell) l11 (* (1+ j) cell)) (v3 (* i cell) l01 (* (1+ j) cell)))))))))
               ;; skirts along the four edges: both sides, so as not to think about which way they face
               (flet ((skirt (i0 j0 i1 j1)
                        (let ((uv (aref uvs i0 j0)) (uv2 (aref uv2s i0 j0)) (c0 (aref cols i0 j0)) (c1 (aref cols i1 j1)))
                          (dolist (order '((0 1 2 3) (3 2 1 0)))
                            (let ((ps (vector (pt i0 j0) (pt i1 j1) (pt i1 j1 drop) (pt i0 j0 drop))) (cs (vector c0 c1 c1 c0)))
                              (object-call st "add_triangle_fan" (map 'vector (lambda (k) (aref ps k)) order) (vector uv uv uv uv)
                                           (map 'vector (lambda (k) (aref cs k)) order) (vector uv2 uv2 uv2 uv2)))))))
                 (dotimes (k n) (skirt k 0 (1+ k) 0) (skirt k n (1+ k) n) (skirt 0 k 0 (1+ k)) (skirt n k n (1+ k)))))
             (when *smooth-ground* (object-call st "index"))
             (object-call st "generate_normals")
             (object-set node "mesh" (object-call st "commit"))
             (when (plusp water)
               (let ((w (gdext:make-object "MeshInstance3D")))
                 (object-call wst "generate_normals")
                 (object-set w "mesh" (object-call wst "commit"))
                 (object-set w "material_override" (water-material))
                 (object-set w "cast_shadow" 0)
                 (node-add-child node w))))
           t))))))

(defun wanted-tiles (x z)
  "Return the tiles needed around the map point (X, Z).
Fine ones cover the two nearest coarse ones along each axis."
  (let ((keys '()))
    (flet ((span (lod per)                                   ; PER fine ones per coarse one: take the 2×2 coarse ones nearest to the point
             (let* ((big (* per (tile-size lod))) (bx (floor (- x (/ big 2)) big)) (bz (floor (- z (/ big 2)) big)))
               (loop for ix from (* bx per) below (* (+ bx 2) per)
                     do (loop for iz from (* bz per) below (* (+ bz 2) per) do (push (list lod ix iz) keys))))))
      (span 0 4) (span 1 4))
    (let ((s (tile-size 2)) (r 2))
      (loop for ix from (- (floor x s) r) to (+ (floor x s) r)
            do (loop for iz from (- (floor z s) r) to (+ (floor z s) r) do (push (list 2 ix iz) keys))))
    keys))

(defun tile-ready-p (key) (let ((tl (gethash key *tiles*))) (and tl (second tl))))

(defun children-ready-p (key)
  "Return true if all 16 tiles of the next, more detailed level under the tile KEY are ready."
  (destructuring-bind (lod ix iz) key
    (and (plusp lod)
         (loop for cx from (* ix 4) below (* (1+ ix) 4)
               always (loop for cz from (* iz 4) below (* (1+ iz) 4) always (tile-ready-p (list (1- lod) cx cz)))))))

(defun show-tiles ()
  "Update tile visibility.
A coarse tile is visible until all of its fine ones are ready; a fine one once its whole sixteen is ready."
  (maphash (lambda (key tl)
             (destructuring-bind (lod ix iz) key
               (object-set (first tl) "visible"
                           (and (second tl) (not (children-ready-p key))
                                (or (= lod 2) (children-ready-p (list (1+ lod) (floor ix 4) (floor iz 4))))))))
           *tiles*))

(defun lay-land (x z &optional now)
  "Lay out the land around the map point (X, Z).
Remove the surplus tiles and queue the missing ones (or build them at once if NOW)."
  (when (ext:getenv "KIOSK_NOLAND")                          ; the drive-through in checks looks at buildings — it does not need the land
    (setf *land-at* (list x z))
    (return-from lay-land nil))
  (let ((want (wanted-tiles x z)))
    (maphash (lambda (key tl) (unless (member key want :test #'equal) (node-queue-free (first tl)) (remhash key *tiles*))) *tiles*)
    (setf *tile-jobs* (remove-if-not (lambda (job) (gethash (car job) *tiles*)) *tile-jobs*))
    (dolist (key (sort want #'> :key #'first))                ; coarse ones first: there is never a void under them
      (unless (gethash key *tiles*)
        (let ((node (gdext:make-object "MeshInstance3D")))
          (object-set node "material_override" (ground-material))
          (object-set node "visible" nil)
          (when (= (first key) 2) (object-set node "cast_shadow" 0))
          (node-add-child *land* node)
          (node-3d-set-position node (v3 (* (second key) (tile-size (first key))) 0 (* (third key) (tile-size (first key)))))
          (setf (gethash key *tiles*) (list node nil))
          (setf *tile-jobs* (append *tile-jobs* (list (cons key (tile-job key node))))))))
    (setf *land-at* (list x z))
    (when now
      ;; At once — only the coarse tiles around the wagon: they cover everything, there is no void underfoot.
      ;; The rest is built over the frames (STEP-CHUNKS), nearest first: everything used to be laid here in one
      ;; go, and startup (and with it any far rebase) stalled for seven seconds — three times longer in a browser.
      (flet ((far (key) (destructuring-bind (lod ix iz) key                    ; to the middle of the tile's 'parent':
                          (let ((s (* 4 (tile-size lod))))                     ; its sixteen children become ready together
                            (+ (abs (- x (* (+ (floor ix 4) .5) s))) (abs (- z (* (+ (floor iz 4) .5) s))))))))
        (let ((s (tile-size 2)))
          (dolist (job *tile-jobs*)
            (destructuring-bind (lod ix iz) (car job)
              (when (and (= lod 2) (<= (abs (- ix (floor x s))) 1) (<= (abs (- iz (floor z s))) 1))
                (loop until (funcall (cdr job)))
                (setf (second (gethash (car job) *tiles*)) t)))))
        (setf *tile-jobs* (stable-sort (remove-if (lambda (job) (second (gethash (car job) *tiles*))) *tile-jobs*)
                                       (lambda (a b) (or (> (first (car a)) (first (car b)))
                                                         (and (= (first (car a)) (first (car b))) (< (far (car a)) (far (car b))))))))
        (show-tiles)))))

(defun settle-land ()
  "Lay the land under the new position of the wagon.
If it is still empty under it (startup, a far jump), build it at once."
  (multiple-value-bind (x z) (road-point *dist*)
    (lay-land x z)
    (unless (tile-ready-p (list 0 (floor x (tile-size 0)) (floor z (tile-size 0))))
      (lay-land x z t))
    (setf *roads-at* nil)))

(defun clear-land ()
  (maphash (lambda (key tl) (declare (ignore key)) (node-queue-free (first tl))) *tiles*)
  (clrhash *tiles*)
  (setf *tile-jobs* '() *land-at* nil))

(defun rebuild-chunks ()
  "Rebuild the land around the wagon in one go (a new trip, a jump, a parameter change)."
  (clear-land)
  (multiple-value-bind (x z) (road-point *dist*) (lay-land x z t)))

(defun step-chunks ()
  "Re-lay the land once the wagon has moved a quarter of a tile.
The build queue works within the budget."
  (multiple-value-bind (x z) (road-point *dist*)
    (when (or (null *land-at*) (> (max (abs (- x (first *land-at*))) (abs (- z (second *land-at*)))) 16.0))
      (lay-land x z)))
  (when *tile-jobs*
    (let ((t0 (get-internal-real-time)) (done nil)
          (budget (* (if (> (length *tile-jobs*) 40) (* 3 *land-budget-ms*) *land-budget-ms*)      ; catching up after startup and a rebase
                     (/ internal-time-units-per-second 1000))))
      (loop while (and *tile-jobs* (< (- (get-internal-real-time) t0) budget))
            do (when (timed :chunk-slice (funcall (cdr (first *tile-jobs*))))
                 (let ((tl (gethash (car (pop *tile-jobs*)) *tiles*))) (when tl (setf (second tl) t done t)))))
      (when done (show-tiles)))))

(defun pick-weighted (plist)
  (let ((x (random 1.0)))
    (loop for (k w) on plist by #'cddr do (decf x w) (when (<= x 0) (return-from pick-weighted k)))
    (first plist)))

(defun crown (g dims rgb x y z &rest keys)
  "Build the crown of a deciduous tree in G for the current season; in winter, bare branches."
  (let ((c (season-crown rgb)))
    (if c
        (apply #'shape g :rock dims c x y z keys)
        (dolist (a '(0 2.1 4.2))
          (shape g :box (list .06 (* .9 (first dims)) .06) "#4a3424" (* .3 (cos a)) (- y (* .2 (first dims))) (* .3 (sin a))
                 :rot (list (* .5 (sin a)) 0 (* .5 (cos a))))))))

(defun build-prop (g kind side)
  "Build a thing of kind KIND in the group G (already in place).
SIDE is ±1, the side of the road. The colours of foliage and flowers follow the season."
  (case kind
    (:pine (shape g :cyl '(.15 .25 3 5) "#3a2a1e" 0 1.5 0)
     (loop for j below 3 do (shape g :cone (list (- 1.4 (* j .35)) 1.8) (season-conifer (nth j '("#1e3a24" "#24482c" "#2a5232"))) 0 (+ 2.4 j) 0)))
    (:round (shape g :cyl '(.18 .25 2.4 5) "#4a3424" 0 1.2 0) (crown g '(1.4) "#3a6a2a" 0 3 0))
    (:autumn (shape g :cyl '(.16 .24 2.6 5) "#4a3424" 0 1.3 0)
     (crown g (list (+ 1.3 (random .5))) (nth (random 3) '("#d86a20" "#c84020" "#e0a030")) 0 3.1 0))
    (:birch (shape g :cyl '(.1 .14 4 6) "#e0dcd0" 0 2 0) (crown g '(1.0) "#8ab040" 0 4 0))
    (:bush (crown g (list (+ .6 (random .4))) "#4a6a2a" 0 .35 0 :scale '(1.4 .7 1.2)))
    (:snag (shape g :cyl '(.08 .2 2.6 5) "#5a5048" 0 1.3 0 :rot (list .15 0 .1))
     (shape g :box '(.06 1.0 .06) "#5a5048" .3 2.0 0 :rot (list 0 0 -.8))
     (shape g :box '(.05 .7 .05) "#5a5048" -.25 1.6 0 :rot (list 0 0 .9)))
    (:lantern (shape g :box '(.12 2.2 .12) "#4a3626" 0 1.1 0) (shape g :box '(.6 .06 .06) "#4a3626" (* side -.25) 2.15 0)
     (shape g :box '(.2 .26 .2) "#ffd080" (* side -.5) 1.95 0 :emit "#ffb050" :energy 1.2))
    (:mush (loop for j below 5 for c = (nth (mod j 3) '("#40e0d0" "#ff70c0" "#a080ff"))
                 do (shape g :box '(.05 .2 .05) "#e8e0d0" (* (- j 2) .25) .1 (* (sin j) .3))
                    (shape g :ball '(.14 6 3) c (* (- j 2) .25) .24 (* (sin j) .3) :emit c :energy .4 :scale '(1 .5 1))))
    (:rock (shape g :rock (list (+ .5 (random .6))) "#4a4648" 0 .3 0) (shape g :rock '(.3) "#3a5a3a" .5 .2 .3))
    (:cairn (loop for j below 4 do (shape g :rock (list (- .32 (* j .06))) "#6a6670" 0 (+ .25 (* j .4)) 0)))
    (:cliff (shape g :rock (list (+ 4 (random 3.0))) "#46424e" 0 2.5 0 :scale '(1 1.6 1)))
    (:flowers (loop repeat (if (eq *season* :autumn) 5 12)
                    for c = (nth (random 5) '("#ff70a0" "#ffe060" "#a080ff" "#ffffff" "#ff9040"))
                    for x = (- (random 3.0) 1.5) for z = (- (random 3.0) 1.5)
                    do (shape g :box '(.03 .3 .03) "#4a8a3a" x .15 z :shadow nil)
                       (shape g :ball '(.08 5 3) c x .32 z :shadow nil)))
    (:drift (shape g :rock (list (+ .5 (random .5))) "#f0f4f8" 0 0 0 :scale '(1.6 .35 1.2)))
    (:hay (shape g :cyl '(.6 .6 1 10) "#d8b050" 0 .6 0 :rot (list 0 0 (/ pi 2))))
    (:fence (loop for j below 4 do (shape g :box '(.1 .8 .1) "#7a5a3a" 0 .4 (- (* j .9) 1.35))
                                   (when (> j 0) (shape g :box '(.06 .08 .9) "#7a5a3a" 0 .55 (- (* j .9) 1.8)))))
    (:pumpkin (loop for j below 3 do (shape g :ball (list (+ .25 (* j .05)) 8 5) "#e07020" (- (* j .6) .6) .22 (* (sin j) .4)
                                            :scale '(1 .75 1) :emit (when (= j 1) "#ff8020") :energy .6)))
    (:campfire (loop for j below 4 do (shape g :box '(.8 .1 .1) "#3a2a1e" 0 .06 0 :rot (list 0 (* j .8) 0)))
     (shape g :cone '(.25 .6 5) "#ffa030" 0 .4 0 :emit "#ff7010" :energy 2))
    (:reeds (loop repeat 9 do (shape g :box (list .04 (+ .9 (random .5)) .04) (if (eq *season* :winter) "#8a7a5a" "#6a7a3a")
                                     (- (random 1.6) .8) .5 (- (random 1.6) .8) :shadow nil))))
  (when (member kind '(:flowers :mush :reeds :pumpkin :lantern :campfire :rock :cairn :fence))
    (loop for child across (node-get-children g)
          do (object-set child "visibility_range_end" *small-far*)
             (object-set child "visibility_range_end_margin" 15.0)
             (object-set child "visibility_range_fade_mode" *small-fade*))))

(defun add-prop (d side)
  (when (bare-at d) (return-from add-prop nil))                  ; a bridge, a tunnel
  (when (town-at d) (return-from add-prop (town-prop d side)))   ; a town has its own roadside (doroga.lisp)
  (let* ((veg (pick-weighted (layer-weights :veg d)))
         (kinds (getf *veg-props* veg))
         (kind (season-prop (nth (random (length kinds)) kinds)))
         (x (* side (if (eq kind :cliff) (+ 16 (random 8.0)) (+ (if (< side 0) 7.8 4) (random 12.0)))))   ; on the left — beyond the oncoming lane
         (h (min (height-at x d) (height-at (- x 1.2) d) (height-at (+ x 1.2) d)))   ; on a slope — by the lower edge, so it does not hang in the air
         (wet (> (max (weight :water :lake d) (weight :water :river d)) .01)))
    (when (and wet (< h (- *water-y* .1))) (setf kind :reeds))     ; only reeds grow in water
    (destructuring-bind (wx wy wz) (road-pos d x)                 ; at a fork this spot may turn out to be another road
      (declare (ignore wy))
      (when (on-road-p wx wz) (return-from add-prop nil)))
    (let ((g (apply #'group *world* (road-pos d x (if wet (max *water-y* h) (- h .3))))))
      (node-3d-set-rotation g (v3 0 (- (road-frame d)) 0))
      (build-prop g kind side)
      (setf *props* (nconc *props* (list (cons g d)))))))

;;; The window around the wagon. Everything by the roadside is built not 'ahead in ascending path coordinate'
;;; but in a window around the wagon: far along its heading (*AHEAD*), a little behind (*BEHIND*). Where it is
;;; heading is *DIR*: +1 along the path, −1 against it. COVER builds up the things standing on a grid (poles,
;;; lanterns, trees), LISTED the things standing by a list (bus stops, gas stations, steles); whatever has
;;; left the window is removed by the SWEEPs. A jump to another place on the map is to them just 'the window moved'.

(defparameter *lane-x* -4.5 "The middle of the second lane (sideways from the path axis).
It carries the traffic oncoming to those driving forward.")

(defun lane () (if (> *dir* 0) 0.0 *lane-x*))
(defun ahead-of (d) "Return the distance to path coordinate D along the wagon's heading (negative when behind)." (* (- d *dist*) *dir*))
(defun side-x (off) "Convert OFF, to the right of the wagon, to the offset sideways from the path axis." (+ (lane) (* off *dir*)))
(defun turning-p () (> (abs (- *yaw-now* (if (> *dir* 0) 0.0 pi))) .03))

(defparameter *turn-legs* '((1 80) (-1 20) (1 80))
  "The U-turn of a lorry on a road too narrow to turn in one go: forward with the wheel to the left, back
with it to the right, forward to the left again. Each leg is (direction degrees): the heading turns the
same way on every leg, by so many degrees, along an arc.")
(defparameter *turn-speed* 1.8 "The speed of the truck during a U-turn, m/s.")
(defparameter *turn-pause* .5 "The pause between the legs of a U-turn, s: the driver changes gear.")
(defvar *u-turn* nil "The U-turn under way: (radius sign leg turned pause), or NIL.")

(defun turn-shift ()
  "Return the sideways shift a whole U-turn of radius 1 makes, along the legs of *TURN-LEGS*."
  (let ((th 0.0) (shift 0.0))
    (loop for (dir deg) in *turn-legs*
          do (let ((a (* deg (/ pi 180))))
               (incf shift (* dir (- (cos th) (cos (+ th a)))))
               (incf th a)))
    shift))

(defun step-turn (delta)
  "Perform the U-turn: the truck drives along arcs, as a lorry turns on a narrow road (*TURN-LEGS*): the
heading turns only while it moves, and it moves from its lane to the other one. Only the driver moves
the truck along the road; guests are told where it stands."
  (let ((want (if (> *dir* 0) 0.0 pi)))
    (cond ((not (turning-p)) (setf *u-turn* nil *yaw-now* want)
           (let ((dl (- (lane) *lane-now*)) (ls (* 1.8 delta)))       ; the last few centimetres
             (setf *lane-now* (if (< (abs dl) ls) (lane) (+ *lane-now* (* ls (signum dl)))))))
          (t (unless *u-turn*
               ;; the heading turns the way that takes the first leg forward towards the other lane
               (let* ((shift (- (lane) *lane-now*)) (sign (* (if (minusp shift) -1 1) (if (minusp (cos *yaw-now*)) -1 1))))
                 (setf *u-turn* (list (max 2.0 (/ (abs shift) (turn-shift))) sign 0 0.0 0.0))))
             (destructuring-bind (r sign leg turned pause) *u-turn*
               (let ((spec (nth leg *turn-legs*)))
                 (cond ((plusp pause) (setf (fifth *u-turn*) (max 0.0 (- pause delta))))
                       ((null spec) (setf *yaw-now* want *lane-now* (lane) *u-turn* nil))
                       (t (destructuring-bind (dir deg) spec
                            (let* ((left (- (* deg (/ pi 180)) turned))
                                   (da (min left (/ (* *turn-speed* delta) r))) (ds (* r da)) (th *yaw-now*))
                              ;; heading TH from the road: forward it moves along the road by cos TH and towards *LANE-X* by sin TH
                              (setf *yaw-now* (+ th (* sign da)))
                              (incf *lane-now* (* dir ds (sin th)))
                              (unless (net-guest-p) (incf *dist* (* dir ds (cos th))))
                              (if (>= da left)
                                  (setf (third *u-turn*) (1+ leg) (fourth *u-turn*) 0.0 (fifth *u-turn*) *turn-pause*)
                                  (setf (fourth *u-turn*) (+ turned da)))))))))))))

(defvar *spans* (make-hash-table :test 'equal) "The table of what is already built on the grid: key → (first . last) grid node number.")
(defvar *listed* (make-hash-table :test 'equal) "The table of what is already built by the list: key → (path-coordinate . reach).")

(defun win-lo (&optional (reach 0.0)) (- *dist* (if (> *dir* 0) *behind* *ahead*) reach))
(defun win-hi (&optional (reach 0.0)) (+ *dist* (if (> *dir* 0) *ahead* *behind*) reach))

;;; The path coordinate is the address of a thing on the road, not space. There may be other roads near the
;;; wagon: at a fork, at a crossroads. Their pieces inside the visibility circle are a window too — *SIDE-ROADS*;
;;; then whatever stands on a neighbouring road is seen in advance, and moving onto it swaps nothing out.

(defvar *side-roads* '() "The pieces of other roads inside the visibility circle: (road-start from to) in path coordinates.")
(defvar *side-tick* 0)

(defun note-side-roads (&optional now)
  "Recompute *SIDE-ROADS* once every few frames, or right away if NOW: the wagon has been moved."
  (when (or now (zerop (mod (incf *side-tick*) 8)))
    (multiple-value-bind (x z) (road-point *dist*)
      (let ((own (edge-of *dist*)) (r (float *ahead* 1.0)) (found '()))
        (loop for (d nil dist e) in (roads-near x z)
              do (unless (or (eq e own) (>= dist r))
                   (let* ((half (sqrt (- (* r r) (* dist dist))))
                          (lo (max (edge-d0 e) (- d half))) (hi (min (edge-d1 e) (+ d half)))
                          (old (assoc (edge-d0 e) found)))
                     (if old
                         (setf (second old) (min lo (second old)) (third old) (max hi (third old)))
                         (push (list (edge-d0 e) lo hi) found)))))
        (setf *side-roads* found)))))

(defun in-window-p (d &optional (reach 0.0))
  "Return true if path coordinate D is in the window (with the margin REACH).
That is, on our own road near the wagon or on a piece of a neighbouring one."
  (or (< (win-lo reach) d (win-hi reach))
      (some (lambda (s) (< (- (second s) reach) d (+ (third s) reach))) *side-roads*)))

(defun span-in-window-p (lo hi &optional (reach 0.0))
  "Return true if the path segment from LO to HI touches the window (with the margin REACH)."
  (or (and (< lo (win-hi reach)) (> hi (win-lo reach)))
      (some (lambda (s) (and (< lo (+ (third s) reach)) (> hi (- (second s) reach)))) *side-roads*)))

(defun out-of-window-p (d &optional (reach 0.0)) (not (in-window-p d (+ reach 6.0))))

(defun cover (key step fn &optional (reach 0.0))
  "Call FN (path-coordinate number) for every grid node with spacing STEP in the window and not yet built.
What is built is tracked per road: *SPANS* keeps under KEY a list of (road-start first-node . last-node)."
  (let ((book (gethash key *spans*)) (fresh '()))
    (flet ((span (edge lo hi)
             (let* ((i0 (ceiling lo step)) (i1 (floor hi step)) (old (cdr (assoc edge book))))
               (if (or (null old) (> i0 (1+ (cdr old))) (< i1 (1- (car old))))
                   (loop for i from i0 to i1 do (with-solids (funcall fn (* i step) i)))          ; the window moved — everything anew
                   (progn (loop for i from (1+ (cdr old)) to i1 do (with-solids (funcall fn (* i step) i)))
                          (loop for i from i0 below (car old) do (with-solids (funcall fn (* i step) i)))))
               (push (cons edge (cons i0 i1)) fresh))))
      (span (edge-d0 (edge-of *dist*)) (win-lo reach) (win-hi reach))
      (loop for (edge lo hi) in *side-roads* do (span edge (- lo reach) (+ hi reach))))
    (setf (gethash key *spans*) fresh)))

(defvar *keep-reach* 0.0 "The margin beyond the window within which to keep what is being built now (a gas station is seen from afar).")

(defmacro listed ((key d &optional (reach 0.0)) &body body)
  "Build the thing KEY standing at path coordinate D while it is in the window (with the margin REACH).
It is built once, until it is removed. This is called every frame for everything on the map, so the cheap
window check comes first, and the key and the building itself are evaluated only for what is nearby."
  (let ((dv (gensym "D")) (rv (gensym "REACH")) (kv (gensym "KEY")))
    `(let ((,dv ,d) (,rv ,reach))
       (when (in-window-p ,dv ,rv)
         (let ((,kv ,key))
           (unless (gethash ,kv *listed*)
             (setf (gethash ,kv *listed*) (cons ,dv ,rv))
             (let ((*keep-reach* ,rv)) (with-solids ,@body))))))))

(defun sweep-listed ()
  (maphash (lambda (key v) (when (out-of-window-p (car v) (cdr v)) (remhash key *listed*))) *listed*))

(defun step-props ()
  (cover :props *prop-step* (lambda (d i) (add-prop d (if (evenp i) 1 -1))))
  (setf *props* (remove-if (lambda (p) (when (out-of-window-p (cdr p)) (node-queue-free (car p)) t)) *props*)))

;;; Bridges — where the river ahead crosses the road

(defun add-bridge (d)
  (let ((g (apply #'group *world* (road-pos d))))
    (multiple-value-bind (course slope) (road-frame d)
      (node-3d-set-rotation g (v3 slope (- course) 0)))
    (model g "wood-bridge")
    (push (cons g d) *bridges*)))

(defun step-bridges ()
  "Build a little bridge where the stream passes from one side of the road to the other."
  (cover :bridges 20.0
         (lambda (d i)
           (declare (ignore i))
           ;; the stream must be real: water right by the roadbed on at least one side. In a town and on
           ;; level grounds the riverbed is filled in — there a bridge would lie in the middle of a dry street
           ;; (that is how it was: of sixteen bridges on the ring road only one saw water)
           (when (and (/= (signum (river-x d)) (signum (river-x (- d 20.0)))) (> (weight :water :river d) .05)
                      (not (town-at d)) (not (bare-at d))
                      (some (lambda (x) (< (height-at x d) *water-y*)) '(-20.0 -12.0 9.0 16.0)))
             (add-bridge d))))
  (setf *bridges* (remove-if (lambda (b) (when (out-of-window-p (cdr b)) (node-queue-free (car b)) t)) *bridges*)))

;;; As a whole

(defun make-pivots (root)
  "Create the two pivots under ROOT.
Turn turns the world and the sun to the course; Rig tilts the wagon with the camera."
  (setf *turn* (group root) *rig* (group root)))

(defun make-world (parent)
  "Create World under PARENT (Turn): the things at the start of the path.
The land (Land) is a separate node; it outlives a rebuild of the world."
  (when *world* (node-queue-free *world*))
  (unless *land* (setf *land* (group parent)))
  (setf *world* (group parent) *dist* 0.0 *props* '() *bridges* '() *river-side* 0
        *origin* (list 0.0 0.0 0.0))
  (node-3d-set-position *land* (v3 0 0 0))
  (clrhash *spans*) (clrhash *listed*) (clear-solids)                       ; a new world — nothing has been built yet
  (step-props)
  (place-world))

(defun place-world ()
  "Put the wagon on the road point under it and return the course and the slope.
The Rig itself is placed by step-cart together with the sway. Far from the origin, move the origin: the
world of things and the land are shifted."
  (multiple-value-bind (x z course) (road-point *dist*)
    (let ((y (road-y *dist*)) (slope (* (cos *yaw-now*) (nth-value 1 (road-frame *dist*)))))
      (setf x (+ x (* *lane-now* (cos course))) z (+ z (* *lane-now* (sin course)))        ; its own lane
            course (+ course *yaw-now*))
      (destructuring-bind (ox oy oz) *origin*
        (declare (ignore oy))
        (when (> (max (abs (- x ox)) (abs (- z oz))) *rebase*)
          (setf *origin* (list (float (round x) 1.0) (float (round y) 1.0) (float (round z) 1.0)))
          (destructuring-bind (ox oy oz) *origin*
            (node-3d-set-position *world* (v3 (- ox) (- oy) (- oz)))
            (node-3d-set-position *land* (v3 (- ox) (- oy) (- oz))))))
      (destructuring-bind (ox oy oz) *origin*
        (setf *rig-at* (list (- x ox) (- y oy) (- z oz)) *course* (float course 1.0) *slope* slope))
      (values course slope))))

(defun step-world (delta)
  (step-turn delta)
  (note-side-roads)
  (if (and (off-ring-p) (or (> *dir* 0) (plusp *mega-s*)))                ; behind a door we stand still on the ring:
      (incf *mega-s* (* *dir* *speed* delta))                              ; the way driven there is its own count (place.lisp)
      (incf *dist* (* *dir* *speed* delta (cos (nth-value 1 (road-frame *dist*))))))   ; along the road itself — at the same speed
  (when (ring) (step-way))                                            ; an exit, a crossroads, an entry, a new lap
  (place-world)
  (unless (off-ring-p) (timed :chunks (step-chunks)))                  ; behind a door the land is not shown: it is not built either, and waits
  (step-roads)
  (timed :props (step-props))
  (step-bridges))
