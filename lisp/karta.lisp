;;;; Terrain map. The road is a curve on the (x, z) plane, not a straight line: a ring road through all
;;;; the towns. It is still convenient to name a world point as 'path coordinate D and so much sideways
;;;; from the axis' (everything by the road is placed that way), but now this is converted to map
;;;; coordinates (ROAD-POINT, SIDE-POINT), and the land height at any map point is computed by a single
;;;; function, LAND-HEIGHT: near a road from its strip (HEIGHT-IN from land.lisp: shoulders, lots,
;;;; lakes), far from all roads from the common field of hills and mountains, and blended in between.
;;;; If two roads are close (a fork), their strips are blended by proximity.
;;;; Heading: 0 points to -z, positive turns right (towards +x). Sideways to the right of the road is
;;;; (cos θ, sin θ).

(in-package #:kiosk-trip)

(defparameter *map-step* 2.0 "The spacing of the road curve points, in metres.")
(defparameter *ring-lead* 600.0 "The number of metres by which the ring road starts before path coordinate zero.
This puts the seam of the loop out in the fields rather than at the warehouse.")
(defparameter *ring-tail* 260.0 "The length of road after the last town up to the seam of the loop.")
(defparameter *grid-cell* 256.0 "The cell size of the 'which road is nearby' index, in metres.")
(defparameter *road-near* 300.0 "The distance from a road, in metres, up to which the land is that road's strip.")
(defparameter *road-far* 430.0 "The distance from a road beyond which the land is the common field.
Between *ROAD-NEAR* and this the two blend smoothly.")

(defstruct edge
  "A road on the map.
It covers path coordinates D0 to D1, holds points every *MAP-STEP* (x, z, heading) and records
in NEXT where its end leads."
  d0 d1 xs zs ths next)

(defvar *edges* '() "All the roads of the map; the first one is the ring road.")
(defvar *road-grid* (make-hash-table) "The map from a map cell to the road points in it, each as (ROAD . POINT-INDEX).")
(defvar *rows* (make-hash-table) "The road strip rows computed so far, keyed by path coordinate (a multiple of two metres).")

(defparameter *ring-top* 30000.0 "The upper bound of the ring road's path coordinates.
Spokes to and from the Crossroads start above it (40000, 70000 and so on).")
(defun ring-d-p (d) "Return true if path coordinate D is on the ring road." (< d *ring-top*))

(defun edge-of (d)
  "Return the road that path coordinate D belongs to."
  (or (find-if (lambda (e) (and (<= (edge-d0 e) d) (< d (edge-d1 e)))) *edges*) (first *edges*)))

(defun ring () (first *edges*))
(defun ring-length () (let ((e (ring))) (if e (- (edge-d1 e) (edge-d0 e)) 1e9)))

(defun wrap-d (d)
  "Return the ring-road path coordinate D wrapped into the ring's bounds.
Past the end of the loop comes its start again."
  (let ((e (ring)))
    (if (and e (ring-d-p d))
        (let ((len (- (edge-d1 e) (edge-d0 e))))
          (+ (edge-d0 e) (mod (- d (edge-d0 e)) len)))
        d)))

(defun road-point (d)
  "Return the road axis point at path coordinate D as three values: x, z and heading."
  (let ((e (edge-of d)))
    (if (null e)
        (values 0.0d0 (- (float d 1d0)) 0.0d0)                       ; no map yet: a straight line, as before
        (let* ((d (if (eq e (ring)) (wrap-d d) (max (edge-d0 e) (min (- (edge-d1 e) .01) d))))
               (u (/ (- d (edge-d0 e)) *map-step*)) (i (min (- (length (edge-xs e)) 2) (max 0 (floor u)))) (f (- u i))
               (xs (edge-xs e)) (zs (edge-zs e)) (ths (edge-ths e)))
          (values (+ (aref xs i) (* f (- (aref xs (1+ i)) (aref xs i))))
                  (+ (aref zs i) (* f (- (aref zs (1+ i)) (aref zs i))))
                  (+ (aref ths i) (* f (- (aref ths (1+ i)) (aref ths i)))))))))

(defun road-pos (d &optional (x 0.0) (up 0.0))
  "Return the world point (x y z) at road height for path coordinate D.
The point is X sideways from the axis and UP above the road. The land is not consulted."
  (multiple-value-bind (px pz th) (road-point d)
    (list (+ px (* x (cos th))) (+ (road-y d) up) (+ pz (* x (sin th))))))

(defun make-ring ()
  "Build the ring road through all the towns and return it as an EDGE.
It is an overall turn through a full circle plus local bends that follow the terrain. The end
is pulled towards the start so that the loop closes."
  (let* ((d0 (- *ring-lead*)) (d1 (+ (town-end (car (last *towns*))) *ring-tail*)) (len (- d1 d0))
         (n (ceiling len *map-step*)) (len (* n *map-step*))
         (xs (make-array (1+ n) :element-type 'double-float)) (zs (make-array (1+ n) :element-type 'double-float))
         (ths (make-array (1+ n) :element-type 'double-float)) (x 0d0) (z 0d0))
    (dotimes (i (1+ n))
      ;; In a town the street is almost straight: houses, fences and blocks are laid out by ruler, and on a
      ;; local 'snake' a long building would stick out onto the roadway. The overall gentle turn of the ring
      ;; road stays; local bends fade out 200 m before a town and come back 200 m after it.
      (let* ((d (+ d0 (* i *map-step*)))
             (wiggle (reduce #'min *towns* :initial-value 1.0
                             :key (lambda (tw) (cond ((< (town-start tw) d (town-end tw)) 0.0)
                                                     (t (min 1.0 (/ (min (abs (- d (town-start tw))) (abs (- d (town-end tw)))) 200.0)))))))
             (th (+ (* 2 pi (/ (* i *map-step*) len)) (* wiggle wiggle (- 3 (* 2 wiggle)) (heading (max 0.0 d))))))
        (setf (aref xs i) x (aref zs i) z (aref ths i) (float th 1d0))
        (incf x (* *map-step* (sin th))) (decf z (* *map-step* (cos th)))))
    (let ((ex (aref xs n)) (ez (aref zs n)))                          ; spread the closing error over the whole loop
      (dotimes (i (1+ n))
        (decf (aref xs i) (* ex (/ i n))) (decf (aref zs i) (* ez (/ i n)))))
    (setf (aref ths n) (+ (aref ths 0) (* 2 pi)))
    (make-edge :d0 d0 :d1 (+ d0 len) :xs xs :zs zs :ths ths :next d0)))

(defun grid-key (x z) (+ (* 100003 (floor x *grid-cell*)) (floor z *grid-cell*)))

(defun index-edge (e)
  "Enter the points of road E into the index, every fourth one (8 m apart)."
  (loop for i from 0 below (length (edge-xs e)) by 4
        do (push (cons e i) (gethash (grid-key (aref (edge-xs e) i) (aref (edge-zs e) i)) *road-grid*))))

(defvar *map-seed* nil "The seed for which the map has already been built.")

(defun build-map ()
  "Build the road map from the town layout.
Called from PLAN-PLACES, before the land. The same seed gives the same map, so in that case
neither the map nor the land already laid out is touched (turning around, a new lap)."
  (clrhash *rows*)
  (unless (and *edges* (eql *map-seed* *trip-seed*))
    (clrhash *road-grid*)
    (setf *edges* (list (make-ring)) *map-seed* *trip-seed*)
    (make-spokes)
    (mapc #'index-edge *edges*)
    (when *land* (clear-land) (clear-roads))))

(defun roads-near (x z)
  "Return the roads near the map point (X, Z) as a list of (PATH-COORDINATE SIDEWAYS DISTANCE ROAD).
On the inner side of a sharp bend a point can have several equally close sections of one road.
All of them are returned (the nearest point per 64 m of path, if it is closer than its
neighbours), otherwise the land would jump from one section to another.
Return NIL if no road is closer than about *GRID-CELL*."
  (let ((best '()) (cx (floor x *grid-cell*)) (cz (floor z *grid-cell*)))
    (loop for ix from (1- cx) to (1+ cx)
          do (loop for iz from (1- cz) to (1+ cz)
                   do (dolist (s (gethash (+ (* 100003 ix) iz) *road-grid*))
                        (let* ((e (car s)) (i (cdr s)) (dx (- x (aref (edge-xs e) i))) (dz (- z (aref (edge-zs e) i)))
                               (r2 (+ (* dx dx) (* dz dz))) (bucket (floor i 32))        ; 32 points 2 m apart make 64 m of path
                               (old (find-if (lambda (b) (and (eq (fourth b) e) (eql (seventh b) bucket))) best)))
                          (cond ((null old) (push (list r2 dx dz e i nil bucket) best))
                                ((< r2 (first old)) (setf (first old) r2 (second old) dx (third old) dz (fifth old) i)))))))
    ;; keep only those closer than both neighbours along the path; beyond the butt end of a road (before
    ;; its start or after its end) it has no strip: the road it merged with is in charge there
    (loop for b in best
          for found = (and (notany (lambda (o) (and (eq (fourth o) (fourth b)) (= 1 (abs (- (seventh o) (seventh b)))) (< (first o) (first b)))) best)
                           (destructuring-bind (r2 dx dz e i unused bucket) b
                             (declare (ignore unused bucket))
                             (let* ((th (aref (edge-ths e) i)) (lat (+ (* dx (cos th)) (* dz (sin th)))) (along (- (* dx (sin th)) (* dz (cos th))))
                                    (d (+ (edge-d0 e) (* i *map-step*) along)))
                               (when (or (eq e (ring)) (< (edge-d0 e) d (edge-d1 e)))
                                 (list d lat (sqrt r2) e)))))
          when found collect found)))

;;; ------------------------------------------------------------------ land height on the map

(defun row-near (d)
  "Return the strip row at path coordinate D rounded to two metres.
The row comes from a cache, because the land asks for it for thousands of points."
  (let ((key (round d 2)))
    (or (gethash key *rows*)
        (progn (when (> (hash-table-count *rows*) 9000) (clrhash *rows*))
               (setf *row* nil)
               (setf (gethash key *rows*) (row-at (* 2.0 key)))))))

(defvar *basins* '() "The lowlands on the map far from roads, each as (X Z RADIUS HEIGHT).
The sea at the port is one of them.")

(defun wild-height (x z)
  "Return the land height at (X, Z) as it is far from all roads.
It is a large field of hills with occasional ridges; in the lowlands (*BASINS*) the bottom is flat."
  (let ((h (+ 14.0
              (* 90.0 (- (fbm (+ (* x .0011) 3.0) (+ (* z .0011) 9.0)) .42))
              (* 150.0 (expt (fbm (+ (* x .0027) 70.0) (* z .0027)) 2))
              (* 5.0 (fbm (* x .02) (* z .02))))))
    (dolist (b *basins* h)
      (destructuring-bind (bx bz r y) b
        (let ((dist (sqrt (+ (expt (- x bx) 2) (expt (- z bz) 2)))))
          (when (< dist r) (setf h (+ h (* (- 1 (sstep (* r .6) r dist)) (- y h))))))))))

(defun strip-height (d lat)
  "Return the height above world level in the road strip at path coordinate D, LAT sideways.
Under the roadway itself the land is slightly lower than it, so that the road ribbon never sinks
into the ground (except on a bridge over a chasm, where the land has dropped away)."
  (let* ((row (row-near d)) (h (height-in lat row)))
    (unless (deck-at d)
      (let ((k (sstep 8.5 11.5 (abs (+ lat 2.0)))))                   ; corridor: both lanes and the pavements
        (setf h (+ (* k h) (* (- 1 k) (min h -.14))))))
    (+ (road-y d) h)))

(defun land-height (x z)
  "Return the land height at the map point (X, Z).
The second value is the nearest road as (PATH-COORDINATE SIDEWAYS DISTANCE ROAD), or NIL.
Several roads or sections may be nearby: their strips are blended by proximity, and the one
right underfoot (closer than about 12 m) outweighs all the others, so that a neighbouring
road's land never creeps onto the roadway."
  (let ((near (roads-near x z)) (wild (wild-height x z)))
    (if (null near)
        (values (float wild 1.0) nil)
        (let* ((best (first (sort (copy-list near) #'< :key #'third))) (rmin (third best)) (sum 0.0) (total 0.0))
          (dolist (r near)
            (destructuring-bind (d lat dist e) r
              (declare (ignore e))
              (let ((w (* (+ 1e-6 (expt (- 1 (sstep 0 *road-near* dist)) 4))
                          (+ 1 (* 1e6 (- 1 (sstep 6 16 (abs lat)))))                  ; the road underfoot matters more
                          (- 1 (sstep 0 40 (- dist rmin))))))                          ; a farther section fades out smoothly
                (when (> w 0)
                  (incf sum (* w (strip-height d (max (- *road-far*) (min *road-far* lat))))) (incf total w)))))
          (let ((k (sstep *road-near* *road-far* rmin)))
            (values (float (+ (* (- 1 k) (/ sum total)) (* k wild)) 1.0) best))))))

;;; ------------------------------------------------------------------ network: exits to the Crossroads and outs from it
;;; Six spokes meet at the Crossroads in the middle of the map. Three of them are 'exits': their path
;;; coordinate grows from the ring road to the Crossroads; three are 'outs': it grows from the Crossroads
;;; to the ring road. This is only the direction of counting: all of them are driven both ways (see
;;; TURNS-FROM below).

(defparameter *exit-plan* '((3 .12) (7 .12) (11 .10)) "The exits from the ring road.
Each entry is (INDEX FRACTION): the stretch before town number INDEX and the fraction of that stretch.")
(defparameter *merge-plan* '((1 .88) (3 .90) (9 .90)) "The outs onto the ring road, in the same format as *EXIT-PLAN*.")
(defvar *exits* '() "The exits, each as (RING-PATH-COORDINATE . ROAD-TO-THE-CROSSROADS).")
(defvar *outs* '() "The spokes leading away from the Crossroads.
Their NEXT is the ring-road path coordinate they come out at.")
(defvar *hub* nil "The position of the Crossroads, as (X Z).")
(defvar *way* '() "The planned turns in order, each as (WHERE TO DIRECTION); see ROUTE.")
(defvar *spoke-stations* '() "The path coordinates of the gas stations on the spokes to the Crossroads.")

(defun curve-edge (d0 ax az ath bx bz bth next)
  "Build a road as a smooth arc from point (AX, AZ) with heading ATH to point (BX, BZ) with heading BTH.
The road starts at path coordinate D0, has points every *MAP-STEP* and leads to NEXT. Return the EDGE."
  (let* ((m (* .8 (sqrt (+ (expt (- bx ax) 2) (expt (- bz az) 2)))))
         (t0x (* m (sin ath))) (t0z (* m -1 (cos ath))) (t1x (* m (sin bth))) (t1z (* m -1 (cos bth)))
         (n 1600) (px (make-array (1+ n))) (pz (make-array (1+ n))) (acc (make-array (1+ n))))
    (dotimes (i (1+ n))
      (let* ((u (/ i (float n 1d0))) (u2 (* u u)) (u3 (* u2 u))
             (h00 (+ (* 2 u3) (* -3 u2) 1)) (h10 (+ u3 (* -2 u2) u)) (h01 (+ (* -2 u3) (* 3 u2))) (h11 (- u3 u2)))
        (setf (aref px i) (+ (* h00 ax) (* h10 t0x) (* h01 bx) (* h11 t1x))
              (aref pz i) (+ (* h00 az) (* h10 t0z) (* h01 bz) (* h11 t1z))
              (aref acc i) (if (zerop i) 0d0 (+ (aref acc (1- i)) (sqrt (+ (expt (- (aref px i) (aref px (1- i))) 2)
                                                                        (expt (- (aref pz i) (aref pz (1- i))) 2))))))))
    (let* ((k (floor (aref acc n) *map-step*))
           (xs (make-array (1+ k) :element-type 'double-float)) (zs (make-array (1+ k) :element-type 'double-float))
           (ths (make-array (1+ k) :element-type 'double-float)) (i 0))
      (dotimes (s (1+ k))
        (let ((want (* s *map-step*)))
          (loop while (and (< i (1- n)) (< (aref acc (1+ i)) want)) do (incf i))
          (let* ((f (/ (- want (aref acc i)) (max 1d-6 (- (aref acc (1+ i)) (aref acc i)))))
                 (dx (- (aref px (1+ i)) (aref px i))) (dz (- (aref pz (1+ i)) (aref pz i)))
                 (th (atan dx (- dz))))
            (setf (aref xs s) (float (+ (aref px i) (* f dx)) 1d0) (aref zs s) (float (+ (aref pz i) (* f dz)) 1d0))
            ;; the heading must not jump across ±π between adjacent points, or the wagon would spin around
            (setf (aref ths s) (if (zerop s) (float th 1d0)
                                   (let ((prev (aref ths (1- s)))) (+ prev (- (mod (+ (- th prev) pi) (* 2 pi)) pi))))))))
      (make-edge :d0 d0 :d1 (+ d0 (* k *map-step*)) :xs xs :zs zs :ths ths :next next))))

(defun make-spokes ()
  "Build the Crossroads in the middle of the ring road and the six spokes to and from it."
  (let* ((r (ring)) (n (length (edge-xs r)))
         (cx (/ (loop for i below n sum (aref (edge-xs r) i)) n)) (cz (/ (loop for i below n sum (aref (edge-zs r) i)) n)))
    (setf *hub* (list cx cz) *exits* '() *outs* '())
    (loop for (i f) in *exit-plan* for k from 0
          do (let ((da (gap-d i f)))
               (multiple-value-bind (ax az ath) (road-point da)
                 (push (cons da (curve-edge (+ 40000.0 (* 10000 k)) ax az ath cx cz (atan (- cx ax) (- (- cz az))) :hub)) *exits*))))
    (loop for (i f) in *merge-plan* for k from 0
          do (let ((db (gap-d i f)))
               (multiple-value-bind (bx bz bth) (road-point db)
                 (push (curve-edge (+ 70000.0 (* 10000 k)) cx cz (atan (- bx cx) (- (- bz cz))) bx bz bth db) *outs*))))
    (setf *exits* (sort *exits* #'< :key #'car) *outs* (nreverse *outs*))
    (setf *edges* (append (list r) (mapcar #'cdr *exits*) *outs*))))

(defun off-road-p (d)
  "Return true if path coordinate D is not on a road.
That is past the road's end or right at the Crossroads, where nothing is placed."
  (and (not (ring-d-p d))
       (notany (lambda (e) (< (+ (edge-d0 e) (if (eq (edge-next e) :hub) 30 150)) d (- (edge-d1 e) (if (eq (edge-next e) :hub) 190 40))))
               (rest *edges*))))

(defun ring-ahead (a b)
  "Return the driving distance along the ring road from path coordinate A to path coordinate B.
If B is behind, the distance goes around the loop; if B was overshot by a couple of metres, it is zero."
  (let ((left (- b a)))
    (cond ((< -40.0 left 0.0) 0.0) ((< left 0.0) (+ left (ring-length))) (t left))))

(defun edge-len (e) (- (edge-d1 e) (edge-d0 e)))

;;; Roads are two-way. A route is a list of turns (where to direction): 'having driven along your road to
;;; path coordinate WHERE, switch to path coordinate TO and drive in DIRECTION' (+1 is towards growing
;;; path coordinates, -1 is back). Forks merge tangentially, as in real life: you leave the ring road for
;;; an exit only when driving forward, and when driving the ring road backwards you leave onto an out,
;;; towards the Crossroads. At the Crossroads you may take any of the six spokes.

(defun ring-gap (a b dir) (if (> dir 0) (ring-ahead a b) (ring-ahead b a)))

(defun hub-turns (e at)
  (append (loop for o in *outs* unless (eq o e) collect (list at (edge-d0 o) 1))
          (loop for x in *exits* unless (eq (cdr x) e) collect (list at (- (edge-d1 (cdr x)) 1.0) -1))))

(defun turns-from (e dir)
  "Return the turns available from road E when driving in direction DIR.
The result is a list of (WHERE TO NEW-DIRECTION)."
  (cond ((eq e (ring))
         (if (> dir 0)
             (loop for x in *exits* collect (list (car x) (edge-d0 (cdr x)) 1))
             (loop for o in *outs* collect (list (edge-next o) (- (edge-d1 o) 1.0) -1))))
        ((eq (edge-next e) :hub)                                        ; exit: forward is the Crossroads, back is the ring road
         (if (> dir 0)
             (hub-turns e (edge-d1 e))
             (list (list (edge-d0 e) (car (rassoc e *exits*)) -1))))
        (t (if (> dir 0)                                                ; out: forward is the ring road, back is the Crossroads
               (list (list (edge-d1 e) (edge-next e) 1))
               (hub-turns e (edge-d0 e))))))

(defun route (pos dir target &optional (depth 4))
  "Return the shortest route from path coordinate POS, driving in direction DIR, to path coordinate TARGET.
The result is (LENGTH . TURNS), or NIL if no route is found within DEPTH turns."
  (let ((e (edge-of pos)) (best nil))
    (flet ((try (len hops) (when (or (null best) (< len (car best))) (setf best (cons len hops))))
           (gap (to) (if (eq e (ring)) (ring-gap pos to dir) (* dir (- to pos)))))
      (when (and (eq (edge-of target) e) (>= (gap target) -40.0))
        (try (max 0.0 (gap target)) '()))
      (when (> depth 0)
        (dolist (turn (turns-from e dir))
          (when (>= (gap (first turn)) -1.0)
            (let ((more (route (second turn) (third turn) target (1- depth))))
              (when more (try (+ (max 0.0 (gap (first turn))) (car more)) (cons turn (cdr more)))))))))
    best))

(defvar *dir* 1 "The driving direction: +1 is towards growing path coordinates, -1 is back.")

(defun plan-way (target &optional (may-turn t))
  "Return the shortest route to path coordinate TARGET as three values.
The values are the length, the turns and the direction to set off in. Turning around on the
spot is allowed only if MAY-TURN is true (we stand where there is room)."
  (let ((a (route *dist* *dir* target)) (b (and may-turn (route *dist* (- *dir*) target))))
    (cond ((and b (or (null a) (< (+ (car b) 30.0) (car a)))) (values (car b) (cdr b) (- *dir*)))
          (a (values (car a) (cdr a) *dir*))
          (t (values nil nil *dir*)))))

(defun default-turn (e dir)
  "Return the turn taken from the end of road E in direction DIR when no turn is planned.
The ring road is endless, so it has no such turn and the result is NIL."
  (unless (eq e (ring)) (first (turns-from e dir))))

(defun way-on (d)
  "Return the driving distance to path coordinate D along the planned turns (*WAY*).
Return NIL if D is not on the way."
  (let ((pos *dist*) (dir *dir*) (total 0.0) (way *way*))
    (dotimes (leg 8 nil)
      (let* ((e (edge-of pos)) (ringp (eq e (ring))) (turn (or (first way) (default-turn e dir))))
        (flet ((gap (to) (if ringp (ring-gap pos to dir) (* dir (- to pos)))))
          (when (and (eq (edge-of d) e) (>= (gap d) -40.0)
                     (or (null turn) (not ringp) (<= (gap d) (gap (first turn)))))
            (return (+ total (max 0.0 (gap d)))))
          (unless turn (return nil))
          (incf total (max 0.0 (gap (first turn))))
          (setf pos (second turn) dir (third turn) way (rest way)))))))

(defun way-to (d) (or (way-on d) (abs (- d *dist*))))

(defvar *yaw-now* 0.0 "The angle by which the wagon is currently turned away from the road heading.
0 means driving forward, π means backward; values in between occur during a turnaround.")

(defvar *lane-now* 0.0 "The lane offset the wagon is currently in.
0 means driving forward, *LANE-X* means backward; values in between occur during a turnaround.")

(defun reset-heading () "Stand as at the start of a trip: nose forward, in the lane of that direction." (setf *dir* 1 *yaw-now* 0.0 *lane-now* 0.0))
(defun head-back () "Stand nose backwards, in the lane of that direction: a game saved while driving back." (setf *dir* -1 *yaw-now* pi *lane-now* *lane-x*))

;;; The wagon and the map. The player lives in wagon coordinates (right, back), as is usual for anything
;;; that walks inside a vehicle. Everything that stands outside is converted into them by a real rotation
;;; and shift, not 'along the road': on a bend of the road an object stays where it is drawn.

(defun cart-local (wx wz)
  "Return the map point (WX, WZ) in wagon coordinates as two values: right and back."
  (multiple-value-bind (px pz th) (road-point *dist*)
    (let* ((c (+ th *yaw-now*)) (dx (- wx px (* *lane-now* (cos th)))) (dz (- wz pz (* *lane-now* (sin th)))))
      (values (+ (* dx (cos c)) (* dz (sin c))) (- (* dz (cos c)) (* dx (sin c)))))))

(defun cart-world (lx lz)
  "Return the map point, as two values, of the wagon-relative point LX to the right and LZ back."
  (multiple-value-bind (px pz th) (road-point *dist*)
    (let ((c (+ th *yaw-now*)))
      (values (+ px (* *lane-now* (cos th)) (* lx (cos c)) (* lz -1 (sin c)))
              (+ pz (* *lane-now* (sin th)) (* lx (sin c)) (* lz (cos c)))))))

(defun on-road-p (wx wz &optional (margin 1.0))
  "Return true if the map point (WX, WZ) is on the roadway of some road (both lanes, plus MARGIN)."
  (some (lambda (c) (< (- *lane-x* 2.25 margin) (second c) (+ 2.25 margin))) (roads-near wx wz)))

(defun deep-at-p (wx wz)
  "Return true if the map point (WX, WZ) is water or a drop.
That means the land there is 2.6 m below the nearest road, or below the water level where the
road has a lake or a river beside it. A bridge deck can be walked on."
  (let ((near (first (sort (roads-near wx wz) #'< :key #'third))))
    (if (null near)
        (< (- (land-height wx wz) (road-y *dist*)) -2.6)                 ; no roads nearby: measure from the wagon's road
        (destructuring-bind (d lat dist e) near
          (declare (ignore dist e))
          (let ((drop (- (land-height wx wz) (road-y d))))
            (and (not (and (deck-at d) (< (abs lat) 6.0)))
                 (or (< drop -2.6)
                     (and (< drop (- *water-y* .25))
                          (> (max (weight :water :lake d) (weight :water :river d)) .01)))))))))

(defun hop (to &optional (dir *dir*))
  "Switch to path coordinate TO on another road and drive in direction DIR.
This covers an exit, the Crossroads, an out and a new lap. The land and the roads stay the same,
since they follow the map; the roadside is built up within the window. The wagon keeps facing
where it was facing: at the Crossroads it turns onto the new road smoothly."
  (let ((facing (+ (nth-value 2 (road-point *dist*)) *yaw-now*)))
    (setf *dir* dir)
    (relocate to)
    (setf *yaw-now* (- facing (nth-value 2 (road-point *dist*))))))

(defun step-way ()
  "Switch to the next road on reaching a planned turn or the end of the road.
The driver decides; a guest follows (net.lisp moves the guest by itself when the driver's
path coordinate jumps)."
  (unless (net-guest-p)
    (let* ((e (edge-of *dist*)) (turn (first *way*)))
      (flet ((take (turn) (when (eq turn (first *way*)) (pop *way*)) (hop (second turn) (third turn))))
        (cond ((eq e (ring))
               (cond ((and turn (<= 0.0 (* *dir* (- *dist* (first turn))) 30.0)) (take turn))
                     ((>= *dist* (edge-d1 e)) (hop (+ (edge-d0 e) (- *dist* (edge-d1 e)))))        ; a new lap
                     ((< *dist* (edge-d0 e)) (hop (- (edge-d1 e) (- (edge-d0 e) *dist*) .01)))))
              ((if (> *dir* 0) (>= *dist* (- (edge-d1 e) .5)) (<= *dist* (+ (edge-d0 e) .5)))
               (take (or turn (default-turn e *dir*)))))))))

(defun plan-junctions ()
  "Plan the lots and level sections at the exits, the outs and the Crossroads.
Gas stations before the Crossroads are planned too. Called from PLAN-PLACES."
  (setf *spoke-stations* '())
  (when *exits*
    (let ((hub-y (raw-road-y (- (edge-d1 (cdr (first *exits*))) 60))))
      (flet ((flat (d0 d1 y ramp) (push (list d0 d1 y ramp) *levels*))
             (apron (d0 d1) (push (make-pad :d0 d0 :d1 d1 :ramp 30.0 :x0 -70.0 :x1 70.0 :m0 60.0 :m1 60.0 :y -.14) *pads*)))
        (loop for (da . e) in *exits*
              do (let ((da da) (d0 (edge-d0 e)))                                 ; exit: at ring road height while the roads are close
                   (flat d0 (+ d0 320) (lambda (d) (road-y (+ da (- d d0)))) 200.0))
                 (flat (- (edge-d1 e) 220) (edge-d1 e) hub-y 240.0)              ; approach to the Crossroads
                 (apron (- (edge-d1 e) 60) (edge-d1 e))
                 (let ((s (- (edge-d1 e) 150.0)))                                ; gas station before the Crossroads
                   (push s *spoke-stations*)
                   (push (make-pad :d0 (- s 20) :d1 (+ s 20) :ramp 14.0 :x0 4.0 :x1 27.0 :m0 4.0 :m1 14.0 :y -.37) *pads*)))
        (dolist (o *outs*)
          (flat (edge-d0 o) (+ (edge-d0 o) 160) hub-y 240.0)
          (apron (edge-d0 o) (+ (edge-d0 o) 60))
          (let ((d1 (edge-d1 o)) (next (edge-next o)))                          ; out: the same, merging with the ring road
            (flat (- d1 320) d1 (lambda (d) (road-y (- next (- d1 d)))) 200.0)))))
    (clrhash *rows*) (setf *row* nil)))

(defun build-hub ()
  "Build the Crossroads scenery and return its node.
It is a disc of asphalt, a post with signs for the three outs, and street lamps."
  (let* ((e (cdr (first *exits*))) (y (road-y (- (edge-d1 e) 1.0)))
         (g (group *world* (first *hub*) (+ y .09) (second *hub*))))
    (model g "hub")
    (loop for o in *outs* for k from 0
          do (let* ((tw (find-if (lambda (tw) (> (town-start tw) (edge-next o))) *towns*))
                    (th (aref (edge-ths o) 0)) (arm (group g 0 (+ 6.0 (* k .9)) 0)))
               (node-3d-set-rotation arm (v3 0 (- th) 0))
               (model arm "hub-arm")
               (sign arm (if tw (town-title tw) "КОЛЬЦО") .08 0 -2.6 :h .42 :rgb "#f0ece0" :yaw (/ pi 2))
               (sign arm (if tw (town-title tw) "КОЛЬЦО") -.08 0 -2.6 :h .42 :rgb "#f0ece0" :yaw (- (/ pi 2)))))
    g))
