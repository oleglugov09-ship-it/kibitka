;;;; The landscape as a function of the path coordinate: three independent layers along the road
;;;; (relief, water, vegetation), each with its own pieces of random length that blend smoothly
;;;; at the joints. The road itself lies in the relief: it winds (the heading depends on the relief,
;;;; a serpentine in the mountains) and goes up and down the slopes of the big height field.
;;;; The height of a point (x is the sideways offset from the road, d the path coordinate along it)
;;;; is computed from here, so land, water and trees always agree. Everything is ordinary
;;;; parameters and functions: they can be redefined from the REPL, and (live:regen) rebuilds
;;;; the land with the new ones.

(in-package #:kiosk-trip)

(defparameter *trip-seed* 1 "The trip seed: the layer pieces are derived from it.")

;;; Climate is state; climate.lisp changes and describes it
(defvar *season* :summer "The season: :spring, :summer, :autumn or :winter.")
(defvar *weather* :clear "The weather: :clear, :cloudy, :rain, :fog or :snow.")
(defvar *wx* (list :cloud 0.0 :rain 0.0 :snow 0.0 :fog 0.0) "The weather right now: fractions 0..1 that change smoothly.")
(defun wx (k) (getf *wx* k))

;;; The noise is in double-float with declared types: ECL compiles it to C without
;;; boxing numbers, so the land builds several times faster.

(declaim (inline %hash2))
(defun %hash2 (x y)
  (declare (optimize (speed 3) (safety 0)) (double-float x y))
  (let ((h (* (sin (+ (* x 127.1d0) (* y 311.7d0))) 43758.5453d0)))
    (- h (ffloor h))))

(defun hash2 (x y)
  "Return a pseudo-random number in 0..1 from the pair of numbers X and Y of any type."
  (%hash2 (float x 1d0) (float y 1d0)))

(declaim (inline vnoise))
(defun vnoise (x y)
  (declare (optimize (speed 3) (safety 0)) (double-float x y))
  (let* ((xi (ffloor x)) (yi (ffloor y)) (xf (- x xi)) (yf (- y yi))
         (u (* xf xf (- 3d0 (* 2d0 xf)))) (v (* yf yf (- 3d0 (* 2d0 yf))))
         (a (%hash2 xi yi)) (b (%hash2 (+ xi 1d0) yi)) (c (%hash2 xi (+ yi 1d0))) (d (%hash2 (+ xi 1d0) (+ yi 1d0))))
    (+ a (* (- b a) u) (* (- c a) v) (* (+ a (- b) (- c) d) u v))))

(defun fbm (x y)
  "Return three octaves of value noise at X, Y, in 0..1."
  (let ((x (float x 1d0)) (y (float y 1d0)))
    (declare (optimize (speed 3) (safety 0)) (double-float x y))
    (float (+ (* .55d0 (vnoise x y)) (* .3d0 (vnoise (* x 2.1d0) (* y 2.1d0))) (* .15d0 (vnoise (* x 4.3d0) (* y 4.3d0))))
           1.0)))

(defun sstep (a b x)
  (let ((tt (max 0 (min 1 (/ (- x a) (- b a)))))) (* tt tt (- 3 (* 2 tt)))))

;;; Layers

(defparameter *layers*
  '((:relief :kinds (:plain :plain :plain :hills :hills :hills :mountains :mountains :mountains :canyon) :len (260 760))   ; canyons are rare
    (:water :kinds (:none :river :river :lake) :len (200 620))
    (:veg :kinds (:pine :meadow :autumn :birch :rocky :steppe :marsh) :len (180 520)))
  "The layers: for each, its kinds (a repeated kind is more frequent) and piece lengths in meters.")

(defparameter *names*
  '(:plain "равнина" :hills "холмы" :mountains "горы" :canyon "каньон" :serpentine "серпантин" :none "" :river "река" :lake "озеро"
    :pine "хвойный лес" :meadow "цветущий луг" :autumn "осенняя роща" :birch "березняк" :rocky "каменистая пустошь"
    :steppe "степь" :marsh "болото"))

(defparameter *amp* '(:plain .3 :hills .9 :mountains 1.7 :canyon .7 :serpentine 1.6) "The relief strength by kind.")
(defvar *route* '() "The route sheet: entries (from to layer kind).
On such a stretch the layer is not random but prescribed (serpentine, marsh, gorge).
Assembled by vista.lisp.")
(defvar *decks* '() "The decks: entries (from to) where the road runs on a deck above the land, a bridge over a chasm.")
(defvar *bare* '() "The bare ranges: entries (from to) where nothing grows or stands by the road, a bridge or a tunnel.")
(defun deck-at (d) (some (lambda (z) (<= (first z) d (second z))) *decks*))
(defun bare-at (d) (or (some (lambda (z) (<= (first z) d (second z))) *bare*) (off-road-p d)))   ; and past the end of the road too
(defparameter *blend* 90.0 "The length of the blend between pieces, in meters.")

(defvar *segments* (make-hash-table) "The map from a layer to its vector of pieces (start . kind), which grows along the path.")

(defvar *road* nil "The road's x every *ROAD-STEP* meters of path from *ROAD-START*.")

(defvar *row* nil "The last computed row (see ROW-AT).")

(defun reset-segments () (clrhash *segments*) (setf *road* nil *row* nil))

(defun segments (layer upto)
  (let* ((spec (cdr (assoc layer *layers*))) (kinds (getf spec :kinds)) (len (getf spec :len))
         (salt (position layer '(:relief :water :veg)))
         (arr (or (gethash layer *segments*)
                  (setf (gethash layer *segments*)
                        (make-array 1 :adjustable t :fill-pointer t
                                      :initial-element (cons 0.0 (nth (floor (* (hash2 *trip-seed* salt) (length kinds))) kinds)))))))
    (loop while (< (car (aref arr (1- (length arr)))) (+ upto 1000))
          do (let* ((i (length arr)) (prev (aref arr (1- i)))
                    (h1 (hash2 (* i 7.3) (+ *trip-seed* (* salt 13))))
                    (h2 (hash2 (+ (* i 3.1) 5) (+ (* *trip-seed* .7) salt)))
                    (rest (remove (cdr prev) kinds))             ; do not repeat the previous kind
                    (k (nth (floor (* h1 (length rest))) rest)))
               (vector-push-extend (cons (+ (car prev) (first len) (* h2 (- (second len) (first len)))) k) arr)))
    arr))

(defun layer-weights (layer d)
  "Return the weights of the kinds of LAYER at path coordinate D as a plist (kind weight ...).
Stretches of the route sheet override the random pieces."
  (let ((base (base-layer-weights layer d)))
    (dolist (r *route* base)
      (destructuring-bind (d0 d1 l kind) r
        (when (and (eq l layer) (< (- d0 *blend*) d (+ d1 *blend*)))
          (let ((w (* (sstep (- d0 *blend*) d0 d) (- 1 (sstep d1 (+ d1 *blend*) d)))))
            (return (if (>= w 1)
                        (list kind 1)
                        (list* kind (+ w (* (- 1 w) (or (getf base kind) 0)))
                               (loop for (k v) on base by #'cddr unless (eq k kind) append (list k (* v (- 1 w)))))))))))))

(defun base-layer-weights (layer d)
  "Return the weights of the kinds of LAYER at path coordinate D from its random pieces alone."
  (let* ((arr (segments layer d)) (i (1- (length arr))))
    (loop while (and (> i 0) (> (car (aref arr i)) d)) do (decf i))
    (destructuring-bind (start . kind) (aref arr i)
      (let ((tt (if (> i 0) (sstep 0 *blend* (- d start)) 1)))
        (if (< tt 1)
            (list kind tt (cdr (aref arr (1- i))) (- 1 tt))
            (list kind 1))))))

(defun weight (layer kind d) (or (getf (layer-weights layer d) kind) 0))

(defun dominant (layer d)
  (let ((w (layer-weights layer d)))
    (if (and (cddr w) (> (fourth w) (second w))) (third w) (first w))))

(defun scene-name (d)
  "Return the name of the scenery at path coordinate D.
The dominant vegetation, relief and water names are joined with ' · ', three names at most."
  (format nil "~{~a~^ · ~}"
          (remove "" (mapcar (lambda (l) (getf *names* (dominant l d))) '(:veg :relief :water)) :test #'string=)))

;;; The road: heading and height

(defparameter *bends*
  '(:plain (.12 .005 .04 .017) :hills (.32 .014 .08 .031) :mountains (.6 .045 .1 .09) :canyon (.28 .02 .06 .05)
    :serpentine (.98 .05 .12 .11))
  "The road bends by relief: (angle frequency angle2 frequency2).
The heading is theta = angle*sin(frequency*d) + angle2*sin(frequency2*d).")
(defparameter *rise* '(:plain 8 :hills 45 :mountains 90 :canyon 20 :serpentine 110) "The height range of the big height field by relief, in meters.")
(defparameter *big-scale* .0025 "The frequency of the big field: the smaller it is, the longer the climbs.")
(defparameter *road-follow* .5 "The degree to which the road follows the big field: the rest is embankments and cuttings.")
(defparameter *road-step* 2.0)
(defparameter *road-start* -300.0)

(defun heading (d)
  "Return the road heading at path coordinate D in radians.
0 is straight ahead (-z), plus is to the right (x+)."
  (let ((ph (* *trip-seed* 1.7)))
    (loop for (k w) on (layer-weights :relief d) by #'cddr
          sum (destructuring-bind (a f a2 f2) (getf *bends* k)
                (* w (+ (* a (sin (+ (* f d) ph))) (* a2 (sin (+ (* f2 d) (* 2 ph))))))))))

(defun road-x (d)
  "Return the road's sideways position at path coordinate D: the integral of the heading's tangent."
  (unless *road* (setf *road* (make-array 1 :adjustable t :fill-pointer t :initial-element 0.0)))
  (let ((n (ceiling (- d *road-start*) *road-step*)))
    (loop while (<= (length *road*) (1+ n))
          do (let* ((i (length *road*)) (mid (+ *road-start* (* (- i .5) *road-step*))))
               (vector-push-extend (+ (aref *road* (1- i)) (* *road-step* (tan (heading mid)))) *road*)))
    (let* ((u (/ (- d *road-start*) *road-step*)) (i (max 0 (floor u))) (f (- u i)))
      (+ (aref *road* i) (* f (- (aref *road* (1+ i)) (aref *road* i)))))))

(defun relief-sum (plist d)
  "Return the relief property from PLIST (kind -> number) blended by the kind weights at D."
  (loop for (k w) on (layer-weights :relief d) by #'cddr sum (* w (getf plist k))))

;;; A row: everything that depends only on the path coordinate D is computed once per grid row,
;;; not per point (the land is built in rows across the road).

(defstruct (row (:constructor %make-row))
  d seed amp canyon lake river rx ry rise river-x meadow dry
  pads)                        ; pads at this D: lists (weight x0 x1 margin0 margin1 height)

;;; Pads. The world is permanent, so everything that stands on the land by plan rather than
;;; 'wherever' (towns, gas stations, bus stops, landmarks, the railway) tells the land in advance:
;;; 'it is flat here'. The land is pulled up to the pad instead of the building hanging over a
;;; slope. The list is assembled by vista.lisp.

(defstruct pad
  d0 d1 (ramp 30.0)            ; along the road: from, to and the length of the smooth entry
  x0 x1 (m0 12.0) (m1 12.0)    ; across: from, to (m from the axis, signed) and the smooth margin on each side
  (y 0.0)                      ; the pad's height above the road
  shift)                       ; a function D -> sideways shift of the pad (the railway leaves the highway) or NIL

(defvar *pads* '() "The pads of the world (PAD structures).")
(defvar *levels* '() "The level stretches of the road: entries (from to height approach).
A house stands level, so the road beside it does not climb either: in a town it is the whole
street; at a gas station, a diner or a landmark it runs along the pad.")

(defun pads-at (d)
  (loop for p in *pads*
        when (< (- (pad-d0 p) (pad-ramp p)) d (+ (pad-d1 p) (pad-ramp p)))
          collect (let ((sh (if (pad-shift p) (funcall (pad-shift p) d) 0.0)))
                    (list (* (sstep (- (pad-d0 p) (pad-ramp p)) (pad-d0 p) d)
                             (- 1 (sstep (pad-d1 p) (+ (pad-d1 p) (pad-ramp p)) d)))
                          (+ (pad-x0 p) sh) (+ (pad-x1 p) sh) (pad-m0 p) (pad-m1 p) (pad-y p)))))

(defun raw-road-y (d)
  "Return the road height at path coordinate D from the big field, without levelling in towns.
The value is the mean over 120 m of path: in the mountains the field itself is too rugged, and
a road laid on it would have grades near 60 %, which is not how roads are built."
  (let ((rise (relief-sum *rise* d)))
    (* *road-follow* (/ (loop for o in '(-60.0 -30.0 0.0 30.0 60.0)
                              sum (big-height-in (road-x (+ d o)) (%make-row :d (+ d o) :seed *trip-seed* :rise rise)))
                        5))))

(defun level-road-y (d raw)
  "Return the road height at path coordinate D, given the height by relief RAW.
On a level stretch it is that stretch's height. Between two stretches the road goes from one to
the other smoothly over the whole gap (not as a step next to one of them); when a stretch is
farther away than its approach, the road returns to RAW."
  (let ((before nil) (after nil))                            ; nearest stretch behind and nearest ahead
    (dolist (l *levels*)
      (destructuring-bind (d0 d1 y ramp) l
        (declare (ignore ramp))
        (cond ((<= d0 d d1) (return-from level-road-y (if (functionp y) (funcall y d) y)))
              ((< d1 d) (when (or (null before) (> d1 (second before))) (setf before l)))
              (t (when (or (null after) (< d0 (first after))) (setf after l))))))
    (flet ((toward (l gap)                               ; a stretch height may be a function of the path: at a fork the road follows the ring road
             (if (and l (< gap (fourth l)))
                 (let ((y (if (functionp (third l)) (funcall (third l) (if (eq l before) (second l) (first l))) (third l))))
                   (+ y (* (sstep 0 (fourth l) gap) (- raw y))))
                 raw)))
      (let* ((ga (if before (- d (second before)) 1e9)) (gb (if after (- (first after) d) 1e9))
             (ya (toward before ga)) (yb (toward after gb)))
        (cond ((null before) yb) ((null after) ya)
              (t (+ ya (* (sstep 0 1 (/ ga (+ ga gb))) (- yb ya)))))))))

(defun big-height-in (x row)
  "Return the big height field at point X of row ROW.
These are the hills and mountains whose slopes the road follows."
  (* (row-rise row) 2
     (- (fbm (+ (* x *big-scale*) 11) (+ (* (row-d row) *big-scale*) (* (row-seed row) 3.1))) .5)))

(defun row-at (d)
  (let ((r *row*))
    (if (and r (eql (row-d r) d) (eql (row-seed r) *trip-seed*))
        r
        (let ((r (%make-row :d d :seed *trip-seed*
                            :amp (relief-sum *amp* d) :rise (relief-sum *rise* d) :rx (road-x d)
                            :canyon (weight :relief :canyon d)
                            :lake (weight :water :lake d) :river (weight :water :river d) :river-x (river-x d)
                            :meadow (weight :veg :meadow d)
                            :dry (min 1.0 (+ (weight :veg :autumn d) (* .6 (weight :veg :rocky d)) (weight :veg :steppe d))))))
          (setf (row-ry r) (level-road-y d (raw-road-y d))
                (row-pads r) (pads-at d)
                *row* r)))))

(defun road-y (d)
  "Return the road height at path coordinate D, interpolated between rows computed two meters apart."
  (let* ((k (floor d 2)) (f (/ (- d (* 2 k)) 2.0)) (a (row-ry (row-near (* 2.0 k)))))
    (+ a (* f (- (row-ry (row-near (* 2.0 (1+ k)))) a)))))

(defun road-frame (d)
  "Return the road's frame at path coordinate D as two values.
The first is the heading on the map (radians: 0 is toward -z, plus is right), the second the
grade (radians, plus is uphill)."
  (values (nth-value 2 (road-point d))
          (atan (- (road-y (+ d 3)) (road-y (- d 3))) 6.0)))

;;; Height

(defun river-x (d)
  "Return the river's sideways offset from the road at path coordinate D.
The river flows for a long time on one side, 16-34 m away, and rarely and sharply crosses
under the road: there is a bridge there."
  (* (tanh (* 20 (sin (+ (* d .0045) *trip-seed*)))) (+ 25 (* 9 (sin (+ (* d .019) 1.3))))))
(defparameter *river-y* -1.6)
(defparameter *water-y* -.55)

(defun height-at (x d)
  "Return the height above the road at the point X sideways from it at path coordinate D.
The value comes from the real land of the map (LAND-HEIGHT)."
  (multiple-value-bind (px pz th) (road-point d)
    (- (land-height (+ px (* x (cos th))) (+ pz (* x (sin th)))) (road-y d))))

(defun height-in (x row)
  (let* ((d (row-d row)) (ax (abs x)) (amp (row-amp row))
         (canyon (row-canyon row)) (lake (row-lake row)) (river (row-river row))
         (h (* amp (+ (- (* 16 (fbm (+ (* x .018) 40) (* d .018))) 4) (* 3 (fbm (* x .07) (* d .07)))))))
    (incf h (* (min 120 (max 0 (- ax 110))) .3 amp))                         ; mountains on the horizon, not by the road and not as a wall
    (when (> ax 320) (incf h (far-rise x row)))                               ; far ridges: they are seen in the far background
    (when (> canyon 0) (incf h (* canyon (sstep 9 15 ax) (+ 12 (* 10 (fbm (* x .1) (* d .05)))))))
    (setf h (* h (sstep 4 26 ax) (+ .3 (* .7 (sstep 30 110 ax)))))           ; flat and gentle by the road, mountains grow in the distance
    (when (> ax 10)                                                           ; the slope of the big field: up or down sideways
      (let ((rel (- (big-height-in (+ (row-rx row) x) row) (row-ry row))))
        (incf h (* (if (> rel 0) (* .5 rel) rel) (sstep 10 90 ax) (- 1 (* .8 (max lake river)))))))   ; gentler upward
    (when (and (> lake 0) (> x 0))                                            ; the lake is on the window side
      (let ((lb (* (sstep 10 22 x) (- 1 (sstep 75 95 x)) lake)))
        (setf h (- (* h (- 1 lb)) (* 2.2 lb)))))
    (when (> river 0)
      (let ((rd (abs (- x (row-river-x row)))))
        (incf h (* (- *river-y* h) (sstep 7 2.5 rd) river))))
    (dolist (p (row-pads row))                                                ; pads: the land is flat here
      (destructuring-bind (wd x0 x1 m0 m1 y) p
        (when (< (- x0 m0) x (+ x1 m1))
          (let ((w (* wd (sstep (- x0 m0) x0 x) (- 1 (sstep x1 (+ x1 m1) x)))))
            (incf h (* w (- y h)))))))
    (float h 1.0)))

(defun far-rise (x row)
  "Return the height of the far ridges at point X of row ROW, beyond 320 m from the road.
They are tall in the mountains and gentle swells on the plain."
  (let ((ax (abs x)))
    (* (sstep 320 900 ax) (+ .25 (* .45 (row-amp row)))
       (+ 20 (* 170 (expt (fbm (+ (* x .0032) 70) (* (row-d row) .0028)) 2))))))
