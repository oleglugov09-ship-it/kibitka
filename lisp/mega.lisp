;;;; The way behind the tunnel: another place than the ring, laid out around the wagon as it drives and without end.
;;;; This file is what every such place shares - the cells, the path through them, what moves in them, how one gets
;;;; in. What the places are is elsewhere: the megastructure in mega-zones.lisp and mega-monuments.lisp, the summer
;;;; in leto.lisp.

(in-package #:kiosk-trip)

;;; The tunnel does not come out on the other side of its hill. Some way in, the old world is no longer shown at
;;; all - no land, no towns, no cars, nobody - and what there is instead is built around the wagon itself and
;;; goes by it. On the ring the wagon stands still from then on (STEP-WORLD), and the way driven inside, *MEGA-S*,
;;; is counted apart. The place is made of zones some two and a half kilometres long, one after another; a zone is
;;; built cell by cell (*MEGA-CELL* metres) ahead of the wagon and taken away behind it, and what a cell holds
;;; depends on its number alone, so everybody in the wagon sees the same.

(defparameter *mega-cell* 42.0 "The length of one cell, in metres.")
(defparameter *mega-zone* 64 "The number of cells in a zone: some five minutes of driving.")
(defparameter *mega-first* 24 "The number of cells the way runs straight and level for after the door: the threshold lies along them.")
(defvar *mega-parts* nil "The parts of the place that is shown, a plist: :cells (the group the cells hang in), :turn and :tilt (what
turns them against the wagon), :entry (the tunnel we came by), :at (the cell all is built around), and what its world added.")
(defvar *mega-cells* (make-hash-table) "The cells built right now: a table from a cell's number to its node.")

;;; The worlds. Which place lies behind the tunnel is *MEGA-WORLD*; what is known of each is in *WORLDS*, a plist
;;; of plists, and a world adds itself with ADD-WORLD. :FLAG is the word in the page address that starts the game
;;; at the tunnel with this world behind it. The rest are functions, any of which may be missing:
;;;   :KIND (z)            the kind of zone number Z, the first being 0;
;;;   :DROP (j)            how far cell J goes down without tilting, in metres;
;;;   :PARTS (g turn entry) build what does not go by - under G in the wagon's axes, under TURN in the axes of the
;;;                        place, under ENTRY round the tunnel's mouth - and return a plist to keep in *MEGA-PARTS*;
;;;   :STEP (parts)        every frame, move and show those parts;
;;;   :AIR (s)             what the air of the place is S metres in, as a plist for SET-AIR.
;;; And two things that are not functions: :RULES, what goes on in the world as it does on the ring (place.lisp);
;;; :OFFMAP, the card shown there instead of the map, a format string that takes the kilometres driven.

(defvar *mega-world* :mega "Which place lies behind the tunnel: a key of *WORLDS*.")
(defvar *worlds* '() "What is known of each place behind the tunnel: see above.")
(defun add-world (key &rest plist)
  "Make the world KEY known, with what PLIST says of it."
  (setf (getf *worlds* key) plist (getf *place-rules* key) (getf plist :rules))
  key)
(defun world (key &optional default) "Return what is known under KEY of the world behind the tunnel, or DEFAULT." (getf (getf *worlds* *mega-world*) key default))
(defun world-call (key &rest args) "Call the function the world behind the tunnel has under KEY, if it has one." (let ((f (world key))) (and f (apply f args))))

;;; The zones. What is known of each kind of zone is in *MEGA-ZONES*; a world adds its own with ADD-ZONES. :NAME is
;;; what the corner of the screen says, :BUILD the function that builds a cell of it. :TURN is how many degrees a
;;; cell may turn and :STEEP how steep it may be; :WAY fixes the side it turns to, :DOWN makes it go down only.
;;; :CELLS is how many cells long it is (*MEGA-ZONE* unless said), :BARE means no bus stop is put in it. The rest
;;; is the world's own. What a zone is built of is code and stands here; its numbers - how long, how bent, how
;;; lit - are content, and the base mod sets them with TUNE-ZONES.

(defvar *mega-zones* '() "The kinds of zones and what is known of each: see above.")
(defun add-zones (plist) "Make the kinds of zones in PLIST known." (loop for (kind spec) on plist by #'cddr do (setf (getf *mega-zones* kind) (copy-list spec))))
(defun tune-zones (plist)
  "Set what PLIST says of zones already known, leaving the rest of each as it is: PLIST goes from a kind of zone to a
plist of what to set. The numbers of the zones are set this way by the base mod (project/mods/base/worlds.lisp)."
  (loop for (kind spec) on plist by #'cddr
        do (loop for (key value) on spec by #'cddr do (setf (getf (getf *mega-zones* kind) key) value))))
(defun zone (kind key &optional default) "Return what is known under KEY of the zones of KIND, or DEFAULT." (getf (getf *mega-zones* kind) key default))

(defvar *zone-ends* (make-array 0 :adjustable t :fill-pointer t) "The cell each zone known so far ends before, by the zone's number.")

(defun way-zone (i)
  "Return the number of the zone cell I lies in, the cell that zone starts at, and how many cells long it is. Zones
are as long as their kinds say, so where one ends depends on all before it; that is worked out once and kept."
  (let ((i (max i 0)) (ends *zone-ends*))
    (loop while (or (zerop (length ends)) (<= (aref ends (1- (length ends))) i))
          do (let ((z (length ends)))
               (vector-push-extend (+ (if (zerop z) 0 (aref ends (1- z))) (zone (world-call :kind z) :cells *mega-zone*)) ends)))
    (let* ((z (position-if (lambda (end) (> end i)) ends)) (start (if (zerop z) 0 (aref ends (1- z)))))
      (values z start (- (aref ends z) start)))))

(defun mega-kind (i) "Return the kind of the zone that cell I belongs to." (world-call :kind (way-zone i)))
(defun zone-cell (i) "Return which cell of its zone cell I is, the first being 0." (- (max i 0) (nth-value 1 (way-zone i))))

(defun mega-name () "Return what the zone we are in is called." (zone (mega-kind (floor *mega-s* *mega-cell*)) :name))

;;; The way is not straight. Every cell turns a little and climbs or drops a little
;;; against the one before, by its number alone; how much depends on its zone. A cell's place is the sum of all the turns before it, kept in *MEGA-POSES* as it is first asked for.

(defvar *mega-poses* (make-hash-table) "A table from a cell's number to where it starts and how it lies: (x y z yaw pitch).")
(defvar *mega-pose-top* 0 "The highest cell whose place is known.")

(defun mega-bend (i)
  "Return how cell I turns against the one before it, in radians (left is positive), and how steep it is (up is positive)."
  (if (< i *mega-first*)
      (values 0.0 0.0)
      (let* ((kind (mega-kind i)) (b (floor i 8)) (way (vr b 21.0)) (grade (vr b 33.0)))
        (values (* (/ pi 180) (zone kind :turn 0.0)
                   (or (zone kind :way) (cond ((< way .3) 0) ((< way .55) 1) ((< way .8) -1) ((< way .9) 2) (t -2))))
                (atan (* (zone kind :steep 0.0) (cond ((zone kind :down) -1) ((< grade .4) 0) ((< grade .7) -1) (t 1))))))))

(defun mega-drop (j) "Return how far cell J goes down without tilting, in metres." (or (world-call :drop j) 0.0))

(defun mega-height (s)
  "Return the height of the way at S metres along it."
  (let* ((c *mega-cell*) (i (floor s c)) (f (- (/ s c) i)) (a (second (mega-pose i))))
    (+ a (* f (- (second (mega-pose (1+ i))) a)))))

(defun mega-pose (i)
  "Return where cell I starts and how it lies, as (x y z yaw pitch). Cells before the first run straight and level."
  (cond ((<= i 0) (list 0.0 0.0 (- (* i *mega-cell*)) 0.0 0.0))
        ((gethash i *mega-poses*))
        (t (when (zerop (hash-table-count *mega-poses*)) (setf *mega-pose-top* 0 (gethash 0 *mega-poses*) (list 0.0 0.0 0.0 0.0 0.0)))
           (loop for j from *mega-pose-top* below i                          ; walk on from the last one known
                 do (destructuring-bind (x y z yaw pitch) (gethash j *mega-poses*)
                      (multiple-value-bind (turn tilt) (mega-bend (1+ j))
                        (let ((c *mega-cell*))
                          (setf (gethash (1+ j) *mega-poses*)
                                (list (- x (* c (cos pitch) (sin yaw))) (- (+ y (* c (sin pitch))) (mega-drop j)) (- z (* c (cos pitch) (cos yaw)))
                                      (+ yaw turn) tilt))))))
           (setf *mega-pose-top* (max *mega-pose-top* i))
           (gethash i *mega-poses*))))

;;; A place may be at work, and the way a part of the work: a conveyor belt, the inside of a pipe, a shelf
;;; of an archive, the line a pen is writing ahead of us, containers a crane sets down one by one so that the road
;;; does not end. What moves is kept by cell in *MEGA-LIVES* as (kind node . numbers) and stepped every frame:
;;; :ink - a stretch of the line, there only once the pen has passed; :drop - a thing that hangs high far ahead,
;;; is lowered into place as we come and is lifted away behind; :press - a thing going up and down.

(defvar *mega-lives* (make-hash-table) "A table from a cell's number to what moves in it: a list of (kind node . numbers).")
(defvar *mega-live-now* '() "What moves in the cell being built right now.")
(defparameter *mega-pen* 240.0 "How far ahead of us the pen writes, in metres.")

(defun live (kind node &rest numbers)
  "Record that NODE of the cell being built moves in the manner KIND; return NODE. What moves is a node of its own,
not a part of the cell's batch (shapes.lisp)."
  (let ((node (solo node)))
    (unbatch node)
    (push (list* kind node numbers) *mega-live-now*)
    node))

(defun step-mega-lives ()
  (let ((c *mega-cell*))
    (maphash (lambda (i lives)
               (let ((ahead (- (* i c) *mega-s*)))
                 (dolist (l lives)
                   (destructuring-bind (kind node . numbers) l
                     (ecase kind
                       (:ink (object-set node "visible" (< ahead *mega-pen*)))
                       (:drop (destructuring-bind (x y z) numbers                       ; lowered ahead of us, lifted behind
                                (node-3d-set-position node (v3 x (+ y (cond ((> ahead 130.0) (* 1.1 (- ahead 130.0))) ((< ahead -90.0) (* 1.1 (- -90.0 ahead))) (t 0.0))) z))))
                       (:train (destructuring-bind (x y z way period phase) numbers      ; comes in, stands at the platform, goes
                                 (let* ((u (mod (+ phase (/ *time* period)) 1.0))
                                        (off (cond ((< u .3) (* 2600.0 (expt (- 1.0 (/ u .3)) 2))) ((< u .7) 0.0) (t (- (* 2600.0 (expt (/ (- u .7) .3) 2)))))))
                                   (node-3d-set-position node (v3 x y (+ z (* way off)))))))
                       (:swing (destructuring-bind (amp period phase) numbers            ; a boom going from side to side
                                 (node-3d-set-rotation node (v3 0 (* amp (sin (+ phase (/ (* 6.283 *time*) period)))) 0))))
                       (:spin (destructuring-bind (yaw period) numbers                   ; a wheel turning
                                (node-3d-set-rotation node (v3 0 yaw (/ (* 6.283 *time*) period)))))
                       (:press (destructuring-bind (x z y0 y1 period phase) numbers
                                 (node-3d-set-position node (v3 x (+ y0 (* (- y1 y0) (+ .5 (* .5 (sin (+ phase (/ (* 6.283 *time*) period))))))) z)))))))))
             *mega-lives*)))

;;; Each kind of zone builds its cell in a function of its own (a single function for all of them grew past what
;;; the browser build could take). Inside, G is the cell's group, I its number, C its length, MID the z of its
;;; middle, AT its place in a run of sixteen, and (R k) the cell's own dice: K names the die, by a number or,
;;; better, by a word - (r :lean). Where in its zone the cell is, (ZONE-CELL i) says.

(defun dice-key (name)
  "Return the number that stands for the die NAME, a number itself or a symbol: a die named by a word needs no
number picked by hand, and cannot fall together with another by mistake."
  (if (realp name)
      (float name)
      (let ((h 7)) (loop for ch across (symbol-name name) do (setf h (mod (+ (* h 31) (char-code ch)) 99991))) (+ 5000.0 h))))

(defmacro defzone (name &body body)
  `(defun ,name (g i c mid at)
     (declare (ignorable g i c mid at))
     (flet ((r (k) (vr (+ i 1000.5) (dice-key k))))
       (declare (ignorable (function r)))
       ,@body)))

;;; A cell is built by its zone; then, if the zone says so, the hall around it (the megastructure's); then, every
;;; sixteenth cell, one of our own bus stops under a lamp.

(defun mega-slab (g w rgb &optional (x -1.8))
  "Lay a slab W metres wide along the cell G, its top just under the wheels."
  (shape g :box (list w 1.0 *mega-cell*) rgb x -.55 (- (/ *mega-cell* 2)) :shadow nil))

(defun mega-stop (g i)
  "Put one of our bus stops by the way in the cell G, under a lamp."
  (let ((stop (group g *stop-x* 0.0 -20.0)))
    (shape g :box '(9.0 .5 14.0) "#6f6e69" 8.6 -.3 -20.0 :shadow nil)
    (build-stop stop (* 7.0 (floor i 16)))
    (shape g :box '(.14 5.6 .14) "#3a3c40" 5.0 2.8 -15.0 :shadow nil)
    (shape g :box '(.6 .2 .4) "#ffe2a8" 5.3 5.6 -15.0 :emit "#ffcf8a" :energy 3 :shadow nil)
    (light g "#ffcf8a" 2.2 26.0 5.6 5.0 -15.0)))

(defun build-mega-cell (cells i)
  "Build cell I under CELLS and return its group. Inside it x is to the right, y up, and the cell runs from z = 0
back to z = -*MEGA-CELL*."
  (let* ((c *mega-cell*) (pose (mega-pose i)) (g (group cells (first pose) (second pose) (third pose)))
         (kind (mega-kind i)) (at (mod i 16)))
    (node-3d-set-rotation g (v3 (fifth pose) (fourth pose) 0))
    (with-batch (g)                                                      ; a cell is hundreds of shapes and a few dozen draws
      (funcall (zone kind :build) g i c (- (/ c 2)) at)
      (when (zone kind :hall) (mega-hall g i (zone kind :hall)))
      (when (and (= at 8) (not (zone kind :bare))) (mega-stop g i)))
    g))

(defun build-mega ()
  "Build what holds the place around the wagon, in the wagon's own axes (x to the right, z backwards), and return its
group. The cells themselves are built by STEP-MEGA-CELLS as the wagon goes."
  (let* ((g (group *cart*)) (tilt (group g)) (turn (group tilt)) (cells (group turn)) (entry (group g)))
    (let ((threshold (getf (getf *doors* *mega-in*) :threshold))) (when threshold (funcall threshold entry)))   ; the door we came by, left behind
    (clrhash *mega-cells*) (clrhash *mega-poses*) (clrhash *mega-lives*) (setf (fill-pointer *zone-ends*) 0)
    (setf *mega-parts* (list* :cells cells :turn turn :tilt tilt :entry entry :at nil (world-call :parts g turn entry)))
    g))

(defun build-missing-cell (cells i)
  "Build cell I under CELLS unless it stands already; return true if it was built."
  (unless (gethash i *mega-cells*)
    (let ((*mega-live-now* '()))
      (setf (gethash i *mega-cells*) (build-mega-cell cells i) (gethash i *mega-lives*) *mega-live-now*))
    t))

(defun step-mega-cells ()
  "Keep the cells around the wagon built: some way behind, far ahead, no more than one new one a frame."
  (let* ((c *mega-cell*) (cells (getf *mega-parts* :cells)) (here (floor *mega-s* c))
         (lo (floor (- *mega-s* 170.0) c)) (hi (floor (+ *mega-s* 620.0) c)) (fresh 0))
    ;; where we are on the way, and which way we face there: the place is turned and moved the other way
    (let* ((f (- (/ *mega-s* c) here)) (a (mega-pose here)) (b (mega-pose (1+ here))))
      (flet ((mix (k) (+ (nth k a) (* f (- (nth k b) (nth k a))))))
        (node-3d-set-rotation (getf *mega-parts* :tilt) (v3 (- (mix 4)) 0 0))
        (node-3d-set-rotation (getf *mega-parts* :turn) (v3 0 (- (mix 3)) 0))
        (setf *sun-turn* (- (mix 3)))                                    ; the sun stands still over the place, not over the wagon
        (node-3d-set-position cells (v3 (- (mix 0)) (- (mix 1)) (- (mix 2))))))
    (world-call :step *mega-parts*)
    (step-mega-lives)
    (unless (eql (getf *mega-parts* :at) here)                           ; nothing to do while we stay in one cell and all is built
      (loop for i being the hash-keys of *mega-cells* using (hash-value node)
            do (unless (<= (1- lo) i (1+ hi)) (node-queue-free node) (remhash i *mega-cells*) (remhash i *mega-lives*)))
      (loop for i from here to hi while (< fresh 1) do (when (build-missing-cell cells i) (incf fresh)))       ; the nearest first
      (loop for i from (1- here) downto lo while (< fresh 1) do (when (build-missing-cell cells i) (incf fresh)))
      (when (zerop fresh) (setf (getf *mega-parts* :at) here)))))

;;; The doors. One leaves the ring through a door and comes back through the same one. What is known of each is
;;; in *DOORS*, a plist of plists, and a door adds itself with ADD-DOOR:
;;;   :IN ()         true when the wagon has just gone through, :OUT () when it has come back out;
;;;   :WORLD         the world behind it, where the door decides (else whichever *MEGA-WORLD* says);
;;;   :GLOOM ()      how dark it is on the approach, on the ring's side, from 0 to 1;
;;;   :THRESHOLD (entry)  build under ENTRY what of the door is still seen from the other side.
;;; A guest does not go through doors of its own: where the wagon is, the driver says (the common thing :PLACE).

(defvar *doors* '() "What is known of each door: see above.")
(defun add-door (key &rest plist) "Make the door KEY known, with what PLIST says of it." (setf (getf *doors* key) plist) key)
(defvar *mega-in* nil "The door we went through, while we are behind it; NIL on the ring.")
(defun inside-mega-p () "Return true while we are behind a door." (and *mega-in* t))

(defun step-doors ()
  "See whether the wagon has gone through a door or come back, and say where it is."
  (unless (net-guest-p)
    (if *mega-in*
        (when (funcall (getf (getf *doors* *mega-in*) :out)) (setf *mega-in* nil))
        (loop for (key door) on *doors* by #'cddr
              when (funcall (getf door :in))
                do (setf *mega-in* key *mega-s* 0.0)
                   (when (getf door :world) (setf *mega-world* (getf door :world)))
                   (return))))
  (setf *place* (if *mega-in* *mega-world* :ring)))

(defcommon :place :value (list (or *mega-in* :ring) *mega-world* (* 10 (round *mega-s* 10)))
  :spec '(keyword keyword integer) :driver t
  :accept (lambda (v)
            (destructuring-bind (door world s) v
              (when (getf *worlds* world) (setf *mega-world* world))
              (setf *mega-in* (and (getf *doors* door) door))
              (when (> (abs (- s *mega-s*)) 30.0) (setf *mega-s* (float s 1.0))))))

;;; The tunnel is a door: one is through when deep enough in it that no daylight shows, and out again only by
;;; backing out. Its vault goes on over us for a while on the other side and is left behind.

(defun build-tunnel-threshold (entry)
  (shape entry :box '(.8 7.0 330.0) "#3c3c40" -7.9 3.5 -35.0 :shadow nil)
  (shape entry :box '(.8 7.0 330.0) "#3c3c40" 4.3 3.5 -35.0 :shadow nil)
  (shape entry :box '(13.0 .8 330.0) "#34343a" -1.8 7.4 -35.0 :shadow nil)
  (shape entry :box '(11.4 .12 330.0) "#2c2c2f" -1.8 .0 -35.0 :shadow nil)                              ; its roadway
  (loop for z from 126.0 downto -196.0 by 8.0 do (shape entry :box '(.5 .12 2.4) "#2a2620" -1.8 6.95 z :shadow nil))
  (dolist (x '(-13.0 9.4)) (shape entry :box '(9.0 12.0 1.6) "#9a9890" x 6.0 -200.4 :shadow nil))
  (shape entry :box '(31.4 4.6 1.6) "#9a9890" -1.8 10.0 -200.4 :shadow nil))

(add-door :tunnel
          :in (lambda () (and *tunnel* *mega* (> *dir* 0) (< (+ (first *tunnel*) 90.0) *dist* (- (second *tunnel*) 40.0))))
          :out (lambda () (or (null *tunnel*) (null *mega*) (< *dist* (+ (first *tunnel*) 85.0))))
          :gloom (lambda () (if (and *tunnel* *mega* (> *dir* 0) (< *dist* (- (second *tunnel*) 40.0)))
                                (max 0.0 (min 1.0 (/ (- *dist* (first *tunnel*) 20.0) 70.0)))
                                0.0))
          :threshold 'build-tunnel-threshold)

;;; The air. Everything of the light and the weather that a place changes goes through SET-AIR, as one description:
;;;   :GLOOM    how much of the day is shut out, from 0 to 1 (1 unless said: behind a door it is dark);
;;;   :HAZE     how much pale sourceless light stands there, and :FOG how thick the air is - both come on slowly;
;;;   :HOUR     the second of the day cycle the sky shows whatever the clock says;
;;;   :WEATHER  the weather to aim at whatever the forecast, a plist like *WX*;
;;;   :JUICE    how far the film over the picture is turned to full colour, from 0 to 1.
;;; On the ring there is no description: the air is the ring's own, but for the dark on the approach to a door.

(defvar *juice* 0.0 "How far the picture has been turned from the faded film to full colour, from 0 to 1.")
(defun juice (k)
  "Turn the film that lies over the picture towards full colour by K, from 0 (the faded film of the ring) to 1."
  (when (and *grade* (not (eql k *juice*)))
    (setf *juice* k)
    (object-call *grade* "set_shader_parameter" "sat" (+ .74 (* .3 k)))
    (object-call *grade* "set_shader_parameter" "gain" (+ .94 (* .04 k)))))

(defun set-air (air)
  "Make the air what the plist AIR describes; with NIL, the air of the ring."
  (flet ((towards (now want) (+ now (* .012 (- want now)))))
    (cond (air
           (let ((fog (getf air :fog)))
             (setf *gloom* (float (getf air :gloom 1.0) 1.0)
                   *haze* (towards *haze* (getf air :haze 0.0))
                   *fog-inside* (and fog (towards (or *fog-inside* fog) fog))
                   *sky-hour* (getf air :hour) *wx-force* (getf air :weather)))
           (juice (* (getf air :juice 0.0) (- 1.0 *gloom*))))
          (t (setf *gloom* (float (or (loop for (nil door) on *doors* by #'cddr
                                            when (getf door :gloom) maximize (funcall (getf door :gloom)))
                                      0.0)
                                  1.0)
                   *haze* 0.0 *fog-inside* nil *sky-hour* nil *wx-force* nil *sun-turn* 0.0)
             (juice 0.0)))))

(defun step-mega ()
  "Show the place behind the door to those in it, and nothing of the ring; lay it out along the way driven; give it
its air."
  (step-doors)
  (let ((in (inside-mega-p)))
    (when *land* (object-set *land* "visible" (not in)))
    (when *world* (object-set *world* "visible" (not in)))
    (cond ((and in (null *mega-node*)) (setf *mega-node* (build-mega)))
          ((and (not in) *mega-node*) (node-queue-free *mega-node*) (clrhash *mega-cells*) (clrhash *mega-lives*) (setf *mega-node* nil *mega-parts* nil)))
    (set-air (and in (or (world-call :air *mega-s*) (list :gloom 1.0))))
    (when (and in *mega-parts*)
      (step-mega-cells)
      (node-3d-set-position (getf *mega-parts* :entry) (v3 0 (- (mega-height *mega-s*)) *mega-s*))
      (object-set (getf *mega-parts* :entry) "visible" (< *mega-s* 900.0))
      (unless (or (net-guest-p) (place-rule :pumps))
        (when (< *fuel* .5) (setf *fuel* 1.0))))))                               ; nowhere to fill up: the tank is kept up

;;; A way straight into the tunnel, for working on what is behind it: the page address with a world's flag in it
;;; (?tunnel, ?summer), or the variable KIOSK_TUNNEL on the desktop. Once the lobby is closed, whoever owns the
;;; world is put a hundred metres before the tunnel with a full tank, the intro skipped, and the autopilot set
;;; through it.

(defvar *tunnel-spawn* :ask "Whether to start at the tunnel: :ASK until the address has been read, then T or NIL; :LOBBY while the lobby is open, :DONE once we are there.")

(defun tunnel-mode-p () "Return true if this game was started at the tunnel." (and (member *tunnel-spawn* '(t :lobby :done)) t))

(defun flagged-world (text)
  "Return the world whose flag stands in TEXT, or NIL."
  (loop for (key spec) on *worlds* by #'cddr
        when (and (getf spec :flag) (search (getf spec :flag) text)) return key))

(defun step-tunnel-spawn ()
  (when (eq *tunnel-spawn* :ask)
    (let* ((env (ext:getenv "KIOSK_TUNNEL"))
           (query (and (object-call (gdext:singleton "OS") "has_feature" "web")
                       (ignore-errors (object-call (gdext:singleton "JavaScriptBridge") "eval" "location.search"))))
           (world (flagged-world (cond ((and env (plusp (length env))) (if (flagged-world env) env "tunnel")) ((stringp query) query) (t "")))))
      (when world (setf *mega-world* world))
      (setf *tunnel-spawn* (and world t))))
  (when (and (eq *tunnel-spawn* t) *lobby-open*) (setf *tunnel-spawn* :lobby))        ; wait for the lobby to be answered
  (when (and (eq *tunnel-spawn* :lobby) *tunnel* *cart* (not *lobby-open*)
             (or (not *dev*) (and (boundp '*dev-started*) (symbol-value '*dev-started*))))     ; on the polygon: after its own start
    (setf *tunnel-spawn* :done)
    (unless (net-guest-p)
      (when (eq *story-stage* :intro)                                                   ; no intro: straight to the road
        (setf *story-stage* :owing)
        (when *show* (setf *show* nil) (show-card)))
      (setf *fuel* 1.0 *auto* nil *speed* 0.0 *dir* 1)
      (relocate (- (first *tunnel*) 100.0))
      (let ((next (or (find-if (lambda (tw) (> (town-stop-d tw) (second *tunnel*))) *towns*) (first *towns*))))
        (go-to-town next))
      (say-all "Старт у туннеля: бак полный, автопилот ведёт сквозь него"))))

(defun step-tunnel ()
  (step-tunnel-spawn)
  (step-mega)
  (when *tunnel*
    (destructuring-bind (d0 d1) *tunnel*
      (let ((from (- d0 36.0)) (to (+ d1 36.0)))
        (cover :tunnel 8.0
               (lambda (d i) (declare (ignore i))
                 (when (<= from d to)
                   (let ((*keep-reach* 290.0)) (add-tunnel-piece d (< (- d from) 8) (< (- to d) 8)))))
               290.0)))))
