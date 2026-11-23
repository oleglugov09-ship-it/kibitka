;;;; The LIVE package: watch and change the trip from the REPL (Slynk, Autolith, tools/godot-ask).
;;;; Every function runs in the Godot main thread (main-thread.lisp), so it can be called
;;;; from any thread. The parameters (*speed*, *amp*, *grass*, *kc*, *sky-keys*…)
;;;; can also simply be SETF-ed from the REPL: whatever is computed every frame is picked up at once,
;;;; the land is rebuilt by (live:regen), the wagon by (live:rebuild-cart).

(defpackage #:live
  (:use #:cl)
  (:export #:everyone #:answers #:state #:pace #:time-of-day #:time-scale #:new-trip #:regen #:rebuild-cart
           #:window #:sit #:fullscreen #:lights #:style #:disc #:discs #:door #:profile #:season #:weather #:meta #:spell #:aa #:look #:view #:host #:join #:net #:stand #:skip #:screenshot #:eval-on-main #:on-main #:on-main-sync))

(in-package #:kiosk-trip)

(defmacro live-fn (name args doc &body body)
  `(defun ,(intern (symbol-name name) '#:live) ,args ,doc
     (on-main-sync (lambda () ,@body))))

(live-fn everyone (form)
  "Evaluate FORM here and on every guest at once: (live:everyone '(setf kiosk-trip::*tank-price* 64)).
FORM may also be a string with several forms. Return the local result; the guests' replies are in
(live:answers)."
  (everyone (if (stringp form) form (let ((*package* (find-package '#:kiosk-trip))) (prin1-to-string form)))))

(live-fn answers () "Return what the guests replied to the last live edit: ((name . result-or-error) ...)." *answers*)

(live-fn state ()
  "Return where the wagon is and what is around."
  (list :dist (round *dist*) :speed *speed* :seed *trip-seed*
        :scene (scene-name *dist*)
        :season *season* :weather *weather* :weather-left (round *weather-left*)
        :road (multiple-value-bind (course slope) (road-frame *dist*)
                (list :course-deg (round (* course 180) pi) :slope-% (round (* 100 (tan slope))) :height (round (road-y *dist*))))
        :day-t (round (day-phase *day-t*)) :daylight (float (daylight (sky-now)) 1.0) :time-scale *time-scale*
        :props (length *props*) :bridges (length *bridges*)
        :pos (list (vector3-x *pos*) (vector3-z *pos*)) :yaw *yaw* :pitch *pitch*
        :window *window-open* :seat (first *seat*) :meta *meta*
        :view *view*))

(live-fn view (&optional (where :outside))
  "Set the view to WHERE: :outside for the orbit camera, :inside for the view from the wagon."
  (setf *view* where))

(live-fn host (&optional (name "Водитель")) "Open a lobby as the driver called NAME." (net-host name))
(live-fn join (address &optional (name "Попутчик")) "Join the driver at ADDRESS as NAME." (net-join address name))
(live-fn net () "Return the state of the network."
  (list :role *net-role* :id *net-id* :others *net-others* :wait *net-wait* :departed *departed* :ver *net-ver* :version *net-version* :refused *net-refused*
        :peers (mapcar (lambda (p) (list (getf p :id) (getf p :name) (getf p :wait))) *net-peers*)))

(live-fn window (&optional (open t)) "Slide the big window open when OPEN is true, or shut it when OPEN is NIL." (setf *window-open* open))

(live-fn pace (v) "Set the truck speed to V, in m/s.
Without the autopilot the truck brakes at once; to drive, call (kiosk-trip::go-to-town ...)." (setf *speed* (float v 1.0)))

(live-fn time-of-day (what)
  "Set the time of day to WHAT: seconds since dawn, or :dawn :day :noon :sunset :night :midnight."
  (setf *day-t* (float (case what (:dawn 20) (:day 200) (:noon 390) (:sunset 720) (:night 860) (:midnight 1000)
                         (t what))
                       1.0)))

(live-fn time-scale (k) "Make the day cycle run K times faster." (setf *time-scale* (float k 1.0)))

(live-fn new-trip (&optional seed) "Start a new trip with a fresh landscape, from SEED when given."
  (if seed (start-trip :seed seed) (start-trip)) *trip-seed*)

(live-fn regen () "Rebuild the land and the roadside with the current parameters, at the same path coordinate."
  (let ((d *dist*))
    (reset-segments)
    (dolist (p *props*) (node-queue-free (car p)))
    (setf *props* '())
    (remhash :props *spans*)                 ; the roadside trees — anew
    (step-props)
    d))

(live-fn rebuild-cart () "Rebuild the wagon; use it after changing sizes and colours." (make-kibitka *rig*) t)

(live-fn lights (&optional (on t)) "Switch the inside lights on when ON is true, or off when ON is NIL (the gaps show). In game: L."
  (setf *lights-on* on) (apply-lights) on)

(live-fn style (&optional what)
  "Set the textures to WHAT: :painted, :pixel, :pbr for soft PBR, or NIL for none."
  (setf *style* what) (apply-style)
  what)

(live-fn discs () "Re-read the discs in project/discs/ and return them with what is playing."
  (load-discs) (show-discs) (list :discs (mapcar #'first *discs*) :playing (and *playing* (disc-name *playing*))))

(live-fn disc (&optional which)
  "Play the disc WHICH: a number or a file name without extension; NIL takes the disc out."
  (let ((i (if (stringp which) (position which *discs* :key #'first :test #'string-equal) which)))
    (setf *crate* (remove i *crate*))
    (when (and *playing* (not (eql *playing* i))) (setf *crate* (append *crate* (list *playing*))))
    (play-disc i)
    (and i (disc-name i))))

(live-fn door (&optional (open t))
  "Open the door when OPEN is true, or close it when OPEN is NIL."
  (unless (eq (and open t) *door-open*) (toggle-door))
  *door-open*)

(live-fn meta (&optional (on t) (curtains-open nil))
  "Enter the meta room when ON is true, or return to the wagon when ON is NIL, without the spell.
CURTAINS-OPEN leaves the curtains open."
  (setf *meta* on *meta-curtains-shut* (not curtains-open))
  (apply-meta)
  (list :meta *meta* :curtains (if *meta-curtains-shut* :shut :open)))

(live-fn spell ()
  "Cast the spell as the Q key does: put out the lights and close the shutters and the door if needed."
  (unless *meta*
    (setf *lights-on* nil *window-open* nil *window-k* 0.0 *door-open* nil *door-k* 0.0)
    (apply-lights) (apply-window) (apply-door))
  (cast-spell)
  *spell*)

(live-fn season (&optional which)
  "Set the season to WHICH: :spring :summer :autumn :winter; with no argument, the next one.
The new season shows on what is built ahead; call (live:regen) to apply it everywhere at once."
  (set-season (or which (nth (mod (1+ (position *season* *seasons*)) 4) *seasons*))))

(live-fn weather (&optional which minutes)
  "Set the weather to WHICH: :clear :cloudy :rain :fog :snow.
It blends in over about 30 s and holds for MINUTES minutes."
  (set-weather (or which (pick-weather)) minutes))

(live-fn aa (&optional (mode :taa))
  "Set anti-aliasing to MODE: :fxaa on the edges of the finished frame (the default), :taa temporal
(it smears in motion), or NIL for none. MSAA is off: it gave broken pixels on edges, and the glow
blew them up into white blots."
  (let ((vp (node-get-viewport *root*)))
    (object-set vp "use_taa" (eq mode :taa))
    (object-set vp "screen_space_aa" (if (eq mode :fxaa) 1 0))
    mode))

(live-fn profile () "Return the profile since the previous call: frame time, Lisp parts, Godot counters." (profile-report))

(live-fn fullscreen (&optional (on t)) "Go full screen when ON is true, or windowed when ON is NIL. In game: F11." (set-fullscreen on) on)

(live-fn sit (&optional (where :stool))
  "Sit at WHERE: :stool by the table, :fold on the folded-down shelf, :bed or :crate; NIL stands up."
  (let ((seat (and where (find where *seats* :key #'first))))
    (cond ((null where) (setf *seat* nil))
          (seat (setf *pos* (v3 (second seat) 0 (third seat)) *seat* nil)
                (toggle-sit) *seat*)
          (t (list :нет-такого-места (mapcar #'first *seats*))))))

(live-fn look (yaw &optional (pitch 0.0)) "Look towards YAW and PITCH: yaw -1.57 faces the window, 0 faces forward." (setf *yaw* (float yaw 1.0) *pitch* (float pitch 1.0)))

(live-fn stand (x z) "Stand at X (across) and Z (along) inside the wagon." (setf *pos* (v3 x 0 z)))

(live-fn skip (meters) "Drive METERS ahead in one go; the land and the roadside catch up."
  (jump-dist (+ *dist* (* *dir* meters)))
  (round *dist*))

(live-fn screenshot (&optional (path "user://shot.png"))
  "Save a screenshot to PATH and return its path in the OS."
  (let ((image (object-call (object-call (node-get-viewport *root*) "get_texture") "get_image")))
    (object-call image "save_png" path)
    (project-settings-globalize-path path)))

(defun live:eval-on-main (function) (on-main-sync function))
(defun live:on-main (function) (on-main function))
(defun live:on-main-sync (function) (on-main-sync function))
