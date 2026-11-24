;;;; The scene root (main.tscn, TripRoot): at start it builds the sky, the world (under Turn),
;;;; the wagon and the camera (under Rig); every frame it moves the world, the day cycle, the curtains and
;;;; the camera. The state lives in special variables of the other files: visible and changeable from the REPL.

(in-package #:kiosk-trip)

(defclass trip-root (node-3d)
  ()
  (:metaclass godot-class))

(defvar *time* 0.0 "The seconds since the scene started.")

(defparameter *world-seed* 1 "The world seed.
It is one for everyone and for ever: terrain, road and towns do not change from launch to launch.")

(defun relocate (d)
  "Move to path coordinate D, anywhere on the map.
The world is not rebuilt: the land lies by the map, and roadside things are built in a window around
the wagon. The tank, the money and the things are left alone."
  (reset-trade)
  (jump-dist (max (if (ring) (edge-d0 (ring)) 0.0) d)))        ; the ring road starts before path coordinate zero

(defun start-trip (&key (seed *world-seed*))
  "Start a new trip with seed SEED: the world from scratch, the wagon and the sky stay."
  (setf *trip-seed* seed)
  (stages
    (reset-trade t)
    (reset-roadside)               ; towns are laid out before the land: the map and its pads are built from them
    (make-world *turn*)
    (settle-land)))

;;; The progress of the game (path coordinate, money, tank, planks, hint step) is kept between launches: the
;;; things in the wagon are saved, and without the rest a restart became a gift: the same crates, 10 roubles again.

(defvar *game-saved* nil)
(defvar *game-timer* 3.0)

(defun game-now () (list (round *dist*) *money* (float *fuel* 1.0) *planks* *onboard* *dir* (round *day-t* 10)))

(defun save-game (delta)
  "Save the game once every 3 seconds if something has changed, counting time by DELTA.
A guest does not write: the game is not theirs."
  (when (and (<= (decf *game-timer* delta) 0) (not (net-guest-p)))
    (setf *game-timer* 3.0)
    (let ((now (game-now)))
      (unless (equal now *game-saved*)
        (setf *game-saved* now)
        (ignore-errors (put-save :game now))))))

(defun load-game ()
  "Continue from where we stopped.
Return NIL if there is no save."
  (let ((g (get-save :game)))
    (when (and (fits g '(real real real real integer &optional t t))
               (<= 0 (first g) 1e6) (<= 0 (third g) 1))
      (destructuring-bind (dist money fuel planks onboard &optional (dir 1) day) g
        (when (and (integerp day) (<= 0 day 100000000)) (setf *day-t* (* 10.0 day)))  ; the day cycle goes on, not restarts
        (when (eql dir -1) (setf *dir* -1 *yaw-now* pi *lane-now* *lane-x*))       ; stood nose backwards: still do
        (setf *money* (round money) *fuel* (float fuel 1.0) *planks* (max 0 (round planks)) *onboard* onboard
              *game-saved* g)
        (when (> dist 20) (relocate dist))
        t))))

(defun new-game ()
  "Start from the depot: the starting things, money and tank."
  (when *auto* (setf *auto* nil))
  (setf *carry* nil *carry-from* nil *onboard* 0 *planks* 6)
  (reset-world)                                           ; every part of the state goes back to the start (delo.lisp)
  (show-carry)
  (start-trip)
  (set-items (address-crates *start-interior*))
  (save-interior)
  (say "Новая игра. Мы на складе"))

(defun make-kibitka (rig)
  "Build the whole wagon under RIG.
That is the body, the truck under it, everything bolted on inside, the things and the record player."
  ;; the previous wagon is about to vanish with everything that stood on it: forget the references, leave the nodes
  (setf *client* nil *client-seen* nil *leavers* '() *tweens* '() *net-avatars* '())
  (let ((c (make-cart rig)))
    (make-team c)
    (make-byt c)
    (make-uyut c)
    (make-kettle c)
    (make-fridge c)
    (make-mood c)
    (make-home c)
    (make-spyglass c)
    (make-folder c)
    (make-trophies c)
    (make-interior c)
    (make-jukebox c)
    c))

(defmethod ready ((root trip-root))
  (stages
  (setf *root* root *time* 0.0)
  (setf *random-state* (make-random-state t))
  (init-lang) (init-dev)
  (load-mods)                                             ; stories and whatever else mods bring (mods.lisp)
  (adopt-goods)                                           ; a mod's goods without a model get a place among the things
  (make-action-names)
  (add-layout-twins)
  (make-pivots root)
  (make-sky root)
  (make-land root)                                        ; the land: the baked map, drawn by Terrain3D (zemlya.lisp). The world stands on it
  (start-trip)
  (read-mega-paints)
  (read-places)                                           ; where things stand in the wagon: from the Blender files (mesta.lisp)
  (make-kibitka *rig*)
  (make-camera *rig*)
  (unless *baking*
    (object-call *terrain* "set_camera" *camera*)
    (load-water *land*))
  (load-place)
  (load-game)
  (load-discs)
  (make-hint root)
  (make-climate *rig*)
  (make-meta *rig*)
  (make-grade root)
  (make-hud root)
  (make-log root)
  (make-lobby root)
  (make-look root)
  (make-stars root)
  (make-aurora root)
  (make-scope root)
  (make-docs root)
  (load-world)                                            ; every part of the state comes from the save (delo.lisp)
  (make-map root)
  ;; a probe of live editing: can this build run code sent to it (in the browser, through the interpreter).
  ;; A line in the log at every launch; it shows whether (live:everyone ...) will work for the player
  (format t "~&Проба живой правки: ~a~%" (run-text "(progn (defun eval-probe (x) (* x 3)) (eval-probe 14))"))
  (when (member "torture" *dev-flags* :test #'string=) (gc-torture))   ; the browser build's memory under load: dev.lisp
  (unless (net-from-env) (open-lobby))))

(defvar *paused* nil "True while the pause menu (Esc) is open: the world stands still, the REPL works.")

(defun tree-paused-p () (object-get (object-call *root* "get_tree") "paused"))

;;; Minimise the window or switch to another tab and the browser stops calling frames, so the driver's
;;; truck freezes (the guests meanwhile ride on at the last speed). On return we work out how much
;;; real time has passed, and the truck 'drives' that time in one go: burning gasoline as it goes,
;;; and if the tank runs out, at a crawl, as usual.

(defvar *last-ticks* nil "The wall-clock time of the previous frame, in ms.")

(defun catch-up (gap)
  "Catch up on path, gasoline and time of day after being away for GAP seconds."
  (unless (or (net-guest-p) (< *speed* .2))
    (let* ((range (* *fuel* *tank-km* 1000.0))                       ; how far the tank will last
           (wish (* *speed* gap))
           (free (if (<= wish range) wish (+ range (* 2.5 (- gap (/ range *speed*))))))
           (to-turn (if *way* (max 0.0 (- (way-to (first (first *way*))) 5.0)) free))                 ; not past the next turn
           (driven (if *auto* (min free to-turn (max 0.0 (way-to (car *auto*)))) free)))               ; autopilot stops at target
      (setf *fuel* (max 0.0 (- *fuel* (/ (min driven range) (* 1000 *tank-km*)))))     ; gasoline goes by distance, not time
      (log-event :away :gap (round gap) :driven (round driven) :dist (round (+ *dist* (* *dir* driven))) :fuel *fuel*)
      (if (> driven 60) (jump-dist (+ *dist* (* *dir* driven))) (incf *dist* (* *dir* driven)))))
  (incf *day-t* (* gap *time-scale*)))

(defvar *bake-wanted* *baking* "True until the map is baked, in a run that is not a game but the baking of the map (bake.lisp).")
(defvar *things-wanted* (and (ext:getenv "KIOSK_BAKE_THINGS") t) "True until the things of the map are baked, in a run that only
does that (tools/bake-map.sh --things).")
(defvar *ribbons-wanted* (and (ext:getenv "KIOSK_BAKE_ROADS") t) "True until the ribbons of the roads are baked, in a run that
only does that: from the map as it is baked, not from the formulas (tools/bake-map.sh --roads).")

(defmethod process ((root trip-root) delta)
  (when *things-wanted*
    (setf *things-wanted* nil)
    (bake-things)
    (object-call (object-call root "get_tree") "quit")
    (return-from process nil))
  (when *ribbons-wanted*
    (setf *ribbons-wanted* nil)
    (bake-ribbons)
    (object-call (object-call root "get_tree") "quit")
    (return-from process nil))
  (when *bake-wanted*                                      ; on the first frame, once the scene stands: bake and leave
    (setf *bake-wanted* nil)
    (bake-map root)
    (object-call (object-call root "get_tree") "quit")
    (return-from process nil))
  (let ((now (object-call (gdext:singleton "Time") "get_ticks_msec")))
    (when *last-ticks*
      (let ((gap (- (/ (- now *last-ticks*) 1000.0) delta)))
        (when (and (> gap 1.0) (not (tree-paused-p))) (noting :catch-up (catch-up (min gap 1800.0))))))
    (setf *last-ticks* now))
  ;; the root runs on pause too (process_mode ALWAYS): the REPL queue does not stall, but the world does
  (quiet-debugger-on)                                            ; log.lisp: no Lisp debugger, ever
  (let ((paused (tree-paused-p)))
    (when (and *paused* (not paused)) (input-set-mouse-mode 2))   ; 'Continue': we look with the mouse again
    (setf *paused* paused)
    ;; the network and the lobby live on pause too: otherwise after a minute in the menu everyone times out
    ;; an error in a frame must not pass silently: to the console and to the log (each one once)
    (handler-case
        (progn
          (if paused
              (progn (incf *time* delta)                       ; network timers run on pause too
                     (step-net delta) (step-lobby))
              (step-trip delta))
          (save-game delta)
          (step-saves delta)
          (step-copies)
          (step-map)
          (step-log delta))
      (error (e) (note-error e "кадр")))))

(defun step-trip (delta)
  (incf *time* delta)
  (note-frame delta)
  (timed :frame
    (timed :world (step-world delta))
    (timed :sky (step-sky delta))
    (timed :cart (step-cart delta *time*))
    (timed :walk (step-walk delta))
    (timed :style (step-style delta))
    (timed :hint (step-hint delta) (step-notes delta))
    (timed :door (step-door delta))
    (timed :climate (step-climate delta))
    (timed :roadside (step-roadside delta))
    (timed :vista (step-vista delta))
    (step-interior delta)
    (step-byt delta)
    (step-uyut delta)
    (step-kettle delta)
    (step-spoil)
    (step-mood delta)
    (step-home delta)
    (step-finds)
    (unless (net-guest-p) (step-market-note))
    (step-prikol)
    (step-check delta)
    (step-gnomes)
    (unless (net-guest-p) (step-stories))
    (step-act delta) (step-story delta)
    (unless (net-guest-p) (step-plan))
    (step-stars)
    (step-aurora)
    (step-plane)
    (step-trade delta)
    (step-auto delta)
    (step-net delta)
    (step-lobby) (step-lang) (step-debug) (step-cheats) (step-js-repl)
    (step-look)
    (timed :meta (step-meta delta))))

(defmethod unhandled-input ((root trip-root) event)
  (unless (or *paused* *lobby-open* *map-open* *show*) (noting "key" (handle-input event))))
