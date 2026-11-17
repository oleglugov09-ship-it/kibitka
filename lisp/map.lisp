;;;; Map and autopilot. The map hangs on the back wall: press E on it and the road diagram with the
;;;; twelve towns opens. Pick a town and the truck drives to its bus stop by itself and parks. If the
;;;; town is behind us we 'turn around': the world is rebuilt 300 m before it (there is one road, we
;;;; always drive forward). The driver picks the route; for guests the map shows where we are.

(in-package #:kiosk-trip)

(defvar *map* nil "The map panel.")
(defvar *map-open* nil)
(defvar *map-parts* nil "The map widgets, as a plist: :towns (buttons), :truck (marker), :note, :close.")
(defvar *map-key* nil "True if the action key was held down in the previous frame.")
(defvar *auto* nil "The autopilot target, as (TARGET-PATH-COORDINATE . TOWN-NAME), or NIL.")
(defvar *auto-waited* 0.0 "The number of seconds the autopilot has already waited at the bus stop.")
(defvar *auto-skipped* '() "The bus stops where a waiting player did not board; the autopilot no longer brakes for them.")
(defvar *auto-waiting* nil "The name of the player the autopilot is waiting for at a bus stop right now.")
(defparameter *auto-brake* 1.2 "The deceleration the autopilot uses on approach, in m/s².")

(defun map-xy (x z)
  "Return the panel position of the terrain map point (X, Z) as two values.
The Crossroads is in the middle and north (-z) is at the top."
  (let ((k (/ 112.0 (/ (ring-length) (* 2 pi)))))
    (values (+ 320 (* k (- x (first *hub*)))) (+ 180 (* k (- z (second *hub*)))))))

(defun make-map (root)
  (let* ((layer (gdext:make-object "CanvasLayer")) (bg (gdext:make-object "ColorRect")))
    (object-set layer "layer" 6)
    (node-add-child root layer)
    (node-add-child layer bg)
    (object-set bg "position" (vector2 0 0)) (object-set bg "size" (vector2 640 360))
    (object-set bg "color" (color .04 .05 .06 .72))
    (hud-rect bg 20 16 600 328 '(.85 .82 .72) .97)
    (hud-rect bg 20 16 600 2 '(.42 .34 .22))
    (hud-label bg "КАРТА" 30 22 12 '(.2 .16 .1))
    (hud-label bg "Выбери город — грузовик доедет сам, кратчайшей дорогой. Дороги двусторонние: он сам решит, в какую сторону ближе, и развернётся, где просторно. S — остановить в пути."
               30 328 6 '(.3 .26 .18))
    (let ((dots (loop repeat 260 collect (hud-rect bg -10 -10 2 2 '(.42 .34 .22))))
          (towns (loop for i below *town-count*
                       collect (let ((b (lobby-control "Button" bg 0 0 78 16 "")))
                                 (object-set b "toggle_mode" t)
                                 (object-call b "add_theme_font_size_override" "font_size" 7)
                                 b)))
          (marks (loop repeat *town-count* collect (hud-rect bg -10 -10 6 6 '(.64 .15 .17))))
          (loads (loop repeat *town-count* collect (hud-label bg "" 0 0 6 '(.5 .12 .1))))
          (close (lobby-control "Button" bg 524 22 88 18 "Закрыть (E)")))
      (object-set close "toggle_mode" t)
      (setf *map-parts* (list :towns towns :close close :loads loads :dots dots :marks marks
                              :fuel (let ((c (lobby-control "CheckBox" bg 400 22 118 18 "К ближайшей АЗС")))
                                      (object-call c "add_theme_font_size_override" "font_size" 7)
                                      c)
                              :tp (let ((c (lobby-control "CheckBox" bg 290 22 106 18 "Чит: телепорт")))
                                    (object-call c "add_theme_font_size_override" "font_size" 7)
                                    c)
                              :hub (hud-label bg "Перекрёсток" 0 0 6 '(.2 .16 .1))
                              :truck (hud-rect bg 0 0 8 8 '(.15 .3 .5))
                              :note (hud-label bg "" 30 40 7 '(.2 .16 .1)))))
    (setf *map* bg)
    (object-set bg "visible" nil)))

(defun show-map ()
  "Lay the diagram out over the terrain map.
Roads become dots, towns become buttons and the truck becomes a marker."
  (when (and (ring) *hub*)
    ;; roads: a dot roughly every 240 m of each road
    (let ((dots (getf *map-parts* :dots)))
      (dolist (e *edges*)
        (loop for i from 0 below (length (edge-xs e)) by 120
              do (when dots
                   (multiple-value-bind (x y) (map-xy (aref (edge-xs e) i) (aref (edge-zs e) i))
                     (object-set (pop dots) "position" (vector2 (- x 1) (- y 1)))))))
      (dolist (d dots) (object-set d "position" (vector2 -10 -10))))
    (multiple-value-bind (hx hy) (map-xy (first *hub*) (second *hub*))
      (object-set (getf *map-parts* :hub) "position" (vector2 (- hx 22) (+ hy 4))))
    (let ((left (parcels-left)))
      (loop for b in (getf *map-parts* :towns) for tw in *towns* for m in (getf *map-parts* :marks)
            for l in (getf *map-parts* :loads) for i from 0
            do (multiple-value-bind (px pz) (road-point (town-stop-d tw))
                 (multiple-value-bind (x y) (map-xy px pz)
                   (let* ((ux (- x 320)) (uy (- y 180)) (len (max 1.0 (sqrt (+ (* ux ux) (* uy uy))))))
                     (object-set m "position" (vector2 (- x 3) (- y 3)))
                     (object-set b "position" (vector2 (+ x (* 44 (/ ux len)) -39) (+ y (* 20 (/ uy len)) -8)))
                     (object-set l "position" (vector2 (+ x (* 44 (/ ux len)) -39) (+ y (* 20 (/ uy len)) 8)))
                     (multiple-value-bind (total hops) (plan-way (town-stop-d tw) (turn-place-p))
                       (object-set b "text" (format nil "~a ~,1f~:[~;*~]" (town-title tw) (/ total 1000) hops)))
                     (let ((n (cdr (assoc i left)))) (object-set l "text" (if n (format nil "посылок: ~d" n) ""))))))))
    (multiple-value-bind (px pz) (road-point *dist*)
      (multiple-value-bind (x y) (map-xy px pz)
        (object-set (getf *map-parts* :truck) "position" (vector2 (- x 4) (- y 4))))))
  (object-set (getf *map-parts* :note) "text"
              (format nil "Бака хватит на ~,1f км~@[ · едем в ~a~] · звёздочка — путь через Перекрёсток~:[~; · маршрут выбирает водитель~]"
                      (* *fuel* *tank-km*) (and *auto* (cdr *auto*)) (net-guest-p))))

(defvar *map-click* nil "True if the mouse button was down in the previous frame.
It lets us catch the press itself rather than the hold.")

(defun nearest-road-d (x z)
  "Return the path coordinate of the point of any road nearest to the map point (X, Z)."
  (let ((best nil) (least nil))
    (dolist (e *edges* best)
      (loop for i from 0 below (length (edge-xs e)) by 5
            do (let ((r2 (+ (expt (- x (aref (edge-xs e) i)) 2) (expt (- z (aref (edge-zs e) i)) 2))))
                 (when (or (null least) (< r2 least)) (setf least r2 best (+ (edge-d0 e) (* i *map-step*)))))))))

(defun map-teleport ()
  "Move the wagon to the nearest piece of road when the map is clicked with the 'teleport' box ticked.
This is a cheat. Return T if the wagon was moved."
  (let* ((down (object-call (gdext:singleton "Input") "is_mouse_button_pressed" 1))
         (click (and down (not *map-click*))))
    (setf *map-click* down)
    (when (and click (object-get (getf *map-parts* :tp) "button_pressed"))
      (let* ((at (object-call *map* "get_local_mouse_position")) (mx (vector2-x at)) (my (vector2-y at)))
        (when (and (< 24 mx 616) (< 46 my 326))                          ; on the diagram itself, not on the buttons above it
          (cond ((net-guest-p) (say "Переносит водитель") nil)
                (t (let* ((k (/ 112.0 (/ (ring-length) (* 2 pi))))
                          (d (nearest-road-d (+ (first *hub*) (/ (- mx 320) k)) (+ (second *hub*) (/ (- my 180) k)))))
                     (setf *auto* nil *auto-then* nil *way* '() *speed* 0.0 *dir* 1 *yaw-now* 0.0 *lane-now* 0.0)
                     (relocate d)
                     (log-event :cheat :what :teleport :dist (round d))
                     (dolist (b (getf *map-parts* :towns)) (object-set b "button_pressed" nil))
                     (say-all "Чит: перенеслись")
                     t))))))))

(defun open-map ()
  (setf *map-open* t *map-key* t)
  (object-set (getf *map-parts* :fuel) "button_pressed" (and *auto* (equal (cdr *auto*) "ближайшая АЗС") t))
  (object-set *map* "visible" t)
  (show-map)
  (input-set-mouse-mode 0))

(defun close-map ()
  (setf *map-open* nil)
  (object-set *map* "visible" nil)
  (input-set-mouse-mode 2))

(defvar *auto-then* nil "The destination after turning around at the nearest bus stop, as (TARGET-PATH-COORDINATE . NAME).")

(defun turn-place-p ()
  "Return true if there is room here to turn around.
That means a town, bus stop, gas station or the Crossroads, and the truck standing still."
  (and (< *speed* 1.0)
       (or (town-at *dist*) (near-p *stop-ds*) (near-p *stations*)
           (and (not (ring-d-p *dist*))
                (let ((e (edge-of *dist*)))
                  (< (abs (- *dist* (if (eq (edge-next e) :hub) (edge-d1 e) (edge-d0 e)))) 80.0))))))

(defun swap-sides ()
  "Move the people waiting at bus stops to our side of the road after the truck has turned around."
  (dolist (s *stops*)
    (loop for p in (second s) for i from 0
          do (destructuring-bind (d x) (stop-spot (first s) i) (place-person p d x))))
  (dolist (p *net-peers*)
    (when (getf p :node) (node-queue-free (getf p :node)) (setf (getf p :node) nil))))

(defvar *auto-replan* nil "True if we turned towards a bus stop to pick up a passenger.
Once the passenger boards, the course must be planned again.")

(defun right-side-place-p ()
  "Return true if we stand where everything playable is only to the right of the road.
Such places are a town, gas station, booth, observatory or drop-off point."
  (or (town-at *dist*) (near-p (service-ds))))

(defun park-right (&optional replan)
  "Pull onto the lot when the truck has stopped at a right-side place facing backwards.
The truck turns so that its side and window face the lot. With REPLAN true, ask the autopilot
to plan its course again afterwards."
  (when (and (< *dir* 0) (not (turning-p)) (right-side-place-p))
    (when replan (setf *auto-replan* t))
    (setf *dir* 1)
    (swap-sides)
    (say-all "Заезжаем на площадку")))

(defun head-to (dir)
  "Set off in direction DIR, turning around first if the truck faces the other way."
  (unless (= dir *dir*)
    (setf *dir* dir)
    (swap-sides)
    (say-all "Разворачиваемся")))

(defun next-turn-spot ()
  "Return the nearest bus stop ahead on our road, where the truck can turn around.
The result is (DISTANCE-TO-IT . PATH-COORDINATE), or NIL if there is none."
  (let ((best nil) (e (edge-of *dist*)))
    (dolist (d *stop-ds* best)
      (when (eq (edge-of d) e)
        (let ((gap (if (eq e (ring)) (ring-gap *dist* d *dir*) (* *dir* (- d *dist*)))))
          (when (and (> gap 30.0) (or (null best) (< gap (car best)))) (setf best (cons gap d))))))))

(defun set-course (target title)
  "Plan the autopilot's course to TARGET, announced under the name TITLE.
Return the length and the turns, or NIL if there is no road. If driving backwards is shorter
but there is no room to turn around here, head for the nearest bus stop first and turn there."
  (let ((free (turn-place-p)))
    (multiple-value-bind (total hops dir) (plan-way target free)
      (let ((back (and (not free) (route *dist* (- *dir*) target))) (spot (and (not free) (next-turn-spot))))
        (cond ((and back spot (or (null total) (< (+ (car back) (* 2 (car spot)) 200.0) total)))
               (setf *auto* (cons (cdr spot) title) *auto-then* (cons target title) *auto-skipped* '() *way* '())
               (values (+ (car back) (* 2 (car spot))) (cdr back) t))
              (total (head-to dir)
                     (setf *auto* (cons target title) *auto-then* nil *auto-skipped* '() *way* hops)
                     (values total hops nil)))))))

(defun go-to-town (tw)
  "Drive to the town TW on autopilot."
  (let ((target (town-stop-d tw)))
    (cond ((net-guest-p) (say "Маршрут выбирает водитель"))
          ((rail-ride tw))                                        ; the wagon is on a flatcar, so we go by train (prikol.lisp)
          ((< (abs (- target *dist*)) 12) (say (format nil "Мы уже в городе ~a" (town-title tw))))
          (t (multiple-value-bind (total hops later) (set-course target (town-title tw))   ; shortest road in either direction
               (log-event :auto :town (town-index tw) :from (round *dist*) :fuel *fuel* :hub (and hops t) :dir *dir*)
               (say-all (format nil "Автопилот: едем в ~a, ~,1f км~:[~; — через Перекрёсток~]~:[~; · развернёмся на ближайшей остановке~]"
                            (town-title tw) (/ total 1000) hops later)))))))

(defun step-map ()
  (when *map-open*
    (let ((down (some (lambda (a) (object-call (gdext:singleton "Input") "is_action_pressed" (action-name a) nil))
                      '(:map :use))))                                  ; M closes it the way it was opened; E out of old habit
      (when (and down (not *map-key*)) (close-map) (return-from step-map))
      (setf *map-key* down))
    (when (object-get (getf *map-parts* :close) "button_pressed")
      (object-set (getf *map-parts* :close) "button_pressed" nil)
      (close-map) (return-from step-map))
    (when (map-teleport) (close-map) (return-from step-map))
    ;; checkbox: stop at the nearest gas station on the way (unticking it cancels the autopilot)
    (let ((box (getf *map-parts* :fuel)) (to-fuel (and *auto* (equal (cdr *auto*) "ближайшая АЗС"))))
      (cond ((and (object-get box "button_pressed") (not to-fuel) (not (net-guest-p)))
             (let ((best nil) (free (turn-place-p)))                       ; nearest by road, in either direction
               (dolist (s *stations*)
                 (let ((len (plan-way s free)))
                   (when (and len (or (null best) (< len (car best)))) (setf best (cons len s)))))
               (cond (best (set-course (cdr best) "ближайшая АЗС")
                           (say-all (format nil "Автопилот: к ближайшей АЗС, ~,1f км" (/ (car best) 1000)))
                           (close-map) (return-from step-map))
                     (t (object-set box "button_pressed" nil) (say "По этой дороге заправок нет")))))
            ((and (not (object-get box "button_pressed")) to-fuel) (setf *auto* nil))))
    (loop for b in (getf *map-parts* :towns) for tw in *towns*
          do (when (object-get b "button_pressed")
               (object-set b "button_pressed" nil)
               (go-to-town tw)
               (close-map) (return-from step-map)))
    (show-map)))

(defun step-auto (delta)
  "Advance the autopilot by DELTA seconds.
It accelerates to cruising speed, brakes smoothly towards the target and stops at the bus stop.
Without the autopilot the truck does not drive: whatever got it moving earlier, it brakes to a
full stop."
  (when (and (not *auto*) (not (net-guest-p)))
    (if (> *speed* 0.0)
        (setf *speed* (max 0.0 (- *speed* (* *brake* delta))))
        (park-right)))
  (when (and *auto* (not (net-guest-p)) (turning-p)) (setf *speed* 0.0))        ; turn around only while standing still
  (when (and *auto* (not (net-guest-p)) (not (turning-p)))
    ;; a new player is waiting on the way: stop at their bus stop by ourselves and drive on as soon as they board
    (let* ((pickup (first (sort (loop for p in *net-peers*
                                      for w = (getf p :wait)
                                      when (and (getf p :placed) w (way-on w) (< (way-on w) (way-to (car *auto*)))
                                                (not (member w *auto-skipped*)))
                                        collect (cons w (getf p :name)))
                                #'< :key (lambda (c) (way-on (car c))))))
           (goal (if pickup (car pickup) (car *auto*)))
           (left (way-to goal)))                       ; on the ring road a target behind us is ahead of us one lap later
      (cond ((and *auto-replan* (null pickup))                          ; passenger aboard: carry on to our destination
             (setf *auto-replan* nil)
             (set-course (car *auto*) (cdr *auto*)))
            ((and pickup (< left 1.5))
             (setf *speed* 0.0)
             (park-right t)
             (unless (equal *auto-waiting* (cdr pickup))
               (setf *auto-waiting* (cdr pickup) *auto-waited* 0.0)
               (say-all (format nil "Остановка: садится ~a. Дальше поедем сами" (cdr pickup))))
             ;; did not board within 20 seconds (walked away, connection failed): drive on, pick them up another time
             (when (> (incf *auto-waited* delta) 20.0)
               (push (car pickup) *auto-skipped*)
               (say-all (format nil "~a не сел — едем дальше" (cdr pickup)))))
            ((and (< left 1.5) *auto-then*)                              ; reached the turning spot, now head for the target
             (setf *speed* 0.0)
             (let ((then *auto-then*))
               (setf *auto* nil *auto-then* nil)
               (set-course (car then) (cdr then))))
            ((< left 1.5)
             (setf *speed* 0.0)
             (say-all (format nil "Приехали: ~a" (cdr *auto*)))
             (log-event :arrived :dist (round *dist*) :fuel *fuel*)
             (setf *auto* nil *auto-waiting* nil))
            (t (let ((want (min *max-speed* (sqrt (* 2 *auto-brake* (max 0.0 (- left 1.0)))))))
                 (setf *speed* (if (< *speed* want)
                                   (min want (+ *speed* (* *throttle* delta)))
                                   (max want (- *speed* (* *brake* delta)))))))))))
