;;;; The camera, walking and keys. Inside we walk in first person: WASD, Shift to run, the mouse to look
;;;; (a click captures the mouse, Esc releases it). Outside (V) the camera orbits the truck; it drives
;;;; only on autopilot (map.lisp), S stops it. Keys are Godot actions; their table *ACTIONS*
;;;; with the handlers is below, and they are rebound in the settings. Where we stood or sat is remembered
;;;; between runs (user://player.sexp).

(in-package #:kiosk-trip)

(defparameter *eye* 1.2 "The eye height above the floor: we are cats, shorter than a human.")
(defparameter *mouse-sens* .0025)
(defparameter *bounds* '(-1.4 1.4 -2.55 2.68) "The bounds x0 x1 z0 z1 on the wagon floor, right up to the back wall with the door.")

(defvar *seats* '() "The places where one can sit right now: (what x z look-direction eye-height); built by REFRESH-SEATS.")
(defparameter *sit-reach* 1.3 "The distance to a seat within which one can sit down.")
(defvar *seat* nil "The seat we are sitting on, or NIL.")
(defvar *camera* nil)
(defvar *pos* (v3 .2 0 .9) "The position where we stand (x, -, z).")
(defvar *yaw* -1.45 "The head yaw: 0 is forward (z−), −π/2 is towards the window (x+).")
(defvar *pitch* 0.0)

(defparameter *view* :outside
  "The point we look from.
:outside is from the outside, with the camera orbiting the wagon; :inside is from within, walking.")
(defvar *orbit-yaw* .42 "The side the outside camera is on (0 is straight behind).")
(defvar *orbit-pitch* .3 "The elevation of the outside camera.")
(defparameter *orbit-dist* 14.0 "The distance from the outside camera to the wagon, in metres.")

(defun outside-p () (and (eq *view* :outside) (not *meta*)))

(defun step-outside (delta)
  "Step the outside view: the camera orbits the truck.
The truck drives only on autopilot (the map); the player can just stop it with S."
  (declare (ignorable delta))
  (when *torch-node* (object-set *torch-node* "visible" nil))   ; the camera is outside — the torch is not in the frame
  (unless (net-guest-p)                                        ; only the driver stops the truck
    (when (and *auto* (action-down :move-back))                ; STEP-AUTO brakes to a full stop
      (setf *auto* nil)
      (say-all "Стоп. Дальше — по карте (M)")))
  (let ((cp (cos *orbit-pitch*)) (d *orbit-dist*))
    (node-3d-set-position *camera* (v3 (* d (sin *orbit-yaw*) cp) (+ 2.2 (* d (sin *orbit-pitch*))) (- (* d (cos *orbit-yaw*) cp) 1.5)))
    (node-3d-set-rotation *camera* (v3 (- *orbit-pitch*) *orbit-yaw* 0))))

(defun make-camera (root)
  (setf *camera* (gdext:make-object "Camera3D"))
  (object-set *camera* "fov" 72.0)
  (object-set *camera* "near" .08)             ; a little farther than usual: more precise depth, less flicker
  (object-set *camera* "far" 2600.0)           ; the far plane is visible a kilometre or a kilometre and a half away
  (node-add-child root *camera*)
  (object-set *camera* "current" t))

;;; Keys are Godot actions (the [input] section of project.godot); they are rebound in the settings,
;;; on the 'Controls' tab. The light, curtains, shutters, table and ladder are not keys but objects in the
;;; wagon: you look at them and press 'use' (E), see USABLES in byt.lisp. The menu labels an action with its
;;; name in title case ('Move Forward') and translates it — we supply the Russian translation ourselves
;;; (MAKE-ACTION-NAMES). Esc, F2, F11 and the build keys (R and the digits) remain fixed.

(defparameter *actions*
  '((:move-forward "move_forward" "Вперёд")
    (:move-back "move_back" "Назад; снаружи — остановить грузовик")
    (:move-left "move_left" "Влево")
    (:move-right "move_right" "Вправо")
    (:run "run" "Бежать")
    (:use "use" "Действие: нажать, сесть" press-use)
    (:view "view" "Вид: снаружи или изнутри" press-view)
    (:map "map" "Карта: куда ехать" press-map)
    (:build "build" "Стройка" press-build)
    (:break-crate "break_crate" "Разобрать ящик на доски" press-break)
    (:buy-fuel "buy_fuel" "Купить бензин" buy-fuel)
    (:buy-planks "buy_planks" "Купить доски" buy-planks)
    (:spell "spell" "Заклинание" press-spell)
    (:torch "torch" "Фонарик" press-torch)
    (:jump "jump" "Прыжок" press-jump)
    (:hide-hud "hide_hud" "Скрыть приборы и надписи" press-hide-hud)
    (:unstuck "unstuck" "Застрял: встать в центр кибитки" press-unstuck))
  "The action table: (key godot-action-name settings-label [press-handler]).
Actions without a handler (walking, running) are polled every frame. A handler returns true if the press
was its own: otherwise the following actions try the key (R is both rotation in build mode and fuel at
the gas station).")

;;; The keyboard layout. Actions are bound to physical keys, but under the Russian layout the input system
;;; may hand the game a letter instead of a key (the Cyrillic letter on the W key rather than W). So every
;;; action has a twin — the same key by its Russian letter — and the keys we compare ourselves go through KEY-CODE.

(defparameter *ru-keys* "QЙWЦEУRКTЕYНUГIШOЩPЗAФSЫDВFАGПHРJОKЛLДZЯXЧCСVМBИNТMЬ"
  "The Latin key and the Russian letter on it, in pairs.")

(defun ru-of (latin) (let ((i (position (code-char latin) *ru-keys*))) (and i (evenp i) (char-code (char *ru-keys* (1+ i))))))
(defun latin-of (code)
  (let ((i (and (< 1024 code 1120) (position (char-upcase (code-char code)) *ru-keys*))))
    (if (and i (oddp i)) (char-code (char *ru-keys* (1- i))) code)))

(defun key-code (event)
  "Return the key pressed in EVENT as a Latin key code, in any layout."
  (let ((p (object-get event "physical_keycode")))
    (latin-of (if (and (integerp p) (plusp p)) p (object-get event "keycode")))))

(defun key-event (&key keycode physical)
  (let ((e (gdext:make-object "InputEventKey")))
    (when keycode (object-set e "keycode" keycode))
    (when physical (object-set e "physical_keycode" physical))
    (object-set e "pressed" t)
    e))

(defun add-layout-twins ()
  "Give every action on a letter key a twin by the Russian letter of the same key."
  (let ((map (gdext:singleton "InputMap")))
    (loop for (nil name) in *actions*
          do (loop for latin from 65 to 90
                   do (let ((ru (ru-of latin)))
                        (when (and ru (object-call (key-event :physical latin) "is_action" name nil)
                                   (not (object-call (key-event :keycode ru) "is_action" name nil)))
                          (object-call map "action_add_event" name (key-event :keycode ru))))))))

(defun action-name (key) (or (second (assoc key *actions*)) (error "Нет действия ~s" key)))

(defun make-action-names ()
  "Register the action labels for the settings menu (and a few pause menu words) in both languages."
  (dolist (locale '("ru" "en"))
    (let ((tr (gdext:add-reference (gdext:make-object "Translation"))))
      (godot:object-set tr "locale" locale)
      (loop for (nil name label) in *actions*
            do (object-call tr "add_message" (string-capitalize (substitute #\Space #\_ name))
                            (if (string= locale "ru") label (or (gethash label *en*) label)) ""))
      (loop for (en ru) in '(("Primary" "Основная") ("Secondary" "Запасная") ("Tertiary" "Третья") ("Pause" "Пауза")
                             ("Other language" "English"))
            do (object-call tr "add_message" en (cond ((string= locale "ru") ru)
                                                      ((string= en "Other language") "Русский")
                                                      (t en)) ""))
      (object-call (gdext:singleton "TranslationServer") "add_translation" tr))))

;;; ------------------------------------------------------------------ language

(defun locale-lang () (if (eql 0 (search "ru" (object-call (gdext:singleton "TranslationServer") "get_locale"))) :ru :en))

(defun set-lang (lang)
  "Switch the language to LANG on the fly: the menu, the labels already placed and everything said later."
  (setf *lang* lang)
  (object-call (gdext:singleton "TranslationServer") "set_locale" (if (eq lang :en) "en" "ru"))
  (ignore-errors (put-save :lang (list lang)))
  (when *root* (retranslate *root*)))

(defun init-lang ()
  "Set the language at startup: KIOSK_LANG, else the one chosen earlier, else the system or browser language."
  (let ((env (ext:getenv "KIOSK_LANG")) (saved (first (ignore-errors (get-save :lang)))))
    (setf *lang* (cond ((and env (plusp (length env))) (if (string= env "en") :en :ru))
                       ((member saved '(:ru :en)) saved)
                       ((string= (object-call (gdext:singleton "OS") "get_locale_language") "ru") :ru)
                       (t :en)))
    (object-call (gdext:singleton "TranslationServer") "set_locale" (if (eq *lang* :en) "en" "ru"))))

(defun step-lang ()
  "Pick up the menu language for the whole game: the language button in the pause menu changes it."
  (unless (eq (locale-lang) *lang*) (set-lang (locale-lang))))

(defun action-down (name)
  "Return true if the action NAME is held down.
While the lobby is open, keys go to its fields, not to the game."
  (and (not *lobby-open*) (not *map-open*) (not *look-open*) (not *show*)
       (object-call (gdext:singleton "Input") "is_action_pressed" (action-name name) nil)))

(defun action-p (event name)
  "Return true if EVENT is a key press of the action NAME."
  (object-call event "is_action_pressed" (action-name name) nil nil))

(defun toggle-sit ()
  "Sit down on the nearest seat or stand up."
  (if *seat*
      (setf *seat* nil)
      (let ((best (seat-near)))
        (when best
          (destructuring-bind (what x z yaw eye) best
            (declare (ignore what eye))
            (setf *seat* best *pos* (v3 x 0 z) *yaw* yaw *pitch* -.05))))))

;;; Where we stood or sat is remembered between runs (user://player.sexp)

(defvar *place-saved* nil)
(defvar *place-timer* 0.0)


(defun place-now ()
  (list (vector3-x *pos*) (vector3-z *pos*) *yaw* *pitch* *view* *seat*))

(defun save-place (delta)
  "Save the place once a second if something has changed.
The meta room and waiting at the bus stop are not remembered."
  (when (and (<= (decf *place-timer* delta) 0) (not *meta*) (member *view* '(:inside :outside))
             (not (outdoors-p (vector3-x *pos*) (vector3-z *pos*))))
    (setf *place-timer* 1.0)
    (let ((now (place-now)))
      (unless (equal now *place-saved*)
        (setf *place-saved* now)
        (ignore-errors (put-save :place now))))))

(defun load-place ()
  "Return to where we were last time; return T on success."
  (let ((p (get-save :place)))
    (when (and (fits p '(real real real real (:member :inside :outside) t))
               (or (null (sixth p)) (and (listp (sixth p)) (= (length (sixth p)) 5) (every #'realp (rest (sixth p))))))
      (destructuring-bind (x z yaw pitch view seat) p
        (setf *pos* (v3 x 0 z) *yaw* yaw *pitch* pitch *view* view *seat* seat *place-saved* p)
        (when (and (> z 1.9) (null seat)) (setf *door-open* t *door-k* 1.0) (apply-door))   ; we stood in the vestibule — the door is open
        t))))

(defun unstuck ()
  "Stand in the middle of the wagon when stuck in a wall, in things or behind the door."
  (unless (eq *view* :wait)
    (when *meta* (setf *meta* nil) (apply-meta))
    (when *build* (toggle-build))
    (setf *seat* nil *pos* (v3 0 0 .6) *yaw* 0.0 *pitch* 0.0 *view* :inside)
    (log-event :unstuck)
    (say "Ты в центре кибитки")))

(defun step-wait ()
  "Wait for the wagon at the bus stop: stand under the shelter and look around with the mouse."
  (node-3d-set-position *camera* (v3 5.3 1.5 1.2))
  (node-3d-set-rotation *camera* (v3 *pitch* *yaw* 0)))

(defun step-walk (delta)
  (save-place delta)
  (cond ((eq *view* :wait) (step-wait))
        ((outside-p) (step-outside delta))
        (t (step-inside delta))))

(defun step-inside (delta)
  (step-jump delta)
  (let* ((fx (- (sin *yaw*))) (fz (- (cos *yaw*))) (rx (cos *yaw*)) (rz (- (sin *yaw*)))
         (mx 0.0) (mz 0.0))
    (when (action-down :move-forward) (incf mx fx) (incf mz fz))
    (when (action-down :move-back) (decf mx fx) (decf mz fz))
    (when (action-down :move-right) (incf mx rx) (incf mz rz))
    (when (action-down :move-left) (decf mx rx) (decf mz rz))
    (let ((len (sqrt (+ (* mx mx) (* mz mz)))))
      (when (and *seat* (> len 0)) (setf *seat* nil))       ; started walking — stood up
      (when (> len 0)
        (let* ((x (vector3-x *pos*)) (z (vector3-z *pos*))
               (v (* *walk-speed* (boost-factor) (if (action-down :run) *run-factor* 1.0)))        ; Shift — run
               (nx (+ x (* (/ mx len) v delta))) (nz (+ z (* (/ mz len) v delta))))
          (cond ((walkable-p nx nz) (setf *pos* (v3 nx 0 nz)))     ; hit something — slide along the wall
                ((walkable-p nx z) (setf *pos* (v3 nx 0 z)))
                ((walkable-p x nz) (setf *pos* (v3 x 0 nz)))))))
    ;; we set off while standing on the ground at the gas station — we are taken inside
    (let ((x (vector3-x *pos*)) (z (vector3-z *pos*)))
      (when (and (outdoors-p x z) (not *meta*)
                 (or (> *speed* .6) (not (ground-walkable-p x z))))         ; set off or the ladder was raised
        (setf *pos* (v3 0 0 .6) *seat* nil)
        (say (if (> *speed* .6) "Кибитка тронулась — ты внутри" "Лесенку подняли — ты внутри"))))
    ;; the wagon floor is raised above the ground: we go down to the ground by the ladder
    (let* ((x (vector3-x *pos*)) (z (vector3-z *pos*))
           (down (cond (*meta* 0.0)
                       ((> (abs x) 1.75) 1.0)
                       (t (max 0.0 (min 1.0 (/ (- z 3.85) .7)))))))
      ;; the eye height is one for everything: both the camera and the aim (*EYE-NOW*). The aim used to be
      ;; computed 'from the wagon floor' while the outside camera stood on the ground: a metre of difference,
      ;; and things outdoors were almost impossible to hit
      (setf *eye-now* (+ (* *floor* (- 1 down)) (if *seat* (fifth *seat*) *eye*) *jump-y*
                         (if (and (not *meta*) (outdoors-p x z)) (ground-rise x z) 0.0)))        ; outside — along the ground
      (node-3d-set-position *camera* (v3 x *eye-now* z)))
    (when *torch-node* (object-set *torch-node* "visible" t))
    (node-3d-set-rotation *camera* (v3 *pitch* *yaw* 0))))

(defun fullscreen-p ()
  (= (object-call (gdext:singleton "DisplayServer") "window_get_mode") 3))

(defun set-fullscreen (on)
  "Switch to full screen (WINDOW_MODE_FULLSCREEN) if ON is true, else to a window."
  (object-call (gdext:singleton "DisplayServer") "window_set_mode" (if on 3 0)))

;;; Action presses. Every function returns T — the press has been handled.

(defun press-use () (use-thing) t)
(defun press-view () (unless *meta* (setf *view* (if (eq *view* :outside) :inside :outside))) t)
(defun press-map () (unless *meta* (open-map)) t)

(defvar *eye-now* 2.15 "The current eye height above the wagon bottom: the camera looks from it and the aim is computed from it.")
(defvar *jump-y* 0.0 "The current height above the floor or the ground during a jump, in metres.")
(defvar *jump-v* 0.0 "The upward jump speed, in m/s.")
(defparameter *jump-speed* 3.3 "The push-off speed: a cat jumps half a metre up.")

(defun press-jump ()
  (when (and (zerop *jump-y*) (not *seat*) (not (outside-p)) (not (eq *view* :wait))) (setf *jump-v* *jump-speed*))
  t)

(defun step-jump (delta)
  (when (or (plusp *jump-y*) (plusp *jump-v*))
    (decf *jump-v* (* 9.8 delta))
    (setf *jump-y* (max 0.0 (+ *jump-y* (* *jump-v* delta))))
    (when (zerop *jump-y*) (setf *jump-v* 0.0))))
(defun press-torch ()                                         ; Shift+F — under the muzzle
  (unless *meta* (toggle-torch (object-call (gdext:singleton "Input") "is_action_pressed" (action-name :run) nil)))
  t)
(defun press-build () (toggle-build) t)
(defun press-break () (unless (or *meta* (outside-p)) (break-crate)) t)
(defun press-spell ()
  (unless (and (not *meta*) (outdoors-p (vector3-x *pos*) (vector3-z *pos*))) (cast-spell))   ; no spellcasting from the ground at the gas station
  t)
(defun press-hide-hud () (setf *hud-hidden* (not *hud-hidden*) *hud-timer* 0.0) t)
(defun press-unstuck () (unstuck) t)

(defun handle-input (event)
  (when *look-open* (look-key event) (return-from handle-input nil))   ; the profile is open — the game does not listen to keys
  (when *scope* (scope-input event) (return-from handle-input nil))    ; looking through the big telescope
  (when *docs-open* (docs-key event) (return-from handle-input nil))   ; the documents folder is open
  (cond ((object-call event "is_class" "InputEventMouseMotion")
         (when (= (input-get-mouse-mode) 2)
           (let ((rel (object-get event "relative")))
             (cond ((outside-p)
                    (decf *orbit-yaw* (* (vector2-x rel) *mouse-sens* (if *zoom* .36 1.0)))
                    (setf *orbit-pitch* (max .04 (min 1.2 (+ *orbit-pitch* (* (vector2-y rel) *mouse-sens* (if *zoom* .36 1.0)))))))
                   (t
                    (decf *yaw* (* (vector2-x rel) *mouse-sens* (if *zoom* .36 1.0)))
                    (setf *pitch* (max -1.3 (min 1.3 (- *pitch* (* (vector2-y rel) *mouse-sens* (if *zoom* .36 1.0)))))))))))
        ((object-call event "is_class" "InputEventMouseButton")
         (when (object-get event "pressed")
           (let ((button (object-get event "button_index")))
             (cond ((/= (input-get-mouse-mode) 2) (input-set-mouse-mode 2))
                   ((and *build* (= button 1)) (place-item))    ; build mode: place a thing
                   ((and *build* (= button 2)) (remove-item))   ; build mode: remove a thing
                   ((and *act* (= button 1)))                    ; a booth activity is on: the click is its own (activities.lisp)
                   ((and *tool-up* (= button 1))                 ; the camera is at the eye — click
                    (when (eq *tool* :camera) (press-camera)))
                   ((= button 1) (hand-thing))))))   ; LMB on the stove, the record player or a crate
        ((object-call event "is_class" "InputEventKey")
         (when (and (object-get event "pressed") (not (object-get event "echo")))
           (let ((k (key-code event)))
             (cond ((= k +key-escape+) (input-set-mouse-mode 0))             ; fixed keys
                   ((= k +key-f2+) (open-lobby))
                   ((= k 73) (open-look))   ; I — the profile: clothes (look.lisp)
                   ((debug-key k))                                   ; F3 and jumps between places (dev.lisp)
                   ((= k +key-f9+) (cheat-fuel))
                   ((= k (1- +key-f9+)) (cheat-money))               ; F8
                   ((= k +key-f11+) (set-fullscreen (not (fullscreen-p))))
                   ((eq *view* :wait))                                 ; at the bus stop we only look
                   ((and *build* (build-key k)))                       ; build mode: R and the digits
                   (t (loop for (key nil nil handler) in *actions*     ; the rest goes by the action table
                            thereis (and handler (action-p event key) (funcall handler))))))))))
