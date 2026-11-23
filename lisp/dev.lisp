;;;; For development: the debug screen (F3) and the 'dev polygon', all booths in a row at the start of the
;;;; first town. The polygon is switched on at startup: KIOSK_DEV=1 for the desktop game, ?dev in the address
;;;; for the browser. It changes the booth layout, so playing together works only when everyone has it on.

(in-package #:kiosk-trip)

(defvar *debug-on* nil "True while the debug screen (F3) is shown.")
(defvar *debug-label* nil)
(defvar *dev-started* nil "True once the dev polygon has opened the debug screen and moved the wagon to the first booth.")
(defvar *poi* 0 "The index of the place last jumped to (PageUp/PageDown while the debug screen is open).")

(defun init-dev ()
  (setf *dev* (and (or (let ((e (ext:getenv "KIOSK_DEV"))) (and e (plusp (length e))))
                       (and (object-call (gdext:singleton "OS") "has_feature" "web")
                            (let ((q (ignore-errors (object-call (gdext:singleton "JavaScriptBridge") "eval" "location.search"))))
                              (when (stringp q)
                                (setf *dev-flags* (remove-if-not (lambda (f) (search f q)) '("nostart" "nopond" "nobare" "torture"))))
                              (and (stringp q) (search "dev" q)))))
                   t)))

;;; A REPL for the browser build in dev mode (?dev): the page puts Lisp text into window.__lisp (an array)
;;; and gets each answer back in window.__lispOut. It is how tools/web-eyes.py sets up a scene to film
;;; (the step js:). Only with ?dev: in a normal game nothing reads the page.

(defun step-js-repl ()
  (when (and *dev* (object-call (gdext:singleton "OS") "has_feature" "web"))
    (let ((text (ignore-errors (object-call (gdext:singleton "JavaScriptBridge") "eval"
                                            "(window.__lisp && window.__lisp.length) ? window.__lisp.shift() : ''"))))
      (when (and (stringp text) (plusp (length text)))
        (let ((answer (run-text text)))
          (ignore-errors
           (object-call (gdext:singleton "JavaScriptBridge") "eval"
                        (cl:format nil "(window.__lispOut = window.__lispOut || []).push(String.fromCodePoint(~{~d~^,~}))"
                                   (map 'list #'char-code answer)))))))))

;;; A torture test for the memory of the browser build (the page address with ?torture). The game's data
;;; are lists of numbers, keywords and strings held in local variables while more is allocated; if the
;;; collector cannot see a local, it frees what is still in use, and what comes back is garbage. Each round
;;; builds such lists, forces a collection in the middle and checks every element afterwards.

(defun torture-round (n seed)
  "Build lists of N entries, collect garbage while holding them only in locals, and return the number of damaged entries."
  (let* ((a (loop for i below n collect (list i (cl:format nil "line ~d" (+ i seed)) (* 1.5 i) (if (evenp i) :even :odd))))
         (b (mapcar (lambda (e) (cons (first e) (copy-seq (second e)))) a))
         (junk (loop for i below (* 4 n) collect (make-string 24 :initial-element #\x))))   ; pressure on the collector
    (si:gc t)
    (let ((c (mapcar (lambda (e) (list (third e) (fourth e))) a)) (bad 0))
      (setf junk nil)
      (si:gc t)
      (loop for e in a for p in b for q in c for i from 0
            do (unless (and (listp e) (eql (first e) i) (stringp (second e)) (string= (second e) (cl:format nil "line ~d" (+ i seed)))
                            (floatp (third e)) (= (third e) (* 1.5 i)) (eq (fourth e) (if (evenp i) :even :odd))
                            (consp p) (eql (car p) i) (stringp (cdr p)) (string= (cdr p) (second e))
                            (listp q) (eql (first q) (third e)) (eq (second q) (fourth e)))
                 (incf bad)))
      bad)))

(defun gc-torture (&optional (rounds 300) (n 400))
  "Run the torture ROUNDS times; print and return the number of rounds that came back damaged."
  (let ((damaged 0) (errors 0))
    (dotimes (r rounds)
      (handler-case (when (plusp (torture-round n r)) (incf damaged))
        (error (c) (incf errors) (when (< errors 4) (cl:format t "~&Memory torture, round ~d: ~a~%" r c)))))
    (cl:format t "~&Memory torture: rounds ~d, damaged ~d, with an error ~d~%" rounds damaged errors)
    (list damaged errors)))

(defun places-of-interest ()
  "Return everything worth checking by hand, in order along the road, as (path-coordinate . name) pairs."
  (flet ((en (text) (or (gethash text *en*) text)))        ; the debug screen is always in English
    (sort (append (loop for (d i) in *gimmicks* collect (cons d (en (third (find i *gimmick-plan* :key #'first)))))
                  (loop for tw in *towns* for i from 0 collect (cons (town-stop-d tw) (cl:format nil "market: ~a" (en (town-name i)))))
                  (loop for d in *stations* collect (cons d "gas station"))
                  (when *observatory* (list (cons *observatory* "observatory")))
                  (when *depot-d* (list (cons *depot-d* "scrap depot")))
                  (when *airmail-d* (list (cons *airmail-d* "airmail"))))
          #'< :key #'car)))

(defun jump-place (step)
  "Jump to the next (STEP 1) or the previous (STEP -1) place in the list.
Only the driver moves the wagon."
  (let ((list (places-of-interest)))
    (cond ((net-guest-p) (say "Переносит водитель"))                 ; shown to the player, so it goes through the translation
          (list (setf *poi* (mod (+ *poi* step) (length list)))
                (let ((p (nth *poi* list)))
                  (cancel-course) (halt)
                  (relocate (car p))
                  (say (cl:format nil "~d/~d · ~a · ~d m" (1+ *poi*) (length list) (cdr p) (round (car p)))))))))

(defun debug-text ()
  (let* ((tw (town-at *dist*)) (place (ignore-errors (walk-place)))
         (near (first (sort (copy-list (places-of-interest)) #'< :key (lambda (p) (abs (- (car p) *dist*)))))))
    (cl:format nil "F3 or ` — debug~:[~; · DEV POLYGON~] · F4 or \\ — cheats · PageUp/PageDown — jump between places
fps ~d · lang ~a · view ~a~@[ · mini-game ~a~]
path ~,1f m · ~a · speed ~,1f · dir ~a · lane ~,1f~@[ · autopilot → ~a~] · turns ahead ~d
town: ~a · nearest: ~a
cat: x ~,2f z ~,2f · eye ~,2f · carrying ~a · standing on ~a
day-t ~,0f (light ~,2f) · ~a · ~a · fuel ~,2f · money ~d
net: ~a~@[ · room ~a~] · peers ~d
scenes built ~d of ~d · land chunks ~d · solid cells ~d
mods: ~:[none~;~:*~{~a~^, ~}~] · stories ~d"
               *dev* (round (object-call (gdext:singleton "Engine") "get_frames_per_second")) *lang* *view*
               (and *act* (getf *act* :kind))
               *dist* (if (ring-d-p *dist*) "ring" "spoke through the Crossroads") *speed* *dir* *lane-now* (cdr *auto*) (length *way*)
               (if tw (let ((name (town-name (position tw *towns*)))) (or (gethash name *en*) name)) "—") (if near (cl:format nil "~a (~d m)" (cdr near) (round (- (car near) *dist*))) "—")
               (vector3-x *pos*) (vector3-z *pos*) *eye-now* (or (car *carry*) "—") (or (first place) "—")
               *day-t* (daylight *day-t*) *season* *weather* *fuel* *money*
               (or *net-role* "solo") *net-room* (length *net-peers*)
               (count-if #'scene-node *scenes*) (length *scenes*) (length *chunks*) (hash-table-count *solid-grid*)
               (mapcar (lambda (m) (if (eq (third m) :ok) (first m) (cl:format nil "~a FAILED" (first m)))) *mods*) (length *stories*))))

(defun toggle-debug ()
  (setf *debug-on* (not *debug-on*))
  (unless *debug-label*
    (let ((layer (gdext:make-object "CanvasLayer")) (bg (gdext:make-object "ColorRect")))
      (node-add-child *root* layer) (node-add-child layer bg)
      (godot:object-set layer "layer" 20)
      (godot:object-set bg "position" (vector2 4 4)) (godot:object-set bg "size" (vector2 420 124))
      (godot:object-set bg "color" (color 0 0 0 .62))
      (setf *debug-label* (hud-label bg "" 5 3 7 '(.75 1.0 .75)))
      (setf (getf (symbol-plist '*debug-label*) :bg) bg)))
  (godot:object-set (getf (symbol-plist '*debug-label*) :bg) "visible" *debug-on*))

(defun debug-key (k)
  "Handle the debug key K.
Return T when the key was ours."
  (cond ((or (= k (1+ +key-f2+)) (= k 96)) (toggle-debug) t)          ; F3, or the backquote key where the browser keeps F3 for itself
        ((or (= k (+ 2 +key-f2+)) (= k 92)) (toggle-cheats) t)        ; F4, or the backslash key
        ((and *debug-on* (= k 4194324)) (jump-place 1) t)            ; PageDown
        ((and *debug-on* (= k 4194323)) (jump-place -1) t)))         ; PageUp

(defun step-debug ()
  ;; the dev polygon greets you where the booths are: the debug screen is open and the wagon stands at the first one
  (when (and *dev* (not *dev-started*) (> *time* 1.0) (not *lobby-open*) (not (member "nostart" *dev-flags* :test #'string=)))
    (setf *dev-started* t)
    (unless *debug-on* (toggle-debug))
    (unless (net-guest-p) (setf *poi* -1) (jump-place 1)))
  (when (and *debug-on* *debug-label*)
    (godot:object-set *debug-label* "text" (or (ignore-errors (debug-text)) "debug: could not build the text"))))

;;; ------------------------------------------------------------------ cheats (F4)
;;; A panel of switches and buttons for testing: move fast, never run dry, jump between places, change
;;; the weather, get goods, start a booth game without walking up to it. It is in English, like the
;;; debug screen, and written past the translation (GODOT:OBJECT-SET).

(defvar *cheats-open* nil "True while the cheat panel is shown.")
(defvar *cheat-ui* nil "The cheat panel: a plist of :bg and :buttons, the latter (key . button) pairs.")
(defvar *cheat-on* '() "The switches that are on: a list of keys among :paws :truck :fuel :noon.")
(defparameter *cheat-plan*
  '((:paws "Fast paws x6" 60 78 :switch) (:truck "Fast truck x6" 60 104 :switch)
    (:fuel "Endless fuel" 60 130 :switch) (:noon "Always noon" 60 156 :switch)
    (:money "+5 rub" 60 196) (:tank "Full tank" 60 222) (:daily "Reset once-a-day limits" 60 248) (:planks "Planks +10" 60 274)
    (:next "Next place (PageDown)" 250 78) (:prev "Previous place (PageUp)" 250 104)
    (:weather "Weather: next" 250 130) (:season "Season: next" 250 156) (:night "Night / day" 250 182)
    (:milk "Give milk" 250 222) (:fish "Give fish" 250 248) (:coal "Give coal" 250 274)
    (:tir "Play: shooting range" 440 78) (:fishing "Play: fishing" 440 104) (:mine "Play: mine" 440 130) (:goat "Play: goat" 440 156)
    (:debt "Pay off the debt" 440 196) (:ending "Ending: play / stop" 440 222))
  "The cheat panel: entries of (key label x y [:switch]).")

(defun cheat-p (key) (and (member key *cheat-on*) t))

(defun cheat-label (key label)
  (if (member key '(:paws :truck :fuel :noon)) (cl:format nil "~a: ~:[off~;ON~]" label (cheat-p key)) label))

(defun make-cheats ()
  (let ((layer (gdext:make-object "CanvasLayer")) (bg (gdext:make-object "ColorRect")))
    (node-add-child *root* layer) (node-add-child layer bg)
    (godot:object-set layer "layer" 21)
    (godot:object-set bg "position" (vector2 40 40)) (godot:object-set bg "size" (vector2 560 280))
    (godot:object-set bg "color" (color .08 .1 .09 .94))
    (let ((title (hud-label bg "" 20 10 10 '(.75 1.0 .75))))
      (godot:object-set title "text" "CHEATS · F4 or \\ — close · for the driver or solo play"))
    (setf *cheat-ui*
          (list :bg bg
                :buttons (loop for (key label x y) in *cheat-plan*
                               collect (let ((b (lobby-control "Button" bg (- x 40) (- y 40) 180 22)))
                                         (godot:object-set b "toggle_mode" t)
                                         (godot:object-set b "text" (cheat-label key label))
                                         (cons key b)))))))

(defun toggle-cheats ()
  (unless *cheat-ui* (make-cheats))
  (setf *cheats-open* (not *cheats-open*))
  (godot:object-set (getf *cheat-ui* :bg) "visible" *cheats-open*)
  (input-set-mouse-mode (if *cheats-open* 0 2)))

(defun cheat (key)
  "Do what the button KEY of the cheat panel does."
  (case key
    ((:paws :truck :fuel :noon)
     (setf *cheat-on* (if (cheat-p key) (remove key *cheat-on*) (cons key *cheat-on*)))
     (case key
       (:paws (setf *walk-speed* (if (cheat-p :paws) 13.2 2.2)))
       (:truck (setf *max-speed* (if (cheat-p :truck) 54.0 9.0)))))
    (:money (cheat-money)) (:tank (cheat-fuel))
    (:planks (incf *planks* 10) (say (cl:format nil "Planks: ~d" *planks*)))
    (:debt (pay-debt *debt*) (say "The debt is paid off: the ticket is sold at the savings bank"))
    (:ending (toggle-cheats) (if *ending* (stop-ending) (start-ending)))
    (:daily (setf *gimmick-spent* '()) (say "Once-a-day limits are reset"))
    (:next (jump-place 1)) (:prev (jump-place -1))
    (:weather (set-weather (let ((all '(:clear :cloudy :rain :fog :snow))) (nth (mod (1+ (or (position *weather* all) 0)) 5) all)) 5))
    (:season (set-season (nth (mod (1+ (position *season* *seasons*)) 4) *seasons*)))
    (:night (setf *day-t* (if (> (daylight *day-t*) .5) 900.0 330.0)))
    (:milk (give-good :milk "Milk in your paws")) (:fish (give-good :fish "Fish in your paws")) (:coal (give-good :coal "Coal in your paws"))
    ((:tir :fishing :mine :goat) (toggle-cheats) (start-play (if (eq key :fishing) :fish key)))))

(defun step-cheats ()
  "Keep the switches in force and answer the buttons of the open panel."
  (unless (net-guest-p)
    (when (cheat-p :fuel) (setf *fuel* 1.0))
    (when (cheat-p :noon) (setf *day-t* 390.0)))
  (when *cheats-open*
    (loop for (key . b) in (getf *cheat-ui* :buttons)
          do (when (object-get b "button_pressed")
               (godot:object-set b "button_pressed" nil)
               (cheat key)
               (when *cheat-ui*
                 (godot:object-set b "text" (cheat-label key (second (assoc key *cheat-plan*)))))))))
