;;;; Sound. The buses are in project/default_bus_layout.tres (named in Russian there): 'Music' (the record
;;;; player), 'Wagon' (wheels, footsteps), 'Street' (wind, birds, river) - the player sets their volume
;;;; in the menu (Esc -> Settings). 'Street' goes through the hidden '_Walls' bus: the tighter
;;;; the shutters and curtains are closed, the quieter and duller it is; all closed - silence. Birds -
;;;; one voice at a time at random moments, by day and without rain; rain sounds follow the weather.
;;;; Wheels and footsteps on the planks are always heard. Sounds - project/sounds/ (tools/make-sounds.py);
;;;; files are read as bytes and handed to Godot streams without import.

(in-package #:kiosk-trip)

(defparameter *step-every* .48 "The distance in meters walked between two footstep sounds: a cat's stride is short.")
(defparameter *walls-bus* "_Стены" "The name of the hidden bus through which the wagon walls muffle the street.")

(defparameter *outside-noise* nil "Whether to play the constant street noise.")
(defvar *audio* '() "An alist from a name to its player: :outside :water :wheels :steps.")
(defvar *walls* nil "The index of the hidden walls bus.")
(defvar *lowpass* nil "The low-pass filter of the hidden walls bus.")
(defparameter *birds-on* nil "Whether the birds sing.
They are off: their chirping annoyed the author.")
(defparameter *bird-gap* '(8 40) "The shortest and longest pause between birds, in seconds.")
(defvar *bird-timer* 10.0)
(defvar *last-pos* nil)
(defvar *walked* 0.0)

;;; Files -> streams

(defun file-bytes (path)
  "Return the bytes of the game file at PATH (res://...).
Reading goes through the engine, which works both from disk and from the game package."
  (coerce (file-access-get-file-as-bytes path) '(simple-array (unsigned-byte 8) (*))))

(defun file-here-p (path) (file-access-file-exists path))

(defun save-form (path form)
  "Write FORM as text to the file PATH (user://...) and return T on success.
Writing goes through the engine: in the browser that puts the data into persistent storage,
while Lisp's ordinary file output does not."
  (let ((file (file-access-open path 2)))              ; FileAccess.WRITE
    (when file
      (object-call file "store_string" (with-standard-io-syntax (prin1-to-string form)))
      (object-call file "close")
      t)))

(defun load-form (path)
  "Read what SAVE-FORM wrote to PATH.
Return NIL if the file is missing or damaged."
  (when (file-here-p path)
    (ignore-errors
     (with-standard-io-syntax
       (let ((*read-eval* nil)) (values (read-from-string (file-access-get-file-as-string path))))))))

(defun path-type-p (path type)
  (let ((n (length path)) (k (length type)))
    (and (> n k) (string-equal type (subseq path (- n k))))))

(defun u16 (v i) (+ (aref v i) (ash (aref v (1+ i)) 8)))
(defun u32 (v i) (+ (u16 v i) (ash (u16 v (+ i 2)) 16)))

(defun wav-stream (path &key loop)
  "Build an AudioStreamWAV from the PCM file (8 or 16 bit) at PATH, looped if LOOP."
  (let ((v (file-bytes path)) (s (gdext:make-object "AudioStreamWAV")) (i 12) channels rate bits)
    (when (< (length v) 46)
      ;; an empty stream in the browser brings all sound down - hand back a short silence and say what was missing
      (format t "~&Звук не прочитан: ~a (~d байт)~%" path (length v))
      (object-set s "format" 1) (object-set s "mix_rate" 22050)
      (object-set s "data" (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0))
      (return-from wav-stream s))
    (loop while (< (+ i 8) (length v))
          do (let ((id (map 'string #'code-char (subseq v i (+ i 4)))) (size (u32 v (+ i 4))))
               (cond ((string= id "fmt ") (setf channels (u16 v (+ i 10)) rate (u32 v (+ i 12)) bits (u16 v (+ i 22))))
                     ((string= id "data")
                      (let ((data (subseq v (+ i 8) (min (length v) (+ i 8 size)))))
                        (when (= bits 8) (map-into data (lambda (b) (logxor b 128)) data))   ; 8-bit WAV is unsigned, Godot expects signed
                        (object-set s "format" (if (= bits 16) 1 0))
                        (object-set s "mix_rate" rate)
                        (object-set s "stereo" (= channels 2))
                        (object-set s "data" data)
                        (when loop
                          (object-set s "loop_mode" 1)
                          (object-set s "loop_end" (floor (length data) (* channels (/ bits 8))))))
                      (return)))
               (setf i (+ i 8 size (mod size 2)))))
    s))

(defun mp3-stream (path &key loop)
  (let ((s (gdext:make-object "AudioStreamMP3")))
    (object-set s "data" (file-bytes path))
    (object-set s "loop" (and loop t))
    s))

(defun audio-stream (path &key loop)
  (if (path-type-p path ".mp3") (mp3-stream path :loop loop) (wav-stream path :loop loop)))

(defun sound-path (name)
  (format nil "res://sounds/~a.wav" name))

;;; Buses and players

(defun walls-bus ()
  "Find the hidden walls bus and its low-pass filter."
  (let ((as (gdext:singleton "AudioServer")))
    (setf *walls* (object-call as "get_bus_index" *walls-bus*))
    (when (>= *walls* 0) (gdext:setf-reference *lowpass* (object-call as "get_bus_effect" *walls* 0)))))

(defun player (parent stream &key (db 0.0) (bus "Master") positional (x 0.0) (y 0.0) (z 0.0) (play t))
  (let ((p (gdext:make-object (if positional "AudioStreamPlayer3D" "AudioStreamPlayer"))))
    (object-set p "stream" stream)
    (object-set p "volume_db" db)
    (object-set p "bus" bus)
    (node-add-child parent p)
    (when positional
      (node-3d-set-position p (v3 x y z))
      (object-set p "unit_size" 2.5))
    (when play (object-call p "play"))
    p))

(defun make-sound (root)
  "Create the sounds under ROOT: street, river, wheels, footsteps.
Also load the discs from project/discs/ and make the hint."
  (dolist (p *audio*) (when (cdr p) (node-queue-free (cdr p))))
  (setf *audio* '())
  (walls-bus)
  (flet ((add (k p) (push (cons k p) *audio*)))
    ;; the steady street noise is off: it is tiring. Birds, rain and water stay; the volume of the
    ;; 'Street' bus is in the settings, on the 'Sound' tab
    (add :outside (player root (wav-stream (sound-path "outside") :loop t) :db -8.0 :bus "Улица" :play *outside-noise*))
    (add :water (player root (wav-stream (sound-path "water") :loop t) :db -80.0 :bus "Улица"))
    (add :wheels (player root (wav-stream (sound-path "wheels") :loop t) :db -12.0 :bus "Кибитка"))
    (let ((rnd (gdext:make-object "AudioStreamRandomizer")))
      (loop for i from 1 to 4 do (object-call rnd "add_stream" -1 (wav-stream (sound-path (format nil "step-~a" i))) 1.0))
      (object-set rnd "random_pitch" 1.15)
      (add :steps (player root rnd :db 0.0 :bus "Кибитка" :play nil)))
    (let ((rnd (gdext:make-object "AudioStreamRandomizer")))               ; outdoors the ground crunches instead
      (loop for i from 1 to 4 do (object-call rnd "add_stream" -1 (wav-stream (sound-path (format nil "gravel-~a" i))) 1.0))
      (object-set rnd "random_pitch" 1.2)
      (add :gravel (player root rnd :db -3.0 :bus "Master" :play nil)))
    (let ((rnd (gdext:make-object "AudioStreamRandomizer")))
      (loop for i from 1 to 6 do (object-call rnd "add_stream" -1 (wav-stream (sound-path (format nil "bird-~a" i))) 1.0))
      (object-set rnd "random_pitch" 1.08)
      (add :birds (player root rnd :db -18.0 :bus "Улица" :play nil)))
    (add :rain (player root (wav-stream (sound-path "rain") :loop t) :db -80.0 :bus "Улица")))
  (load-discs)
  (make-hint root))

(defun audio (k) (cdr (assoc k *audio*)))

(defun closedness ()
  "Return how closed the wagon is to the street: 0 with the window slid open, .6 with it shut."
  (* .6 (- 1.0 *window-k*)))                       ; window shut - the street sounds duller

(defun step-sound (delta)
  (when *audio*
    (let ((open (if *meta* 0.0 (- 1.0 (closedness)))))
      (object-call (gdext:singleton "AudioServer") "set_bus_volume_db" *walls*
                   (if (< open .02) -80.0 (* 20 (log (max open .001) 10))))
      (when *lowpass* (object-set *lowpass* "cutoff_hz" (+ 500.0 (* 19500 open open))))
      (let ((wet (max (weight :water :river *dist*) (weight :water :lake *dist*))))
        (object-set (audio :water) "volume_db" (if (< wet .02) -80.0 (+ -10 (* 20 (log wet 10))))))
      (let ((rain (wx :rain)))
        (object-set (audio :rain) "volume_db" (if (< rain .02) -80.0 (+ -8 (* 20 (log rain 10))))))
      (let ((w (audio :wheels)) (sp (abs *speed*)))
        (object-set w "volume_db" (if (or *meta* (< sp .1)) -80.0 (+ -24 (* 7 (min 1 (/ sp 7))))))   ; an engine hum is kept low: it never stops
        (object-set w "pitch_scale" (max .7 (min 1.5 (+ .7 (* .09 sp)))))))
    ;; birds: one at a time, out of step, by day and without rain or snow
    (when (<= (decf *bird-timer* delta) 0)
      (setf *bird-timer* (+ (first *bird-gap*) (random (float (- (second *bird-gap*) (first *bird-gap*))))))
      (when (and *birds-on* (> (daylight *day-t*) .35) (< (+ (wx :rain) (wx :snow)) .3) (not (eq *season* :winter)))
        (let ((b (audio :birds)))
          (object-set b "volume_db" (- -14.0 (random 12.0)))   ; nearer and farther
          (object-call b "play"))))
    ;; footsteps: a knock on the planks inside, a crunch on the ground outside
    (let ((p *pos*))
      (when (and *last-pos* (not *seat*))
        (let ((moved (sqrt (+ (expt (- (vector3-x p) (vector3-x *last-pos*)) 2) (expt (- (vector3-z p) (vector3-z *last-pos*)) 2)))))
          (when (< moved 1.0) (incf *walked* moved)))                        ; a jump across the wagon is not a walk
        (when (> *walked* *step-every*)
          (setf *walked* 0.0)
          (object-call (audio (if (and (not *meta*) (outdoors-p (vector3-x p) (vector3-z p))) :gravel :steps)) "play")))
      (setf *last-pos* p))
    (step-hint delta)
    (step-notes delta)))
