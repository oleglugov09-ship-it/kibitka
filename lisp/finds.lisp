;;;; Finds and the places where you get out of the wagon. Bottles and bundles of waste paper lie around
;;;; at bus stops and gas stations: you pick them up (E) and hand them in at the recycling depot in the
;;;; industrial town. In the mountains an observatory stands by the road: a spyglass lies there and a big
;;;; telescope stands there (the sky is stars.lisp). You can get out where *WALK-HERE* is set: gas station,
;;;; bus stop, observatory, recycling depot. Everything lying outside is given in road coordinates (sideways,
;;;; path coordinate, height above the road) and is converted to wagon coordinates for the crosshair (ROAD-USABLE).

(in-package #:kiosk-trip)

(defvar *bottles* 0 "The number of bottles being carried.")
(defvar *papers* 0 "The number of waste paper bundles being carried.")
(defvar *spyglass* nil "True when the spyglass has been found.")
(defvar *picked* '() "The finds already picked up: entries (place kind number).")
(defvar *find-nodes* '() "The finds nearby: entries ((place kind number) node x path-coordinate).")
(defvar *stop-ds* '() "The path coordinates of all bus stops: finds lie by them and you can get out there.")
(defvar *observatory* nil "The path coordinate of the observatory.")
(defvar *depot-d* nil "The path coordinate of the recycling depot.")
(defvar *walk-here* nil "True when the wagon stands where you can get out.
Such places are a gas station, a bus stop, the observatory and the recycling depot.")

(defun save-finds ()
  "Write the finds to the save. A guest does not: what it holds of them is the driver's."
  (unless (net-guest-p) (put-save :finds (list *bottles* *papers* (and *spyglass* t) *picked*))))

(defun load-finds ()
  (let ((f (get-save :finds)))
    (when (fits f '(integer integer t list))
      (setf *bottles* (first f) *papers* (second f) *spyglass* (and (third f) t) *picked* (fourth f)))))

(defstate :finds
  :clear (lambda () (setf *bottles* 0 *papers* 0 *spyglass* nil *picked* '()))
  :load #'load-finds :save #'save-finds :show (lambda () (show-spyglass)))

(defun road-usable (x d up label action &optional (reach 2.6) (radius .35))
  "Return a usable for an item outside, converted to wagon coordinates for the crosshair.
The item is X sideways from the road axis, at path coordinate D, UP above the road; LABEL, ACTION
and REACH are passed on to USABLE. RADIUS is the target size: a booth window is large, a bottle
is small."
  (destructuring-bind (wx wy wz) (road-pos d x)
    (multiple-value-bind (lx lz) (cart-local wx wz)
      (usable lx (+ (- wy (road-y *dist*)) up (- *floor*)) lz label action nil reach radius))))

;;; ------------------------------------------------------------------ where you can get out

(defun walk-place ()
  "Return what the wagon stands by as (kind path-coordinate), or NIL.
The kind is one of :station, :stop, :observatory, :depot, :airmail or :gimmick."
  (flet ((near (d r) (and d (< (abs (- d *dist*)) r))))
    (when (< *speed* .4)
      (let ((station (find-if (lambda (d) (near d 30)) *stations*))
            (stop (find-if (lambda (d) (near d 16)) *stop-ds*)))
        (cond (station (list :station station))
              ((near *observatory* 34) (list :observatory *observatory*))
              ((near *depot-d* 30) (list :depot *depot-d*))
              ((near *airmail-d* 30) (list :airmail *airmail-d*))
              ((find-if (lambda (g) (near (first g) 26)) *gimmicks*)        ; several booths may be nearby: take the nearest
               (let ((g (first (sort (remove-if-not (lambda (g) (near (first g) 26)) *gimmicks*) #'<
                                     :key (lambda (g) (abs (- (first g) *dist*)))))))
                 (list :gimmick (first g) (second g))))
              (stop (list :stop stop)))))))


;;; ------------------------------------------------------------------ bottles and waste paper

(defun find-spots (d)
  "Return what lies by the place at path coordinate D: entries (kind number x offset-along).
Each place has its own set, the same for all players."
  (append (loop for i below (floor (* 3.6 (vr d 51))) collect (list :bottle i (+ 5.4 (* 3.0 (vr d (+ 60 i)))) (- (* 5.0 (vr i (+ d 3))) 2.5)))
          (loop for i below (floor (* 2.4 (vr d 52))) collect (list :paper i (+ 5.6 (* 2.6 (vr d (+ 70 i)))) (- (* 5.0 (vr (+ i 7) d)) 2.5)))
          (forage-spots d)))                                              ; what grows here today (forage.lisp)

(defun build-find (kind x d)
  (destructuring-bind (px py pz) (side-point x d 0.02)
    (let ((g (group *world* px py pz)))
      (node-3d-set-rotation g (v3 0 (* 6 (vr x d)) 0))
      (case kind
        ((:mushroom :toadstool :berry :apple :herb :nut) (build-plant g kind))
        (:bottle (model g "find-bottle"))
        (:paper (model g "find-paper")))
      g)))

(defun find-places ()
  "Return the path coordinates of the places where finds lie: the bus stops and the gas stations."
  (append *stop-ds* (mapcan (lambda (tw) (copy-list (town-stations tw))) *towns*)))

(defun step-finds ()
  "Update the finds by the nearby places: build them, and remove the far ones."
  (let ((day (list (today) *season* *weather*)))                    ; a new day or new weather: what grows has changed, lay the places out anew
    (unless (equal day *finds-day*)
      (setf *finds-day* day)
      (dolist (f *find-nodes*) (node-queue-free (second f)))
      (setf *find-nodes* '())))
  (let ((near (remove-if-not (lambda (d) (< (abs (- d *dist*)) 70)) (find-places))))
    (setf *find-nodes* (remove-if (lambda (f) (when (or (not (member (first (first f)) near))            ; drove away, or someone picked it up
                                                        (member (list (round (first (first f))) (second (first f)) (third (first f)))
                                                                *picked* :test #'equal))
                                                (node-queue-free (second f)) t))
                                  *find-nodes*))
    (dolist (d near)
      (unless (or (find d *find-nodes* :key (lambda (f) (first (first f)))) (bare-at d))
        (loop for (kind i x along) in (find-spots d)
              do (let ((id (list (round d) kind i)))
                   (unless (member id *picked* :test #'equal)
                     (push (list (list d kind i) (build-find kind x (+ d along)) x (+ d along)) *find-nodes*))))
        (push (list (list d :none 0) (group *world*) 0 d) *find-nodes*)))))      ; a marker: 'this place is already laid out'

(defshared pick-find :find (d kind i)
  "Pick up a find: the place D (an integer), its KIND and its number I.
The action is shared: once one player picks it up, it is gone for everyone."
  (let ((id (list (round d) kind i)))
    (cond ((member id *picked* :test #'equal) (say "Это уже подобрали"))
          ((forage-p kind)                                               ; a plant: the good goes into the paws
           (when (cond ((not (plant-there-p d kind i)) (say "Здесь уже ничего не растёт") nil)   ; yesterday's plant, or one a guest made up
                       (t (give-good kind (forage-text kind))))
             (push id *picked*)
             (forget-old-plants)
             (save-finds)
             (log-event :foraged :kind kind :dist (round *dist*))))
          ((not (member kind '(:bottle :paper))) (say "Это уже подобрали"))
          (t (push id *picked*)
             (if (eq kind :bottle) (incf *bottles*) (incf *papers*))
             (save-finds)
             (say (format nil "~a. Несём: бутылок ~d, макулатуры ~d — сдать можно в Красном Куте"
                          (if (eq kind :bottle) "Подобрал бутылку" "Подобрал связку макулатуры") *bottles* *papers*))))))

(defshared take-spyglass :spyglass ()
  (cond (*spyglass* (say "Столик астронома"))
        (t (setf *spyglass* t) (save-finds) (show-spyglass)
           (say-all "Подзорная труба! Висит в кибитке у двери, над выключателем — E по ней, и смотри на звёзды"))))

(defshared sell-finds :sell ()
  (cond ((zerop (+ *bottles* *papers*)) (say "Сдавать нечего: бутылки и макулатура лежат у остановок и заправок"))
        (t (let ((sum (+ (* *bottle-price* *bottles*) (* *paper-price* *papers*))))
             (incf *money* sum)
             (log-event :recycled :bottles *bottles* :papers *papers* :money *money*)
             (say-all (format nil "Сдали ~d бутылок и ~d связок: +~/kiosk-trip::rub/" *bottles* *papers* sum))
             (note-quest :clean (+ *bottles* *papers*))
             (setf *bottles* 0 *papers* 0)
             (save-finds)))))

;;; ------------------------------------------------------------------ observatory and recycling depot

(defun build-observatory (d)
  "Build the observatory by the mountain road at path coordinate D.
It has a dome, a terrace with the big telescope and a small table with the spyglass.
Return the group node."
  (let ((g (apply #'group *world* (road-pos d 3.2 -.35))))
    (node-3d-set-rotation g (v3 0 (- (road-frame d)) 0))
    (model g "observatory")
    (sign g "ОБСЕРВАТОРИЯ" 11.9 3.4 4.0 :h .6 :rgb "#2c4a7a")
    ;; the big telescope on the terrace
    (unless *spyglass*
      (shape g :cyl '(.035 .05 .5 8) "#c9a13a" 7.0 .9 6.0 :rot (list (/ pi 2) .5 0) :metal .7 :rough .3))
       ; a red lamp: it does not dazzle the eyes
    (flood g 9.0 2.0 0 :rgb "#ff6050" :energy 1.2 :range 18.0)
    g))

(defun build-scrap-depot (d)
  "Build the glass and waste paper recycling depot at path coordinate D.
It has a booth with a window, crates of bottles, bales of paper and scales. Return the group node."
  (let ((g (apply #'group *world* (road-pos d 3.2 -.35))))
    (node-3d-set-rotation g (v3 0 (- (road-frame d)) 0))
    (model g "scrap-depot")
    (sign g "ПРИЁМ СТЕКЛОТАРЫ" 9.78 3.9 0 :h .42 :rgb "#e8e0d0") (sign g "И МАКУЛАТУРЫ" 9.78 3.45 0 :h .42 :rgb "#e8e0d0")
    g))

(defun finds-usables ()
  "Return the usables for what can be pressed outside at the place where the wagon stands."
  (let ((place (walk-place)))
    (append
     (loop for f in *find-nodes*
           unless (eq (second (first f)) :none)
             collect (let ((f f))
                       (road-usable (third f) (fourth f) (plant-height (second (first f)))
                                    (lambda () (case (second (first f))
                                                 (:bottle "E — подобрать бутылку") (:paper "E — подобрать макулатуру")
                                                 (t (forage-label (second (first f))))))
                                    (lambda () (pick-find (round (first (first f))) (second (first f)) (third (first f)))))))
     (when (and place (eq (first place) :observatory))
       (let ((d (second place)))
         (list (road-usable 11.2 (+ d 6.0) 1.5
                            (lambda () (if (< (daylight (sky-now)) .3) "E — посмотреть в большой телескоп" "Большой телескоп. Днём звёзд не видно — приходи ночью"))
                            #'open-scope)
               (road-usable 10.2 (- d 6.0) .6
                            (lambda () (if *spyglass* "Столик астронома" "E — взять подзорную трубу"))
                            #'take-spyglass))))
     (when (and place (eq (first place) :depot))
       (list (road-usable 12.9 (second place) 1.3
                          (lambda () (format nil "E — сдать бутылки (~d) и макулатуру (~d): +~/kiosk-trip::rub/"
                                             *bottles* *papers* (+ (* *bottle-price* *bottles*) (* *paper-price* *papers*))))
                          #'sell-finds 3.2 .7))))))
