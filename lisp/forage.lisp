;;;; Foraging: what grows by the bus stops. Mushrooms in a wood in damp weather (and toadstools among them),
;;;; cranberries on the marsh, apples by a village, herbs in a meadow; in winter nothing grows. What grows
;;;; where is content: the mod's *FORAGE* table (its shape is described in economy.lisp).
;;;;
;;;; A plant is a find like a bottle (finds.lisp): it stands at a spot that is the same for all players,
;;;; E picks it, and once picked it is gone for everyone. Its number carries the game day, so the next day
;;;; it has grown again; a good picked goes into the paws. Around each spot stands what makes the roadside
;;;; richer whether one picks or not: a stump with ferns, bushes, an apple tree, a patch of flowers.

(in-package #:kiosk-trip)

(defun village-stop-p (d)
  "Return true if the place at path coordinate D belongs to a village town or lies within 700 m of one."
  (some (lambda (tw) (and (eq (town-theme tw) :village) (< (- (town-start tw) 700.0) d (+ (town-end tw) 700.0)))) *towns*))

(defvar *finds-day* nil "What the finds by the road are laid out for: (day season weather).")

(defun grows-p (entry d &optional any-weather)
  "Return true if the *FORAGE* ENTRY grows at path coordinate D right now; with ANY-WEATHER, whatever the weather."
  (destructuring-bind (good where seasons weathers count) entry
    (declare (ignore good count))
    (and (member *season* seasons)
         (or any-weather (null weathers) (member *weather* weathers))
         (some (lambda (kind) (if (eq kind :village) (village-stop-p d) (> (weight :veg kind d) .4))) where))))

(defun forage-spots (d &optional any-weather)
  "Return what grows by the place at path coordinate D today: entries (good number x offset-along).
The number is 100·day + 10·(place of the good in *FORAGE*) + i: at most ten goods, at most ten plants of each."
  (let ((day (today)))
    (loop for entry in *forage* for k from 0
          when (grows-p entry d any-weather)
            append (loop for i below (fifth entry)
                         when (< (vr (+ d (* 13 k)) (+ day i)) .75)              ; not every day, not every stop
                           collect (list (first entry) (+ (* 100 day) (* 10 k) i)
                                         (+ 6.2 (* 3.2 (vr (+ d k) (+ 80 i)))) (- (* 9.0 (vr (+ i 11 k) d)) 4.5))))))

(defun forage-p (kind) (and (assoc kind *forage*) t))

(defun plant-there-p (d kind i)
  "Return true if the plant KIND number I really grows today at the place whose path coordinate rounds to D.
The driver asks this before giving the good: a plant of yesterday, or one that a guest's message made up,
gives nothing. The weather is not asked: it may have just changed, and the plant still stands on the screen."
  (let ((place (find-if (lambda (p) (< (abs (- p d)) 1.0)) (find-places))))
    (and place (integerp i)
         (find-if (lambda (s) (and (eq (first s) kind) (eql (second s) i))) (forage-spots place t))
         t)))

(defun plant-model (kind)
  "Return the name of the model of the plant that gives KIND (models/plant-KIND.blend); a mod's plant is a bush."
  (format nil "plant-~(~a~)" (if (member kind '(:mushroom :toadstool :berry :apple :nut :herb)) kind :bush)))

(defun build-plant (g kind)
  "Build in G the plant that gives the good KIND, with what grows around it."
  (model g (plant-model kind)))

(defun plant-height (kind) "Return the height at which the plant of KIND is picked." (case kind (:apple 1.1) (:nut .8) (t .12)))

(defun forage-label (kind)
  (case kind
    ((:mushroom :toadstool) "E — сорвать гриб")
    (:berry "E — набрать клюквы")
    (:apple "E — сорвать яблоки")
    (:herb "E — нарвать трав")
    (:nut "E — набрать орехов")
    (t "E — сорвать")))

(defun forage-text (kind)
  (case kind
    (:mushroom "Грибы в лапах. На сковородку — или продать там, где их ищут. К утру испортятся: в холодильник или на сушилку")
    (:toadstool "Грибы в лапах. Красные, в белую точку… точно съедобные?")
    (:berry "Клюква в лапах. На сковородке из неё выйдет варенье")
    (:apple "Яблоки в лапах. С хлебом на сковородке — шарлотка, в чайник с водой — компот")
    (:herb "Травы в лапах. В чайник с водой — будет травяной чай: бодрит")
    (:nut "Орехи в лапах. С мёдом на сковородке — козинаки")
    (t "Сорвал. В лапах")))

(defun forget-old-plants ()
  "Drop from the list of picked finds the plants of days gone by: they have grown again anyway."
  (let ((day (today)))
    (setf *picked* (remove-if (lambda (id) (and (forage-p (second id)) (or (not (integerp (third id))) (< (floor (third id) 100) day)))) *picked*))))
