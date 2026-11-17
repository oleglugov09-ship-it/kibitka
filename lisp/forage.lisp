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

(defun grows-p (entry d)
  "Return true if the *FORAGE* ENTRY grows at path coordinate D right now."
  (destructuring-bind (good where seasons weathers count) entry
    (declare (ignore good count))
    (and (member *season* seasons)
         (or (null weathers) (member *weather* weathers))
         (some (lambda (kind) (if (eq kind :village) (village-stop-p d) (> (weight :veg kind d) .4))) where))))

(defun forage-spots (d)
  "Return what grows by the place at path coordinate D today: entries (good number x offset-along)."
  (let ((day (today)))
    (loop for entry in *forage* for k from 0
          when (grows-p entry d)
            append (loop for i below (fifth entry)
                         when (< (vr (+ d (* 13 k)) (+ day i)) .75)              ; not every day, not every stop
                           collect (list (first entry) (+ (* 100 day) (* 10 k) i)
                                         (+ 6.2 (* 3.2 (vr (+ d k) (+ 80 i)))) (- (* 9.0 (vr (+ i 11 k) d)) 4.5))))))

(defun forage-p (kind) (and (assoc kind *forage*) t))

(defun build-plant (g kind)
  "Build in G the plant that gives the good KIND, with what grows around it."
  (ecase kind
    ((:mushroom :toadstool)
     (shape g :cyl '(.16 .2 .22 8) "#5a4636" .5 .11 .3)                                             ; a stump
     (dotimes (i 5) (shape g :box '(.03 .3 .16) "#4a7a48" (+ .5 (* .3 (cos (* i 1.3)))) .12 (+ .3 (* .3 (sin (* i 1.3)))) :rot (list .5 (* i 1.3) 0) :shadow nil))
     (shape g :cyl '(.02 .028 .1 6) "#e8e0d0" 0 .05 0 :shadow nil)
     (shape g :ball '(.07 8 5) (if (eq kind :toadstool) "#c02a22" "#8a5a34") 0 .11 0 :scale '(1 .55 1))
     (when (eq kind :toadstool)
       (loop for (x z) in '((.03 .01) (-.02 .03) (-.01 -.035)) do (shape g :ball '(.012 4 3) "#f0ece0" x .143 z :shadow nil))))
    (:berry
     (loop for (x z s) in '((0 0 .22) (.5 .3 .3) (-.4 .5 .26))
           do (shape g :ball (list s 7 5) "#3c5a34" x (* s .6) z :scale '(1 .6 1))
              (dotimes (i 5) (shape g :ball '(.022 5 3) "#a3202c" (+ x (* s .7 (cos (* i 1.3)))) (+ (* s .7) (* .05 (sin (* i 2.1)))) (+ z (* s .7 (sin (* i 1.3)))) :shadow nil))))
    (:apple
     (shape g :cyl '(.09 .13 1.5 7) "#4a3424" .9 .75 0)                                              ; an apple tree; the low branch is ours
     (shape g :ball '(1.0 9 6) "#4a7a48" .9 2.1 0 :scale '(1 .75 1))
     (shape g :box '(.9 .05 .05) "#4a3424" .45 1.25 0 :rot '(0 0 -.25))
     (shape g :ball '(.3 7 5) "#4a7a48" 0 1.2 0 :scale '(1 .7 1) :shadow nil)
     (dotimes (i 7) (shape g :ball '(.045 6 4) (if (evenp i) "#c8382c" "#d8a030") (+ .9 (* .8 (cos (* i .9)))) (+ 1.75 (* .3 (sin (* i 1.7)))) (* .8 (sin (* i .9))) :shadow nil))
     (dotimes (i 3) (shape g :ball '(.045 6 4) "#c8382c" (* .12 (cos (* i 2.1))) 1.08 (* .12 (sin (* i 2.1))) :shadow nil))
     (dotimes (i 3) (shape g :ball '(.04 6 4) "#b8862c" (+ .6 (* .25 i)) .04 (- (* .3 i) .3) :shadow nil)))   ; windfalls
    (:nut
     (shape g :ball '(.7 8 6) "#4a6a38" 0 .9 0 :scale '(1 1.1 1))                                    ; a hazel bush
     (dotimes (i 4) (shape g :cyl '(.02 .03 1.0 5) "#4a3424" (* .15 (cos (* i 1.6))) .5 (* .15 (sin (* i 1.6))) :rot (list (* .2 (sin i)) 0 (* .2 (cos i)))))
     (dotimes (i 6) (shape g :ball '(.03 6 4) "#8a5a34" (* .55 (cos (* i 1.1))) (+ .75 (* .2 (sin (* i 2.3)))) (* .55 (sin (* i 1.1))) :shadow nil)))
    (:herb
     (dotimes (i 9)
       (let ((x (* .5 (cos (* i 2.4)) (/ i 9.0))) (z (* .5 (sin (* i 2.4)) (/ i 9.0))))
         (shape g :box '(.012 .28 .012) "#5a7a3a" x .14 z :rot (list (* .1 (sin i)) 0 (* .1 (cos i))) :shadow nil)
         (shape g :ball '(.025 5 3) (nth (mod i 3) '("#9a6ab8" "#e8d060" "#f0ece0")) x .29 z :shadow nil))))))

(defun plant-height (kind) "Return the height at which the plant of KIND is picked." (case kind (:apple 1.1) (:nut .8) (t .12)))

(defun forage-label (kind)
  (ecase kind
    ((:mushroom :toadstool) "E — сорвать гриб")
    (:berry "E — набрать клюквы")
    (:apple "E — сорвать яблоки")
    (:herb "E — нарвать трав")
    (:nut "E — набрать орехов")))

(defun forage-text (kind)
  (ecase kind
    (:mushroom "Грибы в лапах. На сковородку — или продать там, где их ищут. К утру испортятся: в холодильник или на сушилку")
    (:toadstool "Грибы в лапах. Красные, в белую точку… точно съедобные?")
    (:berry "Клюква в лапах. На сковородке из неё выйдет варенье")
    (:apple "Яблоки в лапах. С хлебом на сковородке — шарлотка, в чайник с водой — компот")
    (:herb "Травы в лапах. В чайник с водой — будет травяной чай: бодрит")
    (:nut "Орехи в лапах. С мёдом на сковородке — козинаки")))

(defun forget-old-plants ()
  "Drop from the list of picked finds the plants of days gone by: they have grown again anyway."
  (let ((day (today)))
    (setf *picked* (remove-if (lambda (id) (and (forage-p (second id)) (< (floor (third id) 100) day))) *picked*))))
