;;;; Things in the wagon: parcels, milk, barrels, sacks, a lamp. A thing is a record (kind x height z spin
;;;; [town-number]); with a town number it is a parcel. They stand on the floor, on the rack, on the
;;;; windowsills and on each other; take away a support and what was on top comes down (SETTLE-ITEMS). The
;;;; arrangement is kept between runs in user://interiorN.sexp (N grows when the starting one changes).
;;;;
;;;; Goods and parcels are carried in the paws (trade.lisp). Build mode (B) is for furnishings: 1-9 and 0 -
;;;; the thing, R - rotate, LMB - put it where we look, RMB - remove.

(in-package #:kiosk-trip)

(defparameter *kinds*
  '((:crate "ящик" (.8 .45 .85)) (:box "ящик малый" (.5 .45 .5)) (:mattress "матрац" (.8 .14 1.7))
    (:pillow "подушка" (.5 .13 .34)) (:blanket "накидка" (.86 .05 1.15)) (:rug "половик" (.9 .02 1.4))
    (:lamp "лампа" (.2 .34 .2)) (:samovar "самовар" (.3 .5 .3)) (:sack "мешок" (.55 .32 .4))
    (:barrel "бочка" (.56 .72 .56))
    ;; goods: build mode does not offer them (there are not enough keys, nor should there be) - they are carried in the paws, see trade.lisp
    (:milk "молоко" (.12 .22 .12)) (:blin "блины" (.2 .1 .2)))
  "The things that can be placed, each as (kind name (width height depth)).")
(defvar *items* '() "The things standing in the wagon, each as ((kind x y z spin [town-number]) . node).
A thing with a town number is a parcel.")
(defvar *build* nil "Whether build mode is on.")
(defvar *pick* 0 "The index in *KINDS* of the chosen thing.")
(defvar *spin* 0 "The rotation of the blank, in quarter turns.")
(defvar *ghost* nil "The translucent blank shown where the thing will stand.")
(defvar *ghost-key* nil)
(defvar *build-hint* "" "The text to show under the crosshair in build mode.")

(defvar *start-interior* '()
  "The things we start with, from mods: entries of (kind x bottom-height z quarter-turns). The boxes get
addresses on the first load; new parcels are brought to the window (trade.lisp).")

(defun item-size (kind spin)
  "Return the width (x), height and depth (z) of a thing of KIND turned by SPIN quarter turns."
  (destructuring-bind (w h d) (third (assoc kind *kinds*))
    (if (oddp spin) (values d h w) (values w h d))))

(defun build-item (parent kind x y z spin &optional addr props)
  "Build a thing of KIND under PARENT at X and Z, its bottom at height Y above the floor.
SPIN is its rotation in quarter turns, ADDR the town number on the side of a crate, PROPS its properties.
Return the node."
  (let ((g (group parent x (+ *floor* y) z)) (wood "#8a6440") (dark "#5a3e26"))
    (node-3d-set-rotation g (v3 0 (* spin (/ pi 2)) 0))
    (ecase (if (and (good-p kind) (not (member kind '(:milk :blin)))) :good kind)   ; milk and blini have clauses of their own
      ((:crate :box)
       (destructuring-bind (w h d) (third (assoc kind *kinds*))
         (shape g :box (list w h d) wood 0 (/ h 2) 0)
         (dolist (sx '(-1 1))                                   ; slats at the corners and across - a crate, not a cube
           (dolist (sz '(-1 1)) (shape g :box (list .06 (+ h .01) .06) dark (* sx (- (/ w 2) .02)) (/ h 2) (* sz (- (/ d 2) .02))))
           (shape g :box (list .03 .08 (+ d .02)) dark (* sx (/ w 2)) (* h .72) 0)
           (shape g :box (list .03 .08 (+ d .02)) dark (* sx (/ w 2)) (* h .28) 0))
         (if addr                                                 ; a parcel: the address on all four sides, top and bottom -
             (let ((town (parcel-town addr)))                     ; the town's stamp: seen however it stands and from whichever shelf you look
               (let ((from (parcel-from addr)))                   ; a story parcel: the crate says who it is from, with a wax seal
                 (when from
                   (dolist (sz '(-1 1))
                     (sign g (format nil "от: ~a" from) 0 (* h .28) (* sz (+ (/ d 2) .012)) :h .055 :rgb "#7a2a22" :yaw (if (> sz 0) 0.0 pi)))
                   (shape g :cyl '(.04 .04 .012 8) "#a3262c" (* w .3) (+ h .008) (* d .25) :shadow nil)))
               (dolist (sz '(-1 1))
                 (stamp g town 0 (* h .56) (* sz (+ (/ d 2) .012)) .24 :yaw (if (> sz 0) 0.0 pi)))
               (dolist (sx '(-1 1))
                 (stamp g town (* sx (+ (/ w 2) .012)) (* h .56) 0 .24 :yaw (* sx (/ pi 2))))
               (stamp g town 0 (+ h .012) 0 .24 :pitch (- (/ pi 2)))
               (stamp g town 0 -.012 0 .24 :pitch (/ pi 2)))
             (shape g :box (list (* w .45) .12 .012) "#2a221c" 0 (* h .55) (/ d 2) :shadow nil))))
      (:mattress
       (shape g :box '(.8 .14 1.7) "#d8d2c0" 0 .07 0)
       (loop for i below 5 do (shape g :box '(.035 .143 1.702) "#5a6a8a" (- (* i .17) .34) .07 0 :shadow nil)))
      (:pillow (shape g :ball '(.2 8 5) "#f0e8d8" 0 .08 0 :scale '(1.25 .42 .85)))
      (:blanket
       (shape g :box '(.86 .05 1.15) "#7a3a34" 0 .025 0)
       (dolist (sx '(-1 1)) (shape g :box '(.03 .3 1.15) "#7a3a34" (* sx .43) -.12 0))
       (shape g :box '(.87 .052 .1) "#d8a840" 0 .025 .38 :shadow nil))
      (:rug
       (shape g :box '(.9 .02 1.4) "#8a1a22" 0 .01 0 :shadow nil)
       (shape g :box '(.72 .024 1.22) "#e0b050" 0 .012 0 :shadow nil)
       (shape g :box '(.58 .028 1.08) "#8a1a22" 0 .014 0 :shadow nil))
      (:lamp
       (shape g :cyl '(.07 .09 .08 8) "#6a5a30" 0 .04 0 :metal .6 :rough .4)
       (shape g :cyl '(.05 .065 .16 8) "#ffe0a0" 0 .17 0 :emit "#ffb050" :energy 2.5 :shadow nil)
       (shape g :cyl '(.03 .05 .07 8) "#6a5a30" 0 .29 0 :metal .6 :rough .4)
       (light g "#ffb070" 1.0 3.0 0 .25 0))
      (:samovar
       (shape g :cyl '(.11 .1 .08 12) "#d8a040" 0 .04 0 :metal .85 :rough .3)
       (shape g :cyl '(.15 .11 .16 12) "#d8a040" 0 .16 0 :metal .85 :rough .3)
       (shape g :cyl '(.14 .16 .16 12) "#d8a040" 0 .32 0 :metal .85 :rough .3)
       (shape g :cyl '(.06 .13 .1 12) "#d8a040" 0 .45 0 :metal .85 :rough .3)
       (shape g :box '(.14 .025 .025) "#d8a040" .2 .22 0 :metal .85 :rough .3 :rot (list 0 0 -.3)))
      (:milk
       (shape g :box '(.12 .17 .12) "#f0ede4" 0 .085 0)
       (shape g :box '(.128 .06 .128) "#3a6aa8" 0 .06 0 :shadow nil)       ; the label sticks out 4 mm: any closer and it flickers
       (shape g :box '(.12 .06 .02) "#f0ede4" 0 .19 0 :shadow nil)
       (shape g :box '(.03 .04 .03) "#d8d2c0" .03 .2 0 :shadow nil))
      (:good
       (cond ((and (getf props :cut) (cut-look kind)) (build-cut g kind))
             ((member kind *modelled*) (build-good g kind))
             (t (build-packet g kind)))                              ; a mod's good without a model of its own
       (when (getf props :shop) (shape g :box '(.045 .03 .004) "#f0ede4" .05 .03 .075 :shadow nil)))   ; a shop's price tag    ; cut on the board: slices   ; the towns' goods - goods.lisp
      (:blin
       (shape g :cyl '(.1 .1 .012 12) "#e4e0d4" 0 .006 0)
       (dotimes (i 5) (shape g :cyl (list (- .08 (* i .003)) .08 .014 10) (if (evenp i) "#d8a040" "#c88a30") (* .004 (- i 2)) (+ .02 (* i .014)) 0 :shadow nil))
       (shape g :box '(.03 .012 .03) "#f0e8b0" 0 .092 0 :shadow nil))
      (:sack
       (shape g :ball '(.26 8 5) "#a08a5a" 0 .16 0 :scale '(1.05 .62 .78))
       (shape g :box '(.1 .1 .1) "#8a744a" .24 .26 0 :rot '(0 0 .5)))
      (:barrel
       (shape g :cyl '(.26 .26 .72 12) "#6a5038" 0 .36 0)
       (dolist (y '(.12 .36 .6)) (shape g :cyl '(.275 .275 .04 12) "#34363a" 0 y 0 :metal .5 :rough .5 :shadow nil))))
    g))

;;; Storage


(defun save-interior ()
  (when (net-guest-p) (return-from save-interior))          ; a guest's things are the host's - leave our own file alone
  ;; what is in the paws right now is saved as standing where it was taken from (or by the window): quit the game - nothing lost
  (ignore-errors
   (put-save :interior
              (append (mapcar #'car *items*) (when *carry* (list (carry-spec)))))))

(defun spec-ok-p (it)
  "Return true if the thing IT is recorded correctly: (kind x height z spin [town-number [properties]])."
  (and (listp it) (<= 5 (length it) 7) (props-ok-p (seventh it)) (assoc (first it) *kinds*)
       (every (lambda (v) (and (realp v) (< -6 v 6))) (subseq it 1 4))
       (member (fifth it) '(0 1 2 3))
       (or (null (sixth it)) (and (integerp (sixth it)) (<= 0 (sixth it) 99999999)))))


(defun specs-ok-p (specs)
  (and (listp specs) (<= (length specs) *max-items*) (every #'spec-ok-p specs)))

(defparameter *moved-furniture*
  '(((:barrel -.9 0.0 2.3 0) . (:barrel -1.12 0.0 1.26 0))
    ((:sack -.9 .72 2.3 0) . (:sack -1.12 .72 1.26 0))
    ((:sack 1.0 0.0 2.35 1) . (:sack 1.1 0.0 -1.85 1)))
  "Things of the old starting arrangement that now stand inside the stove and the washstand, and where they go:
an old save still has them at the old places.")

(defun sublis-specs (moves specs)
  "Return SPECS with every record that is the old side of MOVES replaced by its new side."
  (mapcar (lambda (spec) (or (cdr (assoc spec moves :test #'equal)) spec)) specs))

(defun load-interior ()
  "Return the saved arrangement, or the starting one if the save is missing or damaged."
  (let ((specs (get-save :interior)))
    (if (and specs (specs-ok-p specs))
        (mapcar #'old-cut-spec (sublis-specs *moved-furniture* specs))
        (address-crates *start-interior*))))                          ; the world is permanent: addresses are town numbers, leave them alone

(defun set-items (specs)
  "Place the things anew from the list SPECS, already checked by SPECS-OK-P."
  (stop-flying (mapcar #'cdr *items*))               ; things still flying to their place are about to be recreated
  (dolist (it *items*) (node-queue-free (cdr it)))
  (setf *items* (loop for spec in specs collect (cons spec (apply #'build-item *cart* spec))))
  (refresh-seats))

(defun bed-seats ()
  "Return the seats on beds and crates, all facing the big window.
One can sit on a mattress and on a floor-level crate with nothing standing on it."
  (flet ((clear-p (x y z)
           (notany (lambda (it) (destructuring-bind (k ix iy iz sp &optional addr props) (car it)
                                  (declare (ignore k sp addr props))
                                  (and (< (abs (- ix x)) .4) (< (abs (- iz z)) .45) (> iy y))))
                   *items*)))
    (loop for ((kind x y z)) in *items*
          when (eq kind :mattress) collect (list :bed x z -1.57 (+ y .14 .45))
          when (and (eq kind :crate) (zerop y) (clear-p x y z)) collect (list :crate x z -1.57 (+ .45 .48)))))

(defun refresh-seats ()
  (setf *seats* (append (fold-seats) (bed-seats)))
  ;; we sat on a crate and it was carted off or taken apart - stand up
  (when (and *seat* (not *meta*) (not (member *seat* *seats* :test #'equal))) (setf *seat* nil)))

(defun make-interior (cart)
  "Place the things in the freshly assembled wagon CART."
  (setf *ghost* nil *ghost-key* nil
        *items* (loop for spec in (if *items* (mapcar #'car *items*) (load-interior))
                      collect (cons spec (apply #'build-item cart spec))))
  (refresh-seats))

;;; Build mode

(defun aim-point ()
  "Return where we are looking as (x z height supporting-thing): at the floor or at the top of a thing.
Return NIL when looking past everything."
  (let* ((ex (vector3-x *pos*)) (ey *eye-now*) (ez (vector3-z *pos*))
         (dir (look-dir)) (dx (vector3-x dir)) (dy (vector3-y dir)) (dz (vector3-z dir))
         (best nil) (best-t 4.0))
    (when (< dy -.02)
      (flet ((try (top inside item)
               (let ((tt (/ (- (+ *floor* top) ey) dy)))
                 (when (< .15 tt best-t)
                   (let ((x (+ ex (* tt dx))) (z (+ ez (* tt dz))))
                     (when (funcall inside x z) (setf best-t tt best (list x z top item))))))))
        (try 0.0 (lambda (x z) (and (< (abs x) 1.42) (< (abs z) 2.72))) nil)
        (loop for (cx cz w d top) in (fixture-surfaces)                   ; the shelves of the rack
              do (let ((cx cx) (cz cz) (w w) (d d))
                   (try top (lambda (px pz) (and (< (abs (- px cx)) (/ w 2)) (< (abs (- pz cz)) (/ d 2)))) nil)))
        (dolist (it *items*)
          (destructuring-bind (kind x y z spin &optional addr props) (car it)
            (declare (ignorable addr props))
            (multiple-value-bind (w h d) (item-size kind spin)
              (try (+ y h) (lambda (px pz) (and (< (abs (- px x)) (/ w 2)) (< (abs (- pz z)) (/ d 2)))) it))))))
    ;; the ray ran into the side of a thing (the floor point ended up under it) - so we are looking at that thing:
    ;; climb onto its top, and so on until nothing stands above
    (when best
      (loop
        (let ((over (find-if (lambda (it)
                               (destructuring-bind (kind x y z spin &optional addr props) (car it)
            (declare (ignorable addr props))
                                 (multiple-value-bind (w h d) (item-size kind spin)
                                   (and (< (abs (- (first best) x)) (/ w 2)) (< (abs (- (second best) z)) (/ d 2))
                                        (<= y (third best)) (< (third best) (+ y h))))))
                             *items*)))
          (if over
              (destructuring-bind (kind x y z spin &optional addr props) (car over)
                (declare (ignorable addr props))
                (declare (ignore x z))
                (setf best (list (first best) (second best) (+ y (nth-value 1 (item-size kind spin))) over)))
              (return)))))
    best))

(defun snap (v) (* .05 (round v .05)))

(defun place-item ()
  (let ((a (aim-point)) (kind (first (nth *pick* *kinds*))))
    (when (>= (length *items*) *max-items*) (say "Больше не влезет") (return-from place-item))
    ;; a crate is nailed together from the same three planks it breaks down into - otherwise planks would be endless
    (when (and a (member kind '(:crate :box)))
      (when (< *planks* *crate-planks*)
        (say (format nil "На ящик нужно досок: ~d, а есть ~d" *crate-planks* *planks*))
        (return-from place-item))
      (decf *planks* *crate-planks*))
    (when a
      (destructuring-bind (x z top item) a
        (declare (ignore item))
        (let ((spec (list kind (snap x) top (snap z) *spin*)))
          (push (cons spec (apply #'build-item *cart* spec)) *items*)
          (refresh-seats) (save-interior))))))

(defun support-under (spec below)
  "Return the height at which the thing SPEC finds support.
That is the floor, a shelf of the rack or the top of one of the things BELOW."
  (destructuring-bind (kind x y z spin &rest more) spec
    (declare (ignore more))
    (multiple-value-bind (w h d) (item-size kind spin)
      (declare (ignore h))
      (let ((best 0.0))
        (dolist (f (fixture-surfaces))
          (destructuring-bind (fx fz fw fd top) f
            (when (and (< (abs (- x fx)) (/ fw 2)) (< (abs (- z fz)) (/ fd 2)) (<= top (+ y .06)))
              (setf best (max best top)))))
        (dolist (o below)
          (destructuring-bind (ok ox oy oz os &rest more) o
            (declare (ignore more))
            (multiple-value-bind (ow oh od) (item-size ok os)
              (let ((top (+ oy oh)))
                (when (and (<= top (+ y .06))
                           (< (abs (- x ox)) (- (/ (+ w ow) 2) .06))
                           (< (abs (- z oz)) (- (/ (+ d od) 2) .06)))
                  (setf best (max best top)))))))
        best))))

(defun settle-items ()
  "Lower everything whose support was removed until it stands on something; things only go down.
Return true if anything moved."
  (let ((done '()) (moved nil))
    (dolist (s (sort (mapcar #'car *items*) #'< :key #'third))
      (let ((y (support-under s done)))
        (cond ((> (- (third s) y) .03) (setf moved t) (push (list* (first s) (second s) y (cdddr s)) done))
              (t (push s done)))))
    (when moved (set-items (reverse done)))
    moved))

(defun remove-item ()
  (let ((it (fourth (aim-point))))
    (when (and it (sixth (car it))) (say "Чужую посылку не выбросить — её ждут") (return-from remove-item))
    (when it
      (when (and (member (first (car it)) '(:crate :box)) (not (sixth (car it))))   ; our own crate was removed - the planks come back
        (incf *planks* *crate-planks*))
      (node-queue-free (cdr it))
      (setf *items* (remove it *items*))
      (settle-items)
      (refresh-seats) (save-interior))))

(defun toggle-build ()
  (unless *meta*
    (setf *build* (not *build*))
    (when *build* (setf *view* :inside *seat* nil))
    (unless *build* (when *ghost* (object-set *ghost* "visible" nil)))))

(defun build-key (k)
  "Handle the build mode key K.
Return T if the key was ours."
  (cond ((= k +key-r+) (setf *spin* (mod (1+ *spin*) 4)) t)
        ((<= +key-1+ k (+ +key-1+ 8)) (setf *pick* (min (- k +key-1+) (1- (length *kinds*)))) t)
        ((= k (1- +key-1+)) (setf *pick* (min 9 (1- (length *kinds*)))) t)))    ; key 0 - the tenth thing

(defun step-interior (delta)
  (declare (ignore delta))
  (when (and *build* (or *meta* (eq *view* :outside))) (toggle-build))
  (when *build*
    (destructuring-bind (kind name size) (nth *pick* *kinds*)
      (declare (ignore size))
      (multiple-value-bind (w h d) (item-size kind *spin*)
        (unless (equal *ghost-key* (list kind *spin*))
          (when *ghost* (node-queue-free *ghost*))
          (setf *ghost* (shape *cart* :box (list w h d) "#60ff90" 0 0 0 :alpha .35 :shadow nil :unshaded t)
                *ghost-key* (list kind *spin*)))
        (let ((a (aim-point)))
          (object-set *ghost* "visible" (and a t))
          (when a (node-3d-set-position *ghost* (v3 (snap (first a)) (+ *floor* (third a) (/ h 2)) (snap (second a)))))))
      (setf *build-hint* (format nil "Ставим: ~a · ЛКМ поставить · ПКМ убрать · R · 1–9, 0 · B" name)))))

