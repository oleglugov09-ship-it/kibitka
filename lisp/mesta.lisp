;;;; Places in the wagon are kept in the Blender files, next to what stands there. Where the stove is, how high its top,
;;;; where the shelves are and how wide, where the light switch hangs: each is an object in the file of the model that
;;;; shows it - an empty named at_NAME for a point, a flat cube named surface_NAME for a top to put things on or a floor
;;;; to walk (models.lisp, TAKE-PLACES). Move the stove in Blender together with its at_stove, and the fire, the kettle,
;;;; the pan and the hint "E" move with it. READ-PLACES copies them into the variables the rules have always used;
;;;; the numbers written at those variables are what holds when a file has no such object (and that is logged).
;;;; Heights in the variables are above the wagon floor, as before.
;;;; What is hung by code on a node of its own (the table, the bunks, the guitar, the camera, the door) is still placed
;;;; by its number in code: its model is drawn around its own origin.
;;;; Outside the wagon the same holds for the booths: at_window and at_spot in models/budki/booth-N.blend are where a booth
;;;; answers to E (prikol.lisp, PRIKOL-USABLES).

(in-package #:kiosk-trip)

(defun place-point (model name)
  "Return the point NAME of MODEL as (x height-above-the-floor z), or NIL (logged) when the file has none."
  (let ((p (model-point model name)))
    (cond (p (list (first p) (- (second p) *floor*) (third p)))
          (t (note-error (format nil "в модели ~a нет точки at_~a" model name) "places") nil))))

(defun place-tops (model prefix)
  "Return the surfaces of MODEL named PREFIX..., each as (x z width depth height-above-the-floor); NIL is logged."
  (or (loop for (x top z w d) in (model-surfaces model prefix) collect (list x z w d (- top *floor*)))
      (progn (note-error (format nil "в модели ~a нет поверхностей surface_~a" model prefix) "places") nil)))

(defvar *rack-surfaces* nil "The overhead racks as (x z width depth height) entries, read from the furniture model.")
(defvar *dryer-surface* nil "The dryer over the stove as (x z width depth height), read from the appliances model.")

(defun read-places ()
  "Set the places of the wagon from the Blender files (see the head of this file)."
  (macrolet ((at ((x y z) model name &body body)
               `(let ((p (place-point ,model ,name)))
                  (when p (destructuring-bind (,x ,y ,z) p (declare (ignorable ,x ,y ,z)) ,@body)))))
    ;; the furniture: the stove with what hangs by it, the washstand, the things on the walls, the stepladder
    (at (x y z) "furniture" "stove" (setf *stove-at* (list x z) *stove-top* y)
      (at (rx ry rz) "furniture" "kettle_ring" (setf *kettle-ring* (list (- rx x) ry (- rz z))))
      (at (nx ny nz) "furniture" "pan_nail" (setf *pan-nail* (list (- nx x) ny (- nz z)))))
    (at (x y z) "furniture" "washstand" (setf *washstand-at* (list x z) *washstand-top* y))
    (at (x y z) "furniture" "switch" (setf *switch-at* (list x y z)))
    (at (x y z) "furniture" "map" (setf *map-at* (list x y z)))
    (at (x y z) "furniture" "steps" (setf *steps-at* (list x z)))
    (at (x y z) "furniture" "calendar" (setf *calendar-at* (list x y z)))
    (at (x y z) "furniture" "diary" (setf *diary-at* (list x y z)))
    (at (x y z) "furniture" "roll" (setf *roll-at* (list x y z)))
    (at (x y z) "furniture" "sheet" (setf *sheet-at* (list x y z)))
    (at (x y z) "furniture" "hook" (setf *hook-at* (list x y z)))
    ;; the shelf unit on the front wall: its span, its levels, and the places on a level, six big ones in three sections
    (let ((shelves (place-tops "furniture" "shelf")))
      (when shelves
        (destructuring-bind (x z w d top) (first shelves)
          (declare (ignore d top))
          (let ((x0 (- x (/ w 2))))
            (setf *shelf-x* (list x0 (+ x0 w)) *shelf-z* z
                  *shelf-tops* (sort (mapcar #'fifth shelves) #'<)
                  *shelf-levels* (cons 0.0 (sort (mapcar #'fifth shelves) #'<))
                  *shelf-sections* (loop for k below 3 collect (+ x0 (* w (/ (+ k .5) 3))))
                  *shelf-slots-x* (loop for k below 6 collect (+ x0 (* w (/ (+ k .5) 6))))
                  *rack-front* (+ z .3))))))
    (let ((racks (place-tops "furniture" "rack")))
      (when racks
        (setf *rack-surfaces* racks *rack-top* (fifth (first racks))
              *rack-z* (destructuring-bind (x z w d top) (first racks) (declare (ignore x w top)) (list (- z (/ d 2)) (+ z (/ d 2)))))))
    ;; the appliances: the fridge and its shelves, the dryer, the radiogram and its records, the shelves of figures
    (at (x y z) "appliances" "fridge" (setf *fridge-at* (list x z)))
    (let ((shelves (place-tops "appliances" "fridge")))
      (when shelves (setf *fridge-shelves* (sort (mapcar #'fifth shelves) #'<))))
    (let ((dryer (place-tops "appliances" "dryer")))
      (when dryer (setf *dryer-surface* (first dryer) *dryer-top* (fifth (first dryer)))))
    (at (x y z) "appliances" "jukebox" (setf *jukebox-at* (list x z) *jukebox-y* y))
    (at (x y z) "appliances" "crate" (setf *crate-at* (list x z)))
    (at (x y z) "appliances" "radio" (setf *radio-at* (list x y z)))
    (let ((shelves (place-tops "appliances" "trophy")))
      (when shelves
        (setf *trophy-x* (first (first shelves)) *trophy-z* (second (first shelves)) *trophy-tops* (sort (mapcar #'fifth shelves) #'<))))
    ;; the body and the porch: where one may walk, where the doorway is
    (let ((walk (first (place-tops "kibitka-body" "walk"))))
      (when walk (destructuring-bind (x z w d top) walk (declare (ignore top))
                   (setf *bounds* (list (- x (/ w 2)) (+ x (/ w 2)) (- z (/ d 2)) (+ z (/ d 2)))))))
    (at (x y z) "kibitka-body" "door" (setf *door-at* (list x z)))
    (let ((porch (first (place-tops "porch" "porch"))))
      (when porch (destructuring-bind (x z w d top) porch (declare (ignore top))
                    (setf *porch* (list (- x (/ w 2)) (+ x (/ w 2)) (- z (/ d 2)) (+ z (/ d 2)))))))))
