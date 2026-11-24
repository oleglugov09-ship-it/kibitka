;;;; Things that stand on the map, ready-made. The world used to be put up from code as the wagon drove - a town, a
;;;; row of houses, a diner each built in the frame it came into view, a frame of 100 ms and more in the browser.
;;;; Now what stands still is built once, when the map is baked (BAKE-THING in bake.lisp, tools/bake-map.sh --things),
;;;; and saved as a scene of the engine in project/terrain/things; the game only loads it: LOAD-THING.
;;;;
;;;; A saved thing carries its nodes, meshes and the places of its batches. The game's materials (MATERIAL in
;;;; shapes.lisp) it does not carry: they are saved once, each a file in project/terrain/mats, and a thing only points
;;;; at them - the engine puts them on as it loads the thing, with no work on our side. LOAD-MATERIALS reads those
;;;; files at the start, so that the game and the things share one material of a kind, and the style's textures and
;;;; the seasons reach both. Three notes on the root say the rest:
;;;;   "mats"   - where the few materials that are not the game's own go (a mosaic, a town's mural);
;;;;   "solids" - what of it cannot be walked through (tverd.lisp);
;;;;   "moves"  - what of it turns, swings and travels (STEP-MOVES in places.lisp).

(in-package #:kiosk-trip)

(defparameter *things-dir* "res://terrain/things" "Where the baked things lie.")

(defun thing-path (key) (format nil "~a/~a.scn" *things-dir* key))

(defvar *things-here* (make-hash-table :test 'equal) "A table from a thing's key to whether its file is there: asked once.")

(defun thing-baked-p (key)
  "Return true if the thing KEY is baked: its file is among the game's."
  (multiple-value-bind (known found) (gethash key *things-here*)
    (if found
        known
        (setf (gethash key *things-here*) (and (object-call (gdext:singleton "ResourceLoader") "exists" (thing-path key)) t)))))

(defparameter *mats-dir* "res://terrain/mats" "Where the game's materials lie, saved when the things were baked.")

(defun mat-path (n) (format nil "~a/m~d.res" *mats-dir* n))

(defun load-materials ()
  "Read the materials saved with the things into *MATERIALS*, so that MATERIAL returns the very ones the things wear.
Call it before anything is built. Return how many were read."
  (let ((index (format nil "~a/index.txt" *mats-dir*)) (n 0))
    (when (file-access-file-exists index)
      (loop for (key . number) in (let ((*read-eval* nil)) (read-from-string (file-access-get-file-as-string index)))
            do (let ((m (present (object-call (gdext:singleton "ResourceLoader") "load" (mat-path number)))))
                 (when (and m (null (gethash key *materials*)))
                   (gdext:setf-reference (gethash key *materials*) m)
                   (incf n)))))
    n))

(defun material-of-key (key)
  "Return the game's material for KEY, the list MATERIAL files it under."
  (case (first key)
    (:mosaic (funcall 'mosaic-material (second key)))                    ; a cell of the mosaic atlas (city.lisp)
    (:mural (funcall 'mural-material (second key) (third key)))          ; a town's mural (towns.lisp)
    (t (destructuring-bind (rgb emit energy rough metal vertex alpha unshaded double) key
         (material rgb :emit emit :energy energy :rough rough :metal metal :vertex vertex :alpha alpha :unshaded unshaded :double double)))))

(defun read-note (node name)
  "Return what the note NAME on NODE says, as Lisp data, or NIL when there is none."
  (when (object-call node "has_meta" name)
    (let ((*read-eval* nil)) (values (read-from-string (princ-to-string (object-call node "get_meta" name)))))))

(defun load-thing (key &optional (parent *world*))
  "Put the baked thing KEY on the map, under PARENT - World, or Land for what lives and dies with the roads - and return
its root node and what of it moves (for STEP-MOVES); NIL when it is not baked."
  (when (thing-baked-p key)
    (let* ((scene (present (object-call (gdext:singleton "ResourceLoader") "load" (thing-path key))))
           (root (and scene (present (object-call scene "instantiate")))))
      (when root
        (node-add-child parent root)
        (loop for (path over surfaces overrides) in (read-note root "mats")
              do (let ((node (object-call root "get_node" path)))
                   (when over (object-set node "material_override" (material-of-key over)))
                   (when surfaces
                     (let ((mesh (if (object-call node "is_class" "MultiMeshInstance3D")
                                     (object-get (object-get node "multimesh") "mesh")
                                     (object-get node "mesh"))))
                       (loop for k in surfaces for i from 0
                             do (when k (object-call mesh "surface_set_material" i (material-of-key k))))))
                   (loop for k in overrides for i from 0
                         do (when k (object-call node "set_surface_override_material" i (material-of-key k))))))
        (loop for (x z hw hd yaw) in (read-note root "solids") do (add-solid root x z hw hd yaw))
        (values root
                (loop for (path . what) in (read-note root "moves")
                      collect (cons (object-call root "get_node" path) what)))))))
