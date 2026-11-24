;;;; Things that stand on the map, ready-made. The world used to be put up from code as the wagon drove - a town, a
;;;; row of houses, a diner each built in the frame it came into view, a frame of 100 ms and more in the browser.
;;;; Now what stands still is built once, when the map is baked (BAKE-THING in bake.lisp, tools/bake-map.sh --things),
;;;; and saved as a scene of the engine in project/terrain/things; the game only loads it: LOAD-THING.
;;;;
;;;; A saved thing carries its nodes and meshes, and three notes on its root:
;;;;   "mats"   - which of the game's materials (MATERIAL in shapes.lisp) go where: the materials themselves are
;;;;              not saved, or every thing would carry its own copy of every texture;
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

(defun material-of-key (key)
  "Return the game's material for KEY, the list MATERIAL files it under."
  (destructuring-bind (rgb emit energy rough metal vertex alpha unshaded double) key
    (material rgb :emit emit :energy energy :rough rough :metal metal :vertex vertex :alpha alpha :unshaded unshaded :double double)))

(defun read-note (node name)
  "Return what the note NAME on NODE says, as Lisp data, or NIL when there is none."
  (when (object-call node "has_meta" name)
    (let ((*read-eval* nil)) (values (read-from-string (princ-to-string (object-call node "get_meta" name)))))))

(defun load-thing (key)
  "Put the baked thing KEY on the map, under World, and return its root node and what of it moves (for STEP-MOVES);
NIL when it is not baked."
  (when (thing-baked-p key)
    (let* ((scene (present (object-call (gdext:singleton "ResourceLoader") "load" (thing-path key))))
           (root (and scene (present (object-call scene "instantiate")))))
      (when root
        (node-add-child *world* root)
        (loop for (path over surfaces overrides) in (read-note root "mats")
              do (let ((node (object-call root "get_node" path)))
                   (when (object-call node "has_meta" "buffer")            ; the places of a batch: see BAKE-WELD
                     (object-set (object-get node "multimesh") "buffer" (object-call node "get_meta" "buffer")))
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
