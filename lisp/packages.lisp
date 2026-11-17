(defpackage #:kiosk-trip
  (:documentation "Kibitka: a house-truck on the road. Godot nodes are created from Lisp (shapes.lisp) and
moved by it every frame.")
  (:use #:cl #:godot #:godot-clos)
  (:shadow #:random                       ; our own RANDOM, see random.lisp
           ;; our own wrappers: through them buildings report the outlines of their solids (shapes.lisp)
           #:format #:object-set                 ; text translation on the fly: delo.lisp, shapes.lisp
           #:node-3d-set-position #:node-3d-set-rotation #:node-3d-set-scale #:node-queue-free))                     ; our own RANDOM, see random.lisp
