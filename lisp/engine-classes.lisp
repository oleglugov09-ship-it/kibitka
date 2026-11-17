;;;; Engine classes beyond the ones gdecl declares; a class comes after the ones it
;;;; inherits from. Mesh and material properties are set through OBJECT-SET, for which
;;;; the classes below are enough; per-frame calls go through Node3D methods (they are in gdecl).

(in-package #:kiosk-trip)

(define-engine-class "OS")
(define-engine-class "Engine")
(define-engine-class "ProjectSettings")
;; game files are read through the engine (res://): the browser and the exported game have no ordinary files
(define-engine-class "FileAccess")
(define-engine-class "DirAccess")
