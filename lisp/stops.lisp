;;;; Bus stops. In the Soviet Union everyone built them however they pleased, and each is a small sculpture:
;;;; a wave, a mushroom, a tent, a bird, a rainbow, a yurt, a saucer on legs. There are eighteen kinds;
;;;; STOP-LOOK decides which one stands at path coordinate D, the same for all players. Sixteen are drawn in
;;;; Blender (models/ostanovki/stop-K.blend, redrawn 25.11.2026 as solid things: tools/draw/make_stops.py); the box takes
;;;; its tiles' colours from the place (doroga.lisp) and the mural wall the ornament of its town (here).
;;;; A bus stop stands in group G: x+ is away from the road (the back wall is there), z runs along the
;;;; road, the floor is y 0.
;;;; The bench, the slab and the sign are shared (ADD-STOP); the canopy must not come closer to the road
;;;; than x −1.3.

(in-package #:kiosk-trip)

(defparameter *stop-rgb* '("#3f6f73" "#a8843c" "#8a4038" "#55708a" "#b0aa98" "#6a8a5a" "#c9a13a" "#7a5a7a")
  "The faded paint colours of the bus stops.")
(defun stop-rgb (d &optional (k 0)) (nth (mod (+ (floor d 11) k) (length *stop-rgb*)) *stop-rgb*))

(defun stop-mosaic-wall (g d)
  "Build the mural wall stop in G.
It is a full-height slab bearing the ornament of the town nearest to D, with a short canopy on top."
  (model g "stop-12")
  (let ((tw (or (town-at d) (find-if (lambda (tw) (> (town-start tw) d)) *towns*))))
    (or (and tw (mural-quad g .86 1.75 0 -1 2.9 (town-index tw) :b))
        (shape g :box '(.04 2.2 2.6) (stop-rgb d) .86 1.7 0 :shadow nil))))

(defparameter *stop-count* 18 "The number of kinds of bus stop.")

(defun stop-look (g d)
  "Put in group G the kind of bus stop that belongs at path coordinate D, the same for all players.
Sixteen kinds are drawn in Blender (models/stop-K.blend) and repainted in the faded colours of the place; the box
with its tiles and the mural wall differ from stop to stop and are built in code."
  (let ((k (mod (floor d 7) *stop-count*)))
    (case k
      (0 (stop-box g d))
      (12 (stop-mosaic-wall g d))
      (t (model g (format nil "stop-~d" k) 0.0 0.0 0.0 :paint (loop for i below 8 collect (stop-rgb d i)))))))
