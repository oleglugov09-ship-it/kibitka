;;;; The wagon as we get it: what stands in it at the start of a new game.

;;; Each entry is (kind x bottom-height z quarter-turns). The boxes get addresses on the first load.
(setf *start-interior*
      '(;; three parcels from the warehouse - for the first three towns (middle shelf)
        (:box -1.18 1.1 -2.5 0) (:box -.71 1.1 -2.5 0) (:box -.24 1.1 -2.5 0)
        ;; milk for sale - on the bottom shelf
        (:milk -.24 .55 -2.5 0) (:milk .24 .55 -2.5 0) (:milk .71 .55 -2.5 0) (:milk 1.18 .55 -2.5 0)
        (:lamp 1.18 1.1 -2.5 0)
        ;; by the carpet wall between the fridge and the stove: a barrel with a sack on it; another sack by the rack
        (:barrel -1.12 0.0 1.26 0) (:sack -1.12 .72 1.26 0) (:sack 1.1 0.0 -1.85 1)))
