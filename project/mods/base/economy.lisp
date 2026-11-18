;;;; The deals and the rules of trade: what the booths ask, who comes to the window, what needs a licence.
;;;; The prices of goods are in towns.lisp; the rest of the numbers (fuel, parcels, the debt) are in
;;;; lisp/economy.lisp, and a mod may SETF any of them after this file.

;;; The kinds of client who come at a market and the share of each.
(setf *client-mix* '((:sender . .35) (:seller . .25) (:buyer . .4)))

;;; The deals, each as (how many, for how much; money is in units of five kopecks).
(setf *plank-price* '(3 5))            ; planks at a gas station
(setf *carpenter-deal* '(6 5))         ; planks at the carpenter's (the gas station gives three for five)
(setf *jar-deal* '(3 6))               ; empty jars with lids at the hardware shop

;;; The money that comes with a town council paper, by the paper.
(setf *quest-money* '((:honor . 15) (:feast . 60)))

;;; The goods that need a licence: (goods-kind licence display-name).
(setf *restricted* '((:coal :coal "уголь") (:berry :air "авиагруз (клюква)")))
