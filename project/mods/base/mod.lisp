;;;; The base content of Kibitka: everything the game shows that is not logic. The core (lisp/) knows how
;;;; towns trade, how booths and gnomes work and how places are built; this mod says which towns, goods,
;;;; booths, gnomes and places there are. Without it the game has no world to plan.
;;;;
;;;; Each file sets tables the core declares empty; the docstring of each table (in lisp/) describes its
;;;; shape. To change the game, copy this folder under another name into your mods folder and edit, or
;;;; write a small mod that changes one table after this one has loaded (mods load in alphabetical order).

(include "towns.lisp")
(include "booths.lisp")
(include "gnomes.lisp")
(include "places.lisp")
(include "looks.lisp")
(include "guide.lisp")
(include "lines.lisp")
(include "story.lisp")
(include "calendar.lisp")
(include "forces.lisp")
(include "seasons.lisp")
(include "people.lisp")
(include "en.lisp")
