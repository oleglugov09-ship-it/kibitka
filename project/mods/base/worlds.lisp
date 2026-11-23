;;;; The places behind the tunnel: the numbers of their zones. What a zone is built of is code (lisp/mega-zones.lisp,
;;;; lisp/mega-monuments.lisp, lisp/leto.lisp); how long it is, how the way bends in it and how it is lit is set
;;;; here, and can be changed without rebuilding the game. A zone keeps what is not named.
;;;;
;;;;   :cells  how many cells long the zone is, a cell being 42 m (64 unless said);
;;;;   :turn   how many degrees the way may turn from one cell to the next; :steep how steep it may be;
;;;;   :haze   how much pale light stands in a zone of the megastructure, from 0 (pitch dark) to 1;
;;;;   :sun    how strong the day from the breach is there, against the haze (.5 unless said);
;;;;   :fog    how thick the air is (.0047 unless said: a great hall is seen from end to end only in clear air).

;;; The megastructure: the order its zones come in the first time round (after that they are shuffled).
(setf *mega-order* '(:metro :quarry :dam :reactor :factory :pipes :communal :archive :paper :rail :statues :garages :spiral :poles :columns))

(when (fboundp 'tune-zones)                 ; not there when the core is loaded without the places (the unit checks)
 (tune-zones
 '(;; the megastructure
   :lift     (:haze .5 :sun 1.4 :fog .0009 :cells 24)
   :metro    (:haze .5 :sun 0.0 :fog .0009 :cells 80)
   :quarry   (:haze .7 :sun 3.0 :fog .0009 :turn 3.0 :steep .05)
   :dam      (:haze .5 :sun 1.4 :fog .0012 :turn .6)
   :reactor  (:haze .5 :sun 1.0 :fog .0016 :turn 1.5)
   :factory  (:haze .5 :turn 1.5)
   :pipes    (:haze .24 :turn 4.5 :steep .07)
   :communal (:haze .38 :turn 1.0)
   :archive  (:haze .34 :turn 2.0)
   :paper    (:haze .55 :turn 4.5 :cells 48)
   :rail     (:haze .3 :turn 2.0 :steep .015)
   :statues  (:haze .3 :turn 1.5)
   :garages  (:haze .3 :turn 5.0 :steep .045)
   :spiral   (:haze .12 :turn 6.0 :steep .07)
   :poles    (:haze .3 :turn 4.0)
   :columns  (:haze .22 :turn 2.5)
   ;; the summer: flat land, a road that bends a little
   :morning  (:turn 2.0)
   :noon     (:turn 1.5)
   :storm    (:turn 1.5 :cells 40)
   :evening  (:turn 2.0)
   :night    (:turn 2.0))))

;;; The summer: its hours in the order they come, each with the second of the day cycle (0 is dawn, 1200 a full
;;; day) the sky shows there; and how far ahead the town stands that one never reaches, in metres.
(setf *summer-hours* '(:morning 62.0 :noon 350.0 :storm 470.0 :evening 722.0 :night 960.0))
(setf *summer-town* 1700.0)
