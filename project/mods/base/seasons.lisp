;;;; The turn of the year: what the towns use more of in each season, the red-letter days, and what the
;;;; loudspeaker at the market says (lisp/plan.lisp, lisp/life.lisp).

;;; A plist of season and entries of (good factor): how many times its norm a town uses of the good then.
;;; A good has its season, when it runs short, and its slack season, when the shops fill up again: the plan's
;;; output covers the year, not the season.
(setf *season-use*
      '(:winter ((:coal 2.0) (:honey 1.8) (:water .5) (:milk .7) (:tools .5))     ; the stoves; tea with honey
        :spring ((:tools 2.0) (:books 1.5) (:cans .6) (:bread .7))                ; sowing time; exams
        :summer ((:water 2.0) (:milk 1.5) (:coal .5) (:honey .6) (:books .6))     ; the heat; the pioneer camps
        :autumn ((:cans 1.6) (:bread 1.4))))                                      ; stocking up; everyone is out on the potatoes

;;; The sleeves of the three records that come with the wagon: (file name, title).
(setf *disc-titles*
      '(("placeholder-1" "Дорожная") ("placeholder-2" "Весёлый шофёр") ("placeholder-3" "Вечер в Заречном")))

;;; The holidays: (season, day of the season from zero, key, name, the goods buyers come for).
;;; The keys are what life.lisp dresses the towns by.
(setf *holidays*
      '((:spring 0 :subbotnik "Субботник" ())
        (:spring 1 :mayday "1 Мая" (:water :bread))
        (:autumn 0 :school "1 сентября" (:books))
        (:autumn 1 :november "7 Ноября" (:fish :cans))
        (:winter 1 :newyear "Новый год" (:honey :cans))))

;;; What the loudspeaker says, by the time of day; one line is picked for each arrival at a market.
(setf *speaker-lines*
      '(:morning ("Из репродуктора: «Начинаем утреннюю гимнастику. Ноги на ширине плеч…»"
                  "Из репродуктора: «Доброе утро, товарищи! Московское время — шесть часов»"
                  "Из репродуктора: пионерская зорька, горн и барабан")
        :day ("Из репродуктора: «В рабочий полдень» — концерт по заявкам тружеников"
              "Из репродуктора: «Передаём сигналы точного времени…»"
              "Из репродуктора: сводка с полей и марш")
        :evening ("Из репродуктора: «Театр у микрофона»"
                  "Из репродуктора: вечерний выпуск и вальс"
                  "Из репродуктора: «Спокойной ночи, малыши» — для тех, кто не спит")))
