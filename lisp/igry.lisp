;;;; Booth games: shooting range, fishing, mine, goat. Each one takes some skill of the paws and is played in
;;;; the world, in first person (activities.lisp). Whoever walked up plays, a guest too; the driver only
;;;; admits them to the game (money, 'once per day cycle', free paws) and hands out the prize by score:
;;;; BEGIN-PLAY and PLAY-RESULT here are that admission and that prize.

(in-package #:kiosk-trip)

(defun mouse-held-p () (object-call (gdext:singleton "Input") "is_mouse_button_pressed" 1))

(defun begin-play (kind)
  "Admit whoever asked to the game KIND, as the driver: yourself or a guest (who is sent a message)."
  (if *actor* (peer-send *actor* (list :play kind)) (start-play kind)))

(defun start-play (kind)
  "Begin the game KIND where we stand, unless one is already on."
  (when (and (member kind '(:tir :fish :mine :goat)) (null *act*)) (start-act kind)))

;;; ------------------------------------------------------------------ prize by score (the driver decides)

(defshared play-result :score (kind score)
  "Hand the prize to whoever played the game KIND, given the final SCORE."
  (when (and (integerp score) (<= 0 score 9))
    (log-event :played :kind kind :score score)
    (case kind
      (:tir (cond ((>= score 5) (setf *badge* t) (save-prikol)
                   (or (give-good :cans "Пять из пяти! Значок «Меткий стрелок» и банка тушёнки — приз")
                       (say "Пять из пяти! Значок «Меткий стрелок» твой. Приз не взять — руки заняты"))
                   (say-all "Пять из пяти в тире!"))
                  ((= score 4) (or (give-good :cans "Четыре из пяти: банка тушёнки. Значок — за пять")
                                   (say "Четыре из пяти. Приз не взять — руки заняты")))
                  (t (say (format nil "Попал ~d из 5. Приз — с четырёх" score)))))
      (:fish (cond ((zerop score) (say "Без улова. Попробуй ещё"))
                   ((spent-p 0))
                   ((give-good :fish "Поймал рыбу! Больше сегодня не клюёт — рыбу ищут на рынках") (spend 0))))
      (:mine (cond ((< score 3) (say "Пустая порода. Пласт где-то рядом — попробуй ещё"))
                   ((spent-p 8))
                   ((give-good :coal "Поднялся с ящиком угля. В буржуйке горит втрое дольше доски — E по печке") (spend 8))))
      (:goat (cond ((< score 4) (say "Манька недовольна: почти ничего не надоил. Попробуй ещё"))
                   ((spent-p 4))
                   ((give-good :milk "Полное ведро — нет, целая коробка молока. Манька довольна") (spend 4)))))))
