;;;; Booth games: shooting range, fishing, mine, goat. This used to be a dice roll: press E and read what
;;;; came out. Now each one takes some skill of the paws: aiming, striking, finding the seam, keeping the
;;;; beat. Whoever walked up plays, on their own screen (a guest too); the driver only admits them to the
;;;; game (money, 'once per day cycle', free paws) and hands out the prize by score: PLAY-RESULT.
;;;;
;;;; A game runs on a flat 640×360 screen over the world: rectangles and labels, the mouse. The state is *PLAY*.

(in-package #:kiosk-trip)

(defvar *play* nil "The game in progress: a plist with :kind (:tir :fish :mine :goat) and its score, or NIL.")
(defvar *play-ui* nil "The game screen: a plist of :layer and :bg.")
(defvar *play-click* nil "True if the mouse button was down last frame; this catches the press, not the hold.")

(defun make-play (root)
  (let ((layer (gdext:make-object "CanvasLayer")) (bg (gdext:make-object "Control")))
    (object-set layer "layer" 7)
    (node-add-child root layer) (node-add-child layer bg)
    (object-set layer "visible" nil)
    (setf *play-ui* (list :layer layer :bg bg))))

(defun play-rect (x y w h rgb &optional (a 1.0)) (hud-rect (getf *play-ui* :bg) x y w h rgb a))
(defun play-label (text x y &optional (size 8) (rgb '(.92 .88 .76))) (hud-label (getf *play-ui* :bg) text x y size rgb))
(defun move-rect (r x y) (object-set r "position" (vector2 x y)))
(defun play-mouse ()
  (let ((at (object-call (getf *play-ui* :bg) "get_local_mouse_position"))) (values (vector2-x at) (vector2-y at))))
(defun mouse-held-p () (object-call (gdext:singleton "Input") "is_mouse_button_pressed" 1))

(defun begin-play (kind)
  "Admit whoever asked to the game KIND, as the driver: yourself or a guest (who is sent a message)."
  (if *actor* (peer-send *actor* (list :play kind)) (start-play kind)))

(defun start-play (kind)
  "Open the screen of game KIND locally."
  (when (and *play-ui* (member kind '(:tir :fish :mine :goat)) (null *play*))
    (let ((bg (getf *play-ui* :bg)))
      (dolist (c (coerce (object-call bg "get_children") 'list)) (node-queue-free c)))
    (setf *play-click* t *play* (list :kind kind :t 0.0 :done nil))
    (object-set (getf *play-ui* :layer) "visible" t)
    (input-set-mouse-mode 0)
    (ecase kind (:tir (open-tir)) (:fish (open-fish)) (:mine (open-mine)) (:goat (open-goat)))))

(defun end-play (score)
  "Close the game screen and report SCORE to the driver."
  (let ((kind (getf *play* :kind)))
    (setf *play* nil)
    (object-set (getf *play-ui* :layer) "visible" nil)
    (input-set-mouse-mode 2)
    (when score (play-result kind score))))

(defun finish-play (score text)
  "Mark the game as played: show the result TEXT and close a second later with SCORE."
  (unless (getf *play* :done)
    (setf (getf *play* :done) (list (+ (getf *play* :t) 1.2) score))
    (object-set (getf *play* :title) "text" text)))

(defun step-play (delta)
  (when *play*
    (incf (getf *play* :t) delta)
    (let* ((down (mouse-held-p)) (click (and down (not *play-click*))))
      (setf *play-click* down)
      (cond ((getf *play* :done)
             (when (> (getf *play* :t) (first (getf *play* :done))) (end-play (second (getf *play* :done)))))
            ((input-is-key-pressed 4194305) (end-play nil))                    ; Esc: leave without a prize
            (t (ecase (getf *play* :kind)
                 (:tir (step-tir delta click)) (:fish (step-fish delta click down))
                 (:mine (step-mine click)) (:goat (step-goat delta click))))))))

;;; ------------------------------------------------------------------ shooting range: five shots at tin ducks

(defun open-tir ()
  (play-rect 0 0 640 360 '(.05 .06 .05) .86)
  (play-rect 90 60 460 240 '(.16 .22 .14))
  (dolist (y '(118 178 238)) (play-rect 90 y 460 6 '(.36 .26 .16)))
  (setf (getf *play* :ducks)
        (loop for i below 6
              collect (let ((y (nth (mod i 3) '(98 158 218))))
                        (list (play-rect 0 y 26 18 (if (evenp i) '(.86 .72 .2) '(.78 .3 .22)))
                              (+ 110.0 (random 380.0)) y (* (if (evenp i) 1 -1) (+ 55.0 (random 60.0))) t)))
        (getf *play* :shots) 5 (getf *play* :hits) 0
        (getf *play* :cross) (list (play-rect 0 0 15 1 '(1 1 1)) (play-rect 0 0 1 15 '(1 1 1)))
        (getf *play* :title) (play-label "" 100 38 9)))

(defun step-tir (delta click)
  (let ((time (getf *play* :t)))
    (dolist (d (getf *play* :ducks))                                           ; the ducks ride back and forth along the shelves
      (incf (second d) (* (fourth d) delta))
      (when (or (< (second d) 96) (> (second d) 518)) (setf (fourth d) (- (fourth d))))
      (move-rect (first d) (if (fifth d) (second d) -60) (third d)))
    (multiple-value-bind (mx my) (play-mouse)
      (let ((cx (+ mx (* 9 (sin (* time 1.7))) (* 4 (sin (* time 4.1)))))       ; the paw trembles: the sight drifts around the mouse
            (cy (+ my (* 7 (sin (* time 2.3))) (* 3 (cos (* time 3.3))))))
        (move-rect (first (getf *play* :cross)) (- cx 7) cy)
        (move-rect (second (getf *play* :cross)) cx (- cy 7))
        (when (and click (plusp (getf *play* :shots)))
          (decf (getf *play* :shots))
          (let ((hit (find-if (lambda (d) (and (fifth d) (< -2 (- cx (second d)) 28) (< -2 (- cy (third d)) 20))) (getf *play* :ducks))))
            (when hit (setf (fifth hit) nil) (incf (getf *play* :hits)))))))
    (object-set (getf *play* :title) "text"
                (format nil "ТИР · выстрелов: ~d · попаданий: ~d · ЛКМ — стрелять, Esc — уйти" (getf *play* :shots) (getf *play* :hits)))
    (when (zerop (getf *play* :shots))
      (finish-play (getf *play* :hits) (format nil "Попал ~d из 5" (getf *play* :hits))))))

;;; ------------------------------------------------------------------ fishing: strike and reel in

(defun open-fish ()
  (play-rect 0 0 640 360 '(.04 .06 .08) .86)
  (play-rect 90 150 460 150 '(.12 .3 .42))
  (play-rect 90 60 460 90 '(.5 .62 .72))
  (setf (getf *play* :float) (play-rect 318 140 6 16 '(.86 .2 .16))
        (getf *play* :bar) (play-rect 170 316 300 10 '(.2 .2 .22))
        (getf *play* :zone) (play-rect 170 316 64 10 '(.3 .62 .3))
        (getf *play* :mark) (play-rect 170 312 4 18 '(1 1 1))
        (getf *play* :title) (play-label "" 100 38 9)
        (getf *play* :phase) :wait (getf *play* :until) (+ 2.5 (random 4.5)) (getf *play* :tries) 3
        (getf *play* :pos) 0.0 (getf *play* :good) 0.0 (getf *play* :bad) 0.0))

(defun step-fish (delta click down)
  (let ((time (getf *play* :t)))
    (flet ((again (text) (if (plusp (decf (getf *play* :tries)))
                             (setf (getf *play* :phase) :wait (getf *play* :until) (+ time 2.0 (random 4.0)) (getf *play* :note) text)
                             (finish-play 0 (format nil "~a Рыба ушла совсем" text)))))
      (ecase (getf *play* :phase)
        (:wait
         (move-rect (getf *play* :float) 318 (+ 140 (* 2 (sin (* time 3.0)))))
         (object-set (getf *play* :title) "text"
                     (format nil "РЫБАЛКА · ~@[~a · ~]ждём… нырнёт поплавок — ЛКМ! Попыток: ~d" (getf *play* :note) (getf *play* :tries)))
         (cond (click (again "Рано дёрнул — спугнул."))
               ((> time (getf *play* :until)) (setf (getf *play* :phase) :bite (getf *play* :until) (+ time .65)))))
        (:bite
         (move-rect (getf *play* :float) 318 152)
         (object-set (getf *play* :title) "text" "КЛЮЁТ! ЛКМ!")
         (cond (click (setf (getf *play* :phase) :reel (getf *play* :pos) 100.0 (getf *play* :good) 0.0 (getf *play* :bad) 0.0))
               ((> time (getf *play* :until)) (again "Проспал поклёвку."))))
        (:reel                                                                 ; keep the slider in the green: holding the button pulls it right
         (let* ((zone (+ 118.0 (* 100.0 (sin (* time 1.6))) (* 18.0 (sin (* time 4.3)))))
                (pos (max 0.0 (min 296.0 (+ (getf *play* :pos) (* delta (if down 150.0 -120.0))))))
                (in (< zone pos (+ zone 60))))
           (setf (getf *play* :pos) pos)
           (if in (incf (getf *play* :good) delta) (incf (getf *play* :bad) delta))
           (move-rect (getf *play* :zone) (+ 170 zone) 316)
           (move-rect (getf *play* :mark) (+ 170 pos) 312)
           (move-rect (getf *play* :float) (+ 318 (* 30 (sin (* time 5.0)))) 150)
           (object-set (getf *play* :title) "text"
                       (format nil "ТЯНИ! Держи белый бегунок в зелёном: зажал ЛКМ — вправо, отпустил — влево · ~d%"
                               (round (* 100 (/ (getf *play* :good) 3.5)))))
           (cond ((>= (getf *play* :good) 3.5) (finish-play 1 "Вытащил!"))
                 ((>= (getf *play* :bad) 1.6) (again "Сорвалась.")))))))))

;;; ------------------------------------------------------------------ mine: ten strikes to find the seam

(defun open-mine ()
  (play-rect 0 0 640 360 '(.03 .03 .04) .9)
  (let* ((cols 8) (rows 4) (coal '()))
    (loop while (< (length coal) 5) do (pushnew (random (* cols rows)) coal))
    (setf (getf *play* :cells)
          (loop for i below (* cols rows)
                collect (let ((x (+ 152 (* 42 (mod i cols)))) (y (+ 90 (* 42 (floor i cols)))))
                          (list (play-rect x y 40 40 '(.3 .28 .26)) x y (and (member i coal) t) nil)))
          (getf *play* :cols) cols (getf *play* :strikes) 10 (getf *play* :found) 0
          (getf *play* :title) (play-label "" 100 38 9))))

(defun step-mine (click)
  (let ((cells (getf *play* :cells)) (cols (getf *play* :cols)))
    (when (and click (plusp (getf *play* :strikes)))
      (multiple-value-bind (mx my) (play-mouse)
        (let ((i (position-if (lambda (c) (and (not (fifth c)) (< (second c) mx (+ (second c) 40)) (< (third c) my (+ (third c) 40)))) cells)))
          (when i
            (let ((c (nth i cells)))
              (decf (getf *play* :strikes))
              (setf (fifth c) t)
              (cond ((fourth c)                                                 ; coal
                     (incf (getf *play* :found))
                     (object-set (first c) "color" (color .06 .06 .08 1.0))
                     (play-rect (+ (second c) 12) (+ (third c) 10) 8 6 '(.6 .6 .7)))
                    (t (let ((near (loop for j below (length cells)              ; barren rock: how much coal is around
                                         count (and (fourth (nth j cells)) (/= i j)
                                                    (<= (abs (- (mod i cols) (mod j cols))) 1) (<= (abs (- (floor i cols) (floor j cols))) 1)))))
                         (object-set (first c) "color" (color .46 .42 .36 1.0))
                         (play-label (if (zerop near) "" (format nil "~d" near)) (+ (second c) 15) (+ (third c) 12) 12 '(.1 .08 .06))))))))))
    (object-set (getf *play* :title) "text"
                (format nil "ШАХТА · ударов: ~d · угля: ~d из 3 · цифра — сколько угля в соседних клетках" (getf *play* :strikes) (getf *play* :found)))
    (cond ((>= (getf *play* :found) 3) (finish-play (getf *play* :found) "Ящик полон!"))
          ((zerop (getf *play* :strikes)) (finish-play (getf *play* :found) "Кирка затупилась. Пустая порода")))))

;;; ------------------------------------------------------------------ goat: milk to the beat

(defun open-goat ()
  (play-rect 0 0 640 360 '(.06 .05 .04) .86)
  (setf (getf *play* :bar) (play-rect 170 200 300 14 '(.24 .2 .16))
        (getf *play* :zone) (play-rect 296 200 48 14 '(.3 .62 .3))
        (getf *play* :mark) (play-rect 170 195 5 24 '(1 1 1))
        (getf *play* :pail) (play-rect 300 270 40 4 '(.92 .92 .88))
        (getf *play* :title) (play-label "" 100 38 9)
        (getf *play* :pulls) 6 (getf *play* :good) 0 (getf *play* :phase) 0.0 (getf *play* :speed) 1.0))

(defun step-goat (delta click)
  (incf (getf *play* :phase) (* delta (getf *play* :speed) 5.2))
  (let ((pos (+ 148.0 (* 148.0 (sin (getf *play* :phase))))))
    (move-rect (getf *play* :mark) (+ 170 pos) 195)
    (when (and click (plusp (getf *play* :pulls)))
      (decf (getf *play* :pulls))
      (when (< 124 pos 176)                                                    ; in the green: a squirt into the pail
        (incf (getf *play* :good))
        (object-set (getf *play* :pail) "size" (vector2 40 (+ 4 (* 6 (getf *play* :good)))))
        (move-rect (getf *play* :pail) 300 (- 270 (* 6 (getf *play* :good)))))
      (setf (getf *play* :speed) (* 1.12 (getf *play* :speed))))                ; Manka gets used to it, so the beat quickens
    (object-set (getf *play* :title) "text"
                (format nil "КОЗА МАНЬКА · дои в такт: ЛКМ, когда бегунок в зелёном · осталось ~d · в ведре ~d" (getf *play* :pulls) (getf *play* :good)))
    (when (zerop (getf *play* :pulls))
      (finish-play (getf *play* :good) (if (>= (getf *play* :good) 4) "Полное ведро!" "Манька лягнула ведро")))))

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
