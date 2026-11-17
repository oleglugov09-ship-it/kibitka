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

(defmacro with-play (slots &body body)
  "Run BODY with each name in SLOTS standing for that field of the game in progress.
Reading, SETF, INCF and DECF all work on the names. Do not rebind a slot name inside BODY."
  `(symbol-macrolet ,(loop for s in slots collect `(,s (getf *play* ,(intern (symbol-name s) '#:keyword)))) ,@body))

(defun begin-play (kind)
  "Admit whoever asked to the game KIND, as the driver: yourself or a guest (who is sent a message)."
  (if *actor* (peer-send *actor* (list :play kind)) (start-play kind)))

(defun start-play (kind)
  "Open the screen of game KIND locally.
The shooting range and fishing have no screen: they are played in the world (activities.lisp)."
  (when (and (member kind '(:tir :fish :mine :goat)) (null *play*) (null *act*)) (return-from start-play (start-act kind)))
  (when (and *play-ui* (member kind '(:tir :fish :mine :goat)) (null *play*))
    (let ((bg (getf *play-ui* :bg)))
      (dolist (c (coerce (object-call bg "get_children") 'list)) (node-queue-free c)))
    (setf *play-click* t *play* (list :kind kind :time 0.0 :done nil))
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
  (with-play (done time title)
    (unless done
      (setf done (list (+ time 1.2) score))
      (object-set title "text" text))))

(defun step-play (delta)
  (when *play*
    (with-play (time done kind)
      (incf time delta)
      (let* ((down (mouse-held-p)) (click (and down (not *play-click*))))
        (setf *play-click* down)
        (cond (done (when (> time (first done)) (end-play (second done))))
              ((input-is-key-pressed 4194305) (end-play nil))                  ; Esc: leave without a prize
              (t (ecase kind
                   (:tir (step-tir delta click)) (:fish (step-fish delta click down))
                   (:mine (step-mine click)) (:goat (step-goat delta click)))))))))

;;; ------------------------------------------------------------------ shooting range: five shots at tin ducks

(defun open-tir ()
  (play-rect 0 0 640 360 '(.05 .06 .05) .86)
  (play-rect 90 60 460 240 '(.16 .22 .14))
  (dolist (y '(118 178 238)) (play-rect 90 y 460 6 '(.36 .26 .16)))
  (with-play (ducks shots hits cross title)
    (setf ducks (loop for i below 6
                      collect (let ((y (nth (mod i 3) '(98 158 218))))
                                (list (play-rect 0 y 26 18 (if (evenp i) '(.86 .72 .2) '(.78 .3 .22)))
                                      (+ 110.0 (random 380.0)) y (* (if (evenp i) 1 -1) (+ 55.0 (random 60.0))) t)))
          shots 5 hits 0
          cross (list (play-rect 0 0 15 1 '(1 1 1)) (play-rect 0 0 1 15 '(1 1 1)))
          title (play-label "" 100 38 9))))

(defun step-tir (delta click)
  (with-play (time ducks cross shots hits title)
    (dolist (d ducks)                                                          ; the ducks ride back and forth along the shelves
      (incf (second d) (* (fourth d) delta))
      (when (or (< (second d) 96) (> (second d) 518)) (setf (fourth d) (- (fourth d))))
      (move-rect (first d) (if (fifth d) (second d) -60) (third d)))
    (multiple-value-bind (mx my) (play-mouse)
      (let ((cx (+ mx (* 9 (sin (* time 1.7))) (* 4 (sin (* time 4.1)))))       ; the paw trembles: the sight drifts around the mouse
            (cy (+ my (* 7 (sin (* time 2.3))) (* 3 (cos (* time 3.3))))))
        (move-rect (first cross) (- cx 7) cy)
        (move-rect (second cross) cx (- cy 7))
        (when (and click (plusp shots))
          (decf shots)
          (let ((hit (find-if (lambda (d) (and (fifth d) (< -2 (- cx (second d)) 28) (< -2 (- cy (third d)) 20))) ducks)))
            (when hit (setf (fifth hit) nil) (incf hits))))))
    (object-set title "text" (format nil "ТИР · выстрелов: ~d · попаданий: ~d · ЛКМ — стрелять, Esc — уйти" shots hits))
    (when (zerop shots) (finish-play hits (format nil "Попал ~d из 5" hits)))))

;;; ------------------------------------------------------------------ fishing: strike and reel in

(defun open-fish ()
  (play-rect 0 0 640 360 '(.04 .06 .08) .86)
  (play-rect 90 150 460 150 '(.12 .3 .42))
  (play-rect 90 60 460 90 '(.5 .62 .72))
  (with-play (bob bar zone mark title phase until tries pos good bad)
    (setf bob (play-rect 318 140 6 16 '(.86 .2 .16))
          bar (play-rect 170 316 300 10 '(.2 .2 .22))
          zone (play-rect 170 316 64 10 '(.3 .62 .3))
          mark (play-rect 170 312 4 18 '(1 1 1))
          title (play-label "" 100 38 9)
          phase :wait until (+ 2.5 (random 4.5)) tries 3
          pos 0.0 good 0.0 bad 0.0)))

(defun step-fish (delta click down)
  (with-play (time phase until tries note bob zone mark title pos good bad)
    (flet ((again (text) (if (plusp (decf tries))
                             (setf phase :wait until (+ time 2.0 (random 4.0)) note text)
                             (finish-play 0 (format nil "~a Рыба ушла совсем" text)))))
      (ecase phase
        (:wait
         (move-rect bob 318 (+ 140 (* 2 (sin (* time 3.0)))))
         (object-set title "text" (format nil "РЫБАЛКА · ~@[~a · ~]ждём… нырнёт поплавок — ЛКМ! Попыток: ~d" note tries))
         (cond (click (again "Рано дёрнул — спугнул."))
               ((> time until) (setf phase :bite until (+ time .65)))))
        (:bite
         (move-rect bob 318 152)
         (object-set title "text" "КЛЮЁТ! ЛКМ!")
         (cond (click (setf phase :reel pos 100.0 good 0.0 bad 0.0))
               ((> time until) (again "Проспал поклёвку."))))
        (:reel                                                                 ; keep the slider in the green: holding the button pulls it right
         (let ((green (+ 118.0 (* 100.0 (sin (* time 1.6))) (* 18.0 (sin (* time 4.3))))))
           (setf pos (max 0.0 (min 296.0 (+ pos (* delta (if down 150.0 -120.0))))))
           (if (< green pos (+ green 60)) (incf good delta) (incf bad delta))
           (move-rect zone (+ 170 green) 316)
           (move-rect mark (+ 170 pos) 312)
           (move-rect bob (+ 318 (* 30 (sin (* time 5.0)))) 150)
           (object-set title "text"
                       (format nil "ТЯНИ! Держи белый бегунок в зелёном: зажал ЛКМ — вправо, отпустил — влево · ~d%"
                               (round (* 100 (/ good 3.5)))))
           (cond ((>= good 3.5) (finish-play 1 "Вытащил!"))
                 ((>= bad 1.6) (again "Сорвалась.")))))))))

;;; ------------------------------------------------------------------ mine: ten strikes to find the seam

(defun open-mine ()
  (play-rect 0 0 640 360 '(.03 .03 .04) .9)
  (let ((wide 8) (rows 4) (coal '()))
    (loop while (< (length coal) 5) do (pushnew (random (* wide rows)) coal))
    (with-play (cells cols strikes found title)
      (setf cells (loop for i below (* wide rows)
                        collect (let ((x (+ 152 (* 42 (mod i wide)))) (y (+ 90 (* 42 (floor i wide)))))
                                  (list (play-rect x y 40 40 '(.3 .28 .26)) x y (and (member i coal) t) nil)))
            cols wide strikes 10 found 0
            title (play-label "" 100 38 9)))))

(defun step-mine (click)
  (with-play (cells cols strikes found title)
    (when (and click (plusp strikes))
      (multiple-value-bind (mx my) (play-mouse)
        (let ((i (position-if (lambda (c) (and (not (fifth c)) (< (second c) mx (+ (second c) 40)) (< (third c) my (+ (third c) 40)))) cells)))
          (when i
            (let ((c (nth i cells)))
              (decf strikes)
              (setf (fifth c) t)
              (cond ((fourth c)                                                 ; coal
                     (incf found)
                     (object-set (first c) "color" (color .06 .06 .08 1.0))
                     (play-rect (+ (second c) 12) (+ (third c) 10) 8 6 '(.6 .6 .7)))
                    (t (let ((near (loop for j below (length cells)              ; barren rock: how much coal is around
                                         count (and (fourth (nth j cells)) (/= i j)
                                                    (<= (abs (- (mod i cols) (mod j cols))) 1) (<= (abs (- (floor i cols) (floor j cols))) 1)))))
                         (object-set (first c) "color" (color .46 .42 .36 1.0))
                         (play-label (if (zerop near) "" (format nil "~d" near)) (+ (second c) 15) (+ (third c) 12) 12 '(.1 .08 .06))))))))))
    (object-set title "text" (format nil "ШАХТА · ударов: ~d · угля: ~d из 3 · цифра — сколько угля в соседних клетках" strikes found))
    (cond ((>= found 3) (finish-play found "Ящик полон!"))
          ((zerop strikes) (finish-play found "Кирка затупилась. Пустая порода")))))

;;; ------------------------------------------------------------------ goat: milk to the beat

(defun open-goat ()
  (play-rect 0 0 640 360 '(.06 .05 .04) .86)
  (with-play (bar zone mark pail title pulls good phase speed)
    (setf bar (play-rect 170 200 300 14 '(.24 .2 .16))
          zone (play-rect 296 200 48 14 '(.3 .62 .3))
          mark (play-rect 170 195 5 24 '(1 1 1))
          pail (play-rect 300 270 40 4 '(.92 .92 .88))
          title (play-label "" 100 38 9)
          pulls 6 good 0 phase 0.0 speed 1.0)))

(defun step-goat (delta click)
  (with-play (phase speed mark pulls good pail title)
    (incf phase (* delta speed 5.2))
    (let ((at (+ 148.0 (* 148.0 (sin phase)))))
      (move-rect mark (+ 170 at) 195)
      (when (and click (plusp pulls))
        (decf pulls)
        (when (< 124 at 176)                                                   ; in the green: a squirt into the pail
          (incf good)
          (object-set pail "size" (vector2 40 (+ 4 (* 6 good))))
          (move-rect pail 300 (- 270 (* 6 good))))
        (setf speed (* 1.12 speed)))                                           ; Manka gets used to it, so the beat quickens
      (object-set title "text"
                  (format nil "КОЗА МАНЬКА · дои в такт: ЛКМ, когда бегунок в зелёном · осталось ~d · в ведре ~d" pulls good))
      (when (zerop pulls) (finish-play good (if (>= good 4) "Полное ведро!" "Манька лягнула ведро"))))))

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
