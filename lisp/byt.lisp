;;;; Household: everything bolted to the wagon that is used from inside. A shelf unit across the whole front
;;;; wall, a fold-down bunk shelf on the wall with the rug and a tea table in its place, the big window (two
;;;; sliding panes, windowsills, shutters, a step for customers, a bell), the stove, the light switch, the map
;;;; on the wall, the thermometer. In winter the wagon grows cold unless the stove is burning: frost on the
;;;; panes, frost at the screen edges. Also here are the on-screen gauges (tank, rubles) and the list of
;;;; 'things to press' (USABLES): what you look at and press E or LMB on.

(in-package #:kiosk-trip)

;;; The shelf unit

(defparameter *shelf-x* '(-1.42 1.42) "The x extent, from and to, of the shelf unit that spans the whole front wall.
The record player and the discs sit on the left of its bottom shelf.")
(defparameter *shelf-z* -2.5)
(defparameter *shelf-tops* '(.55 1.1 1.65) "The heights of the shelves above the floor.")

(defun make-shelf (c)
  (destructuring-bind (x0 x1) *shelf-x*
    (dolist (x (list x0 (+ x0 (/ (- x1 x0) 3)) (- x1 (/ (- x1 x0) 3)) x1))
      (dolist (dz '(-.24 .24))
        (shape c :box '(.05 2.1 .05) "#34363a" x (+ *floor* 1.05) (+ *shelf-z* dz) :metal .5 :rough .5)))
    (dolist (top *shelf-tops*)
      (shape c :box (list (- x1 x0) .04 .54) "#8a6440" (/ (+ x0 x1) 2) (+ *floor* top -.02) *shelf-z*))))

;;; The overhead racks are like luggage racks on a train: right under the roof along both long walls, above
;;; the cards and pennants. You put the same things on them as on the shelf unit, looking up (SHELF-SPOT in trade.lisp).
(defparameter *rack-top* 2.28 "The height of the overhead racks above the floor.")
(defparameter *rack-z* '(-2.0 2.4) "The z extent, from and to, of the overhead racks along the wall.")

(defun make-racks (c)
  "Build the overhead racks under C as a grating, like luggage nets on a train.
They are lengthwise slats with sparse crossbars, so from below you can see what lies on them
(a solid board right under the roof could not be seen through by a cat below)."
  (destructuring-bind (z0 z1) *rack-z*
    (dolist (sx '(-1 1))
      (let ((x (* sx (- *half-width* .2))) (y (+ *floor* *rack-top* -.012)) (mid (/ (+ z0 z1) 2)))
        (dolist (dx '(-.15 -.05 .05 .15))                                              ; slats along the wall
          (shape c :box (list .022 .022 (- z1 z0)) "#8a6440" (+ x dx) y mid :shadow nil))
        (loop for z from z0 to z1 by .55                                               ; crossbars
              do (shape c :box '(.34 .018 .022) "#5a3e26" x (- y .02) z :shadow nil))
        (shape c :box (list .02 .06 (- z1 z0)) "#5a3e26" (- x (* sx .18)) (+ y .03) mid :shadow nil)   ; a rim on the room side
        (loop for z from (+ z0 .2) to (- z1 .2) by 1.1
              do (shape c :box '(.34 .03 .03) "#34363a" x (- y .19) z :rot (list 0 0 (* sx -.6)) :metal .5 :rough .5))))))

(defun fixture-surfaces ()
  "Return the surfaces things can be put on besides the floor and other things.
Each entry is (x z width depth height)."
  (destructuring-bind (x0 x1) *shelf-x*
    (append (loop for top in *shelf-tops* collect (list (/ (+ x0 x1) 2) *shelf-z* (- x1 x0) .54 top))
            (destructuring-bind (z0 z1) *rack-z*                                       ; overhead racks
              (loop for sx in '(-1 1) collect (list (* sx (- *half-width* .2)) (/ (+ z0 z1) 2) .36 (- z1 z0) *rack-top*)))
            (list (stove-surface))                                                 ; the stove top: cooking happens here (uyut.lisp)
            (fridge-surfaces) (list (dryer-surface))                               ; the fridge shelves and the dryer above the stove (fridge.lisp)
            ;; with the window slid open there is one shared windowsill: through the opening, from our edge to the
            ;; outer one. Both we and the customer put things on the same board, each as far as they can reach
            (when (> *window-k* .9)
              (list (list (+ *half-width* .12) 0.0 1.02 2.5 .95))))))

;;; The fold-down table and the seats by the window

(defvar *fold-down* nil "True when the table and the seats are folded down.")
(defvar *fold-k* 0.0 "The fold position: 0 is stowed against the wall, 1 is folded down.")
(defvar *fold-nodes* '() "The hinges of the fold-down shelf: entries (node stowed-angle).")

(defparameter *bunk-z* -1.25 "The z of the middle of the two bunks: they are by the rug wall, next to the rack.")
(defparameter *top-bunk* 1.4 "The height of the upper bunk above the floor.")

(defun make-top-bunk (c)
  "Build the upper bunk under C: a fixed shelf above the fold-down one, with a short ladder at its foot."
  (let* ((f *floor*) (g (group c (- .48 *half-width*) (+ f *top-bunk*) *bunk-z*)))
    (shape g :box '(.72 .06 1.9) "#5a3e26" 0 0 0)
    (shape g :box '(.68 .1 1.84) "#3f5f73" 0 .08 0 :shadow nil)                  ; mattress
    (shape g :box '(.5 .09 .34) "#e8e0d0" 0 .17 -.72 :shadow nil)                ; pillow
    (shape g :box '(.66 .03 1.0) "#a8843c" 0 .145 .38 :shadow nil)               ; blanket
    (shape g :box '(.03 .12 1.9) "#3a2a1a" .36 .09 0)                            ; a rail so nobody rolls off
    (dolist (sz '(-.9 .9))                                                       ; brackets to the wall
      (shape g :box '(.6 .04 .04) "#34363a" -.04 -.2 sz :rot (list 0 0 .5) :metal .5 :rough .5))
    (dolist (sx '(.12 .34))                                                      ; the ladder at the foot
      (shape g :box '(.035 1.4 .035) "#5a3e26" sx -.7 .98))
    (dolist (y '(-1.1 -.75 -.4 -.05)) (shape g :box '(.26 .03 .035) "#8a6440" .23 y .98))))

(defun make-fold (c)
  "Build the fold-down bunk shelf under C on the wall with the rug, as on a train.
Raised, it is pressed to the wall; folded down, it is a bed. Above it is the fixed upper bunk."
  (setf *fold-nodes* '())
  (make-top-bunk c)
  ;; the hinge is 30 cm from the wall: the raised shelf stands on edge in front of the rug, mattress to the wall
  ;; (otherwise it would sink into the wall itself), underside to the room; folded down, it reaches the wall
  (let* ((x (- .3 *half-width*)) (f *floor*) (h (group c x (+ f .5) *bunk-z*)))
    (shape h :box '(.72 .06 1.9) "#5a3e26" .18 0 0)
    (shape h :box '(.68 .1 1.84) "#7a3a34" .18 .08 0 :shadow nil)                ; mattress
    (shape h :box '(.5 .09 .34) "#e8e0d0" .18 .17 -.72 :shadow nil)              ; pillow
    (shape h :box '(.66 .03 1.0) "#3f6f73" .18 .145 .38 :shadow nil)             ; blanket
    (dolist (sz '(-.8 .8)) (shape h :box '(.5 .03 .06) "#3a2a1a" .2 -.045 sz))   ; crossbars underneath
    (dolist (sz '(-.7 .7)) (shape h :box '(.04 .05 .22) "#d8a840" .55 -.01 sz :metal .6 :rough .4 :shadow nil))   ; handles on the edge
    (push (list h (/ pi 2)) *fold-nodes*)
    (apply-fold)))

(defun apply-fold ()
  (loop for (node stowed) in *fold-nodes*
        do (node-3d-set-rotation node (v3 0 0 (* stowed (- 1 *fold-k*)))))
  ;; the shelf is fully raised: the tea table stands in its place
  (when *table*
    (let ((up (< *fold-k* .05)))
      (unless (eq up (object-get *table* "visible"))
        (object-set *table* "visible" up)
        (when up (reset-table))))))

;;; The tea table: it stands in the shelf's place while the shelf is raised. One stool per player in the wagon.
;;; On the table are the kettle (kettle.lisp: E pours a cup of tea once it has boiled), cups (E drinks, you get warmer) and bagels (E eats one).

(defparameter *table-at* '(-.82 -1.25) "The x and z of the middle of the table: where the lower bunk is.")
(defparameter *table-top* .5 "The height of the tabletop above the floor.")
(defparameter *stools* '((-.82 -2.07 3.1416) (-.82 -.43 0.0) (-.18 -1.55 1.5708) (-.18 -.95 1.5708))
  "The stools: entries (x z facing-of-the-sitter).
As many are placed as there are players.")
(defparameter *cup-spots* '((-.82 -1.61) (-.82 -.89) (-.56 -1.55) (-.56 -.95)) "The spots on the table where a cup goes, one by each stool.")
(defvar *table* nil "The table node with everything on it.")
(defvar *stool-nodes* '())
(defvar *cup-nodes* '())
(defvar *bagel-nodes* '())
(defvar *stools-shown* 0)

(defun make-table (c)
  (let* ((f *floor*) (tx (first *table-at*)) (tz (second *table-at*)) (top *table-top*)
         (g (group c 0 f 0)))
    (shape g :box '(.62 .05 1.0) "#8a6440" tx top tz)
    (shape g :box '(.58 .012 .96) "#e8e0d0" tx (+ top .03) tz :shadow nil)                 ; tablecloth
    (dolist (sx '(-.26 .26)) (dolist (sz '(-.44 .44)) (shape g :box '(.05 .5 .05) "#5a3e26" (+ tx sx) (/ top 2) (+ tz sz))))
    ;; a plate of bagels
    (shape g :cyl '(.2 .16 .02 14) "#f0ede4" (+ tx .08) (+ top .045) (+ tz .0) :shadow nil)
    (setf *bagel-nodes*
          (loop for i below 6 for a = (* i (/ (* 2 pi) 6))
                collect (let ((bg (group g (+ tx .08 (* .115 (cos a))) (+ top .07) (+ tz (* .115 (sin a))))))
                          (shape bg :cyl '(.065 .065 .028 10) "#c8964a" 0 0 0 :shadow nil)
                          (shape bg :cyl '(.026 .026 .03 6) "#f0ede4" 0 0 0 :shadow nil)
                          bg)))
    (setf *cup-nodes*
          (loop for (x z) in *cup-spots*
                collect (let ((cup (group g x (+ top .035) z)))
                          ;; a tea glass in a metal holder, as on a train
                          (shape cup :cyl '(.03 .03 .085 10) "#8a4a1c" 0 .058 0 :shadow nil)                           ; tea
                          (shape cup :cyl '(.036 .034 .11 10) "#d8e8f0" 0 .065 0 :alpha .28 :rough .1 :shadow nil)      ; the glass
                          (shape cup :cyl '(.041 .041 .06 10) "#b0b4b8" 0 .04 0 :metal .8 :rough .35)                  ; the holder
                          (shape cup :cyl '(.05 .052 .012 10) "#b0b4b8" 0 .006 0 :metal .8 :rough .35 :shadow nil)     ; its foot
                          (shape cup :box '(.008 .06 .012) "#b0b4b8" .072 .045 0 :metal .8 :rough .35 :shadow nil)     ; the handle
                          (dolist (y '(.018 .072)) (shape cup :box '(.034 .008 .012) "#b0b4b8" .057 y 0 :metal .8 :rough .35 :shadow nil))
                          (object-set cup "visible" nil)
                          cup)))
    (setf *stool-nodes*
          (loop for (x z) in *stools*
                collect (let ((st (group g x 0 z)))
                          (shape st :cyl '(.17 .17 .04 10) "#7a3a34" 0 .32 0)
                          (dolist (a '(.5 2.6 4.7)) (shape st :box '(.04 .3 .04) "#5a3e26" (* .11 (cos a)) .15 (* .11 (sin a))))
                          (object-set st "visible" nil)
                          st)))
    (setf *table* g *stools-shown* 0)
    (object-set g "visible" (< *fold-k* .05))))

(defun reset-table ()
  "Lay the table anew: six bagels, and the cups empty (put away)."
  (dolist (n *bagel-nodes*) (object-set n "visible" t))
  (dolist (n *cup-nodes*) (object-set n "visible" nil)))

(defun table-shown-p () (and *table* (not *fold-down*) (< *fold-k* .05)))

(defun step-table ()
  "Show one stool per player in the wagon, four at most."
  (when *table*
    (let ((n (min (length *stools*) (1+ (length *net-others*)))))
      (unless (= n *stools-shown*)
        (setf *stools-shown* n)
        (loop for st in *stool-nodes* for i from 0 do (object-set st "visible" (< i n)))
        (refresh-seats)))))

(defun pour-tea ()
  (let ((cup (find-if-not (lambda (c) (object-get c "visible")) (subseq *cup-nodes* 0 (max 1 *stools-shown*)))))
    (cond (cup (object-set cup "visible" t) (say "Налил стакан чая") t)
          (t (say "Стаканы полные — пейте") nil))))

(defun drink-tea (cup)
  "Drink the glass CUP: what it does depends on what was brewed in the kettle (kettle.lisp)."
  (object-set cup "visible" nil)
  (case *kettle-brew*
    (:compote (setf *warm* (min 1.0 (+ *warm* .3))) (say "Компот. Сладко и тепло"))
    (:herbal (setf *warm* (min 1.0 (+ *warm* .15)) *boost-until* (+ *time* *water-time*)) (say "Травяной чай. Бодрит: пять минут бегаешь быстрее"))
    (:mors (setf *warm* (min 1.0 (+ *warm* .2)) *boost-until* (+ *time* (/ *water-time* 2))) (say "Клюквенный морс. Кисло и бодро"))
    (:trip (setf *trip-until* (+ *time* *trip-time*)) (say "Мухоморный отвар… Стены дышат. Гномики, кажется, машут"))
    (t (setf *warm* (min 1.0 (+ *warm* .15))) (say "Чай. Теплее"))))

(defun eat-bagel ()
  (let ((b (find-if (lambda (n) (object-get n "visible")) *bagel-nodes*)))
    (cond (b (object-set b "visible" nil) (say "Хрум. Сушка"))
          (t (say "Сушки кончились. Опусти и подними полку — стол накроют заново")))))

(defun table-usables ()
  (when (table-shown-p)
    (let ((tx (first *table-at*)) (tz (second *table-at*)) (top *table-top*))
      (append
       (list (usable tx top tz nil nil nil 2.6 .5)                 ; the tabletop itself is a blocker: the stools are behind it
             (usable (+ tx .08) (+ top .08) tz
                   (lambda () (if (some (lambda (n) (object-get n "visible")) *bagel-nodes*) "E — съесть сушку" "Сушки кончились"))
                   #'eat-bagel nil 2.6 .22))
       (loop for cup in *cup-nodes* for (x z) in *cup-spots*
             when (object-get cup "visible")
               collect (let ((cup cup)) (usable x (+ top .07) z (lambda () "E — выпить чай") (lambda () (drink-tea cup)) nil 2.6 .09)))))))

(defun seat-usables ()
  "Return the usables for seats you can sit on by looking at them: stools and mattresses.
The bunk shelf is handled separately (FIXTURE-USABLES)."
  (unless *seat*
    (loop for seat in (seats-here)
          unless (member (first seat) '(:fold :top))
            collect (let ((seat seat))
                      (destructuring-bind (what x z yaw eye) seat
                        (declare (ignore what))
                        (usable x (max .2 (- eye .6)) z (lambda () "E — сесть")
                                (lambda () (setf *seat* seat *pos* (v3 x 0 z) *yaw* yaw *pitch* -.05))
                                nil 2.6 .3))))))

(defun fold-seats ()
  "Return the places given by the bunks and the table.
On a bunk one lies, head on the pillow, looking up: the eyes are just above the mattress. The upper bunk
is always there; the lower one when it is folded down, otherwise there are the stools at the table."
  (cons (list :top (- .48 *half-width*) (- *bunk-z* .45) 3.1416 (+ *top-bunk* .25))
        (cond (*fold-down* (list (list :fold (- .48 *half-width*) (- *bunk-z* .45) 3.1416 .75)))
              (t (loop for (x z yaw) in *stools* for i below *stools-shown*    ; the shelf is raised: stools at the table
                       collect (list :stool x z yaw .86))))))

(defun toggle-fold ()
  (setf *fold-down* (not *fold-down*))
  (when (and (not *fold-down*) *seat* (eq (first *seat*) :fold)) (setf *seat* nil))
  (refresh-seats)
  (say (if *fold-down* "Полка откинута · E по ней — лечь" "Полка поднята")))

(defun lie-on-bunk (kind)
  "Lie down on the bunk KIND (:FOLD is the lower one, :TOP the upper), or get up if we lie there."
  (if (and *seat* (eq (first *seat*) kind))
      (setf *seat* nil)
      (let ((seat (assoc kind (fold-seats))))
        (when seat (setf *seat* seat *pos* (v3 (second seat) 0 (third seat)) *yaw* (fourth seat) *pitch* .55)))))

(defun sit-on-bunk () (lie-on-bunk :fold))

;;; The window: two sliding panes. Slide them apart and windowsills rise on both sides: the inner one
;;; (we put things on it) and the outer one (whoever came to the window puts things on it).

(defvar *window-open* nil)
(defparameter *step-y* 1.22 "The height of the customers' step above the road: a customer shows in the window from the chest up.")
(defvar *window-k* 0.0 "The window position: 0 is closed, 1 is slid open.")
(defvar *window-nodes* nil "The window nodes as a plist.
:panes holds entries (pane sign), :sills holds entries (hinge stowed-angle open-angle).")

(defun make-window (c)
  (let* ((x *half-width*) (f *floor*) (panes '()) (sills '()))
    (dolist (s '(-1 1))
      (let ((g (group c x (+ f 1.5) (* s .65))))
        (shape g :box '(.03 1.1 1.3) "#a8c4d0" 0 0 0 :alpha .22 :rough .1 :shadow nil)
        (dolist (dy '(-.55 .55)) (shape g :box '(.05 .04 1.3) "#34363a" 0 dy 0 :metal .5 :rough .5 :shadow nil))
        (dolist (dz '(-.65 .65)) (shape g :box '(.05 1.1 .04) "#34363a" 0 0 dz :metal .5 :rough .5 :shadow nil))
        (frost-pane g)
        (rain-pane g)                                                  ; mood.lisp
        (push (list g s) panes)))
    (let ((in (group c (- x .09) (+ f .95) 0)) (out (group c (+ x .09) (+ f .95) 0)))
      (shape in :box '(.3 .04 2.5) "#8a6440" -.15 -.02 0)
      (shape c :box '(.2 .04 2.5) "#8a6440" x (+ f .93) 0)                       ; a board in the opening itself: the windowsill is continuous
      ;; the lower shutter: closed, it hangs along the wall under the window; window slid open, it is the counter sill
      (shape out :box '(.55 .05 2.6) "#8a6440" .275 -.025 0)
      (shape out :box '(.06 .06 2.5) "#d8a840" .5 -.015 0 :shadow nil)
      (push (list in (/ pi 2) 0.0) sills)
      (push (list out (- (/ pi 2)) 0.0) sills))
    ;; the upper shutter: closed, it is pressed to the wall above the window; slid open, it is an awning over the counter
    (let ((up (group c (+ x .1) (+ f 2.07) 0)))
      (shape up :box '(.05 .55 2.6) "#8a6440" 0 .275 0)
      (shape up :box '(.06 .06 2.5) "#d8a840" .01 .5 0 :shadow nil)
      (push (list up 0.0 (- (+ (/ pi 2) .3))) sills))
    ;; the step under the window outside: it folds out together with the windowsills, the customer stands on it
    (let ((step (group c (+ x .1) *step-y* 0)))
      (shape step :box '(.95 .06 2.3) "#5a3e26" .48 0 0)
      (shape step :box '(.9 .02 2.2) "#8a6440" .48 .04 0 :shadow nil)
      ;; legs and stairs are separate: they exist only while the step is folded out
      (let ((legs (group step 0 0 0)))
        (dolist (sz '(-1.0 1.0)) (shape legs :box '(.06 1.2 .06) "#34363a" .88 -.62 sz :metal .5 :rough .5))
        (loop for i below 3 do (shape legs :box '(.5 .04 .16) "#5a3e26" .62 (- -.3 (* i .3)) 1.25 :shadow nil))
        (setf *step-legs* legs))
      ;; folded, it hangs down along the side under the window (not up: it would cover the glass)
      (push (list step (- (/ pi 2)) 0.0) sills))
    ;; the bell hangs in the opening when the window is slid open: you call a customer with it
    (let ((bell (group c (+ x .02) (+ f 1.98) 0)))
      (shape bell :cyl '(.006 .006 .14 4) "#34363a" 0 -.07 0 :shadow nil)
      (shape bell :cyl '(.03 .075 .1 10) "#d8a840" 0 -.19 0 :metal .8 :rough .3 :shadow nil)
      (shape bell :ball '(.022 6 4) "#8a6a20" 0 -.26 0 :shadow nil)
      (setf *bell-node* bell))
    (setf *window-nodes* (list :panes panes :sills sills))
    (apply-window)))

(defvar *bell-node* nil)
(defvar *step-legs* nil)

(defun apply-window ()
  (when *bell-node* (object-set *bell-node* "visible" (> *window-k* .5)))
  (when *step-legs* (object-set *step-legs* "visible" (> *window-k* .7)))
  (loop for (g s) in (getf *window-nodes* :panes)
        do (node-3d-set-position g (v3 *half-width* (+ *floor* 1.5) (* s (+ .65 (* 1.3 *window-k*))))))
  (loop for (g stowed open) in (getf *window-nodes* :sills)
        do (node-3d-set-rotation g (v3 0 0 (+ stowed (* *window-k* (- open stowed)))))))

(defun toggle-window ()
  (when (and *window-open* (window-busy-p))
    (say (if *client* "У окна клиент — сначала отпусти его" "На подоконнике стоят вещи — сначала убери их"))
    (return-from toggle-window))
  (setf *window-open* (not *window-open*))
  (say (if *window-open* "Окно раздвинуто, подоконники подняты" "Окно закрыто")))

;;; The stove, planks, warmth

(defparameter *stove-at* '(-1.18 2.35) "The x and z of the stove: in the corner to the right of the door.")
(defvar *planks* 6 "The number of planks in stock.")
(defvar *fire* 0.0 "The number of seconds the fire will still burn.")
(defvar *warm* .7 "The warmth in the wagon, 0..1.")
(defvar *stove-glow* nil)
(defvar *stove-light* nil)
(defvar *cold-told* nil)

(defvar *stove-planks* '() "The planks sticking out of the firebox, in the order they burn away.")

(defun make-stove (c)
  "Build the stove under C: a box stove level with the washstand, with a wide flat top for the frying pan,
a wing with a ring for the kettle, and planks that stick out of the firebox while they burn."
  (destructuring-bind (x z) *stove-at*
    (let* ((g (group c x *floor* z)) (iron "#1e1e22") (top *stove-top*) (legs .12) (body (- top .04 legs)) (mid (+ legs (/ body 2))))
      (dolist (sx '(-.24 .24)) (dolist (sz '(-.2 .2)) (shape g :box (list .05 legs .05) iron sx (/ legs 2) sz)))
      (shape g :box (list .56 body .48) iron 0 mid 0 :metal .4 :rough .7)
      (shape g :box '(.64 .04 .54) "#2a2a30" 0 (- top .02) 0 :metal .4 :rough .6)
      (shape g :box '(.04 .24 .22) "#2a2a30" .29 mid 0)                           ; the firebox, its door open
      (setf *stove-glow* (shape g :box '(.045 .14 .16) "#ff8030" .295 mid 0 :emit "#ff6010" :energy 3.0 :shadow nil))
      ;; the planks in the fire: one for every plank's worth of burning left, the last thrown in burns away first
      (setf *stove-planks*
            (loop for i below 6
                  collect (shape g :box '(.46 .03 .07) "#8a6440" (+ .44 (* .012 i)) (+ mid -.05 (* .02 (mod i 3))) (- (* .05 (mod i 4)) .075)
                                 :rot (list 0 (* .12 (- (mod i 3) 1)) (* .06 (- (mod i 2) .5))) :shadow nil)))
      (shape g :cyl (list .07 .07 (- 4.2 top) 8) iron -.2 (/ (+ 4.2 top) 2) .17 :metal .4 :rough .7)   ; the pipe through the roof to the outside
      (shape g :box '(.36 .04 .36) "#3a3a40" -.2 2.72 .17 :metal .5 :rough .6)    ; the flashing on the roof
      (shape g :cyl '(.2 .02 .14 8) iron -.2 4.3 .17 :metal .4 :rough .7)         ; the rain cap
      (shape g :cyl '(.02 .02 .12 4) iron -.14 4.2 .17) (shape g :cyl '(.02 .02 .12 4) iron -.26 4.2 .17)
      ;; the wing to the right of the firebox: a ring where the kettle boils (kettle.lisp)
      (dolist (sx '(-.2 .2)) (shape g :box (list .05 legs .05) iron sx (/ legs 2) -.66))
      (shape g :box (list .5 body .46) iron 0 mid -.47 :metal .4 :rough .7)
      (shape g :box '(.58 .04 .5) "#2a2a30" 0 (- top .02) -.49 :metal .4 :rough .6)
      (shape g :cyl '(.12 .12 .016 12) "#3a3a40" 0 (+ top .006) -.49 :metal .5 :rough .5 :shadow nil)
      (shape g :cyl '(.07 .07 .02 10) "#5a2a1e" 0 (+ top .012) -.49 :emit "#ff5020" :energy .8 :shadow nil)   ; 12 mm above the ring under it
      (setf *stove-light* (light g "#ff7030" 0.0 4.5 .4 mid 0)))))

(defun show-fire ()
  "Show as many planks in the firebox as there is burning left, counted in planks."
  (let ((n (ceiling (max 0.0 *fire*) *plank-burn*)))
    (loop for plank in *stove-planks* for i from 0 do (object-set plank "visible" (< i n)))))

(defun looking-at (x y z &optional (reach 2.6) (cone .33))
  "Return true when the player looks at the wagon point (X, Y above the floor, Z).
The point must be no farther than REACH and within the angle CONE of the view direction."
  (let* ((ex (vector3-x *pos*)) (ey *eye-now*) (ez (vector3-z *pos*))
         (dx (- x ex)) (dy (- (+ *floor* y) ey)) (dz (- z ez)) (len (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
         (dir (look-dir)))
    (and (< len reach)
         (< (acos (max -1 (min 1 (/ (+ (* dx (vector3-x dir)) (* dy (vector3-y dir)) (* dz (vector3-z dir))) (max len .01))))) cone))))

(defshared burn-coal :coal ()
  "Put the coal from the paws into the stove: it burns three times longer than a plank.
The stove is shared."
  (when (and *carry* (eq (car *carry*) :coal))
    (setf *carry* nil *carry-from* nil *fire* (min 600.0 (+ *fire* (* *coal-burn* *plank-burn*))) *cold-told* nil)
    (show-carry) (save-interior)
    (say "Уголь в печке: гореть будет долго")))

(defun feed-stove ()
  (cond ((and *carry* (eq (car *carry*) :coal)) (burn-coal))
        ((<= *planks* 0) (say "Досок нет. X по ящику — разобрать на доски"))
        (t (decf *planks*) (setf *fire* (min 300.0 (+ *fire* *plank-burn*)) *cold-told* nil)
           (when (net-guest-p) (incf *net-fed*))            ; tell the driver that a plank went into the stove
           (say (format nil "Подбросил доску. Осталось ~d" *planks*)))))

;;; Things you press E (or LMB) on while looking at them: the stove, the table, the light switch, the shutter
;;; lever, the curtain cord, the ladder. Everything that used to hang on separate keys.

(defparameter *switch-at* '(.9 1.35 2.78) "The light switch by the door: x, height above the floor, z.")
(defparameter *map-at* '(1.45 1.3 -1.77) "The position of the map on the window wall, to the left of the window: x, height above the floor, z.")

(defun make-controls (c)
  (let ((f *floor*))
    (destructuring-bind (x y z) *switch-at*
      (shape c :box '(.13 .17 .03) "#d8d2c0" x (+ f y) z :shadow nil)
      (shape c :box '(.04 .07 .03) "#2a2a30" x (+ f y) (- z .02) :shadow nil))
    ;; the thermometer on the wall by the window: the red column grows with warmth
    (let ((g (group c (- *half-width* .1) (+ f 1.25) 1.5)))
      (shape g :box '(.03 .5 .11) "#e8e4d8" 0 .25 0 :shadow nil)
      (dotimes (i 6) (shape g :box '(.032 .006 .04) "#34363a" -.002 (+ .06 (* i .08)) .03 :shadow nil))
      (shape g :ball '(.03 8 5) "#c02a22" -.02 .03 -.02 :shadow nil)
      (setf *thermo* (shape g :box '(.034 1.0 .022) "#c02a22" -.02 .05 -.02 :shadow nil)))
    ;; the map on the window wall, between the rack and the window
    (destructuring-bind (x y z) *map-at*
      (shape c :box '(.03 .62 .8) "#5a3e26" x (+ f y) z)
      (shape c :box '(.03 .56 .74) "#d8d0b8" (- x .012) (+ f y) z :shadow nil)
      (loop for i below 6 do (shape c :ball '(.022 6 4) "#a3262c" (- x .03) (+ f y (* .12 (sin (* i 1.9)))) (+ z -.28 (* i .112)) :shadow nil))
      (shape c :box '(.01 .012 .62) "#6a5640" (- x .03) (+ f y) z :rot '(.12 0 0) :shadow nil))))

(defparameter *washstand-at* '(1.18 2.27) "The x and z of the basin of the washstand.")
(defparameter *washstand-top* .77 "The height of the washstand worktop: between a stool and the stove top.")

(defun make-washstand (c)
  "Build the washstand under C in the corner between the window and the door: a long worktop with an
enamel basin and a water tank on the wall, and a cutting board with a knife on its left half."
  (let* ((f *floor*) (top *washstand-top*) (g (group c (first *washstand-at*) f (- (second *washstand-at*) .3))))
    (shape g :box '(.58 .04 1.3) "#8a6440" 0 top 0)                                              ; the worktop
    (shape g :box (list .54 (- top .02) .62) "#5f7f7a" 0 (/ (- top .02) 2) .32)                   ; the cupboard under the basin
    (shape g :box (list .02 (- top .14) .56) "#4f6f6a" -.275 (/ top 2) .32 :shadow nil)
    (shape g :ball '(.02 6 4) "#d8a840" -.295 (+ (/ top 2) .06) .12 :metal .6 :rough .4 :shadow nil)
    (dolist (sx '(-.25 .25)) (shape g :box (list .04 (- top .02) .04) "#5a3e26" sx (/ (- top .02) 2) -.61))    ; legs under the board half
    (shape g :cyl '(.24 .19 .11 14) "#e8e4d8" 0 (+ top .075) .3 :rough .3)                        ; the basin
    (shape g :cyl '(.2 .2 .012 14) "#8ab0c4" 0 (+ top .136) .3 :rough .1 :shadow nil)             ; water in it: a centimetre proud of the rim, or the two flicker
    (shape g :cyl '(.1 .09 .22 12) "#d8d2c0" .2 (+ top .34) .3 :rough .4)                         ; the tank on the wall
    (shape g :cyl '(.105 .105 .02 12) "#b8b2a0" .2 (+ top .46) .3 :shadow nil)
    (shape g :cyl '(.012 .012 .07 6) "#8a8e92" .2 (+ top .2) .3 :metal .6 :rough .4 :shadow nil)  ; the push rod: lift it and water runs
    (shape g :box '(.34 .025 .5) "#d8b880" -.02 (+ top .032) -.3 :shadow nil)))                   ; the cutting board; its knife chops (kettle.lisp)

;;; Interaction points. Everything that can be pressed has a position and a size (RADIUS); what fires is what the
;;; gaze rests on: we compute how far the gaze ray missed the middle of the thing in fractions of its size, and
;;; take the one that was hit most precisely. A small thing in front of a big one (a cup on the table, a radio by
;;; the window) wins when you look right at it. A thing with no action is a blocker: looking at the table means
;;; the table, not the stool behind it. You can sit only on the seat you are looking at.

(defstruct (usable (:constructor usable (x y z label action &optional hand (reach 2.6) (radius .22))))
  "A thing you look at and press: a point in the wagon (X, Y above the floor, Z) and its size RADIUS.
LABEL is a function that returns the hint, ACTION is what to do (NIL makes a blocker: it does
nothing but hides what is behind it). With HAND it is taken by hand (LMB), otherwise it is an
interaction (E)."
  x y z label action hand reach radius)

(defun fixture-usables ()
  "Return the usables for what is bolted to the wagon.
These are the map, the stove, the light switch, the ladder, the window and the bunk shelf."
  (remove nil
          (list
           (usable (first *map-at*) (second *map-at*) (third *map-at*) (lambda () "E — карта: куда ехать") #'open-map)
           (usable (+ (first *stove-at*) .3) (* *stove-top* .5) (second *stove-at*)
                   (lambda () (if (> *planks* 0) (format nil "E — подбросить доску · осталось ~d" *planks*) "Досок нет — купи на заправке (P)"))
                   #'feed-stove)
           (usable (first *switch-at*) (second *switch-at*) (third *switch-at*)
                   (lambda () (if *lights-on* "E — выключить свет" "E — включить свет"))
                   (lambda () (setf *lights-on* (not *lights-on*)) (apply-lights)))
           ;; the ladder is beyond the vestibule: you cannot reach it through a closed door
           (when (or (> *door-k* .6) (> (vector3-z *pos*) 2.95))
             (usable 0.0 .1 3.95 (lambda () (if *ladder-down* "E — поднять лесенку" "E — опустить лесенку")) #'toggle-ladder))
           ;; the window: anywhere on the glass, wherever you look
           (usable 1.5 1.5 -.9 #'window-label #'toggle-window nil 2.6 .5)
           (usable 1.5 1.5 0.0 #'window-label #'toggle-window nil 2.6 .5)
           (usable 1.5 1.5 .9 #'window-label #'toggle-window nil 2.6 .5)
           ;; the bunk shelf: raised, E on it folds it down; folded down, E on it seats you,
           ;; E on the yellow handles on the edge or on the wall above it raises it
           (when *fold-down* (usable (- .85 *half-width*) .5 (- *bunk-z* .7) (lambda () "E — поднять полку") #'toggle-fold))
           (when *fold-down* (usable (- .85 *half-width*) .5 (+ *bunk-z* .7) (lambda () "E — поднять полку") #'toggle-fold))
           (if *fold-down*
               (usable (- .5 *half-width*) .62 *bunk-z*
                       (lambda () (if (and *seat* (eq (first *seat*) :fold)) "E — встать" "E — лечь на полку")) #'sit-on-bunk)
               (usable (- .32 *half-width*) .8 *bunk-z* (lambda () "E — откинуть полку") #'toggle-fold))
           ;; raised, the shelf stands on edge and its golden handles are up at chest height: E on either folds it down
           (unless *fold-down* (usable (- .31 *half-width*) 1.05 (- *bunk-z* .7) (lambda () "E — откинуть полку") #'toggle-fold nil 2.6 .16))
           (unless *fold-down* (usable (- .31 *half-width*) 1.05 (+ *bunk-z* .7) (lambda () "E — откинуть полку") #'toggle-fold nil 2.6 .16))
           (usable (- .1 *half-width*) 1.05 *bunk-z*
                   (lambda () (if *fold-down* "E — поднять полку" "E — откинуть полку")) #'toggle-fold)
           ;; the upper bunk: E on its mattress
           (usable (- .5 *half-width*) (+ *top-bunk* .15) *bunk-z*
                   (lambda () (if (and *seat* (eq (first *seat*) :top)) "E — слезть" "E — лечь на верхнюю полку"))
                   (lambda () (lie-on-bunk :top)) nil 2.6 .3))))

(defun usables ()
  "Return everything that can be pressed right now.
The order matters: the first one that fits the crosshair is the one that fires."
  (unless *meta*
    (append (trade-usables) (kettle-usables) (fridge-usables) (table-usables) (seat-usables) (uyut-usables) (stars-usables) (finds-usables) (dalny-usables) (prikol-usables) (gnome-usables) (bumagi-usables) (fixture-usables))))

(defun window-label () (if *window-open* "E — закрыть окно" "E — раздвинуть окно"))

(defun usable-miss (u)
  "Return how far the gaze missed the thing U, in fractions of its size.
A value below 1 is a hit; NIL means it is out of reach."
  (let* ((ex (vector3-x *pos*)) (ey *eye-now*) (ez (vector3-z *pos*))
         (dx (- (usable-x u) ex)) (dy (- (+ *floor* (usable-y u)) ey)) (dz (- (usable-z u) ez))
         (len2 (+ (* dx dx) (* dy dy) (* dz dz))) (dir (look-dir))
         (along (+ (* dx (vector3-x dir)) (* dy (vector3-y dir)) (* dz (vector3-z dir)))))
    (when (and (> along 0.0) (< len2 (expt (usable-reach u) 2)))
      (/ (sqrt (max 0.0 (- len2 (* along along)))) (usable-radius u)))))

(defun usable-here (&optional hand)
  "Return the thing under the crosshair, or NIL.
With HAND, look for what is taken by hand (LMB); without it, for what is interacted with (E).
Blockers always take part: when a blocker is hit most precisely, it is what is under the crosshair
and there is no action."
  (let ((best nil) (least 1.0))
    (dolist (u (usables))
      (when (and (or (null (usable-action u)) (eq (and (usable-hand u) t) (and hand t)))
                 ;; standing outside, the things inside the wagon are behind its wall: not through it
                 (or *meta* (not (outdoors-p (vector3-x *pos*) (vector3-z *pos*))) (outdoors-p (usable-x u) (usable-z u))))
        (let ((miss (usable-miss u)))
          (when (and miss (< miss least)) (setf best u least miss)))))
    (and best (usable-action best) best)))                     ; the most precise hit was a blocker, so nothing is hit

(defun use-usable (&optional hand)
  "Press the thing under the crosshair, passing HAND on to USABLE-HERE.
Return T if there was one."
  (let ((u (usable-here hand)))
    (when u (funcall (usable-action u)) t)))

(defun break-crate ()
  "Handle X: break the crate the player is looking at into three planks.
Addressed parcels are not broken."
  (let ((it (fourth (aim-point))))
    (cond ((null it) (say "Смотри на ящик"))
          ((not (member (first (car it)) '(:crate :box))) (say "На доски идут только ящики"))
          ((sixth (car it)) (say "Это чужая посылка. Её ждут"))
          (t (node-queue-free (cdr it))
             (setf *items* (remove it *items*))
             (incf *planks* *crate-planks*)
             (settle-items)
             (refresh-seats) (save-interior)
             (say (format nil "Ящик разобран. Досок: ~d" *planks*))))))

;;; Frost on the windows

(defparameter *frost-code* "
shader_type spatial;
render_mode unshaded, cull_disabled, shadows_disabled;
uniform float frost = 0.0;
float h(vec2 p) { return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453); }
float n(vec2 p) {
  vec2 i = floor(p), f = fract(p); f = f * f * (3.0 - 2.0 * f);
  return mix(mix(h(i), h(i + vec2(1.0, 0.0)), f.x), mix(h(i + vec2(0.0, 1.0)), h(i + vec2(1.0, 1.0)), f.x), f.y);
}
void fragment() {
  vec2 e = min(UV, 1.0 - UV);
  float edge = 1.0 - smoothstep(0.0, 0.5, min(e.x, e.y) * 2.0);          // от рамы к середине
  float crystals = n(UV * vec2(34.0, 16.0)) * 0.6 + n(UV * vec2(90.0, 44.0)) * 0.4;
  float a = clamp(edge * 1.2 + crystals * 0.9 - 1.5 + frost * 1.9, 0.0, 1.0);
  ALBEDO = vec3(0.84, 0.90, 0.96);
  ALPHA = a * 0.92 * step(0.02, frost);
}")
(defvar *frost-mat* nil)
(defvar *frost-now* -1.0)

(defun make-frost (c)
  "Build the frost material for the panes in the openings under C.
The openings are the big window (x+) and the two small windows (x-)."
  (let ((sh (gdext:make-object "Shader")) (mat (gdext:make-object "ShaderMaterial")))
    (object-set sh "code" *frost-code*)
    (object-set mat "shader" sh)
    (gdext:setf-reference *frost-mat* mat)
    (setf *frost-now* -1.0)
    ;; the frosted panes themselves are hung by MAKE-WINDOW on the sashes, so the frost slides apart with them
    ))

(defun frost-pane (g)
  "Put frost on the sash G."
  (when *frost-mat*
    (let ((mi (gdext:make-object "MeshInstance3D")) (q (gdext:make-object "QuadMesh")))
      (object-set q "size" (vector2 1.3 1.1))
      (object-set mi "mesh" q)
      (object-set mi "material_override" *frost-mat*)
      (object-set mi "cast_shadow" 0)
      (node-add-child g mi)
      (node-3d-set-position mi (v3 -.03 0 0))
      (node-3d-set-rotation mi (v3 0 (- (/ pi 2)) 0)))))

;;; Parcels: a crate has an address, a town number

(defvar *drops* '() "The bus stops where parcels are awaited: entries (path-coordinate . town-number).")

(defun covered-p (spec specs)
  "Return true when something in SPECS stands on the thing SPEC."
  (destructuring-bind (k x y z &rest more) spec
    (declare (ignore k more))
    (some (lambda (o) (and (not (eq o spec)) (< .05 (- (third o) y) .5) (< (abs (- (second o) x)) .5) (< (abs (- (fourth o) z)) .6)))
          specs)))

(defun address-crates (specs)
  "Hand out addresses to the parcels in SPECS and return the new list.
Parcels are crates and boxes with nothing standing on them (the bed supports and the bottoms of
stacks are furniture, they are not delivered). The first three towns are used in turn."
  (let ((n -1))
    (mapcar (lambda (s) (if (and (member (first s) '(:crate :box)) (not (covered-p s specs)))
                            (append (subseq s 0 5) (list (mod (incf n) 3)))
                            (subseq s 0 5)))
            specs)))

(defun parcels-left ()
  "Return the parcels still on board as ((town-number . count) ...), in ascending town number."
  (let ((acc '()))
    (dolist (it *items*)
      (let ((addr (and (sixth (car it)) (parcel-town (sixth (car it))))))
        (when addr (incf (cdr (or (assoc addr acc) (first (push (cons addr 0) acc))))))))
    (sort acc #'< :key #'car)))

;;; The dashboard in the corner: tank needle, thermometer, ruble counter, planks and parcels by the piece

(defvar *hud* nil "The label with the network line above the gauges.")
(defvar *hud-timer* 0.0)
(defvar *hud-hidden* nil "True when H has hidden the gauges, the network line and the hints (for a pretty shot).")
(defvar *dash* nil "The dashboard (a Control).")
(defvar *dash-parts* nil "The dashboard parts as a plist: :needle :warm :fire :digits :planks :rows.")
(defun hud-rect (parent x y w h rgb &optional (a 1.0))
  (let ((r (gdext:make-object "ColorRect")))
    (node-add-child parent r)
    (object-set r "position" (vector2 x y)) (object-set r "size" (vector2 w h))
    (object-set r "color" (color (first rgb) (second rgb) (third rgb) a))
    r))

(defun hud-label (parent text x y &optional (size 8) (rgb '(.85 .82 .72)))
  (let ((l (gdext:make-object "Label")))
    (node-add-child parent l)
    (object-set l "position" (vector2 x y)) (object-set l "text" text)
    (object-call l "add_theme_font_size_override" "font_size" size)
    (object-call l "add_theme_color_override" "font_color" (color (first rgb) (second rgb) (third rgb) 1.0))
    l))

(defparameter *cold-code* "
shader_type canvas_item;
uniform float cold = 0.0;
float hash(vec2 p) { return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453); }
float noise(vec2 p) {
  vec2 i = floor(p); vec2 f = fract(p); f = f * f * (3.0 - 2.0 * f);
  return mix(mix(hash(i), hash(i + vec2(1.0, 0.0)), f.x), mix(hash(i + vec2(0.0, 1.0)), hash(i + vec2(1.0, 1.0)), f.x), f.y);
}
void fragment() {
  vec2 c = UV - 0.5;
  float edge = smoothstep(0.62 - 0.42 * cold, 0.78, length(c * vec2(1.0, 1.25)) * 1.35);
  float n = noise(UV * 26.0) * 0.6 + noise(UV * 70.0) * 0.4;
  float a = cold * edge * (0.45 + 0.55 * n);
  COLOR = vec4(0.86, 0.92, 0.98, clamp(a, 0.0, 0.9));
}
" "The shader for the frost at the screen edges.
The colder it is in the wagon, the farther the frost creeps toward the middle.")

(defvar *cold-mat* nil)
(defvar *cold-now* -1.0)
(defvar *thermo* nil "The column of the wall thermometer by the window.")

(defun make-hud (root)
  "Build the on-screen gauges under ROOT, only what is needed on the road.
The tank needle is at the bottom left and the rubles at the bottom right. Warmth is shown by the
thermometer on the wall by the window and cold by the frost at the screen edges; parcels are on
the map."
  (let ((layer (gdext:make-object "CanvasLayer")) (l (gdext:make-object "Label")) (d (gdext:make-object "Control"))
        (frost (gdext:make-object "ColorRect")) (sh (gdext:make-object "Shader")) (mat (gdext:make-object "ShaderMaterial")))
    (node-add-child root layer)
    ;; the frost goes under the gauges
    (object-set sh "code" *cold-code*) (object-set mat "shader" sh)
    (gdext:setf-reference *cold-mat* mat)
    (object-set frost "position" (vector2 0 0)) (object-set frost "size" (vector2 640 360))
    (object-set frost "material" mat) (object-set frost "mouse_filter" 2)
    (node-add-child layer frost)
    (setf *cold-now* -1.0)
    (node-add-child layer l)
    (object-set l "offset_left" 6.0) (object-set l "offset_top" 4.0)
    (object-call l "add_theme_font_size_override" "font_size" 10)
    (object-call l "add_theme_constant_override" "outline_size" 3)
    (object-call l "add_theme_color_override" "font_outline_color" (color 0 0 0 .8))
    (node-add-child layer d)
    (object-set d "position" (vector2 0 0))
    ;; the tank: a scale from 0 to 1, a red zone, a needle
    (hud-rect d 6 300 58 54 '(.10 .11 .12) .82)
    (hud-rect d 12 306 46 34 '(.86 .83 .72))
    (hud-rect d 14 330 10 4 '(.70 .20 .16))
    (dotimes (i 5) (hud-rect d (+ 15 (* i 9.7)) 309 1 (if (evenp i) 6 3) '(.15 .15 .15)))
    (hud-label d "0" 14 314 7 '(.15 .15 .15)) (hud-label d "1" 50 314 7 '(.15 .15 .15))
    (let ((needle (hud-rect d 34 312 2 26 '(.75 .12 .10))))
      (object-set needle "pivot_offset" (vector2 1 26))
      (hud-rect d 32 335 6 4 '(.15 .15 .15))
      (hud-label d "БЕНЗИН" 14 342 8)
      ;; rubles: a coin and a number under it
      (hud-rect d 596 306 30 30 '(.10 .11 .12) .82)
      (hud-rect d 601 311 20 20 '(.55 .42 .12))
      (hud-rect d 603 313 16 16 '(.86 .68 .24))
      (hud-label d "р" 607 311 12 '(.42 .30 .06))
      (let ((money (gdext:make-object "Label")))
        (node-add-child d money)
        (object-set money "position" (vector2 566 337)) (object-set money "size" (vector2 60 14))
        (object-set money "horizontal_alignment" 2)
        (object-call money "add_theme_font_size_override" "font_size" 12)
        (object-call money "add_theme_constant_override" "outline_size" 3)
        (object-call money "add_theme_color_override" "font_outline_color" (color 0 0 0 .8))
        (setf *dash-parts* (list :needle needle :money money
                                 ;; a dot in the middle of the screen: it shows what you look at and where E will press
                                 :cross (list (hud-rect d 318 178 4 4 '(0 0 0) .55) (hud-rect d 319 179 2 2 '(1 1 1) .9))))))
    (setf *hud* l *dash* d)))

(defun update-dash ()
  (destructuring-bind (&key needle money cross) *dash-parts*
    (dolist (c cross) (object-set c "visible" (and (not (outside-p)) (not *lobby-open*) (not *map-open*))))
    (object-set needle "rotation" (* 1.05 (- (* 2 *fuel*) 1)))
    (object-set money "text" (format nil "~d" *money*))))

(defun hud-text ()
  (format nil "~{~a~^~%~}"
          (remove nil (list (debt-hud)
                            (let ((net (net-hud))) (and (plusp (length net)) net))
                            (and *auto* (format nil "Автопилот → ~a · ~,1f км" (cdr *auto*) (/ (max 0 (way-to (car *auto*))) 1000)))))))

;;; Every frame

(defun step-byt (delta)
  (let ((target (if *window-open* 1.0 0.0)))
    (unless (< (abs (- target *window-k*)) .001)
      (incf *window-k* (* (- target *window-k*) (min 1 (* delta 5))))
      (when (< (abs (- target *window-k*)) .002) (setf *window-k* target))
      (apply-window)))
  (step-table)
  ;; the shelf
  (let ((target (if *fold-down* 1.0 0.0)))
    (unless (< (abs (- target *fold-k*)) .001)
      (incf *fold-k* (* (- target *fold-k*) (min 1 (* delta 6))))
      (apply-fold)))
  ;; the stove and warmth
  (let ((winter (eq *season* :winter)))
    (when (> *fire* 0) (decf *fire* delta))
    (setf *warm* (max 0.0 (min 1.0 (+ *warm* (* delta (cond ((> *fire* 0) .03) (winter -.012) (t .02)))))))
    (when (and winter (< *warm* .3) (not *cold-told*))
      (setf *cold-told* t)
      (say "Холодно. Подбрось доску в буржуйку: E по печке"))
    (let ((k (if (> *fire* 0) (+ .75 (* .25 (sin (* *time* 9)))) 0.0)))
      (when *stove-light* (object-set *stove-light* "light_energy" (* 2.2 k)))
      (when *stove-glow* (object-set *stove-glow* "visible" (> *fire* 0))))
    (show-fire)
    (let ((frost (if winter (max 0.0 (min 1.0 (* 2.0 (- .62 *warm*)))) 0.0)))
      (when (and *frost-mat* (> (abs (- frost *frost-now*)) .01))
        (setf *frost-now* frost)
        (object-call *frost-mat* "set_shader_parameter" "frost" frost))))
  ;; the thermometer by the window and the frost at the screen edges when it is cold in the wagon
  (when *thermo*
    (let ((h (+ .04 (* .4 *warm*))))
      (node-3d-set-scale *thermo* (v3 1 h 1))
      (node-3d-set-position *thermo* (v3 -.02 (+ .05 (/ h 2)) -.02))))
  (let ((cold (if (or *meta* (outside-p)) 0.0 (max 0.0 (min 1.0 (/ (- .5 *warm*) .5))))))
    (when (and *cold-mat* (> (abs (- cold *cold-now*)) .01))
      (setf *cold-now* cold)
      (object-call *cold-mat* "set_shader_parameter" "cold" cold)))
  (when (and *hud* (<= (decf *hud-timer* delta) 0))
    (setf *hud-timer* .3)
    (let ((text (if (or *meta* *hud-hidden*) "" (hud-text))))
      (unless (string= text (object-get *hud* "text")) (object-set *hud* "text" text)))
    (object-set *dash* "visible" (not (or *meta* *hud-hidden*)))
    (when *hint* (object-set *hint* "visible" (not *hud-hidden*)))
    (unless *meta* (update-dash))))

(defun make-byt (cart)
  "Build everything bolted down into the freshly assembled wagon CART."
  (make-shelf cart) (make-racks cart) (make-fold cart) (make-table cart) (make-stove cart) (make-frost cart) (make-controls cart) (make-washstand cart) (make-window cart))
