;;;; Goods and trade between towns. Every town makes something, which is sold cheap there, and is short
;;;; of something, which fetches a high price there (the tables are in economy.lisp). A seller at the window
;;;; offers only local goods, a buyer looks only for what is short: you cannot buy and resell the same thing
;;;; on the spot, the goods have to be hauled. Between towns, clients at bus stops follow the nearest town.

(in-package #:kiosk-trip)

(defun dish-p (kind) "Return the recipe entry if KIND is a cooked dish." (find kind *recipes* :key #'second))
(defun raw-of (kind) "Return the raw good the dish KIND is cooked from; for a non-dish return KIND itself." (let ((r (dish-p kind))) (if r (first r) kind)))
(defun good-name (kind) (cond ((dish-p kind) (third (dish-p kind))) (t (or (second (assoc kind *goods*)) "товар"))))
(defun good-base (kind) (or (third (assoc kind *goods*)) 5))
(defun town-makes (i) (first (nth i *town-trade*)))
(defun town-wants (i) (second (nth i *town-trade*)))

(defun trade-town (&optional town)
  "Return the number of the town whose rules trade follows here.
That is TOWN if given, otherwise the town nearest to the wagon."
  (or town
      (let ((best nil))
        (multiple-value-bind (x z) (road-point *dist*)             ; nearest on the map, not by path coordinate
          (flet ((far (tw) (multiple-value-bind (tx tz) (road-point (town-stop-d tw)) (+ (expt (- tx x) 2) (expt (- tz z) 2)))))
            (dolist (tw *towns* (and best (town-index best)))
              (when (or (null best) (< (far tw) (far best))) (setf best tw))))))
      0))

(defun trade-good (kind town)
  "Return the good a client of KIND brings (a seller: the shop's surplus) or wants (a buyer: what is short) in TOWN.
The plan decides (plan.lisp); when it has nothing to say, the town's nature does."
  (let* ((town (trade-town town))
         (feast (and (eq kind :buyer) (< (random 1.0) *holiday-chance*) (holiday-goods)))   ; on a holiday they come for its goods
         (list (cond (feast feast)
                     ((eq kind :seller) (or (surplus-goods town) (town-makes town)))
                     (t (or (short-goods town) (town-wants town))))))
    (when (member kind '(:buyer :seller)) (nth (random (length list)) list))))

(defun rush-p (&optional (town (trade-town)))
  "Return true during the factory whistle in Krasny Kut.
The shift is leaving the factory: buyers are many and they pay more."
  (and (eql town *rush-town*) (< (first *rush-hours*) (day-phase *day-t*) (second *rush-hours*))))

(defun trade-price (good kind town &optional maker)
  "Return the price of GOOD for a client of KIND in TOWN.
A seller is the shop, with the list price, the same everywhere, or with MAKER the maker itself, which
sells above the plan at an agreed price. A buyer gives an agreed price: the emptier the shop here, the
more. The town's power moves both for its own and for those it has quarrelled with."
  (let ((town (trade-town town)))
    (max 1 (round (* (standing-factor town kind)
                     (cond ((eq kind :buyer) (* (agreed-price town good) (if (member good (holiday-goods)) *holiday-markup* 1)))
                           (maker (* *maker-markup* (list-price good)))
                           (t (list-price good))))))))

(defun note-trade (good side &optional from-shop)
  "Record a deal: SIDE :sold means the town got one GOOD from us, :bought means we took one.
FROM-SHOP says that the thing sold carries the mark of a shop (the property :shop).
The shop's books change; a sale of what the town is short of is a deed in our favour with its power, and
a resale of a shop good is speculation: a deed against us, and one more for the inspectors."
  (let* ((town (trade-town (and *client* (getf *client* :town)))) (raw (raw-of good)) (turn nil))
    (cond ((eq side :sold)
           (let ((needed (>= (shortage town raw) .25)))
             (set-stock town raw (+ (stock town raw) 1))
             (cond (from-shop (note-resale) (setf turn (add-deeds town -1)))
                   (needed (setf turn (add-deeds town 1))))))
          (t (set-stock town raw (- (stock town raw) 1))))
    (save-plan)
    (when turn (say-standing turn))
    (when (and (eq side :sold) (dish-p good)) (note-quest :cook))))             ; the town council's dinner quest counts dishes sold             ; the town council's dinner quest counts dishes sold             ; the town council's dinner quest counts dishes sold

(defun way-pay (way)
  "Return what a private sender pays for a parcel that goes WAY metres by road: the farther, the more."
  (round (+ *parcel-base* (* *parcel-per-km* (/ way 1000)))))

(defun cargo-tariff (way)
  "Return the tariff of a plan's crate that goes WAY metres: a transfer, on the scale of an enterprise's money."
  (* *transfer-scale* (way-pay way)))

(defun delivery-pay ()
  "Return the pay from the recipient for a delivered parcel (more with the town council stamp)."
  (* *parcel-delivery* (if *stamp* *stamp-factor* 1)))

(defun client-good ()
  "Return the good of the current client: what a seller brings or what a buyer looks for.
While the client walks to the window the goods are in :GOODS as (z kind address), at the window they
are in :STALL as (node z kind address)."
  (when *client*
    (let ((items (or (mapcar #'rest (getf *client* :stall)) (getf *client* :goods))))
      (case (getf *client* :kind)
        (:buyer (let ((coins (find :coins items :key #'second))) (and coins (third coins))))
        ((:seller :ration) (second (first items)))))))

(defun wanted-p (kind)
  "Return true if the current buyer will take a good of KIND.
That is the good sought or a dish made from it (blini will do for milk)."
  (let ((want (client-good)))
    (and want (or (eq kind want) (eq (raw-of kind) want)))))

(defun market-note (i)
  "Return one line saying what the shop in town I has plenty of and what it has not been sent."
  (let ((plenty (surplus-goods i)) (short (short-goods i)))
    (format nil "~a: завезли ~:[всего понемногу~;~:*~{~a~^, ~}~]; не завезли ~:[ничего такого~;~:*~{~a~^, ~}~]" (town-name i)
            (mapcar #'good-name (subseq plenty 0 (min 3 (length plenty))))
            (mapcar #'good-name (subseq short 0 (min 3 (length short)))))))

(defvar *market-told* nil "The market that has already been announced.")

(defun step-market-note ()
  "Announce what is traded here once the wagon stops at a market (once per arrival)."
  (let ((place (place-here)))
    (cond ((and place (eq (first place) :market))
           (unless (or (eql *market-told* (third place)) (and *toast* (> (cdr *toast*) 0)))
             (setf *market-told* (third place))
             (say-all (format nil "Рынок. ~a~:[~;. Гудок: смена идёт с завода!~]" (market-note (third place)) (rush-p (third place))))))
          ((> *speed* 2.0) (setf *market-told* nil)))))

(defparameter *cut-rgb* '((:fish "#d89a8a") (:bread "#e8c080") (:mushroom "#d8c8a8") (:toadstool "#d8c8a8") (:apple "#f0e8c0") (:cans "#9a5a34"))
  "What can be cut on the board, and the colour of its slices: entries of (good colour).")

(defparameter *cut-dish-rgb* '((:fried "#b8782c"))
  "The dishes that keep the look of slices when cooked from a cut good, and the colour of the slices: (dish colour).")

;;; The record of a thing is (kind x height z spin [address [properties]]). The properties are a list of
;;; alternating keys and values that says what happened to this very thing: it was cut on the board, it
;;; came from a shop. A new property needs a line in *PROP-KEYS* and nothing else: the record, the save,
;;; the paws and the messages between players carry whatever is listed there.
(defparameter *prop-keys* '(:cut :shop)
  "The properties a thing may have, in the order they are written: :cut - cut on the board, :shop - bought in a shop at the list price.")

(defun clean-props (props)
  "Return PROPS with only the known keys that are true, in the order of *PROP-KEYS*: equal things get equal records."
  (loop for key in *prop-keys* when (getf props key) append (list key t)))

(defun props-ok-p (props)
  "Return true if PROPS is a properly written list of properties."
  (and (listp props) (ignore-errors (equal props (clean-props props)))))

(defun make-spec (kind x y z spin &optional addr props)
  "Return the record of a thing; the address and the properties are written only when there are any."
  (let ((props (clean-props props)))
    (append (list kind x y z spin) (cond (props (list addr props)) (addr (list addr))))))

(defun spec-props (spec) "Return the properties of the item record SPEC." (seventh spec))
(defun spec-prop (spec key) "Return the property KEY of the item record SPEC." (getf (seventh spec) key))
(defun spec-cut-p (spec) "Return true if the item record SPEC is a good that was cut." (spec-prop spec :cut))

(defun old-cut-spec (spec)
  "Return SPEC as it is written now: saves made before things had properties marked a cut good by a half turn."
  (if (and (= (length spec) 5) (eql (fifth spec) 2) (cut-look (first spec)))
      (make-spec (first spec) (second spec) (third spec) (fourth spec) 0 nil '(:cut t))
      spec))

(defun cuttable-p (kind) (and (assoc kind *cut-rgb*) t))

(defun cut-look (kind)
  "Return the colour of the slices if a thing of KIND, put down as cut, is shown as slices; else NIL."
  (second (or (assoc kind *cut-rgb*) (assoc kind *cut-dish-rgb*))))

(defun build-cut (g kind)
  "Build the good KIND cut up: a row of slices, raw ones on a small board, cooked ones on a plate."
  (if (assoc kind *cut-dish-rgb*)
      (shape g :cyl '(.11 .11 .012 12) "#e4e0d4" 0 .006 0)
      (shape g :box '(.2 .012 .14) "#c8a060" 0 .006 0))
  (dotimes (i 6) (shape g :box '(.018 .07 .1) (cut-look kind) (- (* i .03) .075) .05 0 :rot (list 0 0 -.35) :shadow nil)))

(defun build-good (g kind)
  "Build the good KIND as a thing in G: it stands on a shelf, is carried in paws, goes on the windowsill."
  (ecase kind
    (:driedapple (shape g :box '(.004 .16 .004) "#d8d2c0" 0 .08 0 :shadow nil)             ; apple rings on a string
                 (dotimes (i 5) (shape g :cyl '(.035 .035 .012 8) "#d8b070" 0 (+ .025 (* i .03)) 0 :rot (list (* .3 (- (mod i 2) .5)) 0 0) :shadow nil)))
    (:driedherb (dotimes (i 5) (shape g :box '(.008 .15 .008) "#7a7a4a" (* .012 (- i 2)) .075 0 :rot (list 0 0 (* .14 (- i 2))) :shadow nil))   ; a dry bunch
                (shape g :box '(.05 .012 .02) "#d8d2c0" 0 .13 0 :shadow nil))
    (:pickled (jar-model g 0 0 0 "#8a6a4a"))                                                ; jars closed at the crate (home.lisp)
    (:jarcompote (jar-model g 0 0 0 "#d8a040"))
    (:cannedfish (shape g :cyl '(.06 .06 .05 12) "#b0b4b8" 0 .025 0 :metal .7 :rough .3)     ; a flat tin
                 (shape g :cyl '(.062 .062 .025 12) "#2c4a7a" 0 .025 0 :shadow nil))
    (:nut (shape g :cone '(.07 .1 6) "#d8d2c0" 0 .05 0 :rot (list pi 0 0))                 ; nuts in a paper cone
          (dotimes (i 5) (shape g :ball '(.022 6 4) "#8a5a34" (* .03 (cos (* i 1.3))) .115 (* .03 (sin (* i 1.3))) :shadow nil)))
    (:sandwich (shape g :cyl '(.1 .1 .012 12) "#e4e0d4" 0 .006 0)
               (dolist (x '(-.045 .045))
                 (shape g :box '(.07 .02 .1) "#c8843c" x .024 0) (shape g :box '(.06 .012 .09) "#d89a8a" x .04 0 :shadow nil)))
    (:berrypie (shape g :cyl '(.1 .1 .012 12) "#e4e0d4" 0 .006 0)
               (shape g :cyl '(.085 .09 .04 12) "#d8a040" 0 .032 0)
               (shape g :cyl '(.065 .065 .012 10) "#a3202c" 0 .054 0 :shadow nil))
    (:soup (shape g :cyl '(.09 .07 .08 12) "#d8d2c0" 0 .04 0)
           (shape g :cyl '(.082 .082 .012 12) "#c8782c" 0 .08 0 :shadow nil)
           (shape g :box '(.012 .012 .12) "#b0b4b8" .05 .1 .02 :rot '(.5 0 0) :metal .6 :shadow nil))
    (:bakedapple (shape g :cyl '(.1 .1 .012 12) "#e4e0d4" 0 .006 0)
                 (dolist (x '(-.04 .04)) (shape g :ball '(.04 7 5) "#c8843c" x .05 0) (shape g :ball '(.016 5 3) "#d89a20" x .09 0 :shadow nil)))
    (:brittle (dotimes (i 3) (shape g :box '(.16 .02 .05) "#8a5a34" 0 (+ .012 (* i .022)) (* .01 (- i 1)) :rot (list 0 (* .2 (- i 1)) 0))))
    ((:mushroom :toadstool)                                                             ; a handful in a paper cone
     (shape g :cone '(.07 .1 6) "#d8d2c0" 0 .05 0 :rot (list pi 0 0))
     (loop for (x z) in '((-.03 .0) (.03 .02) (.0 -.03))
           do (shape g :cyl '(.012 .015 .06 6) "#e8e0d0" x .11 z :shadow nil)
              (shape g :ball '(.035 7 4) (if (eq kind :toadstool) "#c02a22" "#8a5a34") x .145 z :scale '(1 .6 1) :shadow nil)
              (when (eq kind :toadstool) (shape g :ball '(.008 4 3) "#f0ece0" (+ x .012) .162 z :shadow nil))))
    (:apple (shape g :cyl '(.09 .07 .06 8) "#8a6440" 0 .03 0)                             ; a small basket of apples
            (loop for i below 4 do (shape g :ball '(.035 7 5) (if (evenp i) "#c8382c" "#d8a030") (* .04 (cos (* i 1.6))) .08 (* .04 (sin (* i 1.6))) :shadow nil)))
    (:herb (dotimes (i 5) (shape g :box '(.008 .16 .008) "#5a7a3a" (* .012 (- i 2)) .08 0 :rot (list 0 0 (* .12 (- i 2))) :shadow nil))   ; a bunch tied with string
           (shape g :box '(.05 .012 .02) "#d8d2c0" 0 .05 0 :shadow nil)
           (dotimes (i 3) (shape g :ball '(.014 5 3) "#9a6ab8" (* .02 (- i 1)) .165 0 :shadow nil)))
    (:shroomfry (shape g :cyl '(.1 .1 .012 12) "#e4e0d4" 0 .006 0)                        ; a plate of fried mushrooms
                (dotimes (i 6) (shape g :ball '(.022 5 4) "#7a4a2c" (* .05 (cos i)) .028 (* .05 (sin i)) :scale '(1 .5 1) :shadow nil)))
    (:creamfry (shape g :cyl '(.09 .07 .08 12) "#d8d2c0" 0 .04 0)                         ; a bowl: mushrooms in sour cream
               (shape g :cyl '(.082 .082 .012 12) "#f0e8d8" 0 .078 0 :shadow nil)
               (dotimes (i 4) (shape g :ball '(.016 5 4) "#7a4a2c" (* .035 (cos (* i 1.6))) .088 (* .035 (sin (* i 1.6))) :shadow nil)))
    (:charlotte (shape g :cyl '(.1 .1 .012 12) "#e4e0d4" 0 .006 0)                        ; an apple pie
                (shape g :cyl '(.085 .09 .05 12) "#d8a040" 0 .037 0)
                (dotimes (i 5) (shape g :box '(.03 .006 .012) "#f0d8a0" (* .04 (cos (* i 1.25))) .064 (* .04 (sin (* i 1.25))) :shadow nil)))
    (:dried (shape g :box '(.004 .16 .004) "#d8d2c0" 0 .08 0 :shadow nil)                  ; dried mushrooms on a string
            (dotimes (i 5) (shape g :ball '(.026 6 4) "#6a4a34" 0 (+ .025 (* i .03)) 0 :scale '(1 .45 1) :shadow nil)))
    (:fried (shape g :cyl '(.1 .1 .012 12) "#e4e0d4" 0 .006 0)                          ; a plate with a browned fish on it
            (shape g :ball '(.085 8 5) "#b8782c" 0 .04 0 :scale '(.6 .4 1.3))
            (shape g :cone '(.05 .08 4) "#9a6424" 0 .04 .14 :rot (list (/ pi 2) 0 0) :scale '(.3 1 1)))
    (:jam (shape g :cyl '(.05 .05 .13 10) "#a3262c" 0 .065 0 :rough .2)                 ; a jar with a lid
          (shape g :cyl '(.054 .054 .025 10) "#d8a840" 0 .142 0 :metal .6 :rough .4)
          (shape g :box '(.07 .05 .004) "#f0ece0" 0 .07 -.05 :shadow nil))
    (:toast (shape g :cyl '(.1 .1 .012 12) "#e4e0d4" 0 .006 0)
            (dotimes (i 3) (shape g :box '(.09 .018 .07) "#c8843c" (* .02 (- i 1)) (+ .022 (* i .02)) (* .015 (- i 1)) :rot (list 0 (* i .4) 0))))
    (:stew (shape g :cyl '(.09 .07 .08 12) "#d8d2c0" 0 .04 0)                           ; a bowl with stew in it
           (shape g :cyl '(.082 .082 .012 12) "#7a4a2c" 0 .078 0 :shadow nil)
           (dotimes (i 3) (shape g :ball '(.016 5 4) "#9a5a34" (* .03 (cos (* i 2.1))) .09 (* .03 (sin (* i 2.1))) :shadow nil)))
    (:fish (shape g :ball '(.1 8 5) "#8aa0b0" 0 .06 0 :scale '(.6 .5 1.4) :metal .4 :rough .4)
           (shape g :cone '(.06 .1 4) "#6a8090" 0 .06 .17 :rot (list (/ pi 2) 0 0) :scale '(.3 1 1)))
    (:bread (shape g :box '(.2 .08 .1) "#b87434" 0 .04 0)                              ; a brick loaf: flat sides, it stacks
            (shape g :box '(.19 .025 .09) "#c8843c" 0 .09 0 :shadow nil)
            (dolist (x '(-.05 0 .05)) (shape g :box '(.012 .006 .07) "#e8c080" x .104 0 :rot '(0 .4 0) :shadow nil)))
    (:coal (shape g :cyl '(.085 .065 .13 10) "#6a6e72" 0 .065 0 :metal .5 :rough .5)       ; a bucket of coal
           (shape g :cyl '(.088 .088 .012 10) "#4c5052" 0 .13 0 :metal .5 :rough .5 :shadow nil)
           (dolist (x '(-.085 .085)) (shape g :box '(.008 .09 .008) "#34363a" x .17 0 :shadow nil))
           (shape g :box '(.18 .008 .008) "#34363a" 0 .215 0 :shadow nil)
           (loop for (x y z s) in '((0 .15 0 .045) (-.04 .14 .03 .035) (.04 .14 -.02 .04) (.02 .145 .04 .03) (-.03 .145 -.04 .032))
                 do (shape g :rock (list s) "#1a1a1e" x y z)))
    (:tools (shape g :box '(.24 .1 .12) "#a3262c" 0 .05 0) (shape g :box '(.14 .02 .02) "#2a2c30" 0 .13 0)
            (dolist (x '(-.06 .06)) (shape g :box '(.02 .04 .02) "#2a2c30" x .11 0)))
    (:books (shape g :box '(.16 .037 .22) "#6a3a2e" 0 .019 0)                           ; two books tied with string: they stack
            (shape g :box '(.155 .037 .215) "#2c4a7a" .003 .057 0 :rot '(0 .05 0))
            (shape g :box '(.15 .03 .21) "#e8e0d0" .004 .019 .003 :shadow nil)
            (shape g :box '(.012 .082 .225) "#d8d2c0" 0 .041 0 :shadow nil))
    (:cans (shape g :cyl '(.055 .055 .11 12) "#b0aa98" 0 .055 0 :metal .7 :rough .3)       ; one can, a plain cylinder: it stacks
           (shape g :cyl '(.057 .057 .06 12) "#a3262c" 0 .055 0 :shadow nil)
           (shape g :cyl '(.058 .058 .008 12) "#8a8e92" 0 .108 0 :metal .7 :rough .3 :shadow nil))
    (:water (shape g :cyl '(.04 .045 .2 8) "#7ab0c8" 0 .1 0 :alpha .7 :rough .2) (shape g :cyl '(.02 .035 .07 6) "#7ab0c8" 0 .235 0 :alpha .7)
            (shape g :cyl '(.046 .046 .06 8) "#e8e0d0" 0 .1 0 :shadow nil))
    (:honey (shape g :cyl '(.07 .06 .13 10) "#d89a20" 0 .065 0 :rough .3) (shape g :cyl '(.075 .075 .03 10) "#e8e0d0" 0 .145 0)
            (shape g :box '(.08 .05 .005) "#f0ece0" 0 .07 .068 :shadow nil))
    (:berry (shape g :cyl '(.085 .06 .07 10) "#b08a50" 0 .035 0)                         ; a wicker basket heaped with cranberries
            (shape g :cyl '(.09 .09 .012 10) "#8a6a3a" 0 .07 0 :shadow nil)
            (dolist (x '(-.085 .085)) (shape g :box '(.01 .1 .012) "#8a6a3a" x .12 0 :shadow nil))
            (shape g :box '(.18 .01 .012) "#8a6a3a" 0 .17 0 :shadow nil)
            (dotimes (i 14) (shape g :ball '(.016 5 3) (if (zerop (mod i 4)) "#c8303c" "#a3202c")
                                   (* (if (< i 9) .055 .025) (cos (* i 1.9))) (if (< i 9) .082 .095) (* (if (< i 9) .055 .025) (sin (* i 1.9))) :shadow nil)))))

(setf *kinds* (append *kinds*
                      '((:fish "рыба" (.16 .12 .3)) (:bread "хлеб" (.2 .1 .1)) (:coal "уголь" (.18 .2 .18)) (:tools "инструмент" (.24 .15 .12))
                        (:books "книги" (.16 .08 .22)) (:cans "тушёнка" (.11 .11 .11)) (:water "минеральная вода" (.1 .27 .1))
                        (:honey "мёд" (.15 .16 .15)) (:berry "клюква" (.18 .14 .18))
                        ;; dishes off the cooktop (uyut.lisp): blini are listed in interior.lisp
                        (:mushroom "грибы" (.16 .17 .16)) (:toadstool "грибы" (.16 .17 .16)) (:apple "яблоки" (.18 .12 .18)) (:herb "травы" (.1 .18 .06))
                        (:shroomfry "жареные грибы" (.22 .06 .22)) (:creamfry "грибы в сметане" (.2 .1 .2)) (:charlotte "шарлотка" (.22 .08 .22))
                        (:dried "сушёные грибы" (.08 .17 .08))
                        (:driedapple "сушёные яблоки" (.08 .17 .08)) (:driedherb "сушёные травы" (.1 .16 .06))
                        (:pickled "солёные грибы" (.12 .16 .12)) (:jarcompote "компот в банке" (.12 .16 .12)) (:cannedfish "рыбные консервы" (.12 .05 .12))
                        (:nut "орехи" (.16 .14 .16)) (:sandwich "бутерброды" (.22 .06 .22)) (:berrypie "пирог с клюквой" (.22 .08 .22))
                        (:soup "суп" (.2 .12 .2)) (:bakedapple "печёные яблоки" (.22 .1 .22)) (:brittle "козинаки" (.18 .08 .08))
                        (:fried "жареная рыба" (.22 .08 .22)) (:jam "варенье" (.12 .17 .12))
                        (:toast "гренки" (.22 .08 .22)) (:stew "горячая тушёнка" (.2 .12 .2)))))
