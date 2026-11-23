;;;; Goods and trade between towns. Every town makes something, which is sold cheap there, and is short
;;;; of something, which fetches a high price there (the tables are in economy.lisp). A seller at the window
;;;; offers only local goods, a buyer looks only for what is short: you cannot buy and resell the same thing
;;;; on the spot, the goods have to be hauled. Between towns, clients at bus stops follow the nearest town.

(in-package #:kiosk-trip)

(defun dish-p (kind) "Return the recipe entry if KIND is a cooked dish." (find kind *recipes* :key #'second))
(defun raw-of (kind) "Return the raw good the dish KIND is cooked from; for a non-dish return KIND itself." (let ((r (dish-p kind))) (if r (first r) kind)))
(defun good-name (kind) (cond ((dish-p kind) (third (dish-p kind))) (t (or (second (assoc kind *goods*)) "товар"))))
(defun good-base (kind) (or (third (assoc kind *goods*)) 5))

(defun trait (kind key)
  "Return the trait KEY of the good or dish KIND, as its record in the mod has it, or NIL.
A good's record is (kind name price key value ...); a dish's traits are in *DISH-TRAITS*. The keys: :spoils -
goes bad overnight outside the fridge; :stack - how many stand on one another; :cut - it can be cut on the
board, and the colour of its slices; :size - (width height depth) and :rgb - the colour, for a good that has
no model of its own and is drawn as a plain packet; :title - its name in the nominative."
  (getf (if (assoc kind *goods*) (cdddr (assoc kind *goods*)) (rest (assoc kind *dish-traits*))) key))
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
             (cond ((and from-shop (fboundp 'grant-paper)                    ; the first resale is forgiven: the rule comes on paper
                         (funcall 'grant-paper :obhss
                                  "У окна — человек в сером плаще. Кладёт на подоконник листок: «ПРЕДУПРЕЖДЕНИЕ. Товар из магазина, с ценником, перепродавать дороже нельзя: спекуляция. На первый раз — без последствий. Своё продавайте смело. ОБХСС»."
                                  "ОБХСС предупредил: магазинное с ценником дороже не перепродают. На первый раз простили — подробности в папке, закладка «ОБХСС»")))
                   (from-shop (note-resale) (setf turn (add-deeds town -1))
                              (when (< *heat* *heat-limit*)
                                (say-all (format nil "Магазинное продали дороже прейскуранта. У ОБХСС на нас сигналов: ~d из ~d" *heat* *heat-limit*))))
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
    (cond ((and place (market-p place))
           (unless (or (eql *market-told* (place-town place)) (and *toast* (> (cdr *toast*) 0)))
             (setf *market-told* (place-town place))
             (say-all (format nil "Рынок. ~a~:[~;. Гудок: смена идёт с завода!~]" (market-note (place-town place)) (rush-p (place-town place))))))
          ((> *speed* 2.0) (setf *market-told* nil)))))

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

(defun spec-kind (spec) "Return what kind of thing the item record SPEC is." (first spec))
(defun spec-x (spec) "Return where the thing of the item record SPEC stands across the wagon." (second spec))
(defun spec-y (spec) "Return the height of the bottom of the thing of the item record SPEC above the floor." (third spec))
(defun spec-z (spec) "Return where the thing of the item record SPEC stands along the wagon." (fourth spec))
(defun spec-spin (spec) "Return how the thing of the item record SPEC is turned: 0 to 3 quarter turns." (fifth spec))
(defun spec-addr (spec) "Return the address of the parcel of the item record SPEC, or NIL: not a parcel." (sixth spec))
(defun spec-props (spec) "Return the properties of the item record SPEC." (seventh spec))
(defun spec-prop (spec key) "Return the property KEY of the item record SPEC." (getf (seventh spec) key))
(defun spec-cut-p (spec) "Return true if the item record SPEC is a good that was cut." (spec-prop spec :cut))

(defun old-cut-spec (spec)
  "Return SPEC as it is written now: saves made before things had properties marked a cut good by a half turn."
  (if (and (= (length spec) 5) (eql (spec-spin spec) 2) (cut-look (spec-kind spec)))
      (make-spec (spec-kind spec) (spec-x spec) (spec-y spec) (spec-z spec) 0 nil '(:cut t))
      spec))

(defun cuttable-p (kind) (and (assoc kind *goods*) (trait kind :cut) t))

(defun cut-look (kind)
  "Return the colour of the slices if a thing of KIND, put down as cut, is shown as slices; else NIL."
  (trait kind :cut))

(defun build-cut (g kind)
  "Build the good KIND cut up: a row of slices, raw ones on a small board, cooked ones on a plate."
  (if (not (assoc kind *goods*))                                   ; a cooked dish
      (shape g :cyl '(.11 .11 .012 12) "#e4e0d4" 0 .006 0)
      (shape g :box '(.2 .012 .14) "#c8a060" 0 .006 0))
  (dotimes (i 6) (shape g :box '(.018 .07 .1) (cut-look kind) (- (* i .03) .075) .05 0 :rot (list 0 0 -.35) :shadow nil)))

(defun good-model (kind) "Return the name of the model of the good KIND (models/good-KIND.blend)." (format nil "good-~(~a~)" kind))

(defun build-good (g kind)
  "Build the good KIND as a thing in G: it stands on a shelf, is carried in paws, goes on the windowsill."
  (model g (good-model kind)))

(defparameter *modelled* '(:fish :bread :coal :tools :books :cans :water :honey :berry :fried :jam :toast :stew
                           :mushroom :toadstool :apple :herb :shroomfry :creamfry :charlotte :dried
                           :nut :sandwich :berrypie :soup :bakedapple :brittle :driedapple :driedherb :vobla :pickled :jarcompote :cannedfish)
  "The goods and dishes BUILD-GOOD has a model for. Any other good from a mod is drawn as a plain packet.")

(defun build-packet (g kind)
  "Build a good that has no model of its own: a packet of its :size and :rgb with a paper label."
  (destructuring-bind (w h d) (third (assoc kind *kinds*))
    (shape g :box (list w h d) (or (trait kind :rgb) "#b0946a") 0 (/ h 2) 0)
    (shape g :box (list (* .6 w) (* .4 h) .004) "#f0ece0" 0 (/ h 2) (+ (/ d 2) .004) :shadow nil)))

(defun adopt-goods ()
  "Give a place among the things to every good a mod brought without a model: its size comes from its record."
  (let ((clashes (number-clashes)))                               ; two names that make one number: say so, loudly
    (when clashes (cl:format t "~&MOD ERROR: these goods or stories share a number, rename one of each pair: ~s~%" clashes)))
  (dolist (entry *goods*)
    (unless (assoc (first entry) *kinds*)
      (setf *kinds* (append *kinds* (list (list (first entry) (or (trait (first entry) :title) (second entry))
                                                (or (trait (first entry) :size) '(.16 .16 .16)))))))))

(setf *kinds* (append *kinds*
                      '((:fish "рыба" (.16 .12 .3)) (:bread "хлеб" (.2 .1 .1)) (:coal "уголь" (.18 .2 .18)) (:tools "инструмент" (.24 .15 .12))
                        (:books "книги" (.16 .08 .22)) (:cans "тушёнка" (.11 .11 .11)) (:water "минеральная вода" (.1 .27 .1))
                        (:honey "мёд" (.15 .16 .15)) (:berry "клюква" (.18 .14 .18))
                        ;; dishes off the cooktop (uyut.lisp): blini are listed in interior.lisp
                        (:mushroom "грибы" (.16 .17 .16)) (:toadstool "грибы" (.16 .17 .16)) (:apple "яблоки" (.18 .12 .18)) (:herb "травы" (.1 .18 .06))
                        (:shroomfry "жареные грибы" (.22 .06 .22)) (:creamfry "грибы в сметане" (.2 .1 .2)) (:charlotte "шарлотка" (.22 .08 .22))
                        (:dried "сушёные грибы" (.08 .17 .08))
                        (:driedapple "сушёные яблоки" (.08 .17 .08)) (:driedherb "сушёные травы" (.1 .16 .06)) (:vobla "вобла" (.14 .18 .04))
                        (:pickled "солёные грибы" (.12 .16 .12)) (:jarcompote "компот в банке" (.12 .16 .12)) (:cannedfish "рыбные консервы" (.12 .05 .12))
                        (:nut "орехи" (.16 .14 .16)) (:sandwich "бутерброды" (.22 .06 .22)) (:berrypie "пирог с клюквой" (.22 .08 .22))
                        (:soup "суп" (.2 .12 .2)) (:bakedapple "печёные яблоки" (.22 .1 .22)) (:brittle "козинаки" (.18 .08 .08))
                        (:fried "жареная рыба" (.22 .08 .22)) (:jam "варенье" (.12 .17 .12))
                        (:toast "гренки" (.22 .08 .22)) (:stew "горячая тушёнка" (.2 .12 .2)))))
