;;;; The towns: their names, what each one makes and lacks, the goods and the recipes for the cooktop.

(setf *town-names*
      '("Заречный" "Северный" "Красный Кут" "Ленинск" "Берёзовка" "Первомайск" "Октябрьский" "Дальний" "Горняк"
    "Светлый" "Узловая" "Сосновка"))

;;; The goods: entries of (kind, name in the accusative case as in 'they sell fish', average price).
(setf *goods*
      '((:milk "молоко" 5) (:fish "рыбу" 7) (:bread "хлеб" 4) (:coal "уголь" 9) (:tools "инструмент" 12) (:books "книги" 10)
    (:cans "тушёнку" 8) (:water "минеральную воду" 5) (:honey "мёд" 9) (:berry "клюкву" 6)
    ;; what is not made anywhere but grows by the bus stops (*forage* below)
    (:mushroom "грибы" 8) (:apple "яблоки" 6) (:herb "травы" 5) (:toadstool "грибы" 1) (:nut "орехи" 9)))

;;; The trade of each town by town number: (what is made here) (what is short here).
(setf *town-trade*
      '(((:fish) (:milk :bread :tools))            ; Zarechny: the river
    ((:cans) (:milk :books :honey :herb))       ; Severny: army depots
    ((:tools) (:milk :fish :bread :mushroom))  ; Krasny Kut: factories
    ((:books) (:milk :honey :water :mushroom)) ; Leninsk: colleges and a printing house
    ((:milk :honey) (:tools :cans :books))     ; Beryozovka: a village
    ((:fish) (:milk :water :coal :nut))        ; Pervomaysk: the port
    ((:bread) (:fish :honey :berry))           ; Oktyabrsky: the bakery plant by the town council
    ((:berry) (:bread :cans :tools :coal :nut)) ; Dalny: cranberries from the swamps, everything else is brought in
    ((:coal) (:milk :bread :cans :herb))       ; Gornyak: coal mines
    ((:water) (:fish :berry :books :apple))    ; Svetly: a resort
    ((:bread) (:coal :tools :water :apple))    ; Uzlovaya: the station bakery
    ((:honey) (:cans :books :tools))))

;;; The recipes: entries of (raw-good dish dish-name-in-the-accusative [second-ingredient [hearth]]).
;;; With a second ingredient both must lie on the stove; such a recipe wins over what each makes alone.
;;; A dish is made by one recipe only: where it sells is decided by its raw good.
;;; The hearth is the stove unless given: :LINE is the dryer above it (overnight).
(setf *recipes*
      '((:milk :blin "блины") (:fish :fried "жареную рыбу") (:berry :jam "варенье") (:bread :toast "гренки") (:cans :stew "горячую тушёнку")
        (:mushroom :shroomfry "жареные грибы") (:mushroom :creamfry "грибы в сметане" :milk)
        (:apple :charlotte "шарлотку" :bread) (:apple :bakedapple "печёные яблоки")
        (:bread :sandwich "бутерброды" :fish) (:berry :berrypie "пирог с клюквой" :bread) (:cans :soup "суп" :water)
        (:nut :brittle "козинаки" :honey)
        (:mushroom :dried "сушёные грибы" nil :line) (:apple :driedapple "сушёные яблоки" nil :line) (:herb :driedherb "сушёные травы" nil :line)
        ;; closed in a jar at the crate under the washstand (the hearth :JAR): it never goes bad
        (:mushroom :pickled "солёные грибы" nil :jar) (:apple :jarcompote "компот в банке" nil :jar) (:fish :cannedfish "рыбные консервы" nil :jar)))

;;; What goes into the kettle with the water: entries of (good brew-key name-of-the-drink).
(setf *brews* '((:apple :compote "компот") (:herb :herbal "травяной чай") (:berry :mors "клюквенный морс")
                (:driedapple :compote "компот из сухофруктов") (:driedherb :herbal "травяной чай")
                (:toadstool :trip "мухоморный отвар")))

;;; What grows by the bus stops: entries of (good where seasons weathers count-a-day).
(setf *forage*
      '((:mushroom (:pine :birch) (:spring :summer :autumn) (:rain :fog :cloudy) 2)
        (:toadstool (:pine :birch) (:summer :autumn) (:rain :fog :cloudy) 1)
        (:berry (:marsh) (:summer :autumn) nil 2)
        (:apple (:village) (:summer :autumn) nil 2)
        (:herb (:meadow :steppe) (:spring :summer) nil 2)
        (:nut (:pine :birch) (:autumn) nil 2)))
