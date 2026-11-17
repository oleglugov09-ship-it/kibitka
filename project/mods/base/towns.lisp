;;;; The towns: their names, what each one makes and lacks, the goods and the recipes for the cooktop.

(setf *town-names*
      '("Заречный" "Северный" "Красный Кут" "Ленинск" "Берёзовка" "Первомайск" "Октябрьский" "Дальний" "Горняк"
    "Светлый" "Узловая" "Сосновка"))

;;; The goods: entries of (kind, name in the accusative case as in 'they sell fish', average price).
(setf *goods*
      '((:milk "молоко" 5) (:fish "рыбу" 7) (:bread "хлеб" 4) (:coal "уголь" 9) (:tools "инструмент" 12) (:books "книги" 10)
    (:cans "тушёнку" 8) (:water "минеральную воду" 5) (:honey "мёд" 9) (:berry "клюкву" 6)))

;;; The trade of each town by town number: (what is made here) (what is short here).
(setf *town-trade*
      '(((:fish) (:milk :bread :tools))            ; Zarechny: the river
    ((:cans) (:milk :books :honey))            ; Severny: army depots
    ((:tools) (:milk :fish :bread))            ; Krasny Kut: factories
    ((:books) (:milk :honey :water))           ; Leninsk: colleges and a printing house
    ((:milk :honey) (:tools :cans :books))     ; Beryozovka: a village
    ((:fish) (:milk :water :coal))             ; Pervomaysk: the port
    ((:bread) (:fish :honey :berry))           ; Oktyabrsky: the bakery plant by the town council
    ((:berry) (:bread :cans :tools :coal))     ; Dalny: cranberries from the swamps, everything else is brought in
    ((:coal) (:milk :bread :cans))             ; Gornyak: coal mines
    ((:water) (:fish :berry :books))           ; Svetly: a resort
    ((:bread) (:coal :tools :water))           ; Uzlovaya: the station bakery
    ((:honey) (:cans :books :tools))))

;;; The recipes: entries of (raw-good dish dish-name-in-the-accusative).
(setf *recipes*
      '((:milk :blin "блины") (:fish :fried "жареную рыбу") (:berry :jam "варенье") (:bread :toast "гренки") (:cans :stew "горячую тушёнку")))
