;;;; The powers of the towns (lisp/plan.lisp): who answers for the supply of each town, and which town is its
;;;; rival: the two share one allocation, the same hands or the same funds, so a friend of one is counted
;;;; against the other. Entries of (town what-it-is who rival-town-or-NIL).

(setf *forces*
      '((0 "рыбколхоз" "бригадир Сомов" nil)
        (1 "войсковая часть" "майор Когтев" 3)            ; the army is given first what the institute waits for
        (2 "завод" "директор Барсуков" 4)                 ; the factory draws hands and iron from the collective farm
        (3 "институт" "профессор Усатов" 1)
        (4 "колхоз" "председатель Пушков" 2)
        (5 "порт" "начальник причала Шкипер" 6)           ; what goes past the till against what must be in the report
        (6 "горсовет" "председатель Мурлыкин" 5)
        (7 "заготконтора" "заготовитель Мохов" nil)
        (8 "шахта" "бригадир Угольков" 9)                 ; the best of the supply goes to the resort, the mine waits
        (9 "курорт" "главврач Белянкин" 8)
        (10 "станция" "начальник станции Свистков" nil)
        (11 "леспромхоз" "мастер Сучков" nil)))
