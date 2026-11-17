;;;; What the named characters say. The core asks for a line by its key; ~a marks where the game puts
;;;; a name (keep them, and keep their order).

(setf *lines*
      '(:sergeant-stop "Сержант Усов: «Гружёные едем? Документики.» Папка висит под картой, слева от окна — E по ней"
        :chairman-gives "Председатель Мурлыкин: «Вот вам ~a. И помните, кто её дал.» Бумага — в папке"
        :chairman-asks "Председатель Мурлыкин: «Сделаете — будет бумага. Не сделаете — будет другая бумага.» Задание «~a»: ~a"))
