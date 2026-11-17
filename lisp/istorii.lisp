;;;; Town stories. No cutscenes: a story travels in parcels. The sender has a name and a few words about
;;;; what is being sent and to whom; the recipient answers; a town or two later one of them recognises you
;;;; at the window and asks how things are over there; the radio retells what you delivered. A chain is
;;;; several parcels between the same cats; the next parcel appears once the previous one is handed over.
;;;;
;;;; There is no engine here: a table of stories and the bookkeeping of who owes what to whom. It is hooked
;;;; into trade at four points of trade.lisp (the sender, parcel taken, parcel delivered, the acquaintance
;;;; at the window) and into the radio (uyut.lisp).

(in-package #:kiosk-trip)

(defparameter *stories*
  '((:jam
     (4 3 "Тётя Зина" "Сыночку в Ленинск, он там на физика учится. Варенье. Только не побейте!"
      "Гриша, студент" "От мамы! Опять варенье… Скажите ей, что ем я нормально."
      "в Ленинске студент физфака получил посылку из дома. Соседи по общежитию довольны."
      (4 "Тётя Зина" "Довезли Грише? Он хоть шапку носит?"))
     (3 4 "Гриша, студент" "Маме в Берёзовку. Зачётка — пусть сама увидит, что не зря."
      "Тётя Зина" "Зачётка! Одни пятёрки… и одна тройка. По физкультуре, ну это в отца."
      "в Берёзовке доярка читала зачётку сына всей ферме. Надои выросли."
      (3 "Гриша, студент" "Мама не ругалась за тройку? Она по физкультуре."))
     (4 3 "Тётя Зина" "Валенки ему. Зима в общежитии — не зима дома."
      "Гриша, студент" "Валенки… В аудитории засмеют. Зато ноги будут тёплые, а смеяться будут с насморком."
      "в Ленинске на лекции по термодинамике замечен студент в валенках. Профессор одобрил: «практик»."
      (4 "Тётя Зина" "На каникулы обещал приехать. Вы уж его подвезите, если встретите.")))
    (:sea
     (5 8 "Боцман Фома" "Брату в Горняк. Ракушка. Пусть под землёй слышит, что море ещё есть."
      "Лука, забойщик" "Ракушка от Фомы… Шумит. Весь забой по очереди слушал."
      "в Горняке шахтёры полсмены слушали ракушку. План, тем не менее, выполнен."
      (5 "Боцман Фома" "Слушал Лука ракушку? То-то. А говорил — «какое мне море»."))
     (8 5 "Лука, забойщик" "Фоме в Первомайск. Кусок антрацита. Пусть помнит, чем топят его пароход."
      "Боцман Фома" "Уголь от брата. Блестит, как море ночью. На полку поставлю, топить им не дам."
      "в Первомайске на буксире «Чайка» в кают-компании выставлен кусок угля. Экскурсии не водят."
      (8 "Лука, забойщик" "Не сжёг он мой уголь? Он может, он хозяйственный."))
     (5 8 "Боцман Фома" "Луке. Билет на пароход, на август. Пусть хоть раз вылезет на свет."
      "Лука, забойщик" "Билет… На август. Это ж надо у бригадира отпроситься. Отпрошусь."
      "в Горняке забойщик впервые за двенадцать лет попросил отпуск. Бригада скинулась на панаму."
      (8 "Лука, забойщик" "Панаму видели? Вся бригада выбирала. Море, говорят, синее. Проверю.")))
    (:honey
     (1 11 "Рядовой Тима" "Бабушке в Сосновку. Письмо. Пишу, что кормят хорошо. Вы ей не говорите, как на самом деле."
      "Баба Нюра, пасечница" "От Тимоши! «Кормят хорошо»… Знаю я, как их кормят. Ну-ка обождите."
      "в Сосновке пасечница получила письмо от внука и немедленно начала собирать посылку."
      (1 "Рядовой Тима" "Бабушка поверила про кормёжку? По глазам вижу — не поверила."))
     (11 1 "Баба Нюра, пасечница" "Тимоше в Северный. Мёд. На всю роту, пусть не жадничает."
      "Рядовой Тима" "Мёд! Трёхлитровая… Товарищ старшина уже идёт. Он всегда чует."
      "в Северном рота в полном составе пила чай с мёдом. Старшина объявил пасеке благодарность."
      (11 "Баба Нюра, пасечница" "Дошёл мёд? Старшина, говорят, благодарность объявил. Пчёлам передам."))
     (1 11 "Рядовой Тима" "Бабушке. Карточка — я в форме, с автоматом. Автомат деревянный, но этого не видно."
      "Баба Нюра, пасечница" "Тимоша… Вытянулся-то как. И ружьё дали. На улей поставлю, пусть пчёлы видят."
      "в Сосновке на главном улье пасеки появилась фотография. Пчёлы работают строже."
      (11 "Баба Нюра, пасечница" "Карточка на улье стоит. Заезжайте на чай — с мёдом, какой Тимоше не достался.")))
    (:bridge
     (2 6 "Инженер Вера" "В горсовет, в Октябрьский. Чертёж моста через Гнилую. Третий раз отправляю."
      "Секретарь горсовета" "Опять мост? Примем, зарегистрируем, рассмотрим. Ответ направим в установленный срок."
      "в Октябрьском зарегистрирован входящий документ. Номер ему присвоен шестизначный."
      (2 "Инженер Вера" "Приняли? А сказали что? «В установленный срок»… Значит, как всегда."))
     (6 2 "Секретарь горсовета" "В Красный Кут, инженеру. Ответ на обращение. Печать гербовая, не помните."
      "Инженер Вера" "«Отказать за отсутствием фондов». Печать красивая. Мост красивее."
      "в Красном Куте инженер завода получила ответ из горсовета. Чертёж моста она, по слухам, не выбросила."
      (6 "Секретарь горсовета" "Вручили? Жалоб не было? Жалобы мы тоже регистрируем."))
     (2 7 "Инженер Вера" "В Дальний, полярникам. Тот же чертёж. Им нужнее, и фондов они не спрашивают."
      "Начальник зимовки" "Мост? Нам? Через ручей к складу — как раз! Доски есть, руки есть. Строим."
      "в Дальнем за две недели построен мост по чертежу, которому в Октябрьском отказали. Ручей доволен."
      (2 "Инженер Вера" "Построили?! За две недели? Вот куда надо было сразу писать.")))
    (:postcard
     (9 10 "Лида, отдыхающая" "В Узловую, стрелочнику Пахому. Открытка с видом. Мы не знакомы, он мне из окна вагона помахал."
      "Пахом, стрелочник" "Открытка? Мне? С пальмой… Я вообще всем машу, работа такая. Но этой — запомнил."
      "в Узловой стрелочник получил открытку с курорта и теперь машет поездам обеими руками."
      (9 "Лида, отдыхающая" "Вручили? Он удивился? Покраснел? Стрелочники краснеют?"))
     (10 9 "Пахом, стрелочник" "В Светлый, Лиде. Расписание поездов. Обвёл тот, что идёт мимо моего поста в полдень."
      "Лида, отдыхающая" "Расписание… Обведено. Девочки, я на процедуры не иду. Я иду на вокзал."
      "в Светлом отдыхающая променяла грязевые ванны на железнодорожное расписание. Врачи разводят руками."
      (10 "Пахом, стрелочник" "Она расписание-то поняла? Там мелко. Я карандашом обвёл, жирно."))
     (9 10 "Лида, отдыхающая" "Пахому. Флажок, сама сшила. Пусть машет не всем подряд, а этим — только мне."
      "Пахом, стрелочник" "Флажок. В горошек. По уставу не положен… В полдень положен."
      "в Узловой на посту номер три замечен неуставной флажок в горошек. Начальник станции сделал вид, что не видит."
      (10 "Пахом, стрелочник" "Полуденный теперь всегда вовремя. Я слежу. Лично.")))
    (:winter
     (3 7 "Марья Львовна, библиотекарь" "В Дальний, на зимовку. Книги. Им там полгода ночь — пусть хоть читают."
      "Начальник зимовки" "Книги! Сорок томов! А то мы инструкцию к примусу наизусть выучили и уже спорим о трактовках."
      "в Дальнем зимовщики получили библиотеку. Спор о примусе временно прекращён."
      (3 "Марья Львовна, библиотекарь" "Дошли книги? Не отсырели? Скажите, чтобы уголки не загибали."))
     (7 3 "Начальник зимовки" "В Ленинск, в библиотеку. Дневник погоды за год. Больше нам отдариться нечем."
      "Марья Львовна, библиотекарь" "Дневник погоды. «Метель. Метель. Метель. Видели песца.» Это же роман! Ставлю в отдел прозы."
      "в Ленинске дневник полярной метеостанции выдан на руки одиннадцать раз. Читатели требуют продолжения."
      (7 "Начальник зимовки" "Наш дневник читают? В отделе прозы? Ребята, пишем аккуратнее — мы теперь литература."))
     (3 7 "Марья Львовна, библиотекарь" "На зимовку. Читательские билеты, шесть штук. И напоминание: «Капитанскую дочку» вернуть до весны."
      "Начальник зимовки" "Билеты! Именные! А «Дочку» не вернём — её Семёныч под подушкой держит."
      "в Дальнем шесть зимовщиков стали читателями ленинской библиотеки. Задолженность — одна книга."
      (3 "Марья Львовна, библиотекарь" "Не вернут «Дочку», да? Я так и знала. Запишу как утрату в пользу Севера.")))
    (:lamp
     (0 8 "Дед Савва, рыбак" "В Горняк, племяннику. Сапоги болотные. В шахте, говорят, тоже мокро."
      "Петя, проходчик" "Сапоги от деда! До пояса! В них и в штрек, и на рыбалку. Если отпустят."
      "в Горняке проходчик вышел на смену в рыбацких сапогах. Вода в забое признала поражение."
      (0 "Дед Савва, рыбак" "Подошли Петьке сапоги? Нога у него как у меня — весло, а не нога."))
     (8 0 "Петя, проходчик" "Деду в Заречный. Лампа шахтёрская. Пусть ночью на реке светит, а то он с лучиной."
      "Дед Савва, рыбак" "Лампа! Светит — аж рыба щурится. Всю жизнь впотьмах ловил, дурак старый."
      "в Заречном на ночной рыбалке замечен яркий свет. Рыба, по словам очевидцев, идёт на него сама."
      (8 "Петя, проходчик" "Дед лампу освоил? Он кнопку-то нашёл? Она сбоку."))
     (0 1 "Дед Савва, рыбак" "В Северный, в часть. Вяленая рыба, мешок. Петька служил там — пусть ребятам будет."
      "Дежурный по части" "Рыба? От деда рядового запаса? Принято. Доложу командиру. После того как попробую."
      "в Северном на вечерней поверке пахло вяленой рыбой. Командир части претензий не имеет."
      (0 "Дед Савва, рыбак" "Съели рыбу солдатики? То-то. Передай: к осени ещё навялю.")))
    (:sap
     (6 11 "Кузьмич, сторож горсовета" "В Сосновку, леснику. Замок амбарный. У него медведь в сарай повадился."
      "Лесник Ефим" "Замок! Кузьмич помнит. Только медведь у меня грамотный — он через крышу."
      "в Сосновке на сарай лесника повешен замок. Медведь изучает вопрос."
      (6 "Кузьмич, сторож горсовета" "Помог замок от медведя? Нет? Через крышу?! Ну, это уже не ко мне, это к кровельщикам."))
     (11 6 "Лесник Ефим" "В Октябрьский, Кузьмичу. Берёзовый сок, бутыль. Пусть в своём горсовете не киснет."
      "Кузьмич, сторож горсовета" "Сок берёзовый… Лесом пахнет. Поставлю на вахте. Председателю не дам."
      "в Октябрьском на вахте горсовета замечена бутыль с берёзовым соком. Посетители задерживаются."
      (11 "Лесник Ефим" "Пьёт Кузьмич сок? Не начальству отдал? Он такой, он может."))
     (6 4 "Кузьмич, сторож горсовета" "В Берёзовку, сестре. Справка, что брат жив, здоров и при должности. С печатью, сам поставил."
      "Сестра Кузьмича" "Справка, что жив. С гербом! На стенку повешу. А сам-то когда приедет? В справке не сказано."
      "в Берёзовке на стене избы висит справка с гербовой печатью о том, что брат хозяйки жив. Соседи завидуют."
      (6 "Кузьмич, сторож горсовета" "Повесила справку? На видное? Хорошая сестра. Печать-то я, между нами, без спросу.")))
    (:beads
     (5 0 "Матрос Гена" "В Заречный, деду Савве. Сеть морская. Он речной сетью ловит, а эта — на кита."
      "Дед Савва, рыбак" "Сеть! Ячея с кулак. В неё у нас разве что корова зайдёт. Но вещь, вещь…"
      "в Заречном испытана морская сеть. Улов: бревно, сапог и участковый, шедший вброд."
      (5 "Матрос Гена" "Ловится на мою сеть? Участковый?! Ну… тоже улов."))
     (0 5 "Дед Савва, рыбак" "В Первомайск, Гене. Копчёный лещ. Пусть знает, что рыба бывает и без соли."
      "Матрос Гена" "Лещ. Речной. Сладкий… Ребята, идите сюда. Нет, стойте, сам съем."
      "в Первомайске экипаж буксира впервые попробовал речную рыбу. Мнения разделились, лещ — нет."
      (0 "Дед Савва, рыбак" "Понравился лещ морякам? Ещё бы. Это вам не селёдка."))
     (5 9 "Матрос Гена" "В Светлый, в санаторий, сестричке Тосе. Бусы из ракушек. Сам сверлил, шилом."
      "Тося, медсестра" "Бусы… Он шилом сверлил? Одна с дыркой насквозь, вторая — мимо. Буду носить."
      "в Светлом медсестра санатория вышла на смену в бусах из ракушек. Пульс у отдыхающих участился."
      (5 "Матрос Гена" "Носит бусы? С халатом? Скажи ей: в другой раз жемчуг привезу. Если найду."))))
  "The stories: entries of (key step...).
A step is (from-town to-town sender sender-words recipient recipient-words radio-summary (town who words));
the final triple says who later recognises us at the window, where, and what they ask. The summary
starts with a lowercase letter: the radio reads it after the word for 'Radio:'.")

(defvar *story-at* '() "The progress of each story: entries of (key . number of the step now waiting to be sent).")
(defvar *story-transit* '() "The story parcels in transit: entries of (recipient-town key step-number).")
(defvar *story-after* '() "The cats who will recognise us at the next meeting: entries of (town who words).")
(defvar *news* '() "The radio summaries of what we have delivered; the first is the oldest.")
(defvar *story-late* nil "The line to be said a little later, as (when . text): after the line about money.")

(defun story-step (key) (or (cdr (assoc key *story-at*)) 0))
(defun story-record (key n) (nth n (rest (assoc key *stories*))))

;;; A story parcel has an address of its own: the town number plus hundreds encoding the story and step. It
;;; shows whose parcel it is: the crate says who it is from, and the recipient answers that very parcel
;;; rather than the first one that happens to come to this town.

(defun parcel-addr (town &optional key n)
  "Return the address of a parcel to town TOWN.
With KEY and N it is a parcel of story KEY, step N."
  (+ town (if key (* 100 (+ 1 (* 10 (position key *stories* :key #'first)) n)) 0)))
(defun parcel-town (addr) "Return the town the parcel with address ADDR goes to." (mod addr 100))
(defun parcel-story (addr)
  "Return whose parcel ADDR is: (key step step-record) for a story parcel, NIL for an ordinary one."
  (let ((id (floor addr 100)))
    (when (plusp id)
      (let* ((story (nth (floor (1- id) 10) *stories*)) (n (mod (1- id) 10)) (step (and story (nth n (rest story)))))
        (and step (list (first story) n step))))))
(defun parcel-from (addr) "Return the sender of the story parcel ADDR, or NIL." (let ((s (parcel-story addr))) (and s (third (third s)))))
(defun parcel-to (addr) "Return the recipient of the story parcel ADDR, or NIL." (let ((s (parcel-story addr))) (and s (fifth (third s)))))

(defun story-sender (town)
  "Return the story sender in town TOWN if one of the stories is waiting to be sent from there.
The result is (recipient-town who words key step-number), or NIL."
  (loop for (key . steps) in *stories*
        do (let* ((n (story-step key)) (step (nth n steps)))
             (when (and step (= (first step) town) (notany (lambda (tr) (eq (second tr) key)) *story-transit*))
               (return (list (second step) (third step) (fourth step) key n))))))

(defun story-taken (key n)
  "Note that the parcel of story KEY (step N) was taken into the wagon.
From now on it is awaited in the recipient town."
  (let ((step (story-record key n)))
    (when step (push (list (second step) key n) *story-transit*) (save-stories))))

(defun story-delivered (addr &optional by-air)
  "Handle the delivery of the parcel with address ADDR.
If it is a story parcel and its step is the current one, return the recipient's answer (a line for the
players; with BY-AIR an airmail notice instead), queue the radio summary and let one of the two
recognise us at the window later; the story moves on. Otherwise return NIL."
  (let ((story (parcel-story addr)))
    (when story
      (destructuring-bind (key n step) story
        (when (= n (story-step key))                                   ; that very step, not a parcel left from a previous game
          (destructuring-bind (from dest sender words who answer news after) step
            (declare (ignore from dest sender words))
            (setf *story-transit* (remove-if (lambda (tr) (eq (second tr) key)) *story-transit*)
                  *story-at* (cons (cons key (1+ n)) (remove key *story-at* :key #'car))
                  *story-after* (append *story-after* (list after))
                  *news* (last (append *news* (list news)) 6))
            (save-stories)
            (if by-air
                (format nil "Авиапочта: ~a получил посылку" who)
                (format nil "~a: «~a»" who answer))))))))

(defun story-regular (town)
  "Return the line of the acquaintance at the window in town TOWN, as 'who: words'.
It is said once, when there is something to ask about; otherwise return NIL."
  (let ((line (find town *story-after* :key #'first)))
    (when line
      (setf *story-after* (remove line *story-after*))
      (save-stories)
      (format nil "~a: «~a»" (second line) (third line)))))

(defun story-news ()
  "Return the next radio summary of our doings, or NIL."
  (when *news* (prog1 (pop *news*) (save-stories))))

(defun say-later (text &optional (after 3.2))
  "Say TEXT to everyone a little later, after AFTER seconds: once they have read the line about money."
  (setf *story-late* (cons (+ *time* after) text)))

(defun step-stories ()
  (when (and *story-late* (> *time* (car *story-late*)))
    (say-all (cdr *story-late*))
    (setf *story-late* nil)))

(defun save-stories () (unless (net-guest-p) (put-save :stories (list *story-at* *story-transit* *story-after* *news*))))
(defun load-stories ()
  (let ((f (get-save :stories)))
    (when (and (listp f) (= (length f) 4) (every #'listp f))
      (setf *story-at* (remove-if-not (lambda (c) (and (consp c) (assoc (car c) *stories*) (integerp (cdr c)))) (first f))
            *story-transit* (remove-if-not (lambda (tr) (and (listp tr) (= (length tr) 3) (story-record (second tr) (third tr)))) (second f))
            *story-after* (remove-if-not (lambda (a) (and (listp a) (= (length a) 3) (integerp (first a)))) (third f))
            *news* (remove-if-not #'stringp (fourth f))))))
(defstate :stories
  :clear (lambda () (setf *story-at* '() *story-transit* '() *story-after* '() *news* '() *story-late* nil))
  :load #'load-stories :save #'save-stories)
