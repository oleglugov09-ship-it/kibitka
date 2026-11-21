#!/usr/bin/env python3
"""The people of the road: one table for both files, the mod's people.lisp and the pairs for its en.lisp.
Edit the table here and run from the repository root: python3 tools/make-people.py"""
P=[]
def person(key, who, who_en, town, at, world, life, when=None):
    P.append(dict(key=key,who=who,who_en=who_en,town=town,at=at,world=world,life=life,when=when))

# ---- the factory
person(':director',"директор Барсуков","director Barsukov",2,"(:window :sender)",[
 ("(:short :tools)","План по инструменту горит, а металл ушёл в часть. Мне выговор, майору — благодарность","The tool plan is on fire, and the metal went to the garrison. A reprimand for me, thanks for the major"),
 ("(:friend)","Значит, так. Кооператив, а работаете как наш цех. Грузить без очереди","Now then. A co-operative, and you work like a shop of ours. To be loaded out of turn"),
 ("(:feud)","С колхозом дружите? Ну пусть колхоз вам и грузит","Friends with the collective farm? Then let the farm load you"),
 ("(:always)","Значит, так. Наряд есть — везите. Нет наряда — не отвлекайте","Now then. There is an order — haul it. No order — do not bother me")],
 [("Тридцать лет на этом заводе. Начинал учеником токаря, вон у того окна","Thirty years at this works. I began as a turner's apprentice, by that window there"),
  ("Сверху спускают план, снизу нет людей. А посередине я","The plan comes down from above, there are no hands below. And I am in the middle"),
  ("Говорят, скоро сами будем решать, что выпускать. Я не доживу, а заводу интересно","They say soon we will decide ourselves what to make. I will not see it, but the works is curious"),
  ("Сын зовёт в кооператив, как у вас. Я ему: а кто план давать будет?","My son calls me into a co-operative like yours. I tell him: and who will give the plan?")])
person(':foreman',"мастер Гайкин","foreman Gaikin",2,"(:window :buyer)",[
 ("(:short :bread)","Ёлки-моталки, смена шесть часов у станка, а в столовой хлеба нет. Давай что есть","Blow me down, six hours at the lathe, and the canteen is out of bread. Give me what you have"),
 ("(:season :winter)","В цеху плюс восемь. Греемся работой","Plus eight in the shop. We keep warm by working"),
 ("(:holiday)","Праздник праздником, а конец квартала никто не отменял","A holiday is a holiday, but nobody cancelled the end of the quarter"),
 ("(:always)","Беру на бригаду. Нас двенадцать гавриков","I am buying for the crew. Twelve lads of us")],
 [("Станок у меня сорок девятого года. Трофейный. Работает лучше новых","My lathe is from forty-nine. A trophy one. It works better than the new ones"),
  ("Молодые не идут: в кооперативе, говорят, втрое платят","The young ones do not come: they say a co-operative pays three times as much"),
  ("Рацпредложение подал. Третий год рассматривают, ёлки-моталки","I filed an improvement proposal. Third year they are considering it, blow me down"),
  ("Внедрили! Премия сорок рублей и грамота","They took it up! A bonus of forty rubles and a certificate")])
# ---- the supply
person(':pusher',"толкач Проныров","fixer Pronyrov",None,"(:window :sender)",[
 ("(:plenty)","Тут этого добра — девать некуда, а через два города за ним очередь. Вот и вся экономика","Here they do not know where to put the stuff, and two towns on there is a queue for it. That is the whole economy"),
 ("(:holiday)","Дорогой мой, в праздник базы закрыты, а вагоны идут. Самое моё время, самое моё","My dear, on a holiday the depots are shut, but the wagons roll. That is my time, my very time"),
 ("(:always)","Дорогой мой, золотой! Фонды выбил, транспорта нет. Выручайте, а я не забуду","My dear, my precious! I got the allocation, there is no transport. Help me out, and I will not forget")],
 [("Я с вашим отцом начинал. Он умел достать подшипник из воздуха","I started out with your father. He could get a bearing out of thin air"),
  ("Толкач — это кто едет за тем, что и так положено по бумаге","A fixer is the one who travels for what is due on paper anyway"),
  ("Отец ваш говорил: кооператив — это толкач, которому разрешили","Your father used to say: a co-operative is a fixer who has been allowed"),
  ("Письмо от него было. Не скажу откуда. Жив, спрашивал про вас","There was a letter from him. I will not say from where. Alive, asked about you")])
person(':storekeeper',"кладовщица Мышкина","storekeeper Myshkina",10,"(:window :sender)",[
 ("(:plenty)","Согласно остаткам, склад забит. Спишем по акту, если не вывезете","As per the balance, the store is packed. We will write it off by an act if you do not take it away"),
 ("(:short)","Согласно накладной — есть. По факту — нет. Не спрашивайте","As per the waybill — it is there. In fact — it is not. Do not ask"),
 ("(:always)","Распишитесь тут, тут и тут. И число","Sign here, here and here. And the date")],
 [("База у нас перевалочная: всё мимо нас и ничего нам","Ours is a transfer depot: everything goes past us and nothing to us"),
  ("Ревизия была. Нашли лишний вагон гвоздей. Лишний!","There was an audit. They found a wagon of nails too many. Too many!"),
  ("Дочка в Ленинске учится на экономиста. Объяснит мне потом, как это всё работает","My daughter studies economics in Leninsk. She will explain to me later how all this works"),
  ("Объяснила. Говорит, никак","She explained. She says it does not")])
# ---- the shop
person(':shopkeeper',"завмаг Усачёва","shop manager Usachova",6,"(:window :seller)",[
 ("(:rationed)","Что по талонам — то под прилавком не держу. Почти","What goes on coupons I do not keep under the counter. Almost"),
 ("(:plenty)","Гражданин, завезли больше, чем просила. Берите, пока не списали","Citizen, they delivered more than I asked. Take it before it is written off"),
 ("(:holiday)","К празднику выбросили дефицит. К обеду не останется","They put out scarce goods for the holiday. By noon none will be left"),
 ("(:always)","Гражданин, цена государственная. Торговаться — это на рынок","Citizen, the price is the state's. For haggling go to the market")],
 [("Магазин приняла в семьдесят девятом. Недостач не было ни разу","I took over the shop in seventy-nine. Not one shortfall since"),
  ("Меня спрашивают, куда девается колбаса. А её не привозят","They ask me where the sausage goes. It is simply not delivered"),
  ("Просилась в кооперативное кафе. Сказали: вы слишком честная","I asked to join a co-operative café. They said: you are too honest"),
  ("Взяли всё-таки. Бухгалтером","They took me after all. As the bookkeeper")])
person(':salesgirl',"продавщица Люся","salesgirl Lyusya",None,"(:window :ration)",[
 ("(:no-talons)","Ой, ну без талона не могу, хоть ты плачь. Меня же с работы снимут","Oh, without a coupon I just cannot, cry as you may. They would sack me, you know"),
 ("(:talons)","Ой, с талончиком — пожалуйста! Один в руки, не обижайтесь","Oh, with a coupon — you are welcome! One to a customer, no offence"),
 ("(:season :winter)","Очередь с шести утра, на морозе. Я им чай выношу","The queue stands from six in the morning, in the frost. I bring them tea"),
 ("(:always)","Вас много, а я одна","There are many of you and one of me")],
 [("Я вообще-то на артистку поступала. Два раза","Actually I applied to be an actress. Twice"),
  ("В кино зовут сниматься. В массовке, в очереди","They ask me to act in a film. As an extra, in a queue"),
  ("Сняли! Меня там видно: я третья от прилавка","They shot it! You can see me there: I am third from the counter")])
# ---- the transport
person(':dispatcher',"диспетчер Рычагов","dispatcher Rychagov",None,":fuel",[
 ("(:rich)","Автобаза — кооперативу: аренду можете платить вперёд, раз деньги есть","Motor depot to the co-operative: you may pay the rent ahead, since there is money"),
 ("(:season :winter)","Автобаза — всем бортам: гололёд, в горах цепи. Кто застрянет — трактор только утром","Motor depot to all trucks: black ice, chains in the mountains. Whoever gets stuck — the tractor comes only in the morning"),
 ("(:season :autumn)","Автобаза — всем бортам: половина парка на уборочной. Попутных не ждите","Motor depot to all trucks: half the fleet is at the harvest. Do not count on a lift"),
 ("(:always)","Автобаза № 3 на связи. Грузовик не бейте, он казённый","Motor depot No. 3 on the air. Do not smash the truck, it is the state's")],
 [("Это я вашему отцу машину выписывал. Он расписался и подмигнул","It was I who signed the truck out to your father. He signed and winked"),
  ("У меня на базе сорок машин и двенадцать шофёров. Остальные — в кооперативах","I have forty trucks at the depot and twelve drivers. The rest are in co-operatives"),
  ("Начальство спрашивает, почему арендованный грузовик возит больше наших. Я молчу","The bosses ask why the rented truck hauls more than ours. I keep quiet")])
person(':trucker',"шофёр Баранкин","driver Barankin",None,":fuel",[
 ("(:season :winter)","Слышь, братишка, на перевале метёт. Я цепи надел и тебе советую","Listen, brother, it is drifting on the pass. I put the chains on and advise you to"),
 ("(:shark)","Слышь, братишка, ты, говорят, Аркадию должен. Плати, он терпеливый до поры","Listen, brother, they say you owe Arkady. Pay up, he is patient only for so long"),
 ("(:heat)","На посту шмонают: ищут, кто магазинное перепродаёт. Не ты?","They are searching at the post: looking for who resells shop goods. Not you?"),
 ("(:always)","Бензин по талонам, запчасти по блату, а рейс по плану. Поехали","Petrol on coupons, spares by connections, and the run by the plan. Off we go")],
 [("Двадцать лет за баранкой. Кольцо знаю, как свою кухню","Twenty years at the wheel. I know the ring like my own kitchen"),
  ("Везу туда пустые ящики, обратно — пустые ящики. Тонно-километры!","I haul empty crates there, and empty crates back. Tonne-kilometres!"),
  ("Жена говорит: уйди в кооператив. А стаж? А пенсия?","The wife says: go into a co-operative. And the years of service? And the pension?"),
  ("Ушёл. Купил «Москвич». Страшно, но интересно","I left. Bought a Moskvich. Scary, but interesting")])
# ---- the village
person(':chairman',"председатель Пушков","chairman Pushkov",4,"(:window :seller)",[
 ("(:season :autumn)","Уборочная. Завод прислал шефов — инженеров с вёдрами. Картошку они видят впервые","Harvest time. The factory sent its patrons — engineers with buckets. They see a potato for the first time"),
 ("(:season :spring)","Мил человек, сеять пора, а запчастей к трактору нет. Привезёшь инструмент — озолочу","Good man, it is time to sow, and no spares for the tractor. Bring tools and I will shower you with gold"),
 ("(:feud)","Вы заводу возите, а завод у меня людей сманивает. Не обессудьте","You haul for the factory, and the factory lures my people away. No offence"),
 ("(:always)","Мил человек, молоко сдаём государству по двадцать копеек. Себе в убыток","Good man, we hand the milk to the state at twenty kopecks. At a loss to ourselves")],
 [("Колхоз у нас миллионер. По долгам","Our farm is a millionaire. In debts"),
  ("Молодёжь уезжает в Красный Кут. Там общежитие и кино","The young leave for Krasny Kut. There is a hostel and a cinema there"),
  ("Думаю пасеку отдать в аренду семье. Раньше за такое сажали","I am thinking of leasing the apiary to a family. They used to jail people for that"),
  ("Отдал. Мёду вдвое. Никто не понимает почему, а я понимаю","I leased it. Twice the honey. Nobody understands why, but I do")])
person(':milkmaid',"доярка Нюра","milkmaid Nyura",4,"(:window :seller)",[
 ("(:season :summer)","Летом молока — залейся. Половину везут в лагеря, детям","In summer the milk flows over. Half goes to the camps, for the children"),
 ("(:season :winter)","Зимой корма впроголодь, и надой такой же","In winter the fodder is short, and so is the yield"),
 ("(:plenty :milk)","Ой, батюшки, молоковоз не пришёл! Берите молочко, а то скиснет","Oh, goodness, the milk lorry did not come! Take the milk, dearie, or it will turn"),
 ("(:always)","Встаю в четыре. Коровки плана не знают","I get up at four. The dear cows do not know the plan")],
 [("У меня двадцать коров, и каждую зовут","I have twenty cows, and each has a name"),
  ("Зорька дала рекорд. Меня — на доску почёта, её — нет. Несправедливо","Zorka set a record. They put me on the board of honour, not her. Unfair"),
  ("Зовут в город, на молокозавод. А коров на кого?","They call me to town, to the dairy. And who will mind the cows?")])
# ---- the inspectors
person(':inspector',"инспектор Кривцов","inspector Krivtsov",None,"(:window :buyer)",[
 ("(:heat)","Отдел по борьбе с хищениями. Хлеб у вас почём? А в магазине почём? Вот и я о том же","The department against theft of socialist property. How much is your bread? And in the shop? That is what I mean"),
 ("(:always)","Так-так. Кооператив — дело законное. Пока своё продаёте","Well, well. A co-operative is a lawful thing. As long as you sell your own")],
 [("Раньше я знал, кого ловить. Теперь спекулянт называется предпринимателем","I used to know whom to catch. Now a speculator is called an entrepreneur"),
  ("Инструкцию прислали новую. Старую не отменили. Работаю по обеим","They sent a new instruction. The old one was not cancelled. I work by both")],
 when="(:heat)")
person(':sergeant',"сержант Усов","sergeant Usov",6,"(:booth 13)",[
 ("(:heat)","Гражданин водитель, на вас сигнал. Пока сигнал, а не протокол","Citizen driver, there is a report on you. So far a report, not a charge sheet"),
 ("(:holiday)","В праздник усиление. Стою с шести, домой к ночи","Reinforced duty on a holiday. On my feet since six, home by night"),
 ("(:always)","Гражданин водитель, штрафы — в окошко. Жалобы — в письменном виде","Citizen driver, fines — at the window. Complaints — in writing")],
 [("Я на этой трассе каждый грузовик знаю. Ваш — по занавескам","I know every truck on this road. Yours — by the curtains"),
  ("Отца вашего останавливал. Документы всегда в порядке. Это и подозрительно","I used to stop your father. His papers were always in order. That is what was suspicious"),
  ("Мне до пенсии два года. Хочу пасеку","Two years to my pension. I want an apiary")])
# ---- the bank
person(':cashier',"кассирша Клавдия Петровна","cashier Klavdia Petrovna",0,"(:booth 14)",[
 ("(:debt-half)","Половину погасили. Отец ваш и трети не внёс","Half is paid off. Your father did not bring in a third"),
 ("(:rich)","На счёте у вас много. Зарплату выдаю раз в неделю, в боковую дверь — весь счёт на руки","There is a lot on your account. I pay the wage once a week, at the side door — the whole account in cash"),
 ("(:always)","Со счёта спишу сразу. Наличные пересчитаю дважды. Следующий!","From the account I take it at once. Cash I count twice. Next!")],
 [("Безналичные и наличные — это разные деньги. На одни покупает завод, на другие — человек","Transfers and cash are different money. A factory buys with one, a person with the other"),
  ("У меня весь город на книжках. Знаю, кто копит на «Жигули», а кто на похороны","I have the whole town on savings books. I know who saves for a Zhiguli and who for a funeral"),
  ("Говорят, вклады обесценятся. Я своим говорю: не верьте. А сама купила ковёр","They say the savings will lose their worth. I tell mine: do not believe it. And bought a carpet myself")])
person(':banker',"управляющий Процентов","bank manager Protsentov",0,"(:booth 14)",[
 ("(:shark)","Позвольте напомнить: вы должны не только нам. Мы знаем. Нам — в первую очередь","Allow me to remind you: you owe not only us. We know. Us first"),
 ("(:debt-half)","Аккуратный заёмщик. Захотите расширяться — дадим ещё","A careful borrower. If you want to expand, we will lend more"),
 ("(:always)","Позвольте заметить: ссуда кооперативу — дело новое. Мы на вас смотрим","Allow me to observe: a loan to a co-operative is a new thing. We are watching you")],
 [("Я тридцать лет давал деньги заводам. Они не возвращали, и это считалось нормой","For thirty years I lent money to factories. They did not repay, and that was the norm"),
  ("Вы первые, кто возвращает. Не знаю, в какую графу это писать","You are the first who repay. I do not know which column to put it in")])
# ---- the shadow
person(':shark',"Аркадий Семёнович","Arkady Semyonovich",5,"(:booth 15)",[
 ("(:shark-paid)","Мы в расчёте. Редкое по нынешним временам слово","We are square. A rare word these days"),
 ("(:rich)","Молодой человек, у вас на счёте цифры, у меня в кармане деньги. Поменяемся?","Young man, you have figures on an account, I have money in my pocket. Shall we swap?"),
 ("(:always)","Молодой человек, процент капает. Я не тороплю, я считаю","Young man, the percent drips. I do not hurry you, I count")],
 [("Я шью джинсы в цеху, которого нет, из ткани, которой не было. И все довольны","I sew jeans in a shop that does not exist, from cloth that never was. And everyone is content"),
  ("Ваш отец взял у меня на первый рейс. Банк тогда кооперативам не давал","Your father borrowed from me for his first run. The bank did not lend to co-operatives then"),
  ("Теперь мне предлагают выйти из тени. А я там тридцать лет, у меня там мебель","Now they offer me to come out of the shadow. I have been there thirty years, I have furniture there")])
person(':spiv',"фарцовщик Жорик","spiv Zhorik",5,"(:window :buyer)",[
 ("(:heat)","Тише, тут ходит один в штатском. Потом договорим","Quiet, there is one in plain clothes about. We will finish later"),
 ("(:plenty)","Чувак, тут этого навалом. Вези туда, где нет, — учить тебя, что ли","Dude, there are heaps of it here. Take it where there is none — do I have to teach you"),
 ("(:always)","Чувак, жвачка, кассеты, пакеты с ковбоем. Фирма́! Не надо? Зря","Dude, chewing gum, cassettes, bags with a cowboy. The real stuff! No? A pity")],
 [("Порт — это окно. Что моряк привёз, то я продал","A port is a window. What a sailor brings, I sell"),
  ("Раньше меня за это гоняли. Теперь спрашивают, где брал","They used to chase me for it. Now they ask where I got it"),
  ("Открываю кооператив. «Импорт». Сам не верю","I am opening a co-operative. «Import». I do not believe it myself")])
# ---- the council
person(':secretary',"секретарь Бумажкина","secretary Bumazhkina",6,"(:booth 12)",[
 ("(:rationed)","Талоны печатаем каждый квартал. Товара под них не печатают","We print the coupons every quarter. They do not print goods for them"),
 ("(:holiday)","К празднику — рапорт о досрочном выполнении. Он у меня с прошлого года готов","For the holiday — a report on fulfilling the plan ahead of time. I have had it ready since last year"),
 ("(:friend)","Вас на доску почёта вносили при мне. Председатель сам диктовал","You were put on the board of honour in my presence. The chairman dictated it himself"),
 ("(:always)","Председатель занят. Изложите в установленном порядке: письменно, в трёх экземплярах","The chairman is busy. Set it out in the established order: in writing, in three copies")],
 [("Через мою машинку прошёл каждый приказ в этом городе. Я знаю, кто что обещал","Every order in this town went through my typewriter. I know who promised what"),
  ("Раньше писали «в целях дальнейшего улучшения». Теперь — «в условиях ускорения». Смысл тот же","They used to write «for further improvement». Now — «under acceleration». The meaning is the same"),
  ("Копирку экономлю. Её тоже по разнарядке","I save carbon paper. That too comes by allocation")])
# ---- the queue
person(':granny',"баба Шура","granny Shura",None,"(:window :buyer)",[
 ("(:rationed)","Я в трёх очередях стою разом: за собой, за соседкой и за сахаром","I stand in three queues at once: for myself, for my neighbour and for sugar"),
 ("(:season :winter)","Ох, милок, зимой очередь — она и греет. Тесно, тепло","Oh, dear, in winter a queue keeps you warm. It is tight and cosy"),
 ("(:holiday)","На праздник внукам гостинец надо. Что у тебя есть, милок?","The grandchildren need a treat for the holiday. What have you got, dear?"),
 ("(:always)","Ох, милок, раньше тоже ничего не было. Но хоть не обещали","Oh, dear, there was nothing before either. But at least they did not promise")],
 [("Пенсия у меня шестьдесят рублей. На что хватает — то и праздник","My pension is sixty rubles. Whatever it covers is the feast"),
  ("Внук в кооператив подался. Джинсы варит. В кастрюле!","My grandson has gone into a co-operative. He boils jeans. In a saucepan!"),
  ("Внук купил мне телевизор. Цветной. Смотрю съезд, как кино","My grandson bought me a television. A colour one. I watch the congress like a film")])
person(':student',"студент Котофеев","student Kotofeyev",3,"(:window :buyer)",[
 ("(:season :autumn)","Нас, в принципе, всем курсом — на картошку. Высшая математика подождёт","Our whole year is, in principle, off to the potatoes. Higher mathematics will wait"),
 ("(:short :books)","В библиотеке на «Мастера» очередь до весны. У вас нет?","At the library the queue for «The Master» runs till spring. Have you got one?"),
 ("(:holiday :school)","Первое сентября. Стипендия сорок рублей, и она уже кончилась","The first of September. The grant is forty rubles, and it is already gone"),
 ("(:always)","Мне бы, в принципе, поесть чего. Дёшево","I could, in principle, do with something to eat. Cheap")],
 [("Учусь на экономиста. Изучаем план, которого никто не выполняет","I am studying economics. We study a plan nobody fulfils"),
  ("Курсовую пишу про кооперативы. Руководитель говорит: тема скользкая","My term paper is on co-operatives. My tutor says: a slippery subject"),
  ("Защитил! Вас в примечаниях упомянул. Без фамилии","I defended it! I mentioned you in the notes. Without a name")])
# ---- the camp
person(':quartermaster',"завхоз Ложкина","supply manager Lozhkina",None,":camp",[
 ("(:plenty :milk)","Молока завезли на три смены вперёд. Киснет","They brought milk for three sessions ahead. It is turning"),
 ("(:always)","По ведомости — триста ртов, четыре раза в день. Считайте сами","By the list — three hundred mouths, four times a day. Count it yourselves")],
 [("Лагерь заводской. Что завод не съел — то детям","The camp is the factory's. What the factory did not eat goes to the children"),
  ("Вожатые просят костёр на закрытие. Дров не дают — спишу тару","The leaders ask for a bonfire at the closing. No firewood is issued — I will write off the crates"),
  ("Третью смену закрываю без недостачи. Сама удивляюсь","I close the third session without a shortfall. I am surprised myself")])
# ---- the army
person(':major',"майор Когтев","major Kogtev",1,"(:window :sender :buyer)",[
 ("(:plenty :cans)","Тушёнки на складах — на три войны. Срок выходит, отпускаем","Tinned meat in the stores for three wars. It is going out of date, we are releasing it"),
 ("(:feud)","С институтом возитесь? Отставить. Им — книжки, нам — приказ","Busy with the institute? Belay that. Books for them, an order for us"),
 ("(:always)","Отставить разговоры. Часть снабжается в первую очередь","Belay the talk. The garrison is supplied first")],
 [("У меня триста бойцов и один план: чтобы все поели","I have three hundred men and one plan: that everyone gets fed"),
  ("Половина роты на уборочной в колхозе. Родину защищаем лопатой","Half the company is at the harvest on the farm. We defend the motherland with a spade"),
  ("Рапорт на увольнение написал. Лежит в столе. Пусть полежит","I wrote my resignation. It lies in the drawer. Let it lie")])
assert len(P)>=20, len(P)

# ---- where everyone ends up. STAYED is the line of the credits for one who is still at the post.
# Those who answer for something may leave at a quarterly report: ANSWERS is what for, ACTING is who stands
# in afterwards, EXILE is what the dismissed one says when met once more, PROMOTED is the farewell, and the
# two -END lines are for the credits.
STAYED={
 ':director':("всё ещё директор. План выполнен на сто и две десятых","still the director. The plan is fulfilled by a hundred point two"),
 ':foreman':("у того же станка. Учит двоих","at the same lathe. Teaching two apprentices"),
 ':pusher':("в дороге. Говорят, видели его в порту с чемоданом","on the road. They say he was seen at the port with a suitcase"),
 ':storekeeper':("на базе. Гвозди наконец сошлись","at the depot. The nails have finally tallied"),
 ':shopkeeper':("в том же магазине. Недостач по-прежнему нет","in the same shop. Still not one shortfall"),
 ':salesgirl':("за прилавком. Фильм показали по второй программе","behind the counter. The film was shown on channel two"),
 ':dispatcher':("на связи. Грузовик приняли обратно без единой царапины","on the air. The truck was taken back without a scratch"),
 ':trucker':("на кольце. На своём «Москвиче»","on the ring. In his own Moskvich"),
 ':chairman':("в колхозе. Пасеку так и не отобрали","on the farm. The apiary was never taken back"),
 ':milkmaid':("с коровами. Зорьку всё-таки повесили на доску","with the cows. Zorka made it onto the board after all"),
 ':inspector':("работает по обеим инструкциям","works by both instructions"),
 ':sergeant':("на посту. До пенсии год","at his post. A year to the pension"),
 ':cashier':("в сберкассе. Ковёр висит","at the savings bank. The carpet hangs"),
 ':banker':("завёл для кооперативов отдельную графу","has opened a separate column for co-operatives"),
 ':shark':("в тени. Там мебель","in the shadow. The furniture is there"),
 ':spiv':("председатель кооператива «Импорт»","chairman of the «Import» co-operative"),
 ':secretary':("печатает. Копирки хватило","typing. The carbon paper lasted"),
 ':granny':("смотрит съезд. Цветной","watching the congress. In colour"),
 ':student':("экономист. Пока без работы","an economist. Out of work so far"),
 ':quartermaster':("закрыла смену. Без недостачи","closed the session. Without a shortfall"),
 ':major':("служит. Рапорт в столе","serving. The resignation is in the drawer"),
}
LEAVE={
 ':director':dict(answers="(:supply 2)",acting=("и. о. директора","the acting director"),
   exile=("Сняли за срыв плана. А металл мне так и не дали. Клюёт сегодня, кстати","Dismissed for failing the plan. And the metal I never got. They are biting today, by the way"),
   fired_end=("снят. Рыбачит в Заречном, не жалуется","dismissed. Fishing in Zarechny, not complaining"),
   promoted=("Забирают в министерство. Тридцать лет у этого окна — и на тебе","They are taking me to the ministry. Thirty years by this window — and there you are"),
   promoted_end=("в министерстве. Спускает планы сверху","at the ministry. Sending plans down from above")),
 ':pusher':dict(answers="(:cargo)",acting=("новый снабженец","the new supply man"),
   exile=("Списали меня: говорят, толкачи больше не нужны, теперь хозрасчёт. Посмотрим","They wrote me off: they say fixers are not needed any more, it is self-financing now. We shall see"),
   fired_end=("не у дел. Пишет воспоминания, никто не печатает","out of work. Writing his memoirs, nobody prints them"),
   promoted=("Зовут в Москву, в Госснаб. Буду оттуда доставать то же самое","They call me to Moscow, to the State Supply. I will be getting the same things from there"),
   promoted_end=("в Госснабе. Достаёт","at the State Supply. Getting things")),
 ':storekeeper':dict(answers="(:stock 10)",acting=("и. о. кладовщика","the acting storekeeper"),
   exile=("Сняли за излишки. За излишки! Стою теперь в очереди, как все","Dismissed for a surplus. For a surplus! Now I stand in the queue like everyone"),
   fired_end=("снята за излишки. Дочка говорит, это закономерно","dismissed for a surplus. Her daughter says it figures"),
   promoted=("Переводят на областную базу. Там вагонами считают","I am being moved to the regional depot. They count in wagons there"),
   promoted_end=("на областной базе. Считает вагонами","at the regional depot. Counting in wagons")),
 ':shopkeeper':dict(answers="(:stock 6)",acting=("и. о. завмага","the acting shop manager"),
   exile=("Тридцать лет без недостачи, а сняли за затоваривание. Что завезли, то и лежало","Thirty years without a shortfall, and dismissed for overstocking. What they delivered just lay there"),
   fired_end=("снята. Бухгалтер в кооперативном кафе","dismissed. The bookkeeper at a co-operative café"),
   promoted=("Забирают в торг, в область. Буду распределять то, чего нет","They are taking me to the regional trade board. I will allocate what there is not"),
   promoted_end=("в областном торге. Распределяет","at the regional trade board. Allocating")),
 ':dispatcher':dict(answers="(:cargo)",acting=("диспетчер автобазы","the motor depot dispatcher"),
   exile=("Автобазу сократили вдвое, меня первым. Машины раздали в аренду — таким, как вы","The depot was cut by half, me first. The trucks were leased out — to the likes of you"),
   fired_end=("сокращён. Водит арендованный грузовик","made redundant. Drives a leased truck"),
   promoted=("Автобаза — кооперативу: перевожусь в управление. Грузовик берегите","Motor depot to the co-operative: I am moving to the directorate. Look after the truck"),
   promoted_end=("в управлении. На связь не выходит","at the directorate. Does not come on the air")),
 ':chairman':dict(answers="(:supply 4)",acting=("и. о. председателя","the acting chairman"),
   exile=("Сняли. За пасеку или за план — не сказали. Торгую вот своим, с огорода","Dismissed. For the apiary or for the plan — they did not say. I sell my own now, from the garden"),
   fired_end=("снят. Арендовал ту самую пасеку","dismissed. Leased that very apiary"),
   promoted=("Забирают в район, в агропром. Колхоз жалко","They are taking me to the district agro-industry board. I am sorry for the farm"),
   promoted_end=("в агропроме. Пишет колхозу планы","at the agro-industry board. Writing plans for the farm")),
 ':inspector':dict(answers="(:order)",acting=("новый инспектор","the new inspector"),
   exile=("Отдел расформировали. Говорят, спекуляции больше нет, есть рынок","The department was disbanded. They say there is no speculation any more, there is a market"),
   fired_end=("отдел расформирован. Охраняет кооперативное кафе","the department disbanded. Guards a co-operative café"),
   promoted=("Переводят в столицу. Там воруют вагонами, интереснее","I am being moved to the capital. They steal by the wagon there, more interesting"),
   promoted_end=("в столице. Ловит вагонами","in the capital. Catching by the wagon")),
 ':sergeant':dict(answers="(:order)",acting=("новый сержант","the new sergeant"),
   exile=("Проводили на пенсию на два года раньше. Не просился. Клюёт?","They saw me off to my pension two years early. I did not ask. Any bites?"),
   fired_end=("на пенсии раньше срока. Завёл пасеку","retired ahead of time. Keeps an apiary"),
   promoted=("Дали лейтенанта и перевели в область. Занавески ваши буду помнить","They made me a lieutenant and moved me to the region. I will remember your curtains"),
   promoted_end=("лейтенант, в области","a lieutenant, in the region")),
 ':banker':dict(answers="(:loan)",acting=("и. о. управляющего","the acting manager"),
   exile=("Сняли за ссуду кооперативу. За вашу. Невозврат, говорят, — а вы же платили?","Dismissed for a loan to a co-operative. Yours. A bad debt, they say — but you were paying, were you not?"),
   fired_end=("снят за ссуду кооперативу. За нашу","dismissed for a loan to a co-operative. Ours"),
   promoted=("Меня берут в новый банк, коммерческий. Вы — мой лучший пример","I am being taken into a new bank, a commercial one. You are my best example"),
   promoted_end=("в коммерческом банке. Показывает нашу ссуду как пример","at a commercial bank. Shows our loan as an example")),
 ':spiv':dict(answers="(:deficit 5)",acting=("какой-то новенький","some newcomer"),
   exile=("Всё завезли, представляешь? Жвачка в магазине лежит. Чем жить человеку","They delivered everything, can you imagine? Chewing gum lies in the shop. What is a man to live on"),
   fired_end=("прогорел: в магазинах всё появилось","went bust: everything turned up in the shops"),
   promoted=("Уезжаю в столицу. Там дефицит настоящий, не чета здешнему","I am off to the capital. The shortage there is the real thing, no match for ours"),
   promoted_end=("в столице. Кооператив «Импорт-2»","in the capital. The «Import-2» co-operative")),
 ':secretary':dict(answers="(:supply 6)",acting=("новая секретарь","the new secretary"),
   exile=("Сократили вместе с должностью. Машинку оставили себе. Стою вот за сахаром","Made redundant together with the post. They kept the typewriter. Here I stand for sugar"),
   fired_end=("сокращена. Печатает на дому","made redundant. Types at home"),
   promoted=("Председатель идёт наверх и берёт меня. Кто-то же должен знать, что он обещал","The chairman is going up and taking me along. Somebody has to know what he promised"),
   promoted_end=("в области, при председателе. Помнит всё","in the region, with the chairman. Remembers everything")),
 ':major':dict(answers="(:supply 1)",acting=("и. о. командира","the acting commander"),
   exile=("Уволили в запас. Рапорт и не понадобился. Бойцов жалко, а так — рыбалка","Discharged to the reserve. The resignation was not even needed. Sorry for the men, otherwise — fishing"),
   fired_end=("в запасе. Рыбачит","in the reserve. Fishing"),
   promoted=("Перевод в округ, в штаб. Часть оставляю сытой — и на том спасибо","A transfer to the district staff. I leave the garrison fed — thanks for that much"),
   promoted_end=("в штабе округа. Подполковник","at the district staff. A lieutenant colonel")),
}
ACTING=[("Я тут временно. Ничего не подписываю","I am here for the time being. I sign nothing"),
 ("Прежний всё в голове держал. У меня только папка","The last one kept it all in his head. I only have a folder"),
 ("Мне сказали ничего не менять. Не меняю","I was told to change nothing. I change nothing"),
 ("Зайдите после отчёта. Или после следующего","Come after the report. Or after the next one"),
 ("Вы с ним дружили? Со мной не обязательно","You were friends with him? No need to be with me"),
 ("Исполняю обязанности. Права не дали","I perform the duties. The rights were not given")]
REPORT=dict(title=("КВАРТАЛЬНЫЙ ОТЧЁТ ПО ОБЛАСТИ~%~%Квартал ~d из четырёх. Выплачена ещё четверть ссуды — область подводит итоги","THE REGION'S QUARTERLY REPORT~%~%Quarter ~d of four. One more quarter of the loan is paid — the region sums up"),
 fired=("ОСВОБОЖДЕНИЕ ОТ ДОЛЖНОСТИ~%~%~a — за невыполнение. На место назначен исполняющий обязанности","RELIEVED OF THE POST~%~%~a — for failing the plan. An acting official is appointed in place"),
 promoted=("ПЕРЕВОД НАВЕРХ~%~%~a — за перевыполнение. Сюда не вернётся","MOVED UP~%~%~a — for beating the plan. Will not come back here"),
 quiet=("Все остались на своих местах. Пока","Everyone has stayed where they were. So far"),
 stays=("Люди дороги","The people of the road"))
# ---- how those met at the window look: the number that picks fur and coat, the clothing set of the cat
# (0 coat and scarf, 1 sheepskin and ushanka, 2 coat and flat cap, 3 overalls and hard hat, 4 leather jacket and
# peaked cap) and the marks put on top (clients.lisp, PERSON-DRESS).
LOOK={
 ':director':"(4 0 (:tie :hat))", ':foreman':"(1 3 ())", ':pusher':"(8 4 (:folder))",
 ':storekeeper':"(3 0 (:glasses :bun))", ':shopkeeper':"(9 0 (:white))", ':salesgirl':"(13 0 (:white :bow))",
 ':chairman':"(5 1 ())", ':milkmaid':"(15 0 (:kerchief-white))", ':inspector':"(0 0 (:hat :glasses))",
 ':spiv':"(7 0 (:cap :shades))", ':granny':"(2 0 (:kerchief))", ':student':"(10 0 (:glasses :book))",
 ':major':"(11 0 (:officer))",
}
assert set(STAYED)==set(p['key'] for p in P) and set(LEAVE)<=set(STAYED)

def q(s): return '"'+s.replace('\\','\\\\').replace('"','\\"')+'"'
NL=chr(10)
out=[""";;;; The people of the road (lisp/people.lisp): one or two for every link in how the country feeds itself.
;;;; :who - the name with the trade; :town - the town number, or NIL for someone met anywhere; :at - where:
;;;; (:window client-kind ...), (:booth number), :fuel (at the pump) or :camp; :when - a condition for turning
;;;; up at all; :world - lines about the world, each with its condition (the first that holds and is new is
;;;; said); :life - the person's own story, in order, a step for each quarter of our loan; :stayed - the line
;;;; of the credits; :look - for one met at the window: (fur-and-coat number, clothing set, marks).
;;;; One who :answers for something may leave at a quarterly report: :acting stands in,
;;;; :exile is said when the dismissed one is met once more, :promoted is the farewell, the -end lines are
;;;; for the credits.
;;;; This file is written by tools/make-people.py together with the pairs for en.lisp: edit the table there.

(setf *acting-lines* '(""" + (NL+'                       ').join(q(ru) for ru,en in ACTING) + """))

(setf *report-lines*
      (list """ + (NL+'            ').join(':'+k+' '+q(v[0]) for k,v in REPORT.items()) + """))

(setf *people*
      (list"""]
for p in P:
    lv=LEAVE.get(p['key'])
    extra=''
    if lv:
        extra=(NL+"             :answers '"+lv['answers']+" :acting "+q(lv['acting'][0])
               +NL+"             :exile "+q(lv['exile'][0])
               +NL+"             :fired-end "+q(lv['fired_end'][0])
               +NL+"             :promoted "+q(lv['promoted'][0])
               +NL+"             :promoted-end "+q(lv['promoted_end'][0]))
    out.append("       (list "+p['key']+" :who "+q(p['who'])+" :town "+(str(p['town']) if p['town'] is not None else 'nil')+" :at '"+p['at']
               +((" :when '"+p['when']) if p['when'] else '')
               +NL+"             :world '("+(NL+'                      ').join('('+c+' '+q(ru)+')' for c,ru,en in p['world'])+")"
               +NL+"             :life '("+(NL+'                     ').join(q(ru) for ru,en in p['life'])+")"
               +NL+"             :stayed "+q(STAYED[p['key']][0])
               +((NL+"             :look '"+LOOK[p['key']]) if p['key'] in LOOK else '')+extra+")")
out.append('       ))\n')
open('project/mods/base/people.lisp','w',encoding='utf-8').write('\n'.join(out))
pairs=[]
for p in P:
    pairs.append((p['who'],p['who_en']))
    for c,ru,en in p['world']: pairs.append((ru,en))
    for ru,en in p['life']: pairs.append((ru,en))
    pairs.append(STAYED[p['key']])
    lv=LEAVE.get(p['key'])
    if lv:
        for k in ('acting','exile','fired_end','promoted','promoted_end'): pairs.append(lv[k])
pairs+=ACTING+list(REPORT.values())
seen=set(); pairs=[x for x in pairs if not (x[0] in seen or seen.add(x[0]))]
E='project/mods/base/en.lisp'; s=open(E,encoding='utf-8').read()
a='\n ;; the people of the road\n'
if a in s: s=s[:s.index(a)+1]+s[s.rindex(' ;; end of the people\n')+len(' ;; end of the people\n'):]
assert ';; the people of the road' not in s
anchor=' "Чего в городе не хватает, то в магазине по талонам'
assert s.count(anchor)==1
block=a.lstrip('\n')+''.join(' '+q(ru)+'\n  '+q(en)+'\n' for ru,en in pairs)+' ;; end of the people\n'
s=s.replace(anchor,block+anchor)
open(E,'w',encoding='utf-8').write(s)
print(len(P),'персонажей,',len(pairs),'строк')
