;;;; The places between the towns: the relief of each stretch, landmarks, diners, and what the telescope shows.

;;; The route sheet, each entry as (stretch from-fraction to-fraction layer kind).
(setf *route-plan*
      '((4 .24 .76 :relief :mountains) (4 .2 .8 :water :none)    ; mountains at the bridge; the gorge is dry
    (3 .25 .75 :veg :pine) (5 .25 .75 :veg :pine)         ; deep forest: gas stations here are inns
    (6 .04 .5 :relief :hills)
    (8 .04 .96 :veg :marsh) (8 .04 .96 :water :lake) (8 .04 .96 :relief :plain)   ; the swamp
    (9 .06 .40 :relief :serpentine)                       ; the serpentine
    (11 .0 .32 :relief :mountains) (11 .0 1.0 :water :none)))   ; the hill the tunnel goes into; no water under the megastructure                       ; the hill the tunnel goes into

;;; The landmarks, each as (stretch before town number, fraction of the stretch, side +-1, what).
(setf *landmark-plan*
      '((1 .35 1 :windmill) (2 .50 -1 :plane) (3 .42 1 :church) (4 .12 1 :pumpjacks) (4 .82 -1 :mast)
    (5 .36 1 :elevator) (6 .60 1 :wheel) (7 .36 1 :pylons) (7 .72 1 :watchtower) (8 .50 -1 :oldbus)
    (9 .45 1 :radar) (2 .62 1 :farm) (10 .62 1 :rocket)
    (3 .60 1 :camp)))                                     ; the pioneer camp in the pine wood, open in summer

;;; The roadside diners on the right, each as (stretch, fraction, sign text, kind, color).
(setf *diner-plan*
      '((1 .72 "ШАШЛЫЧНАЯ" :grill "#a3262c") (2 .30 "ЧАЙНАЯ" :kiosk "#3f6f73") (3 .76 "ПЕЛЬМЕННАЯ" :glass "#2c4a7a")
    (5 .72 "КАФЕ «ВСТРЕЧА»" :glass "#a3262c") (6 .25 "ЧЕБУРЕЧНАЯ" :kiosk "#a8843c") (8 .28 "ШАШЛЫЧНАЯ" :grill "#8a4038")
    (9 .76 "СТОЛОВАЯ № 7" :glass "#3f6f73") (10 .76 "БЛИННАЯ" :kiosk "#a3262c") (11 .22 "КАФЕ «МИНУТКА»" :glass "#a8843c")))

(setf *mark-names*
      '(:windmill "Мельница" :plane "Самолёт" :church "Церковь на холме" :pumpjacks "Качалки" :mast "Радиомачта"
    :elevator "Элеватор" :wheel "Колесо обозрения" :pylons "ЛЭП" :watchtower "Вышка лесника" :oldbus "Автобус в поле"
    :radar "Станция на горе" :farm "Ферма" :rocket "Ракета"))

(setf *names*
      '(:plain "равнина" :hills "холмы" :mountains "горы" :canyon "каньон" :serpentine "серпантин" :none "" :river "река" :lake "озеро"
    :pine "хвойный лес" :meadow "цветущий луг" :autumn "осенняя роща" :birch "березняк" :rocky "каменистая пустошь"
    :steppe "степь" :marsh "болото"))

(setf *scope-sights*
      '(("moon" "Луна: моря и кратеры") ("jupiter" "Юпитер: полосы облаков и четыре луны в ряд") ("saturn" "Сатурн и его кольца")
    ("mars" "Марс: рыжий диск, белая полярная шапка") ("venus" "Венера: узкий серп, как у молодой Луны")
    ("pleiades" "Плеяды: семь сестёр в голубой дымке") ("andromeda" "Туманность Андромеды: другая галактика")))
