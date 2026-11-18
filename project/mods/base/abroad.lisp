;;;; The countries beyond the border (lisp/zagranitsa.lisp). Made up, like the towns: a fraternal republic
;;;; the plan ships goods to, and a western kingdom whose goods come in by sea through the port.

(setf *abroad*
      '((:ozeria :side :brother :every 9 :at 4 :goods (:cans :milk :tools :honey :coal)
         :news "сверхплановая поставка в братскую Озерию: ~a отгрузили из всех городов. В магазинах будет нехватка")
        (:norven :side :west :every 11 :at 7 :goods (:tools :books :fish :cans) :town 5
         :news "в порт пришёл пароход из Норвенского королевства: ~a завезли в ~a. Там избыток, остальным везти по наряду")))
