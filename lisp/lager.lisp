;;;; The pioneer camp as the game plays it: the canteen takes food by transfer, a parcel for the camp is handed in
;;;; at its gates. The camp as a place by the road, and when it is open, is in life.lisp.

(in-package #:kiosk-trip)

(defvar *camp-fed* nil "What the canteen has taken today: (day . count).")

(defun camp-label ()
  (cond ((not (camp-open-p)) "Пионерлагерь «Костёр». Закрыт до лета")
        ((camp-parcel-p *carry*) "E — отдать вожатой посылку для пионера")
        ((null *carry*) "Хозворота лагеря: завхоз берёт для столовой съестное — блюда, молоко, хлеб, яблоки, мёд")
        (t (format nil "E — сдать завхозу ~a" (carry-name *carry*)))))

(defshared camp-supply :camp ()
  "Hand what we carry to the camp's supply manager: food for the canteen, paid by transfer."
  (let ((kind (car *carry*)) (day (today)))
    (unless (eql (car *camp-fed*) day) (setf *camp-fed* (cons day 0)))
    (cond ((camp-parcel-p *carry*) (camp-parcel))
          ((not (camp-open-p)) (say "Сторож: «Смена кончилась. Приезжай летом»"))
          ((null *carry*) (say "Завхоз: «Столовой нужно съестное: блюда, молоко, хлеб, яблоки, мёд. Неси к воротам»"))
          ((or (cdr *carry*) (not (or (dish-p kind) (member kind *camp-food*)))) (say "Завхоз: «Это нам ни к чему. Детей кормим, а не склад держим»"))
          ((>= (cdr *camp-fed*) *camp-daily*) (say "Завхоз: «На сегодня столовая сыта. Завтра приезжай»"))
          (t (let ((pay (round (* (if (dish-p kind) 12 6) (list-price (raw-of kind))))))   ; an organisation pays by transfer, on its own scale
               (incf (cdr *camp-fed*))
               (coop-pay pay)
               (empty-paws)
               (show-carry)
               (let ((turn (add-deeds *rush-town* 1)))                              ; the factory keeps the camp: a deed with its director
                 (save-plan) (save-interior)
                 (log-event :camp :good kind :pay pay)
                 (say-all (format nil "Завхоз расписался: ~a для столовой, ~/kiosk-trip::rub/ на счёт. Шефы с завода это запомнят" (good-name kind) pay))
                 (when turn (say-standing turn))
                 (let ((line (person-speaks :camp))) (when line (say-later line 4.0)))))))))

(defun camp-parcel ()
  "Hand the parcel in the paws to the camp. In summer the leader takes it and pays for the delivery; once
the session is over the watchman takes it to send back to the parents, for half."
  (let* ((open (camp-open-p)) (pay (if open (delivery-pay) (ceiling (delivery-pay) 2))) (addr (cdr *carry*)))
    (add-money pay)
    (empty-paws)
    (show-carry) (save-interior)
    (note-quest :post)
    (let ((answer (story-delivered addr))) (when answer (say-later answer)))   ; a story's parcel moves its story on here too
    (log-event :camp-parcel :pay pay :open open)
    (say-all (if open
                 (format nil "Вожатая: «Отряд, построиться — почта!» Посылка вручена: +~/kiosk-trip::rub/" pay)
                 (format nil "Сторож: «Смена разъехалась. Отошлю родителям обратно». +~/kiosk-trip::rub/" pay)))))

(defun life-usables ()
  "Return the things to press outside that belong to the life of the roadside: the camp's gates."
  (let ((d (camp-d)))
    (when (and d (< *speed* .4) (< (abs (- d *dist*)) 80.0))
      (list (road-usable *camp-gate-x* d 1.3 #'camp-label #'camp-supply 4.5 1.4)))))
