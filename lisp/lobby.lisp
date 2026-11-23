;;;; The in-game lobby: a panel at startup and on F2. On the left: ride alone, open a lobby, or connect
;;;; by code or address. On the right: the list of open lobbies from the room server; joining is one button,
;;;; no code and no password (everyone who has the game sees the list). The name and the last address are
;;;; remembered in user://lobby.sexp. Buttons are polled every frame (toggle_mode): no Godot signals needed here.

(in-package #:kiosk-trip)

(defvar *lobby* nil "The lobby panel (a full-screen Control).")
(defvar *lobby-open* nil)
(defvar *lobby-f2* nil "True if F2 was held on the previous frame (we catch a fresh press to close the panel).")
(defvar *lobby-parts* nil "The panel parts, a plist: :name :address :solo :host :join :leave :stuck :fresh :note
:rooms (the list buttons) :rooms-note.")
(defvar *lobby-rooms* '() "The open lobbies: (CODE GUESTS DRIVER-NAME).")
(defvar *lobby-ws* nil "The socket used to ask for the lobby list.")
(defvar *lobby-asked* -100.0 "The time the list was last asked for.")
(defvar *lobby-opened* 0.0 "The time the panel was opened.")
(defparameter *lobby-rows* 7)


(defun lobby-saved ()
  "Return (NAME ADDRESS) saved from last time."
  (let ((p (get-save :lobby)))
    (if (fits p '(string string)) p (list "" ""))))

(defun lobby-save (name address)
  (ignore-errors (put-save :lobby (list name address))))

(defun lobby-control (class parent x y w h &optional text)
  (let ((c (gdext:make-object class)))
    (node-add-child parent c)
    (object-set c "position" (vector2 x y)) (object-set c "size" (vector2 w h))
    (object-call c "add_theme_font_size_override" "font_size" 10)
    (when text (object-set c "text" text))
    c))

(defun make-lobby (root)
  (multiple-value-bind (bg layer) (screen-panel 5 0 0 640 360 '(.04 .05 .06 .72) root)
    ;; a waybill: yellowed paper, ruled lines, the depot's stamp
    (hud-rect bg 60 60 520 240 '(.88 .84 .70) .98)
    (hud-rect bg 60 60 520 3 '(.42 .30 .16))
    (hud-rect bg 60 297 520 3 '(.42 .30 .16))
    (hud-rect bg 338 100 1 184 '(.55 .48 .36))
    (loop for y from 94 to 290 by 28 do (hud-rect bg 72 y 496 1 '(.74 .68 .54)))
    (hud-label bg "ПУТЕВОЙ ЛИСТ" 80 68 16 '(.20 .16 .12))
    (hud-label bg (format nil "форма № 4-п · выпуск ~a" *net-version*) 250 76 7 '(.42 .36 .28))
    (hud-label bg "Водитель или попутчик: как записать" 80 100 8 '(.26 .20 .14))
    (hud-label bg "Маршруты на линии — нажми, чтобы сесть" 352 100 8 '(.26 .20 .14))
    (let ((stamp (hud-label bg "АВТОБАЗА № 3" 452 250 9 '(.62 .16 .14))) (line (hud-label bg "ВЫЕЗД РАЗРЕШЁН" 446 262 7 '(.62 .16 .14))))
      (object-set stamp "rotation" -.12) (object-set line "rotation" -.12))
    (destructuring-bind (saved-name saved-address) (lobby-saved)
      (let ((name (lobby-control "LineEdit" bg 80 114 244 22 saved-name))
            (solo (lobby-control "Button" bg 80 146 244 22 "Выехать одному"))
            (host (lobby-control "Button" bg 80 172 244 22 "Открыть маршрут — я за рулём"))
            (address (lobby-control "LineEdit" bg 80 214 136 22 saved-address))
            (join (lobby-control "Button" bg 222 214 102 22 "Сесть"))
            (leave (lobby-control "Button" bg 80 262 244 22 "Выйти из сетевой игры"))
            (stuck (lobby-control "Button" bg 352 262 212 22 "Застрял — в центр кибитки (U)"))
            (fresh (lobby-control "Button" bg 352 236 212 22 "Новая игра со склада"))
            (lang (lobby-control "Button" bg 500 68 64 18 ""))
            (rooms (loop for i below *lobby-rows* collect (lobby-control "Button" bg 352 (+ 114 (* i 24)) 212 22 ""))))
        (hud-label bg "Или маршрут № (четыре буквы) / адрес водителя" 80 200 8 '(.26 .20 .14))
        (object-set name "placeholder_text" "Имя") (object-set name "max_length" 16)
        (object-set address "placeholder_text" "ABCD")
        (dolist (b (list* solo host join leave stuck fresh lang rooms)) (object-set b "toggle_mode" t))
        (dolist (b rooms) (object-set b "visible" nil))
        (setf *lobby-parts* (list :name name :address address :solo solo :host host :join join :leave leave :stuck stuck :fresh fresh :lang lang
                                  :rooms rooms
                                  :rooms-note (hud-label bg "Ищем…" 352 118 8 '(.42 .36 .28))
                                  :note (hud-label bg "" 80 242 8 '(.62 .16 .14))))))
    (setf *lobby* bg)
    (object-set bg "visible" nil)))

(defun ask-rooms ()
  "Ask the room server for the list of open lobbies."
  (setf *lobby-asked* *time*)
  (when *lobby-ws* (gdext:release-reference *lobby-ws*) (setf *lobby-ws* nil))
  (let ((base (relay-url)))
    (when base
      (let ((ws (gdext:add-reference (gdext:make-object "WebSocketPeer"))))
        (if (eql 0 (object-call ws "connect_to_url" (format nil "~a/list?v=~a" base *net-version*)))
            (setf *lobby-ws* ws)
            (gdext:release-reference ws))))))

(defun split-by (char text)
  (loop for start = 0 then (1+ end)
        for end = (position char text :start start)
        collect (subseq text start end)
        while end))

(defun show-rooms ()
  (let ((rooms (remove *net-room* *lobby-rooms* :key #'first :test #'equal)))    ; we do not show our own lobby to ourselves
    (loop for b in (getf *lobby-parts* :rooms) for i from 0
          for room = (nth i rooms)
          do (object-set b "visible" (and room t))
             (when room
               (destructuring-bind (code guests name) room
                 (declare (ignore code))
                 (object-set b "text" (format nil "~a · в кибитке ~d" name (1+ guests))))))
    (object-set (getf *lobby-parts* :rooms-note) "text"
                (cond (rooms "") ((relay-url) "Пока никого. Открой своё — друзья увидят его здесь") (t "Сервер комнат не настроен")))))

(defun step-rooms ()
  "Refresh the lobby list: ask every 5 seconds while the panel is open; the answer is one message."
  ;; the first half minute: every 5 seconds, then once in 20, after 10 minutes we stop (F2 again restarts it):
  ;; a forgotten tab must not knock on the server around the clock
  (let ((open-for (- *time* *lobby-opened*)))
    (when (and (< open-for 600) (> (- *time* *lobby-asked*) (if (< open-for 30) 5.0 20.0))) (ask-rooms)))
  (when *lobby-ws*
    (dolist (text (ws-texts *lobby-ws*))
      (setf *lobby-rooms*
            (loop for line in (split-by #\Newline text)
                  for parts = (split-by #\Tab line)
                  when (and (= (length parts) 3) (room-code-p (first parts)))
                    collect (list (first parts) (or (ignore-errors (parse-integer (second parts))) 0) (clean-name (third parts) "Водитель"))))
      (show-rooms)
      (object-call *lobby-ws* "close" 1000 ""))                 ; answer received: we close from our side
    (when (= (ws-state *lobby-ws*) 3)
      (gdext:release-reference *lobby-ws*) (setf *lobby-ws* nil))))

(defun open-lobby ()
  (setf *lobby-open* t)
  (object-set *lobby* "visible" t)
  (object-set (getf *lobby-parts* :leave) "visible" (and *net-role* t))
  (object-set (getf *lobby-parts* :fresh) "visible" (> *time* 3))          ; it is of no use on the start screen
  ;; in the middle of a game the top button simply closes the panel
  (object-set (getf *lobby-parts* :solo) "text"
              (cond (*net-role* "Вернуться в игру (F2)") ((> *time* 3) "Вернуться к поездке (F2)") (t "Выехать одному")))
  (godot:object-set (getf *lobby-parts* :lang) "text" (if (eq *lang* :en) "Русский" "English"))
  (setf *lobby-f2* t *lobby-asked* -100.0 *lobby-opened* *time*)
  (show-rooms)
  (object-set (getf *lobby-parts* :note) "text" "")
  (input-set-mouse-mode 0))

(defun close-lobby ()
  (setf *lobby-open* nil)
  (object-set *lobby* "visible" nil)
  (dolist (k '(:name :address)) (object-call (getf *lobby-parts* k) "release_focus"))
  (input-set-mouse-mode 2))

(defun lobby-clicked (key)
  "Return true if button KEY was pressed since the last frame, and release it."
  (let ((b (getf *lobby-parts* key)))
    (when (object-get b "button_pressed") (object-set b "button_pressed" nil) t)))

(defun step-lobby ()
  (when *lobby-open*
    ;; F2 closes the panel the same way it opened it (game keys do not work while it is open)
    (let ((down (input-is-key-pressed +key-f2+)))
      (when (and down (not *lobby-f2*)) (close-lobby) (return-from step-lobby))
      (setf *lobby-f2* down))
    (step-rooms)
    ;; joining someone else's lobby with one button
    (loop for b in (getf *lobby-parts* :rooms) for i from 0
          do (when (object-get b "button_pressed")
               (object-set b "button_pressed" nil)
               (let ((room (nth i (remove *net-room* *lobby-rooms* :key #'first :test #'equal)))
                     (name (string-trim " " (object-get (getf *lobby-parts* :name) "text"))))
                 (when room
                   (lobby-save name (first room))
                   (log-event :lobby :choice :list :name name)
                   (when (net-join (first room) (if (plusp (length name)) name "Попутчик")) (close-lobby))
                   (return-from step-lobby)))))
    (let ((name (string-trim " " (object-get (getf *lobby-parts* :name) "text")))
          (address (string-trim " " (object-get (getf *lobby-parts* :address) "text"))))
      (flet ((note (text) (object-set (getf *lobby-parts* :note) "text" text)))
        (cond ((lobby-clicked :lang)
               (set-lang (if (eq *lang* :en) :ru :en))
               (godot:object-set (getf *lobby-parts* :lang) "text" (if (eq *lang* :en) "Русский" "English")))
              ((lobby-clicked :stuck) (unstuck) (close-lobby))
              ((lobby-clicked :fresh)
               (if (net-guest-p) (note "Новую игру начинает водитель") (progn (new-game) (close-lobby))))
              ((lobby-clicked :solo) (lobby-save name address)
               (unless (or *net-role* (> *time* 3)) (log-event :lobby :choice :solo :name name))
               (close-lobby))
              ((lobby-clicked :leave) (net-stop) (say "Сетевая игра закрыта") (close-lobby))
              ((lobby-clicked :host)
               (lobby-save name address) (log-event :lobby :choice :host :name name)
               (if (net-host (if (plusp (length name)) name "Водитель")) (close-lobby) (note "Не вышло: порт занят и сервера комнат нет")))
              ((lobby-clicked :join)
               (cond ((zerop (length address)) (note "Впиши код комнаты или адрес"))
                     (t (lobby-save name address) (log-event :lobby :choice :join :name name :code (room-code-p address))
                        (if (net-join address (if (plusp (length name)) name "Попутчик"))
                            (close-lobby)
                            (note (if (room-code-p address) "Игра по коду пока не настроена" "Такой адрес не годится")))))))))))
