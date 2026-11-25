;;;; The main menu: the screen the game opens with, and the one "Main Menu" of the pause menu comes back to.
;;;; Three buttons: ride (our own world, alone), play together (the lobby panel, lobby.lisp), quit.
;;;;
;;;; The game is one scene, and the world behind the menu is the real one: the wagon stands, the day goes on.
;;;; Leaving for the menu (TO-MAIN-MENU) saves the game, closes a network game and stops the wagon; nothing is
;;;; unloaded, so "ride" continues from where we stood. Buttons are polled every frame, as in the lobby.

(in-package #:kiosk-trip)

(defvar *menu* nil "The main menu panel (a full-screen Control).")
(defvar *menu-open* nil "True while the main menu is on the screen: the game keys do not work.")
(defvar *menu-parts* nil "The parts of the main menu, a plist: :ride :friends :quit.")

(defun front-screen-p ()
  "Return true while a screen that stands before the game is up: the main menu or the lobby. The game keys do not
work then, and what waits for the trip to begin (the first words of the story, the copies) goes on waiting."
  (or *menu-open* *lobby-open*))

(defun make-menu (root)
  "Build the main menu over the game under ROOT, hidden."
  (let ((bg (screen-panel 6 0 0 640 360 '(.04 .05 .06 .78) root)))
    ;; a sheet of the same yellowed paper as the waybill of the lobby
    (hud-rect bg 200 64 240 232 '(.88 .84 .70) .98)
    (hud-rect bg 200 64 240 3 '(.42 .30 .16))
    (hud-rect bg 200 293 240 3 '(.42 .30 .16))
    (hud-label bg "КИБИТКА" 262 82 22 '(.20 .16 .12))
    (hud-label bg (format nil "автобаза № 3 · выпуск ~a" *net-version*) 252 112 7 '(.42 .36 .28))
    (let ((ride (lobby-control "Button" bg 222 142 196 28 "Ехать"))
          (friends (lobby-control "Button" bg 222 178 196 28 "Играть вместе"))
          (quit (lobby-control "Button" bg 222 242 196 28 "Выйти из игры")))
      (dolist (b (list ride friends quit)) (object-set b "toggle_mode" t))
      (object-set quit "visible" (not (object-call (gdext:singleton "OS") "has_feature" "web")))   ; a browser tab is closed by its own cross
      (setf *menu-parts* (list :ride ride :friends friends :quit quit)))
    (setf *menu* bg)
    (object-set bg "visible" nil)))

(defun open-menu ()
  "Show the main menu."
  (setf *menu-open* t)
  (object-set *menu* "visible" t)
  (object-set (getf *menu-parts* :ride) "text" (if (> *dist* 12) "Продолжить поездку" "Ехать"))
  (input-set-mouse-mode 0))

(defun close-menu ()
  "Hide the main menu: back to the game."
  (setf *menu-open* nil)
  (object-set *menu* "visible" nil)
  (input-set-mouse-mode 2))

(defun menu-clicked (key)
  "Return true if the button KEY of the main menu was pressed since the last frame, and release it."
  (let ((b (getf *menu-parts* key)))
    (when (object-get b "button_pressed") (object-set b "button_pressed" nil) t)))

(defun to-main-menu ()
  "Leave the trip for the main menu: the game is saved, a network game is closed, the wagon stands."
  (when *lobby-open* (close-lobby))
  (when *net-role* (net-stop))                              ; a guest is at home again, a driver's guests are let go
  (cancel-course) (halt)
  (save-game-now)
  (log-event :main-menu :dist (round *dist*))
  (open-menu))

(defun step-menu ()
  "Answer the buttons of the main menu, and the pause menu's wish to go there (scenes/pause_menu.gd leaves a note
on the scene root: the menus are Godot's, the game is ours)."
  (when (and *root* (object-call *root* "has_meta" "to_main_menu"))
    (object-call *root* "remove_meta" "to_main_menu")
    (to-main-menu))
  (when *menu-open*
    (cond ((menu-clicked :ride) (close-menu))
          ((menu-clicked :friends) (close-menu) (open-lobby))
          ((menu-clicked :quit) (save-game-now) (object-call (object-call *root* "get_tree") "quit")))))
