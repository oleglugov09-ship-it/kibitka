;;; Kibitka with a REPL: Slynk in a debug run (127.0.0.1:4009), a queue into Godot's main
;;; thread and the LIVE package (live.lisp) to inspect and change the trip on the fly. `make live`.
(defsystem "kiosk-trip-live"
    :description "Kibitka with Slynk and the LIVE API"
    :depends-on ("kiosk-trip" "slynk")
    :serial t
    :components ((:file "main-thread")
                 (:file "live")
                 (:file "slynk-server")))
