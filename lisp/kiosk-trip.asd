;;; Kibitka: the wagon drives along an endless road. Godot draws (light, sky,
;;; fog, meshes), Lisp conducts: terrain (land), land and things by the road (world),
;;; the wagon (cart), the door (door), the day cycle (sky), walking and camera (walk), sound (sound),
;;; the record player (jukebox), the meta room (meta), clicking on things (interact), seasons and weather
;;; (climate), the scene root (trip).
(defsystem "kiosk-trip"
    :description "Kibitka on Godot, conducted by Common Lisp"
    :depends-on ("gdecl")
    :serial t
    :components ((:file "packages") (:file "delo") (:file "tverd")
                 (:file "random")
                 (:file "engine-classes")
                 (:file "shapes") (:file "models")
                 (:file "economy")
                 (:file "profile")
                 (:file "textures")
                 (:file "land") (:file "karta") (:file "razmetka")
                 (:file "world")
                 (:file "cart")
                 (:file "place") (:file "sky")
                 (:file "en") (:file "walk")
                 (:file "door")
                 (:file "sound") (:file "save") (:file "stamps")
                 (:file "jukebox")
                 (:file "meta")
                 (:file "interact")
                 (:file "climate")
                 (:file "doroga")
                 (:file "interior")
                 (:file "city")
                 (:file "places") (:file "marks") (:file "towns") (:file "stops") (:file "mega") (:file "mega-zones") (:file "mega-monuments") (:file "leto") (:file "gc-probe") (:file "traffic") (:file "life") (:file "vista")
                 (:file "byt") (:file "trade") (:file "clients") (:file "goods") (:file "plan") (:file "istorii") (:file "uyut") (:file "kettle") (:file "fridge") (:file "mood") (:file "home") (:file "net") (:file "lobby") (:file "look") (:file "stars") (:file "finds") (:file "forage") (:file "dalny") (:file "prikol") (:file "igry") (:file "activities") (:file "bumagi") (:file "debt") (:file "zagranitsa") (:file "kopii") (:file "people") (:file "gnomy") (:file "guide") (:file "sync") (:file "log") (:file "map") (:file "dev") (:file "mods")
                 (:file "trip")))
