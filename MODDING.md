# Modding Kibitka

A mod is a folder with one file, `mod.lisp`: plain Common Lisp that the game reads and evaluates at
startup, in the game package, before the world is planned and the save is loaded.

| Where | Path | Needs a rebuild |
|---|---|---|
| Shipped with the game | `project/mods/<name>/mod.lisp` | yes (`make live` / `make web`) |
| The player's own | `<user data>/mods/<name>/mod.lisp` | no, just restart the game |

On Linux the user data folder is `~/.local/share/godot/app_userdata/Kiosk Trip (Lisp)/`. The browser
build only loads the shipped mods.

Mods load in alphabetical order, the player's after the shipped ones. A mod that fails does not stop
the game: the error goes to the log and the debug screen (`F3`) shows the mod as `FAILED`.

## What the game ships as a mod

`project/mods/stories/` — all nine town stories (chains of parcels with names and lines) and their
English text. The core (`lisp/istorii.lisp`) only knows how to carry a story; with the folder removed
the game runs with plain parcels.

## What a mod may call

| Form | What it does |
|---|---|
| `(define-story :key step...)` | adds a chain of story parcels; the same key replaces a story |
| `(translate "ru" "en" ...)` | adds English text for the mod's own Russian strings |

A story step is `(from-town to-town sender sender-words recipient recipient-words radio-summary
(town who words))`. Towns are numbers 0–11 along the ring road. Text is written in Russian (the game's
source language) and must not contain `"` or `\`: it travels over the network.

```lisp
(define-story :kettle
 (0 1 "Дед Егор" "Внучке чайник. Свистит как паровоз."
  "Внучка Аня" "Свистит! Соседи уже спрашивали, куда мы едем."
  "в Берёзовке завёлся чайник с паровозным свистком."
  (0 "Дед Егор" "Свистит чайник-то?")))

(translate
 "Дед Егор" "Grandpa Yegor"
 "Внучке чайник. Свистит как паровоз." "A kettle for my granddaughter. Whistles like a steam engine.")
```

A mod is ordinary Lisp, so nothing stops it from calling anything else in the game — but only the
forms above are meant to stay stable.

## Limits, as of now

- Players in one wagon must have the same mods: the game does not compare them when a guest joins.
- A player's mod is code that runs with the game's rights. Install only mods you trust.
- There is no switch to turn a mod off other than removing its folder.
