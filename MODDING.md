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

## What the game ships as mods

The core in `lisp/` is logic only; what the game shows comes from two mods.

| Mod | What is in it |
|---|---|
| `project/mods/base/` | the content the world is planned from — see the table below |
| `project/mods/stories/` | the nine town stories: chains of parcels with names and lines |

| File in `base/` | Tables it sets |
|---|---|
| `towns.lisp` | town names, shop signs and slogans, the goods with their prices and traits, what each town makes and lacks, cooktop recipes, the traits of dishes |
| `economy.lisp` | the deals at the booths, who comes to the window, what needs a licence |
| `wagon.lisp` | what stands in the wagon at the start of a new game |
| `story.lisp` | the cards of the intro and the closing credits |
| `calendar.lisp` | the days of the week, the notes and the omens of the tear-off calendar |
| `forces.lisp` | who answers for each town's supply, and which town is its rival |
| `seasons.lisp` | what the towns use more of in each season, the holidays, the loudspeaker, the shift, the pioneer camp's canteen, the record sleeves |
| `people.lisp` | the people of the road; written by `tools/make-people.py`, edit the table there |
| `booths.lisp` | the booths with gimmicks, their notes for the guide book, town council quests |
| `gnomes.lisp` | where each gnome hides and what it is called |
| `places.lisp` | the relief of each stretch, landmarks, diners, what the telescope shows |
| `looks.lisp` | the outfits |
| `guide.lisp` | the pages of the guide book, what the radio says when it has nothing to report |
| `lines.lisp` | what the named characters say (the sergeant, the chairman) |
| `en.lisp` | English for all of the above |

Each table is declared empty in the core with a docstring that describes its shape; a mod fills it
with `(setf *table* '(...))`. Without `base` the game has no world to plan and will not start; without
`stories` it runs with plain parcels.

### A good of your own

A good is one record in `*goods*`: `(kind "name in the accusative" price key value ...)`. The traits after the
price are optional: `:spoils t` — it goes bad overnight outside the fridge; `:stack 3` — so many stand on one
another on a shelf; `:cut "#d89a8a"` — it can be cut on the board, and that is the colour of its slices. The
core has models for its own goods; any other good is drawn as a plain packet with a paper label, and for
that it wants `:size (width height depth)` in metres, `:rgb "#colour"` and `:title "name in the nominative"`:

```lisp
(setf *goods* (append *goods* '((:soap "мыло" 6 :stack 4 :size (.1 .05 .07) :rgb "#e8d8c0" :title "мыло"))))
```

Put it into a town's `*town-trade*` and it is made, allocated, short and sold like the rest. It may stand
anywhere in the list: crates and parcels on the road remember a good or a story by its name, not by its
place. Only renaming a good or a story key orphans what is already on the road.

What a booth, a gnome or a recipe *does* is still code in the core: a mod can move, rename and reprice
them, but a new kind of booth needs code.

## What a mod may call

| Form | What it does |
|---|---|
| `(define-story :key step...)` | adds a chain of story parcels; the same key replaces a story |
| `(translate "ru" "en" ...)` | adds English text for the mod's own Russian strings |
| `(include "file.lisp")` | reads another file from the mod's own folder |
| `(setf *table* '(...))` | replaces one of the content tables listed above |

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
