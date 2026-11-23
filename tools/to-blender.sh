#!/bin/bash
# Move the pieces wrapped in (drawn (parent "NAME") ...) from code to Blender (lisp/models.lisp): run the game once
# without a window and sound so that it writes the pieces down, and make models/NAME.blend of each.
#
#   tools/to-blender.sh            needs the desktop build (`make live`)
#   SOLIDS=1 tools/to-blender.sh   for pieces of the world: what stands in the way gets a cube named solid (an obstacle)
#
# A models/NAME.blend that is already there is left alone: it may have been edited by hand since.
set -euo pipefail
cd "$(dirname "$0")/.."
dir=$(mktemp -d)
KIOSK_DRAW="$dir" KIOSK_NOLOG=1 XDG_DATA_HOME="$dir/data" godot --path project --headless --audio-driver Dummy --quit-after 200 2>&1 | grep "DRAWN\|Ошибка\|DRAW FAILED" || true
for json in "$dir"/*.json; do
  [ -f "$json" ] || continue
  name=$(basename "${json%.json}")
  if ls models/"$name".blend models/*/"$name".blend > /dev/null 2>&1; then echo "to-blender: $name.blend уже есть, не трогаю"; continue; fi
  "${BLENDER:-$HOME/.local/bin/blender}" -b --python tools/draw-model.py -- "$json" "$PWD/models/$name.blend" ${SOLIDS:+solids} 2>&1 | grep "draw-model:\|Error" || true
done
rm -rf "$dir"
