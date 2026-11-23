#!/bin/bash
# Bake the map of the base world from the formulas of the land (lisp/bake.lisp) into project/terrain:
# the terrain's shader, the regions of Terrain3D, the water, the roads. Needs the desktop build (`make live`).
#
#   tools/bake-map.sh                       the whole map: about half an hour
#   KIOSK_BAKE_ONLY="-1 -1 1 1" tools/bake-map.sh    only the regions from (-1 -1) to (1 1): a piece, for a look
#
# What was baked before is removed first. Runs without a window and without sound.
set -euo pipefail
cd "$(dirname "$0")/.."
mkdir -p project/terrain/data project/terrain/water
rm -f project/terrain/data/*.res project/terrain/water/*.res project/terrain/water/*.import project/terrain/data/*.import
tmp=$(mktemp -d)
KIOSK_BAKE=1 KIOSK_BAKE_SHADER="$tmp/terrain3d.gdshader" KIOSK_NOLOG=1 KIOSK_SOLO=1 XDG_DATA_HOME="$tmp/data" \
  timeout 7200 godot --path project --headless --audio-driver Dummy 2>&1 | grep "BAKE\|Ошибка\|ERROR" || true
tools/terrain-shader.py "$tmp/terrain3d.gdshader" project/terrain/terrain.gdshader    # Terrain3D's own shader, with the game's changes
rm -rf "$tmp"
ls project/terrain/data | wc -l
du -sh project/terrain
