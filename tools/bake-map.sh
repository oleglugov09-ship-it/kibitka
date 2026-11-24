#!/bin/bash
# Bake the map of the base world from the formulas of the land (lisp/bake.lisp) into project/terrain:
# the terrain's shader, the regions of Terrain3D, the water, the roads. Needs the desktop build (`make live`).
#
#   tools/bake-map.sh                       the whole map: about half an hour
#   KIOSK_BAKE_ONLY="-1 -1 1 1" tools/bake-map.sh    only the regions from (-1 -1) to (1 1): a piece, for a look
#
#   tools/bake-map.sh --roads               only the ribbons of the roads (project/terrain/roads), from the baked map
#
#   tools/bake-map.sh --editor              only the shader and what the Godot editor needs (terrain.tscn, assets.res,
#                                           material.res); the baked map stays as it is
#
# What was baked before is removed first. Runs without a window and without sound.
set -euo pipefail
cd "$(dirname "$0")/.."
mkdir -p project/terrain/data project/terrain/water project/terrain/roads
if [ "${1:-}" = "--roads" ]; then         # only the ribbons of the roads, from the map as it is baked: a minute
  mkdir -p project/terrain/roads && rm -f project/terrain/roads/*.res
  KIOSK_BAKE_ROADS=1 KIOSK_NOLOG=1 KIOSK_SOLO=1 XDG_DATA_HOME="$(mktemp -d)" \
    timeout 1800 godot --path project --headless --audio-driver Dummy 2>&1 | grep "BAKE\|Ошибка\|ERROR" || true
  exit 0
fi
if [ "${1:-}" = "--editor" ]; then        # only the shader, the scene and the resources for the editor: the map is not touched
  export KIOSK_BAKE_EDITOR=1
else
  rm -f project/terrain/roads/*.res project/terrain/data/*.res project/terrain/water/*.res project/terrain/water/*.import project/terrain/data/*.import
fi
tmp=$(mktemp -d)
KIOSK_BAKE=1 KIOSK_BAKE_SHADER="$tmp/terrain3d.gdshader" KIOSK_NOLOG=1 KIOSK_SOLO=1 XDG_DATA_HOME="$tmp/data" \
  timeout 7200 godot --path project --headless --audio-driver Dummy 2>&1 | grep "BAKE\|Ошибка\|ERROR" || true
mkdir -p build && cp "$tmp/terrain3d.gdshader" build/terrain3d.gdshader         # kept: tools/terrain-shader.py can be run again without baking
tools/terrain-shader.py build/terrain3d.gdshader project/terrain/terrain.gdshader    # Terrain3D's own shader, with the game's changes
# The scene for the Godot editor. The region size is left at Terrain3D's own 256: a scene that sets it after the data crashes
cat > project/terrain/terrain.tscn <<'SCENE'
[gd_scene format=3]

[ext_resource type="Terrain3DMaterial" path="res://terrain/material.res" id="1"]
[ext_resource type="Terrain3DAssets" path="res://terrain/assets.res" id="2"]

[node name="Terrain3D" type="Terrain3D"]
data_directory = "res://terrain/data"
material = ExtResource("1")
assets = ExtResource("2")
collision_mode = 0
mesh_lods = 7
vertex_spacing = 4.0
SCENE
rm -rf "$tmp"
ls project/terrain/data | wc -l
du -sh project/terrain
