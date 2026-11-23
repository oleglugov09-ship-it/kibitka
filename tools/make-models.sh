#!/bin/bash
# The models drawn in Blender, made ready for the game: every models/GROUP/NAME.blend becomes project/models/NAME.glb
# (the folders only sort the drawings for people: a model is known to the game by NAME alone, so names are unique),
# which the game puts in place with (model parent "NAME" x y z) (lisp/models.lisp).
#
#   tools/make-models.sh          export what is missing or older than its .blend
#   tools/make-models.sh -B       export everything
#
# What goes into the model: every mesh of the file, with its modifiers applied; cameras and lights stay behind.
# Empties named at_NAME and cubes named surface_NAME are places the game's rules read (lisp/mesta.lisp); cubes named
# solid are obstacles. None of them is drawn.
# Axes: the game's x (right), y (up), z (along the wagon, the door at +z) are Blender's x, z and -y; a metre is a
# metre; the origin of the file is the point the game places. Needs Blender 4.2 or newer (BLENDER to choose one).
set -euo pipefail
cd "$(dirname "$0")/.."
blender="${BLENDER:-$HOME/.local/bin/blender}"
mkdir -p project/models
n=0
for blend in models/*.blend models/*/*.blend; do
  [ -f "$blend" ] || continue
  glb="project/models/$(basename "${blend%.blend}").glb"
  if [ "${1:-}" != "-B" ] && [ -f "$glb" ] && [ ! "$blend" -nt "$glb" ]; then continue; fi
  "$blender" -b "$blend" --python-expr "
import bpy
for ob in bpy.data.objects: ob.select_set(ob.type in ('MESH', 'EMPTY'))
bpy.ops.export_scene.gltf(filepath='$PWD/$glb', export_format='GLB', use_selection=True, export_apply=True,
                          export_yup=True, export_cameras=False, export_lights=False)
" > /dev/null
  test -s "$glb"
  printf '[remap]\n\nimporter="keep"\n' > "$glb.import"     # the game reads the file itself, as bytes (tools/keep-raw.sh)
  n=$((n+1))
done
echo "make-models: $n моделей"
