#!/bin/bash
# The game reads textures, sounds and discs itself, as bytes (lisp/files.lisp FILE-BYTES).
# So that on export Godot packs the source files rather than its own reprocessed versions,
# each one gets the "keep" import (in the editor: Import → Keep File). Run from `make web`.
set -euo pipefail
cd "$(dirname "$0")/../project"
n=0
for f in textures/*/*.png textures/*/*.webp sounds/*.wav discs/*.wav discs/*.mp3; do
  [ -f "$f" ] || continue
  printf '[remap]\n\nimporter="keep"\n' > "$f.import"
  n=$((n+1))
done
echo "keep-raw: $n файлов"
