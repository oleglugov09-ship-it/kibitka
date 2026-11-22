#!/bin/bash
# Web build: gdecl and the game cross-compiled for wasm32 with the host ECL,
# linked with ECL for the web into a GDExtension side module,
# project/bin/libgdecl.wasm. Then `make web` exports with the release template.
#
#   web/build.sh            -O0, the default: ECL's GC needs it with --spill-pointers. With -O2
#                           a build started with "memory access out of bounds" every time (18.11),
#                           and earlier ones corrupted memory in play
#   OPT=-O2 web/build.sh    faster code, but see above
#
# Needs: ECL for the web built with Emscripten 4.0.20 (Godot 4.7's) and
# -fPIC -fno-exceptions -sSUPPORT_LONGJMP=wasm (ECL_WASM), that Emscripten
# (EMSDK), the host ECL of the same version (HOST_ECL), and gdecl with
# patches/gdecl-web.patch (GDECL).
set -euo pipefail
ROOT=$(cd "$(dirname "$0")/.." && pwd)
GDECL=${GDECL:-$HOME/src/gdecl-live/gdecl}
ECL_WASM=${ECL_WASM:-$HOME/src/ecl-godot-web/ecl-wasm}
HOST_ECL=${HOST_ECL:-$HOME/src/wecl-spike/ecl/ecl-host/bin/ecl}
EMSDK=${EMSDK:-$HOME/src/emsdk-godot}
OPT=${OPT:--O0}   # -O2 broke the heap: a crash at launch, players' worlds wiped (18.11); -O0 passes ?torture
source "$EMSDK/emsdk_env.sh" >/dev/null 2>&1
mkdir -p "$ROOT/build-web"
# ASDF's fasl cache for pass A of build-web.lisp: compiled with wasm32's sizes,
# so kept apart from the desktop build's
export XDG_CACHE_HOME="$ROOT/build-web/cache"
PROJECT_ROOT="$ROOT/" SYSTEM=kiosk-trip GDECL="$GDECL/" ECL_TARGET="$ECL_WASM/" CROSS_TARGET=wasm32 \
  "$HOST_ECL" --norc --load "$ROOT/web/build-web.lisp"
emcc "$OPT" -fPIC -fno-exceptions -sSUPPORT_LONGJMP=wasm -I"$ECL_WASM" -I"$GDECL/src" \
  -c "$GDECL/src/gdecl.c" -o "$ROOT/build-web/gdecl.o"
# --profiling-funcs keeps the names of functions in the module: a crash in the browser then prints a stack one can read
emcc "$OPT" -fPIC -sSIDE_MODULE=1 -sSUPPORT_LONGJMP=wasm --profiling-funcs \
  -sBINARYEN_EXTRA_PASSES=--spill-pointers \
  -o "$ROOT/project/bin/libgdecl.wasm" \
  "$ROOT/build-web/gdecl.o" "$ROOT/build-web/application.a" \
  "$ECL_WASM/libecl.a" "$ECL_WASM/libeclgc.a" "$ECL_WASM/libeclgmp.a"
ls -la "$ROOT/project/bin/libgdecl.wasm"
