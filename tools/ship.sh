#!/bin/bash
# The whole way to the site, checks first: nothing is built for the browser, let alone deployed, until the game
# has been driven through every place without an error.
#
#   tools/ship.sh           check, build for the browser, deploy to Fly
#   tools/ship.sh --dry     everything but the deploy
#
# The checks, in the order of their cost: names defined twice (tools/names.py, a second); the unit checks
# (tools/unit.lisp, seconds); the drive-through (tools/check.sh tour, two minutes, needs the desktop build that
# `make live` makes): every town, the railway, the bridge, the tunnel, and every zone of every place behind the
# tunnel, by day and by night, with no error in the game's log. It runs headless, without sound and with its own
# data folder: the player's saves are not touched.
set -euo pipefail
cd "$(dirname "$0")/.."
export PATH="${ECL_HOST:-$HOME/src/wecl-spike/ecl/ecl-host}/bin:$PATH"
tools/names.py
ecl --norc --shell tools/unit.lisp 2>&1 | tail -1 | tee /dev/stderr | grep -q "всё сошлось"
make live > /dev/null
tools/check.sh tour
make web 2>&1 | grep -i "error\|pack-assets\|keep-raw:" || true
test -s export/web/index.pck
if [ "${1:-}" = "--dry" ]; then echo "ship: готово к выкладке (без выкладки)"; exit 0; fi
fly deploy --remote-only --ha=false --yes
