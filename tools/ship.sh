#!/bin/bash
# The whole way to the site, checks first: nothing is built for the browser, let alone deployed, until the game
# has been driven through every place without an error.
#
#   tools/ship.sh           check, build for the browser, deploy to Fly
#   tools/ship.sh --dry     everything but the deploy
#
# The checks, in the order of their cost: names defined twice or gone but still used (tools/names.py, a second);
# the counts that must not grow (tools/ratchet.py, a second); the unit checks
# (tools/unit.lisp, seconds); then, all at once (tools/check.sh), the solo game, turning back, the saves, a driver with
# a guest (only when really deploying: it needs the internet) and the drive-through (two minutes, needs the desktop build that
# `make live` makes): every town, the railway, the bridge, the tunnel, and every zone of every place behind the
# tunnel, by day and by night, with no error in the game's log. It runs headless, without sound and with its own
# data folder: the player's saves are not touched.
set -euo pipefail
cd "$(dirname "$0")/.."
export PATH="${ECL_HOST:-$HOME/src/wecl-spike/ecl/ecl-host}/bin:$PATH"
tools/names.py
tools/ratchet.py
ecl --norc --shell tools/unit.lisp 2>&1 | tail -1 | tee /dev/stderr | grep -q "всё сошлось"
make live > /dev/null
# pair goes through the real relay (it needs the internet), so it runs only when we are really deploying
checks="solo tour back saves"
[ "${1:-}" = "--dry" ] || checks="$checks pair"
tools/check.sh $checks
# a build that fails must stop here: the old export is removed first, so that it cannot pass for the new one
rm -f export/web/index.pck
if ! make web > /tmp/kibitka-web-build.log 2>&1; then
  grep -i "error" /tmp/kibitka-web-build.log | tail -20
  echo "ship: сборка для браузера не удалась, весь вывод в /tmp/kibitka-web-build.log"; exit 1
fi
grep -i "pack-assets\|keep-raw:" /tmp/kibitka-web-build.log || true
test -s export/web/index.pck
if [ "${1:-}" = "--dry" ]; then echo "ship: готово к выкладке (без выкладки)"; exit 0; fi
fly deploy --remote-only --ha=false --yes
