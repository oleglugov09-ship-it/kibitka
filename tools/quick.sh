#!/bin/bash
# The quick round while working on the code: a minute or two instead of the five of the full one.
#
#   tools/quick.sh            names, counts, unit checks, a build of what has changed, the solo game
#   tools/quick.sh pair back  the same, and these checks of tools/smoke.py as well
#
# It does not replace the full round before the code goes anywhere: tools/ship.sh --dry.
# The game runs headless, without sound, with its own data folder; the port is SMOKE_PORT (4309 by default here,
# away from the ports of tools/check.sh).
set -euo pipefail
cd "$(dirname "$0")/.."
export PATH="${ECL_HOST:-$HOME/src/wecl-spike/ecl/ecl-host}/bin:$PATH"
tools/names.py
tools/ratchet.py
ecl --norc --shell tools/unit.lisp 2>&1 | tail -1 | tee /dev/stderr | grep -q "всё сошлось"
make quick > /tmp/kibitka-quick-build.log 2>&1 || { grep -i "error" /tmp/kibitka-quick-build.log | head -20; echo "quick: сборка не удалась, весь вывод в /tmp/kibitka-quick-build.log"; exit 1; }
tmp=$(mktemp -d); port=${SMOKE_PORT:-4309}; pids=(); bad=0
for name in solo "$@"; do
  XDG_DATA_HOME="$tmp/$name" SMOKE_PORT=$port tools/smoke.py "$name" > "$tmp/$name.out" 2>&1 &
  pids+=($!); port=$((port + 10))
done
for pid in "${pids[@]}"; do wait "$pid" || bad=1; done
for name in solo "$@"; do echo "$name: $(grep -c '^  ok' "$tmp/$name.out") ok"; grep "ПРОВАЛ\|Error\|Traceback" "$tmp/$name.out" | head -6 || true; done
rm -rf "$tmp"
exit $bad
