#!/bin/bash
# All checks at once: the solo game, a driver with a guest, and a tour of the whole road run simultaneously,
# each with its own ports and its own temporary data folder (the player's saves are not touched).
# Usage: tools/check.sh [solo] [pair] [tour]  — with no words given, all of them run. Needs the `make live` build.
cd "$(dirname "$0")/.." || exit 1
what="${*:-solo pair tour back saves}"; tmp=$(mktemp -d); port=4109; pids=()
for name in $what; do
  XDG_DATA_HOME="$tmp/$name" SMOKE_PORT=$port tools/smoke.py "$name" > "$tmp/$name.out" 2>&1 &
  pids+=($!); port=$((port + 10))
done
bad=0
for pid in "${pids[@]}"; do wait "$pid" || bad=1; done
for name in $what; do grep -v "^  ok" "$tmp/$name.out" | tail -6; done
rm -rf "$tmp"
exit $bad
