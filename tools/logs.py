#!/usr/bin/env python3
"""Show the game log from the room server (relay/, Durable Object Log).

    tools/logs.py                 the latest events, grouped by launch
    tools/logs.py --since 1200    only records numbered above 1200
    tools/logs.py --raw           the lines as they are (JSON)

The read key is in ~/.config/kibitka/log-key (on the server it is the LOG_KEY secret).
"""
import argparse, collections, datetime, json, os, urllib.request

URL = "https://kibitka-relay.liltechdude-dev.workers.dev/logs"
p = argparse.ArgumentParser()
p.add_argument("--since", type=int, default=0)
p.add_argument("--limit", type=int, default=5000)
p.add_argument("--raw", action="store_true")
a = p.parse_args()
key = open(os.path.expanduser("~/.config/kibitka/log-key")).read().strip()
req = urllib.request.Request(f"{URL}?key={key}&since={a.since}&limit={a.limit}", headers={"User-Agent": "kibitka-logs"})
rows = json.load(urllib.request.urlopen(req, timeout=30))
if a.raw:
    for r in rows:
        print(r["id"], r["sid"], r["v"], r["line"])
    raise SystemExit
runs = collections.OrderedDict()
for r in rows:
    runs.setdefault(r["sid"], []).append(r)
for sid, rs in runs.items():
    when = datetime.datetime.fromtimestamp(rs[0]["t"] / 1000).strftime("%d.%m %H:%M")
    print(f"\n== запуск {sid} · сборка {rs[0]['v']} · {when} · записей {len(rs)} (№ {rs[0]['id']}–{rs[-1]['id']})")
    for r in rs:
        try:
            e = json.loads(r["line"])
        except ValueError:
            print("   ?", r["line"][:120]); continue
        name, at = e.pop("e", "?"), e.pop("at", 0)
        print(f"  {at // 60:3d}:{at % 60:02d}  {name:<14} " + "  ".join(f"{k}={v}" for k, v in e.items()))
