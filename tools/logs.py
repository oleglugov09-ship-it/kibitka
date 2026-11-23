#!/usr/bin/env python3
"""Show the game log from the room server (relay/, Durable Object Log).

    tools/logs.py --summary       the whole log at a glance: launches by day, and every error players ran into
    tools/logs.py                 the first 5000 records, grouped by launch (the oldest: the server counts from 1)
    tools/logs.py --since 1200    only records numbered above 1200
    tools/logs.py --raw           the lines as they are (JSON)

Launches made for checking (a page address with ?dev) are marked in the log since 21.11.2026; the summary counts
them apart, so that a day of testing does not look like a day of players.

The read key is in ~/.config/kibitka/log-key (on the server it is the LOG_KEY secret).
"""
import argparse, collections, datetime, json, os, re, urllib.request

URL = "https://kibitka-relay.liltechdude-dev.workers.dev/logs"
p = argparse.ArgumentParser()
p.add_argument("--since", type=int, default=0)
p.add_argument("--limit", type=int, default=5000)
p.add_argument("--raw", action="store_true")
p.add_argument("--summary", action="store_true")
a = p.parse_args()
key = open(os.path.expanduser("~/.config/kibitka/log-key")).read().strip()


def fetch(since, limit):
    req = urllib.request.Request(f"{URL}?key={key}&since={since}&limit={limit}", headers={"User-Agent": "kibitka-logs"})
    return json.load(urllib.request.urlopen(req, timeout=30))


def summary():
    """Read the whole log, page by page, and print the launches by day and the errors of real launches."""
    rows, since = [], a.since
    while True:
        page = fetch(since, 5000)
        if not page: break
        rows += page; since = page[-1]["id"]
    runs = collections.OrderedDict()
    for r in rows:
        try: e = json.loads(r["line"])
        except ValueError: continue
        runs.setdefault(r["sid"], {"t": r["t"], "v": r["v"], "events": []})["events"].append(e)
    days, errors = collections.OrderedDict(), []
    for run in runs.values():
        ev = run["events"]; when = datetime.datetime.fromtimestamp(run["t"] / 1000)
        start = next((e for e in ev if e.get("e") == "start"), {})
        pulses = [e for e in ev if e.get("e") == "pulse"]
        d = days.setdefault(when.strftime("%d.%m"), collections.Counter())
        d["launches"] += 1
        if start.get("dev"): d["checks"] += 1; continue
        d["long"] += max((e.get("at", 0) for e in ev), default=0) >= 600
        d["company"] += any(e.get("others", 0) > 0 for e in pulses)
        d["km"] += (max(e.get("dist", 0) for e in pulses) - min(e.get("dist", 0) for e in pulses)) / 1000 if pulses else 0
        for e in ev:
            if e.get("e") == "error":
                d["errors"] += 1
                errors.append((when.strftime("%d.%m %H:%M"), run["v"], str(e.get("where")), re.sub(r"#<[^>]*>", "#<…>", str(e.get("text", "")))[:110]))
    print(f"записей {len(rows)}, запусков {len(runs)}")
    print("день    запусков  проверочных  игроков  дольше 10 мин  в компании    км  ошибок")
    for day, d in days.items():
        print(f"{day}  {d['launches']:8d}  {d['checks']:11d}  {d['launches'] - d['checks']:7d}  {d['long']:13d}  {d['company']:10d}  {d['km']:4.0f}  {d['errors']:6d}")
    print("\nошибки у игроков (каждый текст попадает в журнал один раз за запуск):")
    for when, v, where, text in errors: print(f"  {when} · сборка {v} · {where} · {text}")
    if not errors: print("  нет")


if a.summary:
    summary(); raise SystemExit
rows = fetch(a.since, a.limit)
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
