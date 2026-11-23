#!/usr/bin/env python3
"""Was and now: the same views of the wagon shot in two browser builds and put side by side.

    tools/was-now.py WAS-DIR NOW-DIR OUT-DIR      # two folders like export/web; OUT-DIR/NN-name.png, was on the left

No window and no sound (tools/web-eyes.py); each build takes about two minutes. The views: the wagon from four
sides outside and four ways inside. The swaying is switched off, the hour of day is whatever the fresh game starts at.
"""
import os, subprocess, sys, tempfile, time
from PIL import Image
was, now, out = sys.argv[1:4]; here = os.path.dirname(os.path.abspath(__file__)); os.makedirs(out, exist_ok=True)
VIEWS = [("outside-back-right", '(setf *view* :outside *orbit-yaw* .6 *orbit-pitch* .25 *sway* 0.0 *sky-hour* 650.0 *orbit-dist* 9.0) (set-season :spring) (set-weather :clear 600)'),
         ("outside-front-right", '(setf *orbit-yaw* 2.5 *orbit-pitch* .2)'),
         ("outside-front-left", '(setf *orbit-yaw* -2.5 *orbit-pitch* .35)'),
         ("outside-back-left", '(setf *orbit-yaw* -.7 *orbit-pitch* .6)'),
         ("inside-to-front", '(setf *view* :inside *seat* nil *pos* (v3 0 0 1.8) *yaw* 0.0 *pitch* 0.0)'),
         ("inside-to-door", '(setf *pos* (v3 0 0 -1.6) *yaw* 3.1416 *pitch* 0.0)'),
         ("inside-to-window", '(setf *pos* (v3 -.9 0 0) *yaw* -1.5708 *pitch* 0.0)'),
         ("inside-to-carpet", '(setf *pos* (v3 .9 0 0) *yaw* 1.5708 *pitch* .1)'),
         ("inside-up", '(setf *pos* (v3 0 0 0) *yaw* 0.0 *pitch* 1.2)'),
         ("inside-down", '(setf *pos* (v3 0 0 .5) *yaw* 0.0 *pitch* -1.1)')]


def shoot(folder, tag):
    server = subprocess.Popen([sys.executable, "-m", "http.server", "8765", "--bind", "127.0.0.1"], cwd=folder, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    try:
        time.sleep(1)
        steps = ["go", "wait:50", "click:350,174", "wait:4"] + ["click:480,200", "wait:1"] * 4 + ["lisp:(when *debug-on* (toggle-debug))", "wait:1"]
        for name, form in VIEWS: steps += ["lisp:" + form, "wait:2", f"shot:{out}/.{tag}-{name}.png"]
        r = subprocess.run([os.path.join(here, "web-eyes.py"), "http://127.0.0.1:8765/index.html?dev", tempfile.mkdtemp()] + steps, capture_output=True, text=True, timeout=400)
        bad = [l for l in r.stdout.splitlines() if "Ошибка" in l or "Error" in l or "prompt" in l or "нет ответа" in l]
        if bad: print(tag, "\n".join(bad[:8]))
    finally:
        server.terminate(); server.wait()


shoot(was, "was"); shoot(now, "now")
for n, (name, _) in enumerate(VIEWS):
    a, b = (Image.open(f"{out}/.{t}-{name}.png").convert("RGB") for t in ("was", "now"))
    both = Image.new("RGB", (a.width + b.width + 6, a.height), "white"); both.paste(a, (0, 0)); both.paste(b, (a.width + 6, 0))
    both.save(f"{out}/{n:02d}-{name}.png"); print(f"{out}/{n:02d}-{name}.png")
