#!/usr/bin/env python3
"""Was and now: the same views of the wagon shot in two browser builds and put side by side.

    tools/was-now.py WAS NOW OUT-DIR      # WAS and NOW: a folder like export/web, or the address of a build on a site
                                          # (https://.../play/index.html); OUT-DIR/NN-name.png, was on the left

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
         ("inside-down", '(setf *pos* (v3 0 0 .5) *yaw* 0.0 *pitch* -1.1)'),
         ("booth", '(setf *view* :outside *orbit-dist* 13.0 *orbit-yaw* -1.45 *orbit-pitch* .2)'),
         ("booth-yard", '(setf *orbit-dist* 17.0 *orbit-yaw* -2.2 *orbit-pitch* .5)'),
         ("stops-right", """(progn (setf *view* :outside *orbit-dist* 22.0 *orbit-yaw* 1.2 *orbit-pitch* .45) (loop for k in '(1 3 4 5 8 10 14 17) for n from 0 do (let ((g (group *cart* (if (< n 4) 7.0 -7.0) 0.0 (- (* 6.5 (mod n 4)) 14.0)))) (funcall (if (fboundp 'stop-look) 'stop-look 'build-stop) g (* 7.0 k)))) 1)"""),
         ("stops-left", '(setf *orbit-yaw* -1.9 *orbit-pitch* .35)'),
         ("cars-right", """(progn (setf *orbit-dist* 26.0 *orbit-yaw* 1.0 *orbit-pitch* .4) (loop for (n k) in '((1 nil) (0 nil) (2 nil) (1 :fire) (1 :ambulance) (1 :police) (1 :milk) (1 :bread) (1 :lavka) (1 :army) (1 :runaway)) for i from 0 do (let ((g (make-car n k))) (object-call g "reparent" *cart*) (godot:node-3d-set-position g (v3 (if (< i 6) 14.0 -14.0) 0.0 (- (* 9.0 (mod i 6)) 22.0))) (godot:node-3d-set-rotation g (v3 0 1.5708 0)))) 1)"""),
         ("cars-left", '(setf *orbit-yaw* -2.1 *orbit-pitch* .4)'),
         ("landmarks", """(progn (setf *orbit-dist* 120.0 *orbit-yaw* .8 *orbit-pitch* .5) (loop for f in '(mark-windmill mark-plane mark-church mark-pumpjacks mark-mast mark-elevator mark-ferris-wheel mark-watchtower mark-old-bus mark-radar mark-farm mark-rocket) for i from 0 do (funcall f (group *cart* (* 75.0 (cos (* i .5236))) 0.0 (* 75.0 (sin (* i .5236)))))) 1)"""),
         ("landmarks-back", '(setf *orbit-yaw* -2.3 *orbit-pitch* .5)'),
         ("buildings", """(progn (setf *orbit-dist* 70.0 *orbit-yaw* 1.57 *orbit-pitch* .45) (loop for f in '(culture-house plaza tavern-station swamp-station plain-station depot biplane) for i from 0 do (funcall f (group *cart* -40.0 (if (eq f 'biplane) 12.0 0.0) (- (* 34.0 i) 100.0)))) 1)"""),
         ("buildings-far", '(setf *orbit-dist* 110.0 *orbit-yaw* 2.3 *orbit-pitch* .4)'),
         ("meta-room", '(progn (setf *meta* t *view* :inside *seat* nil *pos* (v3 0 0 2.8) *yaw* 0.0 *pitch* 0.0) (apply-meta) 1)'),
         ("meta-room-back", '(setf *pos* (v3 0 0 -2.8) *yaw* 3.1416)'),
         ("meta-room-side", '(setf *pos* (v3 2.4 0 0) *yaw* 1.5708)')]


def shoot(folder, tag):
    site = folder.startswith("http")
    server = None if site else subprocess.Popen([sys.executable, "-m", "http.server", "8765", "--bind", "127.0.0.1"], cwd=folder, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    try:
        time.sleep(1)
        if server and server.poll() is not None: sys.exit("was-now: port 8765 is taken - somebody else's server would be filmed instead of " + folder)
        steps = ["go", "wait:50", "click:350,174", "wait:4"] + ["click:480,200", "wait:1"] * 4 + ["lisp:(when *debug-on* (toggle-debug))", "wait:1"]
        for name, form in VIEWS: steps += ["lisp:" + form, "wait:2", f"shot:{out}/.{tag}-{name}.png"]
        r = subprocess.run([os.path.join(here, "web-eyes.py"), (folder if site else "http://127.0.0.1:8765/index.html") + "?dev", tempfile.mkdtemp()] + steps, capture_output=True, text=True, timeout=400)
        bad = [l for l in r.stdout.splitlines() if "Ошибка" in l or "Error" in l or "prompt" in l or "нет ответа" in l]
        if bad: print(tag, "\n".join(bad[:8]))
    finally:
        if server: server.terminate(); server.wait()


shoot(was, "was"); shoot(now, "now")
for n, (name, _) in enumerate(VIEWS):
    a, b = (Image.open(f"{out}/.{t}-{name}.png").convert("RGB") for t in ("was", "now"))
    both = Image.new("RGB", (a.width + b.width + 6, a.height), "white"); both.paste(a, (0, 0)); both.paste(b, (a.width + 6, 0))
    both.save(f"{out}/{n:02d}-{name}.png"); print(f"{out}/{n:02d}-{name}.png")
