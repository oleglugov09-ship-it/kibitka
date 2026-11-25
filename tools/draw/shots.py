#!/usr/bin/env python3
"""Look at the bus stops and the houses in the game itself: the desktop game on a hidden screen (as tools/perf-drive.py
does), the wagon put by each kind of bus stop and in the quarters of a few towns, a screenshot of each.

    tools/draw/shots.py OUT-DIR [stops|towns|wood|izby]

Needs the desktop build (`make live`) and sway. No window appears on the desktop."""
import os, subprocess, sys, tempfile, time, shutil
ROOT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..")
os.chdir(ROOT)
g = {"__name__": "smoke", "__file__": "tools/smoke.py"}
exec(compile(open("tools/smoke.py").read().split("if __name__")[0], "tools/smoke.py", "exec"), g)
ev = g["ev"]
out = os.path.abspath(sys.argv[1]); what = sys.argv[2:] or ["stops", "towns"]; os.makedirs(out, exist_ok=True); port = 4298

def displays(): return set(os.listdir("/tmp/.X11-unix")) if os.path.isdir("/tmp/.X11-unix") else set()

before = displays()
conf = tempfile.NamedTemporaryFile("w", suffix=".conf", delete=False); conf.write("output HEADLESS-1 mode 1280x720@60Hz\n"); conf.close()
env = dict(os.environ, WLR_BACKENDS="headless", WLR_LIBINPUT_NO_DEVICES="1"); env.pop("WAYLAND_DISPLAY", None); env.pop("DISPLAY", None)
sway = subprocess.Popen(["sway", "-c", conf.name], env=env, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
game = None
try:
    new = set()
    for _ in range(40):
        time.sleep(.25); new = displays() - before
        if new: break
    if not new: sys.exit("shots: the hidden screen did not come up (is sway installed?)")
    env = dict(os.environ, KIOSK_SLYNK=str(port), KIOSK_NOLOG="1", KIOSK_LANG="ru", KIOSK_SOLO="1", KIOSK_NAME="Снимки",
               DISPLAY=":" + sorted(new)[0][1:], XDG_DATA_HOME=tempfile.mkdtemp()); env.pop("WAYLAND_DISPLAY", None)
    game = subprocess.Popen(["godot", "--path", "project", "--audio-driver", "Dummy", "--display-driver", "x11", "--resolution", "1280x720"],
                            env=env, stdout=open("/tmp/kibitka-shots.log", "w"), stderr=subprocess.STDOUT)
    for _ in range(120):
        time.sleep(.5)
        try:
            if ev(port, "(+ 1 1)").startswith("2"): break
        except Exception: pass
    else: sys.exit("shots: the game did not start, see /tmp/kibitka-shots.log")
    def lisp(form): return ev(port, "(let ((*package* (find-package :kiosk-trip))) (eval (read-from-string " + '"' + form.replace("\\", "\\\\").replace('"', '\\"') + '"' + ")))")
    time.sleep(3)
    lisp("(progn (setf *story-stage* :done) (when *show* (setf *show* nil) (show-card)) 1)")          # no opening words of the story over the picture
    lisp("(progn (setf *hud-hidden* t *day-t* 400.0 *time-scale* 0.0 *view* :outside *sway* 0.0) (set-weather :clear 6000) 1)")
    lisp("(progn (set-season :" + os.environ.get("SEASON", "summer") + ") 1)")                       # SEASON=winter to see the snow
    def shot(name, d, yaw, pitch, dist):
        lisp(f"(progn (jump-dist {d}) (halt) (setf *orbit-yaw* {yaw} *orbit-pitch* {pitch} *orbit-dist* {dist}) 1)")
        time.sleep(3.5)
        path = lisp(f'(live:screenshot "user://{name}.png")').strip('"')
        shutil.copy(path, os.path.join(out, name + ".png")); print("shots:", name, flush=True)
    if "stops" in what:
        stops = lisp("(let ((seen (list))) (dolist (d (sort (copy-list *stop-ds*) (function <))) (let ((k (mod (floor d 7) *stop-count*))) (unless (or (town-at d) (assoc k seen)) (push (cons k (round d)) seen)))) (format nil \"~{~a ~}\" (loop for (k . d) in (sort seen (function <) :key (function car)) append (list k d))))").strip('"').split()
        for k, d in zip(stops[0::2], stops[1::2]):
            shot(f"stop-{int(k):02d}", float(d) - 13.0, -0.45, 0.32, 12.0)
    if what and what[0] == "probe":                             # one place from several sides: shots.py OUT probe D
        d0 = float(what[1]) if len(what) > 1 else float(lisp("(format nil \"~d\" (round (first (remove-if (function town-at) (sort (copy-list *stop-ds*) (function <))))))").strip('"'))
        print("shots: probing the bus stop at", d0, flush=True)
        for i, (off, yaw, pitch, dist) in enumerate(((-13, -0.45, 0.32, 12.0), (-9, -1.3, 0.5, 14.0), (9, 2.6, 0.3, 12.0), (9, -2.6, 0.3, 12.0), (0, 1.57, 0.25, 16.0), (0, -1.57, 0.6, 18.0))):
            shot(f"probe-{i}", d0 + off, yaw, pitch, dist)
        print(lisp("(list *dir* (round *dist*) (length *stops*))"))
    if what and what[0] == "booth":                             # a booth by its number: shots.py OUT booth 5
        d0 = float(lisp("(format nil \"~d\" (round (first (find " + what[1] + " *gimmicks* :key (function second)))))").strip('"'))
        for i, (off, yaw, pitch, dist) in enumerate(((-24, -0.5, 0.25, 22.0), (-10, -1.2, 0.3, 30.0), (14, -2.4, 0.2, 26.0))):
            shot(f"booth-{what[1]}-{i}", d0 + off, yaw, pitch, dist)
    if "wood" in what:                                          # the woods by their kinds, and the izbas at the edge of a town
        for veg in ("pine", "birch", "autumn"):
            d = lisp("(format nil \"~a\" (loop for d from 300 to 60000 by 40 when (and (not (town-at d)) (not (bare-at d)) (eq (dominant :veg d) :" + veg + ") (eq (dominant :veg (+ d 120)) :" + veg + ")) return d))").strip('"')
            if not d.isdigit(): continue
            shot(f"wood-{veg}-road", float(d), 0.0, 0.12, 9.0)
            shot(f"wood-{veg}-side", float(d), -0.9, 0.3, 30.0)
        towns = lisp("(format nil \"~{~a ~}\" (loop for tw in *towns* append (list (round (town-start tw)) (round (town-end tw)))))").strip('"').split()
        for i, (a, b) in enumerate(list(zip(towns[0::2], towns[1::2]))[:2]):
            a, b = float(a), float(b)
            for k in range(2, 10):                                # along the village end of the town, looking back at either side of the street
                shot(f"izby-{i}-{k}", a + k * .01 * (b - a), -2.3, 0.22, 15.0); shot(f"izby-{i}-{k}-r", a + k * .01 * (b - a), 2.3, 0.22, 15.0)
    if "izby" in what:                                          # a row of izbas built on the spot, to see how they differ
        lisp("(progn (setf *prop-step* 4000.0) (jump-dist (loop for d from 300 to 60000 by 40 when (and (not (town-at d)) (not (bare-at d)) (eq (dominant :veg d) :meadow)) return d)) (halt) (let ((g (group *world* 0 0 0))) (destructuring-bind (x y z) (road-pos *dist* 0.0) (godot:node-3d-set-position g (v3 x y z))) (node-3d-set-rotation g (v3 0 (- (road-frame *dist*)) 0)) (dotimes (i 6) (izba g 12.0 (- (* i 17.0) 42.0)))) 1)")
        lisp("(setf *orbit-yaw* -1.25 *orbit-pitch* 0.3 *orbit-dist* 34.0)"); time.sleep(3)
        for name, yaw, dist in (("izby-row-a", -1.0, 24.0), ("izby-row-b", -2.1, 24.0), ("izby-row-c", -1.57, 40.0)):
            lisp(f"(setf *orbit-yaw* {yaw} *orbit-pitch* 0.3 *orbit-dist* {dist})"); time.sleep(2)
            shutil.copy(lisp(f'(live:screenshot "user://{name}.png")').strip('"'), os.path.join(out, name + ".png")); print("shots:", name, flush=True)
    if "towns" in what:
        towns = lisp("(format nil \"~{~a ~}\" (loop for tw in *towns* append (list (string-downcase (town-theme tw)) (round (town-start tw)) (round (town-end tw)))))").strip('"').split()
        done = set()
        for theme, a, b in zip(towns[0::3], towns[1::3], towns[2::3]):
            if not b.lstrip("-").isdigit(): continue
            a, b = float(a), float(b)
            style = {"admin": "stalin", "university": "stalin", "factory": "konstr", "rail": "konstr", "river": "plain"}.get(theme)
            if not style or style in done: continue
            done.add(style)
            for zone, f in (("blocks9", .25), ("old", .365), ("centre", .5), ("old2", .635)):
                shot(f"town-{style}-{theme}-{zone}", a + f * (b - a), -0.5, 0.42, 34.0)
                shot(f"town-{style}-{theme}-{zone}-r", a + f * (b - a), 0.5, 0.42, 34.0)
finally:
    if game: game.terminate()
    sway.terminate()
