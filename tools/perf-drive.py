#!/usr/bin/env python3
"""Drive the whole ring in the desktop game, on the real video card but with no window on the desktop, and say where
the frame rate falls: so that nobody has to feel for the jerks by hand.

    tools/perf-drive.py [OUT.tsv]        about 25 minutes; the table goes to OUT.tsv (default build/perf-drive.tsv)

The game runs on a hidden screen: a headless sway of our own is started (and stopped at the end), and the game draws
through its X server with the video card. The wagon is driven at SPEED (20 m/s; the game's own pace is about 14, so
the world comes a little faster than in play), and every STEP metres (100) the game notes a line: where we are, frames
per second, the longest frame, draw calls, and the three parts of the game's own code that took longest in one frame.
At the end: the worst places, sorted. FROM and TO (path coordinates, metres) choose a part of the ring.
Needs the desktop build (`make live`) and sway.
"""
import os, subprocess, sys, tempfile, time
ROOT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..")
os.chdir(ROOT)
src = open("tools/smoke.py").read()
g = {"__name__": "smoke", "__file__": "tools/smoke.py"}
exec(compile(src.split("if __name__")[0], "tools/smoke.py", "exec"), g)
ev = g["ev"]
out = sys.argv[1] if len(sys.argv) > 1 else "build/perf-drive.tsv"
speed = float(os.environ.get("SPEED", 20)); step = float(os.environ.get("STEP", 100))
start = float(os.environ.get("FROM", 0)); end = float(os.environ.get("TO", 25400)); port = 4288
run = os.environ.get("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}")

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
    if not new: sys.exit("perf-drive: the hidden screen did not come up (is sway installed?)")
    display = ":" + sorted(new)[0][1:]
    env = dict(os.environ, KIOSK_SLYNK=str(port), KIOSK_NOLOG="1", KIOSK_LANG="ru", KIOSK_SOLO="1", KIOSK_NAME="Замер",
               DISPLAY=display, XDG_DATA_HOME=tempfile.mkdtemp()); env.pop("WAYLAND_DISPLAY", None)
    game = subprocess.Popen(["godot", "--path", "project", "--audio-driver", "Dummy", "--display-driver", "x11", "--disable-vsync", "--resolution", "1280x720"],
                            env=env, stdout=open("/tmp/kibitka-perf.log", "w"), stderr=subprocess.STDOUT)
    for _ in range(120):
        time.sleep(.5)
        try:
            if ev(port, "(+ 1 1)").startswith("2"): break
        except Exception: pass
    else: sys.exit("perf-drive: the game did not start, see /tmp/kibitka-perf.log")
    def lisp(form): return ev(port, "(let ((*package* (find-package :kiosk-trip))) (eval (read-from-string " + '"' + form.replace("\\", "\\\\").replace('"', '\\"') + '"' + ")))")
    time.sleep(3)
    print("perf-drive:", lisp('(princ-to-string (object-call (gdext:singleton "RenderingServer") "get_video_adapter_name"))'), flush=True)
    lisp(f"(progn (setf *hud-hidden* t *day-t* 400.0 *time-scale* 0.0) (set-weather :clear 6000) (jump-dist {start}) 1)")
    time.sleep(8)
    lisp(f"""(progn (defparameter cl-user::lines (list)) (defparameter cl-user::mark {start}) (defparameter cl-user::t0 (get-internal-real-time))
      (defun cl-user::note ()
        (let* ((r (profile-report)) (now (get-internal-real-time)) (sec (/ (- now cl-user::t0) internal-time-units-per-second 1.0)))
          (setf cl-user::t0 now)
          (push (format nil "~d ~,1f ~d ~d ~{{~a~^ ~}}" (round *dist*) (/ (getf r :frames) (max sec .1)) (getf r :worst-frame-ms) (getf r :draw-calls)
                        (loop for e in (subseq (sort (remove :frame (getf r :lisp-ms) :key (function first)) (function >) :key (lambda (e) (getf (cdr e) :max))) 0 3)
                              collect (format nil "~(~a~)=~d" (first e) (round (getf (cdr e) :max)))))
                cl-user::lines)))
      (defun step-world (delta)
        (when (< *dist* {end})
          (setf *speed* {speed})
          (timed :turn (step-turn delta)) (note-side-roads) (incf *dist* (* {speed} (min delta .1))) (step-way) (place-world)
          (timed :roads (step-roads)) (timed :props (step-props)) (step-bridges)
          (when (>= *dist* (+ cl-user::mark {step})) (setf cl-user::mark *dist*) (cl-user::note))))
      (profile-report) (setf cl-user::t0 (get-internal-real-time)) 1)""".replace("\n", " "))
    limit = time.time() + (end - start) / speed * 3 + 120
    while time.time() < limit:
        time.sleep(5)
        try:
            if lisp(f"(>= *dist* {end})").strip().startswith("T"): break
        except Exception: pass
    rows = []
    while True:
        chunk = lisp('(format nil "~{~a~^|~}" (loop repeat 4 while cl-user::lines collect (pop cl-user::lines)))').strip().strip('"')
        if not chunk: break
        rows += [r.split(" ") for r in chunk.split("|")]
    rows.reverse()
finally:
    if game: game.terminate()
    sway.terminate(); os.unlink(conf.name)
os.makedirs(os.path.dirname(out) or ".", exist_ok=True)
with open(out, "w") as f:
    f.write("dist_m\tfps\tworst_ms\tdraws\tslowest_code_ms\n")
    for r in rows: f.write("\t".join(r) + "\n")
rows = [r for r in rows if len(r) >= 4][1:]                     # the first line covers the start of the drive itself
if not rows: sys.exit("perf-drive: no samples")
fps = sorted(float(r[1]) for r in rows); worst = sorted(int(r[2]) for r in rows)
print(f"perf-drive: {len(rows)} samples; fps median {fps[len(fps)//2]:.0f}, lowest {fps[0]:.0f}; "
      f"longest frame median {worst[len(worst)//2]} ms, worst {worst[-1]} ms; "
      f"samples with a frame over 100 ms: {sum(w > 100 for w in worst)}, over 50 ms: {sum(w > 50 for w in worst)}, over 33 ms: {sum(w > 33 for w in worst)}")
print("the twelve worst places (by the longest frame):")
for r in sorted(rows, key=lambda r: -int(r[2]))[:12]:
    print(f"  {int(r[0]):6d} m  {float(r[1]):5.1f} fps  {int(r[2]):5d} ms  {r[3]:>5} draws  {' '.join(r[4:])}")
