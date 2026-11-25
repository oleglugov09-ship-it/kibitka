#!/usr/bin/env python3
"""Was and now: the same views of the wagon shot in two browser builds and put side by side.

    tools/was-now.py WAS NOW OUT-DIR [WORD...]    # with words, only the views whose names contain one of them
                                          # WAS and NOW: a folder like export/web, or the address of a build on a site
                                          # (https://.../play/index.html); OUT-DIR/NN-name.png, was on the left

No window and no sound (tools/web-eyes.py); each build takes about two minutes. The views: the wagon from four
sides outside and four ways inside, and more (see VIEWS). The swaying is switched off, the clock is stopped at midday.
"""
import os, subprocess, sys, tempfile, time
from PIL import Image
was, now, out = sys.argv[1:4]; only = sys.argv[4:]; here = os.path.dirname(os.path.abspath(__file__)); os.makedirs(out, exist_ok=True)
VIEWS = [("outside-back-right", '(setf *view* :outside *orbit-yaw* .6 *orbit-pitch* .25 *sway* 0.0 *day-t* 400.0 *time-scale* 0.0 *orbit-dist* 9.0) (set-season :spring) (set-weather :clear 600)'),
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
         ("diners", """(progn (setf *orbit-dist* 40.0 *orbit-yaw* -1.57 *orbit-pitch* .45) (loop for f in '(diner-glass diner-grill diner-kiosk) for rgb in '("#a3262c" "#3f6f73" "#c9a13a") for i from 0 do (let ((g (group *cart* 6.0 0.0 (- (* 40.0 i) 40.0)))) (diner-apron g "КАФЕ" rgb) (funcall f g "КАФЕ" rgb))) 1)"""),
         ("diners-high", '(setf *orbit-dist* 60.0 *orbit-yaw* -1.0 *orbit-pitch* .8)'),
         ("road", """(progn (setf *orbit-dist* 34.0 *orbit-yaw* -1.2 *orbit-pitch* .5) (let ((d (+ *dist* 14.0))) (add-zebra d) (add-bus (+ d 10.0)) (add-stele (+ d 22.0) "ГОРОД") (build-scrap-depot (+ d 44.0)) (build-observatory (- d 50.0))) 1)"""),
         ("road-ahead", '(setf *orbit-dist* 50.0 *orbit-yaw* -2.4 *orbit-pitch* .45)'),
         ("road-behind", '(setf *orbit-dist* 60.0 *orbit-yaw* -.6 *orbit-pitch* .45)'),
         ("misc", """(progn (setf *orbit-dist* 13.0 *orbit-yaw* -1.57 *orbit-pitch* .35) (loop for k in '(:mushroom :toadstool :berry :apple :nut :herb) for i from 0 do (build-plant (group *cart* 4.5 0.0 (- (* 2.2 i) 5.0)) k)) (mark-cow (group *cart* 8.0 0.0 0.0) 0 0 1.0 "#8a5a3a") (mark-cow (group *cart* 8.0 0.0 4.0) 0 0 2.5 "#e8e4d8") (kvas (group *cart*) 8.0 -5.0) (leto-fire (group *cart* 24.0 0.0 27.0)) 1)"""),
         ("misc-far", '(setf *orbit-dist* 24.0 *orbit-yaw* -1.2 *orbit-pitch* .45)'),
         ("misc-inside", '(setf *view* :inside *seat* nil *pos* (v3 0 0 1.8) *yaw* 0.0 *pitch* -.2)'),
         ("camp", """(progn (setf *orbit-dist* 45.0 *orbit-yaw* 1.2 *orbit-pitch* .5) (let ((g (group *cart* 40.0 0.0 0.0))) (mark-camp g)) (build-airmail (+ *dist* 45.0)) 1)"""),
         ("camp-high", '(setf *orbit-dist* 70.0 *orbit-yaw* 2.2 *orbit-pitch* .7)'),
         ("train", """(progn (setf *orbit-dist* 55.0 *orbit-yaw* -1.57 *orbit-pitch* .35) (loop for n in (make-train nil) for i from 0 do (object-call n "reparent" *cart*) (object-set n "visible" t) (godot:node-3d-set-position n (v3 -14.0 0.0 (- 60.0 (* 15.0 i))))) (add-tunnel-piece (+ *dist* 30.0) t nil) (add-tunnel-piece (+ *dist* 38.3) nil nil) (add-tunnel-piece (+ *dist* 46.6) nil t) 1)"""),
         ("train-ahead", '(setf *orbit-dist* 60.0 *orbit-yaw* -2.6 *orbit-pitch* .3)'),
         ("themes", """(progn (setf *orbit-dist* 330.0 *orbit-yaw* -1.57 *orbit-pitch* .5) (let ((tw (or (town-at *dist*) (first *towns*)))) (dolist (th *themes*) (funcall (cdr th) (group *world*) tw))) 1)"""),
         ("themes-low", '(setf *orbit-dist* 200.0 *orbit-yaw* -2.4 *orbit-pitch* .25)'),
         ("themes-back", '(setf *orbit-dist* 260.0 *orbit-yaw* -.7 *orbit-pitch* .35)'),
         ("last", """(progn (setf *orbit-dist* 30.0 *orbit-yaw* 1.57 *orbit-pitch* .4 *seed* (rs-for 7)) (izba (group *cart* -12.0 0.0 -16.0) 0 0) (shop (group *cart* -14.0 0.0 22.0) "ХЛЕБ") (parked-car (group *cart*) -6.0 8.0) (loop for d in '(0.0 2.0) for i from 0 do (let ((g (group *cart* -7.0 0.0 (- (* 7.0 i) 8.0)))) (godot:node-3d-set-rotation g (v3 0 3.1416 0)) (stop-box g d))) (loop for k in '(:snag :hay :fence :cairn :campfire :mush :pumpkin) for i from 0 do (build-prop (group *cart* -5.0 0.0 (- (* 2.5 i) 20.0)) k 1)) 1)"""),
         ("last-near", '(setf *orbit-dist* 18.0 *orbit-yaw* 2.2 *orbit-pitch* .3)'),
         ("last-far", '(setf *orbit-dist* 45.0 *orbit-yaw* .9 *orbit-pitch* .5)'),
         ("cats", """(progn (setf *view* :outside *orbit-dist* 13.0 *orbit-yaw* -1.57 *orbit-pitch* .1) (loop for look below 5 do (loop for row below 2 do (let ((g (group *cart* -5.0 0.0 (+ -3.2 (* 1.5 look) (* .75 row))))) (godot:node-3d-set-rotation g (v3 0 -1.57 0)) (godot:node-3d-set-position g (v3 (- -4.6 (* 1.3 row)) 0.0 (- (* 1.5 look) 3.0))) (make-cat g (+ look (* 3 row) 1) look)))) (loop for marks in '((:tie :hat :folder) (:glasses :bun :book) (:shades :bow) (:white) (:kerchief) (:kerchief-white :cap) (:officer)) for i from 0 do (let ((g (group *cart* -7.4 0.0 (- (* 1.2 i) 3.6)))) (godot:node-3d-set-rotation g (v3 0 -1.57 0)) (let ((c (make-cat g (+ i 2)))) (person-dress (first c) (second c) marks)))) 1)"""),
         ("cats-near", '(setf *orbit-dist* 11.0 *orbit-yaw* -1.25 *orbit-pitch* .16)'),
         ("cats-side", '(setf *orbit-dist* 12.0 *orbit-yaw* -1.95 *orbit-pitch* .3)'),
         ("houses", """(progn (setf *view* :outside *orbit-dist* 70.0 *orbit-yaw* 1.2 *orbit-pitch* .25 *seed* (rs-for 5) *mosaic-town* 0) (let ((g (group *cart* -30.0 0.0 0.0))) (godot:node-3d-set-rotation g (v3 0 3.1416 0)) (panel-house g 5 12.0 62.0 0.0 -50.0) (panel-house g 9 12.0 66.0 0.0 30.0 :slogan "МИРУ МИР") (tower (group g -18.0 0.0 90.0))) 1)"""),
         ("houses-near", '(setf *orbit-dist* 34.0 *orbit-yaw* 1.45 *orbit-pitch* .12)'),
         ("houses-end", '(setf *orbit-dist* 90.0 *orbit-yaw* 2.3 *orbit-pitch* .3)'),
         ("works", """(progn (setf *view* :outside *orbit-dist* 60.0 *orbit-yaw* 1.2 *orbit-pitch* .3 *seed* (rs-for 5)) (let ((g (group *cart* -22.0 0.0 0.0))) (godot:node-3d-set-rotation g (v3 0 3.1416 0)) (factory (group g 0.0 0.0 -30.0) 64.0) (garages (group g 0.0 0.0 30.0) 30.0)) 1)"""),
         ("works-near", '(setf *orbit-dist* 30.0 *orbit-yaw* 1.5 *orbit-pitch* .15)'),
         ("ring-00-plain", """(progn (relocate (loop for d from 1500.0 below 24000.0 by 40.0 when (and (> (weight :relief :plain d) .95) (not (town-at d)) (< (weight :water :lake d) .05)) return d)) (setf *speed* 0.0 *view* :outside *orbit-dist* 38.0 *orbit-yaw* .7 *orbit-pitch* .32) (round *dist*))"""),
         ("ring-00-plain-high", '(setf *orbit-dist* 150.0 *orbit-yaw* 2.4 *orbit-pitch* .7)'),
         ("ring-01-hills", """(progn (relocate (loop for d from 1500.0 below 24000.0 by 40.0 when (and (> (weight :relief :hills d) .95) (not (town-at d))) return d)) (setf *speed* 0.0 *view* :outside *orbit-dist* 38.0 *orbit-yaw* .7 *orbit-pitch* .32) (round *dist*))"""),
         ("ring-01-hills-high", '(setf *orbit-dist* 150.0 *orbit-yaw* 2.4 *orbit-pitch* .7)'),
         ("ring-02-mountains", """(progn (relocate (loop for d from 1500.0 below 24000.0 by 40.0 when (and (> (weight :relief :mountains d) .95) (not (town-at d))) return d)) (setf *speed* 0.0 *view* :outside *orbit-dist* 38.0 *orbit-yaw* .7 *orbit-pitch* .32) (round *dist*))"""),
         ("ring-02-mountains-high", '(setf *orbit-dist* 150.0 *orbit-yaw* 2.4 *orbit-pitch* .7)'),
         ("ring-03-canyon", """(progn (relocate (loop for d from 1500.0 below 24000.0 by 40.0 when (> (weight :relief :canyon d) .95) return d)) (setf *speed* 0.0 *view* :outside *orbit-dist* 38.0 *orbit-yaw* .7 *orbit-pitch* .32) (round *dist*))"""),
         ("ring-03-canyon-high", '(setf *orbit-dist* 150.0 *orbit-yaw* 2.4 *orbit-pitch* .7)'),
         ("ring-04-serpentine", """(progn (relocate (let ((r (find :serpentine *route* :key (function fourth)))) (/ (+ (first r) (second r)) 2))) (setf *speed* 0.0 *view* :outside *orbit-dist* 38.0 *orbit-yaw* .7 *orbit-pitch* .32) (round *dist*))"""),
         ("ring-04-serpentine-high", '(setf *orbit-dist* 150.0 *orbit-yaw* 2.4 *orbit-pitch* .7)'),
         ("ring-05-lake", """(progn (relocate (loop for d from 1500.0 below 24000.0 by 40.0 when (and (> (weight :water :lake d) .95) (not (town-at d))) return d)) (setf *speed* 0.0 *view* :outside *orbit-dist* 38.0 *orbit-yaw* .7 *orbit-pitch* .32) (round *dist*))"""),
         ("ring-05-lake-high", '(setf *orbit-dist* 150.0 *orbit-yaw* 2.4 *orbit-pitch* .7)'),
         ("ring-06-town", """(progn (relocate (town-stop-d (third *towns*))) (setf *speed* 0.0 *view* :outside *orbit-dist* 38.0 *orbit-yaw* .7 *orbit-pitch* .32) (round *dist*))"""),
         ("ring-06-town-high", '(setf *orbit-dist* 150.0 *orbit-yaw* 2.4 *orbit-pitch* .7)'),
         ("ring-07-bridge", """(progn (relocate (- *chasm* 90.0)) (setf *speed* 0.0 *view* :outside *orbit-dist* 38.0 *orbit-yaw* .7 *orbit-pitch* .32) (round *dist*))"""),
         ("ring-07-bridge-high", '(setf *orbit-dist* 150.0 *orbit-yaw* 2.4 *orbit-pitch* .7)'),
         ("ring-08-tunnel", """(progn (relocate (- (first *tunnel*) 70.0)) (setf *speed* 0.0 *view* :outside *orbit-dist* 38.0 *orbit-yaw* .7 *orbit-pitch* .32) (round *dist*))"""),
         ("ring-08-tunnel-high", '(setf *orbit-dist* 150.0 *orbit-yaw* 2.4 *orbit-pitch* .7)'),
         ("ring-09-hub", """(progn (relocate (- (edge-d1 (cdr (first *exits*))) 80.0)) (setf *speed* 0.0 *view* :outside *orbit-dist* 38.0 *orbit-yaw* .7 *orbit-pitch* .32) (round *dist*))"""),
         ("ring-09-hub-high", '(setf *orbit-dist* 150.0 *orbit-yaw* 2.4 *orbit-pitch* .7)'),
         ("ring-10-station", """(progn (relocate (- (first (sort (copy-list *stations*) (function <))) 30.0)) (setf *speed* 0.0 *view* :outside *orbit-dist* 38.0 *orbit-yaw* .7 *orbit-pitch* .32) (round *dist*))"""),
         ("ring-10-station-high", '(setf *orbit-dist* 150.0 *orbit-yaw* 2.4 *orbit-pitch* .7)'),
         ("ring-11-booth", """(progn (relocate (- (first (first (sort (copy-list *gimmicks*) (function <) :key (function first)))) 20.0)) (setf *speed* 0.0 *view* :outside *orbit-dist* 38.0 *orbit-yaw* .7 *orbit-pitch* .32) (round *dist*))"""),
         ("ring-11-booth-high", '(setf *orbit-dist* 150.0 *orbit-yaw* 2.4 *orbit-pitch* .7)'),
         ("ring-12-river", """(progn (relocate (loop for d from 1500.0 below 24000.0 by 40.0 when (and (> (weight :water :river d) .95) (not (town-at d))) return d)) (setf *speed* 0.0 *view* :outside *orbit-dist* 38.0 *orbit-yaw* .7 *orbit-pitch* .32) (round *dist*))"""),
         ("ring-12-river-high", '(setf *orbit-dist* 150.0 *orbit-yaw* 2.4 *orbit-pitch* .7)'),
         ("ring-13-seam", """(progn (relocate (- (edge-d1 (ring)) 60.0)) (setf *speed* 0.0 *view* :outside *orbit-dist* 38.0 *orbit-yaw* .7 *orbit-pitch* .32) (round *dist*))"""),
         ("ring-13-seam-high", '(setf *orbit-dist* 150.0 *orbit-yaw* 2.4 *orbit-pitch* .7)'),
         ("meta-room", '(progn (setf *meta* t *view* :inside *seat* nil *pos* (v3 0 0 2.8) *yaw* 0.0 *pitch* 0.0) (apply-meta) 1)'),
         ("meta-room-back", '(setf *pos* (v3 0 0 -2.8) *yaw* 3.1416)'),
         ("meta-room-side", '(setf *pos* (v3 2.4 0 0) *yaw* 1.5708)')]


SETUP = VIEWS[0][1]                       # the first view also stops the swaying and fixes the hour and the weather
if only: VIEWS = [("setup", SETUP)] + [v for v in VIEWS if any(w in v[0] for w in only)]


def shoot(folder, tag):
    site = folder.startswith("http")
    server = None if site else subprocess.Popen([sys.executable, "-m", "http.server", "8765", "--bind", "127.0.0.1"], cwd=folder, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    try:
        time.sleep(1)
        if server and server.poll() is not None: sys.exit("was-now: port 8765 is taken - somebody else's server would be filmed instead of " + folder)
        steps = ["go", "ready:400", "wait:3", "click:350,174", "wait:4"] + ["click:480,200", "wait:1"] * 4 + ["lisp:(when *debug-on* (toggle-debug))", "wait:1"]
        for name, form in VIEWS:                 # after a jump along the ring the land and the roadside need time to be built
            steps += ["lisp:" + form, "wait:" + ("25" if name.startswith("ring") and not name.endswith("high") else "3" if name.startswith("ring") else "2"), f"shot:{out}/.{tag}-{name}.png"]
        r = subprocess.run([os.path.join(here, "web-eyes.py"), (folder if site else "http://127.0.0.1:8765/index.html") + "?dev", tempfile.mkdtemp()] + steps, capture_output=True, text=True, timeout=1500)
        bad = [l for l in r.stdout.splitlines() if "Ошибка" in l or "Error" in l or "prompt" in l or "нет ответа" in l]
        if bad: print(tag, "\n".join(bad[:8]))
        for l in r.stdout.splitlines():
            if (l.startswith("LISP =>") and only) or "ГОТОВО" in l or "НЕ ДОЖДАЛИСЬ" in l: print(tag, l[:90])
    finally:
        if server: server.terminate(); server.wait()


shoot(was, "was"); shoot(now, "now")
for n, (name, _) in enumerate(VIEWS):
    a, b = (Image.open(f"{out}/.{t}-{name}.png").convert("RGB") for t in ("was", "now"))
    both = Image.new("RGB", (a.width + b.width + 6, a.height), "white"); both.paste(a, (0, 0)); both.paste(b, (a.width + 6, 0))
    both.save(f"{out}/{n:02d}-{name}.png"); print(f"{out}/{n:02d}-{name}.png")
