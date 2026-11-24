#!/bin/bash
# Drive the whole ring in the browser build without a window, on the real video card, and say where the frame rate
# falls: so that nobody has to feel for the jerks by hand.
#
#   tools/serve-web.py &                 # the build must be served (stop it afterwards)
#   tools/perf-drive.sh [OUT.tsv]        # about 25 minutes; the table goes to OUT.tsv (default build/perf-drive.tsv)
#
# The wagon is driven at 20 m/s (the game's own pace is about 14: the world is built a little faster than in play),
# and every 100 m a line is written: where we are, frames per second, the longest frame, draw calls, and the three
# parts of the game's own code that took longest in one frame. At the end: the worst places, sorted.
# SPEED, STEP (metres) and FROM/TO (path coordinates, metres) can be set in the environment; WEB_EYES_GPU chooses
# the video backend (gl-egl by default).
set -euo pipefail
cd "$(dirname "$0")/.."
out="${1:-build/perf-drive.tsv}"; mkdir -p "$(dirname "$out")"
K=kiosk-trip::; speed="${SPEED:-20.0}"; from="${FROM:-0}"; to="${TO:-25400}"
# the game itself notes a line every STEP metres of the way; this script only waits for the end of the drive
step="${STEP:-100}"
drive="(progn (defparameter cl-user::lines (list)) (defparameter cl-user::mark $from.0) (defparameter cl-user::t0 (get-internal-real-time)) (defun cl-user::note () (let* ((r (${K}profile-report)) (now (get-internal-real-time)) (sec (/ (- now cl-user::t0) internal-time-units-per-second 1.0))) (setf cl-user::t0 now) (push (format nil \"~d ~,1f ~d ~d ~{~a~^ ~}\" (round ${K}*dist*) (/ (getf r :frames) (max sec .1)) (getf r :worst-frame-ms) (getf r :draw-calls) (loop for e in (subseq (sort (remove :frame (getf r :lisp-ms) :key (function first)) (function >) :key (lambda (e) (getf (cdr e) :max))) 0 3) collect (format nil \"~(~a~)=~d\" (first e) (round (getf (cdr e) :max))))) cl-user::lines))) (defun ${K}step-world (delta) (when (< ${K}*dist* $to.0) (setf ${K}*speed* $speed) (${K}timed :turn (${K}step-turn delta)) (${K}note-side-roads) (incf ${K}*dist* (* $speed (min delta .1))) (${K}step-way) (${K}place-world) (${K}timed :roads (${K}step-roads)) (${K}timed :props (${K}step-props)) (${K}step-bridges) (when (>= ${K}*dist* (+ cl-user::mark $step)) (setf cl-user::mark ${K}*dist*) (cl-user::note)))) 1)"
steps=(go ready click:350,174 wait:4 click:480,200 wait:2 click:480,200 wait:2 click:480,200 wait:2 click:480,200 wait:2 click:480,200 wait:2
       "lisp:(progn (setf ${K}*hud-hidden* t ${K}*day-t* 400.0 ${K}*time-scale* 0.0) (${K}set-weather :clear 6000) (${K}jump-dist $from.0) 1)" wait:20
       "lisp:$drive" "lisp:(progn (${K}profile-report) (setf cl-user::t0 (get-internal-real-time)) 1)")
steps+=("until:$(python3 -c "print(int(($to-$from)/$speed*4)+120)"):(>= ${K}*dist* $to.0)")
steps+=("lisp:(progn (setf cl-user::lines (reverse cl-user::lines)) 1)")
for ((i = 0; i < (to - from) / step / 6 + 2; i++)); do                    # an answer is cut at 600 characters: six lines at a time
  steps+=("lisp:(format nil \"PERF~{|~a~}\" (loop repeat 6 while cl-user::lines collect (pop cl-user::lines)))")
done
WEB_EYES_MAX=2000000 WEB_EYES_GPU="${WEB_EYES_GPU:-gl-egl}" tools/web-eyes.py "http://127.0.0.1:${PORT:-8765}/index.html?dev" "$(mktemp -d)" "${steps[@]}" 2>&1 \
  | grep -o 'PERF|[^"]*' | sed 's/^PERF|//' | tr '|' '\n' | grep . | tr ' ' '\t' > "$out.tmp" || true
{ printf 'dist_m\tfps\tworst_ms\tdraws\tslowest_code_ms\n'; cat "$out.tmp"; } > "$out"; rm -f "$out.tmp"
python3 - "$out" <<'PY'
import sys
rows = [l.rstrip("\n").split("\t") for l in open(sys.argv[1])][1:]
rows = [r for r in rows if len(r) >= 4]
if not rows: sys.exit("perf-drive: no samples - is the build served?")
fps = sorted(float(r[1]) for r in rows); worst = sorted(int(r[2]) for r in rows)
print(f"perf-drive: {len(rows)} samples; fps median {fps[len(fps)//2]:.0f}, lowest {fps[0]:.0f}; "
      f"longest frame median {worst[len(worst)//2]} ms, worst {worst[-1]} ms; "
      f"samples with a frame over 100 ms: {sum(w > 100 for w in worst)}, over 50 ms: {sum(w > 50 for w in worst)}")
print("the twelve worst places (by the longest frame):")
for r in sorted(rows, key=lambda r: -int(r[2]))[:12]:
    print(f"  {int(r[0]):6d} m  {float(r[1]):5.1f} fps  {int(r[2]):5d} ms  {r[3]:>5} draws  {' '.join(r[4:])}")
PY
