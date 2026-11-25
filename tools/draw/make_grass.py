"""A tuft of grass: a dozen thin blades fanning from one root, each a single triangle. The game strews it by the
thousand (lisp/trava.lisp), so it is as light as a thing can be; its colour and its sway are the game's.
Run: blender -b --factory-startup --python make_grass.py -- OUTDIR"""
import sys, os, math, random
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from kit import Model

out = sys.argv[sys.argv.index("--") + 1]
m = Model(); bm = m.bm("#6a8a4a", "_noshadow"); rnd = random.Random(7)
for i in range(12):
    a = i * 2.4 + rnd.uniform(-0.3, 0.3); r = rnd.uniform(0.02, 0.16); h = rnd.uniform(0.3, 0.62); lean = rnd.uniform(0.05, 0.28); w = 0.035
    cx, cy = math.cos(a) * r, math.sin(a) * r; px, py = -math.sin(a + 0.9) * w, math.cos(a + 0.9) * w
    bm.faces.new([bm.verts.new((cx - px, cy - py, -0.05)), bm.verts.new((cx + px, cy + py, -0.05)),
                  bm.verts.new((cx + math.cos(a) * lean, cy + math.sin(a) * lean, h))])
m.save(os.path.join(out, "grass-tuft.blend"))
