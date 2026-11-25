"""The lighthouse of the booth where one looks at the prices from above: a real one, twenty-two metres to the tip,
in red and white bands, with a gallery and a lantern. It stood six metres tall and less than two wide.
Run: blender -b --factory-startup --python make_lighthouse.py -- OUTDIR"""
import sys, os, math
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from kit import Model

out = sys.argv[sys.argv.index("--") + 1]
WHITE, RED, STONE, DARK, GLASS, LAMP, DOOR = "#e8e4d8", "#a3262c", "#6e6c68", "#2a2c30", "#2e333b", "#ffe9a0 emit #ffd870 3", "#4a3020"
m = Model()
H = 16.5
def r(z): return 2.4 - 0.9 * z / H                                 # the tower narrows upwards
m.lathe(STONE, (0, 0), [(0.0, 0.0), (2.9, 0.0), (2.9, 0.5), (2.5, 0.9), (0.0, 0.9)], n=8, smooth=False)        # the foot
bands = 6
for k in range(bands):
    z0, z1 = 0.9 + k * (H - 0.9) / bands, 0.9 + (k + 1) * (H - 0.9) / bands
    m.lathe(WHITE if k % 2 == 0 else RED, (0, 0), [(r(z0), z0), (r(z1), z1)], n=20)
m.box(DOOR, (-2.42, -0.5, 0.9), (-2.2, 0.5, 3.0)); m.box(WHITE, (-2.5, -0.65, 0.9), (-2.3, 0.65, 3.15))
m.box(STONE, (-3.4, -0.9, 0.4), (-2.3, 0.9, 0.9))                    # the step
for z in (5.0, 8.6, 12.2):                                          # the windows of the stair, one above another, turning
    a = math.pi + (z - 5.0) * 0.35
    cx, cy = (r(z) + 0.02) * math.cos(a), (r(z) + 0.02) * math.sin(a)
    m.box(GLASS, (cx - 0.25, cy - 0.25, z), (cx + 0.25, cy + 0.25, z + 0.9))
# the gallery: a deck on brackets, a rail of posts and two rings
m.lathe(DARK, (0, 0), [(1.5, H - 0.6), (2.5, H), (2.5, H + 0.14), (0.0, H + 0.14)], n=20)
for i in range(16):
    a = 2 * math.pi * i / 16
    m.tube(DARK, (2.4 * math.cos(a), 2.4 * math.sin(a), H + 0.14), (2.4 * math.cos(a), 2.4 * math.sin(a), H + 1.2), 0.03, n=5, caps=False)
for z in (H + 0.7, H + 1.2): m.lathe(DARK, (0, 0), [(2.4, z), (2.4, z + 0.05)], n=20)
# the lantern: a room of glass on a low wall, the lamp inside, a roof with a ball and a rod
m.lathe(WHITE, (0, 0), [(1.45, H + 0.14), (1.45, H + 1.0)], n=16)
m.lathe(GLASS, (0, 0), [(1.4, H + 1.0), (1.4, H + 3.1)], n=16)
for i in range(8):
    a = 2 * math.pi * i / 8
    m.tube(DARK, (1.42 * math.cos(a), 1.42 * math.sin(a), H + 1.0), (1.42 * math.cos(a), 1.42 * math.sin(a), H + 3.1), 0.04, n=5, caps=False)
m.lathe(LAMP, (0, 0), [(0.0, H + 1.3), (0.5, H + 1.5), (0.6, H + 2.1), (0.5, H + 2.7), (0.0, H + 2.9)], n=12, flags="_noshadow")
m.lathe(RED, (0, 0), [(1.7, H + 3.1), (1.6, H + 3.3), (0.9, H + 4.3), (0.25, H + 4.9), (0.0, H + 4.95)], n=16)
m.lathe(DARK, (0, 0), [(0.0, H + 4.9), (0.22, H + 5.05), (0.22, H + 5.25), (0.0, H + 5.4)], n=10)
m.tube(DARK, (0, 0, H + 5.3), (0, 0, H + 6.3), 0.03, 0.01, n=5)
m.solid((-2.6, -2.6, 0.0), (2.6, 2.6, 3.0))
m.save(os.path.join(out, "booth-lighthouse.blend"))
