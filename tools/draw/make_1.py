"""The bus stop with the sun: a concrete wall with a sun in relief, two side fins that lean out over the pavement
and a thin roof that rises from the wall and dips at its front edge."""
import sys, os, math
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from kit import Model

out = sys.argv[sys.argv.index("--") + 1]
m = Model()
WALL, PLINTH, ROOF, GOLD = "#85857f", "#6e6e6a", "#7f8a8c", "#c9a13a"
FIELD, TRIM = "paint0", "paint2"

# the wall on its plinth, a painted field set into it
m.box(PLINTH, (0.86, -1.96, 0.06), (1.12, 1.96, 0.52), bevel=0.015)
m.box(WALL, (0.90, -1.90, 0.52), (1.08, 1.90, 2.58), bevel=0.012)
m.box(FIELD, (0.878, -1.62, 0.66), (0.90, 1.62, 2.40))
for y0 in (-1.70, 1.62):                                     # the frame of the field
    m.box(WALL, (0.862, y0, 0.60), (0.90, y0 + 0.08, 2.46))
for z0 in (0.60, 2.40):
    m.box(WALL, (0.862, -1.70, z0), (0.90, 1.70, z0 + 0.06))

# the sun: a disc with a rim, sixteen rays, the long ones between the short
cy, cz = 0.0, 1.50
m.disc(GOLD, (0.878, cy, cz), 0.36, 0.045, n=28, flags="_noshadow_r320")
m.disc(GOLD, (0.878, cy, cz), 0.43, 0.02, n=28, flags="_noshadow_r320")
for i in range(16):
    a = 2 * math.pi * i / 16
    r0, r1 = 0.50, (0.86 if i % 2 == 0 else 0.68)
    w = 0.075 if i % 2 == 0 else 0.055
    ca, sa = math.cos(a), math.sin(a)
    prof = [(cy + r0 * ca - w * sa, cz + r0 * sa + w * ca), (cy + r0 * ca + w * sa, cz + r0 * sa - w * ca),
            (cy + r1 * ca, cz + r1 * sa)]
    m.prism(GOLD, prof, 0.878, 0.853, flags="_noshadow_r320", axis="x")

# the side fins: narrow at the foot, leaning out to carry the roof
fin = [(1.10, 0.06), (0.52, 0.06), (0.40, 0.55), (-0.92, 2.50), (-0.92, 2.66), (1.10, 2.78)]
for y0 in (-2.06, 1.92):
    m.prism(WALL, fin, y0, y0 + 0.14)
    m.prism(TRIM, [(0.36, 0.55), (0.40, 0.55), (-0.92, 2.50), (-0.96, 2.50)], y0 - 0.004, y0 + 0.144)   # the painted leading edge

# the roof: a thin shell from behind the wall to over the kerb, with a painted fascia
top = []
for i in range(11):
    t = i / 10
    top.append((1.24 - 2.36 * t, 2.84 + 0.20 * math.sin(math.pi * t) - 0.16 * t))
m.shell(ROOF, top, 0.10, -2.2, 2.2)
fx, fz = top[-1]
m.box(TRIM, (fx - 0.035, -2.22, fz - 0.20), (fx + 0.02, 2.22, fz + 0.03))
m.box(ROOF, (1.20, -2.22, 2.70), (1.27, 2.22, 2.86))            # the gutter board at the back

m.solid((0.90, -1.90, 0.06), (1.08, 1.90, 2.4))
m.solid((0.40, -2.06, 0.06), (1.10, -1.92, 2.4))
m.solid((0.40, 1.92, 0.06), (1.10, 2.06, 2.4))
m.save(os.path.join(out, "stop-1.blend"))
