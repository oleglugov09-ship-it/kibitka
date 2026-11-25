"""What every bus stop stands on: the paved pad with a kerb, the bench, the sign on its pole, the bin."""
import sys, os, math
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from kit import Model

out = sys.argv[sys.argv.index("--") + 1]
m = Model()
SLAB, STONE, WOOD, YELLOW, DARK = "#5a5a58", "#85857f", "#6a5038", "#c9a13a", "#2a2c30"

# the pad: paving up to the kerb, the kerb in separate stones along the road
m.box(SLAB, (-1.28, -2.44, 0.0), (1.0, 2.1, 0.06))
y = -2.44
while y < 2.09:
    m.box(STONE, (-1.42, y, 0.0), (-1.28, min(y + 0.88, 2.1) - 0.02, 0.13), bevel=0.012)
    y += 0.9
# a strip of lighter slabs along the back, where the wall stands
m.box(STONE, (0.78, -2.44, 0.0), (1.0, 2.1, 0.075))

# the bench: two cast concrete sides, a seat of four boards and a back of two
side = [(0.31, 0.03), (0.69, 0.03), (0.71, 0.40), (0.78, 0.92), (0.70, 0.92), (0.62, 0.46), (0.33, 0.46), (0.29, 0.40)]
for y0 in (-1.06, 0.94):
    m.prism(STONE, side, y0, y0 + 0.12, flags="_r110")
for k in range(4):
    x0 = 0.31 + k * 0.095
    m.box(WOOD, (x0, -1.22, 0.46), (x0 + 0.08, 1.22, 0.495), bevel=0.006, flags="_r320")
for z0, x0 in ((0.60, 0.665), (0.76, 0.70)):
    m.box(WOOD, (x0, -1.22, z0), (x0 + 0.032, 1.22, z0 + 0.11), bevel=0.006, flags="_r320")

# the sign: a pole in a concrete foot, the yellow plate in a dark frame, a timetable under it
m.box(STONE, (-1.30, -2.50, 0.0), (-1.10, -2.30, 0.16), bevel=0.02)
m.tube(SLAB, (-1.20, -2.40, 0.14), (-1.20, -2.40, 2.72), 0.028, n=10)
m.box(DARK, (-1.228, -2.70, 2.13), (-1.196, -2.10, 2.67), flags="_noshadow_r110")
m.box(YELLOW, (-1.238, -2.675, 2.155), (-1.186, -2.125, 2.645), flags="_noshadow_r110")
m.box(DARK, (-1.224, -2.58, 1.42), (-1.200, -2.22, 1.92), flags="_noshadow_r110")
m.box(STONE, (-1.232, -2.555, 1.445), (-1.192, -2.245, 1.895), flags="_r110")

# the bin: a cast urn narrowing downwards, on a foot
m.tube(DARK, (0.55, 1.78, 0.06), (0.55, 1.78, 0.14), 0.10, 0.10, n=12)
m.tube(DARK, (0.55, 1.78, 0.14), (0.55, 1.78, 0.62), 0.13, 0.19, n=12)
m.tube(STONE, (0.55, 1.78, 0.60), (0.55, 1.78, 0.66), 0.205, 0.205, n=12)

m.solid((0.31, 0.94, 0.03), (0.71, 1.06, 0.53))
m.solid((0.31, -1.06, 0.03), (0.71, -0.94, 0.53))
m.solid((-1.24, -2.44, 0.0), (-1.16, -2.36, 2.6))
m.solid((0.36, 1.59, 0.03), (0.74, 1.97, 0.66))
m.save(os.path.join(out, "stop-base.blend"))
