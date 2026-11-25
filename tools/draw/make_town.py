"""Things of a town and its roadside, redrawn after what stood there in the seventies:
  stele        the sign at the entrance of a town: a concrete pylon with a gear and a star, a long wall the name
               stands on in letters (at_name), a flower bed at its foot
  shop         a village store of the standard design: brick ends, a wall of shop windows in aluminium, a concrete
               canopy, a band for the name over it (paint0), a porch with two doors, crates at the back
  diner-kiosk  a round pavilion: a ring of windows, a serving hatch to the road, a roof like a cap (paint0), three
               iron "mushrooms" to eat standing at
  kvas         the yellow barrel on its trailer, the seller's stool and box, mugs on the tray
Run: blender -b --factory-startup --python make_town.py -- OUTDIR [NAME ...]"""
import sys, os, math
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import kit
from kit import Model, star
kit.PREVIEW.update({"paint0": "#8a4038"})

out = sys.argv[sys.argv.index("--") + 1]
CONCRETE, SLAB, PLINTH, BRICK, SILICATE, GLASS, LIT, ALU, RED, GOLD, DARK, WOOD, WHITE, EARTH = \
    "#85857f", "#9a9a92", "#6e6e6a", "#8a4a3a", "#c4beb0", "#26343c", "#d8c890", "#a8acb0 metal 0.5 rough 0.4", "#8a4038", "#c9a13a", "#2a2c30", "#7a6a56", "#e6e2d6", "#4a3a2a"


def flowers(m, x0, x1, y0, y1, n, seed=0):
    import random; rnd = random.Random(seed)
    for i in range(n):
        m.blob(("#c84a5a", "#e0b030", "#e6e2d6")[i % 3], (rnd.uniform(x0, x1), rnd.uniform(y0, y1), 0.42), (0.11, 0.11, 0.1), seed=i, flags="_noshadow_r110")
    for i in range(n // 2): m.blob("#4a7a3a", (rnd.uniform(x0, x1), rnd.uniform(y0, y1), 0.33), (0.25, 0.25, 0.12), seed=i + 50, flags="_r110")


def stele(m):
    # the base all of it stands on, the bed of flowers in front
    m.box(PLINTH, (-0.9, -6.2, 0.0), (0.9, 2.2, 0.35), bevel=0.03)
    m.box(CONCRETE, (-1.6, -5.8, 0.0), (-0.9, 1.8, 0.28), bevel=0.03); m.box(EARTH, (-1.5, -5.7, 0.28), (-1.0, 1.7, 0.3))
    flowers(m, -1.45, -1.05, -5.6, 1.6, 26)
    # the wall of the name: a slab on two short legs, a red band along its foot
    for y in (-5.2, -1.3): m.box(CONCRETE, (-0.25, y, 0.35), (0.25, y + 0.5, 0.9))
    m.box(SLAB, (-0.22, -5.9, 0.9), (0.22, -0.1, 2.5), bevel=0.02)
    m.box(RED, (-0.24, -5.9, 0.9), (-0.22, -0.1, 1.08), flags="_noshadow_r320")
    m.point("name", (-0.26, -3.0, 1.75))
    # the pylon: two fins that part as they rise, tied by bars, the emblem between them at the top
    for s, lean in ((-1, -0.25), (1, 0.55)):
        y = 0.9 + s * 0.5
        m.prism(CONCRETE, [(y - 0.22, 0.35), (y + 0.22, 0.35), (y + lean + 0.13, 8.2), (y + lean - 0.13, 8.2)], -0.3, 0.3, axis="x")
    for z in (2.2, 3.6, 5.0): m.box(CONCRETE, (-0.18, 0.4, z), (0.18, 1.5 + z * 0.03, z + 0.14))
    cy, cz = 1.05, 6.6
    m.prism(GOLD, [(cy + 0.78 * math.cos(i * math.pi / 8) * (1.0 if i % 2 else 0.86), cz + 0.78 * math.sin(i * math.pi / 8) * (1.0 if i % 2 else 0.86)) for i in range(16)], -0.36, -0.3, axis="x", flags="_noshadow_r320")   # the gear
    m.prism(RED, star(cy, cz, 0.52), -0.4, -0.36, axis="x", flags="_noshadow_r320")
    m.solid((-0.25, -5.9, 0), (0.25, 1.9, 2.5))


def shop(m):
    x0, x1, hw, top = 8.0, 18.0, 11.0, 4.2
    m.box(PLINTH, (x0 - 0.1, -hw - 0.1, -4.0), (x1 + 0.1, hw + 0.1, 0.35))
    m.box(SILICATE, (x0 + 0.3, -hw, 0.35), (x1, hw, top))
    for y in (-hw, hw - 1.6): m.box(BRICK, (x0, y, 0.35), (x0 + 0.3, y + 1.6, top))                                   # the brick ends of the front
    # the wall of glass: the lit windows, mullions and a transom of aluminium, a tiled apron under them
    m.box(PLINTH, (x0 + 0.05, -hw + 1.6, 0.35), (x0 + 0.3, hw - 1.6, 0.75))
    m.box(LIT, (x0 + 0.1, -hw + 1.6, 0.75), (x0 + 0.24, hw - 1.6, 3.1), flags="_noshadow")
    n = 12
    for i in range(n + 1):
        y = -hw + 1.6 + i * (2 * hw - 3.2) / n; m.box(ALU, (x0 + 0.04, y - 0.04, 0.75), (x0 + 0.1, y + 0.04, 3.1), flags="_r320")
    for z in (0.75, 2.45, 3.1): m.box(ALU, (x0 + 0.04, -hw + 1.6, z - 0.03), (x0 + 0.1, hw - 1.6, z + 0.04), flags="_r320")
    m.box(SILICATE, (x0, -hw + 1.6, 3.1), (x0 + 0.3, hw - 1.6, top))
    # the canopy on its brackets, the band of the name over it, the parapet
    m.box(SLAB, (x0 - 2.0, -hw - 0.2, 3.2), (x0 + 0.3, hw + 0.2, 3.42), bevel=0.02)
    for y in (-8.0, -4.0, 0.0, 4.0, 8.0): m.prism(CONCRETE, [(x0, 2.7), (x0, 3.2), (x0 - 1.8, 3.2)], y - 0.1, y + 0.1)
    m.box("paint0", (x0 - 0.2, -6.5, 3.75), (x0, 6.5, 4.95), flags="_noshadow"); m.box(ALU, (x0 - 0.22, -6.6, 3.68), (x0 + 0.02, 6.6, 3.75), flags="_r320")
    for y in (-6.6, 6.5): m.box(ALU, (x0 - 0.22, y, 3.68), (x0 + 0.02, y + 0.1, 5.02), flags="_r320")
    m.box(ALU, (x0 - 0.22, -6.6, 4.95), (x0 + 0.02, 6.6, 5.02), flags="_r320")
    m.box(PLINTH, (x0 - 0.05, -hw - 0.05, top), (x1 + 0.05, hw + 0.05, top + 0.25))
    # the porch: steps, the tambour with two doors, urns either side, a board of notices
    m.box(CONCRETE, (x0 - 1.6, -2.4, 0.0), (x0 + 0.1, 2.4, 0.35), bevel=0.02); m.box(CONCRETE, (x0 - 1.95, -2.2, 0.0), (x0 - 1.6, 2.2, 0.18))
    m.box(ALU, (x0 - 0.5, -1.6, 0.35), (x0 + 0.1, 1.6, 2.6)); m.box(SLAB, (x0 - 0.6, -1.7, 2.6), (x0 + 0.1, 1.7, 2.72))
    for y in (-0.8, 0.8):
        m.box("#6a4a34", (x0 - 0.53, y - 0.5, 0.4), (x0 - 0.5, y + 0.5, 2.4), flags="_r320"); m.box(GLASS, (x0 - 0.545, y - 0.36, 1.2), (x0 - 0.53, y + 0.36, 2.2), flags="_noshadow_r110")
        m.tube(ALU, (x0 - 0.58, y - 0.3 * (1 if y < 0 else -1), 1.0), (x0 - 0.58, y - 0.3 * (1 if y < 0 else -1), 1.5), 0.015, n=4, flags="_r110")
    for y in (-2.1, 2.1):
        m.tube(DARK, (x0 - 1.2, y, 0.35), (x0 - 1.2, y, 0.8), 0.13, 0.19, n=10, flags="_r320")
    m.box(WOOD, (x0 - 0.06, 3.0, 1.0), (x0 + 0.02, 4.6, 2.2), flags="_r320"); m.box(WHITE, (x0 - 0.075, 3.1, 1.1), (x0 - 0.06, 3.7, 2.0), flags="_noshadow_r110"); m.box(WHITE, (x0 - 0.075, 3.85, 1.4), (x0 - 0.06, 4.5, 2.1), flags="_noshadow_r110")
    # the yard side: a goods door under a lamp, crates, a stack of pallets
    m.box("#5a6258", (x0 + 3.0, hw, 0.35), (x0 + 4.6, hw + 0.05, 2.6), flags="_r320")
    for i, (dx, z) in enumerate(((2.0, 0.0), (2.5, 0.0), (2.2, 0.4), (5.2, 0.0))): m.box(WOOD, (x0 + dx, hw + 0.4, z), (x0 + dx + 0.45, hw + 0.85, z + 0.4), flags="_r320")
    m.solid((x0, -hw, -4.0), (x1, hw, top)); m.solid((x0 - 0.5, -1.6, 0), (x0 + 0.1, 1.6, 2.6))


def mushroom(m, x, y, cap):
    m.tube(ALU, (x, y, 0.0), (x, y, 2.25), 0.04, n=6, flags="_r320")
    m.tube(SLAB, (x, y, 0.0), (x, y, 0.06), 0.3, n=10, flags="_r320")
    m.tube(WHITE, (x, y, 0.7), (x, y, 0.75), 0.55, n=14, flags="_r320")
    m.lathe(cap, (x, y), [(1.15, 2.05), (0.5, 2.3), (0.0, 2.38)], n=12, smooth=False, flags="_r320")
    m.lathe(cap, (x, y), [(0.0, 2.3), (1.15, 2.05)], n=12, smooth=False, inside=True, flags="_r320")
    m.solid((x - 0.05, y - 0.05, 0), (x + 0.05, y + 0.05, 0.7))


def kiosk(m):
    cx, r, top = 15.0, 3.3, 3.0
    m.tube(PLINTH, (cx, 0, -0.3), (cx, 0, 0.12), r + 0.5, n=20)
    m.lathe(WHITE, (cx, 0), [(r, 0.12), (r, 1.0)], n=20)                                                           # the apron
    m.lathe("paint0", (cx, 0), [(r + 0.02, 0.2), (r + 0.02, 0.9)], n=20, flags="_noshadow")
    m.lathe(GLASS, (cx, 0), [(r - 0.04, 1.0), (r - 0.04, 2.5)], n=20)                                              # the ring of windows
    for i in range(20):
        a = i * math.pi / 10; m.tube(ALU, (cx + r * math.cos(a), r * math.sin(a), 1.0), (cx + r * math.cos(a), r * math.sin(a), 2.5), 0.04, n=4, flags="_r320")
    m.lathe(WHITE, (cx, 0), [(r, 2.5), (r, top)], n=20)
    # the roof: a cap wider than the walls, a rim, a little spire
    m.lathe("paint0", (cx, 0), [(r + 1.3, top), (r + 1.3, top + 0.22), (r * 0.5, top + 1.2), (0.25, top + 1.45), (0.0, top + 1.5)], n=20, smooth=False)
    m.lathe(SLAB, (cx, 0), [(r - 0.1, top), (r + 1.3, top)], n=20, smooth=False, inside=True)
    m.tube(ALU, (cx, 0, top + 1.45), (cx, 0, top + 2.3), 0.04, 0.01, n=5); m.blob(GOLD, (cx, 0, top + 1.7), (0.14, 0.14, 0.14), seed=1, rough=0.0)
    # the hatch to the road: lit, a counter under it, a board for the name over it
    m.box(LIT, (cx - r - 0.06, -1.3, 1.45), (cx - r + 0.25, 1.3, 2.5), flags="_noshadow")
    m.box(ALU, (cx - r - 0.09, -1.38, 1.38), (cx - r - 0.03, 1.38, 1.45), flags="_r320"); m.box(ALU, (cx - r - 0.09, -0.03, 1.45), (cx - r - 0.03, 0.03, 2.5), flags="_r320")
    m.box(WOOD, (cx - r - 0.6, -1.5, 1.32), (cx - r + 0.1, 1.5, 1.38))
    for y in (-1.3, 1.3): m.prism(ALU, [(cx - r - 0.55, 1.32), (cx - r, 1.32), (cx - r, 0.9)], y - 0.03, y + 0.03, flags="_r320")
    m.box(DARK, (cx - r - 1.34, -2.6, top + 0.24), (cx - r - 1.28, 2.6, top + 0.95), flags="_noshadow_r320")
    m.box("#6a4a34", (cx - 0.5, r - 0.05, 0.15), (cx + 0.5, r + 0.04, 2.2), flags="_r320")                          # the back door
    # the mushrooms to eat at, a bin
    for (x, y), cap in zip(((8.0, 4.0), (7.4, -0.5), (8.4, -4.5)), ("paint0", "#c9a13a", "paint0")): mushroom(m, x, y, cap)
    m.tube(DARK, (10.2, 2.6, 0.0), (10.2, 2.6, 0.6), 0.14, 0.2, n=10, flags="_r320")
    m.solid((cx - r, -r, 0), (cx + r, r, top))


def kvas(m):
    Y = "#d8a830"
    m.tube(Y, (0, -1.3, 1.3), (0, 1.3, 1.3), 0.85, n=16)
    for y, s in ((-1.3, -1), (1.3, 1)): m.tube(Y, (0, y, 1.3), (0, y + s * 0.22, 1.3), 0.85, 0.5, n=16)
    for y in (-0.9, 0.9): m.tube(ALU, (0, y - 0.03, 1.3), (0, y + 0.03, 1.3), 0.865, n=16, flags="_r320")
    m.tube(ALU, (0, 0, 2.1), (0, 0, 2.25), 0.25, n=10, flags="_r320")                                              # the filler hatch
    m.box(DARK, (-0.55, -1.2, 0.5), (0.55, 1.2, 0.6))                                                             # the frame of the trailer
    for s in (-1, 1): m.tube("#141416", (s * 0.62, -0.1, 0.36), (s * 0.82, -0.1, 0.36), 0.36, n=12)
    for s in (-1, 1): m.box(Y, (s * 0.6 - 0.24, -0.6, 0.72), (s * 0.6 + 0.24, 0.4, 0.78), flags="_r320")          # the wings
    m.tube(DARK, (0, 1.2, 0.55), (0, 2.3, 0.4), 0.04, n=5); m.tube(DARK, (0, 2.2, 0.0), (0, 2.2, 0.42), 0.03, n=4)  # the drawbar on its leg
    # the seller's end: the tap and the washer under a lid, a tray of mugs, a stool
    m.box(ALU, (-0.45, -1.95, 0.75), (0.45, -1.5, 1.2)); m.box(Y, (-0.5, -2.0, 1.2), (0.5, -1.45, 1.26))
    m.tube(ALU, (0.0, -1.95, 1.0), (0.0, -2.1, 0.95), 0.025, n=5, flags="_r110")
    m.box(WHITE, (-0.75, -2.0, 0.95), (-0.45, -1.5, 0.98), flags="_r110")
    for i in range(3): m.tube(GLASS, (-0.6, -1.9 + i * 0.15, 0.98), (-0.6, -1.9 + i * 0.15, 1.12), 0.045, n=6, flags="_noshadow_r110")
    m.box(WOOD, (0.7, -2.1, 0.42), (1.05, -1.75, 0.46), flags="_r110")
    for dx in (0.72, 1.0):
        for dy in (-2.08, -1.8): m.box(WOOD, (dx, dy, 0.0), (dx + 0.03, dy + 0.03, 0.42), flags="_r110")
    m.solid((-0.85, -2.0, 0), (0.85, 1.5, 2.1))


KINDS = (("stele", stele), ("shop", shop), ("diner-kiosk", kiosk), ("kvas", kvas))
only = sys.argv[sys.argv.index("--") + 2:]
for name, build in KINDS:
    if only and name not in only: continue
    m = Model(); build(m); m.save(os.path.join(out, name + ".blend"))
