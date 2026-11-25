"""Buildings redrawn from plain boxes:
  culture-house   a house of culture of the fifties: a portico of six columns under a pediment with a lyre and a
                  star, steps, tall windows between pilasters, a cornice, a low roof of iron; at_lit_N are windows
  depot           a warehouse of brick: pilasters, a roof of slate with two slopes, a loading ramp under a canopy,
                  three gates with their numbers, small windows up high
  mark-windmill   a tower mill of eight sides boarded with planks, a turning cap, a gallery, a door (the sails are
                  the game's, on the shaft at x -3.4, z 9)
  mark-church     a whitewashed church: the nave with its drum and blue onion dome, an apse, a bell tower with a
                  tented roof, arched windows, a fence
Run: blender -b --factory-startup --python make_marks.py -- OUTDIR [NAME ...]"""
import sys, os, math
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import kit
from kit import Model, star

out = sys.argv[sys.argv.index("--") + 1]
STONE, PLASTER, WHITE, TRIM, ROOF, GLASS, LIT, DOOR, BRICK, BRICK2, SLATE, CONCRETE, WOOD, PLANK, GOLD, BLUE, GREEN, DARK = \
    "#8a8a84", "#c8b98c", "#e4e0d4", "#ece6d2", "#6a4a40", "#26343c", "#e8c070 emit #ffc060 0.9", "#4a3020", "#8a4a3a", "#7a4034", "#7a7c78", "#85857f", "#6a5038", "#7a6a56", "#c9a13a", "#2c4a7a", "#3c5a48", "#2a2c30"


def arched(m, mat, x, y, z0, w, h, depth=0.08, flags="_noshadow_r320", face="x", n=8):
    """A window with a round head on a wall: at (X, Y) its foot Z0, W wide, H tall to the spring of the arch. FACE "x"
    is a wall that looks along x (the glass spans y), "y" one that looks along y."""
    r = w / 2; a, b = (y, x) if face == "x" else (x, y)
    prof = [(a - r, z0), (a + r, z0)] + [(a + r * math.cos(math.pi * i / n), z0 + h + r * math.sin(math.pi * i / n)) for i in range(n + 1)]
    m.prism(mat, prof, b - depth, b + depth, flags=flags, axis=("x" if face == "x" else "y"))


def culture_house(m):
    x0, x1, hw, top = 16.0, 34.0, 19.0, 10.2
    m.box(STONE, (x0 - 0.15, -hw - 0.15, -4.0), (x1 + 0.15, hw + 0.15, 0.9))
    m.box(PLASTER, (x0, -hw, 0.9), (x1, hw, top))
    m.box(TRIM, (x0 - 0.3, -hw - 0.3, top), (x1 + 0.3, hw + 0.3, top + 0.5)); m.box(TRIM, (x0 - 0.15, -hw - 0.15, top - 0.5), (x1 + 0.15, hw + 0.15, top))   # the cornice
    m.pyramid(ROOF, (x0 - 0.3, -hw - 0.3), (x1 + 0.3, hw + 0.3), top + 0.5, ((x0 + x1) / 2, 0, top + 2.6))
    # the wings: pilasters, between them a tall window over a low one
    n = 0
    for s in (-1, 1):
        for i in range(4):
            y = s * (8.6 + i * 3.1)
            m.box(TRIM, (x0 - 0.18, y - 1.75, 0.9), (x0, y - 1.35, top - 0.5), flags="_r320")
            arched(m, GLASS, x0, y, 4.6, 1.5, 3.2); m.box(TRIM, (x0 - 0.12, y - 0.95, 4.4), (x0, y + 0.95, 4.6), flags="_r320")
            m.box(GLASS, (x0 - 0.06, y - 0.75, 1.5), (x0 + 0.06, y + 0.75, 3.4), flags="_noshadow_r320")
            m.point(f"lit_{n}", (x0 - 0.1, y, 6.2)); n += 1
        m.box(TRIM, (x0 - 0.18, s * hw - (0.4 if s > 0 else 0), 0.9), (x0, s * hw + (0 if s > 0 else 0.4), top - 0.5), flags="_r320")
    for y in (-13.0, -6.5, 0.0, 6.5, 13.0):                                # the side walls: plain tall windows
        for s in (-1, 1): arched(m, GLASS, x0 + 9 + y * 0.5, s * hw, 3.0, 1.6, 4.0, face="y")
    # the portico: steps, six columns on plinths, the entablature the name is written on, the pediment
    for k in range(4): m.box(STONE, (10.2 + k * 0.45, -13.0 + k * 0.3, 0.0), (x0, 13.0 - k * 0.3, 0.225 * (k + 1)))
    for i in range(6):
        y = -10.5 + i * 4.2
        m.box(TRIM, (12.0, y - 0.75, 0.9), (13.5, y + 0.75, 1.3)); m.tube(TRIM, (12.75, y, 1.3), (12.75, y, 7.9), 0.58, 0.48, n=12)
        m.tube(TRIM, (12.75, y, 1.3), (12.75, y, 1.5), 0.68, n=12); m.box(TRIM, (12.05, y - 0.7, 7.9), (13.45, y + 0.7, 8.25))
        m.solid((12.2, y - 0.55, 0), (13.3, y + 0.55, 8.0))
    m.box(TRIM, (11.9, -11.6, 8.25), (x0, 11.6, 9.9)); m.box(PLASTER, (11.86, -10.8, 8.75), (11.9, 10.8, 9.75), flags="_noshadow")
    m.box(TRIM, (11.6, -11.9, 9.9), (x0, 11.9, 10.2))
    m.prism(TRIM, [(-11.9, 10.2), (11.9, 10.2), (0, 13.4)], 11.6, x0 + 1.0, axis="x")
    m.prism(PLASTER, [(-9.6, 10.45), (9.6, 10.45), (0, 13.0)], 11.55, 11.6, axis="x", flags="_noshadow")
    m.prism(GOLD, star(0, 11.45, 0.75), 11.5, 11.55, axis="x", flags="_noshadow_r320")
    for s in (-1, 1): m.prism(GOLD, [(s * 1.3, 10.6), (s * 4.5, 10.6), (s * 4.2, 10.8), (s * 2.0, 11.3), (s * 1.3, 11.0)], 11.5, 11.55, axis="x", flags="_noshadow_r320")   # the laurels
    for y in (-4.2, 0.0, 4.2):                                             # three doors under fanlights
        m.box(DOOR, (x0 - 0.1, y - 1.0, 0.9), (x0 + 0.05, y + 1.0, 4.2), flags="_noshadow"); arched(m, GLASS, x0 - 0.05, y, 4.2, 2.0, 0.0, depth=0.06)
        m.box(GOLD, (x0 - 0.13, y - 0.04, 2.2), (x0 - 0.1, y + 0.04, 2.9), flags="_noshadow_r110")
    for y in (-12.3, 12.3):                                                # a lamp on a post either side of the steps
        m.tube(DARK, (10.8, y, 0.0), (10.8, y, 3.4), 0.07, n=6, flags="_r320"); m.blob("#f0e8c0 emit #ffd890 1.5", (10.8, y, 3.6), (0.28, 0.28, 0.28), seed=1, rough=0.0, flags="_noshadow")
    m.solid((x0, -hw, -4.0), (x1, hw, top)); m.solid((11.0, -13.0, 0), (x0, 13.0, 0.6))


def depot(m):
    x0, x1, hw, eave, ridge = 9.0, 25.0, 23.0, 6.2, 8.6
    m.box(CONCRETE, (x0 - 0.1, -hw - 0.1, -4.0), (x1 + 0.1, hw + 0.1, 1.1))
    m.box(BRICK, (x0, -hw, 1.1), (x1, hw, eave))
    for y in (-hw, hw - 0.3): m.prism(BRICK, [(x0, eave), (x1, eave), ((x0 + x1) / 2, ridge)], y, y + 0.3)                 # the gables
    xm = (x0 + x1) / 2
    for s in (-1, 1): m.prism(SLATE, [(xm, ridge + 0.15), (xm + s * 8.7, eave - 0.05), (xm + s * 8.7, eave + 0.1), (xm, ridge + 0.3)], -hw - 0.5, hw + 0.5)
    for i in range(12):                                                    # pilasters, and a band of lighter brick under the eave
        y = -hw + i * (2 * hw - 0.5) / 11; m.box(BRICK2, (x0 - 0.15, y, 1.1), (x0, y + 0.5, eave), flags="_r320")
    m.box(BRICK2, (x0 - 0.15, -hw, eave - 0.45), (x0, hw, eave), flags="_r320")
    for i in range(11):
        y = -hw + 0.25 + (i + 0.5) * (2 * hw - 0.5) / 11
        if abs(abs(y) - 12.0) > 2.6 and abs(y) > 2.6: m.box(GLASS, (x0 - 0.04, y - 0.8, 4.7), (x0 + 0.05, y + 0.8, 5.5), flags="_noshadow_r320")
    # the ramp, the canopy over it on brackets, three gates with numbers on plates
    m.box(CONCRETE, (6.6, -20.0, 0.0), (x0, 20.0, 1.1), bevel=0.03)
    for k in range(4): m.box(CONCRETE, (6.7, 20.0 + k * 0.3, 0.0), (8.4, 20.3 + k * 0.3, 1.1 - (k + 1) * 0.22))
    m.prism(SLATE, [(x0, 5.5), (6.2, 5.05), (6.2, 5.15), (x0, 5.6)], -20.5, 20.5)
    for y in range(-20, 21, 5): m.prism(DARK, [(x0, 4.6), (x0, 5.45), (6.5, 5.05)], y - 0.05, y + 0.05, flags="_r320")
    for i, y in enumerate((-12.0, 0.0, 12.0)):
        m.box(GREEN, (x0 - 0.1, y - 2.5, 1.1), (x0 + 0.05, y + 2.5, 4.5), flags="_noshadow")
        for yy in (y - 1.25, y, y + 1.25): m.box(DARK, (x0 - 0.12, yy - 0.03, 1.1), (x0 - 0.1, yy + 0.03, 4.5), flags="_r110")
        for zz in (1.6, 4.0): m.box(DARK, (x0 - 0.13, y - 2.5, zz), (x0 - 0.1, y + 2.5, zz + 0.1), flags="_r110")
        m.box("#e6e2d6", (x0 - 0.12, y - 0.3, 4.65), (x0 - 0.08, y + 0.3, 5.0), flags="_noshadow_r110")
        m.solid((x0 - 0.1, y - 2.5, 1.1), (x0, y + 2.5, 4.7))
    for j, (y, z) in enumerate(((-17.0, 1.1), (-16.3, 1.1), (-16.6, 1.6), (6.0, 1.1), (16.5, 1.1), (15.9, 1.1))): m.box(WOOD, (7.0, y, z), (7.6, y + 0.6, z + 0.5), flags="_r320")
    for y in (-6.0, -5.3, 7.2): m.tube("#55708a", (7.4, y, 1.1), (7.4, y, 2.0), 0.29, n=10, flags="_r320")
    m.solid((x0, -hw, -4.0), (x1, hw, eave)); m.solid((6.6, -20.0, 0), (x0, 20.0, 1.1))


def windmill(m):
    n = 8; turn = math.pi / 8
    m.lathe(STONE, (0, 0), [(3.5, 0.0), (3.45, 0.7)], n=n, smooth=False)
    m.lathe(PLANK, (0, 0), [(3.3, 0.7), (2.3, 9.2)], n=n, smooth=False)
    for i in range(n):                                                     # the battens over the corners
        a = i * 2 * math.pi / n; m.tube(WOOD, (3.32 * math.cos(a), 3.32 * math.sin(a), 0.7), (2.32 * math.cos(a), 2.32 * math.sin(a), 9.2), 0.07, n=4, flags="_r320")
    for z in (2.6, 4.6, 6.6): m.lathe(WOOD, (0, 0), [(3.3 - (z - 0.7) * 0.1176 + 0.03, z), (3.3 - (z + 0.12 - 0.7) * 0.1176 + 0.03, z + 0.12)], n=n, smooth=False, flags="_r320")
    # the gallery on struts, its rail
    m.lathe(WOOD, (0, 0), [(2.9, 3.4), (4.3, 3.4), (4.3, 3.5), (2.9, 3.5)], n=n, smooth=False)
    for i in range(n):
        a = i * 2 * math.pi / n; c, s = math.cos(a), math.sin(a)
        m.tube(WOOD, (4.25 * c, 4.25 * s, 3.5), (4.25 * c, 4.25 * s, 4.5), 0.05, n=4, flags="_r320"); m.tube(WOOD, (3.1 * c, 3.1 * s, 2.0), (4.2 * c, 4.2 * s, 3.4), 0.06, n=4, flags="_r320")
        b = (i + 1) * 2 * math.pi / n; m.tube(WOOD, (4.25 * c, 4.25 * s, 4.5), (4.25 * math.cos(b), 4.25 * math.sin(b), 4.5), 0.04, n=4, flags="_r320")
    # the cap: a boarded hood with a ridge, the shaft through its face, the tail pole down to the gallery
    m.lathe("#4a3a2a", (0, 0), [(2.55, 9.2), (2.6, 9.5), (1.9, 11.2), (0.6, 12.2), (0.0, 12.4)], n=10, smooth=False)
    m.tube(DARK, (-3.5, 0, 9.0), (-1.5, 0, 9.3), 0.16, n=8); m.box(WOOD, (-2.7, -0.35, 8.65), (-2.2, 0.35, 9.4))
    m.tube(WOOD, (2.4, 0, 9.6), (5.2, 0, 4.6), 0.07, n=5, flags="_r320")
    # a door with a little roof, two small windows one above the other
    m.box(DOOR, (-3.45, -0.6, 0.7), (-3.2, 0.6, 2.7), flags="_noshadow"); m.prism(WOOD, [(-4.0, 2.75), (-3.1, 3.1), (-3.1, 3.18), (-4.0, 2.83)], -0.9, 0.9, flags="_r320")
    for z in (5.4, 7.6): m.box(GLASS, (-3.3 + (z - 0.7) * 0.1176 - 0.08, -0.3, z), (-3.3 + (z - 0.7) * 0.1176 + 0.1, 0.3, z + 0.6), flags="_noshadow_r320")
    for k in range(3): m.box(STONE, (-4.1 + k * 0.3, -0.8, 0.0), (-3.5, 0.8, 0.7 - k * 0.22))
    m.solid((-3.3, -3.3, 0), (3.3, 3.3, 9.5))


def onion(m, mat, cx, cy, z, r, n=12):
    m.lathe(mat, (cx, cy), [(r * 0.8, z), (r * 1.22, z + r * 0.45), (r * 1.25, z + r * 0.8), (r * 0.95, z + r * 1.3), (r * 0.42, z + r * 1.8), (r * 0.1, z + r * 2.25), (0.0, z + r * 2.5)], n=n)
    top = z + r * 2.5
    m.blob(GOLD, (cx, cy, top + 0.12), (0.14, 0.14, 0.14), seed=1, rough=0.0, flags="_r320")
    m.box(GOLD, (cx - 0.035, cy - 0.035, top), (cx + 0.035, cy + 0.035, top + 1.5), flags="_r320")
    m.box(GOLD, (cx - 0.035, cy - 0.4, top + 0.95), (cx + 0.035, cy + 0.4, top + 1.02), flags="_r320"); m.box(GOLD, (cx - 0.035, cy - 0.22, top + 1.22), (cx + 0.035, cy + 0.22, top + 1.28), flags="_r320")
    m.prism(GOLD, [(cy - 0.25, top + 0.42), (cy + 0.25, top + 0.55), (cy + 0.25, top + 0.61), (cy - 0.25, top + 0.48)], cx - 0.035, cx + 0.035, axis="x", flags="_r320")


def church(m):
    # the nave: a cube with pilasters and a cornice, arched windows in two tiers, a low roof of four slopes
    m.box(STONE, (-3.7, -5.2, 0.0), (3.7, 5.2, 0.5)); m.box(WHITE, (-3.5, -5.0, 0.5), (3.5, 5.0, 8.0))
    m.box(WHITE, (-3.75, -5.25, 7.6), (3.75, 5.25, 8.0)); m.pyramid(GREEN, (-3.8, -5.3), (3.8, 5.3), 8.0, (0, 0, 9.6))
    for s in (-1, 1):
        for y in (-5.0, -1.9, 1.5, 4.6): m.box(WHITE, (s * 3.5 - (0 if s > 0 else 0.14), y, 0.5), (s * 3.5 + (0.14 if s > 0 else 0), y + 0.4, 7.6), flags="_r320")
        for y in (-3.3, 0.0, 3.3):
            arched(m, GLASS, s * 3.5, y, 2.6, 0.9, 1.9); arched(m, GLASS, s * 3.5, y, 5.9, 0.7, 0.6)
        arched(m, LIT, s * 3.52, 0.0, 2.75, 0.6, 1.6, depth=0.06, flags="_noshadow")
    # the drum with its narrow windows and the blue dome
    m.lathe(WHITE, (0, 0), [(1.9, 9.0), (1.9, 12.0), (2.1, 12.1), (2.1, 12.3)], n=12, smooth=False)
    for i in range(8):
        a = i * math.pi / 4 + math.pi / 8; m.box(GLASS, (1.9 * math.cos(a) - 0.2, 1.9 * math.sin(a) - 0.2, 9.8), (1.9 * math.cos(a) + 0.2, 1.9 * math.sin(a) + 0.2, 11.3), flags="_noshadow_r320")
    onion(m, BLUE, 0, 0, 12.3, 1.9)
    # the apse to the east: half a round under half a cone
    m.lathe(WHITE, (0, -5.0), [(2.6, 0.5), (2.6, 5.4)], n=10, a0=math.pi, a1=2 * math.pi)
    m.lathe(GREEN, (0, -5.0), [(2.75, 5.4), (0.0, 7.2)], n=10, a0=math.pi, a1=2 * math.pi, smooth=False)
    for a in (-2.2, -1.57, -0.94): m.box(GLASS, (2.6 * math.cos(a) - 0.18, -5.0 + 2.6 * math.sin(a) - 0.18, 2.4), (2.6 * math.cos(a) + 0.18, -5.0 + 2.6 * math.sin(a) + 0.18, 4.2), flags="_noshadow_r320")
    # the refectory and the bell tower to the west: a square foot, an eight-sided tier with open arches, the tent
    m.box(WHITE, (-2.8, 5.0, 0.5), (2.8, 6.4, 5.6)); m.prism(GREEN, [(-3.0, 5.6), (3.0, 5.6), (0, 6.9)], 5.0, 6.4)
    m.box(STONE, (-2.0, 6.2, 0.0), (2.0, 9.6, 0.5)); m.box(WHITE, (-1.8, 6.4, 0.5), (1.8, 9.4, 10.0)); m.box(WHITE, (-2.0, 6.2, 9.7), (2.0, 9.6, 10.0))
    arched(m, DARK, 0.0, 9.4, 0.5, 1.3, 2.1, face="y", flags="_noshadow"); arched(m, GLASS, 0.0, 9.4, 5.2, 0.7, 1.4, face="y")
    for s in (-1, 1): arched(m, GLASS, s * 1.8, 7.9, 5.2, 0.7, 1.4)
    cy = 7.9
    m.lathe(WHITE, (0, cy), [(1.75, 10.0), (1.75, 13.6), (1.95, 13.7), (1.95, 13.9)], n=8, smooth=False)
    for i in range(4):
        a = i * math.pi / 2; m.box(DARK, (1.75 * math.cos(a) - 0.45, cy + 1.75 * math.sin(a) - 0.45, 10.9), (1.75 * math.cos(a) + 0.45, cy + 1.75 * math.sin(a) + 0.45, 12.9), flags="_noshadow_r320")
    m.lathe(GOLD, (0, cy), [(0.0, 12.2), (0.4, 11.9), (0.5, 11.5), (0.0, 11.5)], n=8, flags="_r320")                 # the bell
    m.lathe(GREEN, (0, cy), [(2.05, 13.9), (0.35, 19.6)], n=8, smooth=False)
    onion(m, GOLD, 0, cy, 19.6, 0.5, n=8)
    for k in range(3): m.box(STONE, (-1.6, 9.6 + k * 0.3, 0.0), (1.6, 9.9 + k * 0.3, 0.5 - (k + 1) * 0.12))
    # the fence along the road: brick posts, iron between them
    for i in range(9):
        y = -12.0 + i * 3.0; m.box(WHITE, (-9.2, y - 0.2, 0.0), (-8.8, y + 0.2, 1.3), flags="_r320"); m.pyramid(GREEN, (-9.25, y - 0.25), (-8.75, y + 0.25), 1.3, (-9.0, y, 1.6), flags="_r320")
        m.solid((-9.1, y - 0.1, 0), (-8.9, y + 0.1, 1.1))
        if i < 8 and i != 4:
            for z in (0.3, 1.0): m.box(DARK, (-9.02, y + 0.2, z), (-8.98, y + 2.8, z + 0.04), flags="_r320")
            for k in range(1, 9): m.box(DARK, (-9.015, y + 0.2 + k * 0.29, 0.3), (-8.985, y + 0.23 + k * 0.29, 1.15), flags="_r110")
    m.solid((-3.5, -5.0, 0), (3.5, 5.0, 8.0)); m.solid((-2.6, -7.6, 0), (2.6, -5.0, 5.4)); m.solid((-1.8, 5.0, 0), (1.8, 9.4, 10.0))


KINDS = (("culture-house", culture_house), ("depot", depot), ("mark-windmill", windmill), ("mark-church", church))
only = sys.argv[sys.argv.index("--") + 2:]
for name, build in KINDS:
    if only and name not in only: continue
    m = Model(); build(m); m.save(os.path.join(out, name + ".blend"))
