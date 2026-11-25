"""The seventeen other bus stops, each a small sculpture as before, now solid: walls with thickness, roofs that are
carried by something, and the details one looks at while waiting. Run: blender -b --factory-startup --python
make_stops.py -- OUTDIR. The road is at -x, the back wall at +x; no roof comes closer to the road than x -1.1."""
import sys, os, math
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from kit import Model, arch, star

out = sys.argv[sys.argv.index("--") + 1]
CONC, DARKC, LIGHT, ROOF, GOLD, WOOD, STEEL, WHITE, GLASS = "#85857f", "#6e6e6a", "#9a9a92", "#7f8a8c", "#c9a13a", "#6a5038", "#4c4e52", "#d8d4c4", "#2e333b"
P = ["paint%d" % i for i in range(8)]


def back_wall(m, mat=CONC, y=1.9, z=2.5, plinth=True, solid=True):
    if plinth: m.box(DARKC, (0.86, -y - 0.06, 0.06), (1.12, y + 0.06, 0.5), bevel=0.015)
    m.box(mat, (0.90, -y, 0.5 if plinth else 0.06), (1.08, y, z), bevel=0.012)
    if solid: m.solid((0.90, -y, 0.06), (1.08, y, min(z, 2.4)))


def post(m, x, y, z1, r=0.05, mat=STEEL, z0=0.06):
    m.tube(mat, (x, y, z0), (x, y, z1), r, n=10)
    m.tube(DARKC, (x, y, z0), (x, y, z0 + 0.1), r * 2.2, r * 1.6, n=10)


def stop_0(m):   # the box: a concrete niche with a field of five tile colours, a flat roof with a deep fascia
    back_wall(m, z=2.55)
    for y0 in (-2.04, 1.90): m.box(CONC, (-0.55, y0, 0.06), (1.08, y0 + 0.14, 2.55), bevel=0.012)
    m.box(ROOF, (-1.1, -2.2, 2.55), (1.25, 2.2, 2.70), bevel=0.01)
    m.box(DARKC, (-1.14, -2.24, 2.48), (-1.08, 2.24, 2.74))
    k = 0
    for row in range(4):
        for col in range(9):
            if (row * 3 + col * 5) % 4 == 0: continue
            y0, z0 = -1.62 + col * 0.36, 0.72 + row * 0.42
            m.box(P[k % 5], (0.878, y0, z0), (0.90, y0 + 0.33, z0 + 0.39)); k += 1
    m.solid((-0.55, -2.04, 0.06), (1.08, -1.90, 2.4)); m.solid((-0.55, 1.90, 0.06), (1.08, 2.04, 2.4))


def stop_2(m):   # the fold: a concrete roof pleated like paper over a wall that steps in and out with it
    n = 6; w = 4.2 / n
    for i in range(n):
        y0 = -2.1 + i * w
        m.prism(CONC, [(y0, 2.42), (y0 + w / 2, 2.74), (y0 + w, 2.42), (y0 + w, 2.30), (y0 + w / 2, 2.62), (y0, 2.30)], -1.1, 1.25, axis="x")
        deep = 0.16 if i % 2 == 0 else 0.0
        m.box(LIGHT if i % 2 == 0 else CONC, (0.92 - deep, y0, 0.06), (1.06 - deep, y0 + w, 2.36))
        if i % 2 == 0: m.box(P[0], (0.895 - deep, y0 + 0.12, 0.7), (0.92 - deep, y0 + w - 0.12, 2.0))
    m.box(P[2], (-1.14, -2.12, 2.24), (-1.08, 2.12, 2.36))
    for y in (-2.0, 2.0): post(m, -0.9, y, 2.32, r=0.045)
    m.box(DARKC, (0.72, -2.14, 0.06), (1.1, 2.14, 0.34), bevel=0.015)
    m.solid((0.76, -2.1, 0.06), (1.06, 2.1, 2.36))

def stop_3(m):   # the pergola: a steel frame, slats of three colours overhead and a lattice at one end
    for y in (-1.9, 1.9):
        post(m, 0.95, y, 2.55); post(m, -0.95, y, 2.45)
        m.box(STEEL, (-1.05, y - 0.04, 2.42), (1.05, y + 0.04, 2.52))
    m.box(STEEL, (0.90, -1.95, 2.50), (0.98, 1.95, 2.60)); m.box(STEEL, (-0.99, -1.95, 2.40), (-0.91, 1.95, 2.50))
    for i in range(13):
        y0 = -2.05 + i * 0.32
        m.box(P[(0, 6, 5)[i % 3]], (-1.1, y0, 2.56), (1.15, y0 + 0.2, 2.62), bevel=0.008)
    for i in range(6):                                            # the lattice at the far end
        m.box(WOOD, (-0.85 + i * 0.32, 1.86, 0.5), (-0.79 + i * 0.32, 1.92, 2.4))
    for z0 in (0.7, 1.45, 2.2): m.box(WOOD, (-0.9, 1.84, z0), (0.9, 1.88, z0 + 0.06))
    m.box(CONC, (0.9, -1.9, 0.06), (1.0, 1.9, 1.1), bevel=0.01); m.solid((0.9, -1.9, 0.06), (1.0, 1.9, 1.1))


def stop_4(m):   # the mushroom: a thick pale stem behind the bench and a wide cap with spots over it
    m.lathe(WHITE, (1.0, 0.0), [(0.0, 0.06), (0.46, 0.06), (0.38, 0.5), (0.29, 1.5), (0.33, 2.15), (0.5, 2.36), (0.0, 2.36)], n=18)
    m.lathe(WHITE, (0.35, 0.0), [(0.0, 2.3), (1.62, 2.24), (1.66, 2.3), (0.0, 2.38)], n=28)
    m.lathe(P[2], (0.35, 0.0), [(1.66, 2.28), (1.74, 2.42), (1.6, 2.82), (1.2, 3.2), (0.6, 3.44), (0.0, 3.5)], n=28)
    for a, r, sp in ((0.4, 1.05, .2), (1.45, 1.3, .16), (2.5, 0.95, .21), (3.5, 1.32, .15), (4.5, 1.0, .19), (5.5, 1.3, .16), (1.0, 0.45, .15), (3.1, 0.42, .14), (5.0, 0.4, .13)):
        zz = 3.5 - 0.62 * (r / 1.3) ** 2
        m.lathe(WHITE, (0.35 + r * math.cos(a), r * math.sin(a)), [(sp * 1.05, zz - 0.07), (sp, zz + 0.02), (0.0, zz + 0.05)], n=10)
    m.solid((0.6, -0.4, 0.06), (1.4, 0.4, 2.3))

def stop_5(m):   # the tent: four posts, a hipped roof with broad eaves and a finial, low walls on two sides
    for x, y in ((0.95, -1.85), (0.95, 1.85), (-0.85, -1.85), (-0.85, 1.85)): post(m, x, y, 2.4, r=0.06, mat=WHITE)
    m.box(WHITE, (-1.0, -2.0, 2.36), (1.1, 2.0, 2.5), bevel=0.01)
    m.pyramid(P[0], (-1.1, -2.2), (1.3, 2.2), 2.5, (0.1, 0.0, 3.75))
    m.box(P[2], (-1.12, -2.22, 2.44), (1.32, 2.22, 2.52))
    m.tube(GOLD, (0.1, 0, 3.7), (0.1, 0, 4.05), 0.03, 0.01, n=8); m.lathe(GOLD, (0.1, 0), [(0, 3.78), (0.09, 3.84), (0.09, 3.9), (0, 3.96)], n=10)
    m.box(WHITE, (0.9, -1.85, 0.06), (1.0, 1.85, 1.0), bevel=0.01)
    for i in range(9): m.box(P[0], (0.885, -1.7 + i * 0.4, 0.2), (0.9, -1.5 + i * 0.4, 0.86))
    m.solid((0.9, -1.85, 0.06), (1.0, 1.85, 1.0))


def stop_6(m):   # the gull: one pylon in the middle, two wings of roof rising from it, a low wall behind the bench
    m.prism(CONC, [(1.12, 0.06), (0.62, 0.06), (0.70, 2.36), (1.06, 2.36)], -0.24, 0.24)
    m.prism(P[0], [(0.0, 2.30), (2.3, 3.0), (2.3, 3.11), (0.0, 2.44)], -1.1, 1.3, axis="x")
    m.prism(P[0], [(0.0, 2.30), (0.0, 2.44), (-2.3, 3.11), (-2.3, 3.0)], -1.1, 1.3, axis="x")
    m.box(P[2], (-1.14, -0.02, 2.26), (-1.08, 0.02, 2.48))
    m.lathe(GOLD, (0.88, 0), [(0, 2.44), (0.2, 2.5), (0.27, 2.7), (0.2, 2.9), (0, 2.96)], n=14)
    m.box(DARKC, (0.56, -0.32, 0.06), (1.18, 0.32, 0.32), bevel=0.02)
    for s0, s1 in ((-1.9, -0.3), (0.3, 1.9)): m.box(LIGHT, (0.92, s0, 0.06), (1.02, s1, 1.2), bevel=0.01)
    m.solid((0.62, -0.24, 0.06), (1.12, 0.24, 2.3)); m.solid((0.92, -1.9, 0.06), (1.02, 1.9, 1.2))

def stop_7(m):   # the rainbow: three arches of colour one behind another, a wall between their feet
    for k, (r, x0, col) in enumerate(((1.95, 0.72, P[2]), (1.62, 0.56, P[6]), (1.29, 0.40, P[0]))):
        m.prism(col, arch(0.0, 0.06, r, r - 0.26, n=18), x0, x0 + 0.34, axis="x")
    m.box(CONC, (0.98, -1.9, 0.06), (1.12, 1.9, 1.9), bevel=0.01)
    m.prism(CONC, arch(0.0, 0.06, 1.9, 0.0, n=18)[:19] + [(-1.9, 0.06)], 0.98, 1.12, axis="x")
    m.box(ROOF, (-1.1, -2.1, 2.1), (0.5, 2.1, 2.2), bevel=0.01)
    for y in (-1.95, 1.95): post(m, -0.95, y, 2.1)
    m.solid((0.98, -1.9, 0.06), (1.12, 1.9, 2.0))


def stop_8(m):   # the sail: a tall triangular wall and a roof hung from its top by two rods
    m.prism(P[6], [(-1.9, 0.06), (1.9, 0.06), (1.9, 0.9), (-1.2, 3.6), (-1.9, 3.6)], 0.92, 1.08, axis="x")
    m.prism(CONC, [(-1.96, 0.06), (1.96, 0.06), (1.96, 0.5), (-1.96, 0.5)], 0.88, 1.12, axis="x")
    m.box(ROOF, (-1.1, -2.1, 2.35), (1.0, 1.6, 2.47), bevel=0.01)
    m.box(P[0], (-1.14, -2.12, 2.3), (-1.08, 1.62, 2.5))
    for y in (-1.7, 0.6): m.tube(STEEL, (0.95, -1.6, 3.5), (-0.9, y, 2.47), 0.018, n=6)
    post(m, -0.9, 1.5, 2.35); post(m, -0.9, -2.0, 2.35)
    m.solid((0.92, -1.9, 0.06), (1.08, 1.9, 2.4))


def stop_9(m):   # the shell: half of a concrete dome open to the road, pale inside, with a painted rim
    outer = [(2.0 * math.cos(a), 0.06 + 2.75 * math.sin(a)) for a in [math.pi / 2 * i / 8 for i in range(9)]]
    inner = [(1.88 * math.cos(a), 0.06 + 2.63 * math.sin(a)) for a in [math.pi / 2 * i / 8 for i in range(9)]]
    m.lathe(CONC, (0.0, 0.0), outer, n=20, a0=-math.pi / 2, a1=math.pi / 2)
    m.lathe(WHITE, (0.0, 0.0), inner, n=20, a0=-math.pi / 2, a1=math.pi / 2, inside=True)
    rim = [(2.02 * math.cos(a), 0.06 + 2.78 * math.sin(a)) for a in [math.pi * i / 20 for i in range(21)]]
    rim += [(1.86 * math.cos(a), 0.06 + 2.61 * math.sin(a)) for a in [math.pi * (20 - i) / 20 for i in range(21)]]
    m.prism(P[0], rim, -0.05, 0.03, axis="x")
    m.solid((0.9, -1.8, 0.06), (1.1, 1.8, 2.0))

def stop_10(m):  # the saucer: a disc on three splayed legs, a dome, a spire and a ring of lamps
    for a in (math.pi, math.pi / 3, -math.pi / 3):
        m.tube(STEEL, (0.3 + 1.2 * math.cos(a), 1.2 * math.sin(a), 0.06), (0.3 + 0.75 * math.cos(a), 0.75 * math.sin(a), 2.4), 0.07, 0.05, n=10)
    m.lathe(ROOF, (0.3, 0), [(0, 2.36), (1.0, 2.36), (1.4, 2.5), (1.42, 2.56), (1.0, 2.7), (0.55, 2.74), (0, 2.74)], n=28)
    m.lathe(P[0], (0.3, 0), [(0.58, 2.72), (0.52, 2.95), (0.3, 3.1), (0, 3.14)], n=20)
    m.tube(P[2], (0.3, 0, 3.1), (0.3, 0, 3.9), 0.035, 0.006, n=8)
    for i in range(10):
        a = 2 * math.pi * i / 10
        m.lathe("#ffe9b0 emit #ffd890 1.5", (0.3 + 1.22 * math.cos(a), 1.22 * math.sin(a)), [(0, 2.38), (0.06, 2.42), (0.06, 2.46)], n=8, flags="_noshadow")
    m.box(CONC, (0.9, -1.4, 0.06), (1.0, 1.4, 1.0), bevel=0.01); m.solid((0.9, -1.4, 0.06), (1.0, 1.4, 1.0))


def stop_11(m):  # the teremok: a little timber house, a steep gable roof with a carved edge and a ridge board
    back_wall(m, mat=WHITE, plinth=False, z=2.3)
    for y0 in (-1.98, 1.84): m.box(WHITE, (-0.4, y0, 0.06), (1.08, y0 + 0.14, 2.3), bevel=0.01)
    for x in (-0.85,):
        for y in (-1.9, 1.9): post(m, x, y, 2.3, r=0.06, mat=WOOD)
    for s in (-1, 1):
        m.prism(P[0], [(0.0, 3.5), (s * 2.35, 2.2), (s * 2.35, 2.08), (0.0, 3.36)] if s > 0 else [(-2.35, 2.2), (0.0, 3.5), (0.0, 3.36), (-2.35, 2.08)], -1.1, 1.25, axis="x")
    m.prism(WHITE, [(-1.98, 2.3), (1.98, 2.3), (0.0, 3.36)], 0.9, 1.08, axis="x")
    for i in range(9):                                           # the carved board under the front edge
        y0 = -2.0 + i * 0.46; z0 = 2.22 + (1.0 - abs(y0 + 0.16) / 2.0) * 1.1
        m.prism(WHITE, [(y0, z0), (y0 + 0.32, z0), (y0 + 0.16, z0 - 0.2)], -1.12, -1.08, axis="x")
    m.tube(GOLD, (-1.1, 0, 3.5), (1.25, 0, 3.5), 0.04, n=8)
    m.solid((0.9, -1.9, 0.06), (1.08, 1.9, 2.3)); m.solid((-0.4, -1.98, 0.06), (1.08, -1.84, 2.3)); m.solid((-0.4, 1.84, 0.06), (1.08, 1.98, 2.3))


def stop_12(m):  # the mural wall: a tall slab for the town's ornament (the game hangs it), a short canopy on brackets
    m.box(DARKC, (0.88, -1.72, 0.06), (1.14, 1.72, 0.3), bevel=0.015)
    m.box(CONC, (0.90, -1.62, 0.3), (1.10, 1.62, 3.3), bevel=0.012)
    m.box(ROOF, (-0.75, -1.9, 3.1), (1.2, 1.9, 3.2), bevel=0.01)
    for y in (-1.5, 0.0, 1.5): m.prism(CONC, [(0.9, 2.5), (0.9, 3.1), (-0.6, 3.1)], y - 0.05, y + 0.05)
    m.solid((0.90, -1.62, 0.06), (1.10, 1.62, 2.4))


def stop_13(m):  # the gate: two tapering pylons, a lintel with a star, a canopy between them
    for y0 in (-2.05, 1.55):
        m.prism(WHITE, [(y0 - 0.06, 0.06), (y0 + 0.56, 0.06), (y0 + 0.46, 3.3), (y0 + 0.04, 3.3)], 0.55, 1.1, axis="x")
        m.box(DARKC, (0.5, y0 - 0.1, 0.06), (1.15, y0 + 0.6, 0.35), bevel=0.02)
        m.box(WHITE, (0.5, y0 - 0.04, 3.3), (1.15, y0 + 0.54, 3.45), bevel=0.01)
    m.box(WHITE, (0.62, -1.6, 2.75), (1.02, 1.6, 3.12), bevel=0.01)
    m.prism(P[2], star(0.0, 2.93, 0.3), 0.56, 0.62, axis="x")
    m.box(ROOF, (-1.05, -1.55, 2.3), (1.0, 1.55, 2.42), bevel=0.01)
    m.box(CONC, (0.92, -1.5, 0.06), (1.04, 1.5, 2.3), bevel=0.01)
    m.solid((0.55, -2.1, 0.06), (1.1, -1.5, 2.4)); m.solid((0.55, 1.5, 0.06), (1.1, 2.1, 2.4)); m.solid((0.92, -1.5, 0.06), (1.04, 1.5, 2.3))


def stop_14(m):  # the porthole: a ribbed wall with a round window, a thin roof on a row of slender columns
    back_wall(m, z=2.6)
    for i in range(7): m.box(LIGHT, (0.872, -1.8, 0.7 + i * 0.26), (0.90, 1.8, 0.76 + i * 0.26))
    m.prism(P[0], arch(-0.9, 1.6, 0.52, 0.40, 0, 2 * math.pi, n=24), 0.84, 0.90, axis="x")
    m.prism(GLASS, arch(-0.9, 1.6, 0.40, 0.0, 0, 2 * math.pi, n=24)[:25], 0.862, 0.872, axis="x")
    m.box(ROOF, (-1.1, -2.2, 2.6), (1.25, 2.2, 2.7), bevel=0.01)
    m.box(DARKC, (-1.14, -2.22, 2.56), (-1.08, 2.22, 2.74))
    for y in (-1.95, -0.65, 0.65, 1.95): post(m, -0.9, y, 2.6, r=0.035)


def stop_15(m):  # the accordion: a roof folded into four gables on thin columns, a low wall behind
    n = 4; w = 4.2 / n
    for i in range(n):
        y0 = -2.1 + i * w
        m.prism(P[(0, 6)[i % 2]], [(y0, 2.35), (y0 + w / 2, 2.95), (y0 + w / 2, 2.83), (y0, 2.23)], -1.1, 1.2, axis="x")
        m.prism(P[(6, 0)[i % 2]], [(y0 + w / 2, 2.95), (y0 + w, 2.35), (y0 + w, 2.23), (y0 + w / 2, 2.83)], -1.1, 1.2, axis="x")
    for y in (-2.1, -1.05, 0.0, 1.05, 2.1):
        post(m, -0.9, y, 2.3, r=0.035, mat=WHITE); post(m, 0.95, y, 2.3, r=0.035, mat=WHITE)
    m.box(CONC, (0.9, -2.0, 0.06), (1.02, 2.0, 1.15), bevel=0.01); m.solid((0.9, -2.0, 0.06), (1.02, 2.0, 1.15))


def stop_16(m):  # the fan: a shell of seven ribs spreading from one hinge behind the bench, a flat roof in front
    for i in range(7):
        a0 = math.pi * (0.08 + 0.84 * i / 7); a1 = math.pi * (0.08 + 0.84 * (i + 1) / 7) - 0.03
        m.prism((P[0], WHITE)[i % 2], [(0.0, 0.3)] + arch(0.0, 0.3, 2.3, 2.3, a0, a1, n=4)[:5], 0.94 - (i % 2) * 0.05, 1.06 - (i % 2) * 0.05, axis="x")
    m.box(DARKC, (0.84, -0.5, 0.06), (1.14, 0.5, 0.42), bevel=0.02)
    m.box(ROOF, (-1.05, -1.9, 2.2), (0.9, 1.9, 2.3), bevel=0.01)
    for y in (-1.8, 1.8): post(m, -0.9, y, 2.2)
    m.solid((0.9, -2.2, 0.06), (1.06, 2.2, 2.4))


def stop_17(m):  # the tower: a square tower with a spire and a clock, the canopy at its foot
    m.box(WHITE, (0.5, -2.05, 0.06), (1.1, -1.45, 3.6), bevel=0.015)
    m.box(DARKC, (0.45, -2.1, 0.06), (1.15, -1.4, 0.4), bevel=0.02)
    m.box(WHITE, (0.44, -2.11, 3.55), (1.16, -1.39, 3.72), bevel=0.01)
    m.pyramid(P[0], (0.42, -2.13), (1.18, -1.37), 3.72, (0.8, -1.75, 4.9))
    m.disc(GOLD, (0.5, -1.75, 3.1), 0.2, 0.02, n=16)
    m.box(ROOF, (-1.05, -1.5, 2.4), (1.15, 2.0, 2.52), bevel=0.01)
    m.box(P[2], (-1.09, -1.52, 2.36), (-1.03, 2.02, 2.56))
    back_wall(m, y=1.75, z=2.4, solid=False)
    post(m, -0.9, 1.85, 2.4); post(m, -0.9, -1.2, 2.4)
    m.solid((0.5, -2.05, 0.06), (1.1, -1.45, 2.4)); m.solid((0.90, -1.75, 0.06), (1.08, 1.75, 2.4))


for k, build in ((0, stop_0), (2, stop_2), (3, stop_3), (4, stop_4), (5, stop_5), (6, stop_6), (7, stop_7), (8, stop_8), (9, stop_9),
                 (10, stop_10), (11, stop_11), (12, stop_12), (13, stop_13), (14, stop_14), (15, stop_15), (16, stop_16), (17, stop_17)):
    m = Model(); build(m); m.save(os.path.join(out, "stop-%d.blend" % k))
