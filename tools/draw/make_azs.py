"""Petrol stations of the seventies. station-plain: the operator's house with a wall of glass, its flat roof thrown
out over the island as a canopy; pumps with round dials; a fire board, drums of oil, a price board on posts, a tyre
for a flower bed. station-tavern: a tea-house of logs in the wood, one old pump in the yard, a table under a roof.
Both stand on a lot whose edge to the road is x 0; the pumps are at x 9.5 as they were.
Run: blender -b --factory-startup --python make_azs.py -- OUTDIR"""
import sys, os, math
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import kit
from kit import Model
import make_izba as iz
kit.PREVIEW.update({"paint0": "#3f6f73", "paint1": "#6a5038", "paint2": "#4e463c"})

out = sys.argv[sys.argv.index("--") + 1]
ASPHALT, CONCRETE, SLAB, PLASTER, BRICK, GLASS, WHITE, RED, CREAM, DARK, STEEL, BLUE, PLANK = \
    "#55554f", "#9a9a92", "#b4b2a8", "#d8d0b8", "#8a4a3a", "#26343c", "#e6e2d6", "#a3262c", "#d8cfa8", "#2a2c30", "#7f8a8c", "#3f6f73", "#7a6a56"


def pump(m, x, y, body=RED):
    """A pump: a tall box with rounded shoulders, a round dial on either face, the hose hung on its side."""
    m.box(CONCRETE, (x - 0.4, y - 0.5, 0.15), (x + 0.4, y + 0.5, 0.25))
    m.box(body, (x - 0.3, y - 0.4, 0.25), (x + 0.3, y + 0.4, 1.25), bevel=0.04)
    m.box(CREAM, (x - 0.3, y - 0.4, 1.25), (x + 0.3, y + 0.4, 1.75), bevel=0.08)
    for s in (-1, 1):
        m.tube(WHITE, (x + s * 0.3, y, 1.45), (x + s * 0.325, y, 1.45), 0.24, n=16, flags="_noshadow")
        m.tube(DARK, (x + s * 0.3, y, 1.45), (x + s * 0.31, y, 1.45), 0.27, n=16)
        m.box(DARK, (x + s * 0.325 - 0.005, y - 0.012, 1.45), (x + s * 0.325 + 0.005, y + 0.012, 1.65))            # the hand of the dial
        m.box(WHITE, (x + s * 0.3 - 0.01, y - 0.2, 0.75), (x + s * 0.3 + 0.01, y + 0.2, 0.95))                      # the plate with the grade
    pts = [(x, y + 0.4, 1.1), (x, y + 0.62, 0.8), (x, y + 0.66, 0.45), (x, y + 0.5, 0.75), (x, y + 0.42, 1.35)]       # the hose
    for a, b in zip(pts, pts[1:]): m.tube(DARK, a, b, 0.025, n=5, caps=False)
    m.box(STEEL, (x - 0.03, y + 0.4, 1.3), (x + 0.03, y + 0.52, 1.42))
    m.solid((x - 0.3, y - 0.4, 0), (x + 0.3, y + 0.4, 1.75))


def lamp(m, x, y, h=7.0):
    m.tube(CONCRETE, (x, y, 0), (x, y, h), 0.13, 0.08, n=6)
    m.tube(STEEL, (x, y, h), (x - 1.6, y, h + 0.5), 0.04, n=5)
    m.box(STEEL, (x - 2.2, y - 0.14, h + 0.42), (x - 1.5, y + 0.14, h + 0.56)); m.box("#f0e8c0 emit", (x - 2.15, y - 0.1, h + 0.4), (x - 1.6, y + 0.1, h + 0.42), flags="_noshadow")
    m.solid((x - 0.13, y - 0.13, 0), (x + 0.13, y + 0.13, 3))


def drum(m, x, y, mat):
    m.tube(mat, (x, y, 0.06), (x, y, 0.96), 0.29, n=10)
    for z in (0.34, 0.66): m.tube(mat, (x, y, z), (x, y, z + 0.03), 0.305, n=10)


def tyre_bed(m, x, y):
    m.lathe(WHITE, (x, y), [(0.3, 0.0), (0.52, 0.05), (0.6, 0.17), (0.52, 0.3), (0.3, 0.34)], n=12)
    m.tube("#4a3a2a", (x, y, 0.0), (x, y, 0.26), 0.3, n=10)
    for i in range(5): m.blob("#c84a5a" if i % 2 else "#e0b030", (x + 0.14 * math.cos(i * 1.3), y + 0.14 * math.sin(i * 1.3), 0.42), (0.1, 0.1, 0.1), seed=i, detail=1)
    for i in range(4): m.blob("#4a7a3a", (x + 0.1 * math.cos(i * 1.6 + 1), y + 0.1 * math.sin(i * 1.6 + 1), 0.32), (0.16, 0.16, 0.1), seed=i + 9)


def plain(m):
    m.box(ASPHALT, (1.0, -15.0, -0.3), (24.0, 15.0, 0.05), flags="_noshadow")
    # the island with its kerb, three pumps and the two posts of the canopy
    m.box(CONCRETE, (8.9, -6.2, 0.0), (10.1, 6.2, 0.17), bevel=0.04)
    for y, body in ((-4.0, RED), (0.0, RED), (4.0, "#c79a3e")): pump(m, 9.5, y, body)                                # the yellow one is diesel
    for y in (-2.0, 2.0):
        m.tube(STEEL, (9.5, y, 0.17), (9.5, y, 3.6), 0.09, n=8); m.box(CONCRETE, (9.25, y - 0.25, 0.17), (9.75, y + 0.25, 0.5), bevel=0.04)
        m.solid((9.4, y - 0.1, 0), (9.6, y + 0.1, 3.0))
    # the operator's house: a brick plinth, plaster, a wall of glass to the pumps with a hatch for the money
    x0, x1, y0, y1, top = 15.0, 20.0, -4.5, 4.5, 3.5
    m.box(BRICK, (x0, y0, 0.0), (x1, y1, 0.9)); m.box(PLASTER, (x0, y0, 0.9), (x1, y1, top))
    m.box(GLASS, (x0 - 0.04, y0 + 0.5, 0.95), (x0 + 0.1, y1 - 1.7, 2.9))
    for i in range(7): yy = y0 + 0.5 + i * (y1 - 1.7 - y0 - 0.5) / 6; m.box(WHITE, (x0 - 0.07, yy - 0.04, 0.95), (x0 - 0.03, yy + 0.04, 2.9))
    for z in (0.9, 2.2, 2.9): m.box(WHITE, (x0 - 0.07, y0 + 0.46, z), (x0 - 0.03, y1 - 1.66, z + 0.07))
    m.box(SLAB, (x0 - 0.35, y0 + 0.4, 0.86), (x0, y1 - 1.6, 0.94))                                                # the sill
    m.box("#f0e0a0 emit", (x0 - 0.08, -0.6, 1.0), (x0 - 0.04, 0.6, 1.5), flags="_noshadow")                       # the lit hatch of the cashier
    m.box(BLUE, (x0 - 0.05, y1 - 1.4, 0.05), (x0 + 0.05, y1 - 0.5, 2.15)); m.box(GLASS, (x0 - 0.07, y1 - 1.25, 1.2), (x0 - 0.05, y1 - 0.65, 1.95))   # the door
    m.box(GLASS, (x0 + 1.2, y0 - 0.04, 1.3), (x1 - 1.2, y0 + 0.1, 2.6)); m.box(WHITE, (x0 + 1.1, y0 - 0.07, 1.2), (x1 - 1.1, y0 - 0.03, 1.3))
    # the roof: one flat slab from behind the house to over the island, its front edge turned up, a blue band on its rim
    prof = [(20.9, top + 0.0), (12.0, top + 0.12), (8.0, top + 0.3), (6.4, top + 0.62)]
    m.shell(SLAB, prof, 0.26, -7.2, 7.2)
    m.box(BLUE, (6.3, -7.25, top + 0.34), (6.42, 7.25, top + 0.66), flags="_noshadow")
    for y in (-7.25, 7.13): m.prism(BLUE, [(20.9, top - 0.05), (6.42, top + 0.55), (6.42, top + 0.63), (20.9, top + 0.03)], y, y + 0.12)
    m.box(STEEL, (6.5, -4.6, top + 0.62), (6.56, 4.6, top + 0.68)); m.box(STEEL, (6.5, -4.6, top + 1.95), (6.56, 4.6, top + 2.01))        # the frame the letters stand in
    for y in (-4.6, -1.5, 1.5, 4.54): m.box(STEEL, (6.5, y, top + 0.62), (6.56, y + 0.06, top + 2.0)); 
    for y in (-3.0, 3.0): m.tube(STEEL, (6.53, y, top + 1.9), (8.2, y, top + 0.3), 0.025, n=4)
    m.point("letters", (6.45, 0.0, top + 1.3)); m.point("price", (4.38, -12.0, 2.55))
    # round the house: the fire board, a box of sand, drums of oil on a rack, the vents of the tanks
    m.box(RED, (16.0, y1 + 0.05, 0.9), (18.4, y1 + 0.12, 2.5))
    for xx in (16.0, 18.3): m.box(RED, (xx, y1 + 0.02, 0.0), (xx + 0.1, y1 + 0.12, 2.6))
    m.lathe(RED, (16.5, y1 + 0.3), [(0.1, 1.3), (0.17, 1.65)], n=8); m.tube(PLANK, (17.2, y1 + 0.16, 1.0), (17.2, y1 + 0.16, 2.3), 0.02, n=4)
    m.box(STEEL, (17.1, y1 + 0.13, 1.0), (17.3, y1 + 0.17, 1.3)); m.tube(PLANK, (17.8, y1 + 0.16, 1.1), (18.1, y1 + 0.16, 2.3), 0.02, n=4); m.box(STEEL, (18.0, y1 + 0.13, 2.2), (18.25, y1 + 0.17, 2.35))
    m.box(RED, (18.8, y1 + 0.1, 0.0), (19.9, y1 + 0.9, 0.7)); m.prism(RED, [(y1 + 0.05, 0.7), (y1 + 0.95, 0.7), (y1 + 0.95, 0.76), (y1 + 0.05, 0.95)], 18.75, 19.95, axis="x")
    for i, mat in enumerate((BLUE, "#4a6a4a", BLUE, "#6a6a62")): drum(m, 16.0 + i * 0.68, y0 - 0.5, mat)
    drum(m, 16.3, y0 - 1.15, "#6a6a62")
    for y in (-11.0, -10.0, -9.0):
        m.tube(STEEL, (21.5, y, 0), (21.5, y, 2.6), 0.04, n=5); m.lathe(STEEL, (21.5, y), [(0.04, 2.6), (0.16, 2.6), (0.04, 2.8)], n=6)
    for y in (-10.5, -9.0, -7.5): m.tube(STEEL, (13.0, y, 0.04), (13.0, y, 0.07), 0.4, n=10, flags="_noshadow")     # the hatches of the tanks
    m.box(WHITE, (21.0, 9.0, 0.0), (23.2, 11.6, 2.3)); m.prism(SLAB, [(20.8, 2.5), (23.4, 2.3), (23.4, 2.38), (20.8, 2.58)], 8.8, 11.8)   # the whitewashed privy
    for y in (9.5, 10.8): m.box(BLUE, (20.97, y - 0.35, 0.05), (21.0, y + 0.35, 1.95))
    # by the road: the price board on two posts, a lamp at either end, a tyre with flowers
    for y in (-13.0, -11.0): m.box(STEEL, (4.45, y - 0.04, 0), (4.53, y + 0.04, 3.3))
    m.box(WHITE, (4.4, -13.1, 1.9), (4.48, -10.9, 3.2)); m.box(BLUE, (4.38, -13.1, 2.95), (4.4, -10.9, 3.2), flags="_noshadow")
    lamp(m, 5.0, 13.0); lamp(m, 22.5, -13.5, 6.0)
    tyre_bed(m, 5.2, -9.5); tyre_bed(m, 14.2, 5.6)
    m.box(PLANK, (14.2, -3.5, 0.42), (14.6, -1.5, 0.47)); 
    for y in (-3.3, -1.7): m.box(CONCRETE, (14.25, y - 0.06, 0.05), (14.55, y + 0.06, 0.42))
    m.solid((x0, y0, 0), (x1, y1, top)); m.solid((21.0, 9.0, 0), (23.2, 11.6, 2.3)); m.solid((4.4, -13.1, 0), (4.55, -10.9, 3.2))


def tea_house(m):
    """The tea-house: a long house of logs with its gable to the road, a porch, a pump from before the war."""
    m.box("#5e5448", (1.0, -13.0, -0.3), (23.0, 13.0, 0.04), flags="_noshadow")
    x0, x1, hw, rows = 14.0, 21.0, 4.6, 10; top = iz.BASE + rows * 2 * iz.R
    m.box(iz.STONE, (x0 - 0.05, -hw - 0.05, -0.4), (x1 + 0.05, hw + 0.05, iz.BASE), bevel=0.03)
    iz.logs(m, x0, x1, -hw, hw, rows)
    ridge, slope = iz.gable_roof(m, x0, x1, hw, top, steep=0.7)
    zc = iz.BASE + rows * iz.R + 0.15
    for y in (-3.1, -1.7, 1.7, 3.1): iz.frame(m, y, zc, x=x0 - iz.R)
    iz.frame(m, 0.0, top + hw * 0.7 * 0.4, w=0.7, h=0.7, x=x0 - 0.06)
    m.box(iz.TRIM, (x0 - 0.09, -hw - 0.12, top - 0.04), (x0 - 0.05, hw + 0.12, top + 0.14))
    m.point("window", (x0 - iz.R - 0.12, -3.1, zc)); m.point("letters", (x0 - 0.2, 0.0, top + 0.55))
    m.box(iz.PLANK, (x0 - 0.12, -2.6, top + 0.2), (x0 - 0.06, 2.6, top + 0.9))                                       # the board the name is written on
    # the porch: steps, a floor, turned posts, a little gabled roof
    m.box(iz.PLANK, (x0 - 2.0, -1.4, 0.0), (x0, 1.4, iz.BASE + 0.1))
    for k in range(2): m.box(iz.PLANK, (x0 - 2.3 - k * 0.3, -1.1, 0.0), (x0 - 2.0 - k * 0.3, 1.1, iz.BASE - 0.1 - k * 0.14))
    m.box(iz.DARK, (x0 - iz.R - 0.03, -0.5, iz.BASE + 0.1), (x0, 0.5, iz.BASE + 2.1)); m.box(iz.TRIM, (x0 - iz.R - 0.06, -0.45, iz.BASE + 0.15), (x0 - iz.R - 0.03, 0.45, iz.BASE + 2.05))
    for y in (-1.3, 1.2): m.box(iz.TRIM, (x0 - 1.95, y, iz.BASE + 0.1), (x0 - 1.83, y + 0.12, 2.75))
    for s in (-1, 1): m.prism(iz.ROOF, [(s * 1.75, 2.7), (0, 3.45), (0, 3.52), (s * 1.75, 2.77)], x0 - 2.3, x0, axis="x")
    m.prism(iz.TRIM, [(-1.5, 2.78), (1.5, 2.78), (0, 3.4)], x0 - 2.28, x0 - 2.24, axis="x")
    m.box(iz.GLASS, (x0 - 2.1, 1.5, 2.2), (x0 - 1.9, 1.7, 2.5)); m.box("#ffd080 emit", (x0 - 2.07, 1.53, 2.22), (x0 - 1.93, 1.67, 2.48), flags="_noshadow")   # the lantern
    # the chimney, and a stove-pipe of the kitchen at the back
    cx, cy = 18.0, 3.0; cz = ridge - abs(cy) * slope
    m.box(iz.WHITE, (cx - 0.35, cy - 0.35, cz - 0.3), (cx + 0.35, cy + 0.35, ridge + 0.6)); m.box(iz.BRICK, (cx - 0.41, cy - 0.41, ridge + 0.6), (cx + 0.41, cy + 0.41, ridge + 0.75))
    m.point("smoke", (cx, cy, ridge + 0.9))
    # the yard: the old pump on its pad, a table and benches under a roof on posts, a stack of logs, a barrel for rain
    m.box(CONCRETE, (8.9, 3.0, 0.0), (10.1, 5.0, 0.17), bevel=0.04)
    m.box(RED, (9.25, 3.7, 0.17), (9.75, 4.3, 1.5), bevel=0.05); m.tube(RED, (9.5, 4.0, 1.5), (9.5, 4.0, 2.0), 0.14, n=8)
    m.lathe(WHITE, (9.5, 4.0), [(0.0, 2.0), (0.24, 2.1), (0.3, 2.3), (0.24, 2.5), (0.0, 2.6)], n=10)                 # the glass globe
    for s in (-1, 1): m.tube(WHITE, (9.5 + s * 0.25, 4.0, 1.15), (9.5 + s * 0.27, 4.0, 1.15), 0.18, n=12, flags="_noshadow")
    for a, b in zip([(9.5, 4.3, 1.2), (9.5, 4.55, 0.7), (9.5, 4.45, 1.3)], [(9.5, 4.55, 0.7), (9.5, 4.45, 1.3), (9.5, 4.32, 1.4)]): m.tube(DARK, a, b, 0.025, n=5, caps=False)
    m.solid((9.25, 3.7, 0), (9.75, 4.3, 2.0))
    for x in (8.0, 11.0):
        for y in (-9.0, -5.6): m.box(iz.PLANK, (x - 0.06, y - 0.06, 0.0), (x + 0.06, y + 0.06, 2.3))
    for s in (-1, 1): m.prism(iz.ROOF, [(9.5 + s * 2.0, 2.25), (9.5, 3.0), (9.5, 3.07), (9.5 + s * 2.0, 2.32)], -9.5, -5.1)
    m.box(iz.PLANK, (9.1, -8.6, 0.72), (9.9, -6.0, 0.78))
    for y in (-8.3, -6.3): m.box(iz.PLANK, (9.2, y - 0.05, 0.0), (9.8, y + 0.05, 0.72))
    for x in (8.55, 10.2):
        m.box(iz.PLANK, (x, -8.6, 0.42), (x + 0.28, -6.0, 0.46))
        for y in (-8.3, -6.3): m.box(iz.PLANK, (x + 0.05, y - 0.04, 0.0), (x + 0.23, y + 0.04, 0.42))
    m.solid((9.1, -8.6, 0), (9.9, -6.0, 0.78))
    for row in range(5):
        for i in range(12):
            y = -hw - 0.2 - 0.0; xx = x0 + 1.0 + i * 0.24 + (row % 2) * 0.1
            m.tube(iz.END, (xx, -hw - 0.75, 0.12 + row * 0.21), (xx, -hw - 0.3, 0.12 + row * 0.21), 0.11, n=5)
    m.tube(iz.PLANK, (x0 - 0.5, hw + 0.2, 0.0), (x0 - 0.5, hw + 0.2, 0.9), 0.36, 0.4, n=9)
    m.solid((x0, -hw, 0), (x1, hw, top)); m.solid((x0 - 2.0, -1.4, 0), (x0, 1.4, iz.BASE))


for name, build in (("station-plain", plain), ("station-tavern", tea_house)):
    m = Model(); build(m); m.save(os.path.join(out, name + ".blend"))
