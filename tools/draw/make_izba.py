"""Izbas in parts, which the game puts together a new way for every yard (IZBA in lisp/city.lisp):
  izba-srub-N    the house: logs laid one on another with their ends crossed, a planked gable to the street, windows
                 in carved frames, a chimney. Points: at_window to light, at_smoke, at_porch where the door is, at_back
                 the middle of the back wall, at_side a place for a well beside the house
  izba-porch-N   what stands at the door: an open porch, a glazed veranda, a boarded entry
  izba-shed, -banya, -woodpile, -stog, -outhouse, -well   what stands in the yard, each drawn round its own middle
  izba-fence-N   the fence of a lot fifteen metres wide: pickets, tall boards, poles
paint0 is the paint of the frames, the bargeboards and the pickets, paint1 the logs, paint2 the roofs.
Run: blender -b --factory-startup --python make_izba.py -- OUTDIR"""
import sys, os, math, random
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import kit
from kit import Model
kit.PREVIEW.update({"paint0": "#5b86a8", "paint1": "#6a5038", "paint2": "#7a7c78"})

out = sys.argv[sys.argv.index("--") + 1]
LOG, ROOF, END, TRIM, WHITE, GLASS, STONE, BRICK, PLANK, DARK = "paint1", "paint2", "#a08660", "paint0", "#e6e2d6", "#1c2630", "#77756e", "#8a4a3a", "#7a6a56", "#3a3028"
R = 0.14                                                        # half the thickness of a log


def frame(m, y, z, w=0.8, h=1.1, shutters=False, x=-R):
    """A window on the street wall, its middle at (Y, Z): the glass, the sash, the carved frame with its crown."""
    m.box(GLASS, (x - 0.03, y - w / 2, z - h / 2), (x + 0.1, y + w / 2, z + h / 2))
    m.box(WHITE, (x - 0.05, y - 0.025, z - h / 2), (x - 0.02, y + 0.025, z + h / 2))                       # the sash: a post and a bar
    m.box(WHITE, (x - 0.05, y - w / 2, z + h * 0.18), (x - 0.02, y + w / 2, z + h * 0.18 + 0.05))
    b = 0.13
    for y0 in (y - w / 2 - b, y + w / 2): m.box(TRIM, (x - 0.07, y0, z - h / 2 - 0.05), (x, y0 + b, z + h / 2 + 0.05))
    m.box(TRIM, (x - 0.09, y - w / 2 - b - 0.05, z - h / 2 - 0.16), (x, y + w / 2 + b + 0.05, z - h / 2))                # the sill board
    m.prism(TRIM, [(y - w / 2 - b - 0.04, z - h / 2 - 0.16), (y, z - h / 2 - 0.36), (y + w / 2 + b + 0.04, z - h / 2 - 0.16)], x - 0.06, x, axis="x")
    top = z + h / 2
    m.prism(TRIM, [(y - w / 2 - b - 0.1, top), (y + w / 2 + b + 0.1, top), (y + w / 2 + b + 0.1, top + 0.14), (y + 0.22, top + 0.2), (y, top + 0.42),
                   (y - 0.22, top + 0.2), (y - w / 2 - b - 0.1, top + 0.14)], x - 0.09, x, axis="x")          # the crown
    m.prism(WHITE, [(y - 0.13, top + 0.07), (y + 0.13, top + 0.07), (y, top + 0.3)], x - 0.11, x - 0.09, axis="x")
    if shutters:
        for s in (-1, 1):
            y0 = y + s * (w / 2 + b + 0.22)
            m.box(TRIM, (x - 0.04, y0 - 0.2, z - h / 2), (x, y0 + 0.2, z + h / 2))
            m.box(WHITE, (x - 0.055, y0 - 0.12, z - h * 0.3), (x - 0.04, y0 + 0.12, z + h * 0.3))


BASE, LOT = 0.35, 15.0


def logs(m, x0, x1, y0, y1, rows, base=BASE, r=R):
    """A box of logs: the long walls and the short take turns, the ends stick out at the corners."""
    m.box(DARK, (x0 + 0.04, y0 + 0.04, base), (x1 - 0.04, y1 - 0.04, base + rows * 2 * r))
    for k in range(rows):
        z = base + r + k * 2 * r
        for x in (x0, x1):
            m.tube(LOG, (x, y0 - 0.3, z), (x, y1 + 0.3, z), r, n=6, caps=False)
            for s, ye in ((-1, y0 - 0.3), (1, y1 + 0.3)): m.tube(END, (x, ye, z), (x, ye + s * 0.005, z), r, r * 0.96, n=6)
        z += r
        if k == rows - 1: break
        for y in (y0, y1):
            m.tube(LOG, (x0 - 0.3, y, z), (x1 + 0.3, y, z), r, n=6, caps=False)
            for s, xe in ((-1, x0 - 0.3), (1, x1 + 0.3)): m.tube(END, (xe, y, z), (xe + s * 0.005, y, z), r, r * 0.96, n=6)


def gable_roof(m, x0, x1, hw, top, steep=0.85, ov=0.55, trim=True):
    """Planked gables at X0 and X1 and a roof of two slopes over them; returns the height of the ridge and the slope."""
    rise = hw * steep; th = 0.09; ridge = top + rise; slope = rise / hw
    for x in (x0, x1): m.prism(PLANK, [(-hw - 0.1, top), (hw + 0.1, top), (0, ridge + 0.1 * slope)], x - 0.06, x + 0.06, axis="x")
    for s in (-1, 1):
        y_e = s * (hw + ov); z_e = top - ov * slope
        m.prism(ROOF, [(y_e, z_e), (0, ridge), (0, ridge + th), (y_e, z_e + th)], x0 - 0.7, x1 + 0.7, axis="x")
        if trim:
            m.prism(TRIM, [(y_e, z_e - 0.16), (0, ridge - 0.16), (0, ridge + th + 0.02), (y_e, z_e + th + 0.02)], x0 - 0.76, x0 - 0.7, axis="x")   # the bargeboard
            m.box(TRIM, (x0 - 0.78, y_e - 0.05 if s > 0 else y_e - 0.03, z_e - 0.45), (x0 - 0.72, y_e + 0.03 if s > 0 else y_e + 0.05, z_e + 0.1))
    m.box(ROOF, (x0 - 0.72, -0.09, ridge + th - 0.03), (x1 + 0.72, 0.09, ridge + th + 0.05))
    if trim: m.prism(TRIM, [(-0.1, ridge + th), (0.1, ridge + th), (0, ridge + th + 0.55)], x0 - 0.78, x0 - 0.72, axis="x")              # the ridge-piece
    return ridge, slope


def srub(m, W, D, rows, windows=3, shutters=False):
    hw = W / 2; top = BASE + rows * 2 * R
    m.box(STONE, (-0.05, -hw - 0.05, -0.4), (D + 0.05, hw + 0.05, BASE), bevel=0.03)
    logs(m, 0.0, D, -hw, hw, rows)
    ridge, slope = gable_roof(m, 0.0, D, hw, top)
    m.box(TRIM, (-0.09, -hw - 0.12, top - 0.04), (-0.05, hw + 0.12, top + 0.14))                                  # the board under the gable
    frame(m, 0.0, top + hw * 0.85 * 0.42, w=0.55, h=0.6, x=-0.06)                                                # the garret window
    zc = BASE + rows * R + 0.15
    ys = [(-1 + 2 * i / (windows - 1)) * (hw - 1.15) for i in range(windows)]
    for y in ys: frame(m, y, zc, shutters=shutters)
    m.point("window", (-R - 0.12, ys[0], zc))
    for xw in ([D * 0.5] if D < 7 else [D * 0.3, D * 0.7]):                                                      # a plain window in the far side wall
        m.box(GLASS, (xw - 0.4, -hw - R - 0.02, zc - 0.5), (xw + 0.4, -hw, zc + 0.5))
        m.box(TRIM, (xw - 0.52, -hw - R - 0.05, zc - 0.62), (xw + 0.52, -hw - R - 0.01, zc - 0.5)); m.box(TRIM, (xw - 0.52, -hw - R - 0.05, zc + 0.5), (xw + 0.52, -hw - R - 0.01, zc + 0.66))
        for s in (-1, 1): m.box(TRIM, (xw + s * 0.46 - 0.06, -hw - R - 0.05, zc - 0.5), (xw + s * 0.46 + 0.06, -hw - R - 0.01, zc + 0.5))
    cx, cy = D * 0.5, -hw * 0.3; cz = ridge - abs(cy) * slope                                                      # the chimney: whitewashed brick, a cap, soot at the lip
    m.box(WHITE, (cx - 0.3, cy - 0.3, cz - 0.3), (cx + 0.3, cy + 0.3, ridge + 0.75)); m.box(BRICK, (cx - 0.36, cy - 0.36, ridge + 0.75), (cx + 0.36, cy + 0.36, ridge + 0.9))
    m.box(DARK, (cx - 0.2, cy - 0.2, ridge + 0.9), (cx + 0.2, cy + 0.2, ridge + 0.93))
    m.box(PLANK, (-0.42, -hw + 0.3, 0.0), (-R, hw - 0.3, BASE + 0.12))                                             # the bank of earth boarded round the foot of the wall
    m.point("smoke", (cx, cy, ridge + 1.0)); m.point("porch", (D * 0.62, hw, 0.0)); m.point("back", (D, 0.0, 0.0)); m.point("side", (D * 0.35, -hw - 2.3, 0.0))
    m.solid((0, -hw, 0), (D, hw, top))


T = 2.6                                                          # the height every porch is built to: under the eaves of the lowest house


def door(m, y=0.0):
    m.box(DARK, (-0.45, y - 0.02, BASE + 0.1), (0.45, y + R + 0.03, BASE + 2.05)); m.box(TRIM, (-0.4, y + R + 0.03, BASE + 0.15), (0.4, y + R + 0.06, BASE + 2.0))


def porch_open(m):
    x0, x1, y1 = -0.9, 0.9, 1.5
    m.box(PLANK, (x0, 0, 0.0), (x1, y1, BASE + 0.1))
    for k in range(2): m.box(PLANK, (x0 + 0.1, y1 + k * 0.3, 0.0), (x1 - 0.1, y1 + (k + 1) * 0.3, BASE - 0.1 - k * 0.14))
    door(m)
    for xx in (x0 + 0.03, x1 - 0.13): m.box(TRIM, (xx, y1 - 0.13, BASE + 0.1), (xx + 0.1, y1 - 0.03, T - 0.3))
    for xx in (x0 + 0.03, x1 - 0.09): m.box(PLANK, (xx, 0.2, BASE + 0.95), (xx + 0.06, y1 - 0.03, BASE + 1.02))    # the rails
    m.prism(ROOF, [(-0.05, T - 0.1), (y1 + 0.3, T - 0.55), (y1 + 0.3, T - 0.48), (-0.05, T - 0.03)], x0 - 0.25, x1 + 0.25, axis="x")


def porch_veranda(m):
    x0, x1, y1 = -1.8, 1.8, 2.3
    m.box(PLANK, (x0, 0, 0.0), (x1, y1, BASE + 0.75))
    for xx in (x0, x1 - 0.1, -0.05): m.box(WHITE, (xx, y1 - 0.1, BASE + 0.75), (xx + 0.1, y1, T - 0.25))
    for xx in (x0, x1 - 0.1): m.box(WHITE, (xx, 0.2, BASE + 0.75), (xx + 0.1, 0.3, T - 0.25))
    m.box(GLASS, (x0 + 0.04, 0.1, BASE + 0.75), (x1 - 0.04, y1 - 0.04, T - 0.25))
    for i in range(1, 6): m.box(WHITE, (x0 + i * 0.6 - 0.02, y1 - 0.02, BASE + 0.75), (x0 + i * 0.6 + 0.02, y1 + 0.01, T - 0.25))
    m.box(WHITE, (x0, y1 - 0.02, BASE + 1.5), (x1, y1 + 0.01, BASE + 1.55))
    m.box(ROOF, (x0 - 0.3, 0, T - 0.25), (x1 + 0.3, y1 + 0.35, T - 0.17)); m.box(TRIM, (x0 - 0.3, y1 + 0.3, T - 0.4), (x1 + 0.3, y1 + 0.36, T - 0.17))
    m.box(PLANK, (x0 - 1.2, 0.5, 0.0), (x0, 1.6, BASE * 0.5)); m.box(PLANK, (x0 - 0.6, 0.5, 0.0), (x0, 1.6, BASE))
    m.box(TRIM, (x0 - 0.03, 0.6, BASE), (x0 + 0.02, 1.5, BASE + 2.0))
    m.solid((x0, 0, 0), (x1, y1, T))


def porch_seni(m):
    """A boarded entry: a box of planks under its own lean-to roof, the door in its end to the street."""
    x0, x1, y1 = -1.3, 1.5, 2.0
    m.box(PLANK, (x0, 0, 0.0), (x1, y1, T - 0.35))
    for i in range(1, 7): m.box(DARK, (x0 + i * 0.4 - 0.01, y1, 0.05), (x0 + i * 0.4 + 0.01, y1 + 0.012, T - 0.35))
    m.box(DARK, (x0 - 0.03, 0.5, BASE - 0.1), (x0, 1.4, BASE + 1.9)); m.box(TRIM, (x0 - 0.06, 0.55, BASE - 0.05), (x0 - 0.03, 1.35, BASE + 1.85))
    m.box(GLASS, (0.2, y1, 1.3), (0.9, y1 + 0.03, 1.9)); m.box(TRIM, (0.12, y1 + 0.03, 1.22), (0.98, y1 + 0.05, 1.3)); m.box(TRIM, (0.12, y1 + 0.03, 1.9), (0.98, y1 + 0.05, 1.98))
    m.prism(ROOF, [(-0.05, T + 0.05), (y1 + 0.35, T - 0.45), (y1 + 0.35, T - 0.38), (-0.05, T + 0.12)], x0 - 0.3, x1 + 0.3, axis="x")
    m.box(PLANK, (x0 - 0.7, 0.5, 0.0), (x0, 1.4, BASE - 0.12)); m.box(PLANK, (x0 - 1.1, 0.5, 0.0), (x0 - 0.7, 1.4, BASE - 0.26))
    m.solid((x0, 0, 0), (x1, y1, T))


def shed(m):
    m.box(PLANK, (-1.7, -1.8, 0.0), (1.7, 1.8, 2.1))
    for i in range(1, 9): m.box(DARK, (-1.7 - 0.012, -1.8 + i * 0.4 - 0.012, 0.05), (-1.7, -1.8 + i * 0.4 + 0.012, 2.1))
    m.box(DARK, (-1.73, -0.2, 0.05), (-1.7, 0.9, 1.85))
    m.prism(ROOF, [(-2.1, 2.72), (2.0, 2.1), (2.0, 2.17), (-2.1, 2.79)], -2.1, 2.1)
    for y in (-1.8, 1.79): m.prism(PLANK, [(-1.7, 2.1), (1.7, 2.1), (-1.7, 2.6)], y, y + 0.01)
    m.solid((-1.7, -1.8, 0), (1.7, 1.8, 2.1))


def banya(m):
    """A bath-house: a low box of logs, one small window, an iron pipe, a bench by the door."""
    logs(m, -1.6, 1.6, -1.8, 1.8, 7, base=0.15, r=0.13)
    top = 0.15 + 7 * 0.26
    gable_roof(m, -1.6, 1.6, 1.8, top, steep=0.55, ov=0.4, trim=False)
    m.box(DARK, (-1.76, -0.4, 0.2), (-1.6, 0.35, 1.75)); m.box(PLANK, (-1.79, -0.35, 0.25), (-1.76, 0.3, 1.7))
    m.box(GLASS, (-1.75, 0.85, 1.05), (-1.6, 1.35, 1.45)); m.box(PLANK, (-1.77, 0.78, 0.98), (-1.74, 1.42, 1.05))
    m.tube(DARK, (0.6, -0.8, top), (0.6, -0.8, top + 1.9), 0.09, n=6)
    m.box(PLANK, (-2.3, -1.6, 0.4), (-1.95, -0.6, 0.45)); 
    for y in (-1.5, -0.7): m.box(PLANK, (-2.25, y - 0.04, 0.0), (-2.0, y + 0.04, 0.4))
    m.solid((-1.6, -1.8, 0), (1.6, 1.8, top))


def woodpile(m):
    for x in (-0.4, 0.4):
        for y in (-1.6, 1.6): m.box(PLANK, (x - 0.05, y - 0.05, 0.0), (x + 0.05, y + 0.05, 1.7 if x < 0 else 1.5))
    m.prism(ROOF, [(-0.7, 1.75), (0.7, 1.5), (0.7, 1.55), (-0.7, 1.8)], -1.9, 1.9)
    for row in range(6):
        for i in range(13):
            y = -1.45 + i * 0.24 + (row % 2) * 0.1
            if y < 1.5: m.tube(END, (-0.3, y, 0.12 + row * 0.21), (0.3, y, 0.12 + row * 0.21), 0.11, n=5)
    m.solid((-0.4, -1.6, 0), (0.4, 1.6, 1.4))


def stog(m):
    m.lathe("#b89a4a", (0, 0), [(1.5, 0.0), (1.75, 0.9), (1.35, 2.1), (0.5, 3.0), (0.0, 3.4)], n=9, smooth=False)
    m.tube(PLANK, (0, 0, 3.0), (0, 0, 4.1), 0.04, n=4)
    m.solid((-1.2, -1.2, 0), (1.2, 1.2, 2.0))


def outhouse(m):
    m.box(PLANK, (-0.55, -0.5, 0.0), (0.55, 0.5, 2.0))
    m.box(DARK, (-0.57, -0.35, 0.1), (-0.55, 0.35, 1.8)); m.box(PLANK, (-0.585, -0.32, 0.13), (-0.57, 0.32, 1.77))
    m.prism(DARK, [(0, 1.5), (0.08, 1.58), (0, 1.66), (-0.08, 1.58)], -0.6, -0.585, axis="x")                     # the little window in the door
    m.prism(ROOF, [(-0.75, 2.25), (0.75, 2.0), (0.75, 2.05), (-0.75, 2.3)], -0.7, 0.7)
    m.solid((-0.55, -0.5, 0), (0.55, 0.5, 2.0))


def well(m):
    m.box(LOG, (-0.6, -0.6, 0.0), (0.6, 0.6, 0.85)); m.box(DARK, (-0.45, -0.45, 0.85), (0.45, 0.45, 0.86))
    m.tube(LOG, (1.6, 0, 0.0), (1.6, 0, 3.4), 0.09, n=5); m.tube(LOG, (3.6, 0, 2.4), (-0.2, 0, 4.3), 0.06, 0.04, n=5)
    m.tube(DARK, (-0.2, 0, 4.3), (0, 0, 1.1), 0.012, n=3, caps=False); m.box(STONE, (-0.13, -0.13, 0.86), (0.13, 0.13, 1.14))
    m.solid((-0.6, -0.6, 0), (0.6, 0.6, 0.85))


FX, DEEP, GATE = -2.6, 14.5, (4.6, 5.6)


def sides(m, rnd, hgt=1.5):
    for s in (-1, 1):                                                                     # grey boards of uneven height down the sides
        x = FX
        while x < DEEP:
            h = hgt + rnd.uniform(-0.12, 0.1); m.box(PLANK, (x, s * LOT / 2 - 0.015, 0.05), (x + 0.92, s * LOT / 2 + 0.015, h)); x += 0.96


def bench(m):
    m.box(PLANK, (FX - 0.5, -2.5, 0.4), (FX - 0.12, -0.7, 0.45))
    for y in (-2.3, -0.9): m.box(PLANK, (FX - 0.45, y - 0.05, 0.0), (FX - 0.17, y + 0.05, 0.4))


def fence_pickets(m):
    rnd = random.Random(1); half = LOT / 2; n = int(LOT / 0.19)
    for i in range(n):
        y = -half + (i + 0.5) * LOT / n
        if GATE[0] < y < GATE[1]: continue
        hgt = 1.05 + 0.04 * math.sin(i * 0.9) + rnd.uniform(-0.02, 0.02)
        m.prism(TRIM, [(y - 0.05, 0.12), (y + 0.05, 0.12), (y + 0.05, hgt), (y, hgt + 0.09), (y - 0.05, hgt)], FX - 0.02, FX, axis="x")
    for z in (0.35, 0.85):
        m.box(PLANK, (FX, -half, z), (FX + 0.04, GATE[0], z + 0.07)); m.box(PLANK, (FX, GATE[1], z), (FX + 0.04, half, z + 0.07))
    for y in [-half + i * LOT / 5 for i in range(6)]:
        if not GATE[0] - 0.2 < y < GATE[1] + 0.2: m.box(PLANK, (FX + 0.02, y - 0.06, 0.0), (FX + 0.14, y + 0.06, 1.2))
    for g in GATE: m.box(PLANK, (FX - 0.04, g - 0.09, 0.0), (FX + 0.14, g + 0.09, 1.75))
    m.box(TRIM, (FX - 0.02, GATE[0] + 0.09, 0.15), (FX + 0.02, GATE[1] - 0.09, 1.35)); m.box(WHITE, (FX - 0.035, GATE[0] + 0.2, 0.3), (FX - 0.02, GATE[1] - 0.2, 1.2))
    m.box(ROOF, (FX - 0.1, GATE[0] - 0.2, 1.75), (FX + 0.2, GATE[1] + 0.2, 1.81))
    bench(m); sides(m, rnd)


def fence_boards(m):
    """Tall grey boards to the street, and a gate for a cart under its own little roof."""
    rnd = random.Random(2); half = LOT / 2; g0, g1 = 2.9, 5.9
    y = -half
    while y < half - 0.01:
        if not g0 - 0.1 < y + 0.1 < g1:
            m.box(PLANK, (FX - 0.015, y, 0.05), (FX + 0.015, min(y + 0.19, half), 1.7 + rnd.uniform(-0.1, 0.08)))
        y += 0.2
    for g in (g0, g1 - 1.0, g1): m.box(LOG, (FX - 0.12, g - 0.12, 0.0), (FX + 0.12, g + 0.12, 2.3))
    m.box(PLANK, (FX - 0.03, g0 + 0.12, 0.1), (FX + 0.03, g1 - 1.12, 1.95)); m.box(TRIM, (FX - 0.03, g1 - 0.88, 0.1), (FX + 0.03, g1 - 0.12, 1.95))
    for z in (0.5, 1.5): m.box(DARK, (FX - 0.05, g0 + 0.12, z), (FX - 0.03, g1 - 1.12, z + 0.08))
    m.prism(ROOF, [(FX - 0.5, 2.3), (FX, 2.55), (FX + 0.5, 2.3), (FX + 0.5, 2.36), (FX, 2.62), (FX - 0.5, 2.36)], g0 - 0.3, g1 + 0.3)
    bench(m); sides(m, rnd, 1.7)


def fence_poles(m):
    """Three poles on posts all the way round, and a gap for a gate: a yard nobody fears for."""
    rnd = random.Random(3); half = LOT / 2
    def run(a, b):
        n = max(1, round(math.dist(a, b) / 2.4))
        for i in range(n + 1):
            t = i / n; m.tube(PLANK, (a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t, 0.0), (a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t, 1.25), 0.06, n=5)
        for z in (0.45, 0.8, 1.12): m.tube(PLANK, (a[0], a[1], z + rnd.uniform(-.05, .05)), (b[0], b[1], z + rnd.uniform(-.05, .05)), 0.035, n=4)
    run((FX, -half), (FX, GATE[0] - 0.4)); run((FX, GATE[1] + 0.4), (FX, half))
    for s in (-1, 1): run((FX, s * half), (DEEP, s * half))


KINDS = (("izba-srub-1", lambda m: srub(m, 6.0, 7.0, 9, 3)), ("izba-srub-2", lambda m: srub(m, 6.6, 9.4, 10, 3, True)),
         ("izba-srub-3", lambda m: srub(m, 5.0, 5.6, 8, 2, True)), ("izba-srub-4", lambda m: srub(m, 5.6, 8.0, 9, 2)),
         ("izba-srub-5", lambda m: srub(m, 7.0, 7.4, 9, 4)),
         ("izba-porch-1", porch_open), ("izba-porch-2", porch_veranda), ("izba-porch-3", porch_seni),
         ("izba-shed", shed), ("izba-banya", banya), ("izba-woodpile", woodpile), ("izba-stog", stog), ("izba-outhouse", outhouse), ("izba-well", well),
         ("izba-fence-1", fence_pickets), ("izba-fence-2", fence_boards), ("izba-fence-3", fence_poles))
if os.path.basename(sys.argv[sys.argv.index("--python") + 1]) == "make_izba.py":      # make_azs.py borrows the logs and the frames
    for name, build in KINDS:
        m = Model(); build(m); m.save(os.path.join(out, name + ".blend"))
