"""Izbas: a house of logs laid one on another with their ends crossed at the corners, a planked gable to the street,
windows in carved frames, a porch, a brick chimney; in the yard a shed, a woodpile, and a picket fence along the
street. paint0 is the paint of the frames, the bargeboards and the pickets: each house has its own.
The house fronts the street at x 0; at_window is a window to light, at_smoke the top of the chimney.
Run: blender -b --factory-startup --python make_izba.py -- OUTDIR"""
import sys, os, math, random
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import kit
from kit import Model
kit.PREVIEW.update({"paint0": "#5b86a8"})

out = sys.argv[sys.argv.index("--") + 1]
TRIM, WHITE, GLASS, STONE, BRICK, PLANK, DARK = "paint0", "#e6e2d6", "#1c2630", "#77756e", "#8a4a3a", "#7a6a56", "#3a3028"
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


def izba(m, W, D, rows, log, end, roof, seed, windows=3, shutters=False, veranda=False, well=False, shed=True, lot=15.0):
    rnd = random.Random(seed)
    base = 0.35; top = base + rows * 2 * R; hw = W / 2
    m.box(STONE, (-0.05, -hw - 0.05, -0.4), (D + 0.05, hw + 0.05, base), bevel=0.03)
    m.box(DARK, (0.04, -hw + 0.04, base), (D - 0.04, hw - 0.04, top))                      # what shows between the logs
    for k in range(rows):                                                                # the logs: the long walls and the short take turns
        z = base + R + k * 2 * R
        for x0 in (0.0, D):
            m.tube(log, (x0, -hw - 0.3, z), (x0, hw + 0.3, z), R, n=6, caps=False)
            for s in (-1, 1): m.tube(end, (x0, s * (hw + 0.3), z), (x0, s * (hw + 0.305), z), R, R * 0.96, n=6)
        z += R
        if k == rows - 1: break
        for y0 in (-hw, hw):
            m.tube(log, (-0.3, y0, z), (D + 0.3, y0, z), R, n=6, caps=False)
            for s, xe in ((-1, -0.3), (1, D + 0.3)): m.tube(end, (xe, y0, z), (xe + s * 0.005, y0, z), R, R * 0.96, n=6)
    # the gables in planks, the roof over them, the bargeboards with the ridge-piece
    rise = hw * 0.85; ov = 0.55; th = 0.09; ridge = top + rise; slope = rise / hw
    for x0, x1 in ((-0.06, 0.06), (D - 0.06, D + 0.06)):
        m.prism(PLANK, [(-hw - 0.1, top), (hw + 0.1, top), (0, ridge + 0.1 * slope)], x0, x1, axis="x")
    for s in (-1, 1):
        y_e = s * (hw + ov); z_e = top - ov * slope
        m.prism(roof, [(y_e, z_e), (0, ridge), (0, ridge + th), (y_e, z_e + th)], -0.7, D + 0.7, axis="x")
        m.prism(TRIM, [(y_e, z_e - 0.16), (0, ridge - 0.16), (0, ridge + th + 0.02), (y_e, z_e + th + 0.02)], -0.76, -0.7, axis="x")   # the bargeboard
        m.box(TRIM, (-0.78, y_e - 0.05 if s > 0 else y_e - 0.03, z_e - 0.45), (-0.72, y_e + 0.03 if s > 0 else y_e + 0.05, z_e + 0.1))
    m.box(roof, (-0.72, -0.09, ridge + th - 0.03), (D + 0.72, 0.09, ridge + th + 0.05))
    m.prism(TRIM, [(-0.1, ridge + th), (0.1, ridge + th), (0, ridge + th + 0.55)], -0.78, -0.72, axis="x")              # the ridge-piece
    m.box(TRIM, (-0.09, -hw - 0.12, top - 0.04), (-0.05, hw + 0.12, top + 0.14))                                  # the board under the gable
    frame(m, 0.0, top + rise * 0.42, w=0.55, h=0.6, x=-0.06)                                                     # the garret window
    # the windows to the street
    zc = base + rows * R + 0.15
    ys = [0.0] if windows == 1 else [(-1 + 2 * i / (windows - 1)) * (hw - 1.15) for i in range(windows)]
    for y in ys: frame(m, y, zc, shutters=shutters)
    m.point("window", (-R - 0.12, ys[0], zc))
    for xw in ([D * 0.5] if D < 7 else [D * 0.3, D * 0.7]):                                                      # a plain window in the far side wall
        m.box(GLASS, (xw - 0.4, -hw - R - 0.02, zc - 0.5), (xw + 0.4, -hw, zc + 0.5))
        m.box(TRIM, (xw - 0.52, -hw - R - 0.05, zc - 0.62), (xw + 0.52, -hw - R - 0.01, zc - 0.5)); m.box(TRIM, (xw - 0.52, -hw - R - 0.05, zc + 0.5), (xw + 0.52, -hw - R - 0.01, zc + 0.66))
        for s in (-1, 1): m.box(TRIM, (xw + s * 0.46 - 0.06, -hw - R - 0.05, zc - 0.5), (xw + s * 0.46 + 0.06, -hw - R - 0.01, zc + 0.5))
    # the chimney: whitewashed brick, a cap, soot at the lip
    cx, cy = D * 0.55, -hw * 0.3; cz = ridge - abs(cy) * slope
    m.box(WHITE, (cx - 0.3, cy - 0.3, cz - 0.3), (cx + 0.3, cy + 0.3, ridge + 0.75)); m.box(BRICK, (cx - 0.36, cy - 0.36, ridge + 0.75), (cx + 0.36, cy + 0.36, ridge + 0.9))
    m.box(DARK, (cx - 0.2, cy - 0.2, ridge + 0.9), (cx + 0.2, cy + 0.2, ridge + 0.93))
    m.point("smoke", (cx, cy, ridge + 1.0))
    # the porch on the yard side, or a glazed veranda
    px = D * 0.62
    if veranda:
        x0, x1, y1 = px - 1.8, px + 1.8, hw + 2.3
        m.box(PLANK, (x0, hw, 0.0), (x1, y1, base + 0.75))
        for xx in (x0, x1 - 0.1, px - 0.05): m.box(WHITE, (xx, y1 - 0.1, base + 0.75), (xx + 0.1, y1, top - 0.25))
        for yy in (hw + 0.2, y1 - 0.1): 
            for xx in (x0, x1 - 0.1): m.box(WHITE, (xx, yy, base + 0.75), (xx + 0.1, yy + 0.1, top - 0.25))
        m.box(GLASS, (x0 + 0.04, hw + 0.1, base + 0.75), (x1 - 0.04, y1 - 0.04, top - 0.25))
        for i in range(1, 6): m.box(WHITE, (x0 + i * 0.6 - 0.02, y1 - 0.02, base + 0.75), (x0 + i * 0.6 + 0.02, y1 + 0.01, top - 0.25))
        m.box(WHITE, (x0, y1 - 0.02, base + 1.5), (x1, y1 + 0.01, base + 1.55))
        m.prism(roof, [(x0 - 0.3, top - 0.25), (x1 + 0.3, top - 0.25), (x1 + 0.3, top - 0.17), (x0 - 0.3, top - 0.17)], hw, y1 + 0.35)   # flat lean-to, a little pitched below
        m.box(TRIM, (x0 - 0.3, y1 + 0.3, top - 0.4), (x1 + 0.3, y1 + 0.36, top - 0.17))
        m.box(PLANK, (x0 - 1.2, hw + 0.5, 0.0), (x0, hw + 1.6, base * 0.5)); m.box(PLANK, (x0 - 0.6, hw + 0.5, 0.0), (x0, hw + 1.6, base))
        m.box(TRIM, (x0 - 0.03, hw + 0.6, base), (x0 + 0.02, hw + 1.5, base + 2.0))
        m.solid((x0, hw, 0), (x1, y1, top))
    else:
        x0, x1, y1 = px - 0.9, px + 0.9, hw + 1.5
        m.box(PLANK, (x0, hw, 0.0), (x1, y1, base + 0.1))
        for k in range(2): m.box(PLANK, (x0 + 0.1, y1 + k * 0.3, 0.0), (x1 - 0.1, y1 + (k + 1) * 0.3, base - 0.1 - k * 0.14))
        m.box(DARK, (px - 0.45, hw - 0.02, base + 0.1), (px + 0.45, hw + R + 0.03, base + 2.05))                         # the door
        m.box(TRIM, (px - 0.4, hw + R + 0.03, base + 0.15), (px + 0.4, hw + R + 0.06, base + 2.0))
        for xx in (x0 + 0.03, x1 - 0.13): m.box(TRIM, (xx, y1 - 0.13, base + 0.1), (xx + 0.1, y1 - 0.03, top - 0.3))
        for xx in (x0 + 0.03, x1 - 0.09): m.box(PLANK, (xx, hw + 0.2, base + 0.95), (xx + 0.06, y1 - 0.03, base + 1.02))    # the rails
        m.prism(roof, [(hw - 0.05, top - 0.1), (y1 + 0.3, top - 0.55), (y1 + 0.3, top - 0.48), (hw - 0.05, top - 0.03)], x0 - 0.25, x1 + 0.25, axis="x")
    # the bank of earth boarded round the foot of the wall, and a bench on it by the street
    m.box(PLANK, (-0.42, -hw + 0.3, 0.0), (-R, hw - 0.3, base + 0.12))
    # the yard: a shed of boards with a lean-to roof, a woodpile under its eave
    half = lot / 2
    if shed:
        sx0, sx1, sy0, sy1 = D + 1.2, D + 4.6, -half + 0.6, -half + 4.2
        m.box(PLANK, (sx0, sy0, 0.0), (sx1, sy1, 2.1))
        for i in range(1, 7): m.box(DARK, (sx0 + i * 0.5 - 0.012, sy1, 0.05), (sx0 + i * 0.5 + 0.012, sy1 + 0.012, 2.1))
        m.box(DARK, (sx0 + 0.3, sy1, 0.05), (sx0 + 1.3, sy1 + 0.03, 1.85))
        m.prism(roof, [(sy0 - 0.3, 2.1), (sy1 + 0.4, 2.65), (sy1 + 0.4, 2.72), (sy0 - 0.3, 2.17)], sx0 - 0.3, sx1 + 0.3, axis="x")
        m.prism(PLANK, [(sy0, 2.1), (sy1, 2.1), (sy1, 2.6)], sx0, sx1, axis="x")
        m.solid((sx0, sy0, 0), (sx1, sy1, 2.1))
        for row in range(4):                                                             # the woodpile
            for i in range(9):
                yy = sy1 + 0.35 + i * 0.24 + (row % 2) * 0.1
                m.tube(end, (sx1 - 0.5, yy, 0.12 + row * 0.22), (sx1 - 0.05, yy, 0.12 + row * 0.22), 0.11, n=5)
    if well:                                                                              # a well with a sweep
        wx, wy = D * 0.3, half - 2.6
        m.box(log, (wx - 0.6, wy - 0.6, 0.0), (wx + 0.6, wy + 0.6, 0.85)); m.box(DARK, (wx - 0.45, wy - 0.45, 0.85), (wx + 0.45, wy + 0.45, 0.86))
        m.tube(log, (wx + 1.6, wy, 0.0), (wx + 1.6, wy, 3.4), 0.09, n=5)
        m.tube(log, (wx + 3.6, wy, 2.4), (wx - 0.2, wy, 4.3), 0.06, 0.04, n=5)
        m.tube(DARK, (wx - 0.2, wy, 4.3), (wx, wy, 1.1), 0.012, n=3, caps=False); m.box(STONE, (wx - 0.13, wy - 0.13, 0.86), (wx + 0.13, wy + 0.13, 1.14))
    # the fence: pickets along the street between posts, a gate; plain boards down the sides
    fx = -2.6; n = int(lot / 0.19); gate = (hw + 1.0, hw + 2.0)
    for i in range(n):
        y = -half + (i + 0.5) * lot / n
        if gate[0] < y < gate[1]: continue
        hgt = 1.05 + 0.04 * math.sin(i * 0.9) + rnd.uniform(-0.02, 0.02)
        m.prism(TRIM, [(y - 0.05, 0.12), (y + 0.05, 0.12), (y + 0.05, hgt), (y, hgt + 0.09), (y - 0.05, hgt)], fx - 0.02, fx, axis="x")
    for z in (0.35, 0.85):
        m.box(PLANK, (fx, -half, z), (fx + 0.04, gate[0], z + 0.07)); m.box(PLANK, (fx, gate[1], z), (fx + 0.04, half, z + 0.07))
    for y in [-half + i * lot / 5 for i in range(6)] + list(gate):
        if gate[0] < y < gate[1]: continue
        m.box(PLANK, (fx + 0.02, y - 0.06, 0.0), (fx + 0.14, y + 0.06, 1.2))
    for g in gate: m.box(log, (fx - 0.04, g - 0.09, 0.0), (fx + 0.14, g + 0.09, 1.75))
    m.box(TRIM, (fx - 0.02, gate[0] + 0.09, 0.15), (fx + 0.02, gate[1] - 0.09, 1.35))                              # the wicket
    m.box(WHITE, (fx - 0.035, gate[0] + 0.2, 0.3), (fx - 0.02, gate[1] - 0.2, 1.2))
    m.box(roof, (fx - 0.1, gate[0] - 0.2, 1.75), (fx + 0.2, gate[1] + 0.2, 1.81))
    m.box(PLANK, (fx - 0.5, -hw + 0.5, 0.4), (fx - 0.12, -hw + 2.3, 0.45))                                          # the bench at the fence
    for y in (-hw + 0.7, -hw + 2.1): m.box(PLANK, (fx - 0.45, y - 0.05, 0.0), (fx - 0.17, y + 0.05, 0.4))
    for s in (-1, 1):                                                                     # the side fences: grey boards of uneven height
        x = fx
        while x < D + 5.0:
            hgt = 1.5 + rnd.uniform(-0.12, 0.1); m.box(PLANK, (x, s * half - 0.015, 0.05), (x + 0.92, s * half + 0.015, hgt)); x += 0.96
    m.solid((0, -hw, 0), (D, hw, top))


KINDS = (("izba-1", lambda m: izba(m, 6.0, 7.0, 9, "#6a5038", "#a08660", "#7a7c78", 1, windows=3, shutters=False)),
         ("izba-2", lambda m: izba(m, 6.6, 9.4, 10, "#8a6a44", "#b89868", "#7a4a3a", 2, windows=3, shutters=True, veranda=True)),
         ("izba-3", lambda m: izba(m, 5.0, 5.6, 8, "#6e665a", "#8e8672", "#4e463c", 3, windows=2, shutters=True, well=True, shed=False)),
         ("izba-4", lambda m: izba(m, 5.6, 8.0, 9, "#54402e", "#8a7250", "#5a6258", 4, windows=2, veranda=True, well=True)))
for name, build in KINDS:
    m = Model(); build(m); m.save(os.path.join(out, name + ".blend"))
