"""The vehicles of the road, each after the one that drove there: a Zhiguli (car-car), a Volga, a Moskvich and a
Zaporozhets for the passenger cars; the militia's Zhiguli; a ZIL-130 with a wooden bed (car-truck) and a red one of the
fire brigade; GAZ-53s with a milk tank, a bread van and a shop on wheels; the army's GAZ-66; a UAZ "loaf" of the
ambulance; a LAZ-695 for the bus. The nose is at +y (the game's -z), the wheels stand on z 0; paint0 is the body.
Run: blender -b --factory-startup --python make_cars.py -- OUTDIR [NAME ...]"""
import sys, os, math
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import kit
from kit import Model
kit.PREVIEW.update({"paint0": "#8a4038"})

out = sys.argv[sys.argv.index("--") + 1]
BODY, GLASS, TYRE, CHROME, DARK, LAMP, RED, AMBER, WHITE, WOOD = "paint0", "#1a2026 rough 0.2", "#141416", "#b8bcc0 metal 0.6 rough 0.35", "#1e2022", "#ffe9b0 emit #ffd890 3", "#a3262c", "#c9812a", "#e0dcd0", "#6a5a48"
FAR, NEAR = "_r320", "_noshadow_r110"                             # what is seen from afar, and the small things seen only close by


def wheels(m, ys, r, half, w=0.2, dual=()):
    """Wheels of radius R at each y of YS, HALF a track apart; DUAL names the ys that carry twin tyres."""
    for y in ys:
        for s in (-1, 1):
            ww = w * (1.9 if y in dual else 1.0)
            m.tube(TYRE, (s * (half - ww), y, r), (s * half, y, r), r, n=12, flags=FAR)
            m.tube(CHROME, (s * half, y, r), (s * (half + 0.012), y, r), r * 0.5, n=8, flags=NEAR)


def arch(m, y, r, half, mat=DARK):
    for s in (-1, 1): m.tube(mat, (s * (half - 0.3), y, r * 0.95), (s * (half + 0.004), y, r * 0.95), r * 1.22, n=12, flags="_noshadow_r320")


def lamps(m, y, z, xs, r=0.085, mat=LAMP):
    for x in xs: m.tube(mat, (x, y - 0.02, z), (x, y + 0.02, z), r, n=10, flags="_noshadow")


def sedan(m, L=4.07, W=1.61, zb=0.92, zr=1.42, cowl=0.55, deck=-1.05, r=0.3, axles=(1.25, -1.17), quad=False, ears=False, stripe=None, beacon=False, lit=True):
    """A three-box saloon: the body from bumper to bumper, the cabin narrower on it, glass all round."""
    h = L / 2; hw = W / 2; z0 = 0.26
    arch(m, axles[0], r, hw); arch(m, axles[1], r, hw)
    m.prism(BODY, [(-h + 0.06, z0), (h - 0.06, z0), (h, 0.52), (h - 0.02, zb - 0.12), (cowl + 0.1, zb), (deck - 0.1, zb), (-h + 0.02, zb - 0.07), (-h, 0.5)], -hw, hw, axis="x")
    cw = hw - 0.09; top0, top1 = cowl - 0.52, deck + 0.42
    m.prism(BODY, [(cowl + 0.05, zb), (top0, zr), (top1, zr), (deck - 0.02, zb)], -cw, cw, axis="x", flags=FAR)
    m.prism(GLASS, [(cowl + 0.03, zb + 0.03), (top0 + 0.02, zr - 0.05), (top0 - 0.02, zr - 0.05), (cowl - 0.02, zb + 0.03)], -cw + 0.07, cw - 0.07, axis="x", flags="_noshadow_r320")   # the windscreen
    m.prism(GLASS, [(deck, zb + 0.03), (top1 - 0.02, zr - 0.05), (top1 + 0.02, zr - 0.05), (deck + 0.05, zb + 0.03)], -cw + 0.07, cw - 0.07, axis="x", flags="_noshadow_r320")
    mid = (top0 + top1) / 2 + 0.05
    for s in (-1, 1):                                               # the side glass: front and rear, a pillar between
        x0, x1 = (s * cw, s * (cw + 0.006)) if s > 0 else (s * (cw + 0.006), s * cw)
        m.prism(GLASS, [(cowl - 0.08, zb + 0.04), (top0 - 0.02, zr - 0.07), (mid + 0.04, zr - 0.07), (mid + 0.04, zb + 0.04)], x0, x1, axis="x", flags="_noshadow_r320")
        m.prism(GLASS, [(mid - 0.04, zb + 0.04), (mid - 0.04, zr - 0.07), (top1 + 0.04, zr - 0.07), (deck + 0.14, zb + 0.04)], x0, x1, axis="x", flags="_noshadow_r320")
        xs = (s * hw, s * (hw + 0.008)) if s > 0 else (s * (hw + 0.008), s * hw)
        m.box(CHROME, (xs[0], -h + 0.3, zb - 0.2), (xs[1], h - 0.3, zb - 0.17), flags=NEAR)                          # the bright strip down the side
        for y in (cowl - 0.05, mid, deck + 0.25): m.box(DARK, (xs[0], y - 0.006, z0 + 0.08), (xs[1], y + 0.006, zb), flags=NEAR)   # the door seams
        if stripe: m.box(stripe, (xs[0], -h + 0.05, zb - 0.38), (xs[1] + s * 0.002, h - 0.05, zb - 0.2), flags="_noshadow_r320")
        if ears: m.box(DARK, (s * hw - (0 if s > 0 else 0.07), deck - 0.35, zb - 0.3), (s * hw + (0.07 if s > 0 else 0), deck + 0.15, zb - 0.12), flags=NEAR)
    for y, s in ((h, 1), (-h, -1)):                                  # the bumpers
        m.box(CHROME, (-hw - 0.02, y - 0.05 if s > 0 else y - 0.03, 0.4), (hw + 0.02, y + 0.03 if s > 0 else y + 0.05, 0.5), flags=FAR)
    if not ears: m.box(DARK, (-hw + 0.3, h - 0.015, 0.58), (hw - 0.3, h + 0.012, zb - 0.16), flags=NEAR)                # the grille
    xs = (-hw + 0.17, hw - 0.17) + ((-hw + 0.37, hw - 0.37) if quad else ())
    lamps(m, h, zb - 0.27, xs, mat=LAMP if lit else WHITE)
    for s in (-1, 1): m.box(RED, (s * (hw - 0.3) - 0.14, -h - 0.012, zb - 0.32), (s * (hw - 0.3) + 0.14, -h + 0.01, zb - 0.18), flags=NEAR)
    if beacon: m.box(DARK, (-0.5, (top0 + top1) / 2 - 0.08, zr), (0.5, (top0 + top1) / 2 + 0.08, zr + 0.06), flags=NEAR)
    wheels(m, axles, r, hw + 0.0)


def cab(m, y0, y1, W, zf, mat, grille=WHITE):
    """A cab with a bonnet, as the ZIL-130 and the GAZ-53 have: the bonnet from Y1 back, the cab behind it to Y0."""
    hw = W / 2; zc = zf + 0.95; zt = zf + 1.75; yb = y0 + 1.55
    m.prism(mat, [(yb, zf), (y1, zf), (y1, zc - 0.12), (y1 - 0.15, zc), (yb, zc + 0.05)], -hw + 0.18, hw - 0.18, axis="x", flags=FAR)       # the bonnet
    for s in (-1, 1):                                                                                             # the wings over the wheels
        x0, x1 = (s * (hw - 0.3), s * hw) if s > 0 else (s * hw, s * (hw - 0.3))
        m.prism(mat, [(yb + 0.1, zf), (y1 + 0.02, zf), (y1 + 0.02, zf + 0.45), (y1 - 0.3, zf + 0.72), (yb + 0.35, zf + 0.72), (yb + 0.1, zf + 0.4)], x0, x1, axis="x", flags=FAR)
    m.prism(mat, [(y0, zf - 0.1), (yb, zf - 0.1), (yb, zc + 0.05), (yb - 0.32, zt), (y0, zt)], -hw, hw, axis="x", flags=FAR)                # the cab
    m.prism(GLASS, [(yb + 0.005, zc + 0.1), (yb - 0.29, zt - 0.1), (yb - 0.31, zt - 0.1), (yb - 0.02, zc + 0.1)], -hw + 0.1, hw - 0.1, axis="x", flags="_noshadow_r320")
    for s in (-1, 1):
        xs = (s * hw, s * (hw + 0.006)) if s > 0 else (s * (hw + 0.006), s * hw)
        m.prism(GLASS, [(y0 + 0.25, zc + 0.12), (yb - 0.12, zc + 0.12), (yb - 0.36, zt - 0.12), (y0 + 0.25, zt - 0.12)], xs[0], xs[1], axis="x", flags="_noshadow_r320")
        m.box(DARK, (xs[0], y0 + 0.2, zf), (xs[1], y0 + 0.212, zt - 0.1), flags=NEAR); m.box(DARK, (xs[0], yb - 0.1, zf), (xs[1], yb - 0.088, zc + 0.1), flags=NEAR)
        m.box(DARK, (s * (hw + 0.02) - 0.02, yb - 0.25, zc + 0.3), (s * (hw + 0.2) + 0.02, yb - 0.2, zc + 0.55), flags=NEAR)       # the mirror
    m.box(grille, (-hw + 0.2, y1 + 0.0, zf + 0.12), (hw - 0.2, y1 + 0.035, zc - 0.15), flags=FAR)                     # the white face
    for i in range(5): m.box(DARK, (-hw + 0.55, y1 + 0.035, zf + 0.22 + i * 0.11), (hw - 0.55, y1 + 0.045, zf + 0.27 + i * 0.11), flags=NEAR)
    lamps(m, y1 + 0.04, zf + 0.42, (-hw + 0.36, hw - 0.36), r=0.1)
    m.box(DARK, (-hw - 0.02, y1 + 0.03, zf - 0.12), (hw + 0.02, y1 + 0.16, zf + 0.06), flags=FAR)                     # the bumper


def chassis(m, y0, y1, zf, axles, r, half, dual):
    m.box(DARK, (-0.45, y0, zf - 0.28), (0.45, y1, zf - 0.08), flags=FAR)
    wheels(m, axles, r, half, w=0.26, dual=dual)
    for s in (-1, 1): m.box(RED, (s * 0.7 - 0.1, y0 - 0.02, zf - 0.25), (s * 0.7 + 0.1, y0, zf - 0.12), flags=NEAR)


def bed(m, y0, y1, hw, z):
    """A wooden bed: the floor, boards round it with gaps and iron straps."""
    m.box(WOOD, (-hw, y0, z), (hw, y1, z + 0.1), flags=FAR)
    for k in range(3):
        zz = z + 0.12 + k * 0.2
        for s in (-1, 1): m.box(WOOD, (s * hw - (0.05 if s > 0 else 0), y0, zz), (s * hw + (0 if s > 0 else 0.05), y1, zz + 0.18), flags=FAR)
        m.box(WOOD, (-hw, y0, zz), (hw, y0 + 0.05, zz + 0.18), flags=FAR); m.box(WOOD, (-hw, y1 - 0.05, zz), (hw, y1, zz + 0.18), flags=FAR)
    n = max(2, round((y1 - y0) / 1.2))
    for i in range(n + 1):
        y = y0 + 0.08 + i * (y1 - y0 - 0.16) / n
        for s in (-1, 1): m.box(DARK, (s * (hw + 0.012) - 0.012, y - 0.03, z), (s * (hw + 0.012) + 0.012, y + 0.03, z + 0.72), flags=NEAR)


def zil(m, mat="#55708a", back="bed"):
    L0, L1, W, zf = -3.3, 3.35, 2.4, 1.0
    cab(m, 0.2, L1, W, zf, mat); chassis(m, L0, 1.9, zf, (2.3, -1.6), 0.5, 1.2, (-1.6,))
    if back == "bed": bed(m, L0, 0.05, 1.2, zf + 0.12)
    else:                                                            # the fire engine: lockers, the tank, a ladder on the roof, hose reels
        m.box(RED, (-1.2, L0, zf + 0.05), (1.2, 0.1, zf + 1.45), bevel=0.04, flags=FAR)
        m.box(WHITE, (-1.21, L0, zf + 0.75), (1.21, 0.1, zf + 0.9), flags="_noshadow_r320")
        for y in (-2.6, -1.7, -0.8):
            for s in (-1, 1): m.box(CHROME, (s * 1.205 - 0.005, y - 0.35, zf + 0.12), (s * 1.205 + 0.005, y + 0.35, zf + 0.7), flags=NEAR)
        for s in (-1, 1): m.tube(CHROME, (s * 0.35, L0 + 0.1, zf + 1.6), (s * 0.35, 0.9, zf + 1.6), 0.03, n=5, flags=FAR)
        for i in range(9): m.tube(CHROME, (-0.35, L0 + 0.3 + i * 0.42, zf + 1.6), (0.35, L0 + 0.3 + i * 0.42, zf + 1.6), 0.02, n=4, flags=NEAR)
        m.tube(DARK, (-0.9, L0 + 0.5, zf + 1.6), (-0.9, -0.3, zf + 1.6), 0.12, n=8, flags=FAR)
        m.box(WHITE, (-1.21, 0.4, zf + 0.3), (1.21, 1.6, zf + 0.42), flags="_noshadow_r320")


def gaz53(m, mat, back):
    L0, L1, W, zf = -3.1, 3.2, 2.28, 0.92
    cab(m, 0.25, L1, W, zf, mat, grille=mat if back == "army" else WHITE); chassis(m, L0, 1.8, zf, (2.2, -1.5), 0.46, 1.14, (-1.5,))
    if back == "milk":                                               # a yellow tank lying on cradles, a hatch on top, a ladder at the back
        m.tube("#d8b040", (0, L0 + 0.15, zf + 0.95), (0, 0.05, zf + 0.95), 0.92, n=14, flags=FAR)
        for y in (L0 + 0.15, 0.05): m.tube("#d8b040", (0, y, zf + 0.95), (0, y + (0.12 if y > -1 else -0.12), zf + 0.95), 0.92, 0.6, n=14, flags=FAR)
        for y in (-2.5, -0.5): m.box(DARK, (-0.8, y - 0.08, zf - 0.05), (0.8, y + 0.08, zf + 0.35), flags=FAR)
        m.tube(CHROME, (0, -1.5, zf + 1.85), (0, -1.5, zf + 1.98), 0.28, n=10, flags=NEAR)
        m.box("#3f6f73", (-0.94, -2.4, zf + 0.85), (0.94, -0.6, zf + 1.05), flags="_noshadow_r320")                # the blue band the word is on
    else:                                                            # a box van: bread (doors at the side), a shop (a hatch with a flap)
        col = {"bread": "#c8b890", "lavka": "#5a7a8a"}[back]
        m.box(col, (-1.14, L0, zf + 0.02), (1.14, 0.1, zf + 2.05), bevel=0.05, flags=FAR)
        if back == "bread":
            for y in (-2.55, -1.55, -0.55):
                for s in (-1, 1):
                    m.box(DARK, (s * 1.146 - 0.004, y - 0.42, zf + 0.25), (s * 1.146 + 0.004, y - 0.41, zf + 1.85), flags=NEAR)
                    m.box(CHROME, (s * 1.15 - 0.01, y - 0.05, zf + 1.0), (s * 1.15 + 0.01, y + 0.05, zf + 1.04), flags=NEAR)
            m.box("#a8843c", (-1.146, L0 + 0.2, zf + 1.45), (1.146, -0.1, zf + 1.7), flags="_noshadow_r320")
        else:
            m.box(GLASS, (1.14, -2.4, zf + 0.9), (1.15, -0.7, zf + 1.6), flags="_noshadow_r320")
            m.prism(WHITE, [(1.14, zf + 1.62), (1.9, zf + 1.85), (1.9, zf + 1.89), (1.14, zf + 1.68)], -2.5, -0.6, flags=FAR)   # the flap raised over the counter
            m.box(WOOD, (1.14, -2.4, zf + 0.82), (1.45, -0.7, zf + 0.88), flags=NEAR)
            m.box(WHITE, (-1.146, L0 + 0.2, zf + 1.5), (1.146, -0.1, zf + 1.72), flags="_noshadow_r320")


def gaz66(m):
    """The army's lorry: the cab over the engine, high on four big wheels, a tilt over the bed."""
    K, TILT = "#4a5a3a", "#5e6444"; W, zf = 2.3, 1.15; hw = W / 2
    chassis(m, -2.8, 2.6, zf, (1.75, -1.55), 0.56, 1.15, ())
    m.prism(K, [(1.0, zf - 0.2), (2.75, zf - 0.2), (2.8, zf + 0.75), (2.5, zf + 1.45), (1.0, zf + 1.45)], -hw, hw, axis="x", flags=FAR)
    m.prism(GLASS, [(2.81, zf + 0.8), (2.52, zf + 1.38), (2.5, zf + 1.38), (2.78, zf + 0.8)], -hw + 0.1, hw - 0.1, axis="x", flags="_noshadow_r320")
    m.box(K, (-0.03, 2.5, zf + 0.75), (0.03, 2.82, zf + 1.42), flags=NEAR)
    for s in (-1, 1):
        xs = (s * hw, s * (hw + 0.006)) if s > 0 else (s * (hw + 0.006), s * hw)
        m.prism(GLASS, [(1.3, zf + 0.8), (2.6, zf + 0.8), (2.4, zf + 1.35), (1.3, zf + 1.35)], xs[0], xs[1], axis="x", flags="_noshadow_r320")
    lamps(m, 2.8, zf + 0.25, (-hw + 0.3, hw - 0.3), r=0.1); m.box(DARK, (-hw, 2.78, zf - 0.3), (hw, 2.92, zf - 0.12), flags=FAR)
    m.box(DARK, (-0.5, 2.76, zf + 0.1), (0.5, 2.8, zf + 0.5), flags=NEAR)
    bed(m, -2.8, 0.9, hw, zf + 0.1)
    m.prism(TILT, [(-hw, zf + 0.8), (hw, zf + 0.8), (hw, zf + 1.6), (hw - 0.35, zf + 1.95), (-hw + 0.35, zf + 1.95), (-hw, zf + 1.6)], -2.8, 0.9, flags=FAR)
    m.tube(TYRE, (0, -2.95, zf + 0.5), (0, -2.8, zf + 0.5), 0.5, n=12, flags=FAR)                                    # the spare


def onebox(m, y0, y1, hw, z0, z1, mat, nose=0.35, tail=0.25, round_=0.25):
    m.prism(mat, [(y0 + 0.1, z0), (y1 - 0.15, z0), (y1, z0 + 0.35), (y1, z0 + (z1 - z0) * 0.5), (y1 - nose, z1 - round_), (y1 - nose - round_, z1),
                  (y0 + tail + round_, z1), (y0 + tail * 0.3, z1 - round_), (y0, z0 + 0.3)], -hw, hw, axis="x", flags=FAR)


def loaf(m):
    """The UAZ-452 of the ambulance: one rounded box on four wheels, white with a red band and crosses."""
    hw, z0, z1 = 0.97, 0.42, 2.05
    arch(m, 1.2, 0.38, hw); arch(m, -1.1, 0.38, hw)
    onebox(m, -2.15, 2.2, hw, z0, z1, WHITE, nose=0.5)
    m.prism(GLASS, [(2.2, 1.32), (1.71, 1.78), (1.69, 1.78), (2.17, 1.32)], -hw + 0.1, hw - 0.1, axis="x", flags="_noshadow_r320")
    m.box(WHITE, (-0.03, 1.7, 1.3), (0.03, 2.21, 1.8), flags=NEAR)
    for s in (-1, 1):
        xs = (s * hw, s * (hw + 0.006)) if s > 0 else (s * (hw + 0.006), s * hw)
        for a, b in ((0.95, 1.6), (0.0, 0.8), (-0.95, -0.15)): m.box(GLASS, (xs[0], a, 1.3), (xs[1], b, 1.75), flags="_noshadow_r320")
        m.box(RED, (xs[0], -2.1, 1.0), (xs[1], 2.1, 1.14), flags="_noshadow_r320")
        m.box(RED, (xs[0], -1.75, 1.35), (xs[1], -1.35, 1.47), flags=NEAR); m.box(RED, (xs[0], -1.61, 1.21), (xs[1], -1.49, 1.61), flags=NEAR)
    lamps(m, 2.2, 0.98, (-0.62, 0.62), r=0.1); m.box(DARK, (-hw, 2.18, 0.45), (hw, 2.28, 0.58), flags=FAR)
    m.box(DARK, (-0.4, 2.19, 0.85), (0.4, 2.21, 1.1), flags=NEAR)
    for s in (-1, 1): m.box(RED, (s * 0.7 - 0.08, -2.16, 0.8), (s * 0.7 + 0.08, -2.14, 1.0), flags=NEAR)
    wheels(m, (1.2, -1.1), 0.38, hw)


def laz(m):
    """The LAZ-695: a rounded bus, white above a coloured skirt, glass along the edge of the roof, the engine at the back."""
    hw, z0, z1, y0, y1 = 1.25, 0.45, 2.95, -4.6, 4.6
    arch(m, 2.6, 0.5, hw); arch(m, -2.3, 0.5, hw)
    onebox(m, y0, y1, hw, z0, z1, WHITE, nose=0.45, tail=0.5, round_=0.4)
    for s in (-1, 1):
        xs = (s * hw, s * (hw + 0.008)) if s > 0 else (s * (hw + 0.008), s * hw)
        m.box(BODY, (xs[0], y0 + 0.15, z0 + 0.02), (xs[1], y1 - 0.1, 1.45), flags="_noshadow_r320")                # the skirt in the colour of the route
        m.box(CHROME, (xs[0], y0 + 0.15, 1.45), (xs[1] + s * 0.004, y1 - 0.1, 1.49), flags=NEAR)
        for i in range(7): m.box(GLASS, (xs[0], y0 + 0.75 + i * 1.12, 1.62), (xs[1], y0 + 1.75 + i * 1.12, 2.35), flags="_noshadow_r320")
        for i in range(6): m.box("#3a5a6a rough 0.2", (s * (hw - 0.12) - 0.01, y0 + 1.0 + i * 1.12, 2.62), (s * (hw - 0.12) + 0.01, y0 + 1.9 + i * 1.12, 2.8), flags=NEAR)   # the tinted lights in the roof
        if s > 0:
            for y in (3.2, -3.3): m.box(DARK, (xs[0], y - 0.4, z0 + 0.1), (xs[1] + 0.004, y + 0.4, 2.4), flags=NEAR); m.box(GLASS, (xs[1], y - 0.3, 1.55), (xs[1] + 0.008, y + 0.3, 2.3), flags=NEAR)
    m.prism(GLASS, [(y1 + 0.005, 1.7), (y1 - 0.42, 2.5), (y1 - 0.44, 2.5), (y1 - 0.02, 1.7)], -hw + 0.12, hw - 0.12, axis="x", flags="_noshadow_r320")
    m.box(WHITE, (-0.03, y1 - 0.44, 1.7), (0.03, y1 + 0.01, 2.5), flags=NEAR)
    m.box(BODY, (-hw + 0.02, y1 - 0.02, z0 + 0.3), (hw - 0.02, y1 + 0.012, 1.45), flags="_noshadow_r320")
    lamps(m, y1 + 0.012, 1.05, (-0.85, 0.85), r=0.11); m.box(CHROME, (-hw, y1 - 0.02, z0 + 0.02), (hw, y1 + 0.1, z0 + 0.18), flags=FAR)
    m.box(DARK, (-0.5, y1 + 0.012, 2.52), (0.5, y1 + 0.03, 2.72), flags=NEAR)                                      # the board with the route
    m.prism(WHITE, [(y0 + 0.9, z1), (y0 + 1.5, z1), (y0 + 1.5, z1 + 0.12), (y0 + 0.2, z1 - 0.25)], -0.45, 0.45, axis="x", flags=FAR)   # the air scoop over the engine
    for i in range(5): m.box(DARK, (-0.7, y0 - 0.01, 0.8 + i * 0.1), (0.7, y0 + 0.02, 0.85 + i * 0.1), flags=NEAR)
    for s in (-1, 1): m.box(RED, (s * 0.95 - 0.1, y0 + 0.02, 1.1), (s * 0.95 + 0.1, y0 + 0.05, 1.3), flags=NEAR)
    wheels(m, (2.6, -2.3), 0.5, hw, w=0.26, dual=(-2.3,))
    m.solid((-hw, y0, 0), (hw, y1, z1))


KINDS = (("car-car", lambda m: sedan(m)),                                                          # VAZ-2101
         ("car-volga", lambda m: sedan(m, L=4.73, W=1.8, zb=0.98, zr=1.48, cowl=0.75, deck=-1.2, r=0.33, axles=(1.5, -1.3))),
         ("car-moskvich", lambda m: sedan(m, L=4.1, W=1.55, zb=0.95, zr=1.47, cowl=0.6, deck=-1.0, axles=(1.3, -1.1), quad=True)),
         ("car-zaz", lambda m: sedan(m, L=3.73, W=1.55, zb=0.9, zr=1.37, cowl=0.85, deck=-0.75, r=0.28, axles=(1.15, -1.0), ears=True)),
         ("car-police", lambda m: sedan(m, stripe="#2c4a7a", beacon=True)),
         ("parked-car", lambda m: sedan(m, lit=False)),
         ("car-truck", lambda m: zil(m)), ("car-fire", lambda m: zil(m, RED, "fire")),
         ("car-milk", lambda m: gaz53(m, "#55708a", "milk")), ("car-bread", lambda m: gaz53(m, "#6a7a8a", "bread")), ("car-lavka", lambda m: gaz53(m, "#5a7a8a", "lavka")),
         ("car-army", gaz66), ("car-ambulance", loaf), ("bus", laz))
only = sys.argv[sys.argv.index("--") + 2:]
for name, build in KINDS:
    if only and name not in only: continue
    m = Model(); build(m); m.save(os.path.join(out, name + ".blend"))
