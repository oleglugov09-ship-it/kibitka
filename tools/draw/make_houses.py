"""Sections of apartment houses, thirteen kinds in four manners. A section is 16 m along the street (y) and 12 m deep
(x from 0, the street facade, to 12, the yard facade); the street is at -x. The game sets sections side by side
(HOUSE-SECTIONS in lisp/city.lisp), so the two ends of a section are only seen at the ends of a house.
paint0 is the wall colour the game gives, paint1 the colour of this very section's balconies. The points
at_lit_FF_N are street windows the game may light. As in life, the entrances are in the yard and the balconies look
at the street. Run: blender -b --factory-startup --python make_houses.py -- OUTDIR."""
import sys, os, math
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import kit
from kit import Model, arch
kit.PREVIEW.update({"paint0": "#8f897c", "paint1": "#7f8a8c"})   # as they come in the game: a grey-beige wall, a faded balcony

out = sys.argv[sys.argv.index("--") + 1]
WALL, TINT = "paint0", "paint1"
GLASS, FRAME, PLINTH, ROOF, ROOF2, DOOR, SEAM, STONE, TIN, WOOD, LOG = "#2e333b", "#d8d4c4", "#55554f", "#4c4e52", "#5c5e62", "#4a3020", "#6a6c70", "#6e6c68", "#5a4038", "#6a5038", "#7a5c3a"
COLS = (6.4, 3.2, -3.2, -6.4)


def slab(m, mat, side, u0, u1, z0, z1, d0, d1, bevel=0.0):
    """A box on a facade: from U0 to U1 along it, Z0 to Z1 up, standing out of the wall from D0 to D1 (metres).
    SIDE is "front" (the street), "back" (the yard), "left" (the end at y -8) or "right" (the end at y +8)."""
    if side == "front": lo, hi = (-d1, u0, z0), (-d0, u1, z1)
    elif side == "back": lo, hi = (12.0 + d0, u0, z0), (12.0 + d1, u1, z1)
    elif side == "left": lo, hi = (u0, -8.0 - d1, z0), (u1, -8.0 - d0, z1)
    else: lo, hi = (u0, 8.0 + d0, z0), (u1, 8.0 + d1, z1)
    m.box(mat, lo, hi, bevel=bevel)


def window(m, side, u, z, w=1.35, h=1.25, frame=FRAME, lit=None):
    """A window on the facade SIDE, its middle at U along the facade and Z up: glass in a pale frame."""
    slab(m, frame, side, u - w / 2 - 0.08, u + w / 2 + 0.08, z - h / 2 - 0.08, z + h / 2 + 0.08, 0.0, 0.03)
    slab(m, GLASS, side, u - w / 2, u + w / 2, z - h / 2, z + h / 2, 0.03, 0.045)
    if lit is not None: m.point("lit_%02d_%d" % lit, (-0.09, u, z))


def body(m, height, plinth=0.8, wall=WALL, x1=12.0):
    """The walls of a section HEIGHT high, on a plinth that goes down under the ground for a slope."""
    m.box(PLINTH, (-0.06, -8.0, -4.0), (x1 + 0.06, 8.0, plinth))
    m.box(wall, (0.0, -8.0, plinth), (x1, 8.0, height))
    m.solid((0.0, -8.0, 0.0), (x1, 8.0, 2.0))


def entrance(m, side, u, z=0.0, canopy=True, w=1.2, porch=1.3):
    """A door with its step and, with CANOPY, a little roof over it."""
    slab(m, FRAME, side, u - w / 2 - 0.1, u + w / 2 + 0.1, z, z + 2.2, 0.0, 0.03)
    slab(m, DOOR, side, u - w / 2, u + w / 2, z, z + 2.1, 0.03, 0.05)
    slab(m, STONE, side, u - w / 2 - 0.5, u + w / 2 + 0.5, z - 0.3, z + 0.1, 0.0, porch)
    if canopy:
        slab(m, SEAM, side, u - w / 2 - 0.7, u + w / 2 + 0.7, z + 2.45, z + 2.57, 0.0, porch + 0.2)
        for s in (-1, 1): slab(m, SEAM, side, u + s * (w / 2 + 0.6) - 0.04, u + s * (w / 2 + 0.6) + 0.04, z + 0.1, z + 2.45, porch, porch + 0.08)


def balcony(m, side, u, z, w=2.5, deep=0.95, mat=TINT, rail=1.0):
    """A balcony: a slab and a parapet of the section's own colour."""
    slab(m, SEAM, side, u - w / 2, u + w / 2, z, z + 0.12, 0.0, deep)
    slab(m, mat, side, u - w / 2 - 0.02, u + w / 2 + 0.02, z + 0.12, z + rail + 0.12, deep - 0.06, deep)
    for s in (-1, 1): slab(m, mat, side, u + s * (w / 2 - 0.02) - 0.03, u + s * (w / 2 - 0.02) + 0.03, z + 0.12, z + rail + 0.12, 0.0, deep)


def seams(m, side, height, floor, n, ys=(-4.8, -1.6, 1.6, 4.8), z0=0.8):
    """The joints between panels: a line under every floor and a line between the rooms."""
    for k in range(1, n): slab(m, SEAM, side, -8.0 if side in ("front", "back") else 0.0, 8.0 if side in ("front", "back") else 12.0, k * floor - 0.02, k * floor + 0.02, 0.0, 0.015)
    for y in ys: slab(m, SEAM, side, y - 0.02, y + 0.02, z0, height, 0.0, 0.015)


def downpipe(m, side, u, height):
    slab(m, ROOF2, side, u - 0.06, u + 0.06, 0.3, height, 0.02, 0.14)


def vents(m, height, ys=(-5.0, 3.0), x=5.0):
    for y in ys: m.box(STONE, (x, y, height), (x + 0.7, y + 0.7, height + 1.3))


def end_windows(m, floors, floor, z_first, us=(3.0, 9.0), w=1.35, h=1.25):
    """Windows in both ends of a section: seen where a house ends."""
    for side in ("left", "right"):
        for k in range(floors):
            for u in us: window(m, side, u, k * floor + z_first, w=w, h=h)


# ------------------------------------------------------------------ the panels of the sixties and after

def khrushchyovka(m):                                             # five low floors of panels under a gable roof
    fl, n = 2.8, 5; h = fl * n
    body(m, h)
    seams(m, "front", h, fl, n); seams(m, "back", h, fl, n)
    for k in range(n):
        z = k * fl + 1.75
        for i, y in enumerate(COLS):
            window(m, "front", y, z, lit=(k, i)); window(m, "back", y, z)
        if k > 0:
            window(m, "back", 0.0, z - 1.4, w=0.9, h=0.9)             # the stair windows sit between the floors, over the door
            for y in (3.2, -3.2): balcony(m, "front", y, k * fl + 0.02)
        window(m, "front", 0.0, z, w=0.9, h=1.25)                   # the kitchens in the middle
    entrance(m, "back", 0.0)
    downpipe(m, "back", 7.7, h); downpipe(m, "front", -7.7, h)
    m.box(SEAM, (-0.25, -8.0, h), (12.25, 8.0, h + 0.18))
    m.prism(ROOF, [(-0.3, h + 0.18), (12.3, h + 0.18), (6.0, h + 2.3)], -8.0, 8.0)
    vents(m, h + 1.4)


def brick_five(m):                                                # the same five floors in brick: a flat roof, balconies in pairs, a shop corner
    fl, n = 2.8, 5; h = fl * n
    body(m, h, plinth=1.0)
    for k in range(1, n): slab(m, FRAME, "front", -8.0, 8.0, k * fl - 0.06, k * fl + 0.06, 0.0, 0.04)     # pale bands between the floors
    for k in range(n):
        z = k * fl + 1.8
        for i, y in enumerate(COLS):
            window(m, "front", y, z, lit=(k, i)); window(m, "back", y, z)
            slab(m, FRAME, "front", y - 0.85, y + 0.85, z + 0.74, z + 0.86, 0.0, 0.05)                    # a lintel of pale brick
        window(m, "front", 0.0, z, w=1.0); window(m, "back", 0.0, z - 1.4 if k else z, w=0.9, h=0.9)
        if k > 0:
            balcony(m, "front", 4.8, k * fl + 0.02, w=4.6, deep=0.9)
            balcony(m, "back", -4.8, k * fl + 0.02, w=2.4, deep=0.85)
    entrance(m, "back", 0.0)
    end_windows(m, n, fl, 1.8)
    m.box(FRAME, (-0.15, -8.0, h), (12.15, 8.0, h + 0.3))
    m.box(WALL, (0.0, -8.0, h + 0.3), (12.0, 8.0, h + 0.9))
    m.box(ROOF, (0.25, -7.75, h + 0.6), (11.75, 7.75, h + 0.7))
    vents(m, h + 0.7, ys=(-4.0, 4.0), x=7.0)


def brezhnevka_9(m):                                              # nine floors: loggias in two strips, the stair glazed in the yard
    fl, n = 2.8, 9; h = fl * n
    body(m, h)
    seams(m, "front", h, fl, n, ys=(-8.0, -4.8, -1.6, 1.6, 4.8)); seams(m, "back", h, fl, n)
    for y in (3.2, -3.2):                                         # the loggias: fins from the ground up, a parapet on every floor
        for s in (-1.42, 1.3): m.box(WALL, (-1.15, y + s, 0.8), (0.0, y + s + 0.12, h))
        for k in range(1, n):
            m.box(SEAM, (-1.15, y - 1.3, k * fl - 0.08), (0.0, y + 1.3, k * fl + 0.06))
            m.box(TINT, (-1.17, y - 1.3, k * fl + 0.06), (-1.09, y + 1.3, k * fl + 1.1))
    for k in range(n):
        z = k * fl + 1.75
        for i, y in enumerate(COLS):
            window(m, "front", y, z, lit=(k, i)); window(m, "back", y, z)
        window(m, "front", 0.0, z, w=1.0)
        if k > 0: slab(m, GLASS, "back", -0.7, 0.7, k * fl - 0.9, k * fl + 1.3, 0.03, 0.05)        # the stair: a strip of glass up the yard side
    slab(m, FRAME, "back", -0.85, 0.85, 2.4, h - 0.3, 0.0, 0.03)
    slab(m, WALL, "back", -1.9, 1.9, 0.8, h + 0.5, 0.0, 0.5)        # the lift shaft stands out of the yard wall
    entrance(m, "back", 0.0, porch=1.9)
    slab(m, SEAM, "back", 2.6, 4.2, 0.0, 2.3, 0.0, 1.2)             # the rubbish chute's little room
    m.box(ROOF, (-0.2, -8.0, h), (12.2, 8.0, h + 0.5))
    m.box(ROOF2, (6.5, -2.2, h + 0.5), (11.6, 2.2, h + 2.7)); m.box(ROOF, (6.4, -2.3, h + 2.7), (11.7, 2.3, h + 2.85))


def ribbon_9(m):                                                  # nine floors with balconies in long bands: the "ship"
    fl, n = 2.8, 9; h = fl * n
    body(m, h)
    seams(m, "back", h, fl, n)
    for k in range(n):
        z = k * fl + 1.75
        for i, y in enumerate((6.2, 2.2, -2.2, -6.2)):
            window(m, "front", y, z, w=2.2, h=1.3, lit=(k, i)); window(m, "back", COLS[i], z)
        if k > 0:
            slab(m, SEAM, "front", -8.0, 8.0, k * fl, k * fl + 0.12, 0.0, 1.1)
            slab(m, TINT if k % 2 else FRAME, "front", -8.0, 8.0, k * fl + 0.12, k * fl + 1.12, 1.03, 1.1)
            window(m, "back", 0.0, z - 1.4, w=1.0, h=1.0)
    for y in (-8.0, -4.2, 0.0, 4.2):
        m.box(WALL, (-1.1, y, 0.8), (0.0, y + 0.12, h)) if y > -8.0 else m.box(WALL, (-1.1, -8.0, 0.8), (0.0, -7.88, h))
    m.box(WALL, (-1.1, 7.88, 0.8), (0.0, 8.0, h))
    entrance(m, "back", 0.0, porch=1.6)
    m.box(ROOF, (-1.2, -8.0, h), (12.2, 8.0, h + 0.45))
    m.box(ROOF2, (7.0, -2.0, h + 0.45), (11.0, 2.0, h + 2.4))


def brezhnevka_12(m):                                             # twelve floors: a bay in the middle, coloured panels under the windows, a shop below
    fl, n = 2.8, 12; h = fl * n
    body(m, h)
    m.box(WALL, (-0.9, -2.6, 2.8), (0.0, 2.6, h))                   # the bay
    seams(m, "back", h, fl, n)
    for k in range(n):
        z = k * fl + 1.75
        for i, y in enumerate(COLS):
            window(m, "front", y, z, lit=(k, i) if k > 0 else None) if k > 0 else None
            window(m, "back", y, z)
            if k > 0: slab(m, TINT, "front", y - 0.9, y + 0.9, z - 1.55, z - 0.75, 0.0, 0.04)      # the panel under the window
        if k > 0:
            m.box(FRAME, (-0.93, -1.9, z - 0.7), (-0.9, 1.9, z + 0.7)); m.box(GLASS, (-0.945, -1.8, z - 0.6), (-0.93, 1.8, z + 0.6))
            window(m, "back", 0.0, z - 1.4, w=1.0, h=1.0)
    for i, y in enumerate(COLS): m.point("lit_00_%d" % i, (-0.09, y, 1.6))
    slab(m, GLASS, "front", -7.4, 7.4, 0.9, 2.5, 0.0, 0.05)        # the shop: one long window with piers, a canopy over the pavement
    for y in (-7.5, -3.7, 0.0, 3.7, 7.5): slab(m, STONE, "front", y - 0.2, y + 0.2, 0.0, 2.7, 0.0, 0.12)
    slab(m, SEAM, "front", -8.0, 8.0, 2.7, 2.85, 0.0, 1.6)
    for y in (-7.9, 7.78): m.box(WALL, (-0.5, y, 2.85), (0.0, y + 0.12, h))
    for k in range(1, n):
        for y in (-6.6, 6.6): balcony(m, "front", y, k * fl + 0.02, w=2.5, deep=0.5) if k % 3 == 1 else None
    slab(m, WALL, "back", -1.9, 1.9, 0.8, h + 0.5, 0.0, 0.5)
    entrance(m, "back", 0.0, porch=1.9)
    m.box(ROOF, (-1.0, -8.0, h), (12.2, 8.0, h + 0.5))
    m.box(ROOF2, (6.5, -2.4, h + 0.5), (11.6, 2.4, h + 3.0)); m.box(ROOF, (6.4, -2.5, h + 3.0), (11.7, 2.5, h + 3.15))


def tower_16(m):                                                  # sixteen floors, one section standing alone: windows all round, balconies at the corners
    fl, n = 2.8, 16; h = fl * n
    body(m, h)
    for k in range(n):
        z = k * fl + 1.75
        for i, y in enumerate(COLS):
            window(m, "front", y, z, lit=(k, i)); window(m, "back", y, z)
        for side in ("left", "right"):
            for u in (2.4, 6.0, 9.6): window(m, side, u, z)
        if k > 0:
            for y in (-6.4, 6.4): balcony(m, "front", y, k * fl + 0.02, w=2.9, deep=0.9, mat=TINT if k % 2 else FRAME)
            window(m, "back", 0.0, z - 1.4, w=1.0, h=1.0)
    for k in range(4, n, 4): m.box(SEAM, (-0.05, -8.05, k * fl - 0.12), (12.05, 8.05, k * fl + 0.12))     # a belt every four floors
    slab(m, WALL, "back", -2.0, 2.0, 0.8, h + 0.6, 0.0, 0.6)
    entrance(m, "back", 0.0, porch=2.0); entrance(m, "front", 0.0, canopy=True)
    m.box(ROOF, (-0.25, -8.25, h), (12.25, 8.25, h + 0.6))
    m.box(ROOF2, (3.5, -3.0, h + 0.6), (8.5, 3.0, h + 3.2)); m.box(ROOF, (3.4, -3.1, h + 3.2), (8.6, 3.1, h + 3.36))
    m.tube(ROOF2, (6.0, 0.0, h + 3.3), (6.0, 0.0, h + 7.5), 0.06, 0.02, n=6)


# ------------------------------------------------------------------ before the panels

def cornice(m, h, steps=((0.25, -0.1, 0.2), (0.5, 0.2, 0.45), (0.75, 0.45, 0.62)), mat=FRAME):
    for o, z0, z1 in steps: m.box(mat, (-o, -8.0, h + z0), (12.0 + o, 8.0, h + z1))


def rustic(m, top, mat=STONE):
    """The ground floor in bands of stone, on the street."""
    slab(m, mat, "front", -8.0, 8.0, 1.0, top, 0.0, 0.12)
    k = 0
    while 1.25 + k * 0.52 < top - 0.1:
        slab(m, PLINTH, "front", -8.0, 8.0, 1.25 + k * 0.52, 1.31 + k * 0.52, 0.1, 0.13); k += 1


def stalinka(m):                                                  # five tall floors: a rusticated base, pilasters, a heavy cornice, an archway
    fl, n = 3.4, 5; h = fl * n
    body(m, h, plinth=1.0)
    rustic(m, fl + 0.5)
    slab(m, FRAME, "front", -8.0, 8.0, fl + 0.5, fl + 0.72, 0.0, 0.3)
    for y in (-8.0, -4.8, -1.6, 1.6, 4.8, 8.0):                      # pilasters through three floors, with bases and capitals
        y0, y1 = max(y - 0.3, -8.0), min(y + 0.3, 8.0)
        slab(m, FRAME, "front", y0, y1, fl + 0.72, 4 * fl + 0.3, 0.0, 0.14)
        for z0 in (4 * fl + 0.3, fl + 0.72): slab(m, FRAME, "front", max(y - 0.38, -8.0), min(y + 0.38, 8.0), z0, z0 + 0.24, 0.0, 0.2)
    for k in range(n):
        z = k * fl + 2.05
        for i, y in enumerate(COLS):
            window(m, "front", y, z, h=1.9, lit=(k, i)); window(m, "back", y, z, h=1.7)
            if k == 1: m.prism(FRAME, [(y - 0.95, z + 1.08), (y + 0.95, z + 1.08), (y, z + 1.5)], -0.1, 0.0, axis="x")
            if k in (2, 3): slab(m, FRAME, "front", y - 0.9, y + 0.9, z + 1.06, z + 1.18, 0.0, 0.1)
        if k > 0: window(m, "front", 0.0, z, w=1.1, h=1.9)
        window(m, "back", 0.0, z - 1.7 if k else z, w=1.0, h=1.4)
    for y in (3.2, -3.2):                                         # balconies with balusters on the third floor
        z = 2 * fl
        m.box(FRAME, (-1.0, y - 1.3, z - 0.02), (0.0, y + 1.3, z + 0.16)); m.box(FRAME, (-1.02, y - 1.32, z + 0.95), (-0.9, y + 1.32, z + 1.05))
        for i in range(9): m.tube(FRAME, (-0.96, y - 1.2 + i * 0.3, z + 0.16), (-0.96, y - 1.2 + i * 0.3, z + 0.95), 0.045, 0.03, n=6, caps=False)
    m.prism(GLASS, arch(0.0, 1.9, 1.5, 0.0, n=12)[:13] + [(-1.5, 0.0), (1.5, 0.0)], -0.14, -0.02, axis="x")     # the archway into the yard
    m.prism(FRAME, arch(0.0, 1.9, 1.75, 1.5, n=12), -0.18, 0.0, axis="x")
    for s in (-1.75, 1.5): m.box(FRAME, (-0.18, s, 0.0), (0.0, s + 0.25, 1.9))
    m.prism(GLASS, arch(0.0, 1.9, 1.5, 0.0, n=12)[:13] + [(-1.5, 0.0), (1.5, 0.0)], 12.02, 12.1, axis="x")
    for k in range(1, n):                                          # the yard: plain balconies of iron, back doors, pipes
        for y in (-3.2, 3.2): balcony(m, "back", y, k * fl + 0.6, w=2.2, deep=0.8, mat=ROOF2, rail=0.9) if k in (1, 3) else None
    for y in (-5.6, 5.6): entrance(m, "back", y, canopy=True, w=1.0, porch=1.0)
    downpipe(m, "back", -7.6, h); downpipe(m, "back", 7.6, h); downpipe(m, "front", 7.75, h)
    end_windows(m, n, fl, 2.05, h=1.7)
    cornice(m, h)
    m.box(WALL, (0.0, -8.0, h + 0.62), (12.0, 8.0, h + 1.3))
    m.prism(TIN, [(-0.2, h + 1.3), (12.2, h + 1.3), (8.0, h + 3.4), (4.0, h + 3.4)], -8.0, 8.0)
    vents(m, h + 3.0, ys=(-6.0, 5.2), x=5.6)


def stalinka_bay(m):                                              # four floors with two bays the height of the house and a gable between them
    fl, n = 3.3, 4; h = fl * n
    body(m, h, plinth=1.0)
    rustic(m, fl + 0.4)
    for y in (-4.6, 4.6):                                         # the bays: three-sided, glazed, with a little balcony on top
        m.box(WALL, (-0.9, y - 1.2, 1.0), (0.0, y + 1.2, h - fl + 0.3))
        m.box(FRAME, (-1.0, y - 1.3, h - fl + 0.3), (0.0, y + 1.3, h - fl + 0.5))
        m.box(FRAME, (-1.0, y - 1.3, h - fl + 1.3), (-0.92, y + 1.3, h - fl + 1.4))
        for i in range(8): m.tube(FRAME, (-0.96, y - 1.2 + i * 0.34, h - fl + 0.5), (-0.96, y - 1.2 + i * 0.34, h - fl + 1.3), 0.045, 0.03, n=6, caps=False)
        for k in range(n - 1):
            z = k * fl + 2.0
            m.box(FRAME, (-0.93, y - 0.85, z - 0.95), (-0.9, y + 0.85, z + 0.95)); m.box(GLASS, (-0.945, y - 0.75, z - 0.85), (-0.93, y + 0.75, z + 0.85))
            m.point("lit_%02d_%d" % (k, 0 if y > 0 else 3), (-0.99, y, z))
        window(m, "front", y, (n - 1) * fl + 2.0, h=1.8, lit=(n - 1, 0 if y > 0 else 3))
    for k in range(n):
        z = k * fl + 2.0
        for i, y in ((1, 1.4), (2, -1.4)): window(m, "front", y, z, w=1.1, h=1.8, lit=(k, i))
        for y in (-7.0, 7.0): window(m, "front", y, z, w=1.0, h=1.8)
        for y in COLS: window(m, "back", y, z, h=1.6)
        window(m, "back", 0.0, z - 1.6 if k else z, w=1.0, h=1.3)
    slab(m, FRAME, "front", -8.0, 8.0, fl + 0.4, fl + 0.6, 0.0, 0.25)
    entrance(m, "front", 0.0, canopy=False, w=1.4); m.prism(FRAME, arch(0.0, 2.3, 1.0, 0.8, n=10), -0.2, 0.0, axis="x")
    entrance(m, "back", 0.0, w=1.0, porch=1.0)
    end_windows(m, n, fl, 2.0, h=1.6)
    cornice(m, h, steps=((0.2, -0.1, 0.15), (0.45, 0.15, 0.4)))
    m.prism(TIN, [(-0.4, h + 0.4), (12.4, h + 0.4), (6.0, h + 3.3)], -8.0, 8.0)
    m.prism(WALL, [(-2.6, h + 0.4), (2.6, h + 0.4), (0.0, h + 2.9)], -0.05, 0.6, axis="x")       # the gable over the door
    m.prism(GLASS, arch(0.0, h + 1.2, 0.55, 0.0, 0, 2 * math.pi, n=14)[:15], -0.08, -0.05, axis="x")
    m.prism(TIN, [(-3.0, h + 0.4), (0.0, h + 3.2), (3.0, h + 0.4), (2.6, h + 0.4), (0.0, h + 2.9), (-2.6, h + 0.4)], -0.25, 0.7, axis="x")
    vents(m, h + 2.4, ys=(-6.4, 5.6), x=5.6)


def stalinka_tall(m):                                             # eight floors and a tower with a spire: the one tall house of the main street
    fl, n = 3.3, 8; h = fl * n
    body(m, h, plinth=1.0)
    rustic(m, 2 * fl + 0.3)
    slab(m, FRAME, "front", -8.0, 8.0, 2 * fl + 0.3, 2 * fl + 0.6, 0.0, 0.35)
    for y in (-8.0, -4.8, -1.6, 1.6, 4.8, 8.0):
        slab(m, FRAME, "front", max(y - 0.3, -8.0), min(y + 0.3, 8.0), 2 * fl + 0.6, 7 * fl + 0.2, 0.0, 0.16)
        slab(m, FRAME, "front", max(y - 0.4, -8.0), min(y + 0.4, 8.0), 7 * fl + 0.2, 7 * fl + 0.5, 0.0, 0.24)
    for k in range(n):
        z = k * fl + 2.0
        for i, y in enumerate(COLS):
            window(m, "front", y, z, h=1.8, lit=(k, i)); window(m, "back", y, z, h=1.6)
            if k == 2: m.prism(FRAME, [(y - 0.95, z + 1.02), (y + 0.95, z + 1.02), (y, z + 1.45)], -0.1, 0.0, axis="x")
        if k > 1: window(m, "front", 0.0, z, w=1.1, h=1.8)
        window(m, "back", 0.0, z - 1.6 if k else z, w=1.0, h=1.3)
    for z0 in (3 * fl, 5 * fl):
        for y in (3.2, -3.2):
            m.box(FRAME, (-1.0, y - 1.3, z0 - 0.02), (0.0, y + 1.3, z0 + 0.16)); m.box(FRAME, (-1.02, y - 1.32, z0 + 0.95), (-0.9, y + 1.32, z0 + 1.05))
            for i in range(7): m.tube(FRAME, (-0.96, y - 1.2 + i * 0.4, z0 + 0.16), (-0.96, y - 1.2 + i * 0.4, z0 + 0.95), 0.045, 0.03, n=6, caps=False)
    m.prism(GLASS, arch(0.0, 3.0, 1.7, 0.0, n=12)[:13] + [(-1.7, 0.0), (1.7, 0.0)], -0.14, -0.02, axis="x")     # a tall portal
    m.prism(FRAME, arch(0.0, 3.0, 2.0, 1.7, n=12), -0.22, 0.0, axis="x")
    for s in (-2.0, 1.7): m.box(FRAME, (-0.22, s, 0.0), (0.0, s + 0.3, 3.0))
    for y in (-5.6, 5.6): entrance(m, "back", y, w=1.0, porch=1.0)
    downpipe(m, "back", -7.6, h); downpipe(m, "back", 7.6, h)
    end_windows(m, n, fl, 2.0, h=1.6)
    cornice(m, h, steps=((0.3, -0.1, 0.25), (0.6, 0.25, 0.55), (0.9, 0.55, 0.75)))
    m.box(WALL, (0.0, -8.0, h + 0.75), (12.0, 8.0, h + 1.6))
    for y in (-7.6, -5.2, 5.2, 7.6): m.pyramid(FRAME, (0.0, y - 0.3), (0.6, y + 0.3), h + 1.6, (0.3, y, h + 2.9))     # little obelisks on the attic
    # the tower: two stages, a clock face, a spire with a star
    m.box(WALL, (2.5, -3.5, h + 1.6), (9.5, 3.5, h + 8.0)); m.box(FRAME, (2.2, -3.8, h + 8.0), (9.8, 3.8, h + 8.4))
    for y in (-2.0, 0.0, 2.0): m.box(GLASS, (2.44, y - 0.5, h + 3.0), (2.5, y + 0.5, h + 6.6))
    m.box(WALL, (4.0, -2.0, h + 8.4), (8.0, 2.0, h + 12.0)); m.box(FRAME, (3.8, -2.2, h + 12.0), (8.2, 2.2, h + 12.3))
    m.disc(FRAME, (4.0, 0.0, h + 10.3), 1.1, 0.06, n=20); m.disc("#c9a13a", (3.94, 0.0, h + 10.3), 0.95, 0.03, n=20)
    m.pyramid(TIN, (3.9, -2.1), (8.1, 2.1), h + 12.3, (6.0, 0.0, h + 19.0))
    m.tube("#c9a13a", (6.0, 0.0, h + 18.8), (6.0, 0.0, h + 21.5), 0.08, 0.02, n=8)


def constructivist(m):                                            # four floors: ribbon windows, a glazed round stair, a flat roof with rails
    fl, n = 3.0, 4; h = fl * n
    body(m, h, plinth=0.5)
    for k in range(n):
        z = k * fl + 1.75
        for s0, s1 in ((-7.4, -1.9), (1.9, 7.4)):                   # the ribbons: one long strip of glass with thin posts
            slab(m, FRAME, "front", s0 - 0.1, s1 + 0.1, z - 0.72, z + 0.72, 0.0, 0.03); slab(m, GLASS, "front", s0, s1, z - 0.62, z + 0.62, 0.03, 0.045)
            for i in range(1, 4): slab(m, FRAME, "front", s0 + i * (s1 - s0) / 4 - 0.04, s0 + i * (s1 - s0) / 4 + 0.04, z - 0.62, z + 0.62, 0.045, 0.06)
            slab(m, FRAME, "back", s0 - 0.1, s1 + 0.1, z - 0.62, z + 0.62, 0.0, 0.03); slab(m, GLASS, "back", s0, s1, z - 0.52, z + 0.52, 0.03, 0.045)
        for i, y in enumerate((6.0, 3.3, -3.3, -6.0)): m.point("lit_%02d_%d" % (k, i), (-0.09, y, z))
        if k > 0: balcony(m, "back", 0.0, k * fl + 0.02, w=3.2, deep=1.2, mat=ROOF2, rail=0.9)     # galleries in the yard
        for side in ("left", "right"): window(m, side, 6.0, z, w=3.0, h=1.2)
    m.lathe(WALL, (0.0, 0.0), [(1.75, 0.5), (1.75, h + 1.2)], n=14, a0=math.pi / 2, a1=3 * math.pi / 2)        # the stair: half a cylinder, glazed
    m.lathe(PLINTH, (0.0, 0.0), [(1.8, -4.0), (1.8, 0.5)], n=14, a0=math.pi / 2, a1=3 * math.pi / 2)
    m.lathe(GLASS, (0.0, 0.0), [(1.77, 2.6), (1.77, h + 0.3)], n=6, a0=math.pi * 0.8, a1=math.pi * 1.2)
    for k in range(1, n + 1): m.lathe(FRAME, (0.0, 0.0), [(1.79, k * fl - 0.2), (1.79, k * fl - 0.08)], n=6, a0=math.pi * 0.8, a1=math.pi * 1.2)
    m.lathe(ROOF, (0.0, 0.0), [(1.85, h + 1.2), (1.85, h + 1.32), (0.0, h + 1.32)], n=14, a0=math.pi / 2, a1=3 * math.pi / 2)
    m.box(DOOR, (-1.8, -0.55, 0.5), (-1.72, 0.55, 2.6)); m.box(SEAM, (-2.6, -1.6, 2.7), (-1.6, 1.6, 2.8))
    for y in (-1.4, 1.4): m.tube(SEAM, (-2.5, y, 0.4), (-2.5, y, 2.7), 0.05, n=8)
    m.box(STONE, (-2.7, -1.7, 0.2), (0.0, 1.7, 0.5))
    entrance(m, "back", 0.0, z=0.5, w=1.0, porch=1.2)
    m.box(SEAM, (-0.1, -8.0, h), (12.1, 8.0, h + 0.35))
    for x in (0.25, 11.75):
        m.tube(ROOF2, (x, -8.0, h + 1.15), (x, 8.0, h + 1.15), 0.035, n=6)
        for y in (-7.8, -4.0, 0.0 if x > 6 else -2.2, 4.0, 7.8): m.tube(ROOF2, (x, y, h + 0.35), (x, y, h + 1.15), 0.03, n=6, caps=False)
    for y in (2.6, 4.8, 7.0):
        m.box(ROOF2, (2.0, y - 0.06, h + 2.6), (10.0, y + 0.06, h + 2.72))
        for x in (2.2, 9.8): m.box(ROOF2, (x - 0.06, y - 0.06, h + 0.35), (x + 0.06, y + 0.06, h + 2.6))


def commune(m):                                                   # five floors on piers: the ground floor open, a block of balconies, a glazed corner
    fl, n = 3.0, 5; h = fl * n
    m.box(PLINTH, (-0.06, -8.0, -4.0), (12.06, 8.0, 0.3))
    m.box(WALL, (3.0, -8.0, 0.3), (12.0, 8.0, fl)); m.box(WALL, (0.0, -8.0, fl), (12.0, 8.0, h))       # the house steps back at the ground
    for y in (-7.4, -3.7, 0.0, 3.7, 7.4): m.tube(FRAME, (0.5, y, 0.3), (0.5, y, fl), 0.22, n=10)        # round piers under it
    m.solid((0.0, -8.0, 0.0), (12.0, 8.0, 2.0))
    for k in range(1, n):
        z = k * fl + 1.7
        slab(m, FRAME, "front", -7.4, 2.2, z - 0.7, z + 0.7, 0.0, 0.03); slab(m, GLASS, "front", -7.3, 2.1, z - 0.6, z + 0.6, 0.03, 0.045)
        for i in range(1, 6): slab(m, FRAME, "front", -7.3 + i * 1.57 - 0.04, -7.3 + i * 1.57 + 0.04, z - 0.6, z + 0.6, 0.045, 0.06)
        balcony(m, "front", 5.4, k * fl + 0.02, w=4.6, deep=1.3, mat=TINT, rail=0.95)
        window(m, "front", 5.4, z, w=2.4, h=1.5)
        for i, y in enumerate((5.4, 0.0, -3.0, -6.0)): m.point("lit_%02d_%d" % (k, i), (-0.09, y, z))
        slab(m, FRAME, "back", -7.4, 7.4, z - 0.6, z + 0.6, 0.0, 0.03); slab(m, GLASS, "back", -7.3, 7.3, z - 0.5, z + 0.5, 0.03, 0.045)
        for side in ("left", "right"): window(m, side, 6.0, z, w=3.4, h=1.2)
    for i, y in enumerate(COLS): m.point("lit_00_%d" % i, (2.9, y, 1.7))
    m.box(GLASS, (2.95, -7.0, 0.6), (3.0, 7.0, 2.6))                # the ground floor behind the piers: all glass
    entrance(m, "back", -4.0, z=0.3, w=1.0, porch=1.1); entrance(m, "back", 4.0, z=0.3, w=1.0, porch=1.1)
    m.box(SEAM, (-0.1, -8.0, h), (12.1, 8.0, h + 0.3))
    m.box(WALL, (7.0, -8.0, h + 0.3), (12.0, -2.0, h + 2.8)); m.box(SEAM, (6.9, -8.1, h + 2.8), (12.1, -1.9, h + 2.95))     # the roof room
    m.box(GLASS, (6.95, -7.4, h + 1.0), (7.0, -2.6, h + 2.3))
    m.tube(ROOF2, (0.3, -8.0, h + 1.1), (0.3, 8.0, h + 1.1), 0.035, n=6)
    for y in (-7.8, -4.0, 0.0, 4.0, 7.8): m.tube(ROOF2, (0.3, y, h + 0.3), (0.3, y, h + 1.1), 0.03, n=6, caps=False)


def low_two(m):                                                   # two floors in plaster under a steep roof: built by prisoners of war, loved ever since
    fl, n = 3.0, 2; h = fl * n
    body(m, h, plinth=0.7)
    for k in range(n):
        z = k * fl + 1.9
        for i, y in enumerate(COLS):
            window(m, "front", y, z, w=1.2, h=1.5, lit=(k, i)); window(m, "back", y, z, w=1.2, h=1.5)
            slab(m, FRAME, "front", y - 0.85, y + 0.85, z + 0.85, z + 1.0, 0.0, 0.08)
    slab(m, FRAME, "front", -8.0, 8.0, fl - 0.1, fl + 0.1, 0.0, 0.08)
    for y in (-8.0, 7.6): slab(m, FRAME, "front", y, y + 0.4, 0.7, h, 0.0, 0.08)                      # the corners in pale bands
    m.box(WALL, (-1.2, -1.4, 0.7), (0.0, 1.4, h + 0.2))                                              # the porch stands forward, with its own gable
    m.prism(TIN, [(-1.9, h + 0.2), (0.0, h + 2.0), (1.9, h + 0.2), (1.5, h + 0.2), (0.0, h + 1.65), (-1.5, h + 0.2)], -1.4, 0.3, axis="x")
    m.prism(WALL, [(-1.5, h + 0.2), (1.5, h + 0.2), (0.0, h + 1.65)], -1.2, -1.1, axis="x")
    m.box(DOOR, (-1.25, -0.6, 0.4), (-1.2, 0.6, 2.6)); m.box(STONE, (-2.2, -1.2, 0.1), (-1.2, 1.2, 0.4))
    m.box(GLASS, (-1.245, -0.5, fl + 1.2), (-1.2, 0.5, fl + 2.6))
    entrance(m, "back", 0.0, z=0.3, w=1.0, porch=1.0)
    end_windows(m, n, fl, 1.9, w=1.2, h=1.5)
    cornice(m, h, steps=((0.3, -0.05, 0.15),))
    m.prism(TIN, [(-0.5, h + 0.15), (12.5, h + 0.15), (6.0, h + 4.6)], -8.0, 8.0)
    for y in (-4.8, 4.8):                                         # dormers
        m.box(WALL, (1.6, y - 0.8, h + 1.2), (3.2, y + 0.8, h + 2.5)); m.box(GLASS, (1.56, y - 0.5, h + 1.5), (1.6, y + 0.5, h + 2.3))
        m.prism(TIN, [(y - 1.0, h + 2.5), (y, h + 3.2), (y + 1.0, h + 2.5)], 1.4, 3.6, axis="x")
    vents(m, h + 3.6, ys=(-6.0, 5.2), x=5.6)


def wooden_two(m):                                                # a two-storey timber house of the thirties: logs, carved frames, a board fence of a porch
    fl, n = 2.9, 2; h = fl * n
    m.box(STONE, (-0.06, -8.0, -4.0), (12.06, 8.0, 0.6))
    m.box(LOG, (0.0, -8.0, 0.6), (12.0, 8.0, h))
    m.solid((0.0, -8.0, 0.0), (12.0, 8.0, 2.0))
    for side in ("front", "back"):
        for k in range(18): slab(m, DOOR, side, -8.0, 8.0, 0.75 + k * 0.29, 0.79 + k * 0.29, 0.0, 0.02)    # the seams between the logs
    for k in range(n):
        z = k * fl + 1.9
        for i, y in enumerate(COLS):
            window(m, "front", y, z, w=1.1, h=1.4, lit=(k, i)); window(m, "back", y, z, w=1.1, h=1.4)
            slab(m, FRAME, "front", y - 0.8, y + 0.8, z + 0.78, z + 1.0, 0.0, 0.07)                   # the carved head of the frame
            slab(m, FRAME, "front", y - 0.75, y + 0.75, z - 0.95, z - 0.82, 0.0, 0.07)
    m.box(WOOD, (-1.6, -1.5, 0.3), (0.0, 1.5, h - 0.3))                                              # the stair porch, boarded
    m.box(GLASS, (-1.64, -0.9, fl + 1.2), (-1.6, 0.9, fl + 2.3)); m.box(DOOR, (-1.65, -0.5, 0.3), (-1.6, 0.5, 2.4))
    m.prism(TIN, [(-1.8, h - 0.3), (0.0, h + 0.9), (1.8, h - 0.3)], -1.75, 0.2, axis="x")
    entrance(m, "back", 0.0, z=0.3, w=1.0, porch=1.0, canopy=False)
    end_windows(m, n, fl, 1.9, w=1.1, h=1.4)
    m.box(WOOD, (-0.35, -8.3, h), (12.35, 8.3, h + 0.14))
    m.prism(TIN, [(-0.5, h + 0.14), (12.5, h + 0.14), (8.5, h + 3.0), (3.5, h + 3.0)], -8.0, 8.0)
    vents(m, h + 2.4, ys=(-6.0, -0.4, 5.2), x=5.6)


KINDS = (("house-section-5", khrushchyovka), ("house-brick-5", brick_five), ("house-section-9", brezhnevka_9), ("house-ribbon-9", ribbon_9),
         ("house-section-12", brezhnevka_12), ("house-tower-16", tower_16), ("house-stalin-5", stalinka), ("house-stalin-4", stalinka_bay),
         ("house-stalin-8", stalinka_tall), ("house-konstr-4", constructivist), ("house-konstr-5", commune), ("house-low-2", low_two),
         ("house-wood-2", wooden_two))
only = sys.argv[sys.argv.index("--") + 2:]
for name, build in KINDS:
    if only and name not in only: continue
    m = Model(); build(m); m.save(os.path.join(out, name + ".blend"))
