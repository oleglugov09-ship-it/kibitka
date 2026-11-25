"""Sections of apartment houses in four manners. A section is 16 m along the street (y) and 12 m deep (x from 0, the
facade, to 12); the street is at -x. Sections are set side by side by the game (HOUSE-SECTIONS in lisp/city.lisp).
paint0 is the wall colour the game gives, paint1 the colour of this very section's balconies. The points at_lit_FF_N
are windows the game may light. Run: blender -b --factory-startup --python make_houses.py -- OUTDIR."""
import sys, os, math
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import kit
from kit import Model, arch
kit.PREVIEW.update({"paint0": "#8f897c", "paint1": "#7f8a8c"})   # as they come in the game: a grey-beige wall, a faded balcony

out = sys.argv[sys.argv.index("--") + 1]
WALL, TINT = "paint0", "paint1"
GLASS, FRAME, PLINTH, ROOF, ROOF2, DOOR, SEAM, STONE, TIN = "#2e333b", "#d8d4c4", "#55554f", "#4c4e52", "#5c5e62", "#4a3020", "#6a6c70", "#6e6c68", "#5a4038"


def window(m, y, z, w=1.35, h=1.25, back=False, frame=FRAME, lit=None):
    """A window in the facade (or in the BACK one), its middle at Y and Z: glass in a pale frame."""
    x0, x1, x2 = (12.0, 12.03, 12.045) if back else (0.0, -0.03, -0.045)
    m.box(frame, (min(x0, x1), y - w / 2 - 0.08, z - h / 2 - 0.08), (max(x0, x1), y + w / 2 + 0.08, z + h / 2 + 0.08))
    m.box(GLASS, (min(x1, x2), y - w / 2, z - h / 2), (max(x1, x2), y + w / 2, z + h / 2))
    if lit is not None: m.point("lit_%02d_%d" % lit, (-0.09, y, z))


def body(m, height, plinth=0.8, wall=WALL):
    """The walls of a section HEIGHT high, on a plinth that goes down under the ground for a slope."""
    m.box(PLINTH, (-0.06, -8.0, -4.0), (12.06, 8.0, plinth))
    m.box(wall, (0.0, -8.0, plinth), (12.0, 8.0, height))
    m.solid((0.0, -8.0, 0.0), (12.0, 8.0, 2.0))


def entrance(m, y, canopy=True, z=0.0):
    m.box(DOOR, (-0.05, y - 0.6, z), (0.0, y + 0.6, z + 2.1))
    m.box(FRAME, (-0.03, y - 0.7, z), (0.0, y + 0.7, z + 2.2))
    m.box(STONE, (-1.3, y - 1.1, z - 0.2), (0.0, y + 1.1, z + 0.12))
    if canopy: m.box(SEAM, (-1.5, y - 1.3, z + 2.45), (0.0, y + 1.3, z + 2.57))


def khrushchyovka(m):                                             # five low floors, a gabled roof, small balconies
    fl, n = 2.8, 5; h = fl * n
    body(m, h)
    for k in range(1, n): m.box(SEAM, (-0.015, -8.0, k * fl - 0.02), (0.0, 8.0, k * fl + 0.02))
    for y in (-4.8, -1.6, 1.6, 4.8): m.box(SEAM, (-0.015, y - 0.02, 0.8), (0.0, y + 0.02, h))
    for k in range(n):
        z = k * fl + 1.75
        for i, y in enumerate((6.4, 3.2, -3.2, -6.4)):
            window(m, y, z, lit=(k, i)); window(m, y, z, back=True)
        if k > 0: window(m, 0.0, z - 1.4, w=0.9, h=0.9)             # the stair windows sit between the floors
        if k > 0:
            for y in (3.2, -3.2):
                m.box(SEAM, (-0.95, y - 1.25, k * fl + 0.02), (0.0, y + 1.25, k * fl + 0.14))
                m.box(TINT, (-0.98, y - 1.28, k * fl + 0.14), (-0.92, y + 1.28, k * fl + 1.15))
                for s in (-1.28, 1.22): m.box(TINT, (-0.98, y + s, k * fl + 0.14), (0.0, y + s + 0.06, k * fl + 1.15))
    entrance(m, 0.0)
    m.box(SEAM, (-0.25, -8.0, h), (12.25, 8.0, h + 0.18))
    m.prism(ROOF, [(-0.3, h + 0.18), (12.3, h + 0.18), (6.0, h + 2.3)], -8.0, 8.0)
    for y in (-5.0, 3.0): m.box(STONE, (5.0, y, h + 1.6), (5.7, y + 0.7, h + 3.0))        # the chimneys of the vents


def brezhnevka(m, n):                                             # nine or twelve floors of panels, loggias, a flat roof
    fl = 2.8; h = fl * n
    body(m, h)
    for k in range(1, n): m.box(SEAM, (-0.015, -8.0, k * fl - 0.025), (0.0, 8.0, k * fl + 0.025))
    for y in (-8.0, -4.8, -1.6, 1.6, 4.8): m.box(SEAM, (-0.015, y - 0.025, 0.8), (0.0, y + 0.025, h))
    for y in (3.2, -3.2):                                         # the loggias: fins from the ground up, a parapet on every floor
        for s in (-1.42, 1.3): m.box(WALL, (-1.15, y + s, 0.8), (0.0, y + s + 0.12, h))
        for k in range(1, n):
            m.box(SEAM, (-1.15, y - 1.3, k * fl - 0.08), (0.0, y + 1.3, k * fl + 0.06))
            m.box(TINT, (-1.17, y - 1.3, k * fl + 0.06), (-1.09, y + 1.3, k * fl + 1.1))
    for k in range(n):
        z = k * fl + 1.75
        for i, y in enumerate((6.4, 3.2, -3.2, -6.4)):
            window(m, y, z, lit=(k, i)); window(m, y, z, back=True)
        if k > 0: window(m, 0.0, z - 1.4, w=1.1, h=1.0)
        window(m, 0.0, z, back=True, w=1.1, h=1.0)
    entrance(m, 0.0)
    m.box(ROOF, (-0.2, -8.0, h), (12.2, 8.0, h + 0.5))
    m.box(ROOF2, (4.0, -2.2, h + 0.5), (8.2, 2.2, h + 2.7))          # the lift's machine room
    m.box(ROOF, (3.9, -2.3, h + 2.7), (8.3, 2.3, h + 2.85))


def stalinka(m):                                                  # five tall floors: a rusticated base, pilasters, a heavy cornice
    fl, n = 3.4, 5; h = fl * n
    body(m, h, plinth=1.0)
    m.box(STONE, (-0.12, -8.0, 1.0), (0.0, 8.0, fl + 0.5))            # the ground floor in bands of stone
    for k in range(5): m.box(PLINTH, (-0.13, -8.0, 1.25 + k * 0.52), (-0.1, 8.0, 1.31 + k * 0.52))
    m.box(FRAME, (-0.3, -8.0, fl + 0.5), (0.0, 8.0, fl + 0.72))        # the belt over it
    for y in (-8.0, -4.8, -1.6, 1.6, 4.8, 8.0):                      # pilasters through three floors, with bases and capitals
        y0, y1 = max(y - 0.3, -8.0), min(y + 0.3, 8.0)
        m.box(FRAME, (-0.14, y0, fl + 0.72), (0.0, y1, 4 * fl + 0.3))
        m.box(FRAME, (-0.2, max(y - 0.38, -8.0), 4 * fl + 0.3), (0.0, min(y + 0.38, 8.0), 4 * fl + 0.55))
        m.box(FRAME, (-0.2, max(y - 0.38, -8.0), fl + 0.72), (0.0, min(y + 0.38, 8.0), fl + 0.95))
    for k in range(n):
        z = k * fl + 2.05
        for i, y in enumerate((6.4, 3.2, -3.2, -6.4)):
            window(m, y, z, w=1.35, h=1.9, lit=(k, i)); window(m, y, z, w=1.35, h=1.9, back=True)
            if k == 1: m.prism(FRAME, [(y - 0.95, z + 1.08), (y + 0.95, z + 1.08), (y, z + 1.5)], -0.1, 0.0, axis="x")     # a little pediment
            if k in (2, 3): m.box(FRAME, (-0.1, y - 0.9, z + 1.06), (0.0, y + 0.9, z + 1.18))
        if k > 0: window(m, 0.0, z, w=1.1, h=1.9); window(m, 0.0, z, w=1.1, h=1.9, back=True)
    for y in (3.2, -3.2):                                         # balconies with balusters on the third floor
        z = 2 * fl
        m.box(FRAME, (-1.0, y - 1.3, z - 0.02), (0.0, y + 1.3, z + 0.16))
        m.box(FRAME, (-1.02, y - 1.32, z + 0.95), (-0.9, y + 1.32, z + 1.05))
        for i in range(9): m.tube(FRAME, (-0.96, y - 1.2 + i * 0.3, z + 0.16), (-0.96, y - 1.2 + i * 0.3, z + 0.95), 0.045, 0.03, n=6, caps=False)
        for s in (-1.3, 1.24): m.box(TINT, (-1.0, y + s, z + 0.16), (0.0, y + s + 0.06, z + 1.0))
    # the archway into the yard
    m.prism(GLASS, arch(0.0, 1.9, 1.5, 0.0, n=12)[:13] + [(-1.5, 0.0), (1.5, 0.0)], -0.14, -0.02, axis="x")
    m.prism(FRAME, arch(0.0, 1.9, 1.75, 1.5, n=12) , -0.18, 0.0, axis="x")
    for s in (-1.75, 1.5): m.box(FRAME, (-0.18, s, 0.0), (0.0, s + 0.25, 1.9))
    # the cornice in three steps, the attic wall and a tin roof with four slopes
    for i, (o, z0, z1) in enumerate(((0.25, h - 0.1, h + 0.2), (0.5, h + 0.2, h + 0.45), (0.75, h + 0.45, h + 0.62))):
        m.box(FRAME, (-o, -8.0, z0), (12.0 + o, 8.0, z1))
    m.box(WALL, (0.0, -8.0, h + 0.62), (12.0, 8.0, h + 1.3))
    m.prism(TIN, [(-0.2, h + 1.3), (12.2, h + 1.3), (8.0, h + 3.4), (4.0, h + 3.4)], -8.0, 8.0)
    for y in (-6.0, 5.2): m.box(STONE, (5.6, y, h + 3.0), (6.4, y + 0.8, h + 4.4))


def constructivist(m):                                            # four floors: ribbon windows, a glazed round stair, a flat roof with rails
    fl, n = 3.0, 4; h = fl * n
    body(m, h, plinth=0.5)
    for k in range(n):
        z = k * fl + 1.75
        for s0, s1 in ((-7.4, -1.9), (1.9, 7.4)):                   # the ribbons: one long strip of glass with thin posts
            m.box(FRAME, (-0.03, s0 - 0.1, z - 0.72), (0.0, s1 + 0.1, z + 0.72))
            m.box(GLASS, (-0.045, s0, z - 0.62), (-0.03, s1, z + 0.62))
            for i in range(1, 4): m.box(FRAME, (-0.06, s0 + i * (s1 - s0) / 4 - 0.04, z - 0.62), (-0.045, s0 + i * (s1 - s0) / 4 + 0.04, z + 0.62))
            m.box(FRAME, (12.0, s0 - 0.1, z - 0.72), (12.03, s1 + 0.1, z + 0.72)); m.box(GLASS, (12.03, s0, z - 0.62), (12.045, s1, z + 0.62))
        for i, y in enumerate((6.0, 3.3, -3.3, -6.0)): m.point("lit_%02d_%d" % (k, i), (-0.09, y, z))
    # the stair: half a cylinder standing out of the facade, glazed in a tall strip, a little higher than the roof
    m.lathe(WALL, (0.0, 0.0), [(1.75, 0.5), (1.75, h + 1.2)], n=14, a0=math.pi / 2, a1=3 * math.pi / 2)
    m.lathe(PLINTH, (0.0, 0.0), [(1.8, -4.0), (1.8, 0.5)], n=14, a0=math.pi / 2, a1=3 * math.pi / 2)
    m.lathe(GLASS, (0.0, 0.0), [(1.77, 2.6), (1.77, h + 0.3)], n=6, a0=math.pi * 0.8, a1=math.pi * 1.2)
    for k in range(1, n + 1): m.lathe(FRAME, (0.0, 0.0), [(1.79, k * fl - 0.2), (1.79, k * fl - 0.08)], n=6, a0=math.pi * 0.8, a1=math.pi * 1.2)
    m.lathe(ROOF, (0.0, 0.0), [(1.85, h + 1.2), (1.85, h + 1.32), (0.0, h + 1.32)], n=14, a0=math.pi / 2, a1=3 * math.pi / 2)
    m.box(DOOR, (-1.8, -0.55, 0.5), (-1.72, 0.55, 2.6)); m.box(SEAM, (-2.6, -1.6, 2.7), (-1.6, 1.6, 2.8))
    for y in (-1.4, 1.4): m.tube(SEAM, (-2.5, y, 0.4), (-2.5, y, 2.7), 0.05, n=8)
    m.box(STONE, (-2.7, -1.7, 0.2), (0.0, 1.7, 0.5))
    for y in (-6.5, 6.5):                                         # round windows by the corners of the ground floor
        pass
    # the flat roof: a low parapet and a pipe rail, a pergola frame over one end
    m.box(SEAM, (-0.1, -8.0, h), (12.1, 8.0, h + 0.35))
    for x in (0.25, 11.75):
        m.tube(ROOF2, (x, -8.0, h + 1.15), (x, 8.0, h + 1.15), 0.035, n=6)
        for y in (-7.8, -4.0, 0.0 if x > 6 else -2.2, 4.0, 7.8): m.tube(ROOF2, (x, y, h + 0.35), (x, y, h + 1.15), 0.03, n=6, caps=False)
    for y in (2.6, 4.8, 7.0):
        m.box(ROOF2, (2.0, y - 0.06, h + 2.6), (10.0, y + 0.06, h + 2.72))
        for x in (2.2, 9.8): m.box(ROOF2, (x - 0.06, y - 0.06, h + 0.35), (x + 0.06, y + 0.06, h + 2.6))


for name, build in (("house-section-5", khrushchyovka), ("house-section-9", lambda m: brezhnevka(m, 9)), ("house-section-12", lambda m: brezhnevka(m, 12)),
                    ("house-stalin-5", stalinka), ("house-konstr-4", constructivist)):
    m = Model(); build(m); m.save(os.path.join(out, name + ".blend"))
