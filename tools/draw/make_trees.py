"""Trees of their true height. A pine of the bor is twenty metres of bare trunk with the crown at the top; a spruce is
a dark spire to the ground; a birch leans, white with black marks; a lime is a broad dome. paint0 is the colour of
the needles or leaves, which the game gives by the season; the -bare ones are what the leafy trees are in winter.
Run: blender -b --factory-startup --python make_trees.py -- OUTDIR"""
import sys, os, math, random
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import kit
from kit import Model
kit.PREVIEW.update({"paint0": "#2a5232"})

out = sys.argv[sys.argv.index("--") + 1]
LEAF, BARK, PINE_BARK, PINE_TOP, BIRCH, MARK, LIME_BARK = "paint0", "#3a2a1e", "#4a3424", "#9a5a34", "#e0dcd0", "#2a2a2a", "#4a3a2c"


def pine(m, seed, h=21.0, lean=0.4):
    rnd = random.Random(seed)
    top = (lean * rnd.uniform(-1, 1), lean * rnd.uniform(-1, 1), h)
    mid = (top[0] * 0.5, top[1] * 0.5, h * 0.55)
    m.tube(PINE_BARK, (0, 0, -0.5), mid, 0.34, 0.24, n=7)            # dark below, orange above, as pines are
    m.tube(PINE_TOP, mid, top, 0.24, 0.1, n=7)
    for k in range(5):                                            # the stubs of dead branches down the trunk
        z = h * (0.28 + 0.07 * k); a = rnd.uniform(0, 6.28); l = rnd.uniform(0.7, 1.5)
        m.tube(PINE_BARK, (mid[0] * z / mid[2] * 0.9, mid[1] * z / mid[2] * 0.9, z), (math.cos(a) * l, math.sin(a) * l, z + 0.3), 0.05, 0.02, n=4, caps=False)
    n = 7
    for k in range(n):                                            # the crown: flat lumps round the top third, the biggest in the middle
        f = k / (n - 1); z = h * (0.66 + 0.34 * f)
        r = (1.0 - abs(f - 0.45) * 1.4) * rnd.uniform(2.8, 3.8) + 0.8
        a = rnd.uniform(0, 6.28); off = rnd.uniform(0.6, 2.4) * (1 - f * 0.8)
        cx = top[0] * (z / h) + math.cos(a) * off; cy = top[1] * (z / h) + math.sin(a) * off
        m.blob(LEAF, (cx, cy, z), (r, r, r * 0.5), seed=seed * 31 + k, rough=0.22)
        if k < n - 2: m.tube(PINE_TOP, (top[0] * (z / h), top[1] * (z / h), z - 0.4), (cx, cy, z), 0.07, 0.03, n=4, caps=False)


def spruce(m, seed, h=15.0):
    rnd = random.Random(seed)
    m.tube(BARK, (0, 0, -0.5), (0, 0, h * 0.9), 0.26, 0.05, n=6)
    tiers = 8
    for k in range(tiers):                                        # skirts of branches, each a little smaller and steeper than the one below
        f = k / (tiers - 1); z0 = h * (0.1 + 0.8 * f); r = (3.3 - 2.9 * f) * rnd.uniform(0.9, 1.1); drop = 1.2 + 1.2 * (1 - f)
        m.lathe(LEAF, (rnd.uniform(-.1, .1), rnd.uniform(-.1, .1)), [(r, z0 - drop * 0.25), (r * 0.55, z0 + drop * 0.3), (0.12, z0 + drop * 1.1)], n=9, smooth=False)
        m.lathe(LEAF, (0, 0), [(0.12, z0 - drop * 0.1), (r, z0 - drop * 0.25)], n=9, smooth=False, inside=True)
    m.lathe(LEAF, (0, 0), [(0.5, h * 0.9), (0.0, h + 0.6)], n=6, smooth=False)


def birch(m, seed, h=15.0, leaves=True):
    rnd = random.Random(seed)
    lean = (rnd.uniform(-1.2, 1.2), rnd.uniform(-1.2, 1.2))
    pts = [(0, 0, -0.5), (lean[0] * 0.3, lean[1] * 0.3, h * 0.4), (lean[0] * 0.8, lean[1] * 0.8, h * 0.75), (lean[0], lean[1], h * 0.97)]
    radii = [0.2, 0.15, 0.09, 0.03]
    for (a, b), (r0, r1) in zip(zip(pts, pts[1:]), zip(radii, radii[1:])): m.tube(BIRCH, a, b, r0, r1, n=6)
    for k in range(7):                                            # the black marks
        f = rnd.uniform(0.03, 0.55); z = h * f; a = rnd.uniform(0, 6.28); r = 0.2 - 0.09 * f / 0.55
        cx, cy = lean[0] * 0.3 * (z / (h * 0.4)), lean[1] * 0.3 * (z / (h * 0.4))
        m.box(MARK, (cx + math.cos(a) * r - 0.07, cy + math.sin(a) * r - 0.07, z), (cx + math.cos(a) * r + 0.07, cy + math.sin(a) * r + 0.07, z + rnd.uniform(0.08, 0.25)))
    for k in range(6):                                            # branches, and on them the hanging clouds of leaves
        f = 0.45 + 0.09 * k; z = h * f; a = k * 2.4 + rnd.uniform(-0.4, 0.4); l = rnd.uniform(1.6, 3.0) * (1.15 - f * 0.6)
        bx, by = lean[0] * f, lean[1] * f; ex, ey, ez = bx + math.cos(a) * l, by + math.sin(a) * l, z + l * 0.55
        m.tube(BIRCH, (bx, by, z), (ex, ey, ez), 0.06, 0.02, n=4, caps=False)
        if leaves:                                                # three small lumps, each hanging lower than the last
            for j in range(3):
                t = 1.0 - j * 0.3
                m.blob(LEAF, (bx + (ex - bx) * t + rnd.uniform(-.3, .3), by + (ey - by) * t + rnd.uniform(-.3, .3), ez - 0.3 - j * 0.75), (1.05 - j * 0.12, 1.05 - j * 0.12, 1.25), seed=seed * 17 + k * 3 + j, rough=0.28)
        else:
            for j in range(3): m.tube(MARK, (ex, ey, ez), (ex + rnd.uniform(-1, 1), ey + rnd.uniform(-1, 1), ez - rnd.uniform(0.6, 1.6)), 0.02, 0.008, n=3, caps=False)
    if leaves:
        for j in range(3): m.blob(LEAF, (lean[0] + rnd.uniform(-.5, .5), lean[1] + rnd.uniform(-.5, .5), h * (0.97 - j * 0.07)), (1.0 + j * 0.15, 1.0 + j * 0.15, 1.2), seed=seed * 17 + 90 + j, rough=0.28)


def lime(m, seed, h=12.5, leaves=True):
    rnd = random.Random(seed)
    m.tube(LIME_BARK, (0, 0, -0.5), (0, 0, h * 0.42), 0.36, 0.26, n=7)
    forks = []
    for k in range(5):
        a = k * 1.26 + rnd.uniform(-0.3, 0.3); l = rnd.uniform(2.2, 3.4)
        end = (math.cos(a) * l, math.sin(a) * l, h * rnd.uniform(0.62, 0.8)); forks.append(end)
        m.tube(LIME_BARK, (0, 0, h * 0.4), end, 0.17, 0.06, n=5, caps=False)
    m.tube(LIME_BARK, (0, 0, h * 0.4), (0.2, 0.1, h * 0.86), 0.2, 0.05, n=5, caps=False)
    if leaves:
        for k, e in enumerate(forks):                             # a dome of many lumps: two on every bough, a ring higher up, the cap
            for j in range(2):
                t = 0.7 + j * 0.45
                m.blob(LEAF, (e[0] * t + rnd.uniform(-.4, .4), e[1] * t + rnd.uniform(-.4, .4), e[2] + 0.2 + j * 0.5 + rnd.uniform(-.4, .4)), (1.7, 1.7, 1.45), seed=seed * 13 + k * 2 + j, rough=0.26)
            m.blob(LEAF, (e[0] * 0.5, e[1] * 0.5, h * 0.9 + rnd.uniform(-.3, .3)), (1.6, 1.6, 1.4), seed=seed * 13 + 40 + k, rough=0.26)
        m.blob(LEAF, (0.2, 0.1, h * 1.0), (1.8, 1.8, 1.4), seed=seed * 13 + 7, rough=0.26)
    else:
        for e in forks + [(0.2, 0.1, h * 0.86)]:
            for j in range(4): m.tube(LIME_BARK, e, (e[0] + rnd.uniform(-1.8, 1.8), e[1] + rnd.uniform(-1.8, 1.8), e[2] + rnd.uniform(0.6, 2.0)), 0.035, 0.01, n=3, caps=False)


for name, build in (("tree-pine-1", lambda m: pine(m, 1, 21.0)), ("tree-pine-2", lambda m: pine(m, 2, 24.0, 0.9)), ("tree-pine-3", lambda m: pine(m, 3, 17.5, 0.6)),
                    ("tree-spruce-1", lambda m: spruce(m, 1, 15.0)), ("tree-spruce-2", lambda m: spruce(m, 2, 11.0)),
                    ("tree-birch-1", lambda m: birch(m, 1, 15.0)), ("tree-birch-2", lambda m: birch(m, 2, 12.5)),
                    ("tree-birch-1-bare", lambda m: birch(m, 1, 15.0, False)), ("tree-birch-2-bare", lambda m: birch(m, 2, 12.5, False)),
                    ("tree-lime-1", lambda m: lime(m, 1, 12.5)), ("tree-lime-2", lambda m: lime(m, 2, 10.0)),
                    ("tree-lime-1-bare", lambda m: lime(m, 1, 12.5, False)), ("tree-lime-2-bare", lambda m: lime(m, 2, 10.0, False))):
    m = Model(); build(m); m.save(os.path.join(out, name + ".blend"))
