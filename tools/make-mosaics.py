#!/usr/bin/env python3
"""Make the mosaics for the end walls of the houses → project/textures/murals/atlas.png.

Every house end in every town gets a mosaic of its own, so the atlas holds TOWNS × PER_TOWN cells of
W×H tiles, one pixel per tile (the seams and the shade of each tile are drawn by the shader in
lisp/city.lisp). A mosaic is put together from a background, a horizon, a hero and a frame; no two cells
share the same combination, and each town leans towards the heroes that suit its character
(*TOWN-THEMES* in lisp/places.lisp). Keep COLS, ROWS, W, H and PER_TOWN in step with *MOSAIC-GRID* there.
Usage: tools/make-mosaics.py"""
import math, pathlib, random
from PIL import Image, ImageDraw

OUT = pathlib.Path(__file__).resolve().parent.parent / "project" / "textures" / "murals" / "atlas.png"
W, H, S = 48, 64, 4                      # tiles across and down; S is the supersampling of the drawing
TOWNS, PER_TOWN, COLS, ROWS = 12, 40, 24, 20

def rgb(h): return tuple(int(h[i:i + 2], 16) for i in (1, 3, 5))

C = {k: rgb(v) for k, v in dict(
    sky="#5a8ab0", cream="#e8dcc0", gold="#d8a030", red="#b03a2c", blue="#2c4a7a", teal="#3f7f83", green="#4a7a48",
    dark="#2a2c34", brown="#7a4a30", white="#f0ece0", orange="#e07a30", pink="#d98a8a", lilac="#7a6aa8",
    sand="#d8c088", night="#1e2a4a", mint="#8ac0a0").items()}

# background top, background low, hero, accent
PALETTES = [("sky", "cream", "red", "gold"), ("blue", "sky", "gold", "white"), ("orange", "cream", "dark", "red"),
            ("blue", "teal", "white", "gold"), ("sky", "gold", "brown", "red"), ("night", "blue", "gold", "white"),
            ("red", "orange", "cream", "gold"), ("teal", "cream", "red", "dark"), ("brown", "orange", "gold", "cream"),
            ("cream", "sand", "red", "blue"), ("lilac", "pink", "white", "gold"), ("green", "mint", "gold", "red"),
            ("teal", "mint", "white", "orange"), ("sand", "cream", "blue", "red"), ("night", "lilac", "gold", "pink"),
            ("sky", "mint", "white", "red"), ("gold", "cream", "red", "blue"), ("pink", "cream", "lilac", "red"),
            ("dark", "blue", "orange", "cream"), ("mint", "cream", "teal", "red")]

class Pen:
    """Draw in tile units on a canvas S times larger."""
    def __init__(self):
        self.img = Image.new("RGB", (W * S, H * S)); self.d = ImageDraw.Draw(self.img)
    def poly(self, pts, c): self.d.polygon([(x * S, y * S) for x, y in pts], fill=c)
    def rect(self, x0, y0, x1, y1, c): self.d.rectangle([x0 * S, y0 * S, x1 * S, y1 * S], fill=c)
    def oval(self, x, y, rx, ry, c): self.d.ellipse([(x - rx) * S, (y - ry) * S, (x + rx) * S, (y + ry) * S], fill=c)
    def disc(self, x, y, r, c): self.oval(x, y, r, r, c)
    def line(self, pts, c, w=1.0): self.d.line([(x * S, y * S) for x, y in pts], fill=c, width=max(1, int(w * S)))

# ---------------------------------------------------------------- backgrounds

def bg_bands(p, a, b, rng):
    p.rect(0, 0, W, H, a); p.oval(W / 2, H * rng.uniform(.95, 1.15), W * .95, H * rng.uniform(.5, .62), b)

def bg_rays(p, a, b, rng):
    p.rect(0, 0, W, H, a); x, y = rng.uniform(8, W - 8), rng.uniform(H * .25, H * .5); n = rng.choice((10, 12, 16))
    for i in range(n):
        t = 2 * math.pi * i / n; u = t + math.pi / n
        p.poly([(x, y), (x + 120 * math.cos(t), y + 120 * math.sin(t)), (x + 120 * math.cos(u), y + 120 * math.sin(u))], b)

def bg_stripes(p, a, b, rng):
    p.rect(0, 0, W, H, a); w = rng.choice((5, 7, 9)); k = rng.choice((-1, 1))
    for i in range(-20, 30, 2):
        p.poly([(i * w, 0), (i * w + w, 0), (i * w + w + k * H * .5, H), (i * w + k * H * .5, H)], b)

def bg_arcs(p, a, b, rng):
    p.rect(0, 0, W, H, a); x = rng.choice((0, W / 2, W)); step = rng.choice((6, 8))
    for k, r in enumerate(range(90, 0, -step)): p.disc(x, H, r, b if k % 2 else a)

def bg_checks(p, a, b, rng):
    p.rect(0, 0, W, H, a); n = rng.choice((6, 8))
    for j in range(0, H, n):
        for i in range(0, W, n):
            if (i // n + j // n) % 2: p.poly([(i + n / 2, j), (i + n, j + n / 2), (i + n / 2, j + n), (i, j + n / 2)], b)

BACKS = dict(bands=bg_bands, rays=bg_rays, stripes=bg_stripes, arcs=bg_arcs, checks=bg_checks)

# ---------------------------------------------------------------- horizons

def hz_none(p, rng): pass

def hz_waves(p, rng):
    y0 = rng.uniform(44, 50); f = rng.uniform(3, 5)
    for k, c in enumerate((C["teal"], C["blue"], C["teal"], C["night"])):
        pts = [(x, y0 + k * 5 + 2 * math.sin(x / f + k * 1.7)) for x in range(-1, W + 2)]
        p.poly(pts + [(W + 1, H + 1), (-1, H + 1)], c)

def hz_mountains(p, rng):
    rock = C[rng.choice(("blue", "lilac", "green", "dark"))]
    for x, h, w in sorted(((rng.uniform(4, W - 4), rng.uniform(18, 32), rng.uniform(12, 18)) for _ in range(3)), key=lambda m: -m[1]):
        p.poly([(x - w, H), (x, H - h), (x + w, H)], rock)
        p.poly([(x - w * .28, H - h * .72), (x, H - h), (x + w * .28, H - h * .72), (x, H - h * .6)], C["white"])

def hz_fields(p, rng):
    cols = [C[c] for c in rng.sample(("green", "gold", "mint", "sand", "brown"), 3)]
    for k in range(4): p.oval(W * rng.uniform(.2, .8), H + 26 - k * 6, W * 1.1, 30 - k * 3, cols[k % 3])

def hz_skyline(p, rng):
    x = -1
    while x < W:
        w = rng.choice((5, 6, 8)); h = rng.uniform(8, 20); c = C[rng.choice(("dark", "night", "brown"))]
        p.rect(x, H - h, x + w, H, c)
        for j in range(int(h // 4)):
            for i in range(int(w // 3)):
                if rng.random() < .6: p.rect(x + 1 + i * 3, H - h + 1.5 + j * 4, x + 2 + i * 3, H - h + 3 + j * 4, C["gold"])
        x += w + 1

def hz_forest(p, rng):
    p.rect(0, H - 6, W, H, C["brown"])
    for x in range(3, W, rng.choice((6, 8))): pine(p, x, H - 6, rng.uniform(4, 6), C["green"], C["brown"])

HORIZONS = dict(none=hz_none, waves=hz_waves, mountains=hz_mountains, fields=hz_fields, skyline=hz_skyline, forest=hz_forest)

# ---------------------------------------------------------------- heroes: (pen, x, y, size, colour, accent)

def pine(p, x, y, s, a, b):
    for k in range(3): p.poly([(x - s * (1.1 - k * .25), y - k * s * .7), (x, y - s * 1.4 - k * s * .7), (x + s * (1.1 - k * .25), y - k * s * .7)], a)
    p.rect(x - s * .15, y, x + s * .15, y + s * .6, b)

def spikes(x, y, r0, r1, n, turn=-math.pi / 2):
    return [(x + math.cos(turn + i * math.pi / n) * (r1 if i % 2 == 0 else r0), y + math.sin(turn + i * math.pi / n) * (r1 if i % 2 == 0 else r0)) for i in range(2 * n)]

def h_sun(p, x, y, s, a, b): p.poly(spikes(x, y, s * .62, s, 12), b); p.disc(x, y, s * .55, a)
def h_star(p, x, y, s, a, b): p.poly(spikes(x, y, s * .42, s, 5), a); p.poly(spikes(x, y, s * .2, s * .5, 5), b)
def h_gear(p, x, y, s, a, b): p.poly(spikes(x, y, s * .72, s, 10), a); p.disc(x, y, s * .75, a); p.disc(x, y, s * .3, b)
def h_moon(p, x, y, s, a, b): p.disc(x, y, s, a); p.disc(x + s * .45, y - s * .2, s * .85, b)

def h_sail(p, x, y, s, a, b):
    p.poly([(x - s, y + s * .5), (x + s, y + s * .5), (x + s * .7, y + s * .9), (x - s * .7, y + s * .9)], b)
    p.rect(x - s * .04, y - s, x + s * .04, y + s * .5, C["dark"])
    p.poly([(x + s * .1, y - s * .9), (x + s * .9, y + s * .35), (x + s * .1, y + s * .35)], a)
    p.poly([(x - s * .1, y - s * .6), (x - s * .8, y + s * .35), (x - s * .1, y + s * .35)], C["cream"])

def h_rocket(p, x, y, s, a, b):
    p.oval(x, y, s * .38, s, a); p.disc(x, y - s * .3, s * .17, b)
    for k in (-1, 1): p.poly([(x + k * s * .3, y + s * .4), (x + k * s * .75, y + s), (x + k * s * .2, y + s * .85)], b)
    p.poly([(x - s * .2, y + s * .95), (x, y + s * 1.5), (x + s * .2, y + s * .95)], C["orange"])

def h_dove(p, x, y, s, a, b):
    p.oval(x, y, s * .8, s * .38, a); p.disc(x + s * .75, y - s * .25, s * .26, a)
    p.poly([(x - s * .2, y), (x - s * .1, y - s), (x + s * .5, y - s * .1)], a); p.poly([(x - s * .7, y), (x - s * 1.3, y - s * .3), (x - s * 1.2, y + s * .3)], a)
    p.poly([(x + s, y - s * .25), (x + s * 1.3, y - s * .15), (x + s, y - s * .1)], b)

def h_wheat(p, x, y, s, a, b):
    p.line([(x, y + s * 1.2), (x, y - s * .8)], b, .6)
    for k in range(6):
        for j in (-1, 1): p.oval(x + j * s * .25, y - s * .8 + k * s * .32, s * .2, s * .14, a)
    p.oval(x, y - s * 1.0, s * .14, s * .22, a)

def h_atom(p, x, y, s, a, b):
    for t in (0, 60, 120):
        e = Image.new("L", p.img.size, 0); ImageDraw.Draw(e).ellipse([(x - s) * S, (y - s * .38) * S, (x + s) * S, (y + s * .38) * S], outline=255, width=int(S * 1.2))
        p.img.paste(a, (0, 0), e.rotate(t, center=(x * S, y * S)))
    p.disc(x, y, s * .22, b)

def h_book(p, x, y, s, a, b):
    p.poly([(x - s, y - s * .4), (x, y - s * .1), (x, y + s * .8), (x - s, y + s * .5)], a); p.poly([(x + s, y - s * .4), (x, y - s * .1), (x, y + s * .8), (x + s, y + s * .5)], C["cream"])
    for k in range(3):
        p.line([(x - s * .85, y - s * .15 + k * s * .25), (x - s * .15, y + s * .1 + k * s * .25)], b, .5); p.line([(x + s * .85, y - s * .15 + k * s * .25), (x + s * .15, y + s * .1 + k * s * .25)], b, .5)

def h_loco(p, x, y, s, a, b):
    p.rect(x - s, y - s * .2, x + s * .5, y + s * .5, a); p.rect(x + s * .5, y - s * .7, x + s, y + s * .5, a); p.rect(x + s * .6, y - s * .55, x + s * .9, y - s * .2, C["cream"])
    p.rect(x - s * .8, y - s * .7, x - s * .55, y - s * .2, C["dark"])
    for k in range(3): p.disc(x - s * .7 + k * s * .7, y + s * .6, s * .26, C["dark"]); p.disc(x - s * .7 + k * s * .7, y + s * .6, s * .1, b)
    for k in range(3): p.disc(x - s * .6 + k * s * .35, y - s * (1.0 + k * .25), s * (.2 + k * .07), C["white"])

def h_fish(p, x, y, s, a, b):
    p.oval(x, y, s, s * .5, a); p.poly([(x - s * .8, y), (x - s * 1.5, y - s * .55), (x - s * 1.5, y + s * .55)], a)
    p.poly([(x - s * .2, y - s * .45), (x + s * .1, y - s * .85), (x + s * .3, y - s * .4)], b); p.disc(x + s * .6, y - s * .1, s * .1, C["dark"])

def h_cat(p, x, y, s, a, b):
    p.oval(x, y + s * .9, s * .75, s * .8, a); p.disc(x, y, s * .62, a)
    for k in (-1, 1):
        p.poly([(x + k * s * .6, y - s * .2), (x + k * s * .58, y - s * .95), (x + k * s * .1, y - s * .5)], a)
        p.disc(x + k * s * .25, y - s * .05, s * .1, C["dark"]); p.line([(x + k * s * .2, y + s * .22), (x + k * s * .9, y + s * .12)], C["dark"], .3)
    p.poly([(x - s * .08, y + s * .12), (x + s * .08, y + s * .12), (x, y + s * .24)], b); p.rect(x - s * .5, y + s * .5, x + s * .5, y + s * .68, b)

def h_truck(p, x, y, s, a, b):
    p.rect(x - s, y - s * .7, x + s * .45, y + s * .3, a); p.poly([(x - s * 1.1, y - s * .7), (x - s * .28, y - s * 1.15), (x + s * .55, y - s * .7)], b)
    p.rect(x - s * .6, y - s * .45, x - s * .1, y, C["gold"]); p.rect(x + s * .45, y - s * .25, x + s, y + s * .3, C["cream"]); p.rect(x + s * .6, y - s * .15, x + s * .9, y + s * .05, C["sky"])
    p.rect(x - s * .1, y - s * 1.45, x + s * .02, y - s * .9, C["dark"])
    for k in (-.6, .65): p.disc(x + k * s, y + s * .4, s * .25, C["dark"]); p.disc(x + k * s, y + s * .4, s * .09, C["cream"])

def h_lighthouse(p, x, y, s, a, b):
    for k in range(5): p.poly([(x - s * (.42 - k * .04), y + s - k * s * .4), (x + s * (.42 - k * .04), y + s - k * s * .4), (x + s * (.38 - k * .04), y + s * .6 - k * s * .4), (x - s * (.38 - k * .04), y + s * .6 - k * s * .4)], a if k % 2 else C["white"])
    p.rect(x - s * .3, y - s * 1.25, x + s * .3, y - s, C["gold"]); p.poly([(x - s * .38, y - s * 1.25), (x, y - s * 1.6), (x + s * .38, y - s * 1.25)], b)
    for k in (-1, 1): p.poly([(x + k * s * .3, y - s * 1.12), (x + k * s * 1.5, y - s * 1.45), (x + k * s * 1.5, y - s * .8)], C["gold"])

def h_house(p, x, y, s, a, b):
    p.rect(x - s * .8, y - s * .1, x + s * .8, y + s, a); p.poly([(x - s, y - s * .05), (x, y - s), (x + s, y - s * .05)], b)
    p.rect(x - s * .25, y + s * .2, x + s * .25, y + s * .65, C["gold"]); p.rect(x + s * .4, y - s * .9, x + s * .6, y - s * .4, C["dark"])

def h_flower(p, x, y, s, a, b):
    p.line([(x, y), (x, y + s * 1.5)], C["green"], .6); p.oval(x + s * .35, y + s, s * .35, s * .15, C["green"])
    for i in range(8): p.disc(x + math.cos(i * math.pi / 4) * s * .6, y + math.sin(i * math.pi / 4) * s * .6, s * .3, a)
    p.disc(x, y, s * .32, b)

def h_kite(p, x, y, s, a, b):
    p.poly([(x, y - s), (x + s * .65, y - s * .1), (x, y + s), (x - s * .65, y - s * .1)], a); p.poly([(x, y - s), (x + s * .65, y - s * .1), (x, y - s * .1)], b)
    pts = [(x + s * .3 * math.sin(k * 1.3), y + s + k * s * .28) for k in range(5)]; p.line(pts, C["dark"], .4)
    for px, py in pts[1:]: p.poly([(px - s * .2, py - s * .1), (px + s * .2, py + s * .1), (px + s * .2, py - s * .1), (px - s * .2, py + s * .1)], b)

def h_pines(p, x, y, s, a, b): pine(p, x - s * .7, y + s * .6, s * .5, a, C["brown"]); pine(p, x + s * .6, y + s * .7, s * .42, a, C["brown"]); pine(p, x, y + s * .5, s * .75, a, C["brown"])

def h_envelope(p, x, y, s, a, b):
    p.rect(x - s, y - s * .6, x + s, y + s * .6, C["cream"]); p.poly([(x - s, y - s * .6), (x, y + s * .15), (x + s, y - s * .6)], a)
    p.line([(x - s, y + s * .6), (x - s * .2, y - s * .05)], a, .4); p.line([(x + s, y + s * .6), (x + s * .2, y - s * .05)], a, .4); p.disc(x, y + s * .12, s * .2, b)

def h_plane(p, x, y, s, a, b):
    p.poly([(x - s, y + s * .1), (x + s * .9, y - s * .15), (x + s * 1.1, y), (x + s * .9, y + s * .15)], a)
    p.poly([(x - s * .1, y), (x - s * .5, y - s * .9), (x + s * .3, y)], b); p.poly([(x - s * .1, y), (x - s * .5, y + s * .9), (x + s * .3, y)], b)
    p.poly([(x - s, y + s * .1), (x - s * 1.2, y - s * .45), (x - s * .7, y)], b)

HEROES = dict(sun=h_sun, star=h_star, gear=h_gear, moon=h_moon, sail=h_sail, rocket=h_rocket, dove=h_dove, wheat=h_wheat,
              atom=h_atom, book=h_book, loco=h_loco, fish=h_fish, cat=h_cat, truck=h_truck, lighthouse=h_lighthouse,
              house=h_house, flower=h_flower, kite=h_kite, pines=h_pines, envelope=h_envelope, plane=h_plane)

# What each town leans to, in the order of *TOWN-THEMES*: heroes, then horizons.
TOWN_TASTE = [
    (("fish", "sail", "dove", "cat"), ("waves", "fields")),                 # river
    (("star", "plane", "rocket", "pines"), ("mountains", "forest")),        # military
    (("gear", "loco", "truck", "sun"), ("skyline", "none")),                # factory
    (("atom", "book", "rocket", "moon"), ("skyline", "none")),              # university
    (("wheat", "house", "flower", "cat"), ("fields", "forest")),            # village
    (("sail", "lighthouse", "fish", "kite"), ("waves", "none")),            # port
    (("star", "dove", "envelope", "sun"), ("skyline", "fields")),           # admin
    (("plane", "moon", "envelope", "pines"), ("mountains", "none")),        # outpost
    (("gear", "sun", "truck", "star"), ("mountains", "skyline")),           # mine
    (("flower", "kite", "sun", "cat"), ("mountains", "waves")),             # resort
    (("loco", "envelope", "gear", "truck"), ("fields", "skyline")),         # rail
    (("pines", "house", "wheat", "dove"), ("forest", "mountains"))]         # sawmill

def frame(p, kind, a, b):
    """Draw the frame of the mosaic: a plain band, teeth, dots or a double line."""
    for x0, y0, x1, y1 in ((0, 0, W, 2), (0, H - 2, W, H), (0, 0, 2, H), (W - 2, 0, W, H)): p.rect(x0, y0, x1, y1, a)
    if kind == "teeth":
        for x in range(2, W - 2, 4): p.poly([(x, 2), (x + 2, 5), (x + 4, 2)], a); p.poly([(x, H - 2), (x + 2, H - 5), (x + 4, H - 2)], a)
    elif kind == "dots":
        for x in range(3, W, 4): p.rect(x, .5, x + 1, 1.5, b); p.rect(x, H - 1.5, x + 1, H - .5, b)
        for y in range(3, H, 4): p.rect(.5, y, 1.5, y + 1, b); p.rect(W - 1.5, y, W - .5, y + 1, b)
    elif kind == "double":
        for x0, y0, x1, y1 in ((3, 3, W - 3, 4), (3, H - 4, W - 3, H - 3), (3, 3, 4, H - 3), (W - 4, 3, W - 3, H - 3)): p.rect(x0, y0, x1, y1, b)

def mosaic(town, rng, used):
    """Draw one mosaic for TOWN that no other cell repeats; return it as a W×H image."""
    heroes, horizons = TOWN_TASTE[town]
    while True:
        hero = rng.choice(heroes) if rng.random() < .6 else rng.choice(list(HEROES))
        horizon = rng.choice(horizons) if rng.random() < .5 else rng.choice(list(HORIZONS))
        back = rng.choice(list(BACKS)); pal = rng.randrange(len(PALETTES)); edge = rng.choice(("plain", "teeth", "dots", "double"))
        if (hero, horizon, back, pal) not in used: break
    used.add((hero, horizon, back, pal))
    top, low, main, accent = (C[k] for k in PALETTES[pal]); p = Pen()
    BACKS[back](p, top, low, rng); HORIZONS[horizon](p, rng)
    if rng.random() < .35 and hero not in ("sun", "moon"):                  # a small sun or moon in a corner
        (h_sun if rng.random() < .7 else h_moon)(p, rng.choice((9, W - 9)), rng.uniform(9, 13), rng.uniform(3.5, 5), accent, top)
    size = rng.uniform(10, 13.5)
    HEROES[hero](p, W / 2 + rng.uniform(-4, 4), rng.uniform(26, 34), size, main, accent)
    frame(p, edge, C["dark"] if rng.random() < .6 else main, C["gold"])
    return p.img.resize((W, H), Image.BOX)

if __name__ == "__main__":
    rng = random.Random(1917); used = set(); atlas = Image.new("RGB", (COLS * W, ROWS * H))
    for town in range(TOWNS):
        for k in range(PER_TOWN):
            n = town * PER_TOWN + k
            atlas.paste(mosaic(town, rng, used), ((n % COLS) * W, (n // COLS) * H))
    OUT.parent.mkdir(parents=True, exist_ok=True); atlas.save(OUT, optimize=True)
    print(len(used), "мозаик →", OUT, f"{OUT.stat().st_size // 1024} КБ")
