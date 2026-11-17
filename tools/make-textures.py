#!/usr/bin/env python3
"""Make the textures of Kibitka for trying out styles.

    tools/make-textures.py painted   — painted: brush strokes, soft posterization, 512 px
    tools/make-textures.py pixel     — pixel: the same drawings, 64 px, 4 tones with dithering
    tools/make-textures.py pbr       — soft PBR: CC0 from Poly Haven, desaturated to almost gray

All textures are seamless and almost gray (mean ~0.5): the color comes from the game — the ground
is tinted by vertex color, wood and fabric by material color. The alpha of the meadow grass is
a mask of "own color" (the flowers are not tinted). The result goes to project/textures/<style>/.
Our own textures are ours; the PBR sources are in SOURCES.txt next to them.
"""

import io
import os
import json
import sys
import urllib.request
from pathlib import Path

import numpy as np
from PIL import Image

N = 512
OUT = Path(__file__).resolve().parent.parent / "project" / "textures"
rng = np.random.default_rng(7)

# ---------- noise, seamless at the edges ----------

def noise(n, gx, gy=None, seed=None):
    """Return values on a gx×gy lattice, smoothly stretched to n×n with wraparound at the edge."""
    gy = gy or gx
    r = np.random.default_rng(seed) if seed is not None else rng
    grid = r.random((gy, gx))
    def axis(g):
        c = np.arange(n) * g / n
        i0 = np.floor(c).astype(int)
        f = c - i0
        return i0 % g, (i0 + 1) % g, f * f * (3 - 2 * f)
    y0, y1, fy = axis(gy)
    x0, x1, fx = axis(gx)
    a = grid[np.ix_(y0, x0)]; b = grid[np.ix_(y0, x1)]
    c = grid[np.ix_(y1, x0)]; d = grid[np.ix_(y1, x1)]
    fx = fx[None, :]; fy = fy[:, None]
    return a + (b - a) * fx + (c - a) * fy + (a - b - c + d) * fx * fy

def fbm(n, g, octaves=4, gy=None):
    gy = gy or g
    v = sum(noise(n, g * 2**o, gy * 2**o) * .5**o for o in range(octaves))
    return v / sum(.5**o for o in range(octaves))

def norm(v, mean=.5, spread=.22):
    v = (v - v.mean()) / (v.std() + 1e-6)
    return np.clip(mean + v * spread, 0, 1)

def posterize(v, levels=5, soft=.35):
    return v * soft + np.round(v * levels) / levels * (1 - soft)

def cells(n, count, seed=None):
    """Return wrapping Voronoi cells: the index of the nearest point and the distances to the 1st and 2nd."""
    r = np.random.default_rng(seed)
    pts = r.random((count, 2)) * n
    yy, xx = np.mgrid[0:n, 0:n]
    best = np.full((n, n), 1e9); second = np.full((n, n), 1e9); idx = np.zeros((n, n), int)
    for k, (px, py) in enumerate(pts):
        dx = np.abs(xx - px); dx = np.minimum(dx, n - dx)
        dy = np.abs(yy - py); dy = np.minimum(dy, n - dy)
        d = np.sqrt(dx * dx + dy * dy)
        closer = d < best
        second = np.where(closer, best, np.minimum(second, d))
        idx = np.where(closer, k, idx)
        best = np.where(closer, d, best)
    return idx, best, second

def strokes(n, count, length, width, angle_jitter=.4, seed=None, vertical=True):
    """Paint brush strokes: short light and dark dashes."""
    r = np.random.default_rng(seed)
    out = np.zeros((n, n))
    yy, xx = np.mgrid[0:n, 0:n]
    for _ in range(count):
        cx, cy = r.random(2) * n
        a = (np.pi / 2 if vertical else 0) + r.normal(0, angle_jitter)
        ca, sa = np.cos(a), np.sin(a)
        dx = (xx - cx + n / 2) % n - n / 2
        dy = (yy - cy + n / 2) % n - n / 2
        u = dx * ca + dy * sa; v = -dx * sa + dy * ca
        L = length * (.6 + r.random() * .8)
        m = np.clip(1 - (u / L) ** 2 - (v / width) ** 2, 0, 1)
        out += m * (1 if r.random() < .5 else -1)
    return out

def gray(v):
    v = np.clip(v, 0, 1)
    return np.stack([v, v, v], -1)

# ---------- drawings (n is the size, everything in fractions of n) ----------

def grass(n, dry=False):
    base = fbm(n, 6)
    blades = noise(n, 96, 12) * .6 + noise(n, 160, 20) * .4
    s = strokes(n, 260 if not dry else 200, n * (.03 if not dry else .05), n * .006, seed=11 if not dry else 12)
    v = base * .5 + blades * .5 + s * .18
    return gray(norm(posterize(norm(v), 6), spread=.16 if not dry else .19))

def meadow(n):
    rgb = grass(n)
    alpha = np.ones((n, n))
    r = np.random.default_rng(5)
    yy, xx = np.mgrid[0:n, 0:n]
    colors = [(1, .45, .65), (1, .85, .35), (.7, .55, 1), (1, 1, 1), (1, .6, .25)]
    for _ in range(140):
        cx, cy = r.random(2) * n
        rad = n * (.006 + r.random() * .006)
        dx = (xx - cx + n / 2) % n - n / 2; dy = (yy - cy + n / 2) % n - n / 2
        m = np.clip(1.6 - np.sqrt(dx * dx + dy * dy) / rad, 0, 1)
        c = colors[r.integers(len(colors))]
        for ch in range(3):
            rgb[..., ch] = rgb[..., ch] * (1 - m) + c[ch] * .9 * m
        alpha = np.minimum(alpha, 1 - m)
    return np.concatenate([rgb, alpha[..., None]], -1)

def road(n):
    base = fbm(n, 5) * .6 + fbm(n, 24) * .4
    idx, d1, d2 = cells(n, 90, seed=3)
    pebble = np.clip(1 - d1 / (n * .018), 0, 1)
    tone = np.random.default_rng(4).random(90)[idx]
    v = norm(posterize(norm(base), 5), spread=.12) + pebble * (tone - .3) * .5
    return gray(v)

def rock(n):
    idx, d1, d2 = cells(n, 28, seed=8)
    tone = np.random.default_rng(9).random(28)[idx]
    crack = np.clip((d2 - d1) / (n * .012), 0, 1)
    v = tone * .55 + fbm(n, 8) * .3 + noise(n, 40) * .15
    v = posterize(norm(v), 5, .25) * (.55 + .45 * crack)
    return gray(norm(v, spread=.2))

def snow(n):
    v = fbm(n, 4) * .7 + noise(n, 30) * .3
    return gray(norm(v, .5, .07))

def sand(n):
    yy, xx = np.mgrid[0:n, 0:n] / n
    warp = fbm(n, 4) * 2
    ripples = np.sin((yy * 9 + warp) * 2 * np.pi)
    v = ripples * .35 + noise(n, 128) * .4 + fbm(n, 6) * .5
    return gray(norm(posterize(norm(v), 6), spread=.12))

def planks(n, rows, seam_every=(.4, .9), gap=.012, grain=1.0, seed=1, shade_bottom=0.0):
    r = np.random.default_rng(seed)
    v = np.zeros((n, n))
    grain_tex = noise(n, 6, 90) * .5 + noise(n, 3, 40) * .5
    for i in range(rows):
        y0, y1 = int(i * n / rows), int((i + 1) * n / rows)
        x = r.random() * n
        seams = []
        while len(seams) < 6 and (not seams or x < seams[0] + n):
            seams.append(x); x += n * (seam_every[0] + r.random() * (seam_every[1] - seam_every[0]))
        row = np.zeros((y1 - y0, n))
        xs = np.arange(n)
        for k in range(len(seams) - 1):
            a, b = seams[k], seams[k + 1]
            m = (((xs - a) % n) < (b - a)) if b - a < n else np.ones(n, bool)
            row[:, m] = r.random() * .1 + .45
        g = np.roll(grain_tex, r.integers(n), 1)[y0:y1]
        row = row + (g - .5) * .5 * grain
        yy = np.linspace(0, 1, y1 - y0)[:, None]
        row = row * (1 - shade_bottom * yy ** 3)
        row[: max(1, int(gap * n))] *= .35                    # the gap between planks
        for s in seams:                                       # joints and nails
            sx = int(s) % n
            row[:, sx:sx + max(1, int(gap * n * .6))] *= .45
            for ny in (.3, .7):
                py = int(ny * (y1 - y0)); px = (sx + int(.015 * n)) % n
                row[max(0, py - 1):py + 2, max(0, px - 1):px + 2] = .15
        v[y0:y1] = row
    return gray(norm(posterize(v, 6, .5), spread=.08))

def bark(n):
    ridges = np.abs(noise(n, 14, 3) - .5) * 2
    v = ridges * .6 + noise(n, 40, 6) * .25 + fbm(n, 5) * .15
    return gray(norm(posterize(norm(v), 5), spread=.22))

def carpet(n):
    yy, xx = np.mgrid[0:n, 0:n] / n
    u = (xx * 4) % 1 - .5; w = (yy * 4) % 1 - .5
    diamond = np.abs(u) + np.abs(w)
    medallion = np.where(diamond < .18, .85, np.where(diamond < .26, .25, np.where(diamond < .34, .7, .45)))
    cross = (np.abs(u) < .03) | (np.abs(w) < .03)
    medallion = np.where(cross & (diamond > .34), .3, medallion)
    weave = (np.sin(xx * n * np.pi) * np.sin(yy * n * np.pi)) * .05
    return gray(norm(medallion + weave + noise(n, 64) * .08, spread=.2))

def fabric(n):
    yy, xx = np.mgrid[0:n, 0:n] / n
    weave = np.sin(xx * n * .5 * np.pi) ** 2 * .5 + np.sin(yy * n * .5 * np.pi) ** 2 * .5
    folds = fbm(n, 3, 2, gy=6)
    return gray(norm(weave * .25 + folds * .75, spread=.09))

def water_normal(n):
    h = sum(noise(n, g, g, seed=20 + g) * a for g, a in ((6, 1), (12, .5), (24, .3), (48, .15)))
    dx = (np.roll(h, -1, 1) - np.roll(h, 1, 1)) * n * .02
    dy = (np.roll(h, -1, 0) - np.roll(h, 1, 0)) * n * .02
    nz = np.ones_like(h)
    l = np.sqrt(dx * dx + dy * dy + nz * nz)
    return np.stack([(-dx / l + 1) / 2, (dy / l + 1) / 2, (nz / l + 1) / 2], -1)

def macro(n):
    return gray(fbm(n, 4, 5))

SET = {
    "grass": lambda n: grass(n), "grass2": lambda n: grass(n) * .9 + .05, "meadow": meadow,
    "dry": lambda n: grass(n, dry=True), "road": road, "rock": rock, "snow": snow, "sand": sand,
    "planks_wall": lambda n: planks(n, 6, seed=1), "planks_floor": lambda n: planks(n, 5, (.7, 1.3), seed=2),
    "planks_roof": lambda n: planks(n, 8, (.5, 1.0), .02, seed=3, shade_bottom=.5),
    "bark": bark, "carpet": carpet, "fabric": fabric,
}

def save(arr, path, n=None):
    img = Image.fromarray((np.clip(arr, 0, 1) * 255).astype(np.uint8))
    if n and img.size[0] != n:
        img = img.resize((n, n), Image.LANCZOS)
    img.save(path)

# ---------- pixel: down to 64 px, 4 tones, Bayer ----------

BAYER = np.array([[0, 8, 2, 10], [12, 4, 14, 6], [3, 11, 1, 9], [15, 7, 13, 5]]) / 16 - .5

def pixelate(arr, n=64, levels=4):
    img = Image.fromarray((np.clip(arr, 0, 1) * 255).astype(np.uint8)).resize((n, n), Image.BOX)
    a = np.asarray(img).astype(float) / 255
    rgb = a[..., :3]
    lum = rgb.mean(-1, keepdims=True)
    t = np.tile(BAYER, (n // 4, n // 4))[..., None]
    lo, hi = lum.min(), lum.max()
    q = np.round(((lum - lo) / (hi - lo + 1e-6)) * (levels - 1) + t) / (levels - 1)
    q = lo + np.clip(q, 0, 1) * (hi - lo)
    out = np.clip(rgb - lum + q, 0, 1)                        # flowers keep their own color
    if a.shape[-1] == 4:
        out = np.concatenate([out, (a[..., 3:] > .5).astype(float)], -1)
    return out

# ---------- soft PBR: CC0 from Poly Haven ----------

PBR = {
    "grass": "leafy_grass", "grass2": "sparse_grass", "dry": "withered_grass", "meadow": "grass_ground",
    "road": "park_dirt", "rock": "cliff_side", "snow": "snow_02", "sand": "brown_mud",
    "planks_wall": "brown_planks_05", "planks_floor": "wood_floor_worn", "planks_roof": "roof_planks",
    "bark": "bark_brown_02", "carpet": "quatrefoil_jacquard_fabric", "fabric": "denim_fabric",
}

def fetch(url):
    req = urllib.request.Request(url, headers={"User-Agent": "kiosk-trip-textures"})
    with urllib.request.urlopen(req, timeout=60) as r:
        return r.read()

def neutral(img, n=512, chroma=.2):
    a = np.asarray(img.convert("RGB").resize((n, n), Image.LANCZOS)).astype(float) / 255
    lum = (a * [.3, .59, .11]).sum(-1, keepdims=True)
    out = lum + (a - lum) * chroma
    return np.clip(out * (.5 / lum.mean()), 0, 1)

def make_pbr(out):
    lines = []
    for name, asset in PBR.items():
        files = json.loads(fetch(f"https://api.polyhaven.com/files/{asset}"))
        diff = (files.get("Diffuse") or files.get("col_1") or files[next(k for k in files if k.startswith("col"))])["1k"]["jpg"]["url"]
        img = Image.open(io.BytesIO(fetch(diff)))
        if np.asarray(img.convert("L")).std() < 8 and "Displacement" in files:   # a flat color — take the pattern from the displacement map
            img = Image.open(io.BytesIO(fetch(files["Displacement"]["1k"]["png" if "png" in files["Displacement"]["1k"] else "jpg"]["url"])))
        save(neutral(img), out / f"{name}.png")
        nor = files.get("nor_gl", {}).get("1k", {}).get("jpg", {}).get("url")
        if nor:
            Image.open(io.BytesIO(fetch(nor))).convert("RGB").resize((N, N), Image.LANCZOS).save(out / f"{name}_n.png")
        lines.append(f"{name}: Poly Haven «{asset}», CC0 — https://polyhaven.com/a/{asset}")
        print(name, asset)
    lines.append("water_n, macro: свои (tools/make-textures.py)")
    (out / "SOURCES.txt").write_text("\n".join(lines) + "\n")

def main(style):
    out = OUT / style
    out.mkdir(parents=True, exist_ok=True)
    if style == "pbr":
        make_pbr(out)
    else:
        only = [n for n in os.environ.get("ONLY", "").split(",") if n]   # ONLY=planks_wall,bark — rebuild only these
        for name, make in SET.items():
            if only and name not in only:
                continue
            arr = make(N)
            if style == "pixel":
                arr = pixelate(arr)
            save(arr, out / f"{name}.png")
            print(name)
        (out / "SOURCES.txt").write_text("Все текстуры свои: tools/make-textures.py, право использования — наше.\n")
    save(water_normal(N), out / "water_n.png")
    save(macro(256), out / "macro.png")

if __name__ == "__main__":
    for s in sys.argv[1:] or ["painted", "pixel", "pbr"]:
        main(s)
