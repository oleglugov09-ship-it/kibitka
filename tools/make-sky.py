#!/usr/bin/env python3
"""Draw what the big telescope shows → project/textures/scope/*.png.

The Moon, Mars, Jupiter with its moons, Saturn with its rings, Venus as a crescent, the Andromeda nebula,
the Pleiades. A round field of view on black, in chunky pixels — like the whole game. Usage: tools/make-sky.py"""
import math, pathlib, random
from PIL import Image, ImageDraw, ImageFilter

OUT = pathlib.Path(__file__).resolve().parent.parent / "project" / "textures" / "scope"
N = 160

def field(seed, stars=60):
    rng = random.Random(seed); img = Image.new("RGB", (N, N), (4, 5, 12)); px = img.load()
    for _ in range(stars):
        x, y, v = rng.randrange(N), rng.randrange(N), rng.randint(60, 200)
        px[x, y] = (v, v, min(255, v + 30))
    return img

def ball(img, cx, cy, r, color, light=(-.5, -.5), bands=None, spots=()):
    """Draw a shaded ball: BANDS is a function latitude → color, SPOTS are (x y r color) on the disc."""
    px = img.load(); lx, ly = light
    for y in range(int(cy - r), int(cy + r) + 1):
        for x in range(int(cx - r), int(cx + r) + 1):
            dx, dy = (x - cx) / r, (y - cy) / r; d2 = dx * dx + dy * dy
            if d2 > 1 or not (0 <= x < N and 0 <= y < N): continue
            dz = math.sqrt(1 - d2); c = bands(dy) if bands else color
            for (sx, sy, sr, sc) in spots:
                if (dx - sx) ** 2 + (dy - sy) ** 2 < sr * sr: c = sc
            k = max(.06, min(1.0, .25 + .85 * (dx * lx + dy * ly + dz * .65)))
            px[x, y] = tuple(min(255, int(v * k)) for v in c)

def finish(img, name):
    """Apply the round field of view and the chunky pixels."""
    mask = Image.new("L", (N, N), 0); ImageDraw.Draw(mask).ellipse([3, 3, N - 4, N - 4], fill=255)
    out = Image.new("RGB", (N, N), (0, 0, 0)); out.paste(img, (0, 0), mask)
    out.resize((N * 3, N * 3), Image.NEAREST).save(OUT / f"{name}.png")

def moon():
    rng = random.Random(1); img = field(1, 20)
    craters = [(rng.uniform(-.8, .8), rng.uniform(-.8, .8), rng.uniform(.04, .13), (150, 150, 146)) for _ in range(26)]
    seas = [(-.3, -.3, .32, (120, 122, 126)), (.25, -.1, .26, (126, 128, 130)), (-.1, .35, .22, (118, 120, 124))]
    ball(img, 80, 80, 66, (214, 212, 200), spots=seas + craters); finish(img, "moon")

def mars():
    img = field(2); ball(img, 80, 80, 40, (200, 96, 56), spots=[(0, -.86, .3, (240, 236, 228)), (-.2, .1, .3, (150, 70, 44)), (.35, .3, .2, (156, 76, 48))])
    finish(img, "mars")

def jupiter():
    img = field(3)
    def bands(lat):
        k = math.sin(lat * 11) + .4 * math.sin(lat * 23)
        return (214, 190, 150) if k > .2 else ((178, 130, 92) if k < -.3 else (226, 208, 176))
    ball(img, 80, 80, 46, None, bands=bands, spots=[(.3, .3, .14, (190, 80, 60))])
    for (x, y) in ((14, 78), (30, 81), (128, 79), (146, 82)): ImageDraw.Draw(img).rectangle([x, y, x + 1, y + 1], fill=(240, 236, 220))
    finish(img, "jupiter")

def saturn():
    img = field(4); d = ImageDraw.Draw(img)
    for r, c in ((66, (150, 130, 96)), (60, (196, 174, 130)), (52, (20, 18, 16)), (49, (170, 150, 110))):
        d.ellipse([80 - r, 80 - r * .34, 80 + r, 80 + r * .34], outline=c, width=3)
    def bands(lat): return (222, 200, 150) if math.sin(lat * 9) > 0 else (204, 180, 132)
    ball(img, 80, 80, 30, None, bands=bands)
    for r, c in ((66, (150, 130, 96)), (60, (196, 174, 130)), (49, (170, 150, 110))):   # the front half of the rings goes over the ball
        d.arc([80 - r, 80 - r * .34, 80 + r, 80 + r * .34], 0, 180, fill=c, width=3)
    finish(img, "saturn")

def venus():
    img = field(5, 30); ball(img, 80, 80, 44, (246, 240, 214), light=(-1.0, -.1)); px = img.load()
    for y in range(N):
        for x in range(N):
            if (x - 96) ** 2 + (y - 80) ** 2 < 44 * 44 and (x - 80) ** 2 + (y - 80) ** 2 < 45 * 45: px[x, y] = (6, 6, 12)
    finish(img, "venus")

def andromeda():
    img = field(6, 90); glow = Image.new("RGB", (N, N), (0, 0, 0)); d = ImageDraw.Draw(glow)
    for r, v in ((62, 26), (48, 44), (34, 70), (20, 110), (9, 180), (4, 240)):
        e = Image.new("RGB", (N, N), (0, 0, 0)); ImageDraw.Draw(e).ellipse([80 - r, 80 - r * .36, 80 + r, 80 + r * .36], fill=(v, v, min(255, int(v * 1.15))))
        glow = Image.blend(glow, Image.composite(e, glow, e.convert("L").point(lambda p: 255 if p else 0)), .9)
    glow = glow.rotate(32).filter(ImageFilter.GaussianBlur(2.2))
    img = Image.composite(glow, img, glow.convert("L").point(lambda p: min(255, p * 3))); finish(img, "andromeda")

def pleiades():
    img = field(7, 80); glow = Image.new("RGB", (N, N), (0, 0, 0)); d = ImageDraw.Draw(glow)
    spots = ((70, 78), (86, 70), (94, 84), (78, 92), (60, 66), (104, 64), (88, 100))
    for (x, y) in spots: d.ellipse([x - 9, y - 9, x + 9, y + 9], fill=(40, 60, 110))
    glow = glow.filter(ImageFilter.GaussianBlur(5)); img = Image.composite(glow, img, glow.convert("L").point(lambda p: min(255, p * 2)))
    d = ImageDraw.Draw(img)
    for (x, y) in spots:
        d.rectangle([x - 1, y - 1, x + 1, y + 1], fill=(230, 240, 255)); d.line([(x - 4, y), (x + 4, y)], fill=(150, 180, 240)); d.line([(x, y - 4), (x, y + 4)], fill=(150, 180, 240))
    finish(img, "pleiades")

def clouds():
    """Make clouds for the whole sky dome → project/textures/sky/clouds.png.

    The noise is computed in three dimensions right on the sphere: the unwrapped image has no "pole",
    and the sky overhead doesn't pinch into a star."""
    import numpy as np
    w, h = 1024, 512
    lon = (np.arange(w) + .5) / w * 2 * np.pi; lat = (np.arange(h) + .5) / h * np.pi          # 0 is the zenith
    lon, lat = np.meshgrid(lon, lat)
    p = np.stack([np.sin(lat) * np.cos(lon), np.cos(lat), np.sin(lat) * np.sin(lon)], -1)
    def lattice(i):
        return np.modf(np.sin(i[..., 0] * 127.1 + i[..., 1] * 311.7 + i[..., 2] * 74.7) * 43758.5453)[0] % 1.0
    def noise(q):
        i = np.floor(q); f = q - i; f = f * f * (3 - 2 * f); out = 0
        for dx in (0, 1):
            for dy in (0, 1):
                for dz in (0, 1):
                    wgt = (f[..., 0] if dx else 1 - f[..., 0]) * (f[..., 1] if dy else 1 - f[..., 1]) * (f[..., 2] if dz else 1 - f[..., 2])
                    out = out + wgt * lattice(i + np.array([dx, dy, dz]))
        return out
    v = sum(noise(p * 2.6 * 2 ** o + 11.3 * o) / 2 ** o for o in range(5)) / 1.94
    v = np.clip((v - .5) * 1.7 + .5, 0, 1)
    out = OUT.parent / "sky"; out.mkdir(parents=True, exist_ok=True)
    Image.fromarray((v * 255).astype("uint8"), "L").convert("RGB").save(out / "clouds.png")

if __name__ == "__main__":
    OUT.mkdir(parents=True, exist_ok=True)
    moon(); mars(); jupiter(); saturn(); venus(); andromeda(); pleiades(); clouds()
    print("7 картинок и облака →", OUT)
