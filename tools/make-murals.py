#!/usr/bin/env python3
"""Make mosaic murals for the towns → project/textures/murals/<town number>-a.png (scene) and -b.png (ornament).

We draw in large shapes on a grid of 72×96 "tiles", then lay them out: each tile gets its own shade and a
dark seam. The scene suits the town's character (see *TOWN-THEMES* in places.lisp). Usage: tools/make-murals.py"""
import math, pathlib, random
from PIL import Image, ImageDraw

OUT = pathlib.Path(__file__).resolve().parent.parent / "project" / "textures" / "murals"
W, H, TILE = 72, 96, 5

def rgb(h): return tuple(int(h[i:i + 2], 16) for i in (1, 3, 5))

SKY, CREAM, GOLD, RED, BLUE, TEAL, GREEN, DARK, BROWN, WHITE, ORANGE = map(rgb, (
    "#5a8ab0", "#e8dcc0", "#d8a030", "#b03a2c", "#2c4a7a", "#3f7f83", "#4a7a48", "#2a2c34", "#7a4a30", "#f0ece0", "#e07a30"))

def base(d, top, low, sun=(18, 20, 9), sun_rgb=GOLD):
    """Draw the sky in two arched bands and a sun with rays."""
    d.rectangle([0, 0, W, H], fill=top)
    d.ellipse([-30, 34, W + 30, 150], fill=low)
    x, y, r = sun
    for i in range(12):
        a = i * math.pi / 6
        d.polygon([(x + math.cos(a - .12) * r * 1.2, y + math.sin(a - .12) * r * 1.2),
                   (x + math.cos(a) * r * 2.3, y + math.sin(a) * r * 2.3),
                   (x + math.cos(a + .12) * r * 1.2, y + math.sin(a + .12) * r * 1.2)], fill=sun_rgb)
    d.ellipse([x - r, y - r, x + r, y + r], fill=sun_rgb)

def waves(d, y0, colors):
    for k, c in enumerate(colors):
        pts = [(x, y0 + k * 7 + 3 * math.sin(x / 5.0 + k)) for x in range(-2, W + 3)]
        d.polygon(pts + [(W + 2, H + 2), (-2, H + 2)], fill=c)

def mountains(d, y0, peaks, rock, snow=WHITE):
    for (x, h, w) in peaks:
        d.polygon([(x - w, y0), (x, y0 - h), (x + w, y0)], fill=rock)
        d.polygon([(x - w * .28, y0 - h * .72), (x, y0 - h), (x + w * .28, y0 - h * .72), (x, y0 - h * .62)], fill=snow)

def pine(d, x, y, s, c=GREEN):
    for k in range(3):
        d.polygon([(x - s * (1.1 - k * .25), y - k * s * .7), (x, y - s * 1.4 - k * s * .7), (x + s * (1.1 - k * .25), y - k * s * .7)], fill=c)
    d.rectangle([x - s * .15, y, x + s * .15, y + s * .6], fill=BROWN)

def gear(d, x, y, r, c, hole):
    for i in range(10):
        a = i * math.pi / 5
        d.polygon([(x + math.cos(a - .16) * r, y + math.sin(a - .16) * r), (x + math.cos(a - .1) * r * 1.3, y + math.sin(a - .1) * r * 1.3),
                   (x + math.cos(a + .1) * r * 1.3, y + math.sin(a + .1) * r * 1.3), (x + math.cos(a + .16) * r, y + math.sin(a + .16) * r)], fill=c)
    d.ellipse([x - r, y - r, x + r, y + r], fill=c); d.ellipse([x - r * .4, y - r * .4, x + r * .4, y + r * .4], fill=hole)

def star(d, x, y, r, c):
    pts = []
    for i in range(10):
        a = -math.pi / 2 + i * math.pi / 5; rr = r if i % 2 == 0 else r * .42
        pts.append((x + math.cos(a) * rr, y + math.sin(a) * rr))
    d.polygon(pts, fill=c)

def gull(d, x, y, s, c=WHITE):
    d.line([(x - s, y - s * .3), (x - s * .4, y - s * .7), (x, y), (x + s * .4, y - s * .7), (x + s, y - s * .3)], fill=c, width=2)

def river(d):
    base(d, SKY, CREAM); waves(d, 58, (TEAL, BLUE, TEAL, BLUE, DARK))
    for (x, y, s) in ((24, 74, 9), (48, 84, 7)):
        d.ellipse([x - s, y - s * .5, x + s, y + s * .5], fill=GOLD); d.polygon([(x + s * .8, y), (x + s * 1.7, y - s * .6), (x + s * 1.7, y + s * .6)], fill=GOLD)
        d.ellipse([x - s * .6, y - 1.5, x - s * .6 + 2, y + .5], fill=DARK)
    d.polygon([(40, 56), (46, 40), (52, 56)], fill=WHITE); d.polygon([(36, 58), (56, 58), (52, 63), (40, 63)], fill=RED)

def military(d):
    base(d, BLUE, SKY, sun=(54, 18, 7), sun_rgb=CREAM); mountains(d, 96, ((16, 30, 22), (56, 38, 26)), GREEN, GREEN)
    star(d, 36, 52, 24, RED); star(d, 36, 52, 13, GOLD)
    for (x, y) in ((14, 30), (26, 20)):
        d.polygon([(x, y), (x + 12, y + 3), (x, y + 6), (x + 3, y + 3)], fill=WHITE)

def factory(d):
    base(d, ORANGE, CREAM, sun=(54, 20, 8), sun_rgb=RED)
    for (x, w, h) in ((10, 6, 50), (22, 6, 62), (34, 6, 44)):
        d.rectangle([x, H - h, x + w, H], fill=BROWN); d.rectangle([x, H - h, x + w, H - h + 5], fill=RED)
        for k in range(3): d.ellipse([x - 3 + k * 5, H - h - 12 - k * 6, x + 9 + k * 6, H - h - 2 - k * 5], fill=WHITE)
    d.rectangle([0, 80, W, H], fill=DARK); gear(d, 52, 66, 13, GOLD, ORANGE); gear(d, 30, 84, 8, CREAM, DARK)

def university(d):
    base(d, BLUE, TEAL, sun=(36, 30, 6), sun_rgb=GOLD)
    for a in (0, 60, 120):
        e = Image.new("RGBA", (W, H), (0, 0, 0, 0)); ImageDraw.Draw(e).ellipse([8, 20, 64, 40], outline=CREAM + (255,), width=2)
        e = e.rotate(a, center=(36, 30)); d.bitmap((0, 0), e.split()[3], fill=CREAM)
    d.polygon([(8, 66), (36, 74), (36, 92), (8, 84)], fill=WHITE); d.polygon([(64, 66), (36, 74), (36, 92), (64, 84)], fill=CREAM)
    for k in range(4):
        d.line([(12, 70 + k * 4), (32, 76 + k * 4)], fill=BLUE, width=1); d.line([(60, 70 + k * 4), (40, 76 + k * 4)], fill=BLUE, width=1)
    star(d, 12, 12, 5, GOLD); star(d, 60, 10, 4, GOLD); star(d, 56, 50, 3, WHITE)

def village(d):
    base(d, SKY, GOLD, sun=(52, 20, 10), sun_rgb=ORANGE); d.rectangle([0, 74, W, H], fill=GREEN)
    d.polygon([(8, 74), (8, 58), (20, 48), (32, 58), (32, 74)], fill=BROWN); d.polygon([(5, 59), (20, 45), (35, 59)], fill=RED)
    d.rectangle([16, 62, 24, 70], fill=CREAM)
    for x in (44, 54, 64):
        d.line([(x, 92), (x, 50)], fill=BROWN, width=2)
        for k in range(6):
            d.polygon([(x, 54 + k * 5), (x - 5, 49 + k * 5), (x - 1, 56 + k * 5)], fill=CREAM); d.polygon([(x, 54 + k * 5), (x + 5, 49 + k * 5), (x + 1, 56 + k * 5)], fill=CREAM)

def port(d):
    base(d, BLUE, SKY, sun=(16, 18, 8)); waves(d, 70, (TEAL, BLUE, TEAL, DARK))
    d.polygon([(12, 62), (60, 62), (54, 74), (18, 74)], fill=RED); d.rectangle([12, 60, 60, 63], fill=DARK)
    d.rectangle([35, 22, 37, 62], fill=BROWN); d.polygon([(38, 24), (58, 56), (38, 56)], fill=WHITE); d.polygon([(34, 30), (16, 56), (34, 56)], fill=CREAM)
    d.polygon([(37, 18), (48, 21), (37, 24)], fill=RED)
    gull(d, 52, 16, 6); gull(d, 62, 28, 4); gull(d, 44, 10, 3)

def admin(d):
    base(d, RED, ORANGE, sun=(36, 26, 11), sun_rgb=GOLD)
    d.rectangle([14, 66, 58, H], fill=CREAM); d.polygon([(10, 66), (36, 52), (62, 66)], fill=WHITE)
    for x in range(18, 58, 8): d.rectangle([x, 68, x + 3, H], fill=WHITE); d.rectangle([x + 3, 68, x + 4, H], fill=BROWN)
    d.rectangle([31, 34, 41, 54], fill=CREAM); d.ellipse([32, 37, 40, 45], fill=WHITE, outline=DARK); d.line([(36, 41), (36, 38)], fill=DARK); d.line([(36, 41), (38, 42)], fill=DARK)
    d.polygon([(31, 34), (36, 24), (41, 34)], fill=BROWN)

def outpost(d):
    base(d, DARK, BLUE, sun=(56, 16, 5), sun_rgb=WHITE)
    for k, c in enumerate((GREEN, TEAL, (120, 200, 160))):
        pts = [(x, 30 + k * 6 + 8 * math.sin(x / 9.0 + k * 1.3)) for x in range(-2, W + 3)]
        d.line(pts, fill=c, width=3)
    mountains(d, 96, ((14, 34, 20), (40, 46, 26), (62, 30, 18)), SKY)
    d.polygon([(18, 56), (46, 52), (52, 55), (46, 58)], fill=ORANGE); d.polygon([(28, 50), (38, 49), (36, 62), (30, 62)], fill=RED); d.polygon([(18, 56), (14, 50), (20, 54)], fill=ORANGE)

def mine(d):
    base(d, BROWN, ORANGE, sun=(36, 34, 9), sun_rgb=GOLD); mountains(d, 96, ((16, 50, 24), (54, 58, 28)), DARK, DARK)
    d.polygon([(30, 96), (36, 60), (42, 96)], fill=BROWN); d.ellipse([29, 54, 43, 68], fill=DARK, outline=GOLD, width=2)
    d.line([(36, 61), (10, 96)], fill=GOLD, width=2); d.line([(36, 61), (62, 96)], fill=GOLD, width=2)
    d.rectangle([22, 84, 50, 92], fill=RED); d.ellipse([24, 90, 30, 96], fill=DARK); d.ellipse([42, 90, 48, 96], fill=DARK)

def resort(d):
    base(d, SKY, CREAM, sun=(54, 18, 9)); mountains(d, 80, ((14, 46, 22), (40, 60, 30), (64, 40, 20)), BLUE)
    d.rectangle([0, 80, W, H], fill=GREEN); pine(d, 12, 86, 7); pine(d, 60, 88, 6)
    d.line([(0, 62), (W, 30)], fill=DARK, width=1); d.rectangle([26, 52, 38, 62], fill=RED); d.rectangle([28, 54, 36, 58], fill=WHITE); d.line([(32, 48), (32, 52)], fill=DARK)

def rail(d):
    base(d, TEAL, CREAM, sun=(14, 16, 7))
    d.polygon([(30, 96), (34, 70), (38, 70), (42, 96)], fill=BROWN); d.line([(30, 96), (34, 70)], fill=DARK, width=2); d.line([(42, 96), (38, 70)], fill=DARK, width=2)
    d.ellipse([20, 36, 52, 68], fill=DARK); d.ellipse([27, 43, 45, 61], fill=RED); star(d, 36, 52, 6, GOLD)
    d.rectangle([32, 22, 40, 38], fill=DARK); d.rectangle([30, 20, 42, 24], fill=RED)
    for k in range(3): d.ellipse([34 + k * 6, 4 + k * 2, 46 + k * 8, 16 + k * 2], fill=WHITE)
    d.rectangle([16, 64, 56, 72], fill=RED); d.ellipse([18, 68, 28, 78], fill=DARK); d.ellipse([44, 68, 54, 78], fill=DARK)

def sawmill(d):
    base(d, SKY, CREAM, sun=(54, 16, 8)); d.rectangle([0, 78, W, H], fill=BROWN)
    pine(d, 12, 74, 9); pine(d, 28, 76, 12); pine(d, 62, 74, 8)
    gear(d, 48, 58, 13, RED, CREAM)
    for (x, y) in ((10, 88), (20, 88), (30, 88), (15, 81), (25, 81)):
        d.ellipse([x - 5, y - 5, x + 5, y + 5], fill=GOLD, outline=BROWN); d.ellipse([x - 2, y - 2, x + 2, y + 2], outline=BROWN)

SCENES = (river, military, factory, university, village, port, admin, outpost, mine, resort, rail, sawmill)
PAIRS = ((TEAL, GOLD), (GREEN, RED), (ORANGE, DARK), (BLUE, GOLD), (GOLD, GREEN), (BLUE, WHITE),
         (RED, GOLD), (DARK, TEAL), (BROWN, GOLD), (SKY, RED), (TEAL, RED), (GREEN, BROWN))

def ornament(d, a, b):
    """Draw an ornament: nested diamonds and a toothed border — in the town's colors."""
    d.rectangle([0, 0, W, H], fill=CREAM)
    for k, c in enumerate((a, CREAM, b, CREAM, a, b)):
        s = 1 - k * .16
        d.polygon([(W / 2, H / 2 - 44 * s), (W / 2 + 32 * s, H / 2), (W / 2, H / 2 + 44 * s), (W / 2 - 32 * s, H / 2)], fill=c)
    for x in range(0, W, 8):
        d.polygon([(x, 0), (x + 4, 7), (x + 8, 0)], fill=b); d.polygon([(x, H), (x + 4, H - 7), (x + 8, H)], fill=b)
    for (x, y) in ((8, 14), (64, 14), (8, 82), (64, 82)): star(d, x, y, 5, a)

def tile(img, seed):
    """Lay the picture in tiles: each gets its own shade, with a seam between them; the mural's edges are a frame."""
    rng = random.Random(seed); out = Image.new("RGB", (W * TILE, H * TILE)); px = img.load(); o = out.load()
    for y in range(H):
        for x in range(W):
            r, g, b = px[x, y][:3]; k = rng.uniform(.86, 1.1)
            if x < 2 or y < 2 or x >= W - 2 or y >= H - 2: r, g, b = (60, 50, 44) if (x + y) % 2 else (200, 160, 60)
            c = (min(255, int(r * k)), min(255, int(g * k)), min(255, int(b * k)))
            for j in range(TILE):
                for i in range(TILE):
                    o[x * TILE + i, y * TILE + j] = tuple(int(v * .55) for v in c) if (i == TILE - 1 or j == TILE - 1) else c
    return out

if __name__ == "__main__":
    OUT.mkdir(parents=True, exist_ok=True)
    for n, scene in enumerate(SCENES):
        img = Image.new("RGB", (W, H)); scene(ImageDraw.Draw(img)); tile(img, n).save(OUT / f"{n}-a.png")
        img = Image.new("RGB", (W, H)); ornament(ImageDraw.Draw(img), *PAIRS[n]); tile(img, 100 + n).save(OUT / f"{n}-b.png")
    print(len(SCENES) * 2, "панно →", OUT)
