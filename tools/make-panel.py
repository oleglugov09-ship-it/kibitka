#!/usr/bin/env python3
"""Make the panel-wall texture → project/textures/<style>/panel.png.

One panel per repeat: smooth concrete with fine specks and a dark seam along the edge. The middle is 50 % gray:
the game multiplies the texture by twice the house color. Usage: tools/make-panel.py"""
import pathlib, random
from PIL import Image

ROOT = pathlib.Path(__file__).resolve().parent.parent / "project" / "textures"
N = 64
rng = random.Random(3)
img = Image.new("RGB", (N, N))
for y in range(N):
    for x in range(N):
        v = 128 + rng.randint(-5, 5)
        if rng.random() < 0.03: v -= 14                       # specks in the concrete
        edge = min(x, y, N - 1 - x, N - 1 - y)
        if edge == 0: v = 78                                   # the seam between panels
        elif edge == 1: v = 112
        if 2 <= y < 9 and x > 1: v -= (9 - y)                  # a streak under the top seam
        img.putpixel((x, y), (v, v, v))
for style in ("pixel", "painted", "pbr"):
    (ROOT / style).mkdir(exist_ok=True)
    img.save(ROOT / style / "panel.png")
print("panel.png →", ROOT)
