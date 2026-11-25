#!/usr/bin/env python3
"""Smaller copies of the pictures for the browser build. A new player downloads the whole game
before the first frame, and most of that was pictures kept as PNG.

    tools/pack-assets.py        make what is missing or older than its source; run from `make web`

For every project/textures/*/*.png a .webp is written next to it (lossy, or lossless where the picture has
transparency). The game reads the small copy when it is there (lisp/files.lisp, PACKED-PATH), and the browser build packs only the
small copies (project/export_presets.cfg, exclude_filter). The sources stay what they were: the generators in
tools/ still write PNG, and a run from the project folder without this script still works.
Needs Pillow with WebP.
"""
import glob, io, os, shutil, subprocess, sys
from PIL import Image

root = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "project")
made = saved = 0


def stale(source, target):
    return not os.path.exists(target) or os.path.getmtime(target) < os.path.getmtime(source)


for png in sorted(glob.glob(os.path.join(root, "textures", "*", "*.png"))):
    webp = png[:-4] + ".webp"
    if stale(png, webp):
        image = Image.open(png)
        if image.mode in ("RGBA", "LA", "P"):
            image.convert("RGBA").save(webp, "WEBP", lossless=True, method=6)
        else:
            # Photographs of a surface shrink ten times when a little is lost; a poster or a mosaic of flat colours
            # is as small kept exactly. Both are tried, and the exact one wins unless the other is clearly smaller.
            # A normal map bends the light: where it is not kept exactly, it is kept a little finer.
            exact, lossy = io.BytesIO(), io.BytesIO()
            image.convert("RGB").save(exact, "WEBP", lossless=True, method=6)
            image.convert("RGB").save(lossy, "WEBP", quality=93 if png.endswith("_n.png") else 88, method=6)
            best = exact if exact.tell() <= 1.15 * lossy.tell() else lossy
            open(webp, "wb").write(best.getvalue())
        made += 1
    saved += os.path.getsize(png) - os.path.getsize(webp)

print(f"pack-assets: made {made}, the small copies are {saved / 1e6:.1f} MB lighter than their sources")
