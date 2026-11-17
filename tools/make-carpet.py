#!/usr/bin/env python3
"""Make the carpet for the wagon wall → project/textures/murals/carpet.png.

The ornament is the one the author picked: a picture of a Caucasian pile carpet with three medallions on a
dark blue field, kept outside the repository (its origin and licence are not known to us, so the source
picture is not published here; pass its path). We cut off the margins and the fringe (TRIM), turn it on its
side, since the carpet hangs along the wall, shrink it to SIZE pixels and keep COLORS colours;
lisp/cart.lisp hangs it unfiltered, so every pixel is a knot.
Usage: tools/make-carpet.py picture"""
import pathlib, sys
from PIL import Image, ImageEnhance

OUT = pathlib.Path(__file__).resolve().parent.parent / "project" / "textures" / "murals" / "carpet.png"
SIZE, COLORS, TRIM = (136, 96), 24, (10, 6, 762, 1074)       # TRIM is for the 773×1128 picture: left, top, right, bottom

if __name__ == "__main__":
    if len(sys.argv) != 2: sys.exit(__doc__)
    src = Image.open(sys.argv[1]).convert("RGB").crop(TRIM).rotate(90, expand=True)
    small = ImageEnhance.Color(src.resize(SIZE, Image.BOX)).enhance(1.15)
    small.quantize(COLORS, method=Image.MEDIANCUT, dither=Image.NONE).convert("RGB").save(OUT, optimize=True)
    print(small.size, "→", OUT)
