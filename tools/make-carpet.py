#!/usr/bin/env python3
"""Make the carpet for the wagon wall → project/textures/murals/carpet.png.

A real Soviet carpet, pixelated: the pictorial rug woven in Soviet Armenia for the tenth anniversary of the
republic's industry (1925–1935), from the collection published by the Ministry of Culture of the Republic of
Armenia; the photograph is in the public domain (Wikimedia Commons, "Armenian rug Soviet Armenia, the
beginning of the 20th century, No. 2175.jpg"). We trim the white margin, shrink it to W pixels across and
keep COLORS colours; lisp/cart.lisp hangs it unfiltered, so every pixel is a knot.
Usage: tools/make-carpet.py [photo.jpg]   (without an argument the photograph is downloaded)"""
import io, pathlib, sys, urllib.request
from PIL import Image

URL = "https://upload.wikimedia.org/wikipedia/commons/4/4e/Armenian_rug_Soviet_Armenia%2C_the_beginning_of_the_20th_century%2C_No._2175.jpg"
OUT = pathlib.Path(__file__).resolve().parent.parent / "project" / "textures" / "murals" / "carpet.png"
W, COLORS, TRIM = 84, 28, (16, 14, 876, 1236)

if __name__ == "__main__":
    if sys.argv[1:]: src = Image.open(sys.argv[1])
    else: src = Image.open(io.BytesIO(urllib.request.urlopen(urllib.request.Request(URL, headers={"User-Agent": "make-carpet/1.0"})).read()))
    src = src.convert("RGB").crop(TRIM)
    small = src.resize((W, round(W * src.height / src.width)), Image.BOX)
    small.quantize(COLORS, method=Image.MEDIANCUT, dither=Image.NONE).convert("RGB").save(OUT, optimize=True)
    print(small.size, "→", OUT)
