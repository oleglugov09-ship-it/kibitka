#!/usr/bin/env python3
"""Make the carpet for the wagon wall → project/textures/murals/carpet.png.

A real Tatar piece, pixelated: a silk blanket in chain stitch and carpet stitch by an unknown Tatar
craftswoman, early 20th century, from the collection of the State Museum of Fine Arts of the Republic of
Tatarstan. The photograph is "Tatar blanket (2024-01-12) 01.jpg" by Vyacheslav Kirillin on Wikimedia Commons,
licence CC BY-SA 4.0: the picture made here is a derivative under the same licence, and the author must be
named wherever the game credits its sources.
The blanket lies in a showcase and was shot at an angle, so we straighten it by its four corners (CORNERS,
on the 960 px wide copy), shrink it to SIZE pixels and keep COLORS colours; lisp/cart.lisp hangs it
unfiltered, so every pixel is a knot.
Usage: tools/make-carpet.py [photo.jpg]   (without an argument the photograph is downloaded)"""
import io, pathlib, sys, urllib.request
from PIL import Image, ImageEnhance

URL = "https://thumb.wikimedia.org/wikipedia/commons/thumb/3/3d/Tatar_blanket_%282024-01-12%29_01.jpg/960px-Tatar_blanket_%282024-01-12%29_01.jpg"
OUT = pathlib.Path(__file__).resolve().parent.parent / "project" / "textures" / "murals" / "carpet.png"
SIZE, COLORS = (128, 76), 28
CORNERS = (116, 212, 52, 578, 900, 626, 868, 218)       # upper left, lower left, lower right, upper right

if __name__ == "__main__":
    if sys.argv[1:]: src = Image.open(sys.argv[1])
    else: src = Image.open(io.BytesIO(urllib.request.urlopen(urllib.request.Request(URL, headers={"User-Agent": "make-carpet/1.0"})).read()))
    flat = src.convert("RGB").transform((SIZE[0] * 8, SIZE[1] * 8), Image.QUAD, CORNERS, Image.BICUBIC)
    small = ImageEnhance.Contrast(ImageEnhance.Color(flat.resize(SIZE, Image.BOX)).enhance(1.4)).enhance(1.15)   # the glass of the showcase washes the colours out
    small.quantize(COLORS, method=Image.MEDIANCUT, dither=Image.NONE).convert("RGB").save(OUT, optimize=True)
    print(small.size, "→", OUT)
