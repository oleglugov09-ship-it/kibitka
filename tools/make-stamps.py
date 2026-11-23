#!/usr/bin/env python3
"""Draw the postage stamp of each town -> project/textures/stamps/<town>.png.

A stamp stands for the town on the map and on parcels instead of its name: a perforated edge, a field in
the town's colour and a picture of what the town is about. No letters, so it reads in any language.
Run: tools/make-stamps.py
"""
import pathlib
from PIL import Image, ImageDraw

OUT = pathlib.Path(__file__).resolve().parent.parent / "project" / "textures" / "stamps"
W, H = 40, 48
PAPER, INK = (240, 236, 224), (34, 34, 42)

def river(d, c):       # waves and a bridge
    d.arc([8, 14, 32, 34], 180, 360, fill=PAPER, width=2); d.line([(8, 24), (32, 24)], fill=PAPER, width=2)
    for y in (31, 36): d.line([(8, y), (12, y - 2), (16, y), (20, y - 2), (24, y), (28, y - 2), (32, y)], fill=PAPER, width=1)
def military(d, c):    # a star
    d.polygon([(20, 10), (23, 20), (33, 20), (25, 26), (28, 37), (20, 30), (12, 37), (15, 26), (7, 20), (17, 20)], fill=PAPER)
def factory(d, c):     # a works with chimneys and smoke
    d.rectangle([8, 26, 32, 38], fill=PAPER); d.rectangle([11, 14, 15, 26], fill=PAPER); d.rectangle([20, 18, 24, 26], fill=PAPER)
    d.ellipse([13, 8, 19, 13], fill=PAPER); d.ellipse([22, 12, 28, 17], fill=PAPER)
    for x in (12, 19, 26): d.rectangle([x, 30, x + 3, 34], fill=c)
def university(d, c):  # an open book
    d.polygon([(8, 16), (19, 19), (19, 36), (8, 33)], fill=PAPER); d.polygon([(32, 16), (21, 19), (21, 36), (32, 33)], fill=PAPER)
    for y in (22, 26, 30): d.line([(10, y), (17, y + 2)], fill=c); d.line([(30, y), (23, y + 2)], fill=c)
def village(d, c):     # a house with a pitched roof
    d.polygon([(20, 10), (33, 22), (7, 22)], fill=PAPER); d.rectangle([10, 22, 30, 37], fill=PAPER)
    d.rectangle([17, 27, 23, 37], fill=c); d.rectangle([26, 12, 28, 18], fill=PAPER)
def port(d, c):        # an anchor
    d.ellipse([17, 9, 23, 15], outline=PAPER, width=2); d.line([(20, 15), (20, 36)], fill=PAPER, width=2)
    d.line([(14, 20), (26, 20)], fill=PAPER, width=2); d.arc([9, 20, 31, 38], 20, 160, fill=PAPER, width=2)
def admin(d, c):       # a building with columns
    d.polygon([(20, 10), (33, 18), (7, 18)], fill=PAPER); d.rectangle([8, 34, 32, 38], fill=PAPER); d.rectangle([8, 18, 32, 20], fill=PAPER)
    for x in (10, 16, 22, 28): d.rectangle([x, 21, x + 2, 33], fill=PAPER)
def outpost(d, c):     # a snowflake
    for a, b in (((20, 10), (20, 38)), ((8, 17), (32, 31)), ((8, 31), (32, 17))): d.line([a, b], fill=PAPER, width=2)
    for p in ((20, 10), (20, 38), (8, 17), (32, 31), (8, 31), (32, 17)): d.ellipse([p[0] - 2, p[1] - 2, p[0] + 2, p[1] + 2], fill=PAPER)
def mine(d, c):        # crossed pickaxes
    d.line([(10, 36), (30, 14)], fill=PAPER, width=2); d.line([(30, 36), (10, 14)], fill=PAPER, width=2)
    d.arc([20, 8, 38, 22], 150, 300, fill=PAPER, width=3); d.arc([2, 8, 20, 22], 240, 30, fill=PAPER, width=3)
def resort(d, c):      # the sun over water
    d.ellipse([13, 11, 27, 25], fill=PAPER)
    for y in (30, 35): d.line([(7, y), (12, y - 2), (17, y), (22, y - 2), (27, y), (33, y - 2)], fill=PAPER, width=1)
def rail(d, c):        # rails running away
    d.line([(12, 38), (18, 10)], fill=PAPER, width=2); d.line([(28, 38), (22, 10)], fill=PAPER, width=2)
    for y, x0, x1 in ((34, 10, 30), (28, 12, 28), (22, 14, 26), (16, 15, 25)): d.line([(x0, y), (x1, y)], fill=PAPER, width=2)
def sawmill(d, c):     # a fir tree
    for top, half, bottom in ((9, 7, 20), (15, 10, 28), (21, 13, 35)): d.polygon([(20, top), (20 + half, bottom), (20 - half, bottom)], fill=PAPER)
    d.rectangle([18, 35, 22, 40], fill=PAPER)

def camp(d, c):        # a campfire: the pioneer camp
    d.polygon([(20, 9), (27, 24), (24, 30), (16, 30), (13, 24)], fill=PAPER); d.polygon([(20, 17), (23, 26), (17, 26)], fill=c)
    d.line([(9, 37), (31, 31)], fill=PAPER, width=3); d.line([(9, 31), (31, 37)], fill=PAPER, width=3)

CAMP = 99  # the camp's number in a parcel's address (*CAMP-TOWN* in life.lisp)

TOWNS = [(river, (63, 111, 147)), (military, (92, 110, 62)), (factory, (163, 38, 44)), (university, (106, 58, 110)),
         (village, (201, 161, 58)), (port, (44, 74, 122)), (admin, (122, 42, 36)), (outpost, (74, 128, 148)),
         (mine, (58, 56, 56)), (resort, (216, 120, 42)), (rail, (90, 62, 38)), (sawmill, (60, 100, 72))]

def stamp(picture, colour):
    img = Image.new("RGBA", (W, H), (0, 0, 0, 0)); d = ImageDraw.Draw(img)
    d.rectangle([1, 1, W - 2, H - 2], fill=PAPER)
    for x in range(3, W - 2, 4): d.rectangle([x, 0, x + 1, 1], fill=PAPER); d.rectangle([x, H - 2, x + 1, H - 1], fill=PAPER)   # perforation
    for y in range(3, H - 2, 4): d.rectangle([0, y, 1, y + 1], fill=PAPER); d.rectangle([W - 2, y, W - 1, y + 1], fill=PAPER)
    d.rectangle([4, 4, W - 5, H - 5], fill=colour); d.rectangle([4, 4, W - 5, H - 5], outline=INK)
    picture(d, colour)
    return img

if __name__ == "__main__":
    OUT.mkdir(parents=True, exist_ok=True)
    for i, (picture, colour) in enumerate(TOWNS): stamp(picture, colour).save(OUT / f"{i}.png")
    stamp(camp, (194, 42, 34)).save(OUT / f"{CAMP}.png")
    print(len(TOWNS), "stamps ->", OUT)
