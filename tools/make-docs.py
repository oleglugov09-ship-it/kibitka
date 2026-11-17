#!/usr/bin/env python3
"""Draw the documents from the folder → project/textures/docs/*.png.

The driver's license, the trading permit, licenses, a certificate of merit, a stamped note, a fine receipt,
a task sheet, the desk. Heavy paper, borders, crested seals, typewritten lines — the paperwork of a small
country of cats. The name, the amounts and the task progress are written on top by the game itself.
Usage: tools/make-docs.py"""
import math, pathlib, random
from PIL import Image, ImageDraw, ImageFont

OUT = pathlib.Path(__file__).resolve().parent.parent / "project" / "textures" / "docs"
FONTS = ["/usr/share/fonts/truetype/dejavu/DejaVuSansMono-Bold.ttf", "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf",
         "/usr/share/fonts/truetype/liberation/LiberationMono-Bold.ttf"]

# The game renders at 640×360 and is stretched to the window, so a document on screen is exactly as many pixels
# as it takes up on the desk (the license is 186×118). We draw at that size right away, without antialiasing and with
# a font no smaller than 8 pixels: a large picture doesn't help here — it would be squeezed down to the same pixels anyway.
MIN_FONT = 8

# The English sheets are the same, line for line: the line width on the sheet is the same, so the translation is short.
LANG = "ru"
EN = {"ВОДИТЕЛЬСКОЕ УДОСТОВЕРЕНИЕ": "DRIVING LICENCE", "Выдано:": "Name:", "Категория: ДОМ-ГРУЗОВИК": "Class: HOUSE-TRUCK",
      "Область: Кольцевая": "Region: Ring", "Действительно бессрочно": "Valid for life", "ГАИ": "ROAD", "№ КБ 000417": "No. KB 000417",
      "РАЗРЕШЕНИЕ НА ТОРГОВЛЮ": "TRADING PERMIT",
      "Предъявителю разрешается розничная": "The bearer may sell any goods", "торговля с колёс всеми товарами,": "at retail from the wagon.",
      "Уголь каменный и авиагруз (ягода": "Coal and air freight (swamp", "болотная) — не более ДВУХ мест": "berries) — TWO pieces of each",
      "каждого. Свыше — оптовая торговля": "at most. More is wholesale and", "по отдельной лицензии.": "needs a separate licence.",
      "ТОРГ": "TRADE", "ЛИЦЕНЗИЯ": "LICENCE", "ГОРСОВЕТ": "COUNCIL", "Подпись: ~~~~": "Signed: ~~~~",
      "ПОЧЁТНАЯ": "DIPLOMA", "ГРАМОТА": "OF HONOUR", "За альбом области:": "For the album of", "пять её примет": "the region: five",
      "запечатлены и": "of its sights", "представлены в": "captured and", "горсовет.": "shown to council.", "Премия — 15 р.": "Prize — 15 rub.",
      "ЗА ТРУД": "LABOUR", "СПРАВКА": "CERTIFICATE", "Почтовые отправления": "The bearer's parcels", "предъявителя оплачиваются": "are paid at",
      "ВДВОЙНЕ.": "DOUBLE RATE.", "ОКНО № 3": "DESK 3", "ПОСТАНОВЛЕНИЕ О ШТРАФЕ": "PENALTY NOTICE", "Сумма:": "Amount:",
      "Оплатить в участке,": "Pay at the station,", "г. Октябрьский": "town of Oktyabrsky", "ЗАДАНИЕ ГОРСОВЕТА": "COUNCIL TASK",
      "нет бумаги": "no paper", "НА ТОРГОВЛЮ УГЛЁМ": "FOR COAL TRADE", "Разрешается провоз": "Permits carrying", "и продажа угля": "and selling coal",
      "каменного в розницу.": "at retail.", "Выдана за почтовый": "Issued for the", "рейс.": "mail run.", "НА АВИАГРУЗ": "FOR AIR FREIGHT",
      "и продажа ягоды": "and selling swamp", "болотной из посёлка": "berries from", "Дальний.": "Dalny.", "Выдана за чистый": "Issued for a clean",
      "город.": "town."}
def t(s): return EN.get(s, s) if LANG == "en" else s

def font(size, scale=None):
    """Return a font of SIZE: without SCALE — "a font of this size in sheet units" (the drawer picks the real one); with SCALE — a ready font."""
    if scale is None: return ("font", size)
    for f in FONTS:
        if pathlib.Path(f).exists(): return ImageFont.truetype(f, round(size * scale))
    return ImageFont.load_default()

class Scaled:
    """Drawer that takes sizes in sheet units and converts them to screen pixels; letters are never smaller than MIN_FONT."""
    def __init__(self, draw, k): self.d, self.k = draw, k; draw.fontmode = "1"
    def _pts(self, pts):
        if isinstance(pts[0], (int, float)): return [round(v * self.k) for v in pts]
        return [(round(x * self.k), round(y * self.k)) for x, y in pts]
    def rectangle(self, box, fill=None, outline=None, width=1): self.d.rectangle(self._pts(box), fill=fill, outline=outline, width=1)
    def ellipse(self, box, fill=None, outline=None, width=1): self.d.ellipse(self._pts(box), fill=fill, outline=outline, width=1)
    def line(self, pts, fill=None, width=1): self.d.line(self._pts(pts), fill=fill, width=1)
    def polygon(self, pts, fill=None, outline=None): self.d.polygon(self._pts(pts), fill=fill, outline=outline)
    def text(self, xy, text, font=None, fill=None):
        text = t(text)
        size = font[1] if isinstance(font, tuple) else 9
        real = globals()["font"](max(MIN_FONT, round(size * self.k)), 1)
        self.d.text((round(xy[0] * self.k), round(xy[1] * self.k)), text, font=real, fill=fill)

def paper(w, h, rgb, seed, edge=None, show=None):
    """Make a W×H sheet in its own units that takes SHOW pixels on the desk: paper texture, worn edges, a double border."""
    k = (show[0] / w) if show else 1.0
    pw, ph = (show if show else (w, h))
    rng = random.Random(seed); img = Image.new("RGB", (pw, ph), rgb); px = img.load()
    for y in range(ph):
        for x in range(pw):
            f = rng.uniform(.95, 1.03) * (1 - .10 * (min(x, y, pw - 1 - x, ph - 1 - y) < 1))
            px[x, y] = tuple(min(255, int(v * f)) for v in rgb)
    d = Scaled(ImageDraw.Draw(img), k); edge = edge or tuple(int(v * .5) for v in rgb)
    d.rectangle([4, 4, w - 5, h - 5], outline=edge)
    return img, d

def seal(d, x, y, r, rgb, text=""):
    """Draw a round seal: two rings, teeth, a star in the middle."""
    d.ellipse([x - r, y - r, x + r, y + r], outline=rgb, width=2); d.ellipse([x - r + 4, y - r + 4, x + r - 4, y + r - 4], outline=rgb)
    for i in range(24):
        a = i * math.pi / 12
        d.line([(x + math.cos(a) * (r - 3), y + math.sin(a) * (r - 3)), (x + math.cos(a) * (r - 1), y + math.sin(a) * (r - 1))], fill=rgb)
    pts = []
    for i in range(10):
        a = -math.pi / 2 + i * math.pi / 5; rr = (r * .5) if i % 2 == 0 else (r * .2)
        pts.append((x + math.cos(a) * rr, y + math.sin(a) * rr))
    d.polygon(pts, outline=rgb)
    text = t(text)
    if text: d.text((x - len(text) * 3, y + r * .55), text, font=font(8), fill=rgb)

def cat_photo(d, x, y, w, h):
    """Draw a photo card: a gray background and a cat's silhouette."""
    d.rectangle([x, y, x + w, y + h], fill=(168, 164, 150), outline=(60, 52, 44))
    cx = x + w // 2
    d.ellipse([cx - 14, y + 14, cx + 14, y + 38], fill=(70, 66, 62))
    d.polygon([(cx - 14, y + 20), (cx - 16, y + 6), (cx - 5, y + 15)], fill=(70, 66, 62)); d.polygon([(cx + 14, y + 20), (cx + 16, y + 6), (cx + 5, y + 15)], fill=(70, 66, 62))
    d.polygon([(cx - 18, y + h), (cx - 12, y + 40), (cx + 12, y + 40), (cx + 18, y + h)], fill=(70, 66, 62))
    for ex in (-6, 6): d.ellipse([cx + ex - 2, y + 24, cx + ex + 2, y + 28], fill=(220, 190, 70))

def lines(d, x, y, rows, rgb=(50, 44, 38), size=9, step=13):
    step = max(step, (MIN_FONT + 2) / d.k)                       # lines don't overlap even on a small sheet
    for i, t in enumerate(rows): d.text((x, y + i * step), t, font=font(size), fill=rgb)

def driver():
    img, d = paper(300, 190, (196, 178, 150), 1, show=(186, 118))
    d.rectangle([8, 8, 291, 30], fill=(122, 42, 36)); d.text((16, 12), "ВОДИТЕЛЬСКОЕ УДОСТОВЕРЕНИЕ", font=font(13), fill=(240, 232, 210))
    cat_photo(d, 16, 42, 70, 92)
    lines(d, 98, 44, ["Выдано:", "", "Категория: ДОМ-ГРУЗОВИК", "Область: Кольцевая", "Действительно бессрочно"])
    d.line([(98, 70), (280, 70)], fill=(90, 80, 70))
    seal(d, 246, 142, 30, (122, 42, 36), "ГАИ")
    d.text((16, 150), "№ КБ 000417", font=font(11), fill=(50, 44, 38))
    img.save(OUT / "driver.png")

def trade():
    img, d = paper(300, 190, (150, 176, 150), 2, show=(186, 118))
    d.rectangle([8, 8, 291, 30], fill=(44, 84, 60)); d.text((16, 12), "РАЗРЕШЕНИЕ НА ТОРГОВЛЮ", font=font(13), fill=(236, 240, 220))
    lines(d, 16, 44, ["Предъявителю разрешается розничная", "торговля с колёс всеми товарами,", "Уголь каменный и авиагруз (ягода", "болотная) — не более ДВУХ мест", "каждого. Свыше — оптовая торговля", "по отдельной лицензии."], size=10, step=15)
    seal(d, 246, 146, 28, (44, 84, 60), "ТОРГ")
    img.save(OUT / "trade.png")

def license(name, title, body, rgb, seed):
    img, d = paper(220, 290, rgb, seed, show=(136, 180))
    d.text((110 - len(t("ЛИЦЕНЗИЯ")) * 6, 16), "ЛИЦЕНЗИЯ", font=font(18), fill=(40, 36, 32))
    d.line([(20, 42), (200, 42)], fill=(40, 36, 32), width=2)
    title = t(title)
    d.text((110 - len(title) * 4, 52), title, font=font(12), fill=(122, 42, 36))
    lines(d, 20, 84, body, size=10, step=16)
    seal(d, 150, 226, 36, (122, 42, 36), "ГОРСОВЕТ")
    d.text((20, 250), "Подпись: ~~~~", font=font(10), fill=(40, 60, 110))
    img.save(OUT / f"{name}.png")

def honor():
    img, d = paper(220, 290, (226, 208, 150), 5, edge=(160, 110, 40), show=(136, 180))
    for i in range(0, 220, 14): d.polygon([(i, 8), (i + 7, 16), (i + 14, 8)], fill=(160, 110, 40))
    d.text((38, 30), "ПОЧЁТНАЯ", font=font(18), fill=(122, 42, 36)); d.text((50, 54), "ГРАМОТА", font=font(18), fill=(122, 42, 36))
    lines(d, 24, 100, ["За альбом области:", "пять её примет", "запечатлены и", "представлены в", "горсовет.", "", "Премия — 15 р."], size=11, step=17)
    seal(d, 110, 244, 30, (160, 110, 40), "ЗА ТРУД")
    img.save(OUT / "honor.png")

def stamp():
    img, d = paper(220, 150, (214, 210, 196), 6, show=(136, 93))
    d.text((20, 14), "СПРАВКА", font=font(16), fill=(40, 36, 32))
    lines(d, 20, 44, ["Почтовые отправления", "предъявителя оплачиваются", "ВДВОЙНЕ."], size=10, step=15)
    seal(d, 170, 104, 32, (40, 60, 130), "ОКНО № 3")
    img.save(OUT / "stamp.png")

def fine():
    img, d = paper(200, 120, (226, 176, 170), 7, edge=(150, 50, 44), show=(160, 96))
    d.rectangle([8, 8, 191, 28], fill=(150, 50, 44)); d.text((14, 11), "ПОСТАНОВЛЕНИЕ О ШТРАФЕ", font=font(11), fill=(250, 236, 226))
    lines(d, 14, 38, ["Сумма:", "", "Оплатить в участке,", "г. Октябрьский"], size=10, step=15)
    img.save(OUT / "fine.png")

def quest():
    img, d = paper(220, 150, (232, 226, 204), 8, show=(200, 136))
    d.text((20, 14), "ЗАДАНИЕ ГОРСОВЕТА", font=font(13), fill=(40, 36, 32)); d.line([(20, 34), (200, 34)], fill=(40, 36, 32))
    for y in (60, 82, 104, 126): d.line([(20, y), (200, y)], fill=(190, 182, 160))
    img.save(OUT / "quest.png")

def desk():
    rng = random.Random(9); img = Image.new("RGB", (320, 180)); px = img.load()
    for y in range(180):
        for x in range(320):
            v = 58 + int(10 * math.sin(y * .35 + 3 * math.sin(x * .02))) + rng.randint(-4, 4)
            px[x, y] = (v + 24, v + 6, v - 12)
    img.resize((640, 360), Image.NEAREST).save(OUT / "desk.png")

def guide():
    """Draw an open guidebook: two pages and a fold down the middle."""
    img, d = paper(600, 340, (226, 216, 190), 11, edge=(150, 120, 80))
    for i in range(10):
        v = 150 + i * 7
        d.line([(300 - i, 8), (300 - i, 331)], fill=(v, v - 10, v - 34)); d.line([(300 + i, 8), (300 + i, 331)], fill=(v, v - 10, v - 34))
    for x in (20, 316): d.line([(x, 32), (x + 264, 32)], fill=(150, 120, 80))
    img.save(OUT / "guide.png")

def slot():
    img = Image.new("RGBA", (220, 290), (0, 0, 0, 0)); d = ImageDraw.Draw(img)
    for i in range(0, 220, 12): d.line([(i, 2), (i + 6, 2)], fill=(210, 200, 170, 140)); d.line([(i, 287), (i + 6, 287)], fill=(210, 200, 170, 140))
    for i in range(0, 290, 12): d.line([(2, i), (2, i + 6)], fill=(210, 200, 170, 140)); d.line([(217, i), (217, i + 6)], fill=(210, 200, 170, 140))
    d.text((66, 130), t("нет бумаги"), font=font(13, 1), fill=(210, 200, 170, 170))
    img.save(OUT / "slot.png")

def everything():
    OUT.mkdir(parents=True, exist_ok=True)
    driver(); trade(); honor(); stamp(); fine(); quest(); desk(); slot(); guide()
    license("coal", "НА ТОРГОВЛЮ УГЛЁМ", ["Разрешается провоз", "и продажа угля", "каменного в розницу.", "", "Выдана за почтовый", "рейс."], (190, 186, 176), 3)
    license("air", "НА АВИАГРУЗ", ["Разрешается провоз", "и продажа ягоды", "болотной из посёлка", "Дальний.", "", "Выдана за чистый", "город."], (176, 190, 204), 4)
    print("документы →", OUT)

if __name__ == "__main__":
    everything()
    LANG, OUT = "en", OUT.parent / "docs-en"
    everything()
