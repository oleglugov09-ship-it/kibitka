#!/usr/bin/env python3
"""Synthesize the sounds of Kibitka — our own (the usage rights are ours).

    tools/make-sounds.py      → project/sounds/*.wav and project/discs/placeholder-*.wav

project/sounds/: outside (wind), bird-1..6 (bird voices — the game plays them one
at a time at random moments), rain (rain), water (a river), wheels (wheels and
creaking), step-1..4 (footsteps on planks). project/discs/: three placeholder tunes
for the record player. Your own discs are any .wav or .mp3 in project/discs/.
"""

import wave
from pathlib import Path

import numpy as np

RATE = 22050
ROOT = Path(__file__).resolve().parent.parent / "project"
rng = np.random.default_rng(3)

def t(sec):
    return np.arange(int(sec * RATE)) / RATE

def lowpass(x, alpha):
    y = np.empty_like(x); acc = 0.0
    for i, v in enumerate(x):
        acc += alpha * (v - acc); y[i] = acc
    return y

def lowpass_fast(x, alpha, passes=1):
    """Apply a one-pole filter by convolving with an exponential (faster than a loop)."""
    n = int(min(len(x), 6 / alpha))
    k = alpha * (1 - alpha) ** np.arange(n)
    for _ in range(passes):
        x = np.convolve(x, k)[: len(x)]
    return x

def loopify(x, fade=1.0):
    """Make a seamless loop: the tail is laid over the beginning."""
    n = int(fade * RATE)
    head, body, tail = x[:n], x[n:-n] if n else x, x[-n:]
    w = np.linspace(0, 1, n)
    return np.concatenate([tail * (1 - w) + head * w, body]) if n else x

def save(path, x, peak=.8):
    x = x / (np.abs(x).max() + 1e-9) * peak
    path.parent.mkdir(parents=True, exist_ok=True)
    with wave.open(str(path), "wb") as w:
        w.setnchannels(1); w.setsampwidth(2); w.setframerate(RATE)
        w.writeframes((x * 32767).astype(np.int16).tobytes())
    print(path.name)

# ---------- outdoors ----------

def outside(sec=40):
    """Make wind: slow gusts, no birds — the birds sound separately."""
    tt = t(sec + 1)
    wind = lowpass_fast(rng.normal(0, 1, len(tt)), .015, 2)
    wind *= .55 + .45 * np.sin(2 * np.pi * tt / 11) * np.sin(2 * np.pi * tt / 17.3 + 1)
    return loopify(wind)

def chirp(f0, f1, dur, vib=0.0, vib_hz=0.0, shape=2):
    n = int(dur * RATE); k = np.arange(n) / RATE
    f = np.linspace(f0, f1, n) + vib * np.sin(2 * np.pi * vib_hz * k)
    env = np.sin(np.linspace(0, np.pi, n)) ** shape
    return np.sin(np.cumsum(2 * np.pi * f / RATE)) * env

def bird(kind, seed):
    """Make one of six different voices: a two-note whistle, a trill, warbles, "twee-twee", a cascade, a turtle dove."""
    r = np.random.default_rng(seed)
    gap = lambda sec: np.zeros(int(sec * RATE))
    j = lambda: r.uniform(.94, 1.06)
    if kind == 1:
        out = [chirp(2900 * j(), 2850, .22, 30, 6), gap(.12), chirp(2400 * j(), 2350, .3, 30, 6)]
    elif kind == 2:
        out = []
        for i in range(r.integers(7, 12)):
            f = 4200 - i * 90
            out += [chirp(f * j(), f * .9, .045), gap(.035)]
    elif kind == 3:
        out = [chirp(2800 * j(), 3100, .75, 380, 23, 1)]
    elif kind == 4:
        out = [chirp(2100, 3900 * j(), .11), gap(.22), chirp(2100, 3900 * j(), .11), gap(.5), chirp(2300, 4100 * j(), .13)]
    elif kind == 5:
        out = []
        for i in range(5):
            f = 4000 - i * 300
            out += [chirp(f * j(), f * .95, .09), gap(.06)]
    else:
        out = [chirp(620 * j(), 600, .35, 8, 5, 1), gap(.15), chirp(560 * j(), 540, .55, 8, 5, 1)]
    x = np.concatenate(out + [gap(.3)])
    return lowpass_fast(x, .6)

def rain(sec=10):
    tt = t(sec + 1)
    n = rng.normal(0, 1, len(tt))
    hiss = n - lowpass_fast(n, .08)                        # the rustle is the high frequencies
    hiss = lowpass_fast(hiss, .5) * .5
    drops = np.zeros_like(tt)
    for _ in range(int(sec * 60)):                          # drops are short clicks
        s0 = rng.integers(0, len(tt) - 300); k = np.arange(300) / RATE
        drops[s0:s0 + 300] += np.sin(2 * np.pi * rng.uniform(1500, 4000) * k) * np.exp(-k * 400) * rng.uniform(.1, .5)
    return loopify(hiss + drops * .6)

def water(sec=12):
    tt = t(sec + 1)
    n = rng.normal(0, 1, len(tt))
    band = lowpass_fast(n, .25) - lowpass_fast(n, .04)
    gurgle = 1 + .5 * np.sin(2 * np.pi * tt * 3.1) * np.sin(2 * np.pi * tt * .7)
    return loopify(band * gurgle)

def wheels(sec=4.0):
    tt = t(sec)
    rumble = lowpass_fast(rng.normal(0, 1, len(tt)), .008, 2)
    rumble /= np.abs(rumble).max()
    rumble *= 1 + .3 * np.sin(2 * np.pi * tt * 2)
    out = rumble * .6
    for beat in np.arange(0, sec, 1.0):                      # the rim joint — a dull thud
        s = int(beat * RATE); n = int(.12 * RATE)
        k = np.arange(n) / RATE
        thump = np.sin(2 * np.pi * 95 * k) * np.exp(-k * 35) + rng.normal(0, .3, n) * np.exp(-k * 80)
        out[s:s + n] += thump * .9
    s = int(2.3 * RATE); n = int(.45 * RATE)                  # a creak once per turn of the wheel
    k = np.arange(n) / RATE
    f = 210 + 25 * np.sin(2 * np.pi * 7 * k)
    saw = ((np.cumsum(f / RATE)) % 1) * 2 - 1
    out[s:s + n] += lowpass_fast(saw, .3) * np.sin(np.linspace(0, np.pi, n)) * .12
    return out

def step(seed):
    r = np.random.default_rng(seed)
    n = int(.22 * RATE); k = np.arange(n) / RATE
    f = r.uniform(80, 120)
    thud = np.sin(2 * np.pi * f * k) * np.exp(-k * 28)
    knock = lowpass_fast(r.normal(0, 1, n), .5) * np.exp(-k * 90) * .5
    creak = np.sin(2 * np.pi * r.uniform(500, 700) * k * (1 + k)) * np.exp(-k * 18) * r.uniform(0, .08)
    return thud + knock + creak

# ---------- placeholder discs ----------

def note_freq(midi):
    return 440 * 2 ** ((midi - 69) / 12)

def voice(midi, dur, kind, vol):
    n = int(dur * RATE); k = np.arange(n) / RATE
    f = note_freq(midi)
    ph = (f * k) % 1
    if kind == "square": w = np.where(ph < .5, 1.0, -1.0); w = lowpass_fast(w, .2)
    elif kind == "tri": w = 4 * np.abs(ph - .5) - 1
    else: w = np.sin(2 * np.pi * f * k)
    a = min(n, int(.01 * RATE)); env = np.ones(n); env[:a] = np.linspace(0, 1, a)
    env *= np.exp(-k * (2.5 if kind == "square" else 1.2))
    env[-min(n, 200):] *= np.linspace(1, 0, min(n, 200))
    return w * env * vol

def disc(seed, root, bpm, bars=16):
    r = np.random.default_rng(seed)
    beat = 60 / bpm
    scale = [0, 2, 4, 7, 9, 12, 14, 16]                       # pentatonic scale
    prog = [[0, 4, 7], [9, -3, 4], [5, 9, 12], [7, 11, 14]]    # I vi IV V
    total = bars * 4 * beat
    out = np.zeros(int(total * RATE) + RATE)
    def put(x, at):
        s = int(at * RATE); out[s:s + len(x)] += x[: len(out) - s]
    motif = [r.integers(0, len(scale)) for _ in range(8)]
    for bar in range(bars):
        ch = prog[bar % 4]
        t0 = bar * 4 * beat
        put(voice(root - 12 + ch[0], beat * 2, "tri", .5), t0)          # bass
        put(voice(root - 12 + ch[0] + 7, beat * 2, "tri", .4), t0 + 2 * beat)
        for i in range(4):                                             # picked chord
            put(voice(root + ch[i % 3], beat * .9, "sine", .12), t0 + i * beat + beat / 2)
        m = motif if bar % 4 != 3 else [x + r.integers(-1, 2) for x in motif]
        for i, deg in enumerate(m):                                    # melody in eighth notes
            if r.random() < .2: continue
            put(voice(root + 12 + scale[int(np.clip(deg, 0, len(scale) - 1))], beat * .45, "tri", .3), t0 + i * beat / 2)   # soft melody
        for i in range(8):                                             # hi-hat
            n = int(.03 * RATE)
            put(lowpass_fast(r.normal(0, 1, n), .4) * np.exp(-np.arange(n) / RATE * 150) * .02, t0 + i * beat / 2)
    return out[: int(total * RATE)]

if __name__ == "__main__":
    save(ROOT / "sounds" / "outside.wav", outside(), .7)
    for i in range(1, 7):
        save(ROOT / "sounds" / f"bird-{i}.wav", bird(i, 50 + i), .6)
    save(ROOT / "sounds" / "rain.wav", rain(), .6)
    save(ROOT / "sounds" / "water.wav", water(), .6)
    save(ROOT / "sounds" / "wheels.wav", wheels(), .8)
    for i in range(4):
        save(ROOT / "sounds" / f"step-{i + 1}.wav", step(i), .8)
    for i, (root, bpm) in enumerate([(60, 96), (67, 112), (62, 80)]):
        save(ROOT / "discs" / f"placeholder-{i + 1}.wav", disc(40 + i, root, bpm), .5)
