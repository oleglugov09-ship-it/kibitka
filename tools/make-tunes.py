#!/usr/bin/env python3
"""Make the guitar and the radio for the wagon → project/sounds/guitar.wav, radio.wav.

Our own tunes from synthesized sounds: a plucked string (Karplus–Strong) and a "receiver" —
the same tune on a little organ, with crackle and a narrow band. Usage: tools/make-tunes.py"""
import math, random, struct, wave, pathlib

RATE = 22050
OUT = pathlib.Path(__file__).resolve().parent.parent / "project" / "sounds"

def note(n):                       # note frequency: 0 is the A above middle C (440 Hz)
    return 440.0 * 2 ** (n / 12)

def pluck(freq, secs, rng):
    """Pluck a string: noise in a ring buffer that smooths itself."""
    n = max(2, int(RATE / freq)); ring = [rng.uniform(-1, 1) for _ in range(n)]
    out = []
    for i in range(int(secs * RATE)):
        j = i % n
        out.append(ring[j])
        ring[j] = 0.996 * 0.5 * (ring[j] + ring[(j + 1) % n])
    return out

def mix(track, at, sound, gain=1.0):
    start = int(at * RATE)
    for i, s in enumerate(sound):
        if start + i < len(track):
            track[start + i] += s * gain

def save(name, track):
    peak = max(abs(s) for s in track) or 1.0
    with wave.open(str(OUT / name), "wb") as w:
        w.setnchannels(1); w.setsampwidth(2); w.setframerate(RATE)
        w.writeframes(b"".join(struct.pack("<h", int(30000 * s / peak)) for s in track))

# chords: (bass, three notes of the picking pattern) — A minor, F, C, G
CHORDS = [(-24, (-12, -9, -5)), (-28, (-16, -9, -4)), (-21, (-9, -5, 0)), (-26, (-14, -7, -2))]
TUNE = [0, 3, 7, 5, 3, 0, -2, 0, 3, 7, 8, 7, 5, 3, 2, 3]          # the melody on top

def guitar():
    rng = random.Random(7); beat = 0.42; bars = 8
    track = [0.0] * int(bars * 4 * beat * RATE)
    for bar in range(bars):
        bass, pick = CHORDS[bar % 4]
        t0 = bar * 4 * beat
        mix(track, t0, pluck(note(bass), 1.6, rng), 0.9)
        for k, p in enumerate((pick[0], pick[1], pick[2], pick[1], pick[0], pick[2], pick[1], pick[2])):
            mix(track, t0 + k * beat / 2, pluck(note(p), 1.1, rng), 0.55)
        if bar >= 4:
            for k in range(2):
                mix(track, t0 + k * 2 * beat, pluck(note(TUNE[(bar * 2 + k) % len(TUNE)] ), 1.4, rng), 0.7)
    save("guitar.wav", track)

def radio():
    rng = random.Random(11); beat = 0.5; bars = 8
    n = int(bars * 4 * beat * RATE); track = [0.0] * n
    for bar in range(bars):
        bass, _ = CHORDS[bar % 4]
        for k in range(4):
            t0 = (bar * 4 + k) * beat
            for f, g in ((note(TUNE[(bar * 4 + k) % len(TUNE)]), 0.6), (note(bass + 12), 0.35)):
                tone = [math.sin(2 * math.pi * f * i / RATE) * (1 + 0.3 * math.sin(2 * math.pi * 5.5 * i / RATE))
                        * min(1.0, i / 400) * max(0.0, 1 - i / (beat * RATE * 1.1)) for i in range(int(beat * RATE))]
                mix(track, t0, tone, g)
    # the receiver: a narrow band (two simple filters), crackle and wavering volume
    low = 0.0; prev = 0.0; out = []
    for i, s in enumerate(track):
        low += 0.35 * (s - low); high = low - prev; prev = low
        crackle = rng.uniform(-1, 1) * (0.05 + (0.5 if rng.random() < 0.0015 else 0.0))
        out.append((high * 3.0 + crackle) * (0.8 + 0.2 * math.sin(i / RATE * 0.7)))
    save("radio.wav", out)

def siren():
    """Make a siren: two tones taking turns."""
    n = int(1.2 * RATE); out = []
    for i in range(n):
        f = 660 if i < n // 2 else 880
        s = math.sin(2 * math.pi * f * i / RATE)
        out.append((1 if s > 0 else -1) * 0.35 + s * 0.65)
    save("siren.wav", out)

def mosquito():
    """Make a mosquito: a thin whine that drifts and trembles."""
    n = int(2.0 * RATE); out = []; phase = 0.0
    for i in range(n):
        t = i / RATE
        f = 620 + 60 * math.sin(2 * math.pi * 1.5 * t) + 25 * math.sin(2 * math.pi * 11 * t)
        phase += 2 * math.pi * f / RATE
        out.append((math.sin(phase) + 0.4 * math.sin(2 * phase)) * (0.6 + 0.4 * math.sin(2 * math.pi * 0.5 * t)))
    save("mosquito.wav", out)

# The final tune: C, G, A minor, F — and home to C. Per chord: (bass, three notes of the picking pattern).
FINALE_CHORDS = [(-21, (-9, -5, -2)), (-26, (-14, -10, -7)), (-24, (-12, -9, -5)), (-28, (-16, -12, -9))]
FINALE_TUNE = [(7, 10), (5, 2), (3, 0), (0, -4), (7, 10), (10, 5), (7, 3), (8, 3),
               (3, 7, 10, 7), (5, 2, 5, 10), (12, 10, 7, 3), (8, 7, 3, 0), (7, 10, 15, 10), (14, 10, 5, 2), (3, 0, 3, 7), (3, 0, -4, 0)]

def finale():
    """Make the tune for the closing credits: a guitar picks four chords, a melody joins and grows,
    a quiet organ comes in under it, and the last chord rings out. About 70 seconds, at half the rate."""
    rng = random.Random(23); beat = 0.5; bars = 4 + 2 * len(FINALE_TUNE) + 1
    track = [0.0] * int((bars * 4 * beat + 4) * RATE)
    for bar in range(bars - 1):
        bass, pick = FINALE_CHORDS[bar % 4]; t0 = bar * 4 * beat
        mix(track, t0, pluck(note(bass), 2.2, rng), 0.9)
        for k, q in enumerate((pick[0], pick[1], pick[2], pick[1], pick[0], pick[2], pick[1], pick[2])):
            mix(track, t0 + k * beat / 2, pluck(note(q), 1.3, rng), 0.45)
        if bar >= 4:
            tune = FINALE_TUNE[(bar - 4) % len(FINALE_TUNE)]; again = bar - 4 >= len(FINALE_TUNE)
            for k, n in enumerate(tune):
                mix(track, t0 + k * 4 * beat / len(tune), pluck(note(n), 1.8, rng), 0.8)
            if again:                                            # the second time round an organ holds the chord
                for q in pick:
                    f = note(q + 12)
                    mix(track, t0, [math.sin(2 * math.pi * f * i / RATE) * min(1.0, i / 3000) * min(1.0, (4 * beat * RATE - i) / 3000)
                                    for i in range(int(4 * beat * RATE))], 0.07)
    for k, n in enumerate((-21, -9, -5, -2, 3, 7)):               # the last chord, strummed
        mix(track, (bars - 1) * 4 * beat + k * 0.07, pluck(note(n), 5.0, rng), 0.8)
    half = [(track[i] + track[i + 1]) / 2 for i in range(0, len(track) - 1, 2)]
    peak = max(abs(v) for v in half) or 1.0
    with wave.open(str(OUT / "finale.wav"), "wb") as w:
        w.setnchannels(1); w.setsampwidth(2); w.setframerate(RATE // 2)
        w.writeframes(b"".join(struct.pack("<h", int(30000 * v / peak)) for v in half))

if __name__ == "__main__":
    import sys
    if sys.argv[1:] == ["finale"]: finale()
    else: guitar(); radio(); siren(); mosquito(); finale()
    print("guitar.wav, radio.wav, siren.wav, mosquito.wav, finale.wav →", OUT)
