"""undraw.py FILE...: replace every (drawn (PARENT "NAME") ...) form: the first of a NAME by (model PARENT "NAME"), the others by nothing."""
import re, sys
seen = set()
for p in sys.argv[1:]:
    s = open(p, encoding="utf-8").read(); n = 0
    while True:
        m = re.search(r'\(drawn \((\S+) ("[^"]+"|\([a-z-]+ [a-z]+\))( :paint \(list [^()]*\))?\)', s)
        if not m: break
        i, depth = m.start(), 0
        while True:
            c = s[i]
            if c == '"':
                i += 1
                while s[i] != '"': i += 2 if s[i] == "\\" else 1
            elif c == ";":
                while s[i] != "\n": i += 1
            elif c == "#" and s[i + 1] == "\\": i += 2
            elif c == "(": depth += 1
            elif c == ")":
                depth -= 1
                if depth == 0: break
            i += 1
        key = m.group(2)
        new = "" if key in seen else '(model %s %s%s)' % (m.group(1), key, " 0.0 0.0 0.0" + m.group(3) if m.group(3) else "")
        seen.add(key); s = s[:m.start()] + new + s[i + 1:]; n += 1
    if n: open(p, "w", encoding="utf-8").write(s); print("undrawn", n, "in", p)
