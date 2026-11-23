#!/usr/bin/env python3
"""Counts that must not grow. Each is a habit of this code that makes it fragile; the plan is to bring them down
(see the plan of putting the code in order), and this keeps them from creeping back up meanwhile.

    tools/ratchet.py          compare with tools/ratchet.txt and fail if a count has grown
    tools/ratchet.py --set    write the present counts down (after bringing one down, or when a rise is meant)

foreign-writes   a global variable changed (SETF, INCF, PUSH...) in a file other than the one that defines it.
                 net, sync, live and dev are left out: the network and the developer's tools write everything.
positional       THIRD ... TENTH and NTH: a record read by the place of a field instead of its name.
guest-checks     NET-GUEST-P outside net and sync: game code asking whether it is the driver.
late-binding     (funcall 'name), (symbol-value 'name), (fboundp 'name), (boundp 'name): a file reaching for
                 something that is loaded after it.
"""
import collections, glob, os, re, sys
root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
book = os.path.join(root, "tools", "ratchet.txt")


def strip(text):
    return re.sub(r';[^\n]*', '', re.sub(r'"(?:\\.|[^"\\])*"', '""', text))


code = {os.path.basename(p): strip(open(p, encoding="utf-8").read())
        for p in sorted(glob.glob(os.path.join(root, "lisp", "*.lisp"))) if os.path.basename(p) != "en.lisp"}
owner = {}
for name, text in code.items():
    for var in re.findall(r'^\(def(?:var|parameter) (\*[^\s()]+\*)', text, re.M):
        owner[var] = name
write = re.compile(r'\((?:setf|setq|incf|decf|push|pop|pushnew|remf|rotatef)\s+(?:\([a-z0-9-]+\s+(?:[^()\s]+\s+)*?)*(\*[a-z0-9<>-]+\*)')
outside = ("net.lisp", "sync.lisp", "live.lisp", "dev.lisp")
now = collections.OrderedDict()
now["foreign-writes"] = sum(1 for name, text in code.items() if name not in outside
                            for var in write.findall(text) if owner.get(var, name) != name)
now["positional"] = sum(len(re.findall(r'\((?:third|fourth|fifth|sixth|seventh|eighth|ninth|tenth|nth) ', text)) for text in code.values())
now["guest-checks"] = sum(text.count("(net-guest-p)") for name, text in code.items() if name not in ("net.lisp", "sync.lisp"))
now["late-binding"] = sum(len(re.findall(r"\((?:funcall|symbol-value|fboundp|boundp) '", text)) for text in code.values())

if "--set" in sys.argv:
    with open(book, "w") as f:
        for key, n in now.items(): f.write(f"{key} {n}\n")
    print("ratchet: записано:", ", ".join(f"{k} {n}" for k, n in now.items()))
    sys.exit(0)
was = dict(line.split() for line in open(book)) if os.path.exists(book) else {}
bad = 0
for key, n in now.items():
    limit = int(was.get(key, n))
    if n > limit:
        bad += 1
        print(f"ratchet: {key} выросло: было {limit}, стало {n}")
    elif n < limit:
        print(f"ratchet: {key} уменьшилось: было {limit}, стало {n} — запиши: tools/ratchet.py --set")
sys.exit(1 if bad else 0)
