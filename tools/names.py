#!/usr/bin/env python3
"""Names defined twice. The game is one package and one namespace: a second DEFUN or DEFPARAMETER of a name, in
another file, silently replaces the first, and whichever file loads later wins. That has stopped the game three
times (a train's car spacing against the pause between headlights; ZONE-AT; *LAMPS*). Run from `make web`, and by
hand: tools/names.py. Prints the names defined more than once and fails if there are any.

It also catches a name that is gone but still used. ECL's compiler says nothing about a call of a function that does
not exist, so a function renamed in one file and still called under its old name in another is found only when
the game runs into that call. Here: a name that was defined in lisp/ at the last commit (or 25 commits back), is
defined nowhere now, and still appears in the code, the mods or the tools."""
import collections, glob, os, re, subprocess, sys
root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
kinds = (("function", r"^\((?:defun|defmacro|defgeneric|defshared) \(?([^\s()]+)"),
         ("variable", r"^\((?:defvar|defparameter|defconstant) ([^\s()]+)"),
         ("structure", r"^\(defstruct \(?([^\s()]+)"))
def fields(text):
    """The functions DEFINE-FIELDS defines in TEXT: (define-fields quest key need) makes QUEST-KEY and QUEST-NEED."""
    return [f"{record}-{field}" for record, rest in re.findall(r"^\(define-fields (\S+) ([^()]*)\)", text, re.M) for field in rest.split()]


bad = 0
places = collections.defaultdict(list)
for path in sorted(glob.glob(os.path.join(root, "lisp", "*.lisp"))):
    text = open(path, encoding="utf-8").read()
    for name in fields(text) + re.findall(kinds[0][1], text, re.M):
        places[name].append(os.path.basename(path))
for name, where in sorted(places.items()):
    if len(where) > 1 and name in {f for p in glob.glob(os.path.join(root, "lisp", "*.lisp")) for f in fields(open(p, encoding="utf-8").read())}:
        bad += 1
        print(f"function {name} is defined {len(where)} times, once by DEFINE-FIELDS: {', '.join(where)}")
for kind, pattern in kinds:
    places = collections.defaultdict(list)
    for path in sorted(glob.glob(os.path.join(root, "lisp", "*.lisp"))):
        for n, line in enumerate(open(path, encoding="utf-8"), 1):
            m = re.match(pattern, line)
            if m: places[m.group(1)].append(f"{os.path.basename(path)}:{n}")
    for name, where in sorted(places.items()):
        if len(where) > 1:
            bad += 1
            print(f"{kind} {name} is defined {len(where)} times: {', '.join(where)}")

def strip(text):
    """TEXT without Lisp strings and comments."""
    return re.sub(r';[^\n]*', '', re.sub(r'"(?:\\.|[^"\\])*"', '""', text))


now = set()
for _kind, pattern in kinds:
    for path in glob.glob(os.path.join(root, "lisp", "*.lisp")):
        now.update(re.findall(pattern, open(path, encoding="utf-8").read(), re.M))
for path in glob.glob(os.path.join(root, "lisp", "*.lisp")):
    now.update(fields(open(path, encoding="utf-8").read()))
before = set()
for rev in ("HEAD", "HEAD~25"):
    for _kind, pattern in kinds:
        got = subprocess.run(["git", "-C", root, "grep", "-h", "-E", pattern.replace("(?:", "(").replace("\\s", "[:space:]"), rev, "--", "lisp"],
                             capture_output=True, text=True).stdout
        before.update(re.findall(pattern, got, re.M))
used = collections.defaultdict(list)
lisp = glob.glob(os.path.join(root, "lisp", "*.lisp")) + glob.glob(os.path.join(root, "project", "mods", "*", "*.lisp")) \
    + glob.glob(os.path.join(root, "tools", "*.lisp"))
# In Lisp a function counts as used where it is called or named: (name ...), #'name, 'name. A bare word may be a
# local variable that happens to share the name of a function that is gone, and ((name ...) is the first binding
# of a LET. A *variable* counts anywhere. In the Python tools only words with a hyphen or stars count.
# tools/was-now.py is skipped: it names functions of old builds on purpose.
calls = r"""(?:(?<!\()\(|\#'|')([^\s()'`,"#;]+)|(\*[^\s()'`,"#;]+\*)"""
for path in lisp + glob.glob(os.path.join(root, "tools", "*.py")):
    if os.path.basename(path) in ("names.py", "was-now.py"): continue
    text = open(path, encoding="utf-8").read()
    if path.endswith(".lisp"):
        tokens = {a or b for a, b in re.findall(calls, strip(text))}
    else:
        tokens = {w for w in re.findall(r"""[^\s()'`,"#;:\[\]{}]+""", text) if "-" in w or w.startswith("*")}
    for token in tokens:
        used[token.lower()].append(os.path.relpath(path, root))
for name in sorted(before - now):
    if name in used:
        bad += 1
        print(f"{name} is defined nowhere any more, but is still used in: {', '.join(sorted(used[name])[:6])}")
sys.exit(1 if bad else 0)
