#!/usr/bin/env python3
"""Names defined twice. The game is one package and one namespace: a second DEFUN or DEFPARAMETER of a name, in
another file, silently replaces the first, and whichever file loads later wins. That has stopped the game three
times (a train's car spacing against the pause between headlights; ZONE-AT; *LAMPS*). Run from `make web`, and by
hand: tools/names.py. Prints the names defined more than once and fails if there are any."""
import collections, glob, os, re, sys
root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
kinds = (("function", r"^\((?:defun|defmacro|defgeneric) \(?([^\s()]+)"),
         ("variable", r"^\((?:defvar|defparameter|defconstant) ([^\s()]+)"),
         ("structure", r"^\(defstruct \(?([^\s()]+)"))
bad = 0
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
sys.exit(1 if bad else 0)
