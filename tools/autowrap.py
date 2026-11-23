"""autowrap.py FILE SPEC...   wrap runs of fixed shapes in (drawn (PARENT "NAME") ...).
SPEC is  theme                 every (let ((b ...)) ...) inside every deftheme: names theme-KEY-N
     or  FUNCTION:PARENT:NAME  the body of that defun (or of the form starting with FUNCTION text)
A form is fixed when it builds shapes and calls nothing that moves, glows on its own, speaks or is random, and
names no variable of a loop around it or parameter of its function."""
import re, sys
IMPURE = ["moving", "light-beam", "chimney-smoke", "sign", "red-light", "flood", "diner-cat", "build-prop", "mark-cow", "group", "random",
          "light", "vr", "life-cat", "make-cat", "lamp", "live", "setf", "push", "mural", "mural-quad", "end-mosaic", "window-rows", "body",
          "mega-slab", "kvas", "parked-car", "stamp", "model", "drawn", "player", "object-set", "usable"]
CONSTANTS = {"*way-x*", "*water-rgb*", "*floor*"}

def parse(s):
    """Return the top-level forms as nodes (start, end, kids); atoms have kids None."""
    i, n, stack, top = 0, len(s), [], []
    while i < n:
        c = s[i]
        if c == ";":
            while i < n and s[i] != "\n": i += 1
        elif c == '"':
            j = i + 1
            while s[j] != '"': j += 2 if s[j] == "\\" else 1
            (stack[-1][2] if stack else top).append((i, j + 1, None)); i = j
        elif c == "#" and s[i + 1] == "\\": 
            (stack[-1][2] if stack else top).append((i, i + 3, None)); i += 2
        elif c == "(":
            stack.append([i, None, []])
        elif c == ")":
            node = stack.pop(); node[1] = i + 1
            (stack[-1][2] if stack else top).append(tuple(node))
        elif not c.isspace() and c not in "'`,#":
            j = i
            while j < n and not s[j].isspace() and s[j] not in '()";': j += 1
            (stack[-1][2] if stack else top).append((i, j, None)); i = j - 1
        i += 1
    return top

def head(s, node): 
    k = node[2]
    return s[k[0][0]:k[0][1]] if k and k[0][2] is None else ""
def atoms(s, node):
    if node[2] is None: yield s[node[0]:node[1]]
    else:
        for k in node[2]: yield from atoms(s, k)
def heads(s, node):
    if node[2] is not None:
        h = head(s, node)
        if h: yield h
        for k in node[2]: yield from heads(s, k)
def binders(s, node):
    """The variables NODE binds for its body, if it is a loop or a binding form."""
    h = head(s, node); k = node[2]; out = set()
    if h in ("dolist", "dotimes") and len(k) > 1 and k[1][2]: out |= {s[k[1][2][0][0]:k[1][2][0][1]]}
    elif h == "loop":
        for a, b in zip(k, k[1:]):
            if a[2] is None and s[a[0]:a[1]] == "for": out |= set(atoms(s, b))
    elif h == "destructuring-bind" and len(k) > 1: out |= set(atoms(s, k[1]))
    elif h in ("let", "let*", "flet") and len(k) > 1 and k[1][2]:
        for b in k[1][2]:
            if b[2]: out |= {s[b[2][0][0]:b[2][0][1]]}
            elif b[2] is None: out |= {s[b[0]:b[1]]}
    return out

def pure(s, node, env, parent):
    hs = set(heads(s, node))
    if not (hs & {"shape", "beam-between"}) or (hs & set(IMPURE)): return False
    own = set()
    def collect(nd):
        if nd[2] is not None:
            own.update(binders(s, nd))
            for k in nd[2]: collect(k)
    collect(node)
    names = set(atoms(s, node))
    if any(len(a) > 2 and a[0] == '*' and a[-1] == '*' and a not in CONSTANTS for a in names): return False     # the state of the game
    if (names & env) - own - {parent}: return False
    # every shape must hang under PARENT or a group... groups are impure, so: under PARENT
    def ok(nd):
        if nd[2] is None: return True
        if head(s, nd) in ("shape", "beam-between"):
            return len(nd[2]) > 1 and s[nd[2][1][0]:nd[2][1][1]] == parent
        return all(ok(k) for k in nd[2])
    return ok(node)

def runs(s, body, env, parent):
    out, cur = [], []
    for f in body:
        if f[2] is not None and pure(s, f, env, parent): cur.append(f)
        else:
            if cur: out.append((cur[0][0], cur[-1][1])); cur = []
    if cur: out.append((cur[0][0], cur[-1][1]))
    return out

path, specs = sys.argv[1], sys.argv[2:]
s = open(path, encoding="utf-8").read(); top = parse(s); edits = []
for spec in specs:
    if spec.startswith("theme"):
        suffix = spec[6:]
        for form in top:
            if head(s, form) != "deftheme": continue
            key = s[form[2][1][0]:form[2][1][1]].lstrip(":"); count = [0]
            def walk(nd, env):
                if nd[2] is None: return
                h = head(s, nd)
                if h in ("let", "let*") and nd[2][1][2] and any(b[2] and s[b[2][0][0]:b[2][0][1]] == "b" for b in nd[2][1][2]):
                    count[0] += 1
                    for a, b in runs(s, nd[2][2:], env, "b"): edits.append((a, b, "b", '"theme-%s-%d%s"' % (key, count[0], suffix)))
                    return
                env2 = env | binders(s, nd)
                for k in nd[2]: walk(k, env2)
            walk(form, set())
    elif spec == "booth":
        for form in top:
            if head(s, form) != "defbooth": continue
            num = s[form[2][1][0]:form[2][1][1]]
            for a, b in zip(form[2], form[2][1:]):
                if a[2] is None and s[a[0]:a[1]] == ":build" and head(s, b) == "lambda":
                    for x, y in runs(s, b[2][2:], set(), "g"): edits.append((x, y, "g", '"booth-%s-extra"' % num))
    else:
        fn, parent, name = spec.split(":")
        for form in top:
            if head(s, form) in ("defun",) and s[form[2][1][0]:form[2][1][1]] == fn:
                params = set(atoms(s, form[2][2])) - {parent, "&optional", "&key", "&rest"}
                body = form[2][3:]
                while len(body) == 1 and head(s, body[0]) in ("let", "let*", "destructuring-bind", "flet", "progn"):     # look inside a single wrapper
                    if head(s, body[0]) in ("let", "let*"):               # a name for a constant is not a variable
                        for b in body[0][2][1][2]:
                            if b[2] and not (len(b[2]) == 2 and b[2][1][2] is None and s[b[2][0][0]:b[2][0][1]] != parent): params.add(s[b[2][0][0]:b[2][0][1]])
                        params.discard(parent)
                    else: params |= binders(s, body[0]) - {parent}
                    body = body[0][2][3:] if head(s, body[0]) == "destructuring-bind" else body[0][2][2:] if head(s, body[0]) != "progn" else body[0][2][1:]
                for a, b in runs(s, body, params, parent): edits.append((a, b, parent, '"%s"' % name))
for a, b, parent, name in sorted(edits, reverse=True):
    s = s[:a] + "(drawn (%s %s) " % (parent, name) + s[a:b] + ")" + s[b:]
open(path, "w", encoding="utf-8").write(s)
print(path, len(edits), "runs wrapped:", sorted({e[3] for e in edits}))
