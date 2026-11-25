"""A small kit for drawing bus stops in Blender from a script: solid pieces with real thickness, gathered into one
mesh per material (one draw each in the game). Blender axes: x away from the road (the back wall is at +x), y along
the road, z up; the floor is z 0. Names of materials are the game's: "#rrggbb" or "paintN" (repainted per place)."""
import bpy, bmesh, math
from mathutils import Vector, Matrix

PREVIEW = {"paint0": "#3f6f73", "paint1": "#a8843c", "paint2": "#8a4038", "paint3": "#55708a",
           "paint4": "#b0aa98", "paint5": "#6a8a5a", "paint6": "#c9a13a", "paint7": "#7a5a7a"}


def rgb(hexa):
    h = hexa.lstrip("#")
    return [pow(int(h[i:i + 2], 16) / 255.0, 2.2) for i in (0, 2, 4)] + [1.0]


class Model:
    def __init__(self):
        for o in list(bpy.data.objects): bpy.data.objects.remove(o, do_unlink=True)
        for m in list(bpy.data.meshes): bpy.data.meshes.remove(m)
        for m in list(bpy.data.materials): bpy.data.materials.remove(m)
        self.parts = {}            # material name -> bmesh
        self.flags = {}            # material name -> name suffix (_noshadow, _r110)
        self.solids = []

    def bm(self, mat, flags=""):
        if mat not in self.parts:
            self.parts[mat] = bmesh.new(); self.flags[mat] = flags
        return self.parts[mat]

    # ---- pieces
    def box(self, mat, lo, hi, bevel=0.0, flags=""):
        """A box from corner LO to corner HI; with BEVEL its edges are chamfered by that much."""
        bm = self.bm(mat, flags); tmp = bmesh.new()
        bmesh.ops.create_cube(tmp, size=1.0)
        size = Vector(hi) - Vector(lo); mid = (Vector(hi) + Vector(lo)) / 2
        bmesh.ops.scale(tmp, vec=size, verts=tmp.verts); bmesh.ops.translate(tmp, vec=mid, verts=tmp.verts)
        if bevel > 0:
            bmesh.ops.bevel(tmp, geom=list(tmp.edges), offset=bevel, segments=1, affect='EDGES', profile=0.5)
        self._merge(bm, tmp)

    def prism(self, mat, profile, y0, y1, flags="", axis="y"):
        """A flat PROFILE, a list of (a, b) points going round once, pushed through from Y0 to Y1. With AXIS "y" the
        points are (x, z) and the piece runs along the road; with "x" they are (y, z) and it runs away from it."""
        bm = self.bm(mat, flags); tmp = bmesh.new()
        if axis == "y": verts = [tmp.verts.new((a, y0, b)) for a, b in profile]
        else: verts = [tmp.verts.new((y0, a, b)) for a, b in profile]
        face = tmp.faces.new(verts)
        vec = (0, y1 - y0, 0) if axis == "y" else (y1 - y0, 0, 0)
        res = bmesh.ops.extrude_face_region(tmp, geom=[face])
        bmesh.ops.translate(tmp, vec=vec, verts=[v for v in res["geom"] if isinstance(v, bmesh.types.BMVert)])
        bmesh.ops.recalc_face_normals(tmp, faces=tmp.faces)
        self._merge(bm, tmp)

    def tube(self, mat, a, b, r0, r1=None, n=14, flags="", caps=True):
        """A round bar from point A to point B, R0 thick at A and R1 at B; its side is smooth."""
        r1 = r0 if r1 is None else r1
        bm = self.bm(mat, flags); tmp = bmesh.new()
        a, b = Vector(a), Vector(b); axis = (b - a); length = axis.length
        rot = axis.to_track_quat('Z', 'Y').to_matrix().to_4x4()
        ring0 = [tmp.verts.new((r0 * math.cos(2 * math.pi * i / n), r0 * math.sin(2 * math.pi * i / n), 0)) for i in range(n)]
        ring1 = [tmp.verts.new((r1 * math.cos(2 * math.pi * i / n), r1 * math.sin(2 * math.pi * i / n), length)) for i in range(n)]
        for i in range(n):
            f = tmp.faces.new((ring0[i], ring0[(i + 1) % n], ring1[(i + 1) % n], ring1[i])); f.smooth = True
        if caps:                                           # the ends have points of their own: flat, with a crisp edge
            c0 = [tmp.verts.new(v.co) for v in ring0]; c1 = [tmp.verts.new(v.co) for v in ring1]
            tmp.faces.new(list(reversed(c0))); tmp.faces.new(c1)
        bmesh.ops.transform(tmp, matrix=Matrix.Translation(a) @ rot, verts=tmp.verts)
        self._merge(bm, tmp)

    def disc(self, mat, centre, r, depth, n=24, flags="", face="-x"):
        """A round plate R wide and DEPTH thick, standing on the wall and facing the road."""
        c = Vector(centre)
        self.tube(mat, c, c + Vector((-depth, 0, 0)), r, r, n=n, flags=flags)

    def shell(self, mat, points, thick, y0, y1, flags=""):
        """A curved slab: POINTS (x, z) are its upper line seen from the side, THICK its thickness, from Y0 to Y1."""
        pts = [Vector((x, 0, z)) for x, z in points]; lower = []
        for i, p in enumerate(pts):
            d = (pts[min(i + 1, len(pts) - 1)] - pts[max(i - 1, 0)]).normalized()
            nrm = Vector((d.z, 0, -d.x))                    # to the right of the way we walk: downwards for a roof
            if nrm.z > 0: nrm = -nrm
            lower.append(p + nrm * thick)
        prof = [(p.x, p.z) for p in pts] + [(p.x, p.z) for p in reversed(lower)]
        self.prism(mat, prof, y0, y1, flags)

    def solid(self, lo, hi):
        """What cannot be walked through: not drawn, the game reads it."""
        self.solids.append((lo, hi))

    def _merge(self, bm, tmp):
        me = bpy.data.meshes.new("tmp"); tmp.to_mesh(me); tmp.free(); bm.from_mesh(me); bpy.data.meshes.remove(me)

    # ---- out
    def material(self, name):
        m = bpy.data.materials.get(name)
        if m: return m
        m = bpy.data.materials.new(name); m.use_nodes = True
        shader = next(n for n in m.node_tree.nodes if n.type == "BSDF_PRINCIPLED")
        colour = rgb(PREVIEW.get(name, name.split()[0]))
        shader.inputs["Base Color"].default_value = colour
        shader.inputs["Roughness"].default_value = 0.85
        m.diffuse_color = colour
        return m

    def save(self, path):
        triangles = 0
        for i, (mat, bm) in enumerate(self.parts.items()):
            bmesh.ops.triangulate(bm, faces=bm.faces)
            triangles += len(bm.faces)
            me = bpy.data.meshes.new(f"{i:02d}_{mat.split()[0]}"); bm.to_mesh(me); bm.free()
            ob = bpy.data.objects.new(f"{i:02d}_{mat.split()[0]}{self.flags[mat]}", me)
            me.materials.append(self.material(mat))
            bpy.context.scene.collection.objects.link(ob)
        for i, (lo, hi) in enumerate(self.solids):
            me = bpy.data.meshes.new("solid"); bm = bmesh.new(); bmesh.ops.create_cube(bm, size=1.0); bm.to_mesh(me); bm.free()
            ob = bpy.data.objects.new("solid" if i == 0 else f"solid.{i:03d}", me)
            ob.location = (Vector(lo) + Vector(hi)) / 2; ob.scale = Vector(hi) - Vector(lo)
            ob.display_type = 'WIRE'; ob.hide_render = True
            bpy.context.scene.collection.objects.link(ob)
        bpy.ops.wm.save_as_mainfile(filepath=path)
        print("SAVED", path, "треугольников", triangles, "материалов", len(self.parts))


# ---- more pieces (added for the other stops and for the houses)

def arch(cy, cz, r_out, r_in, a0=0.0, a1=math.pi, n=16):
    """The outline of a band bent into an arc in the plane of the wall: points (y, z), for Model.prism(..., axis="x")."""
    outer = [(cy + r_out * math.cos(a0 + (a1 - a0) * i / n), cz + r_out * math.sin(a0 + (a1 - a0) * i / n)) for i in range(n + 1)]
    inner = [(cy + r_in * math.cos(a0 + (a1 - a0) * i / n), cz + r_in * math.sin(a0 + (a1 - a0) * i / n)) for i in range(n + 1)]
    return outer + list(reversed(inner))


def star(cy, cz, r, points=5, inner=0.42, turn=math.pi / 2):
    """The outline of a star: points (y, z)."""
    return [(cy + (r if i % 2 == 0 else r * inner) * math.cos(turn + math.pi * i / points),
             cz + (r if i % 2 == 0 else r * inner) * math.sin(turn + math.pi * i / points)) for i in range(2 * points)]


def _lathe(self, mat, centre, profile, n=20, flags="", smooth=True, a0=0.0, a1=None, inside=False):
    """PROFILE, points (radius, z) from the bottom up, turned round the upright through CENTRE (x, y). With A0 and
    A1 only that part of the turn is made (an open shell, seen from outside; with INSIDE, from inside)."""
    bm = self.bm(mat, flags); tmp = bmesh.new(); cx, cy = centre
    whole = a1 is None
    steps = n if whole else n + 1
    span = 2 * math.pi if whole else a1 - a0
    rings = [[tmp.verts.new((cx + r * math.cos(a0 + span * i / n), cy + r * math.sin(a0 + span * i / n), z)) for i in range(steps)]
             for r, z in profile]
    for a, b in zip(rings, rings[1:]):
        for i in range(n if whole else n):
            k = (i + 1) % steps if whole else i + 1
            quad = (a[i], a[k], b[k], b[i])
            f = tmp.faces.new(tuple(reversed(quad)) if inside else quad); f.smooth = smooth
    bmesh.ops.remove_doubles(tmp, verts=tmp.verts, dist=1e-5)
    bmesh.ops.dissolve_degenerate(tmp, edges=tmp.edges, dist=1e-5)
    if whole: bmesh.ops.recalc_face_normals(tmp, faces=tmp.faces)
    self._merge(bm, tmp)


def _pyramid(self, mat, lo, hi, z0, apex, flags=""):
    """A roof with four slopes: over the rectangle LO..HI (x, y) at height Z0, up to the point APEX (x, y, z)."""
    bm = self.bm(mat, flags); tmp = bmesh.new()
    c = [tmp.verts.new((x, y, z0)) for x, y in ((lo[0], lo[1]), (hi[0], lo[1]), (hi[0], hi[1]), (lo[0], hi[1]))]
    tmp.faces.new(list(reversed(c)))
    for i in range(4):
        tmp.faces.new((tmp.verts.new(c[i].co), tmp.verts.new(c[(i + 1) % 4].co), tmp.verts.new(apex)))
    bmesh.ops.recalc_face_normals(tmp, faces=tmp.faces)
    self._merge(bm, tmp)


def _point(self, name, at):
    """A place the game reads by name (at_NAME): not drawn."""
    self.points = getattr(self, "points", []) + [(name, at)]


_old_save = Model.save


def _save(self, path):
    for name, at in getattr(self, "points", []):
        e = bpy.data.objects.new("at_" + name, None); e.location = at; e.empty_display_size = 0.3
        bpy.context.scene.collection.objects.link(e)
    self.points = []
    _old_save(self, path)


Model.lathe, Model.pyramid, Model.point, Model.save = _lathe, _pyramid, _point, _save


import random as _random


def _blob(self, mat, centre, radii, seed=0, detail=1, rough=0.18, flags=""):
    """A lump of foliage or of stone: a faceted ball RADII (x, y, z) big round CENTRE, its points pushed in and out
    by ROUGH, the same for the same SEED."""
    bm = self.bm(mat, flags); tmp = bmesh.new(); rnd = _random.Random(seed)
    bmesh.ops.create_icosphere(tmp, subdivisions=detail, radius=1.0)
    for v in tmp.verts:
        k = 1.0 + rnd.uniform(-rough, rough)
        v.co = Vector((v.co.x * radii[0] * k + centre[0], v.co.y * radii[1] * k + centre[1], v.co.z * radii[2] * k + centre[2]))
    self._merge(bm, tmp)


Model.blob = _blob
