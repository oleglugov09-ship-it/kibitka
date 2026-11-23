"""Make a Blender file of a piece of the game written down by (drawn ...) (lisp/models.lisp).

    blender -b --python tools/draw-model.py -- FOLDER/NAME.json models/NAME.blend

Every shape becomes the same solid in the same place; the shapes of one look are joined into one object named after
its colour, so the game draws each look once. Objects that cast no shadow in the game end with _noshadow.
Axes: the game's x, y (up), z are Blender's x, z, -y. Run by tools/to-blender.sh.
"""
import bpy, bmesh, json, math, sys
from mathutils import Matrix, Euler

src, dst = sys.argv[sys.argv.index("--") + 1:][:2]
M = Matrix(((1, 0, 0, 0), (0, 0, -1, 0), (0, 1, 0, 0), (0, 0, 0, 1)))
UP = Matrix.Rotation(-math.pi / 2, 4, 'X')          # Blender's solids stand along z, the game's along y


def lin(c): return c / 12.92 if c <= .04045 else ((c + .055) / 1.055) ** 2.4
def rgb(h): return [lin(int(h[i:i + 2], 16) / 255) for i in (1, 3, 5)]


def place(pos, rot, scale):
    # Godot turns a node about y, then x, then z (the YXZ order)
    r = Matrix.Rotation(rot[1], 4, 'Y') @ Matrix.Rotation(rot[0], 4, 'X') @ Matrix.Rotation(rot[2], 4, 'Z')
    return Matrix.Translation(pos) @ r @ Matrix.Diagonal((*scale, 1))


def material(s):
    name = s["rgb"] + (f" emit {s['emit']} {s['energy']:g}" if s["emit"] else "") + (f" metal {s['metal']:g}" if s["metal"] else "") \
        + (f" rough {s['rough']:g}" if abs(s["rough"] - .9) > 1e-6 else "") + (f" alpha {s['alpha']:g}" if s["alpha"] else "") + (" unshaded" if s["unshaded"] else "")
    m = bpy.data.materials.get(name)
    if m: return m
    m = bpy.data.materials.new(name); m.use_nodes = True
    b = next(n for n in m.node_tree.nodes if n.type == "BSDF_PRINCIPLED")
    c = rgb(s["rgb"]); a = s["alpha"] or 1.0
    b.inputs["Base Color"].default_value = (*c, 1); b.inputs["Roughness"].default_value = s["rough"]; b.inputs["Metallic"].default_value = s["metal"]
    b.inputs["Alpha"].default_value = a
    if s["emit"]:
        b.inputs["Emission Color"].default_value = (*rgb(s["emit"]), 1); b.inputs["Emission Strength"].default_value = s["energy"]
    if s["unshaded"]:                                   # the colour itself, no light: exported as an unlit material
        out = next(n for n in m.node_tree.nodes if n.type == "OUTPUT_MATERIAL")
        e = m.node_tree.nodes.new("ShaderNodeBackground"); e.inputs["Color"].default_value = (*c, 1)
        if a < 1:
            mix = m.node_tree.nodes.new("ShaderNodeMixShader"); tr = m.node_tree.nodes.new("ShaderNodeBsdfTransparent")
            mix.inputs[0].default_value = a
            m.node_tree.links.new(tr.outputs[0], mix.inputs[1]); m.node_tree.links.new(e.outputs[0], mix.inputs[2]); m.node_tree.links.new(mix.outputs[0], out.inputs["Surface"])
        else:
            m.node_tree.links.new(e.outputs[0], out.inputs["Surface"])
    if a < 1: m.surface_render_method = "BLENDED"
    m.diffuse_color = (*c, a); m.use_backface_culling = True
    return m


def solid(bm, s, T):
    k, d = s["kind"], s["dims"]
    if k == "box":
        return bmesh.ops.create_cube(bm, size=1.0, matrix=T @ Matrix.Diagonal((*d, 1)))["verts"]
    if k in ("cyl", "cone"):
        if k == "cone": d = [0.0, d[0], d[1], d[2] if len(d) > 2 else 7]
        rt, rb, h = d[:3]; seg = int(d[3]) if len(d) > 3 else 8
        vs = bmesh.ops.create_cone(bm, cap_ends=True, cap_tris=False, segments=seg, radius1=max(rb, 1e-5), radius2=max(rt, 1e-5), depth=h, matrix=T @ UP)["verts"]
        for f in {f for v in vs for f in v.link_faces}: f.smooth = len(f.verts) == 4      # the side is round, the caps flat
        for e in {e for v in vs for e in v.link_edges}: e.smooth = all(f.smooth for f in e.link_faces)
        return vs
    r = d[0]; seg, rings = (5, 3) if k == "rock" else (int(d[1]) if len(d) > 1 else 10, int(d[2]) if len(d) > 2 else 6)
    vs = bmesh.ops.create_uvsphere(bm, u_segments=seg, v_segments=rings + 1, radius=r, matrix=T @ UP)["verts"]
    for f in {f for v in vs for f in v.link_faces}: f.smooth = True
    return vs


bpy.ops.wm.read_factory_settings(use_empty=True)
lots = {}
for s in json.load(open(src)):
    T = Matrix.Identity(4)
    for link in s["chain"]: T = place(*link) @ T
    m = material(s); key = m.name + ("" if s["shadow"] else "_noshadow")
    if key not in lots: lots[key] = (bmesh.new(), m)
    solid(lots[key][0], s, M @ T)
for key, (bm, m) in lots.items():
    me = bpy.data.meshes.new(key); bm.to_mesh(me); bm.free(); me.materials.append(m)
    bpy.context.scene.collection.objects.link(bpy.data.objects.new(key, me))
bpy.ops.wm.save_as_mainfile(filepath=dst)
print("draw-model:", dst, len(lots), "objects")
