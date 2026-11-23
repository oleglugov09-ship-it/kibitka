"""The first drawing of the panel house kit: blender -b --python tools/make-house-kit.py -- FLOORS OUT.blend

One entrance section of a panel block, 16 m long and 12 m deep, FLOORS storeys of 2.8 m. In the file: `ground` (the
first storey with the entrance and the foundation), `floor` (one typical storey; an Array modifier stacks it, so
editing one storey edits them all), `roof`, a cube `solid` (the obstacle), and empties at_lit_... where a window may
be lit at night. The game sets sections side by side (lisp/city.lisp, HOUSE-SECTIONS) and paints paint0 (the walls)
and paint1 (the balconies). Once a file exists it is edited in Blender; this script is only how it began.
Axes: the game's x (0 is the facade, the house goes to x+), y (up), z (along the street) are Blender's x, z, -y."""
import bpy, bmesh, sys
from mathutils import Matrix
floors, out = int(sys.argv[sys.argv.index("--") + 1]), sys.argv[sys.argv.index("--") + 2]
M = Matrix(((1, 0, 0, 0), (0, 0, -1, 0), (0, 1, 0, 0), (0, 0, 0, 1)))
STEP, LEN, DEPTH = 2.8, 16.0, 12.0
SHOW = {"paint0": "#7b7d82", "paint1": "#9a9488"}
def lin(c): return c / 12.92 if c <= .04045 else ((c + .055) / 1.055) ** 2.4
def material(name):
    m = bpy.data.materials.get(name)
    if m: return m
    m = bpy.data.materials.new(name); m.use_nodes = True
    h = SHOW.get(name, name); rgb = [lin(int(h[i:i + 2], 16) / 255) for i in (1, 3, 5)]
    b = next(n for n in m.node_tree.nodes if n.type == "BSDF_PRINCIPLED")
    b.inputs["Base Color"].default_value = (*rgb, 1); b.inputs["Roughness"].default_value = .9; m.diffuse_color = (*rgb, 1); m.use_backface_culling = True
    return m
class Part:
    def __init__(self, name): self.name, self.bm, self.mats = name, bmesh.new(), []
    def box(self, mat, dims, pos):
        if mat not in self.mats: self.mats.append(mat)
        vs = bmesh.ops.create_cube(self.bm, size=1.0, matrix=M @ Matrix.Translation(pos) @ Matrix.Diagonal((*dims, 1)))["verts"]
        for f in {f for v in vs for f in v.link_faces}: f.material_index = self.mats.index(mat)
    def done(self, y=0.0):
        me = bpy.data.meshes.new(self.name); self.bm.to_mesh(me); self.bm.free()
        for m in self.mats: me.materials.append(material(m))
        ob = bpy.data.objects.new(self.name, me); bpy.context.scene.collection.objects.link(ob); ob.location = (0, 0, y); return ob
GLASS, WHITE, DARK = "#2e333b", "#d8d4c4", "#55554f"
def windows(p, zs, y, front=True):
    for z in zs:
        p.box(GLASS, (.1, 1.4, 1.5), (-.03 if front else DEPTH + .03, y, z))
        if front: p.box(WHITE, (.22, .07, 1.7), (-.09, y - .74, z))
bpy.ops.wm.read_factory_settings(use_empty=True)
g = Part("ground")                                       # the first storey: the foundation goes 4 m down, for slopes
g.box("paint0", (DEPTH, STEP + 4, LEN), (DEPTH / 2, STEP / 2 - 2, 0)); g.box("#6e6c68", (.1, .7, LEN), (-.05, .35, 0))
windows(g, (-6.4, -3.2, 0.0, 6.4), 1.75); windows(g, (-6, -2, 2, 6), 1.75, front=False)
g.box("#4a3020", (.12, 2.1, 1.2), (-.06, 1.05, 3.2)); g.box(DARK, (1.6, .15, 2.4), (-.8, 2.5, 3.2)); g.box("#6e6c68", (1.2, .2, 2.0), (-.6, .1, 3.2))
for dz in (-1.1, 1.1): g.box(DARK, (.08, 2.4, .08), (-1.5, 1.2, 3.2 + dz))
g.box("#6a6c70", (.04, .06, LEN), (-.02, STEP - .03, 0))
g.done()
f = Part("floor")                                        # a typical storey: four windows and a balcony to the street
f.box("paint0", (DEPTH, STEP, LEN), (DEPTH / 2, STEP / 2, 0)); f.box("#6a6c70", (.04, .06, LEN), (-.02, STEP - .03, 0))
windows(f, (-6.4, -3.2, 3.2, 6.4), 1.65); windows(f, (-6, -2, 2, 6), 1.65, front=False)
f.box(GLASS, (.1, 2.0, 1.6), (-.03, 1.1, 0)); f.box(DARK, (1.0, .12, 2.8), (-.5, .06, 0)); f.box("paint1", (.08, 1.0, 2.8), (-.96, .6, 0))
for dz in (-1.36, 1.36): f.box("paint1", (.9, 1.0, .08), (-.5, .6, dz))
ob = f.done(STEP)
arr = ob.modifiers.new("storeys", "ARRAY"); arr.count = floors - 1; arr.use_relative_offset = False; arr.use_constant_offset = True; arr.constant_offset_displace = (0, 0, STEP)
H = floors * STEP
r = Part("roof")
r.box("#4c4e52", (DEPTH + .4, .5, LEN), (DEPTH / 2, .25, 0)); r.box("#5c5e62", (1.2, .9, 1.2), (6, .95, -4)); r.box("#5c5e62", (3.0, 2.2, 3.0), (7, 1.6, 3.2))
r.done(H)
cube = bpy.data.meshes.new("place"); bm = bmesh.new(); bmesh.ops.create_cube(bm, size=1.0); bm.to_mesh(cube); bm.free()
col = bpy.data.collections.new("places"); bpy.context.scene.collection.children.link(col)
s = bpy.data.objects.new("solid", cube); s.display_type = "WIRE"; col.objects.link(s)
s.matrix_world = M @ Matrix.Translation((DEPTH / 2, 1.0, 0)) @ Matrix.Diagonal((DEPTH, 2.0, LEN, 1))
for fl in range(floors):
    for k, z in enumerate((-6.4, -3.2, 0.0, 6.4) if fl == 0 else (-6.4, -3.2, 3.2, 6.4)):
        e = bpy.data.objects.new(f"at_lit_{fl:02d}_{k}", None); e.empty_display_type = "PLAIN_AXES"; e.empty_display_size = .3; col.objects.link(e)
        e.matrix_world = M @ Matrix.Translation((-.09, fl * STEP + (1.75 if fl == 0 else 1.65), z))
bpy.ops.wm.save_as_mainfile(filepath=out)
print("house kit:", out)
