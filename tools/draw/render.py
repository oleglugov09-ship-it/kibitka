import bpy, sys, math, mathutils, os
out = sys.argv[sys.argv.index("--") + 1]
name = os.path.basename(bpy.data.filepath)[:-6]
sc = bpy.context.scene
# the shared base (slab, bench, sign) stands with every stop in the game
base = os.path.join(os.path.dirname(bpy.data.filepath), "stop-base.blend")
if name != "stop-base" and os.path.exists(base):
    with bpy.data.libraries.load(base) as (src, dst): dst.objects = list(src.objects)
    for o in dst.objects:
        if o: sc.collection.objects.link(o)
repeat = int(os.environ.get("REPEAT", "1"))               # a house is several sections side by side: show it so
if repeat > 1:
    for o in [o for o in sc.objects if o.type == 'MESH' and not o.name.startswith(("solid", "surface"))]:
        for i in range(1, repeat):
            c = o.copy(); c.location.y += 16.0 * i; sc.collection.objects.link(c)
    bpy.context.view_layer.update()
meshes = [o for o in sc.objects if o.type == 'MESH' and not o.name.startswith(("solid", "surface"))]
pts = [o.matrix_world @ mathutils.Vector(c) for o in meshes for c in o.bound_box]
lo = mathutils.Vector([min(p[i] for p in pts) for i in range(3)]); hi = mathutils.Vector([max(p[i] for p in pts) for i in range(3)])
mid = (lo + hi) / 2; size = max((hi - lo).length, 1.0)
ground = 0.0 if lo.z < -1 else lo.z
if lo.z < -1: lo.z = 0.0; mid = (lo + hi) / 2; size = max((hi - lo).length, 1.0)
bpy.ops.mesh.primitive_plane_add(size=400, location=(mid.x, mid.y, ground - .001))
g = bpy.data.materials.new("ground"); g.use_nodes = True
next(n for n in g.node_tree.nodes if n.type == "BSDF_PRINCIPLED").inputs[0].default_value = (.32, .34, .28, 1)
bpy.context.object.data.materials.append(g)
cam = bpy.data.objects.new("cam", bpy.data.cameras.new("cam")); sc.collection.objects.link(cam); sc.camera = cam
# the road is at game -x: Blender -x. Look from the road side, a little from the right and above
view = sys.argv[sys.argv.index("--") + 2] if len(sys.argv) > sys.argv.index("--") + 2 else "side"
d = mathutils.Vector({"side": (-1.0, -.75, .42), "front": (-1.0, .12, .2), "back": (-.55, 1.0, .5), "yard": (1.0, .7, .45), "street": (-1.0, -.35, .12)}[view]).normalized()
cam.location = mid + d * size * 1.45
cam.rotation_euler = (mid - cam.location).to_track_quat('-Z', 'Y').to_euler()
cam.data.lens = 40
sun = bpy.data.objects.new("sun", bpy.data.lights.new("sun", 'SUN')); sc.collection.objects.link(sun)
sun.data.energy = 3.2; sun.rotation_euler = (math.radians(52), 0, math.radians(-125))
w = bpy.data.worlds.new("w"); sc.world = w; w.use_nodes = True
w.node_tree.nodes["Background"].inputs[0].default_value = (.62, .70, .80, 1); w.node_tree.nodes["Background"].inputs[1].default_value = .9
for eng in ("BLENDER_EEVEE", "BLENDER_EEVEE_NEXT", "CYCLES"):
    try: sc.render.engine = eng; break
    except TypeError: pass
if sc.render.engine == "CYCLES": sc.cycles.samples = 16
sc.render.resolution_x, sc.render.resolution_y = [int(v) for v in os.environ.get("SIZE", "480x360").split("x")]
sc.render.filepath = os.path.join(out, name + "-" + view + ".png")
bpy.ops.render.render(write_still=True)
print("RENDERED", name, sc.render.engine)
