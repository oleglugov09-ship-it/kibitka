#!/usr/bin/env python3
"""Make project/terrain/terrain.gdshader of Terrain3D's own shader (dumped by project/terrain/dump_shader.gd).

    tools/terrain-shader.py DUMPED.gdshader OUT.gdshader

Three changes. (1) world_shift: the game keeps the scene origin near the wagon and moves it now and then, and
Terrain3D cannot be moved; so the terrain is drawn where the scene is, and reads its maps where the map is: map =
scene + world_shift. In x and z the shift is a multiple of 1024 m, which keeps the levels of detail on their grids.
(2) The colour map's alpha is not wetness here but how grassy the place is: the season tints grass, not rock or road.
(3) No highlights: a land that glints at grazing angles sparkles far away.
(4) In the Compatibility renderer, which the game uses, the texture arrays are not made linear: it is done here.
Run by tools/bake-map.sh; fails loudly if Terrain3D's shader has changed under it."""
import sys
src, out = sys.argv[1:3]; s = open(src).read()
def rep(old, new, count=1):
    global s
    assert s.count(old) == count, (old, s.count(old)); s = s.replace(old, new)
rep("varying vec3 v_camera_pos;", """varying vec3 v_camera_pos;

// The game's: see tools/terrain-shader.py
uniform vec3 world_shift = vec3(0.);
uniform vec3 season_color : source_color = vec3(1.);
uniform float season_amount = 0.0;
uniform float texture_gain = 2.0;   // the pictures of the land are nearly grey, the place gives the colour""")
rep("v_camera_pos = MAIN_CAM_INV_VIEW_MATRIX[3].xyz;", "v_camera_pos = MAIN_CAM_INV_VIEW_MATRIX[3].xyz + world_shift;")
rep("v_vertex = (MODEL_MATRIX * vec4(VERTEX, 1.0)).xyz;", "v_vertex = (MODEL_MATRIX * vec4(VERTEX, 1.0)).xyz;\n\tv_vertex.xz += world_shift.xz;\n\tvec3 target_pos = _target_pos + world_shift;")
rep("v_vertex_xz_dist = length(v_vertex.xz - _target_pos.xz);", "v_vertex_xz_dist = length(v_vertex.xz - target_pos.xz);")
rep("abs(v_vertex.x - _target_pos.x), abs(v_vertex.z - _target_pos.z)", "abs(v_vertex.x - target_pos.x), abs(v_vertex.z - target_pos.z)")
rep("VERTEX = (VIEW_MATRIX * vec4(v_vertex + displacement, 1.0)).xyz;", "VERTEX = (VIEW_MATRIX * vec4(v_vertex + displacement - world_shift, 1.0)).xyz;")
rep("float wetness = fma(color_map.a, -2., 1.);", "float wetness = 0.;")
rep("ALBEDO = mat.albedo_height.rgb * color_map.rgb;\n\tROUGHNESS = roughness;\n\tSPECULAR = specular;",
    "vec3 place_color = color_map.rgb;\n\tvec3 ground = mat.albedo_height.rgb;\n"
    "#if CURRENT_RENDERER == RENDERER_COMPATIBILITY\n"
    "\t// here the texture arrays come as they are stored, not made linear: do it by hand\n"
    "\tplace_color = pow(place_color, vec3(2.2));\n\tground = pow(ground, vec3(2.2));\n"
    "#endif\n"
    "\tALBEDO = clamp(ground * texture_gain * mix(place_color, season_color, season_amount * color_map.a), 0., 1.);\n\tROUGHNESS = 1.0 + 0. * roughness;\n\tSPECULAR = 0. * specular;")
assert "_target_pos" not in s.split("void vertex()")[1].split("void fragment()")[0].replace("vec3 target_pos = _target_pos + world_shift;", ""), "another use of _target_pos"
open(out, "w").write(s); print("terrain-shader:", out, len(s))
