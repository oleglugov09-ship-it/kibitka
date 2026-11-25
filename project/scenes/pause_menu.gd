@tool
extends "res://addons/maaacks_menus_template/examples/scenes/windows/pause_menu.gd"
# The pause menu of the game. "Main Menu" does not change the scene: the game is one scene, and Lisp runs it.
# It leaves a note on the scene root, which the game picks up on the next frame (STEP-MENU in lisp/menu.lisp).

func _refresh_main_menu_button() -> void:
	main_menu_button.visible = true

func _on_main_menu_confirmation_confirmed():
	get_tree().current_scene.set_meta(&"to_main_menu", true)
	close()
