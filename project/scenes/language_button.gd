extends Button
# Language button in the pause menu: switches the menu language; Lisp translates the rest of the game (step-lang in walk.lisp).

func _pressed() -> void:
	TranslationServer.set_locale("en" if TranslationServer.get_locale().begins_with("ru") else "ru")
