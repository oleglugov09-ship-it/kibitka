# Kibitka on Godot: the conductor is Common Lisp (ECL inside Godot via gdecl).
# gdecl is a checkout of https://github.com/flambard/gdecl at 182d5c0 with patches/gdecl-web.patch applied.
# Desktop Linux only.
GDECL ?= $(HOME)/src/gdecl-live/gdecl
SYSTEM = kiosk-trip
LIBRARY = project/bin/libgdecl.so
GODOT ?= godot

all: $(LIBRARY)

include $(GDECL)/gdecl.mk

import:
	$(GODOT) --headless --path project --import

# With Slynk (127.0.0.1:4009, the port is in project/.slynk-port) and the LIVE package
SLYNK_DIR ?= $(HOME)/src/sly/slynk
live:
	touch lisp/net.lisp   # it holds the fingerprint of all sources (the build version): must be recomputed on any edit
	$(MAKE) -B $(LIBRARY) SYSTEM=kiosk-trip-live LISP_DIRS="lisp $(SLYNK_DIR)"
	test -d project/.godot || $(MAKE) import

# Browser: web/build.sh cross-compiles gdecl and the game for wasm32 (project/bin/libgdecl.wasm),
# then an export with the web_dlink_nothreads_release template.
web:
	tools/names.py
	web/build.sh
	tools/keep-raw.sh
	$(GODOT) --headless --path project --import
	mkdir -p export/web
	$(GODOT) --headless --path project --export-release "Web" ../export/web/index.html

.PHONY: import live web
