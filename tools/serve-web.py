#!/usr/bin/env python3
"""Serve the browser build from export/web on 127.0.0.1, telling the browser to keep nothing.

    tools/serve-web.py [PORT]        (8765 if not given); stop with Ctrl+C

`python3 -m http.server` will not do for a build that is rebuilt while you play: it sends no caching headers, so the
browser keeps the game's files for a while by its own guess, and after a rebuild a reload can mix the new code with
the old pack (the road vanished that way: the code asked the pack for files it did not have yet).
"""
import http.server, os, sys

class Handler(http.server.SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header("Cache-Control", "no-store")
        super().end_headers()
    def log_message(self, *args): pass

os.chdir(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "export", "web"))
port = int(sys.argv[1]) if len(sys.argv) > 1 else 8765
http.server.ThreadingHTTPServer(("127.0.0.1", port), Handler).serve_forever()
