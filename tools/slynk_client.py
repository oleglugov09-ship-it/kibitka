"""Talk to Slynk with a minimal client: one form via slynk:eval-and-grab-output.

The port comes from project/.slynk-port (written by the game built with `make live`
and run as debug: `godot --path project` or F5). A Slynk message is six
hexadecimal digits of length and an s-expression in UTF-8.
"""
import os
import re
import socket

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PORT_FILE = os.path.join(ROOT, "project", ".slynk-port")


def lisp_string(text):
    return '"' + text.replace("\\", "\\\\").replace('"', '\\"') + '"'


def read_lisp_string(text, at):
    """Read the Lisp string at position at (on the quote) -> (string, position after)."""
    assert text[at] == '"'
    out, i = [], at + 1
    while text[i] != '"':
        if text[i] == "\\":
            i += 1
        out.append(text[i])
        i += 1
    return "".join(out), i + 1


class Slynk:
    def __init__(self, port=None, timeout=10):
        port = port or int(open(PORT_FILE).read().strip())
        self.sock = socket.create_connection(("127.0.0.1", port), timeout=timeout)
        self.next_id = 1

    def send(self, sexp):
        data = sexp.encode("utf-8")
        self.sock.sendall(b"%06x" % len(data) + data)

    def recv(self):
        def exactly(n):
            buf = b""
            while len(buf) < n:
                chunk = self.sock.recv(n - len(buf))
                if not chunk:
                    raise ConnectionError("Slynk закрыл соединение")
                buf += chunk
            return buf
        return exactly(int(exactly(6), 16)).decode("utf-8")

    def eval(self, form, package="CL-USER"):
        """Evaluate FORM -> (ok, output, value or error text)."""
        rid = self.next_id
        self.next_id += 1
        self.send(f"(:emacs-rex (slynk:eval-and-grab-output {lisp_string(form)}) "
                  f"{lisp_string(package)} t {rid})")
        while True:
            msg = self.recv()
            if msg.startswith("(:return") and msg.rstrip(")").endswith(f" {rid}"):
                if msg.startswith("(:return (:ok ("):
                    at = msg.index('"')
                    output, at = read_lisp_string(msg, at)
                    at = msg.index('"', at)
                    value, _ = read_lisp_string(msg, at)
                    return True, output, value
                return False, "", self.last_error or msg
            if msg.startswith("(:debug "):
                # An error in the form: Slynk entered the debugger in the worker thread.
                # The condition text is the first line after the level number.
                m = re.match(r'\(:debug (\S+) (\d+) \("', msg)
                self.last_error, _ = read_lisp_string(msg, msg.index('("') + 1)
                # Leave the debugger with the *ABORT restart (the 0th is RETRY, which
                # would evaluate the form again)
                names = re.findall(r'\(\("([^"]+)"|\("([^"]+)" "', msg.split(" nil) ", 1)[1])
                names = [a or b for a, b in names]
                restart = names.index("*ABORT") if "*ABORT" in names else 1
                self.send(f"(:emacs-rex (slynk:invoke-nth-restart-for-emacs {m.group(2)} {restart}) "
                          f'"CL-USER" {m.group(1)} {rid + 1000})')
            # :debug-activate, :write-string, :new-features and the rest — skip them

    last_error = None

    def close(self):
        self.sock.close()
