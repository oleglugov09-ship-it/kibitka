#!/usr/bin/env python3
"""A game kept running in headless Chrome, to change its Lisp on the fly and look at once: no rebuild per change.
No window, no sound. The build must be a ?dev one (export/web after `make web`).

    tools/web-live.py start [ADDRESS-TAIL]   serve export/web, open the game (default tail: ?tunnel&dev), leave the lobby alone
    tools/web-live.py lisp 'FORMS'           evaluate in the game and print the answer
    tools/web-live.py load FILE.lisp         evaluate every top-level form of the file, in order (redefine functions this way)
    tools/web-live.py shot FILE.png          a screenshot (960x540)
    tools/web-live.py click X,Y | key K      as in web-eyes.py
    tools/web-live.py log                    what the page printed since the last call
    tools/web-live.py stop                   close Chrome and the server

What is redefined this way is interpreted and lives until `stop`: put it in the sources too, and build once at the end.
"""
import base64, json, os, signal, subprocess, sys, tempfile, time, urllib.request, websocket
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
STATE = os.path.join(tempfile.gettempdir(), "kibitka-web-live.json")
PORT, HTTP = 9334, 8766


def page():
    tabs = json.load(urllib.request.urlopen(f"http://127.0.0.1:{PORT}/json", timeout=3))
    return websocket.create_connection([t for t in tabs if t["type"] == "page"][0]["webSocketDebuggerUrl"], timeout=3)


class Tab:
    def __init__(self):
        self.ws, self.n, self.replies = page(), 0, {}
        for m in ("Page.enable", "Runtime.enable"): self.send(m)
        self.send("Emulation.setFocusEmulationEnabled", enabled=True)

    def send(self, method, **params):
        self.n += 1; self.ws.send(json.dumps({"id": self.n, "method": method, "params": params})); return self.n

    def pump(self, seconds):
        end = time.time() + seconds
        while time.time() < end:
            try: msg = json.loads(self.ws.recv())
            except websocket.WebSocketTimeoutException: continue
            m = msg.get("method")
            if m == "Page.javascriptDialogOpening":
                print("ДИАЛОГ", msg["params"]["message"][:300], flush=True); self.send("Page.handleJavaScriptDialog", accept=False)
            elif m == "Runtime.consoleAPICalled":
                text = " ".join(str(a.get("value", a.get("description", ""))) for a in msg["params"]["args"])
                if msg["params"]["type"] == "error": print("[error]", text[:300], flush=True)
            elif m == "Runtime.exceptionThrown":
                print("ИСКЛЮЧЕНИЕ", msg["params"]["exceptionDetails"].get("exception", {}).get("description", "")[:300], flush=True)
            elif "id" in msg: self.replies[msg["id"]] = msg

    def call(self, method, **params):
        i = self.send(method, **params)
        for _ in range(80):
            self.pump(.1)
            if i in self.replies: return self.replies.pop(i).get("result", {})
        return {}

    def lisp(self, text, wait=20.0):
        self.send("Runtime.evaluate", expression=f"(window.__lisp = window.__lisp || []).push({json.dumps(text)})")
        end = time.time() + wait
        while time.time() < end:
            self.pump(.1)
            v = self.call("Runtime.evaluate", expression="(window.__lispOut && window.__lispOut.length) ? window.__lispOut.shift() : null",
                          returnByValue=True).get("result", {}).get("value")
            if v is not None: return v
        return "нет ответа"


def forms(text):
    """Split Lisp TEXT into its top-level forms."""
    out, depth, start, i, n = [], 0, None, 0, len(text)
    while i < n:
        ch = text[i]
        if ch == ";":
            while i < n and text[i] != "\n": i += 1
            continue
        if ch == '"':
            i += 1
            while text[i] != '"': i += 2 if text[i] == "\\" else 1
        elif ch == "#" and text[i + 1] == "\\": i += 2
        elif ch == "(":
            if depth == 0: start = i
            depth += 1
        elif ch == ")":
            depth -= 1
            if depth == 0: out.append(text[start:i + 1])
        i += 1
    return out


def main():
    op, args = sys.argv[1], sys.argv[2:]
    if op == "start":
        tail = args[0] if args else "?tunnel&dev"
        null = subprocess.DEVNULL
        srv = subprocess.Popen(["python3", "-m", "http.server", str(HTTP), "--bind", "127.0.0.1"], cwd=os.path.join(ROOT, "export/web"),
                               stdout=null, stderr=null, start_new_session=True)
        chrome = subprocess.Popen([os.environ.get("CHROME", os.path.expanduser("~/.local/bin/google-chrome")), "--headless=new", "--no-sandbox", "--no-first-run",
                                   f"--user-data-dir={tempfile.mkdtemp()}", "--enable-unsafe-swiftshader", "--use-gl=angle", "--use-angle=swiftshader",
                                   "--ignore-gpu-blocklist", "--mute-audio", f"--remote-debugging-port={PORT}", "--remote-allow-origins=*",
                                   "--window-size=960,540", "about:blank"], stdout=null, stderr=null, start_new_session=True)
        json.dump({"srv": srv.pid, "chrome": chrome.pid}, open(STATE, "w"))
        for _ in range(40):
            try: tab = Tab(); break
            except Exception: time.sleep(.5)
        tab.send("Page.navigate", url=f"http://127.0.0.1:{HTTP}/index.html{tail}"); tab.pump(50)
        for t in ("mousePressed", "mouseReleased"): tab.send("Input.dispatchMouseEvent", type=t, x=350, y=174, button="left", clickCount=1); tab.pump(.1)
        tab.pump(5); print("запущено:", tab.lisp("(lisp-implementation-version)"))
    elif op == "stop":
        for pid in json.load(open(STATE)).values():
            try: os.killpg(pid, signal.SIGTERM)
            except ProcessLookupError: pass
        os.remove(STATE)
    else:
        tab = Tab()
        if op == "lisp": print(tab.lisp(args[0])[:1500])
        elif op == "load":
            for f in forms(open(args[0], encoding="utf-8").read()):
                quoted = f.replace("\\", "\\\\").replace('"', '\\"')            # read in the game's package, whatever the listener's is
                v = tab.lisp(f'(let ((*package* (find-package :kiosk-trip))) (eval (read-from-string "{quoted}")))')
                if "rror" in v or v == "нет ответа": print(f[:60].replace("\n", " "), "=>", v[:400])
            print("загружено", args[0])
        elif op == "shot":
            r = tab.call("Page.captureScreenshot", format="png")
            open(args[0], "wb").write(base64.b64decode(r["data"])); print("снимок", args[0])
        elif op == "click":
            x, y = map(float, args[0].split(","))
            for t in ("mousePressed", "mouseReleased"): tab.send("Input.dispatchMouseEvent", type=t, x=x, y=y, button="left", clickCount=1); tab.pump(.1)
        elif op == "log": tab.pump(1.0)
        else: sys.exit(__doc__)


if __name__ == "__main__":
    main()
