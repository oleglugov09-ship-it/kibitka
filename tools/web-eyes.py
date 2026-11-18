#!/usr/bin/env python3
"""Eyes in the browser: open the web build in headless Chrome, press, click, and take screenshots.
No window appears on the desktop. Console lines, uncaught exceptions and dialogs (a prompt means the game
fell into the Lisp debugger) are printed as they come.

    cd export/web && python3 -m http.server 8765 --bind 127.0.0.1 &      # serve the build (stop it afterwards)
    tools/web-eyes.py http://127.0.0.1:8765/index.html PROFILE-DIR STEP...

PROFILE-DIR is the browser profile: the same one on the next run keeps the saves, a fresh mktemp -d starts clean.
Steps, in order:  go            load the page            wait:SECONDS
                  click:X,Y     left click (960x540)     key:K[,HOLD]   a letter, or space enter esc tab up down
                  shot:FILE.png a screenshot             js:EXPRESSION  evaluate in the page and print
The lobby: "set out alone" is at 350,174, "open a route" at 350,201; the intro cards turn on a click at 480,200.
The game takes about 40 s to start under the software renderer. ?dev and ?torture go into the address.
Chrome: ~/.local/bin/google-chrome (set CHROME to change); the snap Chromium will not do.
"""
import json, subprocess, sys, time, urllib.request, tempfile, os, base64, websocket
url = sys.argv[1]; prof = sys.argv[2]; script = sys.argv[3:]
chrome = subprocess.Popen([os.environ.get("CHROME","/home/lil/.local/bin/google-chrome"), "--headless=new", "--no-sandbox", "--no-first-run", f"--user-data-dir={prof}",
    "--enable-unsafe-swiftshader", "--use-gl=angle", "--use-angle=swiftshader", "--ignore-gpu-blocklist", "--autoplay-policy=no-user-gesture-required",
    "--remote-debugging-port=9333", "--remote-allow-origins=*", "--window-size=960,540", "about:blank"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
try:
    for _ in range(40):
        try: tabs = json.load(urllib.request.urlopen("http://127.0.0.1:9333/json", timeout=2)); break
        except Exception: time.sleep(.5)
    ws = websocket.create_connection([t for t in tabs if t["type"] == "page"][0]["webSocketDebuggerUrl"], timeout=3)
    n = [0]; replies = {}
    def send(method, **params):
        n[0] += 1; ws.send(json.dumps({"id": n[0], "method": method, "params": params})); return n[0]
    def pump(seconds):
        end = time.time() + seconds
        while time.time() < end:
            try: msg = json.loads(ws.recv())
            except websocket.WebSocketTimeoutException: continue
            m = msg.get("method")
            if m == "Page.javascriptDialogOpening":
                p = msg["params"]; print(f"ДИАЛОГ {p['type']}: {p['message'][:300]!r}", flush=True); send("Page.handleJavaScriptDialog", accept=False)
            elif m == "Runtime.consoleAPICalled":
                text = " ".join(str(a.get("value", a.get("description", ""))) for a in msg["params"]["args"])
                if not any(k in text for k in ("WebGL", "GL Driver", "user://mods", "get_directories_at")): print(f"[{msg['params']['type']}] {text[:600]}", flush=True)
            elif m == "Runtime.exceptionThrown":
                d = msg["params"]["exceptionDetails"]; print(f"ИСКЛЮЧЕНИЕ {d.get('text','')} {d.get('exception',{}).get('description','')[:500]}", flush=True)
            elif "id" in msg: replies[msg["id"]] = msg
    def call(method, **params):
        i = send(method, **params)
        for _ in range(60):
            pump(.25)
            if i in replies: return replies.pop(i).get("result", {})
        return {}
    for m in ("Page.enable", "Runtime.enable"): send(m)
    send("Emulation.setFocusEmulationEnabled", enabled=True)
    KEYS = {"space": (" ", "Space", 32), "enter": ("\r", "Enter", 13), "esc": ("", "Escape", 27), "tab": ("", "Tab", 9), "up": ("", "ArrowUp", 38), "down": ("", "ArrowDown", 40)}
    def key(k, hold=.08):
        text, code, vk = KEYS[k] if k in KEYS else (k, "Key" + k.upper(), ord(k.upper()))
        send("Input.dispatchKeyEvent", type="keyDown", key=k if k not in KEYS else code, code=code, windowsVirtualKeyCode=vk, nativeVirtualKeyCode=vk, text=text)
        pump(hold)
        send("Input.dispatchKeyEvent", type="keyUp", key=k if k not in KEYS else code, code=code, windowsVirtualKeyCode=vk, nativeVirtualKeyCode=vk)
    for step in script:
        op, _, arg = step.partition(":")
        if op == "go": send("Page.navigate", url=url); print("--- заход", flush=True)
        elif op == "wait": pump(float(arg))
        elif op == "key":
            k, _, hold = arg.partition(","); key(k, float(hold or .08)); pump(.3)
        elif op == "click":
            x, y = map(float, arg.split(","))
            for t in ("mousePressed", "mouseReleased"): send("Input.dispatchMouseEvent", type=t, x=x, y=y, button="left", clickCount=1); pump(.1)
        elif op == "shot":
            r = call("Page.captureScreenshot", format="png")
            if r.get("data"): open(arg, "wb").write(base64.b64decode(r["data"])); print("снимок", arg, flush=True)
        elif op == "js":
            r = call("Runtime.evaluate", expression=arg, returnByValue=True, awaitPromise=True)
            print("JS =>", json.dumps(r.get("result", {}).get("value"), ensure_ascii=False)[:1500], flush=True)
finally:
    chrome.terminate()
