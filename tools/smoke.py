#!/usr/bin/env python3
"""Smoke-test the game.

Launches it (one copy, then a driver with a guest), walks through the main scenarios with commands
over Slynk and prints what matched and what didn't. The exit code is the number of failures.

    tools/smoke.py            # everything
    tools/smoke.py solo       # only the solo scenarios
    tools/smoke.py pair       # only the network ones (needs internet: playing by code goes through the room server)

Needs the `make live` build and free Slynk ports P0 and P1. The player's real saves are hidden for the
duration of the check and put back afterwards; the check does not write to the game log (KIOSK_NOLOG).
"""
import glob, os, shutil, subprocess, sys, time

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(ROOT, "tools"))
from slynk_client import Slynk  # noqa: E402

# A separate data folder per run (XDG_DATA_HOME): the check doesn't touch the player's real saves at all,
# and several runs can go at once. Ports are counted from SMOKE_PORT.
DATA = os.environ.get("XDG_DATA_HOME") or os.path.expanduser("~/.local/share")
USER = os.path.join(DATA, "godot/app_userdata/Kiosk Trip (Lisp)")
P0 = int(os.environ.get("SMOKE_PORT", "4009"))
P1 = P0 + 1
HEADLESS = os.environ.get("SMOKE_WINDOW") is None           # faster without a window; SMOKE_WINDOW=1 to watch with your own eyes
K = "kiosk-trip::"
fails = []
procs = []


SAVES = ("player.sexp", "interior*.sexp", "lobby.sexp", "game.sexp", "wall.sexp", "photo-*.png", "look.sexp", "finds.sexp", "prikol.sexp", "bumagi.sexp", "gnomy.sexp", "save.sexp*", "me.sexp*")
STASH = os.path.join(USER, "smoke-stash")


def clean():
    for pat in SAVES:
        for f in glob.glob(os.path.join(USER, pat)):
            os.remove(f)


def stash():
    """Hide the player's real saves for the duration of the check."""
    os.makedirs(STASH, exist_ok=True)
    for pat in SAVES:
        for f in glob.glob(os.path.join(USER, pat)):
            shutil.move(f, os.path.join(STASH, os.path.basename(f)))


def unstash():
    clean()
    for f in glob.glob(os.path.join(STASH, "*")):
        shutil.move(f, os.path.join(USER, os.path.basename(f)))
    if os.path.isdir(STASH):
        os.rmdir(STASH)


def start(port, **env):
    e = dict(os.environ, KIOSK_SLYNK=str(port), KIOSK_NOLOG="1", KIOSK_LANG="ru", **env)   # the checks compare Russian text
    log = open(f"/tmp/kibitka-smoke-{port}.log", "w")
    p = subprocess.Popen(["godot", "--path", "project", "--audio-driver", "Dummy", "--disable-vsync", "--max-fps", "60"] + (["--headless"] if HEADLESS else []),
                         cwd=ROOT, env=e,
                         stdout=log, stderr=subprocess.STDOUT)
    procs.append(p)
    for _ in range(60):
        time.sleep(0.5)
        try:
            if ev(port, "(+ 1 1)").startswith("2"):      # the main thread already answers — the game is ready
                time.sleep(1)
                return
        except (OSError, RuntimeError):
            pass
    raise SystemExit(f"игра на порту {port} не запустилась")


def stop_all():
    for p in procs:
        p.terminate()
    for p in procs:
        try:
            p.wait(timeout=10)
        except subprocess.TimeoutExpired:
            p.kill()
    for c in conns.values():
        try:
            c.close()
        except OSError:
            pass
    conns.clear()
    if procs:
        time.sleep(2)                                     # Slynk ports are not freed instantly
    procs.clear()


conns = {}


def ev(port, body):
    """Evaluate BODY in the game's main thread and return the printed value.

    There is one connection to the game for the whole scenario: opening one per command wastes a second."""
    form = f"(live:on-main-sync (lambda () {body}))"
    try:
        if port not in conns:
            conns[port] = Slynk(port=port, timeout=30)
        ok, _out, val = conns[port].eval(form)
    except (OSError, EOFError, ConnectionError):          # the connection dropped — one new one and another try
        conns.pop(port, None)
        conns[port] = Slynk(port=port, timeout=30)
        ok, _out, val = conns[port].eval(form)
    if not ok:
        raise RuntimeError(val)
    return val.strip()


def check(name, got, want, within=0):
    """Compare GOT with WANT.

    If GOT is a function, we ask it again and again until it matches or WITHIN seconds run out: we wait
    for the event, not for the clock. A short pause comes first — so as not to take for an answer
    something that was already true before the event."""
    ok = (lambda v: v == want) if not callable(want) else want
    if callable(got):
        ask, deadline = got, time.time() + within
        time.sleep(0.35)
        while True:
            try:
                got = ask()
                good = ok(got)
            except Exception as e:                      # the game isn't ready to answer yet — ask again
                got, good = repr(e), False
            if good or time.time() > deadline:
                break
            time.sleep(0.2)
    good = ok(got)
    print(("  ok   " if good else "  ПРОВАЛ ") + name + ("" if good else f": получили {got}"))
    if not good:
        fails.append(name)


def log_errors(port):
    text = open(f"/tmp/kibitka-smoke-{port}.log", errors="replace").read()
    return (text.count("Ошибка (") + text.count("has been freed") + text.count("In function") + text.count("is undefined")
            + text.count("STACK overflow"))


def arrive_first_market(port):
    ev(port, f"(progn ({K}go-to-town (first {K}*towns*)) (setf {K}*speed* 9.0) ({K}catch-up 200.0) 1)")
    time.sleep(7)


def bring(port, kind, tries=16):
    """Ring the bell until a customer of kind KIND comes; wait until they arrive."""
    for seed in range(tries):
        got = ev(port, f"""(progn (when {K}*client* ({K}client-leave))
            (setf {K}*visits* nil)
            (let ((*random-state* (make-random-state {seed + 1}))
                  ({K}*items* (remove-if (lambda (it) (sixth (car it))) {K}*items*)))
              ({K}ring-bell))
            (getf {K}*client* :kind))""")
        if got == kind:
            time.sleep(3.2)
            return True
    return False


def solo():
    print("Одиночная игра")
    clean()
    start(P0, KIOSK_SOLO="1", KIOSK_NAME="Проба")           # solo — no network
    p = P0
    check("12 городов", ev(p, f"(length {K}*towns*)"), lambda v: v.startswith("12"))
    check("три посылки в три города", ev(p, f"({K}parcels-left)"), "((0 . 1) (1 . 1) (2 . 1))")
    check("четыре молока", ev(p, f"(count :milk {K}*items* :key (lambda (it) (first (car it))))"), lambda v: v.startswith("4"))
    check("бензина ровно до первого рынка",
          ev(p, f"(< 0 (- (* {K}*fuel* {K}*tank-km* 1000) ({K}town-stop-d (first {K}*towns*))) 10)"), "T")
    arrive_first_market(p)
    check("доехали и встали", ev(p, f"(list (< (abs (- {K}*dist* ({K}town-stop-d (first {K}*towns*)))) 3) (zerop {K}*speed*))"), "(T T)")
    check("бак пуст, мы у заправки", ev(p, f"(list (< {K}*fuel* 0.01) (and {K}*at-station* t))"), "(T T)")
    ev(p, f"({K}buy-fuel)")
    check("заправка на 10 р.", ev(p, f"(list {K}*money* (> {K}*fuel* 0.2))"), "(0 T)")
    ev(p, f"(setf {K}*window-open* t)")
    check("получатель подошёл сам", lambda: ev(p, f"(list (getf {K}*client* :kind) (<= (getf {K}*client* :arrive) 0))"), "(:RECIPIENT T)", within=15)
    check("окно при клиенте не закрыть", ev(p, f"(progn ({K}toggle-window) {K}*window-open*)"), "T")
    # take a parcel by hand and put it on our own windowsill
    ev(p, f"""(let ((it (find 0 {K}*items* :key (lambda (it) (sixth (car it))))))
        (setf {K}*view* :inside {K}*pos* (godot:vector3 (second (car it)) 0 -1.3) {K}*yaw* 0.0
              {K}*pitch* (float (atan (- (+ (third (car it)) 0.22) {K}*eye*) 1.2) 1.0))
        1)""")
    aim = ev(p, f"(list {K}*pos* {K}*yaw* {K}*pitch*)")
    check("E посылку не берёт", ev(p, f"(progn ({K}use-thing) {K}*carry*)"), "NIL")
    ev(p, f"""(let ((it (find 0 {K}*items* :key (lambda (it) (sixth (car it))))))      ; E мог усадить на табуретку — целимся заново
        (setf {K}*seat* nil {K}*pos* (godot:vector3 (second (car it)) 0 -1.3) {K}*yaw* 0.0
              {K}*pitch* (float (atan (- (+ (third (car it)) 0.22) {K}*eye*) 1.2) 1.0))
        1)""")
    check("ЛКМ берёт посылку", ev(p, f"(progn ({K}hand-thing) {K}*carry*)"), "(:BOX . 0)")
    ev(p, f"(setf {K}*pos* (godot:vector3 0.5 0 0.0) {K}*yaw* -1.57 {K}*pitch* -0.32)")
    check("ставим на подоконник", ev(p, f"(progn ({K}hand-thing) {K}*carry*)"), "NIL")
    check("посылка вручена, клиент ушёл", lambda: ev(p, f"(list ({K}parcels-left) {K}*client*)"), "(((1 . 1) (2 . 1)) NIL)", within=10)
    check("отправитель приходит", bring(p, ":SENDER"), True)
    check("берём его посылку — он платит",
          ev(p, f"""(let ((m {K}*money*)) ({K}take-from-stall (first (getf {K}*client* :stall)))
                 (list (> {K}*money* m) (car {K}*carry*) (integerp (cdr {K}*carry*))))"""), "(T :BOX T)")
    ev(p, f"""(progn (setf {K}*pos* (godot:vector3 0.3 0 -1.3) {K}*yaw* 0.0 {K}*pitch* -0.75) ({K}hand-thing) 1)""")
    check("посылка встала в кибитке",
          ev(p, f"(list {K}*carry* (reduce (function +) (mapcar (function cdr) ({K}parcels-left))))"), "(NIL 3)")
    check("покупатель приходит", bring(p, ":BUYER"), True)
    check("местный товар покупатель не берёт: рыбу в Заречном делают сами",
          ev(p, f"""(let ((m {K}*money*)) (setf {K}*carry* (cons :fish nil))
                 ({K}take-from-stall (first (getf {K}*client* :stall))) (list (= {K}*money* m) {K}*carry* (and {K}*client* t)))"""),
          "(T (:FISH) T)")
    check("отдаём из рук то, что он ищет, за монеты",
          ev(p, f"""(let ((m {K}*money*)) (setf {K}*carry* (cons ({K}client-good) nil))
                 ({K}take-from-stall (first (getf {K}*client* :stall))) (list (> {K}*money* m) {K}*carry* {K}*client*))"""),
          "(T NIL NIL)")
    check("продавец приходит", bring(p, ":SELLER"), True)
    check("покупаем у продавца то, что здесь в излишке или делают сами (по талонам — с талоном)",
          ev(p, f"""(let* ((m {K}*money*) (offer ({K}client-good))
                        (local (and (member offer (append ({K}surplus-goods 0) ({K}town-makes 0) ({K}rationed-goods 0))) t)))   ; before the deal: it changes the stock
                 (setf {K}*talons* (max 1 {K}*talons*))
                 ({K}take-from-stall (first (getf {K}*client* :stall)))
                 (list (< {K}*money* m) (eq (car {K}*carry*) offer) local))"""),
          "(T T T)")
    ev(p, f"(progn (when {K}*client* ({K}client-leave)) (setf {K}*carry* nil) ({K}show-carry) 1)")
    # the rack: slots
    check("слева на нижней полке место есть (проигрыватель переехал), верхняя свободна",
          ev(p, f"(list (and ({K}shelf-slot -1.0 -2.5 0.55 :box) t) (and ({K}shelf-slot 0.0 -2.5 1.65 :box) t))"), "(T T)")
    # music for coins
    ev(p, "(live:disc 0)")
    ev(p, f"(defparameter cl-user::*m0* {K}*money*)")
    check("музыка приносит монеты", lambda: ev(p, f"(> {K}*money* cl-user::*m0*)"), "T", within=24)
    # the tea table
    check("чай и сушки",
          ev(p, f"""(progn (when {K}*fold-down* ({K}toggle-fold)) ({K}pour-tea) ({K}eat-bagel)
                 (list (count-if (lambda (c) (godot:object-get c "visible")) {K}*cup-nodes*)
                       (count-if (lambda (c) (godot:object-get c "visible")) {K}*bagel-nodes*)))"""), "(1 5)")
    # U-turn: the money and the tank are intact, no customer
    check("разворот сохраняет деньги и бак",
          ev(p, f"""(let ((m {K}*money*) (f {K}*fuel*)) ({K}relocate (- {K}*dist* 200.0))
                 (list (= m {K}*money*) (= f {K}*fuel*) {K}*client* (length {K}*towns*)))"""), "(T T NIL 12)")
    time.sleep(2)
    # gas station: stepping outside
    ev(p, f"(progn ({K}jump-dist (first {K}*stations*)) (setf {K}*speed* 0.0) 1)")
    check("у заправки лесенка опущена, по площадке можно ходить, сквозь грузовик — нет",
          lambda: ev(p, f"(list {K}*ladder-down* ({K}walkable-p 5.0 6.0) ({K}walkable-p 0.0 -5.0))"), "(T T NIL)", within=9)
    # keys go through the action table
    press = lambda code: f"""(let ((e (gdext:make-object "InputEventKey")))
        (godot:object-set e "physical_keycode" {code}) (godot:object-set e "pressed" t) ({K}handle-input e))"""
    v0 = ev(p, f"{K}*view*")
    ev(p, press(86))
    check("V переключает вид", ev(p, f"(not (eq {K}*view* {v0}))"), "T")
    ev(p, press(72))
    check("H прячет приборы", ev(p, f"{K}*hud-hidden*"), "T")
    ev(p, press(77))
    check("M открывает карту, E её закрывает не сразу", ev(p, f"{K}*map-open*"), "T")
    ev(p, f"({K}close-map)")
    check("ошибок в журнале игры нет", log_errors(p), 0)
    stop_all()


def pair():
    print("Водитель и гость")
    clean()
    start(P0, KIOSK_HOST="1", KIOSK_NAME="Олег")
    code = ev(P0, f"{K}*net-room*").strip('"')
    for _ in range(20):
        if ev(P0, f"{K}*ws-open*") == "T":
            break
        time.sleep(0.5)
    start(P1, KIOSK_JOIN=code, KIOSK_NAME="Вася")
    time.sleep(6)
    h, g = P0, P1
    check("гость вошёл и видит тот же мир", ev(g, f"(list {K}*net-role* {K}*net-joined* {K}*trip-seed*)"), "(:GUEST T 1)")
    check("водитель видит гостя", ev(h, f"(mapcar (function second) {K}*net-others*)"), '("Вася")')
    for _ in range(5):
        ev(g, f"(progn (setf {K}*lights-on* (not {K}*lights-on*)) ({K}apply-lights) 1)")
        time.sleep(1.1)
    check("пять щелчков света у гостя дошли до водителя", ev(h, f"{K}*lights-on*"), ev(g, f"{K}*lights-on*"))
    check("…и свет выключен", ev(h, f"{K}*lights-on*"), "NIL")
    milk = f"(count :milk {K}*items* :key (lambda (it) (first (car it))))"
    take = f"""(let ((it (find :milk {K}*items* :key (lambda (it) (first (car it))))))
        (godot:node-queue-free (cdr it))
        (setf {K}*items* (remove it {K}*items*) {K}*carry* (cons :milk nil) {K}*carry-from* (car it) {K}*took* (car it)) 1)"""
    ev(g, take)
    check("гость взял молоко — у водителя его стало три", lambda: ev(h, milk), lambda v: v.startswith("3"), within=8)
    check("…и оно у гостя в руках", ev(g, f"{K}*carry*"), "(:MILK)")
    # both take the very same box
    ev(h, f"""(let ((it (find :milk {K}*items* :key (lambda (it) (first (car it))))))
        (godot:node-queue-free (cdr it)) (setf {K}*items* (remove it {K}*items*)) 1)""")
    ev(g, f"(progn (setf {K}*carry* nil) ({K}show-carry) 1)")
    ev(g, take)
    check("одну коробку взяли оба — у гостя руки пусты", lambda: ev(g, f"{K}*carry*"), "NIL", within=10)
    check("…и молока у обоих поровну", ev(h, milk), ev(g, milk))
    # the customer is shared
    arrive_first_market(h)
    ev(h, f"(setf {K}*window-open* t)")
    check("гость видит получателя у окна", lambda: ev(g, f"(getf {K}*client* :kind)"), ":RECIPIENT", within=18)
    check("получатель дошёл до окна", lambda: ev(h, f"(and {K}*client* (<= (getf {K}*client* :arrive) 0) t)"), "T", within=12)
    ev(g, f"""(let ((it (find 0 {K}*items* :key (lambda (it) (sixth (car it))))))
        (godot:node-queue-free (cdr it))
        (setf {K}*items* (remove it {K}*items*) {K}*carry* (cons :box 0) {K}*carry-from* (car it) {K}*took* (car it))
        ({K}show-carry)
        (setf {K}*view* :inside {K}*pos* (godot:vector3 0.5 0 0.0) {K}*yaw* -1.57 {K}*pitch* -0.32)
        ({K}carry-use) 1)""")
    check("гость вручил посылку: у обоих её нет, руки пусты",
          lambda: (ev(h, f"({K}parcels-left)"), ev(g, f"(list ({K}parcels-left) {K}*carry*)")),
          ("((1 . 1) (2 . 1))", "(((1 . 1) (2 . 1)) NIL)"), within=15)
    ev(g, f"({K}ring-bell)")
    check("гость позвонил — клиент пришёл к обоим",
          lambda: (ev(h, f"(and {K}*client* t)"), ev(g, f"(and {K}*client* t)")), ("T", "T"), within=14)
    # one wave: the guest does shared jobs itself, and both have the same world state
    both = lambda body: (ev(h, body), ev(g, body))
    same = lambda name, body: check(name, *both(body))
    digest = f"({K}world-digest)"
    time.sleep(1.0)
    same("состояние мира у водителя и гостя совпадает", digest)
    ev(g, f"(progn ({K}pick-gnome 3) 1)")
    check("гость взял гномика — он на полке у обоих", lambda: both(f"(and ({K}gnome-found-p 3) t)"), ("T", "T"), within=4)
    ev(h, f"(progn (setf {K}*money* 50) (push 6 {K}*fines*) 1)")
    check("штраф, выписанный водителю, лежит и в папке гостя", lambda: ev(g, f"{K}*fines*"), "(6)", within=4)
    ev(g, f"(progn ({K}pay-fines) 1)")
    check("гость оплатил штраф из общего кошелька", lambda: both(f"(list {K}*fines* {K}*money*)"), ("(NIL 44)", "(NIL 44)"), within=4)
    ev(g, f"(progn ({K}use-quests) 1)")
    check("гость взял задание горсовета — оно у обоих", lambda: both(f"(car {K}*quest*)"), (":POST", ":POST"), within=4)
    ev(g, f"(progn ({K}pick-find (round {K}*dist*) :bottle 0) 1)")
    check("гость подобрал бутылку — счёт общий", lambda: both(f"{K}*bottles*"), lambda v: v[0].startswith("1 ") and v[1].startswith("1 "), within=4)
    ev(h, f"(progn ({K}say-all (string #\\A)) 1)")
    check("сказанное всем дошло до гостя", lambda: ev(g, f"(car {K}*toast*)"), '"A"', within=4)
    same("после дел гостя состояние мира снова совпадает", digest)
    ev(h, f"(progn ({K}go-to-town (second {K}*towns*)) 1)")
    time.sleep(1.5)
    same("куда едем — знают оба", f"(cdr {K}*auto*)")
    # the guest's look (clothes, flashlight) reaches the driver
    ev(g, f"(progn ({K}toggle-torch) 1)")
    check("гость включил фонарик — водитель это видит", lambda: ev(h, f"(and (find-if (lambda (o) (>= (eighth o) 100)) {K}*net-others*) t)"), "T", within=6)
    ev(g, f"(progn ({K}toggle-torch) 1)")
    # live editing for everyone: a value, a new function and an error
    ev(h, f"(progn ({K}everyone \"(setf *tank-km* 9.5) (defun probe-fn (x) (* x 3))\") 1)")
    check("правка водителя выполнилась и у гостя: число и новая функция",
          lambda: ev(g, f"(list {K}*tank-km* (and (fboundp '{K}probe-fn) ({K}probe-fn 14)))"), "(9.5 42)", within=6)
    check("гость ответил водителю результатом", lambda: ev(h, f"(cdr (first {K}*answers*))"), '"=> PROBE-FN"', within=6)
    ev(h, f"(progn ({K}everyone \"(error (string #\\Z))\") 1)")
    check("ошибка у гостя не роняет игру, а приходит водителю текстом",
          lambda: ev(h, f"(and {K}*answers* (search \"ошибка\" (cdr (first {K}*answers*))) t)"), "T", within=6)
    # the guest goes back to its own game: the world state is its own again, not the driver's
    check("в гостях у гостя — хозяйские гномик и задание", ev(g, f"(list (and ({K}gnome-found-p 3) t) (car {K}*quest*))"), "(T :POST)")
    ev(g, f"(progn ({K}net-stop) 1)")
    check("вышел из комнаты — гномики, задание, штрафы и бутылки снова свои",
          lambda: ev(g, f"(list (and ({K}gnome-found-p 3) t) {K}*quest* {K}*fines* {K}*bottles* {K}*net-role*)"), "(NIL NIL NIL 0 NIL)", within=6)
    check("ошибок в журналах нет", log_errors(h) + log_errors(g), 0)
    stop_all()


def saves():
    """Check the saves.

    Old files move into the new one, a restart puts everything back in place, and a corrupted
    file doesn't crash the game — the backup copy is read."""
    print("Сохранения")
    p = P0
    clean()
    os.makedirs(USER, exist_ok=True)
    open(os.path.join(USER, "game.sexp"), "w").write("(700 77 0.5 3 2)")        # an old-format file
    start(p, KIOSK_SOLO="1", KIOSK_NOLAND="1", KIOSK_NAME="Проба")
    check("старое сохранение переехало: деньги на месте", ev(p, f"{K}*money*"), lambda v: v.startswith("77"))
    ev(p, f"(progn (setf {K}*money* 80) ({K}pick-gnome 2) ({K}spend 4) ({K}relocate (find-if-not (function {K}town-at) {K}*stop-ds* :start 8)) ({K}head-to -1) 1)")
    ev(p, f"(progn (setf {K}*game-timer* 0.0) ({K}save-game 1.0) ({K}flush-saves) 1)")      # don't wait for the save timer
    check("новый файл записан", os.path.exists(os.path.join(USER, "save.sexp")), True)
    stop_all()
    start(p, KIOSK_SOLO="1", KIOSK_NOLAND="1", KIOSK_NAME="Проба")
    check("после перезапуска: деньги, гномик, «взято сегодня», направление",
          ev(p, f"(list {K}*money* (and ({K}gnome-found-p 2) t) (and (assoc 4 {K}*gimmick-spent*) t) {K}*dir*)"), "(80 T T -1)")
    ev(p, f"(progn (setf {K}*money* 81) 1)")
    ev(p, f"(progn (setf {K}*game-timer* 0.0) ({K}save-game 1.0) ({K}flush-saves) 1)")
    check("прежний файл остался запасной копией", os.path.exists(os.path.join(USER, "save.sexp.bak")), True)
    stop_all()
    open(os.path.join(USER, "save.sexp"), "w").write("((( мусор")                 # the write was cut short
    start(p, KIOSK_SOLO="1", KIOSK_NOLAND="1", KIOSK_NAME="Проба")
    check("файл испорчен — игра поднялась с запасной копии", ev(p, f"(list {K}*money* (and ({K}gnome-found-p 2) t))"), "(80 T)")
    check("ошибок в журнале игры нет", log_errors(p), 0)
    stop_all()


def tour():
    """Drive around the whole road.

    In every town, at the bridge, the tunnel and the swamp the world must assemble without errors.
    Catches what can't be seen from the start — a broken building standing ten kilometers away."""
    print("Объезд дороги")
    p = P0
    start(p, KIOSK_SOLO="1", KIOSK_NOLAND="1", KIOSK_NAME="Проба")   # no network and no land: we look at the buildings
    spots = ev(p, f"(append (loop for tw in {K}*towns* append (loop for f in '(.15 .45 .75) collect (round (+ ({K}town-start tw) (* f (- ({K}town-end tw) ({K}town-start tw))))))) (list (round (- {K}*chasm* 90)) (round {K}*chasm*) (round (- (first {K}*tunnel*) 60)) (round (+ (first {K}*tunnel*) 80)) (round (+ (first {K}*swamp*) 300))) (loop for (i f) in {K}*landmark-plan* collect (round ({K}gap-d i f))) (loop for (i f) in {K}*diner-plan* collect (round ({K}gap-d i f))) (loop for e in (rest {K}*edges*) append (list (round (+ ({K}edge-d0 e) 400)) (round (/ (+ ({K}edge-d0 e) ({K}edge-d1 e)) 2)) (round (- ({K}edge-d1 e) 260)))))")
    spots = [int(x) for x in spots.replace("(", " ").replace(")", " ").split() if x.lstrip("-").isdigit()]
    for night in ("nil", "t"):
        ev(p, f"(setf {K}*day-t* {'900.0' if night == 't' else '300.0'})")
        for d in spots[:: (1 if night == 'nil' else 6)]:
            ev(p, f"(progn ({K}relocate {d - 40}) t)")
            time.sleep(0.5)
            ev(p, f"(progn (setf {K}*next-event* 0.0 {K}*next-pass* 0.0) t)")
            time.sleep(.25)
    check(f"объезд {len(spots)} мест днём и ночью: игра жива", ev(p, "(+ 1 1)"), lambda v: v.startswith("2"))
    check("ошибок в журнале игры нет", log_errors(p), 0)
    stop_all()


def back():
    """Check the two-way roads.

    A U-turn in town, driving back in the oncoming lane, the zebra crossing, turns at
    the Crossroads in both directions and a lap of the ring road backwards."""
    print("Обратная дорога")
    p = P0
    start(p, KIOSK_SOLO="1", KIOSK_NOLAND="1", KIOSK_NAME="Проба")
    num = lambda v: float(v.split()[0].replace("d", "e")) if v not in ("NIL", "nil") else None
    ev(p, f"(progn ({K}relocate ({K}town-stop-d (first {K}*towns*))) (setf {K}*speed* 0.0) t)")
    time.sleep(1.5)
    ev(p, f"(progn ({K}go-to-town (car (last {K}*towns*))) t)")
    check("в город позади — разворот", ev(p, f"{K}*dir*"), lambda v: v.startswith("-1"))
    check("развернулись и встали на свою полосу", lambda: ev(p, f"(list ({K}turning-p) (round {K}*lane-now* .5))"), lambda v: v.upper().startswith("(NIL -9"), within=14)
    check("после разворота поехали назад", lambda: num(ev(p, f"(- ({K}town-stop-d (first {K}*towns*)) {K}*dist*)")), lambda v: v > 1.0, within=8)
    ev(p, f"(progn ({K}relocate (+ ({K}town-stop-d (car (last {K}*towns*))) 120.0)) (setf {K}*speed* 8.0) t)")
    check("доехали назад до города и встали", lambda: ev(p, f"(list (null {K}*auto*) (< (abs (- {K}*dist* ({K}town-stop-d (car (last {K}*towns*))))) 3.0))").upper(), "(T T)", within=72)
    time.sleep(1.0)
    ev(p, f"(progn (setf {K}*ladder-down* t {K}*ladder-k* 1.0) t)")
    check("снаружи ходим где угодно", lambda: ev(p, f"(and ({K}ground-walkable-p -5.0 9.0) ({K}ground-walkable-p 140.0 -90.0) t)").upper(), "T", within=4)
    check("сквозь грузовик не пройти", ev(p, f"({K}ground-walkable-p 1.0 -4.0)").upper(), "NIL")
    wall = f"""(let ((best nil))
        (maphash (lambda (k v) (declare (ignore k)) (dolist (s v) (when (> (max ({K}solid-hw s) ({K}solid-hd s)) 3.0) (setf best s)))) {K}*solid-grid*)
        (and best (multiple-value-bind (lx lz) ({K}cart-local ({K}solid-x best) ({K}solid-z best)) (list (and ({K}ground-walkable-p lx lz) t) (and ({K}ground-walkable-p -2.5 -14.0) t)))))"""
    check("в городе в дом не войти, а по дороге идти можно", ev(p, wall).upper(), "(NIL T)")
    check("встав в городе носом назад, кибитка заезжает на площадку", ev(p, f"{K}*dir*"), lambda v: v.startswith("1"))
    check("на трассе ждущие стоят на нашей стороне", ev(p, f"(let (({K}*dir* -1)) (< (second ({K}stop-spot (find-if-not (function {K}town-at) {K}*stop-ds*) 0)) {K}*lane-x*))").upper(), "T")
    # a distant goal: we take all the planned turns in order, driving right up to each one
    ev(p, f"(progn ({K}go-to-town (nth 5 {K}*towns*)) t)")
    time.sleep(4.5)
    for i in range(6):
        hop = ev(p, f"(first {K}*way*)")
        if hop.upper() == "NIL":
            break
        ev(p, f"(let ((h (first {K}*way*))) ({K}relocate (- (first h) (* {K}*dir* 25.0))) (setf {K}*speed* 12.0) t)")
        check(f"поворот {i + 1} пройден", lambda: ev(p, f"(equal (first {K}*way*) '{hop})").upper(), "NIL", within=16)
    ev(p, f"(progn ({K}relocate (- ({K}town-stop-d (nth 5 {K}*towns*)) (* {K}*dir* 100.0))) (setf {K}*speed* 8.0) t)")
    check("через повороты доехали до цели", lambda: ev(p, f"(list (null {K}*auto*) (< (abs (- {K}*dist* ({K}town-stop-d (nth 5 {K}*towns*)))) 3.0))").upper(), "(T T)", within=72)
    # at a fork you also see what stands on the neighboring road: the path coordinate is an address, not space
    ev(p, f"(progn (setf {K}*auto* nil {K}*way* nil {K}*dir* 1 {K}*yaw-now* 0.0) ({K}relocate (- ({K}edge-d1 (cdr (first {K}*exits*))) 70.0)) (setf {K}*speed* 0.0) t)")
    time.sleep(2.0)
    roads = f"(length (remove-duplicates (mapcar (lambda (r) ({K}edge-d0 ({K}edge-of (second r)))) {K}*roadside*)))"
    check("у Перекрёстка построено и то, что стоит на соседних дорогах", num(ev(p, roads)), lambda v: v >= 3)
    # a lap of the ring road backwards: going back from the start of the ring road, we end up at its end
    ev(p, f"(progn (setf {K}*dir* -1 {K}*yaw-now* pi {K}*auto* (cons ({K}town-stop-d (car (last {K}*towns*))) \"проба\") {K}*way* nil) ({K}relocate (+ ({K}edge-d0 ({K}ring)) 20.0)) (setf {K}*speed* 12.0) t)")
    check("назад через начало кольца — в его конец", lambda: num(ev(p, f"(- ({K}edge-d1 ({K}ring)) {K}*dist*)")), lambda v: 0 < v < 400, within=24)
    check("ошибок в журнале игры нет", log_errors(p), 0)
    stop_all()


if __name__ == "__main__":
    which = sys.argv[1:] or ["solo", "pair", "tour", "back", "saves"]
    stash()
    try:
        if "solo" in which:
            solo()
        if "pair" in which:
            pair()
        if "tour" in which:
            tour()
        if "back" in which:
            back()
        if "saves" in which:
            saves()
    finally:
        stop_all()
        unstash()
    print("\nВсё сошлось" if not fails else f"\nПровалов: {len(fails)} — " + "; ".join(fails))
    sys.exit(len(fails))
