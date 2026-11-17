// A room = a Durable Object. It holds one driver (seat 0) and up to seven guests (seats 1–7).
// The server knows nothing about the game, it only relays text:
//   guest → server:  "text"             → the driver gets "N|text"
//   driver → server: "N|text"           → guest N gets "text"
//   to the driver about guests: "N|+" joined, "N|-" left
//   driver → server: "N|!"              → disconnect guest N (wrong game version), code 4005
// The driver left — the room closes and disconnects the guests (code 4001).
// A guest knocks on a room with no driver — refused (code 4004). No seats left — 4003.
// Limits against fools and against the bill: a message of up to 16 thousand characters, up to 120 messages per second per socket.

const MAX_GUESTS = 7;
const MAX_BYTES = 16384;
const MAX_PER_SECOND = 120; // the driver sends to each guest separately
const CODE = /^[A-Z]{4}$/;

export default {
  async fetch(request, env) {
    const url = new URL(request.url);
    if (url.pathname === "/log" || url.pathname === "/logs") return env.LOG.get(env.LOG.idFromName("log")).fetch(request);
    if (url.pathname === "/list") {
      // the list of open lobbies — in one WebSocket message (easier for the game than HTTP): lines of "CODE\tguests\tdriver name"
      if (request.headers.get("Upgrade") !== "websocket") return new Response("нужен WebSocket", { status: 426 });
      const pair = new WebSocketPair();
      pair[1].accept();
      const list = await (await env.DIR.get(env.DIR.idFromName("dir")).fetch("https://dir/list?v=" + encodeURIComponent(url.searchParams.get("v") || ""))).text();
      pair[1].send(list || " ");
      // we don't close it ourselves: the game has no time to read the reply if the close follows right behind; the game will close it
      pair[1].addEventListener("close", () => { try { pair[1].close(1000, "всё"); } catch {} });
      return new Response(null, { status: 101, webSocket: pair[0] });
    }
    if (url.pathname !== "/ws") return new Response("kibitka relay\n");
    if (request.headers.get("Upgrade") !== "websocket") return new Response("нужен WebSocket", { status: 426 });
    const room = (url.searchParams.get("room") || "").toUpperCase();
    if (!CODE.test(room)) return new Response("код комнаты — четыре латинские буквы", { status: 400 });
    return env.ROOM.get(env.ROOM.idFromName(room)).fetch(request);
  },
};

export class Room {
  constructor(state, env) {
    this.state = state;
    this.env = env;
    this.rate = new Map(); // socket → [second, message count]
    this.left = new WeakSet(); // sockets whose departure has already been reported
  }

  seat(ws) { return ws.deserializeAttachment().seat; }

  // Tell the lobby directory (Dir) that the room is open and how many guests it has; gone — that it is closed.
  async announce(gone) {
    const host = this.host();
    const dir = this.env.DIR.get(this.env.DIR.idFromName("dir"));
    try {
      if (gone || !host) { if (this.code) await dir.fetch("https://dir/del?code=" + this.code); return; }
      const a = host.deserializeAttachment();
      this.code = a.code;
      const guests = this.state.getWebSockets().length - 1;
      await dir.fetch("https://dir/set?code=" + a.code + "&n=" + guests + "&v=" + encodeURIComponent(a.v || "") + "&name=" + encodeURIComponent(a.name));
    } catch {}
  }
  // the live driver: when a connection is replaced the old one may still be listed — skip it
  host() { return this.state.getWebSockets("host").find((h) => !this.left.has(h)); }
  guest(n) { return this.state.getWebSockets("g" + n)[0]; }

  async fetch(request) {
    const params = new URL(request.url).searchParams;
    const role = params.get("role");
    const code = (params.get("room") || "").toUpperCase();
    const name = (params.get("name") || "Водитель").slice(0, 16);
    const pair = new WebSocketPair();
    const [client, server] = [pair[0], pair[1]];
    const refuse = (code, why) => {
      server.accept();
      server.close(code, why);
      return new Response(null, { status: 101, webSocket: client });
    };
    if (role === "host") {
      const key = (params.get("key") || "").slice(0, 24);
      const old = this.host();
      if (old) {
        // the same driver came back after a drop while the old connection is still listed as alive: replace it,
        // the guests stay in the room. A stranger (a different key) cannot take the room.
        const was = old.deserializeAttachment();
        if (!key || was.key !== key) return refuse(4002, "комната занята");
        this.left.add(old);
        try { old.close(4006, "водитель переподключился"); } catch {}
      }
      this.state.acceptWebSocket(server, ["host"]);
      server.serializeAttachment({ seat: 0, code, name, key, v: (params.get("v") || "").slice(0, 16) });
      // guests who survived the drop have "just joined" as far as the returning driver is concerned
      for (const g of this.state.getWebSockets()) {
        const a = g.deserializeAttachment();
        if (g !== server && a.seat > 0) try { server.send(a.seat + "|+"); } catch {}
      }
      await this.announce();
    } else {
      if (!this.host()) return refuse(4004, "нет такой комнаты");
      let n = 1;
      while (n <= MAX_GUESTS && this.guest(n)) n++;
      if (n > MAX_GUESTS) return refuse(4003, "мест нет");
      this.state.acceptWebSocket(server, ["g" + n]);
      server.serializeAttachment({ seat: n });
      try { this.host().send(n + "|+"); } catch {}
      await this.announce();
    }
    return new Response(null, { status: 101, webSocket: client });
  }

  async webSocketMessage(ws, data) {
    if (typeof data !== "string" || data.length > MAX_BYTES) return;
    const now = Math.floor(Date.now() / 1000);
    const r = this.rate.get(ws);
    if (r && r[0] === now) {
      // we don't swallow a flood silently, we cut the socket: incoming messages count against the plan's limit
      if (++r[1] > MAX_PER_SECOND) { this.gone(ws); try { ws.close(4008, "слишком часто"); } catch {} return; }
    } else this.rate.set(ws, [now, 1]);
    const seat = this.seat(ws);
    if (seat === 0) {
      if (data === "~") { await this.announce(); return; } // once a minute the driver confirms the lobby is alive
      const bar = data.indexOf("|");
      const to = this.guest(Number(data.slice(0, bar)));
      if (bar <= 0 || !to) return;
      const body = data.slice(bar + 1);
      if (body === "!") { this.gone(to); try { to.close(4005, "не та версия"); } catch {} }
      else try { to.send(body); } catch {}
    } else {
      const host = this.host();
      if (host) try { host.send(seat + "|" + data); } catch {}
    }
  }

  async webSocketClose(ws) { await this.gone(ws); }
  async webSocketError(ws) { await this.gone(ws); }

  async gone(ws) {
    if (this.left.has(ws)) return; // both a close and an error may arrive — report only once
    this.left.add(ws);
    this.rate.delete(ws);
    const a = ws.deserializeAttachment();
    if (a.seat === 0) {
      this.code = a.code;
      for (const g of this.state.getWebSockets()) if (g !== ws) try { g.close(4001, "водитель ушёл"); } catch {}
      await this.announce(true);
    } else {
      const host = this.host();
      if (host) try { host.send(a.seat + "|-"); } catch {}
      // the departing socket itself is still listed in the room — count without it
      try {
        const h = host && host.deserializeAttachment();
        if (h) await this.env.DIR.get(this.env.DIR.idFromName("dir")).fetch("https://dir/set?code=" + h.code + "&n=" + Math.max(0, this.state.getWebSockets().length - 2) + "&v=" + encodeURIComponent(h.v || "") + "&name=" + encodeURIComponent(h.name));
      } catch {}
    }
  }
}

// The game log: what happens on the players' side (start, lobby, deliveries, refuelings, errors, frames per second).
// The game sends POST /log?sid=…&v=… — the body holds JSON lines, one event per line. Only the author reads it:
// GET /logs?key=SECRET&since=NUMBER (the LOG_KEY secret — `wrangler secret put LOG_KEY`).
// Stored in this Durable Object's SQLite, at most the 50 thousand latest rows.
const LOG_KEEP = 50000;
const CORS = { "Access-Control-Allow-Origin": "*", "Access-Control-Allow-Methods": "POST, OPTIONS", "Access-Control-Allow-Headers": "Content-Type" };

export class Log {
  constructor(state, env) {
    this.sql = state.storage.sql;
    this.env = env;
    this.sql.exec("CREATE TABLE IF NOT EXISTS log (id INTEGER PRIMARY KEY AUTOINCREMENT, t INTEGER, sid TEXT, v TEXT, line TEXT)");
  }

  async fetch(request) {
    const url = new URL(request.url);
    if (request.method === "OPTIONS") return new Response(null, { status: 204, headers: CORS });
    if (url.pathname === "/log" && request.method === "POST") {
      const body = (await request.text()).slice(0, 16384);
      const sid = (url.searchParams.get("sid") || "").slice(0, 16);
      const v = (url.searchParams.get("v") || "").slice(0, 16);
      const now = Date.now();
      for (const line of body.split("\n").slice(0, 80)) {
        if (line.length > 1 && line.length <= 600) this.sql.exec("INSERT INTO log (t, sid, v, line) VALUES (?, ?, ?, ?)", now, sid, v, line);
      }
      this.sql.exec("DELETE FROM log WHERE id <= (SELECT MAX(id) FROM log) - ?", LOG_KEEP);
      return new Response("ok", { headers: CORS });
    }
    if (url.pathname === "/logs" && request.method === "GET") {
      if (!this.env.LOG_KEY || url.searchParams.get("key") !== this.env.LOG_KEY) return new Response("нет", { status: 403 });
      const since = Number(url.searchParams.get("since") || 0);
      const limit = Math.min(Number(url.searchParams.get("limit") || 2000), 5000);
      const rows = this.sql.exec("SELECT id, t, sid, v, line FROM log WHERE id > ? ORDER BY id LIMIT ?", since, limit).toArray();
      return Response.json(rows);
    }
    return new Response("нет", { status: 404 });
  }
}

// The directory of open lobbies: which rooms are waiting for guests right now. No password — everyone with the game sees it.
// The room writes here itself (Room.announce); an entry lives 3 minutes without a confirmation from the driver,
// so that crashed games don't linger in the list.
const DIR_FRESH = 180000;

export class Dir {
  constructor(state) {
    this.sql = state.storage.sql;
    this.sql.exec("CREATE TABLE IF NOT EXISTS rooms (code TEXT PRIMARY KEY, name TEXT, n INTEGER, t INTEGER)");
    try { this.sql.exec("ALTER TABLE rooms ADD COLUMN v TEXT"); } catch {} // the driver's build version: other versions don't make it into the list
  }

  async fetch(request) {
    const url = new URL(request.url);
    const code = (url.searchParams.get("code") || "").slice(0, 4);
    if (url.pathname === "/set" && /^[A-Z]{4}$/.test(code)) {
      const name = (url.searchParams.get("name") || "").replace(/[\t\n]/g, " ").slice(0, 16);
      this.sql.exec("INSERT OR REPLACE INTO rooms (code, name, n, t, v) VALUES (?, ?, ?, ?, ?)", code, name, Number(url.searchParams.get("n")) || 0, Date.now(), (url.searchParams.get("v") || "").slice(0, 16));
    } else if (url.pathname === "/del") {
      this.sql.exec("DELETE FROM rooms WHERE code = ?", code);
    } else if (url.pathname === "/list") {
      this.sql.exec("DELETE FROM rooms WHERE t < ?", Date.now() - DIR_FRESH);
      const rows = this.sql.exec("SELECT code, n, name FROM rooms WHERE v = ? ORDER BY t DESC LIMIT 12", url.searchParams.get("v") || "").toArray();
      return new Response(rows.map((r) => r.code + "\t" + r.n + "\t" + r.name).join("\n"));
    }
    return new Response("ok");
  }
}
