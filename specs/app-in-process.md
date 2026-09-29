# app-in-process — the app's window without a port

## Overview

okay-watch's operator, 2026-09-29, after the installed app would not
start because a Docker copy of the same service held port 8099: *«меня
беспокоит вообще вся эта проблема с необходимостью угадывать порт
внутри приложения — это разве не абсурд? … чтобы внутри процесса все
вообще работало просто через стримы без http … проблема решаемая или
нет?»*.

It is. The window and the service are ONE process, and a route table is
already a function — `PartialFunction[Request, Response ! Async]`,
which every test calls without a socket. The port exists only because
the embedded WebKit loads `http://` by default. What that costs, all
seen:

- a port to guess, and nothing to say when it is taken (a Docker copy
  of the same service on 8099 answered the app's `/healthz`, so the app
  took it for its own first copy, asked it to come to the front, and
  quit — silently);
- "is a copy running" judged by who answers on the port;
- a loopback HTTP server in a desktop program, with its cookies, its
  local-only filter and its second-copy route.

## What the embedded WebKit does (measured 2026-09-29, JavaFX 26, Xvfb)

A URL scheme of our own, `app://`, with its handler in the process
(`URL.setURLStreamHandlerFactory`):

| what | result |
| --- | --- |
| a page loaded, a link followed, a GET form | works — no socket |
| a POST form to `app://` | not sent |
| `fetch` / XHR to `app://` | "Load failed" |
| `history.pushState` to another `app://` path | SecurityError: a sandboxed document |
| the page → Java bridge (`window.okayApp`) | works; carries method, path, body |

So reads go through the scheme, writes through the bridge, and a write
that lands on another page is a real navigation whose answer the window
already holds.

## Design

### okay-desktop

- `InProcess.Server(routes)` — one service's routes, in the process:
  `send(method, target, headers, body)` runs the route and follows its
  redirects the way a browser does (303 and 301/302 become a GET,
  307/308 keep the method), keeping the cookies the answers set; the
  answer is the final status, headers, bytes and the URL they came from.
  A request carries `peer = 127.0.0.1`: it came from this computer.
- `InProcess.base(name)` = `app://<name>`; `InProcess.install(host,
  server)` registers the scheme once per JVM (the factory can be set
  once; it dispatches by host).
- The scheme's handler answers a GET from the server; a redirect it
  answers with a page that goes on (`<meta http-equiv=refresh>` and
  `location.replace`), so the document's URL is always the page's own.
- **The held answer**: `Server.hold(url, answer)` — the next GET of
  exactly that URL is answered with it, once. That is how a POST whose
  answer is another page becomes a navigation without asking twice.
- `Transport` — what the window does over its service, whichever road:
  `Transport.http(base, cookies)` (the browser road, a port) or
  `Transport.inProcess(server)`; saving, the file dialogs, "is it busy"
  and the menu's POSTs all go through it.
- The bridge (`window.okayApp`): `send(method, url, body, done)` runs
  the request off the UI thread and calls `done(json)` with
  `{status, url, body}`; `open(url)` holds the last answer for `url`
  and navigates to it. `save`, `savePost` and `pick(i)` as before.
- `App.script` (installed on every page): in the app, EVERY form the
  page itself would post — outside the frame, `data-hard`, the first
  screen — goes through `send`, then `open` of where it landed.
- **One copy**: `Instance.claim(data)` takes a lock on `<data>/app.lock`
  (`FileChannel.tryLock`); the first copy listens on a Unix-domain
  socket, `<data>/app.sock`; a second start finds the lock held, says
  `front` on the socket and ends. No port, and nothing else can answer
  for it. Where the socket cannot be made, the second start still ends.
- `Desktop.launch(app, data, serve, …)`: with the window, `serve` gets
  `Mode.InWindow(hand)` and hands its routes over — no port at all;
  without JavaFX or a screen (the browser road), `Mode.OnPort(port)`
  with the product's preferred port if it is free, else any free one,
  written to `<data>/url.txt` so a second start opens the right page.

### okay-ui — Enhance

- One step sends: `send(method, url, body)` → `[html, url]`. In the app
  (`window.okayApp.send` exists) it is the bridge; on a site it is
  `fetch`, as before. A press and the `okay-refresh` fetch both use it.
- An answer from another page, in the app: `okayApp.open(url)` (a real
  navigation, the held answer) instead of `pushState`, which a
  sandboxed document may not do. On a site, `pushState` as before.

## Behavior

- [ ] `Server.send`: a GET answered; a POST that redirects lands on the
      GET of the target; cookies set by one answer go with the next
- [ ] a request through the server carries peer `127.0.0.1`
- [ ] `hold`: the next GET of that URL is the held answer, once
- [ ] the scheme's handler: a page, a redirect as a going-on page, bytes
      with their content type; an unknown host is a 404 page
- [ ] `Transport.inProcess` saves the bytes a route gives, with its
      file name
- [ ] `Instance`: the first claim holds; a second claim in the same data
      folder is refused and its `front` reaches the first
- [ ] `freePort`: the preferred port when free, another when taken
- [ ] `App.script` sends every form through the bridge in the app
- [ ] `Enhance.script` sends through the bridge when there is one, by
      `fetch` otherwise, and navigates rather than `pushState` in the app
- [ ] under Xvfb, the app's window with no port: the first screen, a
      form posted, a page after a redirect — screenshots

## Decisions

- Not our own engine: the report, the annex and print are HTML
  documents, and the look is CSS. okay-ui trees could be drawn natively
  (the Swing host exists) — a different choice, about looks, not ports.
- Not intercepting `http://`: JavaFX 18+ loads http through
  `java.net.http.HttpClient` (the HTTP/2 loader), not through the
  `URLStreamHandler` a factory could replace; and taking over `http`
  would take over every outside page too.
- The server (Docker, Enterprise) keeps its port — a server is its port.
