## app-in-process - the app's window without a port

okay-watch's operator, 2026-09-29, after the installed app would not open
because a Docker copy of the same service held its port: «проблема с
необходимостью угадывать порт внутри приложения — это разве не абсурд? …
чтобы внутри процесса все вообще работало просто через стримы без http».
specs/app-in-process.md.

- Measured first, under Xvfb on JavaFX 26: a scheme of our own (`app://`)
  loads pages, links and GET forms in the process; the embedded WebKit will
  not POST a form, `fetch` or XHR to it, nor `pushState` (a sandboxed
  document); the page → Java bridge works. So reads go through the scheme
  and writes through the bridge.
- okay-desktop: `InProcess.Server` runs a service's routes in the process —
  redirects followed as a browser does, cookies kept, peer 127.0.0.1 — and
  `InProcess.install` puts it behind `app://<host>`; a redirect is shown as
  a page that goes on, its answer held; `hold`/`take`, the answer a POST
  already got, served to the navigation it causes. `Transport` (in the
  process, or `http` for the browser road) for everything the window asks.
  The bridge's `send` (off the UI thread, always answering) and `open`.
  `App.script` sends every POST form of an `app://` page through it.
  `Instance`: one copy per data folder by a lock and a Unix-domain socket
  in it (`front`), not by who answers a port. `launch` with
  `Mode.InWindow(hand)` (no port) or `Mode.OnPort(port)` (a free one,
  `Desktop.free` checking every address and the loopback alone —
  okay-watch's check, moved here), and `failed` for the product's word on
  a start that failed. `Desktop.running`/`front(port)` are gone.
- okay-ui: `Enhance` has one sending step — the bridge when the page has
  one, `fetch` otherwise — and in the app navigates (`okayApp.open`) where
  a site would `pushState`.
- Tests: TestInProcess (6), TestInstance (2), TestDesktop (+1), TestFreePort
  (2, Live), TestChrome (+1). The window with no port under Xvfb: five
  screenshots, zero listening sockets, a second start ended in 1 s.
