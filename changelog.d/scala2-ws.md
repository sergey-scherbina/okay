## scala2-ws - WebSockets from Scala 2.13: a client, sessions as folds, a server over okay-jetty

- Probed from scalac 2.13.18: okay-http's `Frame`, `Socket`,
  `Transports.sockets()` and okay-jetty's `Jetty` are readable, and
  matching on `Frame` works. A socket's operations and a server
  session (a `Stage[Frame, Frame, Unit]`) are programs.
- The new module okay-scala2-ws (its own module, so okay-scala2-http
  does not pull Jetty) adds:
  - `WebSocket.connect` returning a `WsClient` (`send`, `sendText`,
    `close`, and `frames`/`texts` as a `Source`);
  - `WsSession.fold(init)(step)`, a server session as a fold over
    `Stage.transduce`, with `echo` and `replay` (a session run with no
    socket, which is how one is tested);
  - `WsServer.use`, routes and upgrades on one port over okay-jetty;
  - `WebSocket.binary`/`bytes` for `Array[Byte]`.
- A trap: `okay.Chunk` is a Scala 3 top-level alias, invisible from
  Scala 2, but the type it names, `ArraySeq`, is visible. So
  `Frame.Ping(ArraySeq[Byte](...))` works from Scala 2 as it is.
- Probe: `TestWsFromScala2` (3 tests, in the gate) and
  `TestWsLiveFromScala2` (1 test: a real Jetty, a real client socket,
  and an ordinary route on the same port; run and green here).
- Docs: section 8g of docs/scala2.md (copied from the probe), a module
  page, API reference, typepedia, and spec stage 12.
