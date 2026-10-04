## okay2-http-js-transports - okay-http's Scala.js transports on okay2

Part C of okay2-http (spec stage 44, docs §35). On Scala.js,
`Transports.fetch` is an `Http` over the global `fetch` that reads the
body one chunk at a time, and `Transports.sockets()` gives `Sockets` over
the global `WebSocket`, its frames in a bounded `Channel`. Both go through
okay2-platform's typed `Web` facades. `Client` is `Acceptance.check`
linked as a Node main.

`TestAcceptance` (`Live`) runs that program with `node` against a JVM
server, next to the JVM control over the JDK transports: 4 of 4 checks
on each side. okay2 has no Jetty, so the test fixture `AcceptanceServer`
serves both halves on one port. A WebSocket handshake runs the shared
`Acceptance.echo`, and other requests are piped to `Server`. The first
run failed because Node's fetch pool sent the handshake down a kept-alive
REST connection, so the pipe now closes each REST exchange.

Tests: TestAcceptance 2 (Live); okay2-http's default suites on JVM, JS
and Native unchanged, TestWs (Live) over the shared `WsWire`.
