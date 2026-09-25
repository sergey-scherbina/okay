## okay2-http - okay-http's transport half for okay2

The last of the modules the operator asked for (json, xml, sql, http),
part A (spec stage 44, docs §35). `okay2-http` runs on JVM, Scala.js and
Scala Native.

The shared part:
- `Method`, `Body`, `Request`, `Response` (status is data, headers
  case-insensitive), `Http`;
- `one`/`bytes`/`text`/`discard`/`lines`/`sse`/`json`;
- the line framer, and `Frame`/`Socket`/`Sockets`;
- `Ws.over`, which runs a session written as a Stage, and `Ws.texts`;
- `Sse.events` in okay2-stream.

On the JVM: the JDK `Server` as a Resource, `Nio` for raw channels, and
`Transports.http`/`sockets` for the JDK client and WebSocket.

Tests:
- TestFraming runs everywhere: 12 results per platform.
- TestHttp (11), TestNio (5) and TestWs (8) run against real sockets,
  with a test-scope RFC 6455 echo server.

The port-binding suites are `Live`-tagged. okay2 gained the root build's
default `--exclude-tags=Live`, `integrationTest`, and `liveOnly` (the
switch alone, which `scripts/gate.sh` can pass).

Typed routes (`Route`/`Router`) are part B, filed as `okay2-http-routes`.
