## foreign-ws — the okay wire over a WebSocket a server accepts (specs/foreign-ws.md)

A new module, okay-foreign-ws (okay-py + okay-jetty). `ForeignWs.route(path)(session)`
hands each far side that connects over a WebSocket to the session as a
`ForeignWorker`, on a thread of its own, and closes it after the session.
It is served by `Jetty.serve(port)(routes)(route.sessions, route.pushes)`. The
bridge uses no Jetty thread to wait. The engine's lines are the session's
`push`, marked, and the client's frames go into the queue the engine takes
from. `TestForeignWs` runs a JDK WebSocket client through the transcript
(multi-shot `pairs`, the socket closed after the session, a far side gone
mid-run). `TestOkayTsWs` (LIVE, `OKAY_TS`) runs okay-ts over a real
WebSocket against `WireConformance`. The same lane removes an unused import
in foreign-server's `TestOkayTsServed`.
