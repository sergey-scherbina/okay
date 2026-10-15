# okay-foreign-ws

The okay wire over a WebSocket that a server accepts (foreign-ws,
specs/foreign-ws.md).

| | |
|---|---|
| `okay.foreign.ForeignWs.route(path, failed)(session)` | a WebSocket path whose far sides are each handed to `session` as a `ForeignWorker` on a thread of its own, and closed after it. `route.sessions` and `route.pushes` are `Jetty.serve`'s `ws` and `push` |

**Depends on:** okay-py, okay-jetty.

## Why a module of its own

okay-py holds the engine (`ForeignWorker`, the wire, `Foreign.program`) and
does not depend on Jetty. okay-jetty serves WebSockets and does not depend on
okay-py. The link between them depends on both, so it gets a module that
neither carries, the okay-mcp-http pattern.

## The bridge

A Jetty session is a `Stage[Frame, Frame, Unit]`. It awaits the client's
frames and the server's `push` in arrival order, and it tells frames to the
client. A `WireLink` blocks on the engine's thread. The engine's lines go out
as `push` frames, marked with `\u0001`, which no JSON message starts with,
and the session tells each to the client. The client's frames go into the
queue the engine takes from. No Jetty thread waits.

The far side speaks first: its hello is its first text frame. After that,
one JSON text frame per message. A far side that closes mid-run ends the
engine's wait, and the in-flight call throws, as on every link.
`TestForeignWs` covers this with a JDK WebSocket client playing the
transcript (multi-shot `pairs`). `TestOkayTsWs` runs okay-ts over a real
WebSocket against `WireConformance`; it is LIVE, with `OKAY_TS` set to an
okay-ts checkout.
