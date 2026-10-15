# foreign-ws — the okay wire over a WebSocket a server accepts

## Overview

foreign-server (specs/foreign-server.md) gave the okay wire a host that
ACCEPTS far sides, but only on plain TCP, and a browser cannot open a TCP
socket. A program in a page (okay-ts, github.com/sergey-scherbina/okay-ts)
reaches its server through a WebSocket. The same holds for a program behind
a proxy that passes HTTP and nothing else. polyglot-one-wire.md names this
"WebSocket over okay-http … a later stage". This is that stage.

As with TCP, nothing changes but the link. The far side connects and says
hello in its first text frame. The host drives with `program`, `continue`
and `forget`, one JSON text frame per message, and the engine is
`ForeignWorker` as before.

## Interface

```scala
object ForeignWs:
  /** a WebSocket route's session and its push: each far side that connects is
   * given to `session` as an engine, on a thread of its own, and closed after it */
  def route(session: ForeignWorker => Unit): ForeignWs.Route   // ws, push for Jetty.serve

  /** a Jetty server whose WebSocket path serves far sides */
  def serve(port: Int, path: String)(session: ForeignWorker => Unit): org.eclipse.jetty.server.Server
```

## The bridge

A Jetty session is a `Stage[Frame, Frame, Unit]`. It awaits the client's
frames and the server's `push`, in arrival order, and it tells frames to
the client. `WireLink` is blocking: a request out, its answer in, on the
engine's thread. The two meet like this:

- The ENGINE's lines go into a `Channel` that is the session's `push`, each
  marked with a prefix no JSON message can start with. Marked frames are
  sent to the client.
- The CLIENT's text frames go into a queue that the engine's `hello` and
  `roundTrip` take from.
- No Jetty thread blocks. The session only awaits. The engine thread is the
  only one that waits, the same as for every link.

## Behavior

- [ ] A far side that connects over a WebSocket is driven as over TCP:
      `program` to `done`, a callback answered under the caller's Reader,
      a rest continued twice (multi-shot). The far side in this test is
      a JDK WebSocket client playing the transcript.
- [ ] The engine is closed when its session ends. The client sees the
      socket close.
- [ ] A client that closes mid-run ends the engine's wait. The far side is
      gone, the in-flight call throws, as on every link.
- [ ] okay-ts over a WebSocket passes `WireConformance`
      (`TestOkayTsWs`, LIVE, `OKAY_TS` its checkout): multi-shot, callbacks
      under the caller's Reader, direct style, a failure by name.

## Decisions

- **A module of its own, okay-foreign-ws.** It is okay-py plus okay-jetty.
  okay-py does not depend on Jetty and okay-jetty not on okay-py, the
  okay-mcp-http pattern.
- **Text frames, JSON lines.** No frames format (CBOR, deflate) over the
  WebSocket in this stage: the far side's hello offers JSON only, and a
  configure is refused by name.
- **Unauthenticated, and it says so, like TCP.** A token or a cookie at the
  upgrade (`Request` headers) is the server's route to check. The wire's
  HMAC challenge is a later step.
