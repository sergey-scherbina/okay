# Nio — raw TCP on virtual threads

## Overview
`okay.http.Nio` is the byte-level TCP transport: two ends, chunks
between them, `link` for a line protocol on top. It was originally
written on `AsynchronousSocketChannel`, on the premise that the
completion-handler shape is `Async.Await`'s shape and therefore the
natural one. Chasing a flake refuted the premise's VALUE and found
something deeper (okay-http/BUGS.md: nio-serve-stall): under rapid
LISTENER churn — bind/listen/close cycles, the shape of test suites
and per-invocation benchmarks — macOS occasionally (~1.2/1000 rounds)
loses a freshly established connection: the kernel completes the
handshake into the backlog, never delivers it to accept (blocking or
asynchronous alike — both APIs measured, same rate), and closes it
with a clean FIN. With ONE stable listener the same shape ran 8000
consecutive connections clean. So the defect is below both channel
APIs and unreachable from transport code; what a transport CAN choose
is simplicity, and the blocking form is simpler, priced equal by the
cluster transport benchmark (24.4 vs 24.7 ms, docs/benchmarks.md),
and immune to userland completion-dispatch loss — so that is the form
this file now takes.

## Interface
Unchanged but for one type: `listen` hands back a
`ServerSocketChannel` instead of an `AsynchronousServerSocketChannel`.

- `Conn.send(Chunk[Byte] | String): Unit ! Async` — drains fully.
- `Conn.bytes: Source[Chunk[Byte]]` — chunks as they arrive, EOF ends.
- `Conn.close(): Unit ! Async`.
- `connect(host, port): Conn ! Async`.
- `listen(port)(serve): ServerSocketChannel ! Resource` — every
  accepted connection served on its own fiber; the accept loop itself
  is a fiber parked in `accept()`; closing the resource unparks and
  ends it.
- `port(server): Int`, `link(conn): mcp.Link` as before.

JVM-only file. Programs are expected to run on virtual threads
(`Async.spawn` / `Async.run` under `CanBlock`); a `runAsync` drive
would execute the blocking calls in place on the driving thread.

## Behavior
- [x] the existing TestNio suite passes unchanged (echo, 500 ordered
      lines, 300KB drained write, MCP over a socket, resource frees
      the port)
- [x] the churn gate: one stable listener, hundreds of consecutive
      connect/serve/close connections, every line delivered — the
      guarantee the code CAN make; listener churn is NOT gated
      because the loss there is the OS's (a ~45%-flaky gate teaches
      people to ignore red)
- [x] the full-length proofs ran at fix time and are recorded in
      okay-http/BUGS.md: stable listener 8000/8000 clean; listener
      churn loses connections at the same rate on blocking and
      asynchronous channels alike

## Out of scope
- HTTP on raw NIO (specs/http.md's reasoning stands: that is Netty's
  job).
- A retry layer over connect/accept for the listener-churn loss —
  the OS closes those connections cleanly, indistinguishable from a
  server that chose to; retrying is an application policy, not a
  transport one.

## Decisions
- **Blocking channels on virtual threads, not the async layer** — the
  OS-level loss hits both equally, so the choice is on other grounds:
  the blocking form is simpler, measured equal, and cannot lose a
  userland completion dispatch on top of what the OS loses.
- **API preserved** — callers (tests, mcp link) compile unchanged; the
  listener type in `listen`'s signature is the one visible change.
