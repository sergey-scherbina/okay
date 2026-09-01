# Nio — raw TCP on virtual threads

## Overview
`okay.http.Nio` is the byte-level TCP transport: two ends, chunks
between them, `link` for a line protocol on top. It was originally
written on `AsynchronousSocketChannel`, on the premise that the
completion-handler shape is `Async.Await`'s shape and therefore the
natural one. The premise was refuted operationally on 2026-09-01
(okay-http/BUGS.md: nio-serve-stall): under rapid channel churn on
macOS the JDK's asynchronous-channel layer LOSES completion events —
measured at ~1–1.5 per 1000 listen/serve rounds, with the lost event
pinned to the accept dispatch by stage counters (accepted=false,
nothing sent, nothing read), reproduced identically on the default
channel group and on a dedicated one. A lost accept in the old code
also killed the re-arm, silencing the listener forever.

So Nio now stands on blocking channels parked on Loom virtual
threads — the trade the rest of the repository already takes, and one
this repository has priced: the cluster transport benchmark measured
blocking-on-Loom against NIO completion handlers as a wash (24.4 vs
24.7 ms, docs/benchmarks.md). Parking is free; lost wakeups are not.

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
- [ ] the existing TestNio suite passes unchanged (echo, 500 ordered
      lines, 300KB drained write, MCP over a socket, resource frees
      the port)
- [ ] the churn gate: hundreds of consecutive listen/serve/close
      rounds complete with every line delivered — the shape that lost
      accepts at ~1.5/1000 before the rewrite
- [ ] the full-length proof (thousands of rounds, the original repro
      harness) ran clean at fix time and its result is recorded in
      okay-http/BUGS.md

## Out of scope
- HTTP on raw NIO (specs/http.md's reasoning stands: that is Netty's
  job).
- A watchdog/retry layer over asynchronous channels — rejected in
  favor of removing the failure mode structurally.

## Decisions
- **Blocking channels on virtual threads, not a patched async layer**
  — the event loss is below the JDK API surface, cannot be detected
  from above (a lost accept is indistinguishable from a quiet
  listener), and the house's own measurement says the async layer buys
  nothing here.
- **API preserved** — callers (tests, mcp link) compile unchanged; the
  listener type in `listen`'s signature is the one visible change.
