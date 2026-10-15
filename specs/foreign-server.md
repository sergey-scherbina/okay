# foreign-server — a host that ACCEPTS far sides

## Overview

The okay wire (specs/remote-foreign.md, specs/polyglot-one-wire.md) is
driven by the host: Scala sends every request, and the far side answers and
keeps the continuations. Every transport so far has the host doing the
dialling. It opens a child's pipes, connects to a worker's TCP port (Rust
`serve_tcp`, Go `ServeTCP`, the Python gateway), or calls into FFM or wasm.
Nothing in okay ACCEPTS a far side, and "a later stage" is all
polyglot-one-wire.md says about it.

That leaves out the case the operator asked for (2026-10-15, okay-ts
interop): a program written ELSEWHERE whose sensitive effects are handled
HERE. Examples are a TypeScript program in a Node process or on another
machine, or okay-ts (github.com/sergey-scherbina/okay-ts), whose programs are
data with a many-shot `k` and so are a far side as they are. The program
connects to an okay server. The server runs it under Scala callbacks:
answering its operations, auditing them and refusing them, with the policy
and the trail on the server's side of the wire.

Only the direction of the connection changes. The far side still speaks
first (its hello), and the host still drives (`program`, `continue`,
`forget`). So everything the engine already does runs unchanged over an
accepted connection: callbacks, multi-shot, direct style, held values, mux,
streams and `Durable`.

## Interface

```scala
object WireLink:
  /** a far side that CONNECTED to this host: the socket a server accepted */
  def accepted(s: java.net.Socket, helloMillis: Int = 10000): WireLink

final class ForeignServer:
  def port: Int
  /** the next far side to connect, as an engine (its hello read) */
  def accept(): ForeignWorker
  /** every far side to connect, each on a thread of its own, its engine given
   * to `session` and closed after it */
  def serve(session: ForeignWorker => Unit): Unit
  def close(): Unit

object ForeignServer:
  def listen(port: Int = 0, host: String = "127.0.0.1"): ForeignServer
```

The host still chooses each program's `fn`. A connected far side registers
its programs under names, as a worker's modules do, and the server's session
names the one it runs.

## Behavior

- [x] `WireLink.accepted`: the far side's hello read within `helloMillis`,
      else refused by name and the socket closed; a network link (`network`
      true), duplex.
- [x] `ForeignServer.listen(0)` binds an ephemeral port; `accept()` answers
      the next connection's engine; `close()` stops listening.
- [x] `serve(session)`: each connection on a thread of its own, its engine
      closed when the session returns or throws.
- [x] `WireConformance` — the suite every far side passes — passes over an
      ACCEPTED connection, the far side the TypeScript worker dialling the
      server (`TestTsServed`): the same 6 passed, 7 skipped by capability, as
      over pipes (`TestTsPipes`).
- [x] okay-ts, a far side that connects BY ITSELF, passes it too
      (`TestOkayTsServed`, LIVE, `OKAY_TS` its checkout): multi-shot, Scala
      callbacks under the caller's Reader, direct style, a failure by name.
- [x] A TypeScript program's sensitive effect handled and AUDITED here
      (`TestOkayTsAudited`): each transfer a callback whose program tells the
      trail, normalizes the account, and refuses over the limit — a condition
      thrown in TypeScript where the transfer is, caught there.
- [x] A far side that connects and says nothing is refused by name after
      `helloMillis`, and the server goes on accepting.

## Results

- 2026-10-15: `TestForeignServer` 2/2; `TestTsServed` and `TestTsPipes` 6 passed
  and 7 skipped each, the same tests (mux, streams and panics are capabilities
  the TS worker does not claim); `TestOkayTsServed` 4 passed, with tables and
  held values switched off; `TestOkayTsAudited` 1/1.
- A refusal reaches the far side as a condition only in the DIRECT dialogue
  (`Foreign.fn(...).calling`). There each callback's `Left` is a `continue`
  with a condition. Under `Foreign.program`, a callback's `Left` ends the run
  on the host. The audit test uses the dialogue for that reason.

## Decisions

- **A link, not a new engine.** The engine and the protocol are unchanged.
  Only the socket's origin is new, which is the polyglot-one-wire rule
  ("a link is the only new thing per transport").
- **Plain TCP, like `WireLink.tcp`, and it says so.** TLS and the HMAC
  challenge are a later step. `WireAuth` is the host's given whichever way
  the socket was opened, so it applies unchanged. WebSocket for browsers
  (over okay-jetty) is a module of its own after this one.
- **The test's far side dials through a relay**: the TS worker as a child
  process, its pipes copied to a socket connected to the server. The worker
  speaks stdio only, and a relay is the same pair of streams as the
  gateway's, the other way round. okay-ts, a far side that connects by
  itself, is tested in its own repository.
