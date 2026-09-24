# polyglot-one-wire — Scala, Rust and Go as one language

## Overview

The operator's aim (2026-09-24): okay's effects and code in Rust and Go
should work "as if they were one language", in-process through FFM or
over a wire, remote included.

okay already has the ground for it: PROGRAMS AS DATA on a line protocol
(`perform`, `continue`, `done`; one JSON line each way;
specs/remote-foreign.md). Python, R, TypeScript, Haskell and Go already
speak it. A far-side program performs a named operation. The Scala side
runs that operation as an okay program under the caller's handlers
(Reader, State, Choice, Async, …) and answers. The program continues, as
many times as a handler resumes it, and `Durable` journals the whole
walk.

What is missing is that the protocol is tied to ONE transport, a child
process's pipes. This spec cuts it loose, in both directions:

- **The Scala side.** The engine (`PySubprocess`: `Py.program`,
  callbacks, multi-shot, `Durable`) runs over a `WireLink`: a handshake
  line, then one request line and one answer line. The engine does not
  change; the link does.
- **The Rust and Go sides.** One library per language (programs, typed
  operations, a worker holding continuations) serves the same programs
  over any link.

| transport | where | Rust | Go |
|---|---|---|---|
| pipes | a child process | `okay::serve_stdio` | `okay.Serve` |
| TCP | another process, another machine | `okay::serve_tcp` | `okay.ServeTCP` |
| FFM | this process, native code | `okay_exchange` in a cdylib | — (a Go runtime does not belong in the JVM: docs/rust.md#go) |
| WebAssembly | this process, sandboxed (Chicory) | `okay_exchange` in wasm32-wasip1 | `okay_exchange` in `GOOS=wasip1` |

The claim, tested: ONE Scala test body, a conformance suite, passes over
every row and column. It covers multi-shot across the link, a typed
operation answered by a Scala callback under the caller's Reader, a
failure as a condition by name, and the worker living on.

## Stage 1 — wire-links (Scala links; Go over TCP)

- [ ] `WireLink`: `hello(): String`, `roundTrip(line): String`, `close()`.
      `PySubprocess.over(link)` is the engine over any link, and
      `speaking(command)` becomes `over(PipeLink(process))`, unchanged for
      callers.
- [ ] `TcpLink(host, port)` and `PySubprocess.connect(host, port)`: the
      same protocol over a socket. The server speaks first, as the pipe
      worker does.
- [ ] Go: `okay.Worker.Handle(line)` is the protocol without I/O, and
      `Serve` (stdio) and `ServeTCP(addr)` both use it. Each TCP
      connection has its own continuations.
- [ ] A conformance suite (Live), one body over (Go, pipes) and
      (Go, TCP).

## Stage 2 — rust-worker (the Rust library)

- [ ] A Cargo crate `okay` (`okay-rust/lib/okay`, `serde_json` only,
      offline) holding:
      - `Value` and the wire's encoding (as `Okay.hs` and `okay.go`);
      - `Prog` (`done`, `perform`, `and_then`), whose continuations are
        `Rc<dyn Fn>`, callable again, so multi-shot works;
      - typed `Op<A>`, `Program<A>` and `send`;
      - `Worker::handle(line)`, `serve_stdio`, `serve_tcp`.
- [ ] `Rs.ops(module, callbacks)` writes typed operation constructors
      from the Scala callbacks' Schemas, as `Go.ops`/`Hs.ops`/`Ts.ops`
      do.
- [ ] The conformance suite over (Rust, pipes) and (Rust, TCP).

## Stage 3 — in-process (FFM and WebAssembly)

- [ ] The C ABI of a worker in-process: `okay_exchange(req, req_len, out_len) -> resp`
      and `okay_free(resp, len)`. An empty request answers the handshake.
      A Rust cdylib gets it from one macro, `okay::export_worker!(programs)`.
      A Go wasm module gets it from `okay.Exchange` and three
      `//go:wasmexport` lines.
- [ ] `FfmLink(lib)` (okay-rust, JVM) and `WasmLink(lib)` (Chicory):
      `roundTrip` is one call.
- [ ] The conformance suite over (Rust, FFM), (Rust, wasm) and
      (Go, wasm).

## Stage 4 — docs

- [ ] One page, "Rust and Go as okay", with the table above, one program
      written in each language, and the same Scala calling it over every
      link.

## Decisions

- **One protocol, many links**, rather than a binding per transport. The
  protocol is what the engine, `Durable` and the tests already trust.
  A link is the only new thing per transport, and each link is small.
- **In-process is a function call carrying the same JSON line.** It is
  not a native struct per operation. FFM's cost is a copy of a short
  line, and in exchange the Rust program, the Scala engine and every test
  are the SAME code on every transport.
- **Remote is plain TCP, unauthenticated, and says so.** Run it inside a
  trusted network or behind TLS or SSH. An authenticated transport
  (WebSocket over okay-http, with a token) is a later stage and is not
  claimed here.

## Results
