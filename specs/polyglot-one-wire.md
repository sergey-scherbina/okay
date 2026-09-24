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

- **The Scala side.** The engine (`ForeignWorker`: `Foreign.program`,
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

## Direct style: `okay_call(request) -> answer` (operator, 2026-09-24)

Programs as data (`perform`, `then`) are what MULTI-SHOT needs: a
continuation that okay may resume twice has to be a value. Most code
wants less. It wants to call an effect in the middle of a computation
and have the answer, the way Python and R already do with
`okay.call("price_of", x)`. So Rust and Go get the same direct style,
`okay_call(name, args) -> answer`, on every transport:

- **Pipes and TCP.** The wire's existing callback dialogue: the host
  `start`s a function offering callbacks, the far side `ask`s, and the
  host `resume`s it with the answer (the one Python's shim and the TS
  worker speak).
- **FFM.** An UPCALL: the JVM hands the Rust library a function pointer
  (an FFM upcall stub), and `okay_call` is a C call back into the Scala
  handler, on the same thread, inside the Rust call. No second process
  and no line.
- **WebAssembly.** A host function the module imports (`okay.okay_call`),
  answered by Chicory in the same way.

A direct call is answered once, so a handler that resumes twice
(`Choice`) needs the program-as-data form. The docs say which to use
when, and the conformance suite covers both forms.

- [ ] Rust and Go: `okay_call` in the library, over pipes and TCP
      (`start`/`ask`/`resume`), then FFM (upcall) and wasm (host import).
- [ ] The conformance suite gains a direct-style case: a far-side
      function that calls `okay_call` twice and answers from both, under
      the caller's Reader.

## Stage 1 — wire-links (Scala links; Go over TCP)

- [x] `WireLink`: `hello(): String`, `roundTrip(line): String`, `close()`.
      `ForeignWorker.over(link)` is the engine over any link, and
      `speaking(command)` becomes `over(PipeLink(process))`, unchanged for
      callers.
- [x] `TcpLink(host, port)` and `ForeignWorker.connect(host, port)`: the
      same protocol over a socket. The server speaks first, as the pipe
      worker does.
- [x] Go: `okay.Worker.Handle(line)` is the protocol without I/O, and
      `Serve` (stdio) and `ServeTCP(addr)` both use it. Each TCP
      connection has its own continuations.
- [x] A conformance suite (Live), one body over (Go, pipes) and
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

- Stage 1 (wire-links, 2026-09-24).
  - `WireLink` has two links, pipes and TCP. `ForeignWorker.over(link)`
    is the engine, `speaking` is `over(pipes)`, and
    `connect(host, port)` is `over(tcp)`. The Python, Go and Haskell
    suites pass unchanged over the refactored engine.
  - Go: `Worker.Handle(line)` is the protocol with no I/O. `Serve`
    (stdio) and `ServeTCP` use it, and `ServeTCP` gives each connection
    its own Worker and prints `{"listening": "host:port"}` once bound.
    `okay.Main` picks TCP when `OKAY_LISTEN` is set, so one binary does
    both.
  - `WireConformance` is ONE test body (multi-shot, callbacks under the
    caller's Reader, a failure as a condition with the worker running
    on), and it passes over (Go, pipes) and (Go, TCP).
  - Mutant: a worker dropping continuations after one use fails
    multi-shot on BOTH links.

- Direct style for Go (go-direct, 2026-09-24).
  - `okay.Functions` holds direct-style functions `func(c *Ctx, args []any) any`.
    `c.Call(name, args...) (any, error)` and `okay.CallOp(c, op)` are
    `okay_call`.
  - The worker runs a started function on a goroutine and turns each
    Call into an `ask`, which a `resume` answers. `Handle` stays one line
    in, one line out, so pipes and TCP both carry it.
  - The conformance suite's direct case (`quote`: two `okay_call`s under
    the caller's Reader) passes over (Go, pipes) and (Go, TCP).
  - Mutant: a parked call filed under the wrong k fails the direct case.
    A first mutant that did not compile ("k declared and not used") was
    no evidence and is not counted.
