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
- **FFM and WebAssembly.** The SAME dialogue: each `ask` and `resume` is
  one `okay_exchange` call. An upcall (a C function pointer into the
  JVM, or a Wasm host import) was the first plan and is REFUTED. A
  callback is an okay PROGRAM that must run under the caller's handlers
  (Reader, State, Async, ...). An upcall arriving in the middle of an FFM
  call would run it inside the `ForeignEval` handler alone, with every
  other handler missing: silently different semantics. As a dialogue step
  it runs where the program runs, exactly as over a wire.

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

- [x] A Cargo crate `okay` (`okay-rust/lib/okay`, `serde_json` only,
      offline) holding:
      - `Value` and the wire's encoding (as `Okay.hs` and `okay.go`);
      - `Prog` (`done`, `perform`, `and_then`), whose continuations are
        `Rc<dyn Fn>`, callable again, so multi-shot works;
      - typed `Op<A>`, `Program<A>` and `send`;
      - `Worker::handle(line)`, `serve_stdio`, `serve_tcp`.
- [x] `Rs.ops(module, callbacks)` writes typed operation constructors
      from the Scala callbacks' Schemas, as `Go.ops`/`Hs.ops`/`Ts.ops`
      do.
- [x] The conformance suite over (Rust, pipes) and (Rust, TCP).

## Stage 3 — in-process (FFM and WebAssembly)

- [x] The C ABI of a worker in-process: `okay_exchange(req, req_len, out_len) -> resp`
      and `okay_free(resp, len)`. An empty request answers the handshake.
      A Rust cdylib gets it from one macro, `okay::export_worker!(programs)`.
      A Go wasm module gets it from `okay.Exchange` and three
      `//go:wasmexport` lines.
- [x] `FfmLink(lib)` (okay-rust, JVM) and `WasmLink(lib)` (Chicory):
      `roundTrip` is one call.
- [x] The conformance suite over (Rust, FFM), (Rust, wasm) and
      (Go, wasm).

## Stage 4 — docs

- [x] One page, "Rust and Go as okay", with the table above, one program
      written in each language, and the same Scala calling it over every
      link.

## Stage 5 — the wire's encoding and protection, chosen by givens (operator, 2026-09-24)

The operator asked for the link's FORMAT and PROTECTION to be picked at
compile time, by importing a given, each backed by its own external
mechanism:

### The protocol of stage 5a (format and compression)

1. **The handshake.** The worker's first line stays a JSON line, and it
   may ANNOUNCE what it speaks:
   `{"shim":6, "python":"go", "speaks":{"format":["json","cbor"],"compress":["deflate"]}}`.
   A hello with no `speaks` speaks JSON lines only.
2. **Configuration.** If the host's givens are the defaults (JSON, no
   compression), nothing changes, and the wire is lines as before, so old
   workers keep working. Otherwise the host first checks that the worker
   announced both choices, and REFUSES by name if it did not. It then sends
   one JSON line, `{"op":"configure","format":"cbor","compress":"deflate"}`.
   The worker answers with a JSON line (`{"ok":{...}}`, or a condition),
   and from the next message both sides use the configuration.
3. **Frames.** A configured stream (pipes, TCP) carries FRAMES: a 4-byte
   big-endian length, then that many bytes. An in-process link is already
   one call per message, so it carries the bytes alone.
4. **A message's bytes** are the same protocol tree as before, encoded by
   the format (JSON text, or CBOR), then compressed (deflate: zlib's raw
   DEFLATE, RFC 1951). The tree is unchanged, including the value
   escapes (`{"t":"int"}` and so on), so every language's value rules
   stay as they are.
5. **CBOR subset** (RFC 8949): unsigned and negative integers, float16,
   float32 and float64, text strings, arrays and maps of definite length,
   `true`, `false`, `null`, `undefined` (read as null). A decoder refuses
   anything else by name.

- [x] An encoding: JSON (today) or CBOR (okay-codec's), as a
      `given WireFormat`. Framing moves from lines to length-prefixed
      frames, so a binary format fits. JSON stays line-compatible.
- [x] Compression (`given WireCompression`: none, deflate/gzip from the
      JDK, or zstd where a library is present).
- [x] DEFLATE is the DEFAULT, as a preference (operator, 2026-09-24:
      "не обязательным но желательным (по умолчанию) - его при желании
      можно только отключить"). With no import, a stream link (pipes,
      TCP) whose far side announces deflate compresses every message; a
      far side that does not (Haskell, R, an old worker) keeps the plain
      wire with no refusal. `import WireCompression.Off.given` turns it
      off. `import WireCompression.Deflate.given` stays the STRICT choice:
      a far side without it is refused by name. `ForeignWorker.wire` says
      what was negotiated.
- [x] In-process links (FFM, wasm) do not take the preference: a message
      there is a memory copy, and compressing it is pure cost. The strict
      `Deflate` given still applies to them.
- [ ] Encryption (`given WireSecurity`): TLS for TCP (JSSE on the
      JVM, rustls or native-tls in Rust, crypto/tls in Go), with keys
      from their own stores.
- [ ] Authorization (`given WireAuth`): a bearer token or an HMAC
      challenge at the handshake, from a secret the given names (an
      environment variable, a file, okay-security).
- [ ] EVERY language on the wire, not only Rust and Go (operator,
      2026-09-24): Python's shim, R, the TypeScript worker, Haskell, Go
      and Rust. Each far side's library implements the same layers with
      its own platform's mechanisms, and none is special-cased.
- [x] The far side cannot import a Scala given. It is configured by its
      build and environment, it ANNOUNCES what it speaks in the
      handshake, and a mismatch with the Scala side's givens is refused by
      name, never silently downgraded.
- [ ] The conformance suite runs under each combination each far side
      supports, and a table in the docs says which languages support
      which layer.

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

- **A preference is not a downgrade.** Stage 5a's rule "never quietly
  downgraded" is about a choice somebody made: an explicit given is still
  refused by name when the far side lacks it. The default states a
  PREFERENCE, "deflate where both sides have it", and a fallback is what
  a preference means. `ForeignWorker.wire` makes the outcome visible
  rather than silent.

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

- Stage 2 (rust-worker, 2026-09-24).
  - The Rust crate `okay` (`/okay/rust/okay` in okay-py's jar, depending on
    serde_json only) holds `Value`/`Wire`, `Prog` with `Rc<dyn Fn>`
    continuations, typed `Op`/`Program`/`send`, and the direct style:
    `Functions`, `Ctx::call`/`call_op`, run on a thread and parked by
    channels. It also has `Worker::handle`, `serve_stdio`, `serve_tcp`
    and `main`.
  - `RustWorker.build` compiles offline, and `Rs.ops` writes the typed
    constructors. The crate lives in `okay-py/src/main/resources/okay/rust`,
    not in the spec's `okay-rust/lib`: it ships in the same jar as the Go
    and Haskell libraries, next to the engine that drives them.
  - `WireConformance` passes over (Rust, pipes) and (Rust, TCP), including
    the direct case, so Go and Rust answer ONE Scala test body.
  - Mutant: continuations removed after one use fails multi-shot.

- Stage 3 (in-process-worker, 2026-09-24).
  - Rust: `okay::export_worker!(make)` exports `okay_exchange`,
    `okay_alloc` and `okay_free`, with ONE Worker behind a Mutex. The
    `unsafe impl Send` is argued in its comment: every `Rc` is touched
    under the lock only.
  - Go: `okay.Export` in `init()` (a reactor's main never runs), and
    `okay_wasm.go` (`//go:build wasip1`) holds the exports.
  - `InProcessLinks.ffm` and `.wasm` are WireLinks of one call each.
    `RustWorker.buildLibrary` and `GoWorker.buildWasm` build them
    offline.
  - The conformance suite passes over:
    - (Rust, FFM): the full suite;
    - (Go, wasm): the full suite, direct style included, with a
      goroutine parked between two exported calls;
    - (Rust, wasm): multi-shot and callbacks. wasip1 has no threads, so
      no direct style, and a panic traps. That trap is reported with the
      panic's message, and the suite's `survivesPanics` names it.
  - Findings:
    - The foreign engine read wire lines with the TOTAL `Json.parse`,
      which repairs damaged text. A reply cut one byte short (the FFM
      mutant) passed every test. `ForeignWorker.whole` now checks bracket
      balance and parses strictly, and a line cut at ANY byte is refused
      (TestWireWhole). This covers every transport, not just FFM.
    - `WasmLib` freed a zero-length buffer as 0 bytes after allocating
      1, a size mismatch in the module's allocator. It now frees what it
      allocated.
    - A resource directory `okay/rust/okay` on the classpath read to
      scalac as a package `okay.rust.okay`, and it shadowed `okay` inside
      `package okay.rust`. The crate ships as `okay/rust-crate`.
    - REFUTED: `okay_call` as an FFM upcall. A callback must run under
      the caller's handlers, so in-process uses the same dialogue.

- Stage 4 (one-wire-docs, 2026-09-24): docs/one-language.md holds the
  table, the same `quote` in Rust and Go, the same Scala over the four
  links, the conformance suite's claims, and why in-process uses the
  dialogue. It is pinned by the snippet check.

- Stage 5a (wire-format-givens, 2026-09-24).
  - `WireFormat` (json, `WireFormat.Cbor.given`) and `WireCompression`
    (none, `WireCompression.Deflate.given`) are `using` parameters of
    every engine constructor. The defaults live in the companions, so a
    program without the imports keeps its JSON lines byte for byte.
  - Five far sides speak CBOR, each with its own subset codec and no new
    package: Python (struct, zlib), TypeScript (Buffer, node:zlib), Go
    (compress/flate), Rust (flate2 with the pure-Rust backend) and
    Haskell (bytestring, text). Haskell has no DEFLATE, because GHC ships
    no zlib binding. Its hello says so, and a Deflate host is refused by
    name (`TestHsPipesCbor`).
  - `WireConformance` runs unchanged under CBOR on every link built so
    far: Go (pipes, TCP, wasm), Rust (pipes, TCP, FFM, wasm), Python,
    TypeScript and Haskell (pipes).
  - R is NOT done. okay-r has its own engine (`RSubprocess`, a character
    reader with its own deadline), not `ForeignWorker`, so its framing is
    a lane of its own: backlog `wire-givens-r`. It is testable here
    through the local `r-base` image.
  - Mutant: a Haskell worker that confirms `configure` and does not
    switch was caught, but only by the gate's STALL watchdog after 480 s.
    The engine has no read deadline, so a far side that goes silent
    leaves it waiting. The fault is in the far side, but the host
    should say so: backlog `wire-read-deadline`.

- DEFLATE by default (wire-deflate-default, 2026-09-24).
  - `given WireCompression.preferred` is the default: raw DEFLATE with a
    `fallback` to `Off.off`, taken on an in-process link
    (`WireLink.inProcess`) and where the hello does not announce
    deflate. `Off.given` disables it; `Deflate.given` is strict.
    `ForeignWorker.wire` answers "format/compression".
  - Every default live suite now runs compressed where the far side
    speaks it: Python, TypeScript, Go and Rust over pipes and TCP. It
    stays plain over Haskell pipes and in-process (pinned in
    `TestPyPipes`, `TestPyPipesOff`, `TestHsPipes` and `TestRustFfm`),
    and the fake-link cases are in the default gate (`TestWireGivens`,
    6 new).
  - Mutant: dropping the in-process branch of the fallback failed
    exactly the in-process case.

- One name, `okay_call` (okay-call-name, 2026-09-24). The operator asked
  for `okay_call(request) -> answer`, and the direct style had five
  spellings (`ctx.call`/`ctx.call_op`, `c.Call`/`okay.CallOp`,
  `okay.call`, `call`, `okay_call`).
  - Rust: `okay::okay_call(request) -> Result<A, OkayError>`, a free
    function. The call in progress is a thread-local of the thread the
    worker starts the function on, so the function takes only its
    arguments (`function(|args| ...)`), and `impl From<OkayError> for
    String` lets it write `okay_call(op)?`. `Ctx` is private now.
  - Go: `okay.Call(c, request)`, typed; `okay.Named(name, args...)` is
    the untyped request. Go cannot say `okay_call`: an exported name
    begins with a capital, and a goroutine has no local storage in which
    to hide `c`. `c.Call` and `okay.CallOp` are gone.
  - Python `okay.okay_call` and TypeScript `okay_call` are the name, and
    `okay.call`/`call` stay as aliases. R already had `okay_call`.
    Haskell serves programs only (no direct style).

