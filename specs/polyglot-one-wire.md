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

- [x] Rust and Go: `okay_call` in the library, over pipes and TCP
      (`start`/`ask`/`resume`), then FFM and wasm. Done as the SAME
      dialogue, one `okay_exchange` per step, not as an upcall or a host
      import: see "Why a dialogue" in docs/one-language.md (go-direct,
      rust-worker, in-process-worker; named `okay_call` by okay-call-name).
      Rust on wasm32-wasip1 has no threads and so no direct style there.
- [x] The conformance suite gains a direct-style case: a far-side
      function that calls `okay_call` twice and answers from both, under
      the caller's Reader (`WireConformance`'s DIRECT STYLE test, `quote`,
      in every language that has direct style).

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
- [x] Neither do PIPES (wire-compression-measured): the preference is
      taken on a NETWORK link only (`WireLink.network`, true for TCP).
      Measured, a pipe's bandwidth never pays back DEFLATE's CPU, and a
      short message grows. An explicit `Deflate`/`Zlib` given still
      compresses a pipe. This narrows the operator's default above on
      the numbers in Results.
- [x] The JDK codec keeps its Deflaters and Inflaters (a pool of four per
      given, reset between messages); one that refused a message is
      ended, not kept. An empty message inflates.
- [x] R on the same givens (wire-givens-r). The codecs move to okay-codec
      (`okay.codec.WireFormat`, `WireCompression`, `WireCbor`, and the
      framing and negotiation both engines share), so okay-r's
      `RSubprocess` takes the same `using` parameters as `ForeignWorker`;
      `okay.py` keeps its names by `export`.
- [x] A third compression, `zlib` (RFC 1950: DEFLATE with a 2-byte header
      and an adler32 check), because R can do it natively and CHECKED
      (`memCompress`/`memDecompress`, libdeflate) and cannot do raw
      DEFLATE safely: `gzcon` over a hand-made gzip header inflates it but
      prints a CRC error per message and accepts a cut stream, and
      `memDecompress` on a hand-wrapped member was killed by the OOM
      killer (exit 137, measured in r-base 4.4.1). The default preference
      becomes an ORDER: deflate, then zlib, then none. `Zlib.given` is the
      strict choice, like `Deflate.given`.
- [x] R's shim announces `format: [json, cbor]`, `compress: [zlib]`, and
      after a configure speaks frames on a binary stdin (`readBin` after
      `readLines` on one `file("stdin", "rb")`) and `/dev/stdout`. Its
      CBOR encodes the tree jsonlite would print (the same unboxing,
      the same NA rules), so JSON and CBOR carry the same values.
- [x] The R suites' answers are the same under every combination R
      speaks: calls, frames with NA, callbacks (`okay_call`), programs
      (multi-shot), a timeout's respawn (which re-negotiates).
- [x] Encryption (`given WireSecurity`, wire-tls): TLS on a TCP link.
      `WireSecurity.tls(trust)`, where the trust names its own source:
      `Trust.pem(path)` (a CA's or a self-signed server's certificate),
      `Trust.pemFromEnv(name)` (the path in a variable), `Trust.system`
      (the JDK's store). The server's name is VERIFIED against its
      certificate (HTTPS rules: a DNS name or an IP in its SAN). The
      default is plain, as before.
      - Go: `ServeTCP` with `OKAY_TLS_CERT` and `OKAY_TLS_KEY` (PEM files)
        serves `crypto/tls`, and its listening line says `"tls": true`.
      - Rust: the crate's `tls` feature (rustls on ring, offline); the
        same two variables. A worker built without the feature and asked
        for TLS refuses to start, by name.
        `RustWorker.build(dir, features = Seq("tls"))`.
      - Mismatches are refused by name. A TLS host meeting a plain server
        gets "did not complete a TLS handshake". A plain host meeting a
        TLS server hears no hello (a TLS server waits for the client to
        speak first), so the TCP link's hello read has a limit (the
        `WireDeadline` if one is given, else 10 s) and the refusal says
        the server may speak TLS.
      - It composes with `WireAuth`: TLS proves the server and hides the
        traffic, and the HMAC proves the client. A client certificate
        (mTLS) is therefore not in this lane.
      - Pipes and in-process links take no TLS, for the reason they take
        no auth.
- [x] Authorization (`given WireAuth`): a MUTUAL HMAC-SHA256 challenge at
      the handshake (wire-auth). The secret comes from the source the
      given names: `WireAuth.fromEnv(name)`, `WireAuth.fromFile(path)`,
      `WireAuth.secret(bytes)`; a Go or Rust server reads
      `OKAY_WIRE_SECRET` or `OKAY_WIRE_SECRET_FILE`. No given: no
      authentication, as before.
      1. A server with a secret announces it in its hello:
         `"auth":{"scheme":"hmac-sha256","nonce":Ns}` (16 random bytes, hex).
      2. The host answers with a JSON line `{"op":"auth","nonce":Nc,
         "mac":HMAC(secret, "okay-wire client|"+Ns+"|"+Nc)}`.
      3. The server checks it in constant time. It answers
         `{"ok":{"mac":HMAC(secret, "okay-wire server|"+Ns+"|"+Nc)}}` and
         the host checks that in turn, so each side has proved it holds
         the secret without sending it. A wrong mac is refused, and the
         server closes the connection.
      4. Until it has passed, a server with a secret answers every other
         request with a refusal. The `configure` of stage 5a follows the
         auth.
      Mismatches are refused by name on the host: a server that demands
      authentication meets a host without a given, or a host whose given
      demands it meets a server that announced none.
      What this does NOT give: confidentiality, or binding to the
      connection. A relay in the middle can pass the handshake through
      and read everything after it. That is what TLS
      (`given WireSecurity`) is for, and the two compose.
- [x] Where it applies: the TCP servers (Go `ServeTCP`, Rust `serve_tcp`).
      Pipe workers (Python, TypeScript, Haskell, R, and Go or Rust run as
      a child process) are processes the host itself started, with
      nothing between them. In-process links share the address space.
      Neither has anyone to authenticate, and saying so is the honest
      table.

- [x] EVERY language on the wire, not only Rust and Go (operator,
      2026-09-24): Python's shim, R, the TypeScript worker, Haskell, Go
      and Rust. Each far side's library implements the same layers with
      its own platform's mechanisms, and none is special-cased.
- [x] The far side cannot import a Scala given. It is configured by its
      build and environment, it ANNOUNCES what it speaks in the
      handshake, and a mismatch with the Scala side's givens is refused by
      name, never silently downgraded.
- [x] The conformance suite runs under each combination each far side
      supports, and a table in the docs says which languages support
      which layer.
      (wire-support-table, 2026-09-24: docs/one-language.md, "Which
      language supports which layer", with the suites behind every row
      and the two generic cells named as generic)

## Stage 7 — every language on the network (wire-gateway)

Only Go and Rust serve TCP. Python, TypeScript, Haskell (and any other
stdio worker) run as child processes of the host, so `connect`,
`WireAuth` and `WireSecurity` do not reach them. Giving each its own
TCP server would mean four more servers, four more TLS stacks and four
more HMAC checks. One GATEWAY serves them all:

- [x] `okay/py/gateway.py` (shipped in okay-py's jar, standard library
      only): `gateway.py --listen HOST:PORT -- WORKER COMMAND...`. On
      every connection it STARTS the worker command, so each connection
      gets a fresh worker, as with Go and Rust. It relays the worker's
      stdio to the socket, and prints `{"listening": ..., "tls": ...}`
      once bound.
- [x] The gateway is where the network's layers live, so the workers do
      not change:
      - TLS with `OKAY_TLS_CERT` and `OKAY_TLS_KEY`;
      - the stage-5b auth with `OKAY_WIRE_SECRET` or
        `OKAY_WIRE_SECRET_FILE`. The gateway adds the challenge to the
        worker's hello, answers the host's `auth` itself, and refuses
        everything else until it passes, closing on a wrong mac, exactly
        as Go and Rust do.
      After that it relays bytes untouched, so the stage-5a `configure`
      and the frames that follow reach the worker as they would over a
      pipe.
- [x] `ForeignGateway.command(worker, listen)` builds the command line
      from Scala. The Python worker's command is `ForeignWorker`'s own.
- [x] The conformance suite passes through the gateway for Python and
      TypeScript, with a secret and with TLS; Haskell's programs pass
      through it too.
- [x] Refusals are the same as Go's: no secret, the wrong secret, a
      request before the auth, TLS mismatches.

## Stage 6 — reliability (wire-read-deadline)

The operator: "рилайибилити - если чтото отвалилось там и таймаут - что
тогда происходит? должно както все востанавливаться (по желанию конечно
со стороны обработчика) - у нас мне кажется для этого есть все". The
pieces already exist: the fault model (a death throws, and a supervisor
replaces the process, as `PyWorkers` does), R's timeout with respawn,
okay-platform's `retry`, and `Durable`'s insight that a journal of ANSWERS
replays a program.

- [x] `given WireDeadline` (default: none, as before):
      `WireDeadline.after(duration)`. On a stream link (pipes, TCP), an
      answer that does not arrive in time CLOSES the link. The only way to
      abandon a blocked read is to take the wire with it. The call answers
      `Left(Condition("timeout", ...))` as data, and the engine is dead
      afterwards. On an in-process link a call cannot be abandoned (FFM
      and Chicory run on the caller's thread), so a deadline there is
      refused by name at construction rather than promised.
- [x] `ForeignWorker.supervised(open)`: the same handler shape over a
      worker that is REOPENED by `open` after a death or a timeout: a
      fresh process (`start`, `speaking`), a new connection (`connect`),
      with the same givens. A plain call (`Call`, `Frame`, a direct-style
      dialogue) caught in the failure answers `Left(Condition(...))`
      naming what happened, and the next call runs on the fresh worker.
      Whether to retry is the caller's: the far side may have done the
      work before it went silent, and only the caller knows whether
      doing it twice is harmless (okay-platform's `retry`).
- [x] Programs as data SURVIVE a restart. A far-side program is a pure
      function of the answers it was given (`perform`/`then`), so a
      continuation is fully described by its run's `(fn, args)` and the
      PATH of answers that reached it. The supervisor records the paths.
      On a fresh worker it re-runs the program and replays the path,
      which re-derives the continuation, and then continues it. A step
      that failed mid-flight is redone the same way, so a `Choice` over
      a far-side program returns every branch across a killed worker. A
      replay that meets a different operation than the path recorded is
      a far side that is not deterministic, answered as
      `Condition("ReplayDrift", ...)` rather than a wrong answer.
- [x] Held objects (`Hold`) die with their worker: a later use is refused
      by name, as in `PyWorkers`.
- [x] Every stdio worker survives a KILL from outside
      (supervised-crash-every-language): one suite, `CrashConformance`,
      starts the worker's process itself and SIGKILLs it by that pid —
      the way an OOM kill or a crash arrives, not an exit the program
      chose — on Python, TypeScript, Go, Rust and Haskell: between two
      choices of a multi-shot program (every branch comes back), while
      idle (the next program runs on a fresh process), and mid-ask in
      direct style (`WorkerDied` as data, then the next call runs; not
      Haskell, which serves programs only).
- [x] Tests: a silent Python call past its deadline; a worker killed
      between two continuations of a multi-shot program (all four
      branches); a Go TCP connection dropped and reconnected (the same,
      over the network); a direct-style dialogue killed mid-ask; a drift;
      and the deadline refused in-process.

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
- **The default compresses where bytes are the cost, not where CPU is**
  (wire-compression-measured). Refused alternatives: (1) a SIZE
  threshold on every link (compress only past ~512 bytes): it needs a
  per-message flag byte in five workers, and on a pipe even the large
  message loses (0.64 -> 1.5 ms for bytes that cost nothing to move); on
  a network the one message class that loses, the short one, loses two
  bytes and ~2.5 us, below a network's round trip. (2) Keeping DEFLATE on
  pipes and only pooling the codec: the pool took a short message from
  4.1 to 3.1 us, still 5x the plain 0.6 us — the rest is zlib itself.

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

- R on the wire givens (wire-givens-r, 2026-09-24).
  - `okay.codec` (okay-codec's JVM sources) now holds the wire:
    `WireFormat`, `WireCompression`, `WireCbor`, `WireJson.whole` (the
    strict line read), `WireFrames` (lines, then frames) and
    `WireNegotiation` (choose, configure, confirmed). `ForeignWorker` and
    `RSubprocess` are two engines over one handshake, and `okay.py`
    re-exports the names, so `okay.py.WireFormat.Cbor.given` still means
    what it meant.
  - `WireCompression.preferred` is now an ORDER: deflate, zlib, none.
    `Zlib.given` is strict. `announced` accepts a bare string, because
    jsonlite unboxes a one-element list.
  - `RSubprocess` reads bytes (`BufferedInputStream`), not characters. A
    timeout's read of a whole message runs off-thread as before, and the
    respawn negotiates the same wire again (pinned by a test on all four
    wires).
  - shim.R: `say` and `read_msg` handle both modes; `configure` switches
    after its own answer. CBOR is encoded from jsonlite's own parse of
    the JSON it would have printed, so both formats carry the same
    values, and an array of numbers goes out in one `writeBin`. The
    output goes to `/dev/stdout`, opened raw.
  - FOUND and fixed: doubles always left R with 15 significant digits
    (`digits = NA` is jsonlite's "max precision", which is 15). It is
    now `digits = I(17)`. The same-answers suite failed on `sqrt(2)`
    over the OLD JSON wire first.
  - Tests: `RWireConformance`, 6 cases over (json, zlib), (json, none),
    (cbor, zlib) and (cbor, none), plus a refusal of an explicit
    Deflate; all 75 of okay-r's live tests pass on the new default.
    Default gate: zlib's round trip and refusals, the order, and a
    strict Zlib.
  - Mutant: little-endian doubles on the one-writeBin road failed every
    CBOR case and none of the JSON ones.

- Authorization (wire-auth, 2026-09-24).
  - `okay.codec.WireAuth`: `Off`, the default; `secret(bytes)`,
    `fromEnv(name)` and `fromFile(path)`, each read when a worker is
    opened, an unset or empty source refused by name.
    `WireNegotiation.authenticate` runs before `configure`.
    `ForeignWorker.over`, `speaking` and `connect` take it as `using`.
  - Go (`crypto/hmac`, `crypto/rand`) and Rust (HMAC over `sha2`, ten
    lines; the nonce from `/dev/urandom`, no new dependency beyond the
    digest) announce the challenge when `OKAY_WIRE_SECRET` or
    `OKAY_WIRE_SECRET_FILE` is set on a TCP server. They answer every
    other request with a PermissionError until the auth passes, and
    close the connection on a wrong mac.
  - Tests. Default gate: the RFC 4231 vector, a mutual pass that sends
    one line and never the secret, a server whose proof is wrong, every
    refusal by name, and `fromFile`'s newline. Live: the whole
    conformance suite behind a secret on Go and Rust, plus a wrong
    secret, no given, a request before the auth (refused, not served),
    and a given meeting a server without one.
  - Mutant: a Go server that skips its check. The suite still refused
    the connection, but from the HOST's side, because the server's
    answer did not prove the secret. The test failed on the message,
    which is what showed the mutuality working.

- One line in-process (in-process-oneliner, 2026-09-24):
  `ForeignWorker.inProcess(library)` and `ForeignWorker.inProcessWasm(module)`,
  extensions in `okay.rust`, since FFM and Chicory live there. A file
  without `okay_exchange` is refused by name (tested against the Argon2
  kernel).

- Reliability (wire-read-deadline, 2026-09-24).
  - `okay.codec.WireDeadline` (a given, default none). `ForeignWorker` does
    its exchanges on one daemon reader when a deadline is set. At expiry
    it marks itself dead, closes the link, and the call answers
    `Left(Condition("timeout"))` (`PyStep.Done(Left)` for a dialogue).
    `ForeignWorker.alive` says so. In-process the deadline is refused at
    `over`.
  - `ForeignWorker.supervised(open)` gives `SupervisedWorker`, the same
    handler. It reopens on the next use after a death or a timeout, and
    `restarts` counts the reopenings. Continuations are renamed to the
    supervisor's own ids, each remembering (run, path of answers, op,
    args, local k, generation). A continue on a generation that is gone
    re-runs the program and replays the path first, checking at the end
    that the operation and arguments are the recorded ones
    (`ReplayDrift` otherwise). A step that failed mid-flight is retried
    once on a fresh worker. Refs and dialogue keys are `generation << 40
    | local`, so a stale one is refused by name, never re-pointed.
  - Tests. Live (Python): a timeout as data with the worker dead after
    it; supervised recovery after a timeout; a process CRASH
    (`os._exit`) between two choices of a multi-shot program (all four
    branches); a pid-named operation caught as drift; a crash mid-ask
    (WorkerDied); a stale ref. Live (Go): the server killed and restarted
    on its port mid-program, reconnected and replayed. Default gate: the
    in-process refusal.
  - Mutant: a supervisor that never replays failed the three recovery
    tests, with the far side's own "continuation ... is not held here".
  - R's engine respawns after a timeout and, since r-supervised-replay
    (2026-09-24), replays its programs on the fresh R the same way:
    `RSubprocess` renames an R run's continuations and keeps each one's
    path of answers.

- Encryption (wire-tls, 2026-09-24).
  - `okay.codec.WireSecurity`: `Plain`, the default, or `tls(trust)`, with
    `Trust.pem`, `Trust.pemFromEnv` and `Trust.system`. `WireLink.tcp`
    wraps the socket in an `SSLSocket` and turns on HTTPS endpoint
    identification, so the NAME is verified. The hello read is limited by
    the deadline, or 10 s. `connect` takes it as `using`.
  - Go: `tls.NewListener` over the TCP listener when `OKAY_TLS_CERT` and
    `OKAY_TLS_KEY` are set (TLS 1.2 or later), and the listening line
    says `"tls"`. Rust: the `tls` feature (rustls 0.23 on ring, and
    rustls-pki-types' PEM reading, all offline). `serve_lines` now takes
    one duplex stream (`Duplex`, `Stdio`), because a TLS stream cannot be
    split. `RustWorker.build/buildLibrary(features = ...)` passes
    `--features okay/...`.
  - Tests (`TlsConformance`, on Go and Rust; certificates made by
    openssl per run): the conformance suite over TLS; `localhost` by
    name; a stranger's trust, a certificate for another name, plain-to-TLS
    and TLS-to-plain, each refused by name; TLS with `WireAuth`; and a Rust
    worker without the feature refusing at start.
  - Mutant: without endpoint identification, the other-name certificate
    was accepted, and exactly that test failed.

- Every language on the network (wire-gateway, 2026-09-24).
  - `okay/py/gateway.py`: `socket.create_server`, a thread per connection,
    `subprocess.Popen` of the worker, `ssl.SSLContext` server-side, `hmac`
    with `compare_digest`. The hello is rewritten only to add the
    challenge; after the auth, two pumps copy bytes. The worker's
    environment drops the gateway's own keys.
  - Scala: `WorkerCommand`, `ForeignWorker.pythonCommand`,
    `TsWorker.command` (TsWorker.start now uses it),
    `ForeignGateway.start/command/script`.
  - Tests: `TestGatewayPyTls` is the TLS suite Go and Rust pass
    (`TlsConformance`; its compose test now uses `address("quote")`).
    `TestGatewayTsAuth` covers a secret, a wrong secret and a worker per
    connection. `TestGatewayHs` checks that CBOR passes through untouched.
  - Found: the Python shim died with a traceback on a line that was not
    JSON (a TLS handshake reaching a plain gateway), then again on its
    answer to a host that had left. It now answers the line, as the other
    workers do, and exits quietly on a broken pipe.
  - Mutant: a gateway accepting any mac. The wrong-secret test failed,
    and the host still refused, because the gateway could not prove the
    secret back.

- wire-compression-measured (2026-09-24).
  - `WireCodecBench` (okay-py's first JMH): JSON and CBOR x none,
    DEFLATE and the pre-pool DEFLATE (`deflate-fresh`, kept as the
    lane's control), on a small (`continue` step), a medium (40
    arguments) and a large (2000 rows) message. JSON, load ~40:

    | message | bytes plain/deflate | plain | deflate (pooled) | deflate-fresh | B/op pooled vs fresh |
    |---|---|---|---|---|---|
    | small | 51 / 53 | 0.80 us | 3.1 us | 4.1 us | 2 384 vs 18 912 |
    | medium | 1 620 / 355 | 14 us | 22.8 us | 23.8 us | 57 864 vs 70 616 |
    | large | 144 811 / 17 134 | 0.64 ms | 1.5 ms | 3.7 ms (+-5.4) | 3.65 MB vs 3.61 MB |

    CBOR's bytes, from the first run: 34/36, 1 286/395, 110 581/18 332.
  - The default preference is taken on network links only
    (`WireLink.network`); `WireNegotiation.choose` takes `network` in
    place of `inProcess`. Pipes, R's included, settle on `json/none`.
  - Tests: `TestWireGivens` (a pipe fake stays plain, an explicit
    Deflate still compresses it; the pool round-trips mixed sizes, an
    empty message, a refusal followed by a whole message, and 64
    concurrent messages); live: `TestGoTcp` asserts `json/deflate` with
    no import, `TestPyPipes` `json/none`, `TestPyPipesDeflate` (was
    `...Off`), `TestRWireZlib` (was `TestRWireOff`), `TestRWireCbor` now
    asks for zlib by name.
  - Found: an EMPTY message was refused as "cut short" by the old
    inflater too (its last step yields nothing and finishes together).
  - Mutants: the network condition removed (five fake-link tests red),
    and the Deflater kept without a reset (the pool test red).

- supervised-crash-every-language (2026-09-24).
  - `CrashConformance` (okay-py tests): the suite starts the worker's
    command itself (`WorkerCommand`) and SIGKILLs that process; five
    subclasses (Python, TypeScript, Go, Rust, Haskell), 15 results, the
    direct-style case skipped on Haskell.
  - FOUND on its first run, on every language: a far side killed from
    OUTSIDE broke the wire rather than ending it. The JDK closes a dead
    child's pipes, so the next write threw `IOException: Stream closed`,
    which escaped `SupervisedWorker` (it restarts only a worker named
    DEAD) and failed the program. `TestSupervised`'s `os._exit` from
    inside a program had always ended the stream cleanly, so it never
    showed. `ForeignWorker.send` and `RSubprocess.send` now name an
    `IOException` on the wire a dead worker; the same holds for a
    socket a peer reset.
  - Red before the fix (12 of 14 run), green after.
