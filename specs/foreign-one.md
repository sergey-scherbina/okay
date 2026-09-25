# foreign-one — one runtime model behind the foreign facade

## Overview

The operator's ask (2026-09-25): "посмотри как у нас сделано
взаимодействие с внешними языками — R, питон, rust, haskell, clojure,
frege — подумай как это унифицировать и абстрагировать, чтобы один и
тот же код прозрачно работал с каждым из них, чтобы новый язык
вписывался в эту же систему, и чтобы работало всё — эффекты, коллбеки
в обе стороны, стримы, асинхронность, большие объёмы данных — быстро и
оптимально."

The CALLER's side of that is done: `okay-foreign-cluster`'s facade
(specs/foreign-facade.md, landed the same day) is one typeclass per
capability by the module's type — `Calls`/`Frames`/`Streams`/
`Programs`/`Holds`/`Methods`/`Speaks[-M]` — so a job written once runs
on every module type that has the instances it asks for, and a
capability a language lacks is a compile error. This spec is about the
RUNTIMES under it, which are still three families that do not share
enough:

| family | languages | engine | continuation | what only it has |
|---|---|---|---|---|
| wire (`ForeignWorker` over `WireLink`) | Python, TypeScript, Haskell, Go, Rust | one protocol (`call`, `frame`, `start`/`ask`/`resume`, `program`/`continue`/`forget`, `hold`/`method`/`attr`/`release`), links pipes/TCP/FFM/wasm, givens for format, compression, auth, TLS, deadline, `SupervisedWorker` replay (specs/polyglot-one-wire.md) | one-shot (direct style) or multi-shot (programs as data, `k` by id) | the gateway, TCP, auth, TLS, crash conformance |
| R | R | its OWN engine `RSubprocess` + `RValue`/`RCodec`/`REval` + shim.R — the same protocol by shape, a second copy of ~2 300 lines; shares only `okay.codec.Wire*` (wire-givens-r) | the same, in its own code | nothing — and it LACKS the column above (docs/one-language.md "Limits": "R is not behind the gateway") |
| in-JVM (`okay.Foreign` walker over `View[P]`, interop-shared) | Frege, Clojure (Java gatherers through `Push`) | the program tree walked directly; values are `Object`; an operation is tested by `Member[F]` | the language's own function, multi-shot, 0.27 µs a step | zero copy, no wire at all |

Measured against the ask, five gaps sit between the facade and the
runtimes. Each is one stage below, filed as its own backlog item under
`polyglot`, and this spec is the one place their Decisions and Results
go.

1. **Two engines** — R is outside the `ForeignWorker` family, so every
   wire feature is built twice or not for R.
2. **A half-duplex wire** — one exchange in flight per worker; the
   callback dialogue nests strictly; a stream is only host-driven
   (`Streams.viaFrames`), `Speaks.stream` is false everywhere, and a
   foreign function cannot await two okay operations at once even in
   a language that could.
3. **Bulk data is not on every side** — `frame` only in Python, R and
   TypeScript; in-process (FFM) a frame is still IPC bytes COPIED
   through `okay_exchange`.
4. **Two vocabularies for "what the far side may perform"** — on the
   wire an effect is a `Cb[F]` with `Schema`s and typed stubs are
   generated (`Rs.ops`, `Go.ops`, `Hs.ops`, `Ts.ops`); in the JVM
   family an operation is a raw `Object` and Frege/Clojure bind
   `okay.frege.Ops`/`okay.clojure.Ops` by hand.
5. **The facade is not filled, and "add a language" is not a
   procedure** — no `TsModule`/`HsModule`/`GoModule`/`RustModule`;
   Clojure and Frege reach the facade only by hand-registering a
   Scala function on `JvmModule`, not through their own programs.

The claim: after the five stages ONE engine (`ForeignWorker`) serves
every language that is not in the JVM, ONE walker (`okay.Foreign`)
serves every language that is, ONE wire carries calls, callbacks,
programs, streams and frames concurrently on every link, ONE effect
declaration in Scala produces the typed stubs of every language, and a
new language is a checklist against three conformance suites and one
golden transcript — with every existing suite green and the price of
each road measured, not believed.

## What is NOT being redone (decided elsewhere, and right)

- **The facade's shape** (specs/foreign-facade.md Decisions 1–6): a
  typeclass per capability, tiers by the data, `Schema` and `Table` as
  the vocabulary, the JVM's zero-cost tier as the test of the model.
- **`okay_call` as a dialogue, not an upcall** (polyglot-one-wire,
  "Why a dialogue"): a callback is a program that must run under ALL
  the caller's handlers. Stage 2 keeps every transport a dialogue.
- **No native struct per operation over FFM** (polyglot-one-wire,
  Decisions): a message is a line or a frame everywhere.
- **Continuations kept by id, not replayed** (remote-foreign): replay
  is a RECOVERY road, not the multi-shot road.
- **Chunks over a generator, for R** (foreign-highlevel stage 6): R is
  single-threaded, so R stays sequential BY RULE; stage 2 makes that a
  claim in the hello rather than a fact only the shim knows.
- **The lane rules for numbers** (docs/benchmarks.md, `performance`
  skill): a cell in this spec's tables is measured through
  `MeasureFacade`/`WireCodecBench`/`MeasureRFrame`, with load and sha.

## Interface

### One engine: R as a `ForeignWorker` far side (stage 1)

R's shim speaks exactly the messages of `okay/py/shim.py`; the value
escapes it needs and Python's do not are additive on the SAME wire:

| R value | on the wire | why not Python's |
|---|---|---|
| `NULL` | `null` | Python's None is `null` already |
| `NA` (typed) | `{"t":"na","of":"int"\|"double"\|"string"\|"bool"}` | Python has no NA; the escape is the distinctness `TestRCodec` holds (NA ≠ NULL) |
| a named list that is not a data.frame | `{"t":"dict","kv":[...]}` | it is what `Named` already was (wire v2) |
| a `Long` past 2^53 | `{"t":"int","digits":"..."}` | already Python's road since foreign-typed-calls |

Then `okay.r.RSubprocess` is `ForeignWorker.speaking(Seq(rscript, shim))`
with R's `WireDeadline` as the deadline it already takes; `okay.r.R` is
a facade over `okay.py.Foreign` the way `okay.py.Ts` is (33 lines);
`REval` is an alias of `ForeignEval` as `PyEval` is; `RValue` is gone
and `RCodec` is `PyCodec` at `Shape.r` (the one place NA is decided).
`Programs.r`, `Holds.r`, `Speaks.r` in the facade lose their own
bodies and become the Python instances at an R command.

### One wire, multiplexed, with credits (stage 2)

Every message carries an `id`, and the id is USED: a reader per link
matches answers to requests, so more than one exchange is in flight on
one worker. The five message kinds of today keep their shape; two are
added, one each way:

```
host -> {"id": n, "op": "program"|"call"|"frame"|"start"|"continue"|..., ...}   as today
far  <- {"id": n, "ok": ...} | {"id": n, "condition": ...}                       in ANY order
far  <- {"id": n, "ask": {"cb": ..., "args": [...], "k": k}}                     a callback, as today
host -> {"op": "resume", "k": k, "ok"|"condition": ...}                          as today

far  <- {"stream": s, "chunk": <one tier-2 frame>}                               NEW: a far-driven chunk
far  <- {"stream": s, "end": true} | {"stream": s, "condition": ...}
host -> {"stream": s, "credit": c}                                               NEW: back-pressure by credit
```

- A stream `s` is OPENED by an ordinary request whose answer is
  `{"ok": {"stream": s}}` (a `frame` op with `"stream": true`, or a
  program performing `okay.stream(...)`); the host grants `credit`
  chunks, the far side sends at most that many, and asks for nothing —
  a credit of 0 is a pause. This is the credit-based flow control of
  reactive streams and HTTP/2, on the wire okay already has.
- A far side ANNOUNCES it in the hello: `"speaks": {"mux": true,
  "stream": true}`. One without `mux` is served exactly as today —
  one exchange in flight, strictly nested — and `Speaks(module)`
  reports `stream: false`, so the facade's `Streams` takes
  `viaFrames`. R (single-threaded) and Rust on wasip1 (no threads)
  say `mux: false` by design.
- In-process links gain a second entry point beside `okay_exchange`:
  `okay_poll(out_len) -> resp` answers the next message the far side
  has to send (a chunk, an ask from a thread of its own), or nothing.
  `okay_exchange` stays the whole link for a worker without `mux`.
- **Async on the far side is what `mux` gives**: a Go/Rust/TS/Haskell
  function may have several `ask`s outstanding (each its own `k`), and
  two programs of one worker interleave. The host answers every ask as
  a program under the caller's handlers, on the caller's scheduler —
  nothing changes in what a callback IS, only in how many are open.
- **`Durable` and `SupervisedWorker` journal by `(id | stream, seq)`**,
  not by position: the strict nesting was what made "the path of
  answers" a list, and on a multiplexed wire the path is a map from
  request id to its answers. A replay that meets an id the journal
  has no entry for is `ReplayDrift`, as today.
- Every `WireLink` keeps `roundTrip`/`exchange` for the handshake and
  for a non-mux far side; a mux far side is driven by `send` and a
  reader that `receive`s.

### Bulk data on every side, and zero copy in process (stage 3)

- `frame` in the Rust, Haskell and Go shims (the backlog item
  `foreign-frame-op-rust-hs-go` names the per-language shape: a struct
  of columns, the columnar JSON of r-frame-columnar-wire, Arrow where
  the language has a library, announced as `frames: ["arrow"]`).
- **FFM: the Arrow C Data Interface.** In process a frame crosses as
  two structs, `ArrowSchema` and `ArrowArray`, whose buffers the
  producer OWNS and the consumer reads in place: `okay.arrow.Table`
  exported to a Rust `arrow::ffi::FFI_ArrowArray` and back, with the
  message head (`fn`, `args`) as a JSON line beside it. The `frame`
  request carries `{"arrow": {"schema": <addr>, "array": <addr>}}`
  instead of bytes; the far side's answer the same. Wasm has no shared
  buffers with the host in this sense, so its road stays IPC bytes
  written straight into the module's linear memory (as `WasmLib`
  already does).
- **Tier 3 is a stream of frames with credit** (stage 2), not a loop
  of tier-2 exchanges; `Streams.viaFrames` stays as the road for a
  far side without `stream`.

### One effect declaration (stage 4)

A set of operations a far-side program may perform is declared ONCE,
in Scala, as the `Cb`s the facade already has (`Cb[F]`: a name, an
argument `Schema`, an answer `Schema`, a program). From it:

- the wire languages get their typed stubs as today (`Rs.ops`,
  `Go.ops`, `Hs.ops`, `Ts.ops`);
- the JVM languages get theirs the SAME way: `Frege.ops(name, cbs)`
  writes the `native` bindings and the `Operation a` constructors a
  Frege module imports, `Clj.ops` the `defn`s of a namespace — in
  place of the hand-written `okay.frege.Ops`/`okay.clojure.Ops`, which
  stay as the generated output for the core effects (Reader, State,
  Throws, Choose, Async);
- the JVM walker's `Member[F]` test stays the runtime mechanism; the
  DECLARATION is what moves.

So "the same code" holds on the far side too: an effect is spelled
once and a program in any language performs it by its typed name.

### The facade filled, and a language as a checklist (stage 5)

- Module types `TsModule`, `HsModule`, `GoModule`, `RustModule` (a
  name, and what the language needs to find the code: a directory, a
  binary, a library) with the instances each honestly gives after
  stages 1–3 — `Calls`/`Programs`/`Speaks` for all four, `Frames`
  once stage 3 lands, `Holds` where the language has it.
- `CljModule` and `FregeModule` over their own `Foreign.View`, with
  `Programs` instances whose `Op` is empty (in-JVM): a Clojure or
  Frege program as data IS something to walk, so Decision 7 of
  foreign-facade (no JVM instance) is narrowed to `JvmModule`, the
  Scala-function case, where it is right.
- **The protocol as one golden transcript**: `specs/foreign-wire.txt`,
  the messages of every conformance case in order, host and far side
  labelled, which `TestWireTranscript` replays against a fake far side
  and which each shim's own tests replay against the shim. A new
  language is written against the transcript, not by reading shim.py.
- **Adding a language is four things**, written in
  docs/foreign-facade.md "Adding a language": (1) a library speaking
  the transcript on a link (Rust's `okay` crate, 1 010 lines, is the
  size); (2) a module type and the instances it gives; (3) its `ops`
  stub writer; (4) `WireConformance`, `CrashConformance` and
  `FacadeConformance` green for each instance it claims.

## Behavior

Stage 1 — one engine (foreign-one-r):
- [ ] shim.R speaks the `ForeignWorker` protocol at `ShimVersion`; the
      handshake refuses the old shim by name
- [ ] every okay-r suite (75 live, the default-gate codec and journal
      suites) is green unchanged over `ForeignWorker.speaking`; NA and
      NULL are still distinct at every depth
- [ ] `RWireConformance`'s four wires pass through `WireConformance`
      itself, with R as a row of docs/one-language.md's table: gateway,
      TCP, `WireAuth`, TLS, `CrashConformance` (SIGKILL) all green over R
- [ ] `okay-r/src/main` shrinks by the engine, the codec and the value
      enum (a line count in Results); `okay.r.R`'s public API is
      unchanged (the doc snippets still pin)
- [ ] R's timeout-respawn is `WireDeadline` + `supervised`, and
      `TestRReplay` holds

Stage 2 — one wire, multiplexed (foreign-one-mux):
- [ ] a far side announcing `mux` has two programs interleaved on one
      worker, each answered correctly (Go, Rust, TypeScript, Haskell)
- [ ] a far-side function with two asks outstanding is answered under
      the caller's Reader for both (Go, Rust)
- [ ] a far-driven stream of 100 000 rows in chunks of 4 096 arrives in
      order, and the far side never has more than `credit` chunks
      unacknowledged — COUNTED on the far side (a shim counter read at
      the end), not believed; a credit of 0 pauses it
- [ ] a far side WITHOUT `mux` (R, Rust on wasm, a v-old shim) is served
      exactly as today: the existing conformance suites green, and
      `Speaks` says `stream: false`
- [ ] `Durable` journals by id; a replay of a two-program interleaving
      answers both from the journal; a missing id is `ReplayDrift`
- [ ] `SupervisedWorker` recovers a killed worker with two programs
      open: both replayed, every branch of a multi-shot back
- [ ] in-process: `okay_poll` over FFM and wasm; the stream case green
      over (Rust, FFM) and (Go, wasm)
- [ ] the price: `WireCodecBench`'s small/medium/large messages on the
      mux reader within noise of the sequential reader (a reader
      thread that costs a hop per message is a defect; measured before
      and after, alternating)

Stage 3 — bulk on every side (foreign-one-bulk):
- [ ] `frame` served by Rust, Haskell and Go; `Frames[RustModule]`,
      `Frames[HsModule]`, `Frames[GoModule]` pass `FacadeConformance.frames`
- [ ] FFM: a 1M-row `Table` through a Rust `frame` function with NO copy
      of its buffers — held by an allocation count (the JVM's Arrow
      allocator's bytes allocated before and after), not by time alone
- [ ] the measurement table below has a number in every cell a language
      claims; a cell worse than the language's own road is a defect

Stage 4 — one effect declaration (foreign-one-ops):
- [ ] `Frege.ops` and `Clj.ops` write the bindings for a `Cb` set; the
      shipped `okay.frege.Ops` and `okay.core` are regenerated from the
      core effects' declaration and the diff is empty
- [ ] a Frege program performing a generated typed operation runs under
      the Scala caller's Reader; a wrong argument type is a Frege TYPE
      error (as hs-typed-effects holds for GHC)
- [ ] the same for Clojure (a runtime refusal by name — Clojure has no
      static types to refuse with, said so)

Stage 5 — the facade filled (foreign-one-modules):
- [ ] `TsModule`, `HsModule`, `GoModule`, `RustModule`, `CljModule`,
      `FregeModule` with their instances; `FacadeConformance` green for
      every instance each claims; a claimed-but-absent instance fails
      `summon` under `compileErrors`
- [ ] the golden transcript replays against a fake far side in the
      default gate and against every shim in its live suite
- [ ] docs/foreign-facade.md "Adding a language" is the four-step
      checklist, with the transcript named, pinned by the snippet check

## Measurements — language × tier × link (to fill)

Extends specs/foreign-facade.md's table with the rows and links this
spec adds. Medians, load and sha in Results, per the `performance` skill.

| language | link | tier 1 value | tier 2 frame | tier 3 stream (far-driven) | async: two asks open | instrument |
|---|---|---|---|---|---|---|
| R | pipes (`ForeignWorker`) | — | 100 000 rows columnar JSON 180 ms today (MeasureRFrame); Arrow: to measure | viaFrames only (mux: false) | no (single-threaded) | MeasureRFrame, MeasureFacade |
| Rust | FFM, C Data | — | to measure, zero copy | to measure | to measure | MeasureFacade |
| Rust | pipes/TCP | — | to measure | to measure | to measure | MeasureFacade |
| Go | pipes/TCP/wasm | — | to measure | to measure | to measure | MeasureFacade |
| Haskell | pipes | — | to measure | to measure | to measure | MeasureFacade |
| TypeScript | pipes | — | serves `frame` today | to measure | to measure | MeasureFacade |
| Clojure, Frege | in-JVM | 1 µs (JvmModule today) | by reference | by reference | n/a | MeasureFacade |

## Out of scope

- A cross-language call that does not pass through Scala (Python
  calling R): every hop goes through the facade (foreign-facade).
- Python's own asyncio in the shim: Python announces `mux: false`
  until somebody measures that a threaded shim beats a pool of
  processes under the GIL (specs/py.md's original argument).
- mTLS, a WebSocket transport, an authenticated in-process link: the
  links stay what polyglot-one-wire made them.
- okay-foreign-cluster's per-partition `Holds`/`Streams` and the
  foreign REDUCE (lanes foreign-streams-holds and foreign-reduce, in
  progress the day this spec was written): they build on the facade
  and take stage 2's far-driven stream when it exists; nothing here
  changes their interface.

## Stages

- [x] Stage 0 — this spec, on the boards (foreign-one, 2026-09-25).
- [ ] Stage 1 — foreign-one-r: R onto `ForeignWorker`.
- [ ] Stage 2 — foreign-one-mux: the multiplexed wire with credits,
      journal by id, `okay_poll` in process. Go and Rust first (they
      have threads and are the reference far sides), then TypeScript
      and Haskell.
- [ ] Stage 3 — foreign-one-bulk: `frame` in Rust/Haskell/Go
      (subsumes foreign-frame-op-rust-hs-go), Arrow C Data over FFM,
      the table filled.
- [ ] Stage 4 — foreign-one-ops: one effect declaration, JVM stubs
      generated.
- [ ] Stage 5 — foreign-one-modules: the module types, the transcript,
      the checklist.

Order: 1 is a pure deletion with no design risk and unblocks R for
everything after; 3's `frame` half and 5's module types are mechanical
and can run beside 2; 2 is the one protocol change and is spec-gated —
its Decisions are written here BEFORE the Go reference lands; 3's C
Data half follows 2 so streams of frames go on the zero-copy road from
the first; 4 last, since it only moves a declaration.

## Decisions

1. **One engine, and the second one is deleted, not bridged.** A
   bridge (`RSubprocess` implementing `WireLink`) would keep the
   codec and value enum duplicated, which is where the R-only defects
   have lived (the `toInt` truncation, the 15-digit doubles). The
   deletion is the point.
2. **Multiplexing by id, not a second connection per stream.** A
   second socket per stream is how Arrow Flight and gRPC do it, and
   it does not exist on pipes, FFM or wasm; ids exist on every link
   already. One reader per link is the price, and stage 2's last box
   measures it.
3. **Credits, not acks.** An ack per chunk is a round trip per chunk —
   the pull road we have, renamed. A credit lets the far side run
   ahead by a bounded amount and the host set that bound per stream
   (the frame is still the memory bound on both sides).
4. **`mux` is a claim in the hello, and the old shape stays a first-
   class road.** A far side that says nothing is the strict nested
   dialogue of today, byte for byte: nothing written against it
   changes, and R is not a special case, it is a far side with
   `mux: false`.
5. **The journal keys by id because the wire does.** The alternative —
   serialising the multiplexed dialogue into one path for `Durable` —
   would need a deterministic interleaving the far side does not
   promise. A map from id to answers is what actually happened.
6. **C Data, not IPC, in process** — and only in process. The C Data
   Interface is a pointer handoff, which is what FFM is; over a pipe
   or a socket the bytes have to be written anyway, and IPC is the
   right serialisation of the same buffers.
7. **Decision 7 of foreign-facade narrows to `JvmModule`.** A Scala
   function has nothing to walk; a Clojure `(step op k)` or a Frege
   `Step op k` does, and `okay.Foreign` already walks it — so
   `Programs[CljModule]` is a real instance whose `run` is the walker,
   not a wire in disguise.
8. **The transcript is the protocol's specification, the shims are its
   implementations.** Until now the Python shim was the reference by
   being first; a new language had to read it. A transcript is
   replayable, so a shim's conformance to it is a test, not a reading.

## Results

(none yet — stage 0 is the spec)
