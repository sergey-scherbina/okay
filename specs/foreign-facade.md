# foreign-facade — one facade over every foreign language, one data model in three tiers

## Overview

The operator's ask (2026-09-25, backlog polyglot/foreign-facade): our
code must work with every foreign language THE SAME WAY — R, Python,
Haskell, Rust, Clojure, Frege, Go, TypeScript, and the list will grow —
and at the same time squeeze the most out of each: data must cross in
bulk and fast. One data model that does not LIMIT but shows how to do it
right, so that any further language plugs in the same way.

What exists, and what this spec is built FROM rather than beside:

- **The wire.** Programs as data on one line protocol (`perform`,
  `continue`, `done`; specs/remote-foreign.md), served by Python, R,
  TypeScript, Haskell, Go and Rust over pipes, TCP, FFM and Wasm
  (specs/polyglot-one-wire.md; its conformance suite runs one test body
  over every language and transport). Clojure and Frege are IN the JVM:
  a third kind of link, a function call.
- **The typeclass by module** (foreign-engine-typeclass, 2026-09-25):
  `Engine[-M]` (map) and `Reduces[-M]` (reduce), instances per MODULE
  type (`PyModule`, `RModule`, `JvmModule`), each optional, so a job
  names a module and a function and the implicit says who runs it —
  and a reduce on a module without `Reduces` does not compile. That is
  the shape this spec generalises: one typeclass per CAPABILITY.
- **The data.** `Schema`-typed values cross as JSON; `okay.arrow.Table`
  (typed, nested columns; Arrow IPC) is the bulk road, and only Python,
  R and TypeScript serve the `frame` op today (backlog
  foreign-frame-op-rust-hs-go). `PyValue` and `RValue` are two enums
  saying the same thing twice. Measured (specs/r.md, r-arrow): our share
  of a 100 000-row R round trip is 0.3% — the tree road is the ceiling
  and the columnar road the floor, and today a language gets whichever
  its shim happened to grow.

The claim: a job written once against the facade runs on every language
that has the capability it asks for, the data crosses on the fastest
road that language has for the data's SHAPE, and a language that lacks a
road degrades to the one below by a rule — never by a surprise, never
silently to a slower road than its best.

## Interface

### One typeclass per capability, by the module's type

```scala
// the base (exists): a named function over a chunk of rows
trait Engine[-M]:
  def name: String
  def batcher[A: Schema, B: Schema](module: M, fn: String, workers: Int): Batcher[A, B]

// extensions, each its own typeclass, each instance optional
trait Reduces[-M]     // exists: step/merge
trait Calls[-M]:      // tier 1: one typed call, a value in and out
  def call[A: Schema, B: Schema](module: M, fn: String)(a: A): Either[Condition, B] ! Async
trait Frames[-M]:     // tier 2: a Table in, a Table out
  def frame(module: M, fn: String)(in: Table): Either[Condition, Table] ! Async
trait Streams[-M]:    // tier 3: frames crossing one at a time, back-pressured
  def stream[A: Schema, B: Schema](module: M, fn: String, batch: Int)(in: Flow[A]): Flow[B]
trait Programs[-M]:   // programs as data: perform/continue/done, multi-shot where the far side's continuations are values
  type Op[+A]         // the language's own effect (ForeignEval, REval)
  def program[Arg: Schema, Out: Schema, F[+_]](module: M, fn: String, cbs: Vector[Cb[F]])(a: Arg): Either[Batcher.Failed, Out] ! (F + Op)
  def run[A](module: M)(prog: A ! Op): A   // the whole dialogue on ONE worker
trait Cb[F[+_]]:      // a callback: a name and a function at Schema types — one type, not one per language
trait Holds[-M]:      // object handles: method, attribute, release
  def hold[A: Schema](module: M, fn: String)(a: A): Either[Condition, Handle] ! Async
```

A module type per language — `PyModule`, `RModule`, `JvmModule` exist;
`TsModule`, `HsModule`, `GoModule`, `RustModule` are the same shape (a
name, what the language needs to find it) — and adding a language is:
a module type, the instances it can honestly give, and the conformance
suite green for each instance it gives. No `mapXx`, no `XxStage`, no
new door.

### `Speaks[M]`: the runtime truth behind the compile-time promise

An instance says a language CAN; the hello says what this worker DOES
(`"speaks": {"format": [...], "compress": [...], "frames": [...]}` is
already half of it). `Speaks(module)` answers, from the hello, which
tiers and transports this process has: `frames: arrow | columnar-json`,
`stream: yes | no`, `programs: multi-shot | one-shot`, `link: pipes |
tcp | ffm | wasm | in-jvm`. The conformance suite checks the claim in
both directions: a capability the hello claims must pass its test; a
test a language passes must be claimed (a language may not claim less
than it does, or a job degrades for nothing).

### The data model: three tiers, chosen by the DATA

| tier | shape | crosses as | who chooses it |
|---|---|---|---|
| 1 VALUE | a `Schema`-typed value, a record, a small list | one JSON line (the wire's tree) | a call with a value |
| 2 FRAME | a table: `okay.arrow.Table`, or rows `Vector[A]` (`Road.rows`) | Arrow IPC where the far side speaks it; the columnar JSON of r-frame-columnar-wire where it does not | a call with a Table, or with rows |
| 3 STREAM | more than fits in memory: `Source[Chunk[A]]` | one tier-2 frame per chunk, the next asked for by the wire's own `continue` (back-pressure) | a call with a Source |

Every language MUST give tier 1 (the wire needs nothing but a JSON
line). Every language SHOULD give tier 2 with Arrow where it has an
Arrow library (pyarrow, R arrow, Rust `arrow`, Go `arrow/go`, Java for
Clojure/Frege) and MUST accept the columnar JSON shape otherwise. A
language MAY give tier 3; one that does not gets tier 3 as a sequence
of tier-2 frames driven from the Scala side (what `mapIn` does today at
4096 rows a frame). Degradation is a RULE the facade applies and
`Speaks` reports, never a fallback a shim quietly took — and a strict
given (`FrameFormat.Arrow.given`) still refuses by name.

`PyValue` and `RValue` stop being the facade's vocabulary: the facade
speaks `Schema` (a value) and `Table` (a frame); a language's own value
enum is its codec's business, behind its instances. One `Schema`
describes a value on every side, and okay-codec's `JsonSchema` is what
a shim's stub generator reads.

### The zero-cost tier

`JvmModule` (Scala, Clojure, Frege, Java) gets the SAME instances with
the link being a function call and the frame being the SAME `Table`
object: `Frames[JvmModule]` hands the Table by reference. That is the
test that the model does not limit — a tier the fastest language pays
nothing for is a tier every language can grow into.

## Behavior

- [ ] one job text — a call, a frame, a stream — compiles against
      `Calls[M]`/`Frames[M]`/`Streams[M]` and runs on `PyModule`,
      `RModule` and `JvmModule` with only the module changed
- [x] a capability a language lacks is a COMPILE error at the call
      (`compileErrors`), not a runtime refusal (TestFacade)
- [ ] `Speaks(module)` answers the hello's claims, and the conformance
      suite fails when a claim and a test disagree in either direction
- [x] tier 2 crosses as Arrow IPC to a worker whose hello says
      `frames: ["arrow"]`, as columnar JSON otherwise, and the caller's
      Table comes back a Table either way, equal column for column
      (FacadeConformance.frames; columnar JSON exercised here — the
      box's python3 has no pyarrow — and the Arrow road is
      `ForeignWorker.sendArrow`'s, py-arrow's own tests)
- [x] tier 3: rows cross one frame at a time and no frame ever holds
      more than `batch` rows — COUNTED at the frame seam by a `Frames`
      that records what it is handed (FacadeConformance.streams,
      CountingModule), which is the bound on both sides; the carrier is
      the cluster's `Flow[A]`, not a bare `Source[Chunk[A]]` (Results)
- [x] `Frames[JvmModule]` passes the Table by reference (identity holds:
      TestFacade)
- [x] the SHAPE picks the tier: a value is a call, rows are one frame
      (`Road.value`, `Road.rows`), a source a stream — there is no size
      at which rows become calls (Decision 6; the `Frame.Threshold` the
      first draft had is withdrawn)
- [ ] the measurement table below has a number in every cell a language
      claims, from the existing instruments, and a cell worse than that
      language's own best road is a defect, not a result

## Measurements — language × tier × transport

The numbers that keep the facade honest. Known today, before the facade
(medians; the instruments are named so a cell is re-measured, not
believed):

| language | tier 1 value | tier 2 frame | tier 3 stream | instrument |
|---|---|---|---|---|
| Scala (`JvmModule`) | — | 1M rows map: 6–12 ms | — | MeasureForeignMapReduce |
| Python, pipes | — | 1M rows map, JSON: ~200 ms; Arrow: ~95 ms; `@okay.arrow` vectorised: ~88 ms; reduce in Python: +70 ms | as 4096-row frames (mapIn) | MeasureForeignMapReduce, MeasurePyArrow |
| R, pipes | — | 100 000 rows round trip: 13.7 s as JSON records, 180 ms columnar JSON (r-frame-columnar-wire), Arrow: to measure | — | MeasureRFrame |
| TypeScript | — | serves `frame` (columnar JSON); no Arrow | — | to measure |
| Haskell, Go, Rust | tier 1 only (no `frame` op: foreign-frame-op-rust-hs-go) | — | — | to measure |
| Clojure, Frege | in-JVM, `JvmModule` | same object | — | to measure |

Empty cells are the work; a cell filled by this lane goes into Results
with its date, load and sha (the `performance` skill).

## Out of scope (v1)

- Rewriting a shim. The facade is the Scala-side seam and the model;
  each shim grows toward it one tier at a time, `frame` in the Rust,
  Haskell and Go shims first (foreign-frame-op-rust-hs-go).
- A new wire. The line protocol, the transports and the hello stay;
  `Speaks` reads what is there and extends the hello only with keys the
  old shims ignore.
- Cross-language calls that do not pass through Scala (Python calling R
  directly): every hop goes through the facade, which is what makes the
  measurement table one table.

## Stages

- [x] Stage 0 — this spec, on the sprint (foreign-facade, f051b870d).
- [x] Stage 1 — `Calls[M]` and `Speaks[M]` for `PyModule`, `RModule`,
      `JvmModule` over the existing workers (foreign-facade-1,
      2026-09-25): `FacadeConformance` is one body per capability
      (`calls`: echo round-trips a record at its type, a raising
      function is a refusal by kind, a missing one is refused too;
      `speaks`: the report's words are the spec's), run by `TestFacade`
      over the JVM and a test's own `EchoModule` (default gate), by
      `TestPyFacade` over python3 and `TestRFacade` over Rscript (Live).
      A module type without an instance (`Mute`) fails `summon` under
      `compileErrors`. `JvmModule.fn[A, B](name)(f)` registers a function
      for the zero-cost tier.
- [x] Stage 2 — `Frames[M]` (foreign-facade-2, 2026-09-25): a Table in,
      a Table out, for `PyModule` and `RModule` over `PyPool.frame` /
      `RPool.frame` (Arrow where the worker negotiated it, columnar JSON
      otherwise — the worker's own road, `Speaks` says which) and for
      `JvmModule` by reference (`JvmModule.frame(name)(f)`, TestFacade
      holds `eq`). `Road.rows[M, A, B]` is the door: rows of `A` as ONE
      frame, back as rows of `B`. The threshold the plan had is gone —
      Decision 6. Still open from this stage: `frame` in the Ts, Hs, Go
      and Rust shims, so their instances can exist
      (foreign-frame-op-rust-hs-go).
- [x] Stage 3 — `Streams[M]` (foreign-facade-3, 2026-09-25): a `Flow[A]`
      through a frame function one frame per chunk of `batch` rows, back
      as a `Flow[B]` — `Road.flow`. DERIVED for every language with
      `Frames` (`Streams.viaFrames`): the next frame goes when the last
      answered, which is the back-pressure, and neither side ever holds
      more than a frame of it. The conformance body `streams` runs
      10 000 rows through a COUNTING `Frames` in frames of 1 000 and
      checks the count (ten), that no frame was bigger, and that every
      row came back in order (TestFacade); 20 000 rows over python3 in
      frames of 4 096 (Live). A far side that drives a stream itself is
      what `Speaks.stream` will say once a shim grows it.
- [x] Stage 4 — `Programs[M]` (foreign-facade-4, 2026-09-25): one
      `Cb[F]` callback over `Schema` types where okay-py and okay-r each
      had their own over `PyValue`/`RValue`; `Programs[-M]` with the
      language's effect as a type member (`Op`: `ForeignEval`, `REval`),
      `program` answering `Out ! (F + Op)` and `run` keeping the whole
      dialogue on ONE pooled worker, since that worker holds the
      continuations. The conformance body `programs` runs the two
      dialogues of specs/remote-foreign.md over the facade — a callback
      answered under a Reader, and a continuation resumed twice by
      Choice (multi-shot across the process) — green over python3 here.
      No JVM instance (Decision 7).
- [ ] Stage 4b — `Holds[M]`: object handles (Python's `Method`/`Attr`/
      `Release`, R's `Hold`/`Release`) under the same shape, with the
      handle's type a member of the instance.
- [ ] Stage 5 — the measurement table filled, every claimed cell.
- [ ] Stage 6 — docs: docs/foreign-facade.md with runnable examples
      pinned by `TestDocExamplesForeignFacade` and the literature — the
      GraalVM/Truffle interop protocol (one facade over languages, the
      closest prior art and what it costs), Arrow Flight and the "one
      memory format" argument, Jupyter's kernel protocol (a hello that
      says what a kernel speaks), Erlang ports, Substrait — and what
      this design takes and refuses from each.

## Decisions

1. **A typeclass per capability by the module's type**, not a
   `Foreign[L]` object with every method: foreign-engine-typeclass
   proved the shape — a capability a language lacks is a missing
   instance and a compile error, and adding a language adds instances,
   not a case to a match. Contravariance keeps the call free of the
   module's type name.
2. **Tiers by the data, not by the language.** The caller says what it
   has (a value, a Table, a Source); the facade picks the road, `Speaks`
   says which one was taken, and a strict given refuses rather than
   degrades. A language never chooses a slower road than its best,
   because the rule is one place and the table measures it.
3. **`Schema` and `Table` are the vocabulary; `PyValue`/`RValue` are
   codecs.** Two enums that say the same thing are two places to be
   wrong; the third language would have made a third.
4. **The zero-cost tier is the design test.** If `JvmModule` cannot
   pass a Table by reference through the same interface, the interface
   is a wire in disguise.
5. **Shims grow, they are not rewritten.** The conformance suite is
   what a shim grows toward; a tier it does not have is a claim it does
   not make.

6. **The shape picks the tier; there is no threshold** (stage 2). The
   first draft had `Frame.Threshold`: a `Vector[A]` past it would cross
   as a frame, below it as calls. It cannot: a far-side function written
   for a frame takes a dict of columns and one written for a record
   takes a record — the two are different functions, and no count turns
   one into the other. So rows are always one frame (`Road.rows`), a
   value is always a call (`Road.value`), a source is a stream; what a
   frame COSTS per size is a measurement (stage 5), not a switch.

7. **Programs as data have no JVM instance** (stage 4). A program on
   the JVM is a Scala function returning `Out ! F`; there is no far side
   to hold a continuation and nothing to cross, so `Programs[JvmModule]`
   would be a wire in disguise — the compile error is the honest
   answer, and the zero-cost tier for programs is calling the function.

## Results

- **Stage 1 (2026-09-25).** `Calls` answers `Either[Batcher.Failed, B]`
  synchronously, the shape `Engine`'s `Batcher` already has — the pool
  borrows a worker for the call, and an `Async` wrapper belongs at the
  call site, not in every instance; the Interface above says `! Async`
  and stage 3 (streams) is where that is decided for real. One
  `Batcher.Failed` is the refusal on every road: okay-py's and okay-r's
  `Condition` are two types, and a facade with two refusals is two
  facades. A missing function on the JVM is a refusal by kind
  (`NoSuchFunction`) at call time, as Python's `AttributeError` is —
  unlike `Engine`'s `batcher`, which throws at build, because a map is
  built once and a call is made per value. `Speaks` in stage 1 reads the
  wire the worker NEGOTIATED (`+arrow` in `ForeignWorker.wire`) plus
  what specs/remote-foreign.md established about the language
  (Python's and R's continuations are values: multi-shot); the hello
  keys the Interface promises come with stage 2, when there is a
  frame road to announce.
- **Stage 2 (2026-09-25).** `Frames` over the workers' own `frame` op,
  the Table converted at the seam (`ArrowFrames.frame`/`table`,
  `RArrowFrames` twins) and refused by name BEFORE the wire for a
  column the frame cannot say. The JVM instance holds `eq`. The
  threshold withdrawn (Decision 6). Python here, columnar JSON: three
  rows there and back, the empty table too, `fboom` a `ValueError`.
  The empty table found a seam: the JSON frame road cannot type a
  column with no cells and answers `Nulls(0)`, which `Rows.rows[A]`
  refused ("Nulls where the schema has an Int"); a table of no rows
  now reads as no rows whatever its columns say — there is nothing in
  it to refuse (okay-arrow, TestRows). Arrow keeps the schema of an
  empty frame; the JSON road does not, and this is where the two roads
  first differed in what they can carry.
- **Stage 3 (2026-09-25).** The carrier is the cluster's `Flow[A]`
  rather than the `Source[Chunk[A]]` the Interface first wrote: a
  `Flow` is what already runs across partitions and workers
  (`Stage.through`, `mapIn`), and tier 3 IS `mapIn`'s road given a
  name and derived from `Frames` for every language — `Streams` exists
  for any module type with `Frames`, without a line per language. The
  memory bound is proven by counting rather than by measuring heap: a
  fake `Frames` records the rows of every frame it is handed, and the
  suite asserts none exceeds the batch — a heap measurement on a shared
  box would have been a belief. Python here: 20 000 rows in frames of
  4 096 through `fecho`, every row back in order.
- **Stage 4 (2026-09-25).** `Programs` is `Py.program`/`R.program`
  behind one shape: the callbacks are converted at the instance
  (`Py.callback[c.Arg, c.Res](c.name)(c.run)`), so the two language
  `Callbacks` types stay where they are and the facade has one. `run`
  borrows a pooled worker for the WHOLE program — `Pool.use` around
  `prog.runWith(using w.handler)` — because a continuation lives in one
  process; a program spread over two workers would resume nothing.
  Python here: `priced` under a Reader answered 12.0, `pairs` resumed
  twice by Choice answered 11, 21, 12, 22 — multi-shot across the
  process, through the facade.
- Python here: python3 3.14 on the box, echo/boom/missing green over
  pipes, frames `columnar-json` (no pyarrow in the box's interpreter —
  the venv of MeasurePyArrow has it). R: not installed on this box;
  `TestRFacade` skips, the body is the same text.
