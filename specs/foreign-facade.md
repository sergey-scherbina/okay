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

> **Where this goes next (foreign-one-model, 2026-09-26).** The SHAPE of
> this facade — a capability per typeclass, tiers by the data, `Schema`
> and `Table` as the vocabulary, the JVM zero-cost tier as the test — is
> kept by specs/foreign-one.md, which derives the one model under it:
> the tier becomes per ARGUMENT (a value, a table, an object, a stream),
> the typeclasses become compile-time markers on a language tag
> (`Tables[L]`, `Objects[L]`, …) over ONE engine, and `Speaks` becomes
> `runtime.speaks`. Its stage 4 (backlog foreign-one-runtime) is where
> the instances below lose their per-language bodies; the API a job is
> written against does not change.

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
trait Holds[-M]:      // object handles: hold, a function with the handle first, release
  type Ref
  def hold[Arg: Schema](module: M, fn: String)(a: Arg): Either[Batcher.Failed, Ref]
  def apply[Arg: Schema, Out: Schema](module: M, fn: String)(ref: Ref, a: Arg): Either[Batcher.Failed, Out]
  def release(module: M)(ref: Ref): Unit
trait Methods[-M]:    // a held object's own method and attribute (Python)
  type Ref
  def method[Arg: Schema, Out: Schema](module: M, ref: Ref, name: String)(a: Arg): Either[Batcher.Failed, Out]
  def attr[Out: Schema](module: M, ref: Ref, name: String): Either[Batcher.Failed, Out]
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

- [x] one job text — a call, a frame, a stream — compiles against
      `Calls[M]`/`Frames[M]`/`Streams[M]` and runs on `PyModule`,
      `RModule` and `JvmModule` with only the module changed
      (`FacadeConformance.job`; TestFacade over the JVM and both frame
      roads, TestPyFacade/TestRFacade compare their answers with the
      JVM's; a module without `Frames` does not compile)
- [x] a capability a language lacks is a COMPILE error at the call
      (`compileErrors`), not a runtime refusal (TestFacade)
- [x] `Speaks(module)` answers the hello's claims, and the conformance
      suite fails when a claim and a test disagree in either direction
      (`FacadeConformance.agree`: frames and programs OBSERVED, a lie
      each way refused — TestFacade offline, TestPyFacade/TestRFacade
      live; a far-side `stream` claim fails until foreign-one-mux gives
      it an observation)
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
- [~] the measurement table below has a number in every cell a language
      claims, from the existing instruments, and a cell worse than that
      language's own best road is a defect, not a result — python3 and
      the JVM filled here (MeasureFacade); R and Arrow cells need a
      runtime this box lacks; the one cell worse than its own road is
      filed (facade-frame-seam)

## Measurements — language × tier × transport

The numbers that keep the facade honest. Known today, before the facade
(medians; the instruments are named so a cell is re-measured, not
believed):

| language | tier 1 value | tier 2 frame | tier 3 stream | instrument |
|---|---|---|---|---|
| Scala (`JvmModule`) | facade 0.001 ms a call | by reference 0.001 ms; 100 000 rows in and out through `Rows.table`/`Rows.rows` 19.8 ms; 1M rows map: 6–12 ms | — | MeasureFacade, MeasureForeignMapReduce |
| Python, pipes | facade 0.217 ms a call, own road 0.130 ms (the difference is `PyCodec` encode+decode at `Schema`) | 100 000 rows one frame, columnar JSON: rows through the facade 182 ms, rows on the own road 176 ms (the same road since facade-frame-seam), a `Table` through `Frames.frame` 152 ms, the bare frame 140 ms; 1M rows map, JSON: ~200 ms; Arrow: ~95 ms; `@okay.arrow` vectorised: ~88 ms; reduce in Python: +70 ms. ARROW (pyarrow 25.0.1, 2026-09-26): rows through the facade 51–54 ms (own road 53 ms: no overhead), a `Table` through `Frames.frame` 11.5–12.5 ms, the bare frame 17–18 ms — beside 157–170 / 124–130 / 106–123 ms on columnar JSON in the same session | 100 000 rows in 4096-row frames: 194 ms — the same as one frame; on Arrow 70–87 ms (JSON 153–163 ms that session) | MeasureFacade, MeasureForeignMapReduce, MeasurePyArrow |
| R, pipes | — | 100 000 rows round trip: 13.7 s as JSON records, 180 ms columnar JSON (r-frame-columnar-wire); Arrow 61.8 ms beside columnar JSON 169.5 ms, arms alternating (2.7x; 10 000 rows: equal, 21 ms) — R with arrow 25 in its own container | — | MeasureRFrame |
| TypeScript | — | serves `frame` (columnar JSON); no Arrow | — | to measure |
| Haskell, Go, Rust | tier 1; the table call served since foreign-one-bulk (columnar JSON) | Rust in process, 1M rows through a function: Arrow C Data 20.7 ms, columnar JSON 119.8 ms (foreign-arrow-ffm) | — | MeasureRustTable; the rest wait for a `Language` (foreign-more-languages) |
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
- [x] Stage 4b — `Holds[M]` and `Methods[M]` (foreign-facade-4b,
      2026-09-25). `Holds` is hold / a function with the handle as its
      first argument / release, for Python and R; `Methods` (a held
      object's method and attribute) is Python's only — R's objects have
      no methods to call, so R has no instance, an honest absence. The
      handle's type is a member of the instance (`Ref`), and the givens
      are the refined aliases `Holds.Py`/`Holds.R`/`Methods.Py`, so a
      handle from `Holds` is what `Methods` takes (checked by
      `compileErrors`). Where a handle LIVES decides the runtime: Python
      goes through `PyWorkers`, the pool that routes a call naming a
      handle to the worker holding it; R keeps every handle of a module
      on one worker of its own (`r-holds` pool of one). Bodies `holds`
      (two objects, each described with its own state, released) and
      `methods` green over python3 here.
- [x] Stage 5 — the measurement table (foreign-facade-5, 2026-09-25):
      `MeasureFacade` (Live) times each tier through the facade beside
      the language's OWN road, medians of five, load printed. Filled for
      python3 (columnar JSON) and the JVM on this box; R's cells wait for
      an R, Arrow cells for a pyarrow interpreter. What it found is in
      Results: tier 3 costs nothing over tier 2, the JVM's tiers are a
      microsecond, and the tier-2 seam over Python is 37% of the own road
      on 100 000 rows — filed as facade-frame-seam.
- [x] Stage 6 — docs (foreign-facade-6, 2026-09-25):
      docs/foreign-facade.md — the shop priced by whoever runs it, the
      data picking the road, what a language cannot do not compiling,
      programs and handles, adding a language — every Scala line pinned
      by `TestDocExamplesForeignFacade` (the JVM examples run in the
      default gate; the snippet map in TestDocSnippets names
      okay-foreign-cluster/src/test as the page's source), a README row,
      and the literature: Truffle's interop protocol (Grimmer et al.,
      DLS 2015), Wadler & Blott's typeclasses, Raasveldt & Mühleisen on
      client protocols, Arrow Flight, Jupyter's kernel messaging, Erlang
      ports, Substrait, hexagonal architecture — each with what this
      design takes from it.

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
   NARROWED to `JvmModule` by specs/foreign-one.md (Decision 8, stage 4,
   proposed 2026-09-25): a Clojure or Frege program as data IS walkable
   (`okay.Foreign`), so `Programs[CljModule]`/`[FregeModule]` are real
   instances; only the Scala-function module has nothing to cross.

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
- **Stage 4b (2026-09-25).** A handle lives in one process, and that
  decided the runtime per language rather than one pool for all:
  `PyWorkers` already routes by handle (foreign-object-handles), so
  Python's `Holds`/`Methods` run there — a second set of processes
  beside `PyPool`'s for a module that uses both, accepted for now and a
  cell for stage 5; R has no such pool, so a module's handles all live
  on one worker. A refinement cannot sit on a `given` (its `{` reads as
  a body), hence the aliases `Holds.Py`, `Holds.R`, `Methods.Py`.
- **Stage 5 (2026-09-25, load 4.9, python3 3.14 without pyarrow,
  100 000 rows of `Rec(Int, Double, String)`).** Tier 1 over Python:
  0.217 ms through the facade against 0.130 ms on the own road — the
  0.09 ms is `PyCodec` encoding and decoding at `Schema`, which the own
  road skipped; the pipe is the rest. Tier 2: 188 ms against 137 ms —
  the 51 ms is the seam, `Rows.table` (rows → Table, ~10 ms) and
  `ArrowFrames.frame` (Table → PyFrame) in, `ArrowFrames.table` and
  `Rows.rows` out; a cell 37% worse than the language's own road, which
  the spec calls a defect: backlog facade-frame-seam names the fix (a
  Table that goes to the wire as itself where Arrow is spoken, and a
  rows→frame road with one conversion where it is not). Tier 3: 194 ms
  for the same rows in 25 frames of 4 096 — streaming costs nothing
  over one frame; the bound is free. The JVM: a call and a by-reference
  frame are 1 µs; rows in and out through the Rows codec 19.8 ms per
  100 000, the price of tier 2 for a language that needs no wire at all.
- **facade-frame-seam (2026-09-25, load 42.7 — a busy box, so the
  numbers are relative).** The first measurement's "own road" was a
  PRE-BUILT PyFrame with no rows on either side, which no caller has;
  the honest own road for a caller with rows is `PyFrame.of(rows)`, the
  frame over, `.rows[B]` back (PyStage's), and it reads 176 ms. `Frames`
  now has `rows` beside `frame` — by default through `frame` and the
  Rows codec, and Python overrides it with exactly that road — so
  `Road.rows` through the facade reads 182 ms: the same road, and the
  37% was the comparison, not the facade. What WAS a seam is the Table
  road: `Frames.frame` built a PyFrame from the Table and the worker
  built a Table from it again on the Arrow road, two conversions of the
  same columns; `ForeignWorker.frameTable` sends a Table as itself where
  Arrow is spoken and converts once where it is not — 152 ms against the
  bare frame's 140 on the JSON road here, and nothing on the Arrow road
  (a pyarrow interpreter's cell). R's twin (`RSubprocess`) is not done
  and the backlog item keeps it.
- Python here: python3 3.14 on the box, echo/boom/missing green over
  pipes, frames `columnar-json` (no pyarrow in the box's interpreter —
  the venv of MeasurePyArrow has it). R: no `Rscript` on the PATH,
  and `TestR.rscript` runs the `okay-r-test` container's through a shim
  — so `TestRFacade` DOES run here (foreign-facade-close found it; the
  line above said it skipped).
- **foreign-facade-close (2026-09-26).** The last two boxes. ONE JOB
  TEXT is `FacadeConformance.job[M: Calls: Frames: Streams]` — a call, a
  300-row frame, the same rows as a stream in frames of 64 — and its
  answer over Python and R equals its answer over the JVM. `Speaks` is
  held to what the worker DOES by OBSERVATION, not by asking the wire a
  second time: a frame the JVM hands back is the same object
  (by-reference); an EMPTY table keeps its columns' kinds on the Arrow
  road and loses them on the columnar JSON road (foreign-facade-2's
  `Nulls(0)` finding, used as the instrument); programs are observed by
  running `pairs` — four answers is multi-shot, the first only
  one-shot, no instance "none" (or "in-jvm" on the JVM). The check fails
  in both directions, and each is a test: the JSON road called Arrow
  and the Arrow road called JSON (offline, `RoadModule` crossing by a
  real IPC write/read and a real `ArrowFrames` round trip), programs
  claimed with no instance, programs a live Python/R HAS reported as
  none. A mutant that skipped the frames comparison turned the offline
  suite red. `stream` has no observation: every report says false, and
  a report saying true fails until foreign-one-mux can show a far side
  driving a stream.

- **The Arrow cells (facade-frame-seam, 2026-09-26).** pyarrow 25.0.1 in a
  uv venv (`OKAY_PYARROW_PYTHON`, or first on PATH), R with arrow 25 in its
  own container (`RArrow.rscript`). MeasureFacade twice per road, the roads
  alternating, load 6.6–9.9: through the facade on Arrow, 100 000 rows 51–54
  ms against 157–170 ms on columnar JSON, a `Table` 11.5–12.5 ms against
  124–130 ms; the facade's rows equal the own road's (53 ms), so the seam
  costs nothing on either road. MeasureRFrame's new lane, arms alternating:
  R, 100 000 rows, Arrow 61.8 ms against 169.5 ms; at 10 000 rows the two
  are equal — the container's fixed cost dominates there.
