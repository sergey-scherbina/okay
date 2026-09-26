# foreign-one — the foreign languages as ONE model

## Overview

The operator's ask, in two steps. 2026-09-25: "посмотри как у нас
сделано взаимодействие с внешними языками — R, питон, rust, haskell,
clojure, frege — подумай как это унифицировать и абстрагировать, чтобы
один и тот же код прозрачно работал с каждым из них, чтобы новый язык
вписывался в эту же систему, и чтобы работало всё — эффекты, коллбеки в
обе стороны, стримы, асинхронность, большие объёмы данных — быстро и
оптимально." 2026-09-26, after the first cut of this spec listed five
gaps: "ты должен взять то, что сделали до тебя все другие, и ещё раз
подумать и всё это обобщить в одну непротиворечивую и стройную систему,
где не будет ничего лишнего и будет всё правильно и самое необходимое и
немного больше для удобства."

So this is not a list of repairs. It is the model the existing work
IMPLIES, written once, with every existing piece mapped onto it: kept
as is, folded into a derived form, or deleted. The pieces it is derived
from, each of which proved a part of it:

- **Programs as data on one wire** (specs/remote-foreign.md,
  specs/polyglot-one-wire.md): `perform`/`continue`/`done` over pipes,
  TCP, FFM and Wasm, one conformance body per link, negotiation by
  givens, supervision with replay. Proved: one engine can serve six
  languages, and the transport is the smallest part.
- **The caller's facade** (specs/foreign-facade.md): a typeclass per
  CAPABILITY, tiers chosen by the DATA, `Schema` and `Table` as the only
  vocabulary, the JVM's zero-cost tier as the test. Proved: the caller
  never needs the language's name in the code.
- **The cluster stages** (specs/foreign-map-reduce.md and its stage 4
  on feature/foreign-streams-holds): a chunk of typed rows is a frame,
  a frame crosses as one message, a pool of interpreters is the
  parallelism, a model is a recipe per interpreter, a stateful stage is
  a leased interpreter. Proved: everything above a call is a
  COMBINATION of call, table, held object and pool — nothing needs a
  new wire operation.
- **The JVM languages** (specs/frege.md, specs/clojure.md,
  specs/interop-shared.md): a foreign program is a tree — an answer, or
  an operation and a function to the rest — walked by `okay.Foreign`
  over a `View`; the continuation is the language's own function, so
  multi-shot is free and a step costs 0.27 µs. Proved: the same node
  protocol, with no wire at all, is the fastest road.

And what those pieces, taken together, do NOT yet have: they are three
engines (`ForeignWorker`, `RSubprocess`, the JVM walker) with two value
enums, two failure types, three pools, eleven typeclasses (seven of them
with a body per language), two program protocols (one-shot dialogue,
multi-shot data), a half-duplex wire, and a language-named API per
language (`Py.*`, `R.*`, `Ts.*`). Each was right when built; together
they are the same idea written several times.

## The model

### Four things cross a language boundary, and a program

Everything any bridge here has ever carried is one of these:

| thing | on the host | on the wire | in the JVM (Clojure, Frege, Scala) |
|---|---|---|---|
| a **value** | `A: Schema` | the value tree (JSON or CBOR, with the escapes: `int` digits, `na`, `dict`, `ref`, `table`, `stream`) | the object itself |
| a **table** | `okay.arrow.Table` | an Arrow IPC part beside the head where the far side speaks Arrow; columnar JSON in the tree where it does not | the same `Table`, by reference |
| an **object** | `Ref[L]` — a handle to something kept on the far side | `{"t":"ref","id":n}`; lives in ONE worker, dies with it | the object itself |
| a **stream** | `Chunks[A]` / `Flow[A]` | `{"t":"stream","id":s}` in the tree, then `chunk`/`end` messages under CREDIT, in either direction | `Chunks` itself |
| a **program** | `Out ! (F + Foreign[L])` | `program`/`continue`/`forget`: a node is `done(v)` or `perform(name, args, k)` | the language's tree, walked |

An ARGUMENT of a call is any of the first four; the ANSWER of a call is
any of the first four. **The tier is per argument, not per call**: a
far function `scale(frame, model)` takes a table and an object;
`step(frame, acc)` a table and a value; `dedup(rows)` a stream and
answers one. This is the facade's "the shape picks the road" (Decision
6 there) carried to where it ends: there is no `Frame` operation and no
`Hold` operation, there is a `call` whose arguments and answer each go
on their own road.

A PROGRAM is the fifth thing, and it is what makes effects and
callbacks work in both directions: a far-side program performs NAMED
operations; the host answers each one as an okay program under the
caller's own handlers (`Reader`, `State`, `Async`, `Choice`, …), and
continues the far program by `k` — once, or as many times as a handler
resumes it. Whether `k` may be continued twice is a CLAIM of the
language: `multi-shot` where continuations are values (Python's
lambdas, R's closures, Haskell, Rust's `Rc<dyn Fn>`, Go's and Rust's
program-as-data forms, Clojure, Frege), `one-shot` where `k` is a
parked stack (the direct style, `okay_call`). Both are the SAME three
messages; the one-shot far side refuses the second `continue` by name.
The `start`/`ask`/`resume` dialogue of foreign-callbacks and the
`program`/`perform`/`continue` protocol of remote-foreign were the same
protocol with two spellings; the model has one.

### The vocabulary

```scala
package okay.foreign

/** a language, as a TYPE TAG: what the compiler keys capabilities on */
sealed trait Lang
sealed trait Py extends Lang; sealed trait R extends Lang; sealed trait Ts extends Lang
sealed trait Hs extends Lang; sealed trait Go extends Lang; sealed trait Rust extends Lang
sealed trait Jvm extends Lang    // Scala, Clojure, Frege, Java: the same process

/** WHERE code of L runs: a placement plus a pool of workers of it */
trait Runtime[L <: Lang]:
  def speaks: Speaks                  // what THESE workers do (from the hello), as against what L can (the markers)
  def call[Out: Ret[L]](address: Address[L])(args: Arg[L]*): Either[Refused, Out]
  def hold[Out: Ret[L]](address: Address[L])(args: Arg[L]*)(using Objects[L]): Either[Refused, Ref[L]]
  def program[Out: Schema, F[+_]](address: Address[L], cbs: Cbs[F])(args: Arg[L]*)(using Programs[L]): Either[Refused, Out] ! (F + Foreign[L])
  def stream[A: Schema, B: Schema](address: Address[L])(in: Chunks[A])(using Streams[L] | Tables[L]): Chunks[B]
  def run[A](prog: A ! Foreign[L]): A   // the whole program on ONE leased worker (its continuations live there)
  def release(ref: Ref[L]): Unit

/** WHAT is called: a namespace of L, optionally shipped as inline source */
final case class Module[L <: Lang](name: String, inline: Option[String])
/** an address in it: a function, or a held object's method or attribute */
enum Address[L <: Lang]:
  case Fn(module: Module[L], name: String)
  case Method(ref: Ref[L], name: String)(using Methods[L])
  case Attr(ref: Ref[L], name: String)(using Methods[L])

/** an argument: built by a given from a Schema value, a Table (needs Tables[L]),
 *  a Ref[L] (needs Objects[L]) or a Chunks[A] (needs Streams[L]) */
opaque type Arg[L <: Lang]
/** an answer type: Schema, Table (Tables[L]), Ref[L] (Objects[L]), Chunks[B] (Streams[L]) */
trait Ret[L <: Lang, Out]

/** ONE refusal on every road (was: okay.py.Condition, okay.r.Condition, Batcher.Failed) */
final case class Refused(kind: String, message: String)
object Refused: val transient: Set[String]   // the wire's kinds, retried on a fresh worker

/** a callback the far side may perform: a name and a program at Schema types (the facade's Cb) */
trait Cb[F[+_]]; final class Cbs[F[+_]](val all: Vector[Cb[F]])

/** the effect a program performs to reach L — one enum, tagged by L, so two languages in one row are two effects */
enum Foreign[L <: Lang, +A]:
  case Call(address, args, held: Boolean) extends Foreign[L, Either[Refused, Value]]
  case Program(run, address, args)        extends Foreign[L, Either[Refused, Node]]
  case Continue(run, k, answer)           extends Foreign[L, Either[Refused, Node]]
  case Forget(run)                        extends Foreign[L, Unit]
  case Release(ref)                       extends Foreign[L, Unit]
```

**Capabilities are compile-time MARKERS on the language tag**, not
typeclasses with a body per language — because there is ONE engine
under every wire language and one walker under every JVM language, so
the code exists once and only the CLAIM differs:

| marker | means L can carry | who has it |
|---|---|---|
| (none: `Runtime[L]` itself) | a value; a program's `call` | every language |
| `Tables[L]` | a `Table` argument or answer | Py, R, Ts, Rust, Go, Hs (the frame road, stage 6: columnar JSON; Rust in process too, not wasm); Jvm by reference |
| `Objects[L]` | a `Ref[L]`: hold, pass, release | Py, R, Jvm; Rust/Go/Hs/Ts when a caller needs their libraries to keep a table of held values (Decision 18) |
| `Methods[L]` | `Address.Method`/`Attr` on a held object | Py, Ts, Jvm — R's and Rust's objects have nothing to call by name, an honest absence |
| `Programs[L]` | a program as data; `Programs.MultiShot[L]` refines it | every language; MultiShot: all but the direct-style-only far sides |
| `Streams[L]` | a stream argument or answer driven by the far side under credit | after stage 5: Go, Rust (not wasm), Ts, Hs, Py; R and wasm-Rust are `mux: false` by design |

The facade's `Speaks(module)` stays as `runtime.speaks`: the marker
says the language CAN, the hello says these workers DO (Arrow or
columnar JSON, `mux` or not, multi-shot or one-shot), and a STRICT given
(`FrameFormat.Arrow.given`) refuses by name where the two disagree.

### Two runtimes, not seven

- **`WireRuntime[L]`** — today's `ForeignWorker` and its `PyWorkers`/
  `SupervisedWorker`/cluster `Pool` folded into one engine over a
  `WireLink` and ONE `Pool`: N workers of one command or connection;
  `use` (one exchange), `lease` (a program, a partition, a dialogue
  keeps its worker), routing by `Ref` (a ref is `generation << 40 |
  local`, so it names its worker and a stale one is refused by name —
  `SupervisedWorker`'s scheme, made the pool's), `perWorker(recipe)` (a
  value materialised once per worker — what `Models` needs),
  supervision (a dead worker is reopened; a program in flight is
  replayed from its journal of answers by `(id, seq)`; a ref dies with
  its worker). Language-specific is only: the command that starts a
  worker (`Language[L].command`), its `Shape` (where the value tree is
  read differently: R's NA, Python's `dict`), and its environment
  (`PyEnv`/`REnv`). Every wire language is `WireRuntime[L]` with a
  `Language[L]`.
- **`JvmRuntime`** — today's `okay.Foreign` walker over a `View[P]`:
  values, tables, objects and streams cross by reference; a program is
  the language's tree; `Member[F]` tests a raw operation. Clojure and
  Frege supply their `View`; a Scala function is a `JvmModule` entry as
  today. Its `Refused` is the caught exception; its `speaks` is
  `by-reference`, `multi-shot`, `in-jvm`.

`Language[L]` is the one object a language adds: how to start or build
a worker (`command`, `build`), its `Shape`, its typed-stub writer
(`ops(cbs)`: today's `Rs.ops`/`Go.ops`/`Hs.ops`/`Ts.ops`, and after
stage 7 `Frege.ops`/`Clj.ops`), its facade writer (`describe` →
a Scala object, today's `PyFacade`/`RFacade`), and its environment
(`Env`). Adding a language is writing this object and its far-side
library against the transcript (below).

### One wire: five operations, two channels

The far side's contract, replacing `call`/`frame`/`start`/`resume`/
`hold`/`method`/`attr`/`program`/`continue`/`forget`/`release`:

```
host -> {"id":n, "op":"call",     "fn":addr, "args":[…], "held":false}      addr: "mod:fn" | {"ref":r,"name":m} | {"ref":r,"attr":a}
host -> {"id":n, "op":"program",  "run":r, "fn":addr, "args":[…]}
host -> {"id":n, "op":"continue", "run":r, "k":k, "answer":v | "condition":{…}}
host -> {"id":n, "op":"forget",   "run":r}
host -> {"id":n, "op":"release",  "ref":r}

far  <- {"id":n, "ok":v} | {"id":n, "condition":{"kind":…,"message":…}}
far  <- {"id":n, "ok":{"done":v}} | {"id":n, "ok":{"perform":name, "args":[…], "k":k}}

either -> {"stream":s, "chunk":<part>} | {"stream":s, "end":true} | {"stream":s, "condition":{…}}
either -> {"stream":s, "credit":c}
```

- **A message is a head and zero or more PARTS.** A `Table` argument
  or answer is a part (an Arrow IPC stream) named from the tree by
  `{"t":"table","part":i}`; a stream's chunk is one part. On a framed
  wire (pipes, TCP after `configure`) the head's frame is followed by
  its parts' frames; in process `okay_exchange` takes and answers a
  vector of buffers, or — over FFM — Arrow C Data pointers (stage 6).
  Where the far side does not speak Arrow, a table is in the tree as
  the columnar JSON of r-frame-columnar-wire. This replaces py-arrow's
  "the whole message is one Arrow stream with the head in its
  metadata", which could carry exactly one table.
- **`held: true`** keeps the answer on the far side and answers a
  `ref` — what `hold`, and `method`'s `hold` flag, were.
- **`perform` is the one callback message.** A direct-style far side
  (a parked thread) and a program-as-data far side send the same line;
  the difference is the claim `programs: one-shot | multi-shot` in the
  hello and the refusal of a second `continue` on the former.
- **Streams are symmetric.** An argument `{"t":"stream","id":s}` is a
  stream the HOST feeds under the far side's credit; an answer with one
  is a stream the FAR SIDE feeds under the host's credit. A stream
  argument and a stream answer in one call is a full-duplex transform
  — the far side holds whatever state it likes inside its own loop,
  which is what `Stateful`'s `open`/`step`/`finish` was spelling from
  outside. Credit is the flow control of reactive streams and HTTP/2:
  the receiver grants `c` chunks, the sender sends at most `c` more,
  and a credit of 0 is a pause. Neither side holds more than the frame.
- **`id` is used.** A reader per link matches answers by id, so a
  worker announcing `mux` has several programs in flight and several
  `perform`s outstanding — that is the far side's ASYNC: a Go or Rust
  function awaiting two okay operations at once, two programs
  interleaved on one process. A far side without `mux` (R, Rust on
  wasip1, an old shim) is served one exchange at a time, byte for byte
  as today. The host is async already: every `perform` is an okay
  program on the caller's scheduler.
- **The hello claims everything**: `{"shim":7, "lang":"python",
  "speaks":{"format":[…], "compress":[…], "tables":["arrow","json"],
  "objects":true, "methods":true, "programs":"multi-shot",
  "stream":true, "mux":true, "describe":true}}`. `Speaks` is read from
  it and nothing else.
- **`Durable` journals five operations once** (`Journalled[Foreign[L]]`,
  one instance), by `(id, seq)`; `SupervisedWorker`'s replay of a
  program is the pool's, keyed the same way; a `Ref` in a journal
  replays as a refusal by name on a fresh worker (values survive,
  handles do not — foreign-object-handles' rule, unchanged).

The protocol is written down as **one golden transcript**,
`specs/foreign-wire.txt`: every conformance case's messages in order,
both sides labelled, replayed against a fake far side in the default
gate (`TestWireTranscript`) and against each shim in its live suite.
The transcript is the specification; the shims are its implementations
— until now the Python shim was the reference by being first.

### The caller's API, and what is "a little more"

The necessary: `Runtime[L]`'s five methods and the markers. The
convenience, each a few lines DERIVED from them, each existing once
for every language:

```scala
// the module addresses its own functions
m / "score"                                        // Address.Fn(m, "score")
ref.method("predict")                              // Address.Method(ref, "predict"), needs Methods[L]

// typed functions and callbacks, as Py.fn / Py.callback were
val score = rt.fn[Double](m / "score")             // (args: Arg[L]*) => Either[Refused, Double]
val priceOf = Cb[String, Double]("price_of")(sku => Reader.ask[Prices].map(_(sku)))

// held objects as a Resource
rt.holding[Model](m / "fit")(params).use(model => rt.call[Table](m / "predict")(table, model))

// streams: a Flow or Chunks through a far function — far-driven where Streams[L], one frame per chunk otherwise
flow.through(rt, m / "dedup", batch = 4096)

// the cluster, as today, ONE body each (were: PyStage/RStage, PyReducer/RReducer, PyStreamer/RStreamer, PyModel/RModel):
flow.mapIn[B](m / "scale")                          // Calls + Tables
Reduce.in[A, Acc](m / "step", m / "merge")          // step(table, acc) + merge(a, b): call with a table and a value
flow.statefulIn[B](m / "open", m / "step", m / "finish")   // Objects + Tables + Pool.lease; or one stream call where Streams[L]
Model.in(m / "fit", params); flow.mapModel[B](model, m / "scale")  // Objects + Tables + Pool.perWorker
Activity.foreign(rt, m / "score")                   // okay-foreign-workflow, unchanged in shape

// stubs and facades from one declaration
Language[Rust].ops("shop", Cbs(priceOf, discount))  // the typed ops a Rust program performs
Language[Py].facade(m)                              // a Scala object from the module's describe
```

`Py`, `R`, `Ts` … keep existing as the language TAGS, and
`okay.py.Py.fn`, `okay.r.R.fn`, `okay.py.Foreign.*` stay as aliases
for one release, so nothing written against them breaks and the docs
can still say "Python" — but they are the same code.

## Today → the model: keep, fold, delete

| today | in the model |
|---|---|
| `WireLink` (pipes, tcp, ffm, wasm), `WireFormat`/`WireCompression`/`WireAuth`/`WireSecurity`/`WireDeadline`/`FrameFormat` givens, `WireNegotiation`, gateway.py | **keep** — the transport layer is right; FFM/wasm gain parts |
| `ForeignWorker` | **fold** → the engine of `WireRuntime[L]`; loses `Frame`/`Start`/`Resume`/`Hold`/`Method`/`Attr` |
| `ForeignEval` (11 ops) | **fold** → `Foreign[L, +A]` (5 ops) |
| `PyStep.Done/Ask` + `Node` | **fold** → one `Node` |
| `PyValue`, `RValue`, `Wire` enc/dec, `Walk` | **fold** → one `Value` tree with the escapes; `Walk` stays (stack safety) |
| `PyCodec`, `RCodec`, `Shape`, `ToPy` | **fold** → one `Codec` at a `Shape[L]`; `ToPy` → `Arg[L]` (it already took Schema values and refs) |
| `okay.py.Condition`, `okay.r.Condition`, `Batcher.Failed` | **fold** → `Refused` |
| `PyWorkers`, cluster `Pool`/`PyPool`/`RPool`, `Holds.pyWorkers`, `SupervisedWorker` | **fold** → one `Pool` (use, lease, route by ref, perWorker, supervise) |
| `RSubprocess`, shim.R's own protocol | **delete** — R is a `Language[R]` on the engine; shim.R speaks the transcript |
| `Py.*`, `R.*`, `Ts.*`, `Foreign.*` typed APIs | **fold** → `Runtime[L]` + the derived helpers; names kept as aliases |
| `PyStream`/`RStream` (chunked stage) | **fold** → `flow.through(rt, address)` |
| `PyEnv`, `REnv` | **keep** as `Language[L].env` |
| `PyFacade`, `RFacade`, `Rs/Go/Hs/Ts.ops`, `Stubs` | **keep** as `Language[L].facade`/`.ops`; Frege/Clj gain `ops` (stage 7) |
| `TsWorker`, `HaskellWorker`, `RustWorker`, `GoWorker`, `ForeignGateway` | **keep** as `Language[L].command`/`.build` |
| facade `Calls`, `Frames`, `Programs`, `Holds`, `Methods`, `Speaks`, `Cb`, `Road` | **fold** → `Runtime[L]` methods + markers; `Cb` stays; `Speaks` → `runtime.speaks`; `Road` is `Arg`/`Ret` |
| facade `Streams.viaFrames` | **fold** → `stream` where `Streams[L]` is absent and `Tables[L]` present |
| `Engine[M]`, `Batcher` | **fold** → `call` with a table (`Batcher` stays as the cluster's chunk seam, built by `Stage`) |
| `Reduces[M]`, `PyReducer`, `RReducer` | **fold** → `Reduce.in`, one body over `call(table, acc)` and `call(a, b)` |
| `Stateful[M]`, `PyStreamer`, `RStreamer`, `Pool.lease` (feature/foreign-streams-holds) | **fold** → `statefulIn`, one body over `hold`/`call(table, ref)`/`lease`; or one `stream` call where `Streams[L]`; `lease` is the pool's |
| `Models[M]`, `PyModel`, `RModel` | **fold** → `Model.in`/`mapModel`, one body over `Pool.perWorker(hold)` + `call(table, ref)` |
| `JvmModule` | **keep** as `Module[Jvm]`'s registry; `Runtime.jvm` |
| `okay.Foreign` walker, `View[P]`, `Member[F]`, `Push` | **keep** — `JvmRuntime` is the walker; `Push` is a stage driver, not a call |
| `okay.frege.Ops`, `okay.clojure.Ops`, `okay.Operations` | **fold** → generated from the core effects' `Cbs` (stage 7); the names stay |
| `Transducers`, `Gather`, `CoreAsync`, `Frege.list/chunks` | **keep, out of this model** — they are bridges of STREAM SHAPES (transducer ↔ stage, gatherer ↔ stage, core.async ↔ channel, lazy list ↔ Chunks), not calls into a language |
| okay-rust kernels (`PasswordHash`, `Digest`), `NativeLib`, `WasmLib` | **keep** — a kernel is an effect of its own; the links are the model's FFM/wasm links |
| okay-js `Direct`/`Emit` (Scala → JS source) | **keep, out of this model** — code generation, not a runtime |
| `ForeignActivity`, `ForeignProc` | **keep** over `Runtime[L].call` |
| `mapPy`, `mapR`, `Reduce.py`, `Reduce.r`, `Stateful.py/r`, `Models.py/r` | **delete** after their derived forms land — the language is the module's type |

Module layout: `okay-foreign` (the model: `Value`, `Arg`/`Ret`,
`Runtime`, `Module`, `Pool`, the wire engine, `Foreign[L]`, `Cb`, the
transcript test) depending on okay-codec, okay-arrow and okay-stream;
`okay-py`, `okay-r` shrink to their `Language[L]` (shim, `Shape`, env,
describe); `okay-rust` keeps FFM/Chicory and its kernels; okay-frege
and okay-clojure keep their `View`s; `okay-foreign-cluster` keeps the
cluster combinators, each one body. `okay.py` stays as an alias
package for one release. The engine's package stops being called
`okay.py`, which it has not been about since foreign-names (2026-09-24).

## Behavior

Held to by the three conformance bodies that already exist —
`WireConformance` (links), `CrashConformance` (SIGKILL), `FacadeConformance`
(capabilities) — plus the transcript. Every box names the body that
holds it.

The model:
- [ ] one job text — a call with a value, a call with a table, a held
      object passed to a call, a stream through, a program with two
      callbacks resumed twice — compiles against `Runtime[L]` and runs
      unchanged on `Py`, `R`, `Rust`, `Go`, `Hs`, `Ts` and `Jvm`,
      changing only the runtime (FacadeConformance; the body is one
      file, the subclasses name a runtime)
- [ ] a thing a language cannot carry is a compile error at the call:
      a `Table` argument to a runtime without `Tables[L]`, a `Method`
      address without `Methods[L]`, a second continue without
      `Programs.MultiShot[L]` (`compileErrors`)
- [ ] `runtime.speaks` agrees with the hello and the conformance suite
      fails when a claim and a test disagree in EITHER direction
- [ ] the transcript replays against the fake far side (default gate)
      and against every shim (live)

The wire:
- [ ] the five operations serve every case the eleven did: every
      existing live suite of okay-py, okay-r, the Go/Rust/Hs/Ts suites
      and okay-foreign-cluster is green on shim 7 with its test bodies
      unchanged except for renamed constructors
- [ ] a call with a table AND an object AND a value as arguments crosses
      as one head and one part, and answers a table (Py with pyarrow,
      Rust over FFM); the same call answers columnar JSON in the tree
      where Arrow is not spoken, equal column for column
- [ ] two programs interleaved on one `mux` worker, each answered
      correctly; a far function with two `perform`s outstanding answered
      under one Reader (Go, Rust, Ts)
- [ ] a far-driven stream of 100 000 rows in chunks of 4 096 arrives in
      order, the far side never more than `credit` chunks ahead — COUNTED
      on the far side, a credit of 0 pausing it; a host-driven stream the
      same in the other direction; a full-duplex transform (a dedup) both
      at once
- [ ] a far side without `mux` is served byte for byte as today (the
      existing suites, unchanged)
- [ ] `Durable` journals by `(id, seq)`: a replay of an interleaving
      answers both programs from the journal; a killed `mux` worker with
      two programs open is replayed, every multi-shot branch back
- [ ] price: the mux reader within noise of today's sequential exchange
      on `WireCodecBench`'s three messages, measured alternating

The pool:
- [ ] `use`, `lease`, route-by-ref, `perWorker`, supervision are one
      class; `Programs.run`, `statefulIn`, `Model.in`, `hold` are its
      callers; a leased worker whose partition fails for ANY reason
      (a far raise, a downstream that stops pulling) is released — the
      leak found on feature/foreign-streams-holds is the test
- [ ] a ref names its worker; a stale ref (its generation gone) is
      refused by name, never re-pointed

The JVM runtime:
- [ ] `Runtime.jvm` passes FacadeConformance by reference (identity
      holds for a Table and an object); Clojure and Frege programs run
      through `program` as data, multi-shot (their `View`s; the tests of
      okay-clojure/okay-frege unchanged)

The languages:
- [ ] `Language[L]` for every L: command/build, Shape, ops, facade, env;
      Frege and Clojure `ops` generated from the core effects' `Cbs`,
      the shipped `okay.frege.Ops`/`okay.core` regenerated with an
      empty diff
- [ ] the frame road in Rust, Haskell, Go; FFM tables as Arrow C Data
      with NO buffer copy (held by the Arrow allocator's byte count)
- [ ] docs/foreign-facade.md "Adding a language" is the four-step
      checklist against the transcript

Measured, in Results, before it is claimed:
- [ ] the table below has a number in every cell a language claims, and
      a cell worse than that language's own road before the model is a
      defect

## Measurements — language × thing × link (to fill)

| language | link | value | table | object (hold + call) | stream (far-driven) | program step | instrument |
|---|---|---|---|---|---|---|---|
| Jvm | in-JVM | 1 µs today | by reference | by reference | by reference | 0.27 µs (Frege) | MeasureFacade, PriceInterop |
| Py | pipes | 0.130 ms today | 100 000 rows 176 ms JSON, ~95 ms Arrow | to measure | viaFrames only until the shim says `stream` | to measure | MeasureFacade, MeasurePyArrow |
| R | pipes | to measure on the one engine | 180 ms JSON today | to measure | viaFrames (mux: false) | to measure | MeasureRFrame |
| Rust | FFM, C Data | to measure | to measure, zero copy | to measure | to measure | to measure | MeasureFacade |
| Rust, Go | pipes / TCP | to measure | to measure | to measure | to measure | to measure | MeasureFacade |
| Ts, Hs | pipes | to measure | to measure | to measure | to measure | to measure | MeasureFacade |

## Stages

Each stage lands alone, keeps every existing suite green, and DELETES
more than it adds (the line count of what it removed goes in Results).

- [x] Stage 0 — this spec (foreign-one, 2026-09-25; rewritten as the
      model by foreign-one-model, 2026-09-26).
- [x] Stage 1 — **foreign-one-r**: ONE ENGINE. `okay.py.WireSession`
      is the language-neutral wire — handshake (shim version, a `fatal`
      the far side names, auth, format/compression/frames), exchange,
      deadline, the Arrow road, a broken or ended wire as DEAD, `verify` —
      and both `ForeignWorker` (Python's values) and `okay.r.RSubprocess`
      (R's) are handlers over it; R's second copy of all of that is gone.
      R thereby reaches TCP, the gateway, `WireAuth` and `WireSecurity`
      (`RSubprocess.connect`, `.over`, `.command`), and a timeout over TCP
      reconnects and replays. Narrowed on the way (Decision 12): R's VALUE
      tree (`RValue`, the `na` escape, `RCodec`) moves to stage 2 and R's
      REPLAY (its `Kont` beside `SupervisedWorker`'s) to stage 3.
- [x] Stage 2a — **foreign-one-value** (2026-09-26): R on the one value
      tree, effect, API and handler. `PyValue` carries R's typed `NA`;
      shim.R v9 speaks the shared tags; frames cross in the COLUMNAR shape
      wherever the hello says so (Python, TypeScript and R do) and a frame
      CARRIES the value rules it is read by; `Shape` owns a language's
      frame rules; the one API takes the caller's `Shape`; `RValue`,
      `REval`, `RStep`, `RNode`, `RFrame`, `ToR`, `okay.r.Condition` are R's
      names over the shared types; `RSubprocess` is a `ForeignWorker`
      (supervised with a deadline), so R's own replay is gone and R
      recovers from a death — stage 3's R half, done here. R is a row of
      `WireConformance` and `CrashConformance`.
- [x] Stage 2b — **foreign-one-program** (2026-09-26): ONE program
      protocol. The direct style (`start`/`ask`/`resume`) folded into
      `program`/`perform`/`continue` in all six far sides: a direct
      function is a program whose `perform` nodes are marked `once` (a
      parked stack). `ForeignEval.Start`/`Resume` and `PyStep` are gone;
      `Program` carries the callbacks offered and the host's `direct`
      intent; `Continue` carries an answer or a failure. The supervisor
      reads `once`; the pool routes both kinds by run. Go and Rust gained a
      plain `call`.
- [x] Stage 2c — **foreign-one-held** (2026-09-26): `hold`, `method` and
      `attr` folded into ONE `call{fn: address, args, held}` on the wire and
      in the effect: `into enum Address` (a function's name, or a held
      object's method or attribute; a `String` converts to a name) and
      `ForeignEval.Call(fn, args, held)`. `ForeignEval` 9 → 6 cases.
- [x] Stage 2d — **foreign-one-protocol** (2026-09-26): `frame` folded
      into `call` on the wire (the table is the first argument, `table: true`
      asks for one back; on the Arrow road the stream IS that argument), so
      the far sides serve five operations — `call`, `program`, `continue`,
      `forget`, `release` — plus the handshake's `configure`/`auth` and
      `verify`. The golden transcript ships in the jar
      (`okay/py/wire.txt`), held by the host and the reference shim.
      Narrowed (Decisions 13, 14): the host keeps `Frame` as a typed case,
      and parts for several tables wait for a caller with two.
- [ ] Stage 2 — **foreign-one-protocol** (as first written): the five operations and
      parts (shim 7 in every shim; `held`, `Address`, `perform` as the one
      callback message, tables as parts); the transcript written and
      replayed; `Foreign[L, +A]`; `Refused`; `Value` — which ABSORBS
      `RValue` — DONE by stage 2a (foreign-one-value).
- [x] Stage 3 — **foreign-one-pool** (2026-09-26): ONE pool. `okay.py.Pool`
      (+ `Pools`, moved from the cluster) is the pool; `PyWorkers` is its
      routing layer (by ref, by run; a `once` program keeps its lease);
      the cluster's `PyPool`/`RPool` are `PyWorkers` over it (R's workers
      are `ForeignWorker`s since 2a); the facade's second Python pool and
      R's holder-of-one are gone. `perWorker` stayed in `Models` (Decision
      15). A stateful stage gives its worker back on every path.
- [ ] Stage 3 (as first written) — **foreign-one-pool**: one `Pool` (use, lease, route by
      ref, perWorker, supervise); `PyWorkers`, the cluster pools,
      `Holds.pyWorkers` and `SupervisedWorker` folded (R's own replay is
      already gone, stage 2a); the lease-leak test.
- [x] Stage 4 — **foreign-one-runtime** (2026-09-26, narrowed — Decision
      16): `Language[M]` (pool, address, value rules) makes every cluster
      stage kind and every facade capability ONE body; the Python/R twins
      (`PyStage`/`RStage`, `PyReducer`/`RReducer`, `PyStreamer`/
      `RStreamer`, `PyModel`/`RModel`, and each capability's two instances)
      are deleted. The facade's typeclasses stay the capability claims.
      Filed, not built: module types for the other wire languages
      (foreign-more-languages), Clojure/Frege programs in the facade
      (foreign-jvm-programs), the package name (foreign-package-name).
- [ ] Stage 4 (as first written) — **foreign-one-runtime**: `Runtime[L]`, `Module[L]`,
      `Arg`/`Ret`, the markers, `Language[L]`; the facade's typeclasses
      and the cluster's per-language stages become the derived
      combinators (one body each); `okay-foreign` as the module,
      `okay.py` an alias. Subsumes the earlier foreign-one-modules.
- [x] Stage 5 — **foreign-one-mux** (2026-09-26, narrowed — Decision 17):
      far-side SOURCES, derived from the five operations — an iterator held
      on the far side (`call … held`), one call per chunk, `release` at its
      end — `Py.source` (a generator; `StopIteration` its end) and
      `R.source` (a closure; NULL its end), pulled at the consumer's pace.
      No wire change. Duplex multiplexing with credits is filed
      (foreign-mux-duplex) for the caller that needs two requests in
      flight on one worker.
- [ ] Stage 5 (as first written) — **foreign-one-mux**: `id` matched by a reader; streams
      both ways under credit; `okay_poll`; journal by `(id, seq)`. Go and
      Rust first, then Ts and Hs; Python's shim may follow if a threaded
      shim is measured to beat a pool under the GIL.
- [x] Stage 6 — **foreign-one-bulk** (2026-09-26, narrowed — Decision 18):
      the TABLE call in Go, Rust and Haskell over the wire's own columnar
      frame, claimed in each library's hello — Go `okay.Frame`, Rust
      `Value::Table`, Haskell `VTable`, each with a `col(name)` — and in
      Haskell, `call` itself: a named program that answers without
      performing. Every wire language answers the one conformance body's
      table case. Held objects in those libraries and Arrow C Data over
      FFM are filed (foreign-held-values, foreign-arrow-ffm).
- [ ] Stage 6 (as first written) — **foreign-one-bulk**: the frame road in Rust, Hs, Go
      (subsumes foreign-frame-op-rust-hs-go); `Objects` in their
      libraries; Arrow C Data over FFM; the table filled.
- [ ] Stage 7 — **foreign-one-ops** (narrowed — Decision 19): the
      caller's `Foreign.Callbacks` serve Frege and Clojure programs as
      they serve every wire language — the JVM walker performs a
      `Foreign.Call(name, arg)` against them (`calls.jvm`), and
      `Jvm.frege(module, cbs)` / `Jvm.clojure(ns, cbs)` write the typed
      Frege module and the Clojure namespace, as `Hs.ops` writes Haskell's.
      The core effects keep `okay.frege.Ops`/`okay.clojure.Ops`: they are
      the row's own operations, not callbacks.
- [ ] Stage 7 (as first written) — **foreign-one-ops**: `Language[L].ops` for Frege and
      Clojure from the core effects' `Cbs`; the hand-written `Ops`
      become generated output.
- [ ] Stage 8 — **foreign-one-docs**: one entry page, "Foreign
      languages", with the five things, the two runtimes, the markers,
      one program in every language and the same Scala over every
      runtime; the existing pages become its per-language chapters;
      every Scala line pinned.

Order: 1 first (pure deletion; everything after has one engine to
change); 2 and 3 are the protocol and the pool, each a mechanical
collapse with the existing suites as the net; 4 is where the caller's
API becomes one and the cluster combinators lose their per-language
bodies — it waits for foreign-reduce and foreign-streams-holds to land,
and changes their implementation, not their API; 5 is the one NEW
capability and is spec-gated by the Decisions below; 6 after 5 so
streams of tables take the zero-copy road from the first; 7 and 8 close.

## Out of scope

- A cross-language call that does not pass through Scala.
- mTLS, WebSocket, an authenticated in-process link.
- The stream-shape bridges (transducers, gatherers, core.async, lazy
  lists) — closed arcs of their own, not calls.
- GraalPy/Jython as a JVM Python (backlog py-graalpy-engine): a
  `JvmRuntime` with a `View` over Python generators, if ever; nothing
  here prevents it and nothing here needs it.

## Decisions

1. **Four things and a program, not tiers per call.** The facade's
   tiers were the right idea one level too high: a call has arguments
   and an answer, and EACH is a value, a table, an object or a stream.
   `frame`, `hold`, `method`, `attr` were calls with a particular
   argument or answer; naming them separately made eleven operations of
   five and a typeclass of each.
2. **Capabilities are markers on a language tag, not instances with
   bodies.** foreign-facade's typeclasses had a body per language
   because there were three engines. With one wire engine and one JVM
   walker the body exists twice at most, and what differs per language
   is a CLAIM — which is what a marker is. The compile error the facade
   promised is kept exactly.
3. **One program protocol; one-shot is a claim, not a second protocol.**
   `start`/`ask`/`resume` and `program`/`perform`/`continue` carried
   the same three messages. The far side that cannot continue twice
   says so in the hello and refuses the second continue by name; the
   host has one loop and `Durable` one journal.
4. **A message is a head and parts.** One Arrow stream with the head
   in its metadata (py-arrow) carried one table. Parts carry any number
   of tables and stream chunks, on framed wires and in process alike,
   and leave the tree's shape untouched for a far side without Arrow.
5. **Credits, symmetric, on the existing ids** — not a connection per
   stream (Arrow Flight, gRPC), which pipes, FFM and wasm do not have,
   and not an ack per chunk, which is the pull road renamed. The
   receiver grants, the sender runs ahead by a bound, the frame is the
   memory bound on both sides.
6. **One pool, and refs name their worker.** Three pools existed
   because three things needed an interpreter for longer than one
   exchange (a dialogue, a partition, a held object). A `lease` is that
   one thing; routing by a generation-tagged ref is what
   `SupervisedWorker` already did for continuations. `perWorker` is
   `Models`' WeakHashMap made the pool's.
7. **R is deleted into the engine, not bridged.** A bridge would keep
   `RValue`/`RCodec`, where R's own defects lived (`toInt`, 15 digits).
8. **The JVM runtime is the walker, and it IS a `Runtime[L]`.** If
   Clojure and Frege could not pass FacadeConformance by reference
   through the same interface, the interface would be a wire in
   disguise (foreign-facade Decision 4, kept). foreign-facade's
   Decision 7 (no JVM `Programs`) narrows to the Scala-function module:
   a Clojure `(step op k)` is walkable and is a program.
9. **The transcript is the specification.** A shim's conformance is a
   replay, not a reading of shim.py.
10. **Stream-shape bridges stay outside.** A transducer is not a call
    into Clojure; it is the same stage in another notation. Putting it
    under `Runtime[L]` would make the model say something false.
11. **What is "a little more for convenience"** is exactly the derived
    layer: `fn`, `holding`, `through`, `mapIn`, `Reduce.in`,
    `statefulIn`, `Model.in`, `Activity.foreign`, `ops`, `facade` —
    each a few lines over the five methods, each written once for every
    language, each removable without touching the model.

12. **Stage 1 is the ENGINE, not the values** (foreign-one-r,
    2026-09-26). The first text of stage 1 also deleted `RValue` and
    `RCodec`. Measured against the code, that is a change to `PyValue` —
    a new case (R's typed NA) in an enum every codec, walk and facade
    matches on — in the same lane as the engine, two risks behind one
    gate, and exactly what stage 2's "one `Value` tree" is. So stage 1
    folds what is identical today (the transport and everything on it)
    and leaves what is different today (the value tree, and the replay
    that is keyed on it) to the stages that own them. Nothing was
    bridged: R's handler speaks `Json` to the same session Python's does.

13. **`Frame` stays a host case; on the wire it is a `call`**
    (foreign-one-protocol). Measured: 21 files use it, every caller has
    exactly one table and it comes first, and its answer is TYPED (a frame
    read by the rules its request was made under). Folding it into `Call`
    on the host would buy no call anyone makes, at ~30 rewritten sites and
    a lost result type; the wire — what a new language implements — is
    where "one call" pays, and there it is one.
14. **Parts for several tables wait for a caller with two.** A table is a
    value in the tree (`{"t":"frame",...}`) on the JSON road already, so a
    call may carry any number of them there; the Arrow road carries one
    (the stream is the call's first argument). A framing of N parts is
    written when a call needs two Arrow tables — until then it would be
    code no test can exercise.

15. **No `perWorker` in the pool** (foreign-one-pool). The model named it
    for `Models`' cache of one held model per interpreter; measured, it is
    five lines in `Models` with one caller, and a generic per-worker store
    in the pool would need a heterogeneous map and a cast for no second
    user. It moves into the pool when a second caller needs it.

16. **The capability typeclasses ARE the markers** (foreign-one-runtime).
    The model named `Runtime[L]` with marker types because the facade's
    instances each had a body per language. With one engine, pool and
    protocol under them, the bodies fold into `of(lang: Language[M])` and
    the instances that remain are exactly claims — present or absent,
    absent being a compile error at the call, which is what a marker was
    for. A second API beside them would be the duplication this spec
    exists to remove. Module types for TypeScript, Haskell, Go and Rust
    are one `Language` each, written when a cluster or facade caller needs
    one (their table road is stage 6 first).

17. **Sources before multiplexing** (foreign-one-mux). The stage bundled
    two things. A far-side STREAM (a generator, a cursor) needs no new
    operation: it is a held iterator and a call per chunk, pulled — the
    back-pressure is the pull, and the memory bound is one chunk each side.
    CREDITS pay only where the far side may run ahead, which needs a duplex
    wire with several requests in flight on one worker; no caller has that
    need (the pool gives parallelism across workers; Python and R are
    single-threaded by design), and the journal, the supervisor's replay and
    six far sides would all change for it. So the source is derived now, and
    the duplex wire waits for its first caller (foreign-mux-duplex).

18. **The table road is the columnar JSON the wire already has; held
    values and C Data wait for a caller** (foreign-one-bulk). The stage
    bundled three things. The TABLE call is what blocked a cluster stage in
    Go, Rust or Haskell, so it is built: each library reads the columnar
    frame r-frame-columnar-wire defined, claims it in its hello (a claim the
    host already honours), and answers a table in v1 cells, which the host
    reads beside columnar. Haskell had no `call`; it gets one without a
    second registry — a call is a named program that answers without
    performing, and one that performs is refused by name. HELD VALUES in
    these libraries have no caller: none of their far-side values is an
    object with state a caller holds across calls (Decision 16's rule).
    ARROW C DATA over FFM wants the `arrow` crate linked into the test
    library and a caller moving tables big enough that the JSON copy shows
    in a measurement; neither the crate (offline: not in the cargo
    registry, nor Go's module in GOMODCACHE) nor the caller exists. Both
    are filed with their gates.

19. **Callbacks are the one declaration; the core effects are not
    callbacks** (foreign-one-ops). The stage said the hand-written
    `okay.frege.Ops` and `okay.core` would become generated from the core
    effects' `Cbs`. Read against the code, there are no such `Cbs`:
    `okay.core` holds no operations at all (it is the program monad), and
    the two `Ops` objects bind `okay.Operations` — the row's OWN effects,
    which a JVM program performs directly and a wire program cannot. What
    WAS declared twice is a caller's own operation: a Scala
    `Foreign.callback` served Python, TypeScript, Go, Rust, Haskell and R,
    while a Frege or Clojure program needed a hand-written static method
    per operation. So the callback becomes the declaration everywhere:
    the JVM walker resolves a `Foreign.Call(name, arg)` against the
    caller's callbacks, the callback's own Schema decodes the argument (a
    wrong one is refused by name, no cast), and the Frege module the
    generator writes types each operation from the callback's Schemas, so
    a wrong argument there is a Frege type error.

## Results

- Stage 0 (2026-09-25/26): the spec; the first cut's gap list is
  subsumed by the model above.
- **Stage 1, foreign-one-r (2026-09-26).**
  - `okay.py.WireSession` (265 lines, half of them the comments the two
    copies each carried) is the one engine; `ForeignWorker` went
    410 → 237 lines and `RSubprocess` 507 → 348, the rest of both being
    the handler for their own values. Net −67 lines of main code; what
    was removed is the SECOND COPY — R's framing reads, handshake,
    negotiation, Arrow exchange, death detection and verify, ~330 lines.
  - Generic on the way, because R needed them and nothing is lost for
    the others: a hello's `fatal` refused by name, jsonlite's boxed
    scalars read in the hello and in `verify`, a condition message sent
    as several lines joined.
  - R gains the network: `RSubprocess.connect(host, port)` under the
    same `WireAuth`/`WireSecurity` givens as any language,
    `RSubprocess.command(...)` for the gateway, `RSubprocess.over(link)`;
    a timeout over TCP reconnects (the gateway starts a fresh R per
    connection) and replays a program as data onto it.
  - Tests. Live, R 4.4.1 in docker: okay-r 90 passed + 1 skipped (R on
    PATH only), among them `TestRNetwork` (4, new: the gateway with
    `json/zlib` and a multi-shot program; a secret with a Reader
    callback; a wrong secret and none refused by name; a TCP timeout
    reconnected and every branch of a `Choice` replayed). okay-py Live
    226 passed (Python, TypeScript, Go, Rust, Haskell over pipes, TCP,
    CBOR, auth, TLS, gateway, crash; 27 skips by design); okay-rust's
    FFM and wasm link suites 24 passed. Nothing in either suite changed.
  - Found by the first R run: a refusal named the far side by the
    Rscript PATH instead of "the R shim", which `TestRWireCborPlain` pins;
    the pipes session is named as R named itself.
  - Mutant: the session's `fatal` check removed fails "a shim without
    jsonlite refuses BY NAME" — the refusal degrades to a bare version
    drift.
  - Found beside it: an unused import in okay-foreign-cluster's
    `Stateful.scala`, landed by foreign-streams-holds and visible only to
    a cold compile — removed.
- **Stage 2a, foreign-one-value (2026-09-26).**
  - okay-r's main code 1 869 → 1 220 lines (R's value enum, effect, journal
    instance, wire codec, API copy, engine handler and replay deleted);
    main code across okay-py, okay-r and okay-foreign-cluster −376 net.
    What R keeps is R's: its names for the tree (`RValue`), its value
    rules (`RCodec` = `R.shape`, with the frame rules), its handle type,
    its Arrow rules, its shim.
  - The design found two things the earlier code got wrong without a test
    that could see them: the shared API (`Py.fn`/`program`/`hold`/
    `callback`) fixed Python's `Shape` INSIDE `object Py`, so no caller
    could give another — R's conformance row failed with "expected a str,
    got a list of 1" until each took the caller's `Shape`; and a frame
    read by whatever `Shape` was in scope would read an R frame by
    Python's rules in the cluster's R stages — so a frame now CARRIES its
    rules (`PyFrame.shape`, outside equality), tagged by the worker that
    answered it, by `RFrame.of`, and on replay by its request.
  - The columnar frame (R's v2) is the frame on the wire wherever the far
    side's hello claims it (`"frames": ["columnar"]`: Python, TypeScript,
    R); a far side that does not (an old shim) gets v1, and both are read.
  - R's shim v9 speaks the shared tags; the shared decoder still reads R's
    pre-v9 tags (`i`, `raw`, `named`), which `TestRMock`'s v1-frame fixture
    holds.
  - Tests. Live, R 4.4.1 in docker: okay-r 98 passed + 2 skipped, among
    them the new `TestRConformance` (the one `WireConformance` body at
    `R.shape`: multi-shot, callbacks under a Reader, direct style, a
    failure by name) and `TestRCrash` (the one `CrashConformance`: SIGKILL
    between choices, idle, mid-`okay_call` — R now recovers from a DEATH).
    okay-py Live 226, okay-foreign-cluster Live 26 (`TestRFacade` for the
    first time: it found the facade's five R instances addressing R as
    `module:fn`, which R cannot resolve — fixed to `module::fn`),
    okay-foreign-workflow Live 12. Default gate: `TestRFrameRules` (2, new).
  - Mutant: the worker's tagging of an answered frame removed. It SURVIVED
    every existing suite — they compare cells, not rules — which is why
    `TestRFrameRules` exists; with it, the mutant fails.
  - Durable journals of R programs written before v9 no longer replay:
    their fingerprints hashed R's old tags, so the journal's drift check
    refuses them by name rather than answering from them.
- **Stage 2b, foreign-one-program (2026-09-26).**
  - Wire: `start`, `ask` and `resume` are gone from all six far sides
    (Python and TypeScript shim 7, R shim 10, the Go, Rust and Haskell
    libraries 7). A direct function's `okay_call` answers the SAME node a
    program as data answers, `{"perform", "args", "k", "once": true}`, and
    is continued by the same `continue{run, k, answer | condition}`; a
    second continue of a `once` k is refused by name. A function that
    returns a plain value is a program already `done` (it was a refusal
    under two protocols; remote-foreign's box says so now).
  - Host: `ForeignEval` 11 → 9 cases; `PyStep` and the journal's step
    codec deleted; `Fn.calling`, `PyRun`, the pool, the supervisor and the
    workflow activity all walk one node type. The pool keeps a worker out
    only while a `once` node is open (a parked frame would otherwise have
    other calls nested inside it) — decided by the far side's node, not by
    which op the host happened to send. `Program.direct` is the HOST's
    intent, used for one thing: a start that died is not re-run.
  - Go and Rust serve a plain `call` of a direct function (they served
    only `start` and programs before).
  - Tests: every Live suite of okay-py (226), okay-r (98 + callbacks 6),
    okay-rust FFM/wasm (35), okay-foreign-cluster (26) and
    okay-foreign-workflow (12) green; the only test edits are constructors,
    the journal's op names (`program:…`, `continue`) and the plain-return
    case above. Default gate 137.
  - Mutant: the supervisor ignoring `once`. `TestCrashPython` and
    `TestSupervised` fail — and the failure is the reason the flag exists:
    the supervisor re-ran the killed direct function and ANSWERED
    (`Right(6.0)`), silently repeating whatever it did before it died.
  - Durable journals written before this lane that contain a direct
    dialogue (`start`/`resume`) are refused by the drift check.
- **Stage 2c, foreign-one-held (2026-09-26).**
  - The effect has six cases now: `Call`, `Frame` (until the parts
    stage), `Program`, `Continue`, `Forget`, `Release`. `Address` is the
    one place a `String` converts (Scala 3.9 `into`, the rule of
    into-modifier-rule: a target of ours, and nothing else a string could
    mean here), so `REval.Call("stats::median", args)` reads unchanged.
    The journal names are unchanged (`hold:fn`, `method:m`, `attr:a`).
  - Wire: Python and TypeScript shim 8, R shim 11 (an address that is not
    a name is refused by name — R applies functions to objects), the Go,
    Rust and Haskell libraries 8 (the shared version; they hold nothing).
  - Live: okay-py 226, okay-r 98, okay-rust FFM/wasm 35,
    okay-foreign-cluster 30, okay-foreign-workflow 12 — green on the first
    run; tests changed only in constructors and patterns.
  - Mutant: the pool not registering a held answer pool-wide fails both
    pool-of-handles tests (a pool of one refuses its own ref; a pool of
    two gives two objects one id).
- **Stage 2d, foreign-one-protocol (2026-09-26).**
  - The far sides' operations: `call` (a name, a held object's method or
    attribute; `held`; `table`), `program`, `continue`, `forget`,
    `release`, and the handshake's `configure`/`auth` and `verify`.
    Python shim 9, TypeScript 9 — which never served a table before and
    does now — R 12, the Go, Rust and Haskell libraries 9.
  - `okay/py/wire.txt`: the protocol as 14 request/answer steps with the
    module that answers them. `TestWireTranscript` (default gate) holds the
    host to its requests; `TestWireTranscriptPython` (Live) holds the
    reference shim to its answers. Both passed on the first run.
  - Mutant: the host spelling the table flag `tabl` fails the default-gate
    transcript test at step 5.
- **Stage 3, foreign-one-pool (2026-09-26).**
  - Three pools became one: `okay.py.Pool` (FIFO over idle interpreters,
    so load goes round; `prime` for an eager start), `PyWorkers` as its
    routing layer, and the cluster's `Workers` registry giving one
    `PyWorkers` per (language, interpreter, module) — stages, reduces,
    models, stateful stages, the facade's handles, methods and programs,
    Python's and R's, all through it. A dead worker's refs and runs die
    with it; the pool closes it and opens a fresh one on demand.
  - The lease leak (found on feature/foreign-streams-holds): a failing
    step threw out of a partition's iterator with its interpreter still
    leased. `Streamer.abandon` gives the state back; `TestStatefulLease`
    (default gate, a counting streamer) was RED before the fix — the
    failed partition kept its worker — and green after.
  - Live: okay-py 227, okay-r 98, okay-foreign-cluster 30,
    okay-foreign-workflow 12; default 143.
  - Open, and said: a partition whose DOWNSTREAM stops pulling early never
    reaches `finish` or a failure, so its state is not given back until
    the JVM ends; `Chunks` has no close signal to hang it on (backlog
    stateful-early-stop).
- **Stage 4, foreign-one-runtime (2026-09-26).**
  - `Language[M]`: what a wire language is to the cluster and the facade —
    its name and tag, its one pool, how it addresses a function, its value
    rules. `ForeignStage`, `ForeignReducer`, `ForeignStreamer`,
    `ForeignModel` and the facade's `Calls`, `Frames`, `Programs`,
    `Holds`, `Speaks` are one body each over it; the named givens and the
    `py(path)`/`r(path)` factories stay, so no caller changed.
    okay-foreign-cluster: 11 files, +330/−480.
  - Found on the way: the facade's R table road converted twice (Table →
    R frame → Table); the one body sends a Table as itself where Arrow is
    spoken and converts once by the WORKER's rules where it is not — the R
    half of backlog facade-frame-seam, closed.
  - Live: okay-foreign-cluster 30 (every facade capability and every
    cluster stage over python3 and R), okay-foreign-workflow 12.
  - Mutant: R's `Language` addressing with Python's `:` fails 8 tests of
    `TestRFacade` and `TestRMapReduce`.
- **Stage 5, foreign-one-mux (2026-09-26, narrowed by Decision 17).**
  - `PyStream.pulled`: a source over any held iterator and a per-chunk
    call; `Py.source` and `R.source` on it. Nothing new on the wire.
  - Tests (Live): `TestPySource` — every element in order with one call per
    chunk and a release at the end; BACK-PRESSURE: a consumer taking four of
    ten asks the far side for two chunks of three; a generator's failure
    ends the source by name and releases it. `TestRSource` — an R closure,
    every element in order.
  - Mutant: reading the whole source before telling it (read-ahead) fails
    the order test and the back-pressure test.
  - Open: a consumer that stops early leaves the iterator held until its
    worker ends — the same missing close signal as stateful-early-stop,
    which now names sources too.
- **Stage 6, foreign-one-bulk (2026-09-26, narrowed by Decision 18).**
  - Go `okay.Frame`, Rust `Value::Table`, Haskell `VTable`: each reads the
    columnar frame it claims in its hello and answers a table in v1 cells;
    Haskell serves `call` from its named programs (a program that performs
    is refused by name).
  - Tests (Live): WireConformance's table case (`scale`) over 27 rows —
    Python ×4, TypeScript ×3, R, Go ×7 (pipes, TCP, CBOR, auth, TLS, wasm),
    Rust ×7 (pipes, TCP, CBOR, auth, TLS, FFM), Haskell ×3 (pipes, CBOR,
    gateway). Skipped: Rust on wasm (no direct functions). okay-py 253,
    okay-r 100, okay-rust 39, all green.
  - Mutant: Go reading a columnar column's values under the wrong key fails
    TestGoPipes' table case.
  - Open: answers from Go, Rust, Haskell and TypeScript are v1 cells, not
    columnar; worth changing when a measurement shows the answer's codec.
