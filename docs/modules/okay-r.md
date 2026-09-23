# okay-r

R as a handler (specs/r.md; the twin of [`okay-py`](okay-py.md), and
the model is stated once for both): call-shaped foreign compute.
Calls are OPERATIONS — mockable by swapping the handler, supervised
by dead-process-throws. Named functions only:
the enum has no eval-a-string case, structurally, so untrusted input
reaches R only as data.

| | |
|---|---|
| `REval` | `Call(fn, args)` answering an `RValue`, `Frame(fn, in, args)` answering an `RFrame` — the two shapes a statistical function takes |
| `RValue` | R's edge values, the `SqlValue` move told again: `RNull` (the absence of an OBJECT) and `NA(of)` (a missing value INSIDE a vector, typed) are DISTINCT, plus logical, int, double, string, raw bytes and vectors of those |
| `RType` | `Logical`, `Integer`, `Double`, `Character` — what an `NA` is missing FROM |
| `RFrame` | a data.frame as columns of primitives, with `rows[A: Schema]` and `RFrame.of[A: Schema]` mapping a flat case class to and from it — the same move `Typed.rows` makes over a SQL row |
| `Condition` | R's own failure vocabulary as data (`kind`, `message`) — a failing call is a value and the process survives |
| `RSubprocess` | stage 0: one `Rscript` per session running the shim SHIPPED WITH THIS MODULE (a versioned resource whose handshake refuses drift loudly), with a CLEAN environment — the parent leaks nothing into R unless the config names it. `start` takes an optional deadline and an optional set of REQUIRED packages |

## Using it

```scala
import okay.r.*, RValue.*

// one session; the deadline and the required packages are both optional
val r = RSubprocess.start(
  rscript = "Rscript",
  timeoutMillis = Some(30000L),          // a call that hangs is killed and answered
  require = Map("stats" -> "4.4"))       // verified HERE; a drift refuses by name

// a call: the function is ADDRESSED, the arguments are values
r.handler.handle(REval.Call("stats::median", Vector(Vec(Vector(F64(3), F64(1), F64(2))))))
// Right(Vec(Vector(F64(2))))

// a frame in, a frame out — and the case class on both sides
final case class Obs(site: String, temp: Double, n: Int) derives okay.codec.Schema
val frame = RFrame.of(Vector(Obs("kyiv", 21.5, 3))).fold(c => sys.error(c.message), identity)
r.handler.handle(REval.Frame("summary", frame, Vector.empty)) match
  case Right(back) => back.rows[Obs]      // Either[Condition, Vector[Obs]]
  case Left(cond)  => cond                // R's own failure, as data

r.close()
```

Everything a call can answer is a value: `Either[Condition, …]`. The
three ways it can go wrong are three different values, not three
exceptions — R raised a condition (`stop()`, a missing package, a
missing function), the call hit its deadline (`Condition("timeout",
…)`), or the answer did not fit the shape asked for. Only a DEAD
process throws, because a supervisor and not the program decides what
to do about it.

## The guarantees, and what makes each one true

- **No R source, ever.** `REval` has two cases and neither takes code:
  a function is named, arguments are `RValue`s. Untrusted input cannot
  become a program because there is nowhere to put one.
- **A failing call is data and the session survives.** The shim
  catches R's conditions and answers them; the process is not
  restarted and the next call goes through the same one.
- **A hanging call has a deadline, if you set one.** R sits in C
  during `Sys.sleep`, an optimiser or a regex, and no polite protocol
  reaches it — so the deadline kills the PROCESS and starts a fresh
  one, and the call answers `Condition("timeout", …)`. The respawn is
  invisible to the program, and that follows from the no-source rule:
  the API cannot assign anything in an R session, so a fresh process
  has nothing to have lost. The one edge: if the respawn itself fails
  (R gone between two calls), that throws — an engine whose
  interpreter no longer exists is not something a program can handle
  as a value.
- **The environment is what the config names.** `--vanilla` plus an
  emptied environment: no site file, no profile, no saved workspace,
  no inherited variables.
- **The shim cannot drift.** It ships as a resource in this module's
  jar and announces its version at the handshake; a mismatch refuses
  by name rather than guessing.
- **Packages are checked before the engine is handed over**, when
  `require` names them — the verify-at-startup posture the SQL seam
  takes, for the same reason: an analyst's environment drifts, and the
  alternative to a loud refusal is a wrong number later.

## The wire, and what it costs

One JSON object per line each way, through jsonlite. Scalars are
tagged per value, because JSON alone cannot keep R's integer apart
from its double, its four NAs apart from each other, or NA apart from
NaN. A FRAME is columnar (format v2): the type belongs to the column,
the values are a plain array, and the absences are index lists beside
them.

That shape is not a preference, it is a measurement. With a tag per
CELL, a 100 000-row three-column frame took 10.5 seconds through
`identity`, of which our own encode and decode were 0.4% — jsonlite
was building hundreds of thousands of small objects. With the tag on
the column:

| rows | payload | round trip | our share |
|---|---|---|---|
| 10 000 | 0.17 MB | 20.9 ms | 42% |
| 100 000 | 1.88 MB | 179.7 ms | 26% |

58x at 100 000 rows, and the balance flipped: R held 99.6% of the
trip before and under 75% now. `MeasureRFrame` is the harness, and it
is the reason `r-arrow` is still filed rather than built — twice now,
a measurement has sent it back.

## The limits, stated

- **No supervisor.** A dead process throws and the CALLER decides
  whether to retry; the module has no policy of its own and does not
  want one.
- **One session is one process.** Concurrent callers need a session
  each; there is no pool.
- **An empty column loses its type.** `RFrame` types VALUES, not
  columns, so an empty column carries no type on our side — the wire
  can express it, our type cannot.
- **Not an aggregation engine.** A frame that comes back is an
  ordinary `Vector[A]` after `rows`, and the algebra
  (`Aggregator`, `Bulk`) folds it here; nothing of ours runs inside R.

## The two absences, which is where the twin is not a copy

Python has one absence (`None`) and one not-a-number (`NaN`). R has
three things where a naive bridge sees one: `NULL` is the absence of
an object, `NA` is a missing value inside a vector AND carries the
type it is missing from, and `NaN` is a double that happens not to be
a number. Collapsing any two of them silently changes what a
statistical function computes, so `RValue` keeps them apart at the
type level and the shim carries the distinction across the wire.

## Typed calls

`R.fn` is okay-py's `Py.fn` in R. A case class goes to R as a NAMED LIST
and comes back from one:

```scala
val patched = R.fn[Order]("utils::modifyList")(Order("kyiv-7", 2, 1.5), Patch(5)).runWith
assertEquals(patched, Right(Order("kyiv-7", 5, 1.5)))
```

R lacks two things Scala has, and the codec says what crosses instead:

- **Scalars.** Every R value is a vector, so the decoder takes a scalar
  from a length-1 vector:

  ```scala
  assertEquals(R.fn[Double]("stats::median")(Vector(3.0, 1.0, 2.0)).runWith, Right(2.0))
  ```

- **A 64-bit integer.** A `Long` is sent as an R integer when it fits 32
  bits, as a double while the double is exact, and as its digits beyond
  that:

  ```scala
  assertEquals(R.fn[Long]("base::identity")(Long.MaxValue).runWith, Right(Long.MaxValue))
  ```

The frame codec used to truncate a `Long` to 32 bits without any error.
It now uses the same rule.

A decode failure names the path in R's own notation (`$sku`, `[2]`).
Wire v3 (shim 3) added the named-list record. Before it, a named list
that was not a data.frame was sent as a frame.

## Journalled by Durable

`REval` carries its own `Journalled` instance, as okay-py's `PyEval`
does, so okay-agent's `Durable` journals an R call and answers it from
the journal on replay without R:

```scala
val replay = Durable.replayingOver[REval](j)
assertEquals(replay.handle(REval.Call("stats::median", xs)), Right(Vec(Vector(F64(2.0)))))
assertEquals(ran.get, 2, "replay touches no R")
```

The answer is written in the module's wire JSON, so NULL, a typed NA
and NaN come back distinct. The fingerprint is the function plus a
SHA-256 of what it was asked, never the frame it answers, so a
million-row answer does not make the fingerprint large, and drifted
inputs are refused.

## What it is not

Not R-on-the-JVM. The subprocess boundary buys the real interpreter,
every CRAN package, and crash isolation; the spec argues that choice
once for both twins. Not storage, and not an aggregation engine —
specs/data.md's heavy-compute row has Spark and Flink carrying the
MERGE contract and R carrying the CALL contract. A pipeline that
folds a topic into a frame, hands it to `auto.arima` and journals the
answer uses three specs, none of which knows the others' names.

`okay-agent` is deliberately NOT a dependency. The module depends on
okay-codec, for the `Schema` at the edge.

A correction that arrived with the implementation (2026-09-07): both
this spec and specs/py.md used to say an R step is "journalable by
`Durable`", and it is not. `Durable.tools` wraps a `Handler[Tool]` and
`Tool.Call` carries a `ToolCall`; there is no generic
journal-any-operation. An R call reached THROUGH a tool is journalled
because the tool is — journaling an `REval` itself was filed as
`durable-any-operation`. It is true now (foreign-journalled,
2026-09-23): see "Journalled by Durable" below.

## Where the road goes

Stage 0 is the subprocess engine, and it is what exists. Stage 1 is
the served engine (Rserve behind the same handler), wanted when a
consumer needs concurrency without a process per session. `r-arrow`
is frames as Arrow — filed, and sent back twice by measurements: once
when the cost turned out to be our own JSON parser (the okay-py twin,
fixed instead), once when moving the tag to the column took the round
trip from 10.5 s to 180 ms. It has to beat THAT now, with a native
dependency on both sides. `r-restarts` — R's condition system, which
can RESUME rather than only fail — is filed and gated on stage 1.
Every one of them is a new instance of the same handler, not a new
API for a caller.

Nothing here has a consumer yet: no module in this repository calls
`REval`. That is the honest state, and it is why the list above is a
list rather than a plan.
