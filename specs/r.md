# okay-r: R as a handler

## Overview

R is where a large share of the world's applied statistics actually
lives — forecasting, econometrics, bioinformatics, the packages a
business's analysts already trust. This spec adds R to the landscape
of specs/data.md as what it is in this stack's terms: a FOREIGN
COMPUTE runtime, reached the way every nondeterministic external
thing is reached here — **an R call is an OPERATION** (the
`Model`/`Tool` precedent from okay-agent), performed by a handler
that owns the how.

That one framing buys the feature set before any code exists:
testable by swapping the handler (a canned-answers handler IS the
mock) and supervisable like a cluster worker (a dead R process
THROWS — that is the whole protocol, the okay-cluster precedent).

**Corrected while building it (2026-09-07):** this paragraph also
claimed an R step is journalable by `Durable`, and that is NOT true
as written — for R or for Python, where the same sentence had been
copied. `Durable.tools` wraps a `Handler[Tool]`, and `Tool.Call`
carries a `ToolCall` (a name and JSON arguments); there is no generic
"journal any operation type". An `REval` handled by `RSubprocess` is
therefore not journalled by anything today.

What IS true is narrower and worth saying exactly: an R call reached
THROUGH a tool — an agent whose `forecast` tool happens to call R
behind it — is journalled like any other tool call, because the
journalled thing is the tool. Journaling `REval` itself needs
`Durable` generalised beyond `Tool`, which is a change in okay-agent
and is filed as `durable-any-operation` rather than assumed here. The
two claims that survive are proven in `TestRMock`, without an R
anywhere.

What this spec refuses from the start: EMBEDDING. R in the JVM
(JRI/rJava-reverse) is a single-threaded engine with global state
behind JNI; Renjin's package coverage is partial and the project
quiet; GraalVM's FastR is in maintenance. R runs in ITS OWN
PROCESS, with process isolation as the correctness boundary, and
everything else follows from that.

## The model

```scala
package okay.r

/** the operations a program requests; the handler owns transport,
 * process lifecycle and data movement */
enum REval[A]:
  case Call(fn: String, args: Vector[RValue]) extends REval[RValue]
  case Frame(fn: String, in: RFrame, args: Vector[RValue]) extends REval[RFrame]

/** neutral values at the edge — the SqlValue move, R-shaped:
 * NULL/NA, logical, int, double, string, bytes, and vectors of
 * those. Schema binds case classes to RValue rows exactly as it
 * binds them to Sql rows: one flat-product story, told twice. */
enum RValue:
  case RNull                        // R's NULL — the ABSENCE OF AN OBJECT
  case NA(of: RType)                // a missing value INSIDE a vector, TYPED
  case Bool(v: Boolean); case I32(v: Int); case F64(v: Double)
  case Str(v: String); case Bytes(v: Array[Byte])   // raw
  case Vec(v: Vector[RValue])

enum RType: case Logical, Integer, Double, Character

/** a data.frame as columns of primitives; Schema[A] maps a flat
 * case class to/from a frame row-wise */
final case class RFrame(cols: Vector[(String, RColumn)])
```

### R's two absences (written while building it, 2026-09-07)

Python has one `None`, and okay-py's `PyValue` has one case for it.
R has TWO, and flattening them would be the "papered over" this
spec's own behaviour list forbids:

- **`NULL`** is the absence of an OBJECT. `list(a = NULL)` has no
  `a`. It is what a function returns when it returns nothing.
- **`NA`** is a missing value INSIDE a vector, and it is TYPED:
  `NA_integer_`, `NA_real_`, `NA_character_` and a logical `NA` are
  four different values, and `c(1L, NA)` stays an integer vector
  while `c(1L, NULL)` is a one-element integer vector — the NULL
  vanished.

So `RValue` carries both, and `NA` carries its type. A caller that
does not care can ignore the distinction; a caller doing statistics
cannot, because `mean(c(1, NA))` is `NA` and `mean(c(1, NULL))` is
`1`. Getting that wrong silently is exactly the class of error a
forecast never announces.

- **Functions, not strings.** `Call("forecast::auto.arima", args)`
  names a function; there is deliberately NO operation that evals
  an arbitrary R string built at runtime. R code is code — the
  program's R fragments are written by the author, named by the
  author, and untrusted input reaches them only as DATA (RValue/
  RFrame). The same rule the UI wire has (structure cannot be
  injected), applied to a runtime that would happily
  `system("rm -rf")`.
- **verify, a third time.** `RInterop.verify(packages)` at startup:
  the R version and every required package's presence and version,
  mismatches as data naming the package — the Durable fingerprint
  and the Sql `describe` lesson at the R seam. An analyst's
  `renv`/`packrat` owns installation; we own the loud check that
  the environment is the one the program was written against.
- **Errors are data**: an R condition (error/warning) comes back as
  a value naming the condition and message; a timeout kills the
  call and says so; a DEAD process throws, and the supervisor
  (retryChunks / the caller) decides — the parallel-resilience
  fault model unchanged.

## Engines (both behind the one handler)

### jsonlite is a NAMED prerequisite, not a silent import

okay-py's shim is stdlib-only, deliberately. R's cannot be: base R
has no JSON reader. The three roads were a hand-written parser in
base R, R's own binary serialization, or one package — and the
first is our own parser sitting at the trust boundary, which is a
worse thing to own than a dependency that every R installation used
for anything already has.

So the shim requires `jsonlite`, and its absence is refused at the
HANDSHAKE, by name, with the two commands that fix it
(`install.packages("jsonlite")`, or the distribution's
`r-cran-jsonlite`). A named prerequisite an operator can act on
beats a stack trace from inside a shim, and it is the same
"verify over trust" rule this spec already applies to every other
package.

- **Subprocess** (stage 0): `Rscript` per session, values over
  stdin/stdout as CBOR (a tiny R-side shim decodes;
  jsonlite-compatible JSON as the fallback wire since Schema serves
  both), frames as ARROW FILES on disk handed by path (R's `arrow`
  package reads them near zero-copy; ours writes them via a small
  IPC writer — staged, JSON-frame fallback first). Crude, robust,
  zero server administration; the right default for batch
  analytics.
- **Rserve** (stage 1): the served R — Simon Urbanek's Rserve
  speaks QAP1 over TCP, forks a session per connection, and is the
  road for interactive/low-latency use. Behind the same handler:
  first via the existing Java client held at arm's length behind a
  trait (JVM), then — if a consumer needs Native/Node or the
  dependency chafes — a minimal QAP1 client over the Async
  transport, the okay-pg move (this stack speaks SSE, JSON-RPC,
  MCP, RESP-planned, pg-wire-planned; QAP1 is of the same nature).
  A REMOTE Rserve rides specs/tls.md or stays behind localhost/a
  tunnel — Rserve's own auth is weak, and the spec says so rather
  than trusting it.
- Config and secrets per specs/conf.md: the Rserve address and any
  credentials are conf fields; NOTHING of the parent environment
  leaks into the R process unless a config names it (an R process
  inherits no secrets by default — invariant 1's cousin).

## Where it sits in the landscape

specs/data.md's "heavy compute" row grows a sibling: Spark/Flink
carry the AGGREGATION shape (the merge contract), R carries the
CALL shape — a statistical function over a frame, answered. Both
are foreign compute; neither is storage; both meet the rest of the
stack through values with Schemas. A pipeline that folds a topic
into a frame, hands it to `auto.arima`, and journals the answer
uses three specs without any of them knowing the others' names.

## Module

`okay-r`: the `REval`/`RValue`/`RFrame` types and both engines.
JVM first (subprocess + Rserve-client); the QAP1-native road keeps
Native open later. Depends on okay-codec (Schema at the edge) —
okay-agent is NOT a dependency: `Durable` journals R steps because
they are operations, not because the modules know each other.

## Behavior

**Hardened 2026-09-09 (r-measure-harden):** `start(…, require)` runs
`verify` at CONSTRUCTION and refuses to hand out an engine whose
packages drift, the Sql seam's verify-at-startup posture in the same
words. And one edge is now stated where a reader meets it: a respawn
that FAILS after a timeout (R gone between two calls) throws rather
than answering data — the dead-process story one step later, and an
engine whose interpreter no longer exists is not something a program
can handle as a value.

**Audited 2026-09-09 (spec-truth)** against what RUNS, not what exists:
`okayR/test` alone runs the 6 mock tests, and `TestR`'s 18 are Live —
they skip where no R is found and otherwise build their own container
(r-base 4.4.1 + jsonlite). This audit ran them: 17 passed, 1 skipped
(named in its box below). A box checked here names the test that
proves it.

- [x] a Call round-trips scalars and vectors (NULL/NA distinct from
      absent; the R NA story stated, not papered over) — TestR: NULL vs
      NA, an NA keeps its TYPE (R's four NAs are four values), NA vs
      NaN, integer vs double kept apart where JSON would merge them,
      raw bytes and strings; TestRMock walks every RValue shape with no
      R present (checked 2026-09-09, spec-truth)
- [x] a frame maps to a Seq of a flat case class and back; row
      count and column order survive; a column the Schema does not
      name is an error naming the column — HALF BUILT, and the half
      that WAS not is the case class (BUILT since, see below): `RFrame` is
      `Vector[(String, Vector[RValue])]` and okay-r names no `Schema`
      at all. What IS proven: a frame goes out as columns and comes
      back with order and count intact, over the wire too; a column
      carries NA in place; a function answering something that is not
      a frame is a condition naming what arrived. The Schema mapping
      and its unnamed-column error were BACKLOG `r-frame-schema`, and
      are BUILT 2026-09-09 (r-finish): `RFrame.rows[A: Schema]` and
      `RFrame.of[A: Schema]`, fields matched to columns BY NAME with
      the field order as the column order; every mismatch a
      `Condition` naming what does not line up — a column no field
      names, a field with no column, a cell that does not fit (naming
      the column AND the row). An absent cell keeps its COLUMN's type,
      so an `Option` field writes `NA_character_` in a text column
      rather than a logical NA; R's widening is admitted where R
      admits it (an integer column into a Double field) and nowhere
      else. Tested without R (TestRMock) and over the dockerized one
      (TestR: a frame through `identity` and back into the case class)
- [x] an R error (stop()) surfaces as a condition value with the
      message; the process survives for the next call — TestR, plus a
      missing function and a missing package as conditions
- [~] a killed R process makes the in-flight call THROW; a
      supervisor retry gets a fresh process (the dead-worker
      protocol) — the NEXT call after a death throws, and that is
      tested ("a DEAD process makes the next call THROW — the
      supervisor decides, not us"). Killing a call already IN FLIGHT,
      and the retry that gets a fresh process, are not covered: okay-r
      has no supervisor of its own (by design — the caller's is the
      one that decides), so the second half is a claim about a
      CONSUMER, and belongs in the lane that writes one. UPDATE
      (r-finish): the in-flight half now has an answer, and a better
      one than a throw — with a deadline set, the call whose process
      is killed answers `Condition("timeout", …)` as DATA and the
      engine respawns itself
- [x] a timeout kills the call, reports as data, and the engine is
      usable after — BUILT 2026-09-09 (r-finish), where the audit had
      found nothing at all: `RSubprocess.start(…, timeoutMillis)`. The
      blocking read moves to one daemon thread per engine, and only
      where a deadline asks for it; on expiry the PROCESS is killed —
      the only way to stop R mid-call, since a sleeping or optimising
      R is busy in C and no polite protocol reaches it — a fresh one
      takes its place, and the call answers
      `Left(Condition("timeout", …))`. Proven against the dockerized
      R: a 120-second sleep behind a 2-second deadline answers in ~2s
      as data, and the very next call on the same engine is correct.
      With no deadline the engine blocks exactly as before
- [x] verify reports a missing package and a version mismatch by
      name; a passing verify then runs the program's calls — TestR
      names all three (a missing package, a version mismatch, a
      passing verify that says nothing and then runs), and two more
      the box did not ask for: a shim from another version and a shim
      without jsonlite each refuse BY NAME
- [x] no API accepts runtime-built R source; args reach R only as
      RValue/RFrame (structural: the enum has no Eval-a-string case) —
      read on the current tree: `REval` has exactly `Call(fn, args)`
      and `Frame(fn, in, args)`, both taking a NAME and `RValue`s;
      TestR pins the addressing (`pkg::name`, a base name, "the
      program is data rather than code")
- [~] the R process starts with a clean environment: a parent env
      var is invisible in R unless the config names it — the FIRST
      half is proven ("the R process sees EXACTLY what the config
      names, and nothing else we passed", green against the
      dockerized R). The second, a real parent variable being
      invisible, SKIPS wherever R is reached through the container
      shim (the shim forwards the environment on purpose, or it would
      measure docker rather than us) — so it is proven only on a box
      with R on the PATH, and this audit's run was not one
- [~] a journaled R step is skipped on Durable replay — NOT as
      written: `Durable` journals `Tool`, not any operation type. An
      R call reached through a tool is journalled because the TOOL is;
      journaling `REval` itself is `durable-any-operation`. See the
      correction in the overview.
- [ ] (stage 1) the same test program passes over subprocess and
      Rserve engines unchanged (the two-driver acceptance move)

## r-restarts (filed, GATED on r-subprocess)

R's condition system natively has RESTARTS: a handler may not just
refuse but RESUME the computation with a fix. A Delim-shaped
condition API on our side (the condition is a shift; the handler's
answer resumes the captured continuation) would mirror that honestly
instead of flattening restarts into errors — the one place resumable
capture (not just abort) earns Delim's full price. Gated twice: on
r-subprocess landing (itself gated on an R interpreter being
present), and on a consumer that actually uses a restart.

## Out of scope

- embedding R in the JVM (JRI/Renjin/FastR) — rejected in the
  overview with reasons, not deferred
- R calling back into okay mid-evaluation — one direction v1; a
  callback is a second protocol and no consumer has named it
- package installation/management — renv's job; we verify, we do
  not install
- streaming frames (Arrow IPC streams, chunked exchange) — staged
  behind the file road; batch frames first
- Python — the same shape, specced: specs/py.md (`PyEval`, the
  twin; that spec references this one's model rather than
  restating it)

## Decisions

- **A process, never an embedding** — isolation is the correctness
  boundary R's own engine cannot offer in-process; every rejected
  embedding is a shared-fate design. Rejected: JRI/rJava (JNI +
  global interpreter state), Renjin/FastR (coverage/maintenance).
- **Operations, not a client API** — `REval` in a program instead
  of an RClient in a service: journaling, replay, mocking and
  supervision arrive from machinery that already exists. Rejected:
  a standalone client library surface.
- **Named functions only, no string eval** — the injection door
  stays closed structurally, as on the UI wire. Rejected:
  `Eval(code: String)` (every use site would be one interpolation
  away from an incident).
- **Neutral RValue/RFrame with Schema at the edge** — the SqlValue
  decision, third telling; frames are flat products like rows.
  Rejected: exposing engine-native types upward.
- **Subprocess before Rserve, both kept** — the robust road first,
  the served road when latency asks; the same-tests-both-engines
  acceptance keeps them honest. Rejected: Rserve-only (an admin
  dependency for batch jobs that do not need it).
- **verify over trust** — analysts' environments drift; a named
  package check at startup converts "wrong forecast silently" into
  "loud refusal naming forecast==8.x". Rejected: discovering drift
  in the answers.

## What the wire costs (r-measure-harden, 2026-09-09)

`MeasureRFrame` (Live, medians of five against the dockerized R 4.4.1,
`identity` on a 3-column frame) — the number `r-arrow` was filed to
wait for:

| rows | payload | our encode | round trip | our decode | typed rows | OUR share |
|---|---|---|---|---|---|---|
| 10 000 | 0.30 MB | 6.5 ms | 1 546 ms | 7.3 ms | 2.2 ms | 0.9% |
| 100 000 | 3.21 MB | 20.4 ms | 13 686 ms | 18.3 ms | 6.0 ms | 0.3% |

It is the opposite of the Python twin's result, where 60% of the trip
was our own parser. Here our two halves are 0.3%, and it is not the
pipe either: 3.21 MB in 13.7 s is ~230 KB/s while we encode and decode
the same bytes at ~83 MB/s. The cost is R walking the STRUCTURE we
hand it, and the structure is the suspect: `Wire.enc` tags PER CELL —
an integer, an NA and an integral double are each a small object,
because JSON cannot otherwise keep R's integer apart from its double
nor its four NAs apart — so a 100k-row frame is hundreds of thousands
of objects for jsonlite to build, on the one path it cannot take fast.
A frame is columnar and a column is homogeneous, so the tag belongs to
the COLUMN (BACKLOG `r-frame-columnar-wire`): one type, one plain
array, absences as an index list. That costs no dependency, where
Arrow costs a native package on R's side and a reader on ours — so it
goes first, and `r-arrow` waits for the number after it.

## Results (stage 0)

**r-subprocess, 2026-09-07.** okay-r with `REval`/`RValue`/`RFrame`,
the versioned shim as a resource, and the comonadic handler over a
clean-env `Rscript`. Proven against a live R (a container where none
is installed, reached by a shim at the same absolute path inside and
out): `pkg::name` and base addressing, a missing function and a
missing package as conditions with the process SURVIVING, `stop()`
carrying its message, frames columnar both ways with NA in place, the
v99 handshake refusal, the jsonlite refusal by name, `verify` naming
an absent package and a version mismatch, the `/no/such/Rscript`
refusal at start, and the dead process turning the next call into the
supervisor's throw. 17 live, 1 skipped, 6 without an R at all.

Four things the writing decided or found:

- **R's two absences are two cases**, and R's own arithmetic is the
  test: `mean(c(1, 2, NA))` is NA and `mean(c(1, 2, NULL))` is 1.5,
  because the NULL vanished from the vector. An NA also keeps its
  TYPE across the wire, so `c(1L, NA)` comes back an integer vector.
- **A plain JSON number is always a double on this wire.** jsonlite
  reads `3` as an R integer, so without the coercion a caller's
  `F64(3)` arrived in R as an integer — a type change nobody would
  see until a `class()` or a join behaved oddly. An R integer is
  tagged `i` instead.
- **A wire array becomes an atomic vector when every element is a
  scalar.** `sum(c(1, 2, NA))` is the call an analyst writes;
  `sum(list(1, 2, NA))` is an error about types. A mixed or nested
  array stays a list.
- **`RValue.Int` was renamed `I32`** because a case called `Int`
  shadows `scala.Int` for every caller who writes `import RValue.*`,
  which is every caller. It is also more accurate — R's integer is
  32-bit — and matches okay-py's `I64`/`F64`.

And one correction that was not about R: see the overview on
`Durable`.

## Results

(after implementation — round-trip counts, the clean-environment
check, a real forecast package through both engines)
