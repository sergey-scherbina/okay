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

That one framing buys the whole feature set before any code exists:
an R step is journalable by `Durable` (a crashed overnight pipeline
resumes past its R work; `Durable.replaying` re-runs an incident
offline with R's answers frozen), testable by swapping the handler
(a canned-answers handler IS the mock), and supervisable like a
cluster worker (a dead R process THROWS — that is the whole
protocol, the okay-cluster precedent).

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
enum RValue: ...

/** a data.frame as columns of primitives; Schema[A] maps a flat
 * case class to/from a frame row-wise */
final case class RFrame(cols: Vector[(String, RColumn)])
```

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

- [ ] a Call round-trips scalars and vectors (NULL/NA distinct from
      absent; the R NA story stated, not papered over)
- [ ] a frame maps to a Seq of a flat case class and back; row
      count and column order survive; a column the Schema does not
      name is an error naming the column
- [ ] an R error (stop()) surfaces as a condition value with the
      message; the process survives for the next call
- [ ] a killed R process makes the in-flight call THROW; a
      supervisor retry gets a fresh process (the dead-worker
      protocol)
- [ ] a timeout kills the call, reports as data, and the engine is
      usable after
- [ ] verify reports a missing package and a version mismatch by
      name; a passing verify then runs the program's calls
- [ ] no API accepts runtime-built R source; args reach R only as
      RValue/RFrame (structural: the enum has no Eval-a-string case)
- [ ] the R process starts with a clean environment: a parent env
      var is invisible in R unless the config names it
- [ ] a journaled R step is skipped on Durable replay (an agent
      program with an R operation recovers without re-running R)
- [ ] (stage 1) the same test program passes over subprocess and
      Rserve engines unchanged (the two-driver acceptance move)

## Out of scope

- embedding R in the JVM (JRI/Renjin/FastR) — rejected in the
  overview with reasons, not deferred
- R calling back into okay mid-evaluation — one direction v1; a
  callback is a second protocol and no consumer has named it
- package installation/management — renv's job; we verify, we do
  not install
- streaming frames (Arrow IPC streams, chunked exchange) — staged
  behind the file road; batch frames first
- Python — the same shape would serve (a `PyEval` twin over a
  subprocess/Arrow road), noted so the naming stays parallel, but
  not specced until asked

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

## Results

(after implementation — round-trip counts, the clean-environment
check, a real forecast package through both engines)
