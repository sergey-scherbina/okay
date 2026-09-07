# okay-r

R as a handler (specs/r.md; the twin of [`okay-py`](okay-py.md), and
the model is stated once for both): call-shaped foreign compute.
Calls are OPERATIONS — journalable by `Durable`, mockable by swapping
the handler, supervised by dead-process-throws. Named functions only:
the enum has no eval-a-string case, structurally, so untrusted input
reaches R only as data.

| | |
|---|---|
| `REval` | `Call(fn, args)` answering an `RValue`, `Frame(fn, in, args)` answering an `RFrame` — the two shapes a statistical function takes |
| `RValue` | R's edge values, the `SqlValue` move told again: `RNull` (the absence of an OBJECT) and `NA(of)` (a missing value INSIDE a vector, typed) are DISTINCT, plus logical, int, double, string, raw bytes and vectors of those |
| `RType` | `Logical`, `Integer`, `Double`, `Character` — what an `NA` is missing FROM |
| `RFrame` | a data.frame as columns of primitives; a `Schema[A]` maps a flat case class to and from a frame row-wise, exactly as it maps one to a SQL row |
| `Condition` | R's own failure vocabulary as data (`kind`, `message`) — a failing call is a value and the process survives |
| `RSubprocess` | stage 0: one `Rscript` per session running the shim SHIPPED WITH THIS MODULE (a versioned resource whose handshake refuses drift loudly), with a CLEAN environment — the parent leaks nothing into R unless the config names it |

## The two absences, which is where the twin is not a copy

Python has one absence (`None`) and one not-a-number (`NaN`). R has
three things where a naive bridge sees one: `NULL` is the absence of
an object, `NA` is a missing value inside a vector AND carries the
type it is missing from, and `NaN` is a double that happens not to be
a number. Collapsing any two of them silently changes what a
statistical function computes, so `RValue` keeps them apart at the
type level and the shim carries the distinction across the wire.

## What it is not

Not R-on-the-JVM. The subprocess boundary buys the real interpreter,
every CRAN package, and crash isolation; the spec argues that choice
once for both twins. Not storage, and not an aggregation engine —
specs/data.md's heavy-compute row has Spark and Flink carrying the
MERGE contract and R carrying the CALL contract. A pipeline that
folds a topic into a frame, hands it to `auto.arima` and journals the
answer uses three specs, none of which knows the others' names.

`okay-agent` is deliberately NOT a dependency: `Durable` journals R
steps because they are operations, not because the modules were
introduced to each other. The module depends on okay-codec, for the
`Schema` at the edge.

## Where the road goes

Stage 0 is the subprocess engine. Stage 1 is the served engine
(Rserve behind the same handler), and `r-arrow` is frames as Arrow
once the JSON-frame numbers say the wire is the cost. `r-restarts` —
R's condition system, which can RESUME rather than only fail — is
filed and gated on this stage. Every one of them is a new instance of
the same handler, not a new API for a caller.
