## pyvalue-table — a frame as a VALUE, and the functional stateful stage for every language

(operator, 2026-09-27: "Делай а".) `PyValue.Table(frame)` in okay.foreign:
a frame can now sit INSIDE a value — among a call's arguments, inside the
dict a function answers, an element of a list. `Wire.enc/dec` write and
read it where `t == "frame"` at any depth; a frame's own column may not
hold a frame, at any depth of its cells, refused by name both ways
(`IllegalArgumentException` out, a `WireError` condition in, checked on
the JSON before the cell is decoded) — the rule that also bounds the
value -> frame -> cell -> value cycle to one pass, written into
specs/stack-safety-okay.tsv for the eight methods on it. `PyCodec` decodes a `Vector[A]`/`List[A]`
from a `Table` by `A`'s Schema, so `{rows: frame, state: …}` reads
straight into a case class; `Shape.toJson`, `ArrowFrames.kind` and `Jvm`
name the case. The Python shim gains `okay.frame(cols)` (a `dict`
subclass that `enc` tags as a frame wherever it sits; a pandas frame is
tagged the same way); Go, Rust and Haskell already tagged a nested
`Value::Table`/`VTable`, and R's `enc` a nested data.frame.

On that, `StatefulValue[-M]` in okay-foreign-cluster (specs/foreign-map-reduce.md
stage 5): `open(params) -> state`, `step(frame, state) -> {rows, state'}`,
`finish(state) -> frame` — three plain calls, the state a Schema value the
JVM carries between them. That is the stateful stage the COMPILED workers
can have (Decision 23 untouched: nothing is held, nothing is mutated), and
Python, R and the JVM have it beside `Stateful`. No worker is leased for a
partition, so a death costs a chunk's retry rather than the partition.
`flow.statefulValueIn[B, St, P](module, open, step, finish, params)`,
instances `StatefulValue.py/r/worker/jvm`, `JvmModule.streamValue`.

Tests: `TestPyValueTable` (okay-py, default gate: wire round trip inside a
dict, a list and alone; nesting refused both ways; rows from a Table by
Schema; the JVM map), `TestPyFrameInValue` (Live, python3: `okay.frame`
answered inside a dict arrives as a Table, a plain dict of lists stays a
dict, a Table argument arrives as a dict of columns), `TestStatefulValue`
(default gate, JVM: running sum per partition with the state a value,
opened/finished once each, `open` seeded from the parameters, an empty
partition, compile-time refusal naming `StatefulValue`),
`TestPyStatefulValue` and `TestRustStatefulValue` (Live: the same job
text over a `PyModule` and a cargo-built `WorkerModule`).

Commits: (filled at landing).
