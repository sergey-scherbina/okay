## foreign-value-rename — the shared value model named for what it is: `Value`, `Frame`, `Handle`, `ValueCodec`

(operator, 2026-09-27: "PyValue — общий enum на весь okay.foreign — а
почему он Py если общий на весь foreign?") The names came from okay-py,
which built the wire first; foreign-one made the model shared and kept
them (specs/foreign-one.md Decision 26, "for now"). Now `okay.foreign.Value`
(with `Value.Null` for `PyNone` — the name Go, Rust and Haskell use),
`Frame`, `Handle` (a handle to an object held in the worker; `Ref` stays
the enum case carrying one) and `ValueCodec`. Module types stay per
language (`PyModule`, `RModule`, `WorkerModule`, `TsModule`).

The old names stay as aliases in `okay.foreign` (`type PyValue = Value;
val PyValue = Value`, the same for `PyFrame`/`PyRef`/`PyCodec`, and
`Value.PyNone`), the way `okay.RowLift` stayed for `okay.Row`, so every
caller compiles unchanged and `okay.py.Aliases` resolves through them;
they go a release later. Inside `enum ForeignEval` the bare `Frame` is
the frame OP, so its argument type is spelled `okay.foreign.Frame`
there. Swept: 59 Scala files across okay-py, okay-r, okay-rust,
okay-codec, okay-foreign-workflow and okay-foreign-cluster, the docs and
specs that name them, and the stack inventory's rows for
`ValueCodec.scala`. Suites renamed with their files: `TestValueCodec`,
`TestValueCodecDepth`, `TestValueWalk`, `TestValueTable`,
`TestFrameInValue`. Decision 29 in specs/foreign-one.md.

Commits: (filled at landing).
