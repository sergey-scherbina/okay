## shift-merge-guard — one machine guard, and it nests instead of refusing

`Shift.OneMachine` (a `NotGiven`, a compile error at a `Shift` row) and
`Shift.Nesting` (a macro reading the row, used by the keyed `reset`) are
one piece of evidence, `Shift.Machine[F]`: does a machine already run in
`F`? Every door that runs a machine — `run`, `delimited`, `collect`,
`collectUntil`, `resumable`, `drive`, `answer`, `replay`, the keyed
`reset`, `Stacked.run`/`delimited` — runs its own when outermost and,
inside one, pushes its delimiter on the running machine, as
`scope`/`collecting`/`pausing` do. `resumable` around `collect` works as
written; the "SECOND machine" compile error is gone. An abstract row is
a compile error asking for `(using Shift.Machine[F])`, which closes the
hole chapter 12 demonstrated (a generic helper manufacturing the
evidence and throwing `NoPrompt` at a `Shift` row) — and found one in
okay-persist's `Dialogue`. `NoPrompt`'s message states the new rule.
Book chapters 9, 12, 14, 19, 21, 26, 27 and continuations-in-practice
rewritten; TestBookOneMachine, TestDelimNesting, TestDelimLimits,
TestBookComposing, TestBookDisciplines flipped from "refused" to
"nests". specs/shift-merge.md.
