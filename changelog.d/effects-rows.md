## the interface over rows; the facade in the core; the row fixed

Lane effects-rows (specs/freer-min.md, stages 46–47). The core's `Effects[M]`
is over nominal rows, `M[_ <: Row, _]`: `perform` by path (`Member`),
`handle` wherever the effect is (`Removed`), handlers the machine's
(`Answering`, `Handler`), `run`; the machine's `Free[R, A]` is its instance
with no import, and `A ! R` in the core (Bang.scala) is that program —
`pure`, `effect`, `op.perform`, `p.handle(h)`, `p.value`, `Pure + A + B`,
`State % Int`. The machine's `Free` binds at one row: an operation is `Op`,
a program over any row that has its effect, its row from the expected
type; no join, no split at a bind. The classic's typeclass is
`okay.freer.Classic` (the old interface, levels 0 and 1); the classic tree
is an instance of the core's interface as `okay.freer.Rowed` (operations
tagged with their path, handlers run on the machine and reified back);
the union row's machinery — `+`, `Pure`, `%`, `split`, `Interpr`, `!>`,
`Distinct`, `Row.union`/`Row.flat` (were `Answers.union`/`flat`) — is the
classic's. `Prog` is gone. No file imports `okay.*`: the core's names by
name, the classic's by `import okay.freer.*`.
