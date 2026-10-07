## okay-freer: the classic moves to `okay.freer`; the core is the interface

Lane classic-to-freer (specs/freer-min.md, stage 45). The effect library over
the freer tree — `A ! F`, handlers, rows, the effects, `Cont`, `Delimited`,
the macros, its suites and lanes — is module `okay-freer`, package
`okay.freer`, above the core: one `Effects` instance, chosen by the given in
scope. The core (`okay`) keeps the interface — `Effects`, `Control`,
`Answers`, `TypeableK`, `Distinct`, the type classes, `Prog` — on okay-cont
alone; okay-freer depends on the core alone; satellites take okay-freer.
`Classic[M]` is the classic as a typeclass (level 1: `shift`, `reset`,
`handle(m, h)`, `run`, `foldMap` over `Free` and `Eager`); `Classic`, `!` for
short, the toolkit. A program adds `import okay.freer.*` and
`import okay.freer.given` beside `import okay.*`; a macro names the tree at
`okay.freer.Freer`. Rewritten by script across 935 files; docs/modules/okay-freer.md.
