## the bare name is the machine's: `Cont` → `Cps` in the classic

Lane cont-classic-rename (specs/freer-min.md, stage 51). The classic's CPS
paramonad `okay.freer.Cont` is `okay.freer.Cps` (and `okay.scala2.Cps` for
Scala 2), its diagonal `A />> R`; `A /> R` is now the machine's
`Carrier[A, R, R]`, and `Cont` names only the machine, `okay.cont.Cont`.
Source changes: `Cont` → `Cps` and `/>` → `/>>` wherever the CPS one was
meant.
