## effects-foldmap — a program folded into any monad

Operator ask, 2026-10-02. `p.foldMap(nt)` on every `Effects` encoding:
the program's operations translated by `nt: F ==> G` into any okay
`Monad` G, derived from `foldCont` with `S = G[A]` — the fold `Static`
and `Proc` already had. Into another program, `Option` (short-circuits
on `None`) and cats' `IO`. The spec expected a stack bound for an eager
G; measured first, there is none: a million operations fold through
`Option` and `Either` in both shapes, because `Cont`'s data machine
resumes each continuation. TestFoldMap, a case in TestCatsClasses,
specs/effects-foldmap.md, docs/contract.md.
