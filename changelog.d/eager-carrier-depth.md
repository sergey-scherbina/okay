## eager-carrier-depth — tailRecM and foldMap with no stack overflow, in principle

Operator ask, 2026-10-02 ("полный трамплининг"). `TailRecM` is now the
CARRIER's own loop, never derived from `flatMap` (an eager `flatMap`
calls its continuation before returning; Freeman's *Stack Safety for
Free*): core gives `Option`, `Either`, `LazyList`, the context monad and
programs; okay-cats `IO`, `Eval` and any cats `Monad`; okay-zio ZIO and
ZStream (`TailRecM.deferring`, their flatMap defers); okay-kyo `Loop`.
`foldMap` is the target's `tailRecM`. A monad without a loop has no
`tailRecM` — a compile error. Every instance holds a million on a 128 KB
thread, core's also on Scala.js and Native. Corrects effects-foldmap and
monad-tailrecm, whose "a million through Option" ran on an 8 MB stack.
specs/eager-carrier-depth.md.
