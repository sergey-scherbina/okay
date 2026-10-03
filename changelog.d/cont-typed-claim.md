## cont-typed-claim — Cont's casts down to one claim (2026-10-03)

Commits: see `git log --grep cont-typed-claim`.

- Operator ask: "в Cont много кастов и Any — убрать, всё строго
  типизировать". Cont.scala had eleven `asInstanceOf` and an untyped
  `Lazy` (`Freer[Sig, Any, Any, Any]`); it now has ONE cast, the
  private `claim`, and every crossing into the erased machine names it.
- Typed now: `Lazy[R]` answers `R`; the lazy `k` is `LazyK[A, S]`, the
  captured stack from `A` to `S` (ContMacro's `cpsBody` parameter, so
  `call` applies it with no cast); `Resumption`, `Later`, `Root` are
  generic (`A => S`); `answerOf`/`force`/`enter` return the stack's
  own answer; cont-list-combinators-one-walk's `walk` takes its step's
  answer `B`, so `answered` is gone.
- What `claim` still covers, and why (specs/cont-core.md, "What the
  machine still claims"): one root prompt answers every leaf at that
  leaf's own types, which no single `Delimiter[Y, I]` states — a leaf's
  clause and node, a tail body (`S <: R`), a program answer, a run's
  tree and answer. `Any` is left only in the erased machine indexes
  (`P`, `K`, the root). A cast-free Cont would be a different runner;
  not taken (operator's choice).
- No behaviour change: the erased shapes are the same objects as
  before (no node or closure added). Cont suites green on JVM.
