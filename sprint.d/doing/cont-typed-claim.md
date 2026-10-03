- [ ] cont-typed-claim — operator ask (2026-10-03): Cont.scala had grown
      from the two casts of cont-facade-over-free (ae679336) to eleven, and
      `Lazy` erased its answer (`Freer[Sig, Any, Any, Any]`). Down to ONE
      claim function, every crossing into the erased machine through it:
      `Lazy[R] = Freer[Sig, Any, Any, R]`, the lazy `k` a typed stack
      `LazyK[A, S]` (ContMacro.cpsBody's parameter), `Resumption`/`Later`/
      `Root` generic, `answered` gone. NOT a cast-free Cont: one root prompt
      serves every leaf at its own answer type, which a typed `Delimiter[Y, I]`
      cannot state (memory cont-facade-over-free, trap 2) — the operator chose
      "down to one claim" over a new typed runner. No behaviour change; core
      Cont benchmarks should not move (allocation compared).
