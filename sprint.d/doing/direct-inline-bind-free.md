- [~] direct-inline-bind-free — a Free `direct` block's binds are
      virtual calls on `mm$direct: Monad[F]`; hoisting the val at the
      GIVEN'S precise type and selecting `flatMap` on it makes the
      instance's `override inline` member reduce at compile time,
      which on the staged carrier was the difference between 152 568
      and 85 368 B/op (direct-staged, Results §3). For a Free block
      the instance's `flatMap` is `a.flatMap(f)`, inline into `Bind`
      — one virtual call fewer per bind, allocation unknown. Tried on
      every block during direct-staged and it broke two, which are
      this lane's first laws: (1) `ctxMonad[E]`'s declared result
      `E ?=> A` is a type the typer auto-applies, so the precise val
      turns `pure(x)` into a method where a value should be (Erasure
      "bad adapt for mm$direct.pure", TestDirectTryCtx) — keep
      `Monad[F]` for a context-function carrier; (2) a block that
      REBUILDS a lambda (`programLambda`, a nested block under
      `Delim.shift`, TestBookInTheSystem) left the inlined binds'
      proxies with an owner LambdaLift could not find (`key not
      found: method $anonfun`) — the rebuilt lambda must re-own what
      the inliner put under it, or the inline road must not enter a
      rebuilt lambda. NUMBER: okay-direct's `RowLiftBenchmark` and
      `StagedBenchmark.freeDirectNested` before/after, alternating;
      under 3% is a refutation worth recording.
