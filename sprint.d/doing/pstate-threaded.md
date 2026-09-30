- [ ] pstate-threaded — item 2 of the from-scratch list, the payoff of
      freer-consumed-index: a type-changing state as a DATA signature
      on the indexed tree (`PState.Op[S, R, +X]`: `Get[S]` at `(S, S)`,
      `Put[S, T](t)` from `S` to `T`, answering the old state as
      `PState.set` does) run by `State.handle`'s tail-recursive loop
      with the type moving — no continuation object, no `Reentry`, no
      room. Doors `PState.Threaded.get/put/run`, the alias
      `PState.Threaded[A, S, R]`. THE QUESTION IS A NUMBER: a
      `stateThreaded` lane in HandlerBenchmark, the same M-step
      workload as `statePara`, read against `statePara` (1.29x
      `stateEffect` today) and `stateEffect` itself — does the typed
      protocol now cost what the untyped State costs? Three lanes on one
      tree, alternating, MIN of 3, `jmh-lane.sh -f2 -wi3 -i5 -prof gc`.
      Tests in TestState (the loop, the type refusing a Put from the
      wrong state). Additive: own suites + `affected master
      Test/compile` + `okayJVM/Jmh/compile`. Records: history.d, the
      spec's Results, State.scala's header number, docs quoting 1.29x.
