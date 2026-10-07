- [ ] cont-state-cost — the machine's `state` is 10 % behind the classic
      handler (stage 36: 20.3 vs 18.5 µs): a step is two `Answer` nodes
      and two `Bind`s (get, then put) where the classic's step is one; a
      fused `modify`/`update` operation answered once, and the loop's
      `Bind`-of-`Answer` arm looked at in `Machine.go`. Second: a row
      program's `Cap.perform` (Free.scala) builds a `Target` per operation
      — cache it per context as `Perform.reaches` does (stage 28). Lanes:
      `okayContJVM/Jmh/run .*ContBenchmark.stateAnswering$` beside
      `okayJVM/Jmh/run .*HandlerBenchmark.stateEffect$`, one per run.
