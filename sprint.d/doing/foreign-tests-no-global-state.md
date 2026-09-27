- [ ] foreign-tests-no-global-state — the flake foreign-reduce-wire-heal-flake
      named (operator, 2026-09-27: "Делай", item 1): `TestForeignReduce` and
      `TestForeignStage` set a GLOBAL `ReduceJobs.reducer`/`StageJobs.batcher`
      per test, read by in-process workers on their own threads — a
      run-order/parallel-suite effect waiting to happen, seen once red on a
      busy box. Each test gets a job of its own: the batcher/reducer in the
      constructor, a unique name, registered as itself. Then (items 3, 4):
      `Stateful` over `Flow.Owned`/`Scope` so an early stop releases the
      leased interpreter; `Engine.py/r` on the facade's `Frames` road so
      `mapIn` and `Streams.stream` are one road, said so.
