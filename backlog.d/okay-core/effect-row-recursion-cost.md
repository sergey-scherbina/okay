- [ ] effect-row-recursion-cost — PRIORITY: HIGH (the idiomatic road is
      the slow one). stack-safe-mutual-recursion's effects benchmark
      (compare MutualRecursionFxBenchmark, 2026-09-26, round 1): the
      same 1 000 000-level mutual tail recursion that counts every level
      and logs every 1 000th reads, per level —
        okay, effects as interfaces over `!.tailcall`   4.0 ns,  48 B
        okay, effects in the type (`okayRow`: State % Int
          + Writer % String, `State.run(Writer.run(p))`) 35 ns, 376 B
        cats Eval with the same interfaces              2.5 ns,  40 B
      so the road the docs teach costs 9x the road they call a
      workaround. READ IN THE CODE, not yet measured apart: per level
      the program performs TWO operations (`State.modify` is `get`
      then `set`, State.scala:56 — there is no one-step primitive), and
      both are FORWARDED through the inner `Writer.run`, whose loop
      rebuilds the continuation per forwarded operation
      (`Inject(e).flatMap(x => _loop(s)(k(x)))`, one Bind and one
      closure each) before the outer `State.handle` sees it. `.at` is a
      free `coerce` (Row.scala:138) and costs nothing.
      PROPOSED, each to be measured on its own before any lands:
      (1) handler order — `Writer.run(State.handle(p))` handles the
          frequent effect first and forwards only the rare one; if that
          is most of the gap, the docs must say which order to pick and
          why (and a lint or note in `run`'s doc);
      (2) a one-step `Modify` operation in State (`modify`, `update`,
          `swap` become one operation instead of get + set), halving
          the operations of every counter/accumulator program;
      (3) cheaper forwarding in the handler loops (Writer's
          `loopWith`, `State.handle`): forward without rebuilding a
          Bind per operation — the handler-fusion arc measured the
          rebuild at 48 B, 9% of a pass (specs/handler-fusion.md);
      (4) only if (1)-(3) leave a large gap: a fused State+Writer
          runner for this shape (the arc's pass fusion, 1.13-1.29x,
          was gated off; re-measure here where forwarding dominates).
      Guard: the handler lanes in docs/benchmarks.md §2/§2c must not
      regress. (2026-09-26, operator ask: "take on the performance
      problems we found")
