- [ ] scheduler-default-rerun — re-run scheduler-default-decision's table
      (be31ac072; specs/schedulers.md "The default") on today's code, now
      that both of its reopen conditions are met: Wrocław on `adaptive` =
      Loom (adaptive-outside-long-fibers-serial, 00c140980) and blocking TCP
      1.31x Loom (adaptive-blocking-io, 7635c2bee). Same matched pairs, loom
      vs adaptive as the given, in TWO PARTS (operator): part 1 — one round
      of the core lanes (fork/join 10k outside and inside, cancel 1k,
      parallel8, Wrocław 8-core, five-way blocking TCP), recorded; part 2 —
      the second round, §4 100 fibers, spawn, the rest of five-way. Then
      decide: flip `Schedulers.auto` to `adaptive` where Loom exists (with
      docs, spec table, the JDK 17-20 caveat: no spill there) or keep Loom
      with the new table as the reason. (2026-09-28, operator)
