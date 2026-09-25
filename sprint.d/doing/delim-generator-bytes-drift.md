- [ ] delim-generator-bytes-drift — PRIORITY: MEDIUM (a number in the
      ledger is stale). delim-dollar (2026-09-25 morning) recorded
      DelimBenchmark.delimGenerator at 910 330 B/op and pinned "byte-
      identical to master" on it. A control run of the SAME lane on
      master at daee1acf0 (lexical-tail-guard-abort, 2026-09-25 evening,
      jmh-lane, quiet box) reads 942 314 B/op: +32 B per capture arrived
      on master between the two, and no lane's history row claims it.
      Candidates by what landed on the core in between: cont-stack
      stages (Free runner, Delay/StackRoom), stack-safety-core
      (Delim.split as a loop with Wrap frames — a `Wrap.On` per passed
      segment is exactly a small allocation per capture). THE LANE:
      bisect the lane over the day's core commits with jmh-lane (bytes
      are exact, one fork is enough), name the commit, and either take
      the bytes back or record them as the price of what it bought.
      DONE WHEN: history.d has the row naming the commit and the
      delimGenerator baseline in specs/shift0-dollar.md Results is
      corrected. Source: lexical-tail-guard-abort control run.
