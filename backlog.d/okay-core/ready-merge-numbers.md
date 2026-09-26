- [ ] ready-merge-numbers — `Source.mergeReady` landed with its laws
      but UNMEASURED (specs/ready-merge.md, Results): 101 attempts of
      `jmh-lane.sh` in an hour never got the lock and a quiet box at
      once. The lanes are written and compile. Run, alternating, two
      rounds, one lane per call:
      `sh scripts/jmh-lane.sh "compare/Jmh/run MergeBenchmark.<lane>$ -f 2 -wi 3 -i 5"`
      for okaySourceSingleDrain (control), okaySourceMerge,
      okayReadyMergeBuffered, okayReadyMergePure; record with
      `scripts/history.sh new ready-merge`. THE DECISION IT FEEDS: the
      matched pair is Buffered vs SourceMerge (a fiber per side in
      both, only the join differs). At parity or better, file
      `source-merge-via-ready` — `Source.merge` as buffer-each-side +
      `mergeReady`, one merge mechanism (it must keep `chunked` and
      `flushAfter`); worse, name the loss in the spec and profile
      before guessing (suspects: a CLQ node per wake-up, one element
      per turn). Pure is a user-facing price, no decision. TRIGGER:
      the first bench window (`bench-window`) or a quiet box.
      (2026-09-26, ready-merge)
