- [ ] resume-late-small-ring-cost — f50932cbe (`DriveTask.resumeLate`:
      a late answer from a non-worker thread sends the fiber home with
      `forkLong`) made `Source.zip(range 2000, LazyList 2000, capacity = 7)`
      ~6x slower (3000 runs: 10.6 s on master vs 1.7 s before it) and the
      zip's pre-existing lost-pairs race (source-zip-lost-pairs, a
      sibling's) ~6x more frequent (144 vs 23 short runs in 3000). With a
      7-slot ring every few elements are a handoff: a DriveTask, a queue
      and an unpark. FIRST measure the candidates on BOTH shapes (zip cap
      7, MergeCapBenchmark cap 64): `fork` instead of `forkLong`, and a
      revert; keep what holds both, else revert. (2026-09-29)
