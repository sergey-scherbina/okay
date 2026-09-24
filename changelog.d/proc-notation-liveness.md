## proc-notation-liveness - built, measured, refuted, recorded

The operator asked for liveness in `Proc.direct`: dead names dropped from
the environment tuple instead of riding to the end of the block.

- okay-workflow's first JMH, `ProcEnvBench`: a chain of N statements, each
  name read only by the next, run by `foldMap` into Id so that only the
  environment's plumbing is timed. Baseline: 23 ns per statement at 4,
  33 at 64.
- The experiment (pruning before each statement, never in a loop body)
  passed all 137 okay-workflow tests, and was SLOWER in two alternated
  rounds: 1.19x at 4, 1.10x at 16, 1.25x at 64, with bytes per op equal.
  Rebuilding from the live slots allocates what appending did, and the
  prune is an extra `Arr` node per statement, which costs more than the
  shallower projections save.
- Reverted. The commit and its revert stay in the history. The benchmark
  stays as the instrument. The numbers are in src/jmh/history.tsv and in
  specs/proc-notation.md, whose box now says refuted instead of "not
  built". The "filed with its trigger" that box claimed had never reached
  a board.
