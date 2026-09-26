## handler-single-pass - pass fusion re-measured: it is the program's shape, not the handlers

- FusionBenchmark was re-run, each lane its own `jmh-lane.sh` run, in two
  alternated rounds. Fusing State+Writer into one pass is 1.36x (and
  1.31x with Throws) on a foldLeft-built program, and 1.05x on the
  right-nested twin. The bytes saved are the same 26 KB in all three.
- `-prof stack` puts 60-70% of the foldLeft program's time, fused or
  not, in `Free.resume`'s rotation. The same 1000 operations take 32 µs
  left-nested and 13 µs right-nested. The generic fused runner is not
  built. `left-nested-build-cost` (backlog) is the lever: library
  builders that build right-nested first, and a continuation queue only
  if needed.
- DelimBenchmark/SplitBenchmark learn `State.Modify`. They carried six
  E029 warnings in Jmh sources since effect-row-recursion-cost, which
  `Test/compile` does not reach. Spec specs/handler-fusion.md.
