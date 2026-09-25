## jmh-async-output-ignored - JMH's async-profiler reports are ignored, two committed ones removed

- `-prof async` writes `<Benchmark>-<Mode>-<params>/summary-cpu.txt`
  beside the project. okay-compress-zstd-speed's `git add -A` in its
  worktree committed two of them (~7 800 lines). They are removed, and
  `.gitignore` covers the four JMH modes' directories.
