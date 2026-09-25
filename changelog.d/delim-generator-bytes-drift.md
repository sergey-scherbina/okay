## delim-generator-bytes-drift - the ledger's delimGenerator baseline, bisected to one commit

A control run in lexical-tail-guard-abort read DelimBenchmark.delimGenerator
at 942 314 B/op on master where delim-dollar's row said 910 330. Bisected
2026-09-25 with jmh-lane, one worktree per sha, bytes only (exact).

- stack-safety-core (b27ae0c5d): parent 910 312, the commit 942 312, a
  later cont-stack commit 942 312. The whole drift is `Delim.split` as a
  loop: a `Wrap.On` node and its polymorphic frame per segment the walk
  passes, +32 B per capture, where the recursive version used the JVM
  stack. Kept as the price of the loop the stack-safety rule requires;
  backlog `delim-split-wrap-free` has the recipe to take it back.
- specs/stack-safety.md's `Delim.split` bullet states the price;
  specs/shift0-dollar.md's Results note the moved baseline; history.d has
  the rows.
- On the way: two bisect drivers were wrong before one was right — the
  old shas have no scripts/jmh-lane.sh, and running the main checkout's
  copy runs the lane IN the main checkout (the script derives its root
  from its own path). The fix is to copy jmh-lane.sh, quiet.sh and
  jdk-pin.sh into the old worktree and run that copy.
