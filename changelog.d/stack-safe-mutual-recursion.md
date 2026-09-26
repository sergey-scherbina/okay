## stack-safe-mutual-recursion - unbounded mutual recursion, every road measured

- `MutualRecursionBenchmark`: isEven/isOdd at 1 000 000 levels, which
  plain JVM recursion cannot run at all (the trial setup proves the
  overflow). Lanes: the state-machine floor; trampolines (hand-written,
  TailCalls, cats Eval, cats IO, ZIO, kyo IO, okay Free, okay Cont);
  and the roads that are not trampolines (a 1 GB-stack thread,
  Cheney-on-the-MTA exceptions, virtual threads as stack segments,
  Iterator.iterate).
- `MutualRecursionFxBenchmark`: the same with a counter and a logger
  passed as interfaces on every road, and okay's effect row as
  written. Its okayRow reading (35 ns, 376 B a level) is what
  effect-row-recursion-cost fixed.
- Measured PARTIALLY (stopped by the operator to free the box for the
  performance fixes): okay's trampolines at 2.2-2.3 ns a level beside
  cats Eval and the hand trampoline (1.9 ns), ahead of ZIO, kyo and
  cats IO. docs/benchmarks.md §2e.
