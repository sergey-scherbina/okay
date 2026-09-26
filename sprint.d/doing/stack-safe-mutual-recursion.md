- [ ] stack-safe-mutual-recursion — the like-for-like task the
      plain-loop and virtual-call baselines are not: UNBOUNDED mutual
      tail recursion (isEven/isOdd at N = 1 000 000), which plain JVM
      recursion cannot run at all (a trial setup proves the
      StackOverflowError), so every solution needs machinery. Lanes:
      the state-machine loop a tail-call compiler would emit (floor), a
      hand-written trampoline, scala.util.control.TailCalls, cats Eval,
      cats IO, ZIO, kyo IO, okay Free (`!.tailcall`) and okay Cont.
      Measured in one series, docs/benchmarks.md. (2026-09-26,
      operator ask)
