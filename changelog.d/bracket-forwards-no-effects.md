## bracket-forwards-no-effects - `bracket` forwards the use-program's effects; the old one is `bracketNow`

- `bracket(acquire)(release)(use)` is now `Resource` in one expression.
  `use` may perform anything in its row, an outer handler answers it,
  and the release runs when `use` finishes, when a step throws, or when
  a forwarded `Async` step fails. This is the form cats, ZIO and kyo
  call bracket.
- The old function, which runs `use` to the end in place by the row's
  `Handler` under `try/finally`, is `bracketNow`, unchanged. Its callers
  (TestResource, TestDelimLimits, compare's ResourceBenchmark) are
  renamed. docs/benchmarks.md §7 says its numbers belong to
  `bracketNow`.
- Found by the new test: without `Free.delay`, `Resource.run` acquired
  when the program was BUILT, and a program run twice acquired once.
- random-clock-signatures is surveyed and decided (not in the core); the
  numbers are in its backlog item. Spec specs/core-gaps.md stage 5; off
  the sprint.
