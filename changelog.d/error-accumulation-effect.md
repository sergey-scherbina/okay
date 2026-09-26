## error-accumulation-effect - Chronicle: errors that accumulate while the program goes on

- `Chronicle[E]` (src/main/scala/Chronicle.scala) has two operations.
  `dictate(e)` records an error and goes on; `halt` stops with what has
  been recorded. `confess(e)` is dictate then halt. `Chronicle.run`
  answers a `Verdict`: `Clean(a)`, `Warned(a, errors)` or
  `Failed(errors)`. The names follow Haskell's `these` (`MonadChronicle`).
- `Chronicle.all(xs)(f)` accumulates across elements (arrow-kt's
  `mapOrAccumulate`). Each element runs under its own handler, so one
  element's halt does not stop the next. All errors are recorded in
  element order, and it halts at the end if any element halted.
- The backlog item's first question, whether `Writer % E + Throws % E`
  is enough, is answered no in the spec. It takes the row's one `Throws`
  slot, its warnings vanish when the handlers run in the wrong order, and
  it has no `all`.
- TestChronicle: 5 tests, including a `Chronicle + Throws` row and
  100 000 dictates. Spec specs/core-gaps.md stage 3; docs/guide.md; off
  the sprint.
