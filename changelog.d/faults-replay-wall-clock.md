## faults-replay-wall-clock - TestFaults' replay-by-seed law is back in the default gate, off the wall clock

- The law (two sessions with seed 11 tell the same story) was `Live` for
  a day (flaky-faults-replay-live): red in a ci-runner whole build under
  load, green alone. The backlog entry's fix — a frozen clock for the
  budget, as the limiter already had — was not enough on its own:
  `Resilient.http` already took `clock`, and a frozen one reads the
  budget as never spent, but `Deadline.enforce` ARMS the given platform
  timer for the second it says is left, so a starved session still met
  a real deadline where the first had not.
- `Resilient.http` takes `budgetTimer: Option[Timer] = None`, the timer
  its deadlines are armed on; absent, the given one, so no caller
  changes. The session in TestFaults hands it a frozen clock and a
  `ManualTimer` nobody fires: the budget is present, in the composite's
  shape, and cannot cut a call; the wire's 2 ms slow calls stay on the
  platform timer, which only ever finishes them later, never differently.
  `budget.pending == 0` after the 40 calls is a new assertion (every
  deadline was disarmed by its call's completion). The `Live` tag is
  gone; TestFaults 5/5 green in the default gate, with TestHedgeStart
  and TestResilient (13 results, no warnings).
- `ManualTimer` is one test class for the module now
  (`okay-resilience/src/test/scala-jvm/.../ManualTimer.scala`):
  TestResilienceTimed and TestHedgeStart each carried a copy, and this
  was the third user.
