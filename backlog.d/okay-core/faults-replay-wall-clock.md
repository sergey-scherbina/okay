- [ ] faults-replay-wall-clock — okay-resilience's TestFaults "the composite
      under a drawn plan … replays by seed" asserts two sessions with seed 11
      tell the same story, but the session runs on the wall clock (the 1 s
      `budgetMillis` deadline, `slowMillis = 2`), so under load a second
      session can meet a deadline the first did not: red in a ci-runner
      whole build 2026-09-28 (TestFaults.scala:107), green alone. Tagged Live
      by flaky-faults-replay-live so it stops holding the push. Fix: give the
      composite an injectable clock for the budget as the limiter already
      has (`clock = () => 0L`), drive it virtually in the test, and untag.
      (2026-09-28)
