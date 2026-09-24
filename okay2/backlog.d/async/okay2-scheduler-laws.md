- [ ] okay2-scheduler-laws — the Scala 3 core's TestSchedulerLaws and
      TestAdaptiveScheduler are SOAKS (conservation of the Chase-Lev
      deque under thieves while it grows, the lost wakeup after a park,
      a fiber blocked for good in a raw call not hiding a later fork,
      the stuck-check waking a parked worker before starting a new one,
      fairness under a burst). okay2-platform's family test runs every
      scheduler's basic laws (par, spawn/join, a failure as a value, a
      fork from inside a fiber); the soaks are the port of those 350
      lines, run on the same `SchedulerFamily` shape. (2026-09-24)
