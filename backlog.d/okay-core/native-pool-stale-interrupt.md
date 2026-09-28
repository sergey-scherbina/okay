- [ ] native-pool-stale-interrupt — PRIORITY: LOW (trigger: a Native
      pool fiber cancelled after it finished). Found by
      supervised-waits-on-failure (2026-09-28). `Schedulers.pool`'s `Task`
      (okay-platform scala-native Platform.scala) records `runner` when
      its body starts and never resets it, so `cancel()` on a task that
      has already FINISHED interrupts whatever that worker is running
      now — the "never a later, unrelated one" its comment promises does
      not hold, and an interrupt landing in `TaskQueue.take`'s `wait()`
      kills the worker outright. The JVM `forkJoin` fix of the same lane
      is the shape: clear `runner` in a `finally` under a monitor that
      `cancel` also takes, so the interrupt can only land inside the task
      it was meant for. Pin it with a test that cancels a finished task
      while its worker runs a second one, and that second one completes.
