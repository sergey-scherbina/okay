- [ ] hedge-start-hang-under-load — `okay.resilience.TestHedgeStart` ("an
      attempt forked while the answer arrives leaves neither a running
      attempt nor an armed timer", JVM) HUNG to its 30 s munit timeout
      (60.0 s wall) in wire-tls's full gate on 2026-09-24, at load
      average 28 during the matrix. The same tree passed it 3 of 3 alone
      minutes later (load 11–16). wire-tls touches no resilience code
      (okay-codec gained classes only). A hang is not a slow run: before
      calling it a flake, take a thread dump of one (does an attempt wait
      on a timer that was never armed?), which is the lost-wakeup shape
      `parked-workers-refute-exhaustion` found in the scheduler. Tag it
      Live only if it is proven to be scheduling rather than a race.
