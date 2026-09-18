- [ ] growing-order-instrumented-repro — REPRODUCE IT WITH EVIDENCE,
      which is not the same as reproducing it. Four sightings of the
      same shape now exist and a fifth adds nothing: what is missing
      is a break that SAYS WHICH ROAD IT TOOK. So the run is
      instrumented first and long second.
      WHAT TO INSTRUMENT, both named by the retraction in BUGS.md as
      the candidates not yet examined:
        1. the PARKING path — `sendersAt(route)` and the resumed
           `pushDecidingAtOnBehalf`, which by design does NOT repair a
           stale route the way `pushDecidingAt` does (`ours`). Count
           resumes whose parked route is not the part the producer
           would own now.
        2. the STALENESS window around `Growing.inner` — `grow()`
           sets `grown` by CAS, builds the `AdaptiveFifo`, and only
           then assigns `inner`, so a reader seeing `grown == true`
           can still get the ring. Safe for routing (a stale `inner`
           is the ring, and pushing there is correct); not obviously
           safe for everything else.
           CORRECTED 2026-09-18: this said "a plain `var` ... has no
           happens-before edge". `inner` is `@volatile` and has been
           since the file was born (3f3adca1) — there is no
           publication hole, only the window above. The counter is
           shaped by the correction: a test cannot see `grown`, and
           should not be given a back door to it, so the probe counts
           the OBSERVABLE CONSEQUENCE instead — a push landing in
           part 0 after some push had already landed in a part above
           it. Part 0 is the adopted part and is drained first, so
           that is precisely the damage.
      THE PROBE EXISTS: `src/test/scala-jvm/ProbeGrowingOrder.scala`
      (growing-order-probe, 2026-09-18), ignored by default, with a
      `Traced` Buffer decorator injected into the SHIPPED construction
      (`SentinelChannel(Growing(Ring(4), 8, () => Ring(4)))`) so the
      production code carries no instrumentation at all. It prints,
      on a break, the offending producer's every push with the part it
      landed in, the route it asked for, whether it was resumed on
      another thread, and both counters. Its header states what it
      COSTS: one map lookup and one atomic per push, which can mask a
      race this narrow — so a quiet run with tracing on is evidence
      about the traced build, not about the bug. RUN IT under
      `OKAY_PROBE_ROUNDS=40000 OKAY_PROBE_BURNERS=20` — environment, not
      `-D`, because this module forks its tests and the property never
      reached the test JVM.
      RUN, AND THE MECHANISM IS NAMED — 2026-09-18. It is NEITHER
      candidate (BUGS.md, "THE MECHANISM, NAMED"). The parking path
      fired 0 times on every break, nothing went back into the adopted
      part, and no route was stale. A producer's elements are SPLIT
      across the swap — 1/3/5 in part 0, 7 onward in part 1 — and the
      CONSUMER passed part 0 before 5 landed there, drained part 1,
      then came back for 5. "Part 0 is read first" holds per SCAN, not
      globally. Closed as a question; reopened as a design one,
      `growing-order-drain-guarantee` below.
      TWO METHOD FINDINGS worth as much as the answer: the full trace
      MASKS the race (off, the break came at round 1959 of 2000; on,
      6000 rounds found nothing), so the instrument is one byte per
      element written once by its pusher; and the probe counts the
      rounds in which the buffer actually GREW, because "no break in N
      rounds" says nothing until the swap happened (40 000 of 40 000).
