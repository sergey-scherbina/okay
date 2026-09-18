- [ ] **resilience-timed-tests-measure-the-box** — THE MODULE-LEVEL
      TASK the two entries below have been asking for one sighting at
      a time. It is one disease, not two flakes: this module's timed
      suites run in the DEFAULT gate and assert on wall-clock
      deadlines and exact counters, so under a full matrix they
      measure how busy the machine is and report it as a behaviour.
      THE EVIDENCE IS COMPLETE. Five failures across two suites, and
      EVERY ONE of them from a lane that cannot have caused it —
      dataflow numbers, an optics lane, continuations-audit,
      workflow-suspended-driver, and finally a lane whose entire diff
      is five markdown files with no executable code at all. Each
      passed in isolation on the same tree minutes later, 3-of-3 or
      better. One theory was tested and REFUTED (a yield-spin starving
      the fibre it waits for: 12 CPU burners at load 22 did not
      reproduce it), which is recorded below so nobody spends that
      hour again.
      WHAT IS ACTUALLY WRONG: `TestHedgeStart` waits five seconds by
      wall clock for a fibre to answer, and `TestResilienceTimed`
      needs three attempts to start 10 ms apart with the third winning
      at +5 ms and then asserts `starts == 3, cancelled == 2` exactly.
      Beside ninety other module runs, neither bound is defensible —
      not because the code is wrong but because the assertion is about
      the scheduler, not about hedging.
      TWO WAYS TO CLOSE IT, and the owner picks:
        (a) assert on a CONDITION rather than a deadline, and on
            BOUNDS rather than exact counters — `starts <= 3` and
            "eventually no attempt is running" say what hedging
            promises, and say it on any machine. `Live` exists for
            the parts that genuinely need real time.
        (b) move both suites to `integrationTest`, where a timed test
            is allowed to want a quiet box. Cheaper, and it takes the
            guarantee out of the gate that protects it.
      (a) is better and (b) is honest; what is not acceptable is a
      third year of ledger entries.
      TAKEN 2026-09-18 (hedge-bounds), option (a):
        - `TestHedgeStart.until` keeps the CONDITION as its assertion
          and turns the clock into a TRIPWIRE — 60 s, the line past
          which "slow" is "hung" — and SLEEPS instead of spinning on
          `Thread.yield()`, which on a loaded box can hand the core
          back to the one thread with nothing to do.
        - `TestResilienceTimed`'s hedge counters become the promise:
          a hedge happened (`starts >= 2`) and EVERY LOSER IS
          CANCELLED (`cancelled == starts - 1`), which is the only
          claim there that is about hedging rather than about time.
      ONE CORRECTION TO THIS ENTRY, found by running it: it names both
      suites as if both reached the gate. `TestResilienceTimed` is
      ALREADY `Live`-tagged and excluded from `sbt test` — so all four
      sightings are `TestHedgeStart` alone, and the timed suite's
      change is an improvement rather than a fix.
      SECOND REFUTATION OF THE BURNER THEORY, and it cost an hour
      because this entry already said it: 16 burners at load 93 did
      not reproduce the failure with the OLD code either (0 of 4,
      against 0 of 4 new, detector verified on a known-good run
      first). CPU pressure is not the condition. Every sighting was
      inside a FULL MATRIX — hundreds of processes, four sbt JVMs, GC
      pressure and paging — which is why DONE below says matrix and
      not burners. Do not spend a third hour on burners.
      ACCEPTED 2026-09-18: three full `affected master` matrices back
      to back, all GREEN, with `TestHedgeStart` in every one. WHAT
      THAT DOES AND DOES NOT SHOW, said plainly because the criterion
      above asks for "under load": those three ran while a docker
      image build held the VM and siblings were landing — ordinary
      traffic, not the four-matrix crush every sighting came from —
      and the OLD code also passed 4 of 4 at load 93, so no run of
      this size discriminates. What carries the change is that the
      assertions no longer name the scheduler: a condition with a
      hang tripwire, and `cancelled == starts - 1`. Three green
      matrices say it did not regress. The ledger stops growing or it
      does not, and the next sighting decides — if one comes, it is
      now a claim about hedging that failed, which is worth reading.
      DONE MEANS: a full matrix under load with both suites in it,
      three times, no failure — and the assertions readable as claims
      about hedging rather than about timing.
      COST SO FAR: five gate cycles, each roughly ten minutes, plus
      the investigation hour. Paid by five different lanes, none of
      which owned this module.
