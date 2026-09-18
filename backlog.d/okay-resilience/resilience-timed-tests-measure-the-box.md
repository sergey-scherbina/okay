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
      DONE MEANS: a full matrix under load with both suites in it,
      three times, no failure — and the assertions readable as claims
      about hedging rather than about timing.
      COST SO FAR: five gate cycles, each roughly ten minutes, plus
      the investigation hour. Paid by five different lanes, none of
      which owned this module.
