- [ ] **hedge-start-timing-flake** — `TestHedgeStart."an attempt forked
      while the answer arrives leaves neither a running attempt nor an
      armed timer"` fails a landing gate with "timed out waiting for
      the first attempt to answer" when the box is loaded (1-minute
      average ~19, three sbt matrices), and passes alone on the same
      tree seconds later. Same family as `parse-depth-tests-out-of-the-
      gate`, just landed: a wall clock on a shared box. Either the wait
      needs to be a condition rather than a deadline, or the test
      belongs in `integrationTest`. Seen 2026-09-10 by the
      dataflow-numbers gate, which does not touch okay-resilience.
      SECOND SIGHTING 2026-09-17, continuations-audit's gate: same
      test, same message, in a run of 4498 results whose only other
      failure was none — and the lane touches `Delim` and docs, while
      okay-resilience never names `Delim`. 3 of 3 green in isolation
      on the lane's own tree minutes later. Two sightings from two
      lanes that cannot have caused it is no longer one ledger entry:
      the owner's choice between `Live` and bound-based assertions is
      now overdue.
      THIRD SIGHTING 2026-09-17, workflow-suspended-driver's gate —
      same test, same message, another lane that cannot have caused
      it (okay core and okay-persist).
      FOURTH SIGHTING 2026-09-18, gate-bound-test-fanout's gate — same
      test, same message, and the lane changes ONE LINE of build.sbt
      and nothing else. Box at load 76 under a sibling's gate; 3 of 3
      green in isolation on the same tree minutes later. Four lanes,
      none of which can have caused it, is the whole argument: the
      assertion is about the scheduler, not about hedging. Option (a)
      above is the one to take.
      ONE THEORY TESTED AND NOT CONFIRMED, recorded so nobody spends
      the same hour twice: the suite's `until` helper waits by
      spinning on `Thread.yield()` for up to five seconds, and a
      yield-spin BURNS a core rather than waiting — plausibly the core
      the fibre it waits for needs. Changing it to `Thread.sleep(1)`
      is obviously no worse, but the repro DID NOT REPRODUCE: with 12
      CPU burners and a 1-minute load of 22, the ORIGINAL `yield`
      version passed. So the starvation theory is unproven and the
      change was reverted rather than landed on a guess. Whoever picks
      this up: synthetic CPU load is not the shape that breaks it —
      the failures all happened under a full matrix, which is many
      JVMs with many threads and a lot of I/O, not a busy loop.
      FOURTH SIGHTING 2026-09-17, a book lane whose ENTIRE DIFF IS
      PROSE — five markdown files under docs/continuations and not one
      line of executable code. That settles the remaining doubt: no
      lane's code perturbs this, because this lane has no code. What
      the four have in common is only the full matrix, which means the
      test is measuring the box and calling it a behaviour.
      THE DECISION IS OVERDUE AND THE EVIDENCE IS NOW COMPLETE: a
      five-second wall-clock deadline inside a suite that runs beside
      ninety other module runs is not a bound anybody can defend. Move
      it to `integrationTest`, or assert on a CONDITION rather than a
      deadline (`Live` exists for this). Not done here on purpose: a
      documentation lane must not carry a fix to okay-resilience, and
      the owner's choice between the two shapes is a design decision,
      not a patch. Four gates have now paid for it.
