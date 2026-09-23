- [ ] interop-lift-cancellation — UNCHECKED, filed as a question: a
      Frege `liftIO` step and a blocking Clojure call inside a program
      each run as ONE okay step (`Foreign`'s Lift kind, `Free.delay`),
      so cancelling the okay fiber is expected NOT to interrupt them
      mid-step. Write the test first (a lifted sleep of 10 s, a cancel at
      100 ms, assert the fiber ends promptly); if it fails, the fix is
      the one okay-py has — run the lifted action as an `Async` with a
      cancel that interrupts its thread — not a documented caveat.
