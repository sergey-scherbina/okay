- [ ] clojure-cancel-wall-clock — `okay.clojure.TestClojureCancel` "the
      default scheduler: a cancel interrupts (ok/lift f)" asserts a
      WALL-CLOCK bound (`ms < 1000` for a 1500 ms sleep cancelled at
      200 ms) in the DEFAULT gate. py-arrow's gate (2026-09-25, load
      ~200) read 1337 ms, marked=false: the cancel DID land (the mark was
      not left), only late. Alone, minutes later, green. By the "no flaky
      tests in the default gate" policy either the bound is judged on
      the MARK alone (the cancel's effect, which load cannot fake) with
      the time only printed, or the suite is `Live`-tagged. The mark is
      the property; prefer the first. (2026-09-25)
