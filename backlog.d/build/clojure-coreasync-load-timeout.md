- [ ] clojure-coreasync-load-timeout — PRIORITY: MEDIUM (a flaky test in the default gate).
      `okay.clojure.TestCoreAsync` "okay produces, a Clojure `into` consumes"
      timed out after 30 s in a full `affected master` gate at load 35-62
      (lexical-tagged-walk, 2026-09-25), and passed twice at once in isolation
      at load 62. Its second sighting: ci-runner-lock-bypass records the same
      load-induced timeout. The repository's policy is that a test whose result
      depends on timing it cannot control is not in the default gate. THE LANE:
      read what the test waits on, then decide whether it is a real wait on a
      slow box (widen nothing, move the timing out of the assertion) or a hang
      that load exposes (dump it the way the channel hangs were dumped). Tag it
      `Live` only if the dependence on time is inherent. (2026-09-25)
