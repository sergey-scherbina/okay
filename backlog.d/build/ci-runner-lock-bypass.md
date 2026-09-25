- [ ] **ci-runner-lock-bypass — `.work/ci/lock` only stops another
      `ci-runner.sh` invocation; it does nothing against a bare
      `scripts/gate-retry.sh`/`scripts/gate.sh "family all"` run by
      hand.** Found 2026-09-25 the hard way: a manual "run the whole
      build right now, urgently" bypassed the lock entirely, racing a
      LEGITIMATE `ci-runner.sh once` that was already mid-run in the
      same main checkout — two `family all` sbt processes writing the
      same `target/` trees at once, which read as a real RED
      (`scala2probe`: `NoClassDefFoundError` on core classes) and was
      not. Compounded by killing the wrong process with `kill -9`
      instead of `kill_tree` (AGENTS.md is explicit about this and it
      still happened): the SIGKILL took only the outer `ci-runner.sh`,
      orphaning its `gate-retry.sh` child, which kept running
      unsupervised, eventually hit a load-induced timeout
      (`TestCoreAsync`), and reached a RED nobody was left to act on.
      Fix: `gate-retry.sh` (or `gate.sh` itself, for the `family`/`all`
      shape specifically) should take/check the SAME `.work/ci/lock`
      regardless of caller — a manual run either refuses when the lock
      is held, or takes it itself so a concurrent `ci-runner.sh once`
      refuses instead. `scripts/ci-runner.sh status` already reports
      the lock; the missing piece is enforcement outside `ci-runner.sh`
      itself. (2026-09-25)
