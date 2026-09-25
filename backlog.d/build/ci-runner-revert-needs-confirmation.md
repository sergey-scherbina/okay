- [ ] **ci-runner-revert-needs-confirmation — the runner reverts on ONE
      red whole-build run, with no re-check.** Two false-positive
      reverts landed for real on 2026-09-25, both load artifacts, both
      caught and fixed by hand afterward: `mrjar-jdk25-ci-gap`
      (`TestSignals` under load, now `Live`-tagged) and
      `stack-safety-json` (a Native runner killed by signal 9 plus an
      `okayAsyncNative` accept timeout — the change was green on its
      own scoped gate and on okay2-codec). Neither red repeated on a
      second, independent run. `gate.sh` already has this exact
      instinct for one KNOWN false-red shape
      (`native-runner-error`: a lost Native process, re-run alone
      before trusting it) — the runner's bisect+revert path (stage C,
      specs/ci-staged.md) should get the same treatment generically:
      before reverting the sole (or bisected) culprit, re-run ONLY that
      commit's own gate once more; a run that goes GREEN the second
      time is a flake, not reverted, and logged as such instead. Ties
      to `ci-runner-lock-bypass` (same date): both incidents happened
      on a box under genuine, heavy concurrent load — a re-run needs
      its own quiet-wait too, or it just repeats the same false red.
      (2026-09-25)
