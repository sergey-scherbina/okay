- [ ] ci-runner-flake-before-bisect — a load-only red costs a whole bisect
      before the confirmation step calls it a flake. 2026-09-25: the
      whole build went RED at 21:00 on `okay.clojure.TestCoreAsyncChannelLaws`
      ("cannot load namespace clojure.core.async" — green alone on master,
      probed in a detached worktree); the runner bisected ~180 landings
      with an `affected` gate per step until 23:40, converged on
      2a1ebdcd8 (foreign-facade-3, which touched okay-foreign-cluster
      only — a bisect over a flake converges anywhere), re-ran that
      commit's own gate alone, found it GREEN and did not revert. The
      design held; the cost was 2 h 40 min with origin ~200 commits
      behind. The cheaper order: BEFORE bisecting, re-run the failing
      suites alone on HEAD once (`testOnly` of the `==> X` suites, on a
      quiet box, as the confirmation already does for the culprit); a red
      that is green alone is a flake with no culprit to find, and the
      whole-build turn should just run again. Gate:
      ci-runner-selftest.sh with a fake whose whole build is red and whose
      suite alone is green, asserting no bisect ran. (2026-09-25)
