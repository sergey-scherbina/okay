- [ ] ci-runner-confirm-at-culprit — what is left after
      ci-runner-flake-before-bisect (2026-09-26), which now re-runs a
      whole-build red's own suites alone on HEAD before any bisect, so a
      flake no longer reaches the bisect at all. Still open: (1) a red
      that REPRODUCES is bisected and its culprit confirmed by
      `gate.sh "affected $from..$culprit"` on HEAD's tree — for a culprit
      that touched build.sbt that is every module, a second chance for an
      unrelated flake to "confirm" it; confirming by the reproduced SUITES
      at the culprit's tree would say more. (2) a landing is reverted as
      its tip commit, not as the lane's commits since the previous tip.
      Gate: ci-runner-selftest cases for both.
