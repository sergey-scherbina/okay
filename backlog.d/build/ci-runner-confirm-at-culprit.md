- [ ] ci-runner-confirm-at-culprit — the runner's confirmation re-runs
      `gate.sh "affected $from..$culprit"` in the MAIN checkout, so the tree
      it tests is HEAD's, not the culprit's (found 2026-09-26 with
      ci-runner-bisect-intermediate-commits). For a culprit that touched
      build.sbt the affected set is every module, and a load flake at HEAD
      "confirms" it — which is how parquet-codec's first commit was
      confirmed RED. Needs: confirm in the bisect worktree checked out AT
      the culprit (tip), and revert a landing as the lane's commits since
      the previous tip rather than its tip alone. Gate: ci-runner-selftest
      cases where HEAD flakes and the culprit tip is green.
