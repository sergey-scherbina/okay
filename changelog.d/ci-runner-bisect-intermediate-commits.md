## ci-runner-bisect-intermediate-commits — bisect landing tips; never leave a revert half-done

The runner reverted parquet-codec's first commit (d3d92efdc): its bisect
tested every commit, and that one — two commits before the lane's own
docs-index fix — was red, while the lane's tip was green and the
whole-build red was two load flakes (TestCoreAsync, TestReadyMerge). The
revert conflicted with the lanes built on it and was left mid-revert in
the main checkout, blocking every sibling's merge until it was aborted
by hand. `scripts/ci-runner.sh` now bisects LANDING TIPS only (commits a
`release-claim: …, landed as <sha>` names; the rest are skipped) and a
conflicting revert aborts itself. ci-runner-selftest cases 12 and 13,
under sh and bash. What stays open: the confirmation still runs on
HEAD's tree (backlog: ci-runner-confirm-at-culprit).
