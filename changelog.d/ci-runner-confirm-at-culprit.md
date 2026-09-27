## ci-runner-confirm-at-culprit — confirm at the culprit's tree, revert its lane

`scripts/ci-runner.sh`: a bisected (or sole) culprit is confirmed by the
suites the whole build named, re-run in a detached worktree at the
culprit (red) and at the commit before its lane (green) — red at both
means the red predates the lane, and nothing is reverted. The revert is
the lane's commits (every commit of the range naming the culprit's
slug), in one revert commit, not the tip alone. A tips-only bisect that
ends "only skipped commits left" now names the one landing tip among
the candidates. Two bash-3.2 traps fixed (`case` inside `$( )`,
`"$suites—"` under `set -u`). ci-runner-selftest cases 10 and 13 made
tree-driven, 15 and 16 new; PASS under sh and bash; two mutants caught.
specs/ci-staged.md.
