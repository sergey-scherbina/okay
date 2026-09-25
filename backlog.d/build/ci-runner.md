- [ ] **ci-runner — stage B/C of specs/ci-staged.md: the one serial
      whole-build runner that pushes.** `scripts/ci-runner.sh
      once|loop|kick|status|--read`, a `mkdir` lock under `.work/ci/`
      (gitignore it — `.work/active` is tracked, `.work/ci` must not be),
      `origin/master..master` as the range with no state file (what is
      pushed IS what was green), board-only ranges pushed at once,
      `gate.sh "family all"` (+ okay2's suite when the range touches it)
      otherwise, push on green, bisect in a detached worktree with
      `affected from..HEAD` and revert the first bad landing on red,
      `changelog.d/ci-revert-<slug>.md` + a room message. Then
      `land.sh` step 8 becomes `ci-runner.sh kick`, and AGENTS.md's PUSH
      rule becomes "land, then kick — the runner pushes". Selftest in
      `gate-selftest.sh`'s style with a fixture repo and a bare origin;
      the spec's Behavior lists every case. Stage A landed as ci-staged
      (2026-09-25). (2026-09-25)
