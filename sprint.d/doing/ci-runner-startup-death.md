- [ ] ci-runner-startup-death — identify why detached `ci-runner.sh once` exits during `sbt family all` before the gate emits a verdict. Correlate runner, gate and process records; reproduce through `scripts/gate-sentinels.sh` or an equivalent signal recorder without disrupting another agent's build. Record the responsible process or a bounded refutation, then fix the runner launch or file a precise follow-up. Done when the evidence names the termination source or the next diagnostic boundary, and a retry can run safely.
      PROGRESS 2026-09-29 (the operator asked a cloud session to take a
      look; codex's claim of 2026-09-28 still stands and was not taken
      over). Landed as changelog.d/ci-runner-death-recorded.md: a detached
      `once` now starts in a NEW SESSION (setsid on Linux, perl's
      POSIX::setsid on macOS, which has no setsid(1)), since `nohup` alone
      left it in the kicking tool call's process group; and every turn
      writes its pid/pgid/sid/ppid and, on HUP/INT/TERM, "got SIG<x> during
      '<phase>'" before releasing the lock (selftest 9c, sh and bash). NOT
      reproduced: in the cloud harness a nohup-only child survived its tool
      call too, so the group-kill reading is a hypothesis for the Mac's
      harness. NEXT: kick once on the Mac. If it survives, the new session
      was the fix. If it dies, its log names the signal and phase; a log
      that stops with NO such line was SIGKILL, and then
      ~/Library/Logs/kill-stale-builders.log names the killer (AGENTS.md,
      "THE 143, SOLVED").
