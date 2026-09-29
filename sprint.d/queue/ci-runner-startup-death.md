- [ ] ci-runner-startup-death — a detached `ci-runner.sh once` exited
      during `sbt family all` before the gate gave a verdict; its log ended
      right after sbt started. WHAT IS DONE (ci-runner-death-recorded,
      2026-09-29): `kick` starts the run in a NEW SESSION (setsid, or
      perl's POSIX::setsid on macOS), since `nohup` alone left it in the
      kicking tool call's process group; every turn logs its
      pid/pgid/sid/ppid and writes "got SIG<x> during '<phase>'" on
      HUP/INT/TERM before releasing the lock (selftest 9c). The group-kill
      reading could not be reproduced in a cloud harness. NEXT, on the Mac:
      `sh scripts/ci-runner.sh kick` once. A run that reaches a verdict
      means the new session was the fix: close this. A death names its
      signal and phase in `.work/ci/log/`. A log that stops with no such
      line was SIGKILL: read ~/Library/Logs/kill-stale-builders.log
      (AGENTS.md, "THE 143, SOLVED"). The earlier claim (codex,
      2026-09-28) was released by the operator the same day.
