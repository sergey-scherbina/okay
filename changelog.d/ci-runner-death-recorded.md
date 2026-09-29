## ci-runner-death-recorded - a detached runner starts in its own session and writes down how it died

Toward sprint item ci-runner-startup-death (a detached `ci-runner.sh once`
vanishing mid `family all` with nothing in its log after sbt started).

- `kick` starts the detached `once` in a NEW SESSION: `setsid` where it
  exists, `perl -MPOSIX -e 'POSIX::setsid(); exec @ARGV'` on macOS, which
  has no setsid(1), plain otherwise. `nohup` alone only ignores SIGHUP
  and left the run in the kicking tool call's process group, the group a
  harness may signal when the call ends (AGENTS.md, "Exit 143 is SIGTERM",
  sender 1).
- Each turn writes its pid, process group, session and parent, and a
  caught HUP, INT or TERM is logged as "got SIG<x> during '<phase>'"
  before the lock is released. A log that stops with no such line was a
  SIGKILL, which no script can catch; AGENTS.md names who sends those.
- `ci-runner-selftest.sh` case 9c: a kicked run is in a session of its own,
  a TERM to its group is written with the phase, the lock is released and
  nothing is pushed. PASS under sh and bash. (It uses `kill -s TERM --`:
  dash reads `kill -TERM -- -pgid` as an illegal number.)

Not reproduced here: in the cloud harness a nohup-only child outlived its
tool call, so whether the Mac's harness kills the group is still open.
The item stays in the sprint with the next step written on it.
