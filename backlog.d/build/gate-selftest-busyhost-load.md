- [ ] gate-selftest-busyhost-load — `scripts/gate-selftest.sh` case 5
      ("the host WORKING survives") failed on a loaded box
      (2026-09-26, bench-window): its fake host is a shell spin
      (`fake-sbt-idle-children.sh`) and the watchdog counts CPU in
      WHOLE seconds per 6 s window with `GATE_STALL_HOST_CPU=0`, so a
      window in which the spin was starved of a full second reads 0
      and is called a stall. Alone it passes in 12 s on master and on
      the branch. Fix candidates: a longer window in that case, or a
      host threshold of "> 0 in any of N windows"; either way prove it
      by running the case beside 20 CPU burners (the channel-known-
      producers recipe). TRIGGER: the next red of that case, or anyone
      touching the watchdog. (2026-09-26, bench-window)
