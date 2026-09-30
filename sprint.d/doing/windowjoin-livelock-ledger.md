- [ ] windowjoin-livelock-ledger — the ledger entry only (the `bugs`
      skill: a found bug is recorded in the module that owns the fix,
      okay-stream/BUGS.md, new file). Found 2026-09-30 while
      freer-base-remeasure waited for a quiet box: the CI runner's
      `family all` gate (pid 48118) hung 65+ min on a forked okay-stream
      test JVM at 104% CPU, thread okay-own-1-0 RUNNABLE in
      `WindowJoin.trim` (WindowJoin.scala:65) under `arrive`/`left` from
      `Pipe.loop$5`; `okay.TestWindowJoin` printed four tests, the fifth
      ("agreement with joinSorted on a bounded input …") never returns.
      The gate watchdog reads burning CPU as work, so nothing pushes and
      no benchmark runs until the fork dies. Dump kept at
      .work/ci/diag/20260930-windowjoin-spin-63346.threads.txt. The FIX
      is stream-join-windowed's (cd91b05ee), told in the room.
