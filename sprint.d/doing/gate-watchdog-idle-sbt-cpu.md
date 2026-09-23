- [~] gate-watchdog-idle-sbt-cpu — the stall watchdog (gate.sh,
      `GATE_STALL_CPU=5` s per 8 min) did NOT fire on the exact hang
      it was written for. 2026-09-23, docs-adapters-merge's full
      matrix: log frozen at 00:42 for 20+ minutes, `jcmd` showed
      `sbt.ForkTests$Acceptor` in accept and a `ForkTests` task in
      `Thread.join`, NO `java` child at all, and ~100 `node` + Native
      test binaries under sbt, every one at 0.0% CPU. The watchdog
      printed "quiet for 480s but the tree burned 26s of CPU — still
      working" twice: sbt ITSELF (6g heap, one process-reaper thread
      per child, GC) burns ~3 s/min idle, which is five times the
      threshold. Killed by PID by hand; `okayDocsJVM/test` alone was
      green straight after, so the tree was not at fault. Fix
      candidates: subtract the sbt JVM's own CPU and count only its
      CHILDREN (the hang is children-idle), or read the dump's
      signature (Acceptor in accept + no java child) as a stall
      directly. `scripts/gate-selftest.sh` should gain the case: a
      busy-ish parent over idle children must die.

      RECURRENCE 2026-09-23 10:11 (clojure-effects-seqs' affected gate):
      a DIFFERENT idle shape, same blindness — two okay-ui-gtk Native test
      binaries at 0.0% CPU for 16 minutes, sbt's `ComRunner receiver`
      threads blocked in a socket read from them, no ForkTests Acceptor
      at all; the watchdog printed "quiet for 480s but the tree burned
      26s of CPU — still working" and would have waited for ever. So
      the fix is the children-only CPU count, not a dump signature: the
      two hangs share nothing but idle children under a busy-looking
      sbt. Killed by PID, rerun.
