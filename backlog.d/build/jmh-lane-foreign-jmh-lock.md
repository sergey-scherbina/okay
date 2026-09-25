- [ ] jmh-lane-foreign-jmh-lock — `scripts/jmh-lane.sh` reads a JMH
      run that died on `Unable to acquire the JMH lock
      ($TMPDIR/jmh.lock)` as "the run itself failed (exit 1), box was
      quiet — a real failure, not contention", and gives up. It is
      contention: a JMH started outside the script (its own `.work/jmh`
      lock is not JMH's lock) holds `jmh.lock`, and `quiet.sh` let the
      lane start beside it. Found 2026-09-25 by okay-compress-zstd-speed:
      a sibling's `Benchmark.fib` fork in another worktree held it at
      98% CPU. Fix: grep the lane's log for that message and treat it
      like a busy box (wait, retry); and count a live
      `org.openjdk.jmh.runner.ForkedMain` burning CPU as "not quiet".
      A fork ASLEEP at 0% is the idle reaper's orphan (AGENTS.md) and
      is run past with `-Djmh.ignoreLock=true`, not waited on.
