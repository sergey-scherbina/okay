## jmh-lane-foreign-jmh-lock - a JMH run outside jmh-lane is contention, not a failure

- `scripts/jmh-lane.sh` keeps each run's output and reads it. A run
  that died on "Unable to acquire the JMH lock" is contention: another
  JMH, started outside the script, holds `$TMPDIR/jmh.lock`. The script
  now waits for that JMH's forks (up to 30 min) and retries, where it
  used to exit with "a real failure, not contention" (twice on
  2026-09-25).
- The exit code comes through a file, not the `tee` pipe.
- Selftest cases 7 (the lock message is retried) and 8 (a real failure
  is still final). The negative checks in the selftest were
  `grep -qv`, which passes whenever ANY line lacks the word; they are
  `! grep -q` now, case 1's included.
