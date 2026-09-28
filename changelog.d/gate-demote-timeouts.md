## gate-demote-timeouts - a demoted run's munit timeouts are DEMOTED, not RED

- With `OKAY_BENCH_DEMOTE=on` a gate that meets a queued benchmark runs
  on the efficiency cores (`taskpolicy -b`), and heavy tests miss their
  munit timeouts: one-bind-hot-steps' whole build went red on nine
  timeouts in seven modules that were green undemoted (TestGenerate's
  1M: 6.3 s on the performance cores, 254 s demoted against 120). A
  demoted run's timeout is a verdict on the cores, not the tree.
- `gate.sh` now writes a marker line into its own log when it ran
  demoted, and a run with that marker whose EVERY `==> X` is a
  `TimeoutException` ends `gate: DEMOTED`, exit 122 — a no-verdict
  `gate-retry.sh` retries like a kill or a stall, and `--read` on the log
  classifies the same way. One real failure beside the timeouts, or the
  same timeouts in a run that was not demoted, stay RED: a timeout
  excuses nothing but itself. Fixtures `demoted-timeouts.log` and
  `demoted-beside-a-real-failure.log`; `gate-selftest.sh` case 11 runs
  the three directions.
