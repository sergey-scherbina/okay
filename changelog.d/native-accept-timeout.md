## native-accept-timeout - gate.sh knows the third lost-process shape, and a starved rerun is KILLED, not RED

The Native test adapter (test-runner 0.5.12) waits 40 s for a test binary
to connect back (`ComRunner`, `setSoTimeout(40000)`); past that it logs
`Force close … Accept timed out`, kills the binary (exit 137, "fatal
signal 9") and sbt fails `(<m> / Test / loadedTestFrameworks)` before any
test ran. gate.sh knew two shapes of a lost test process and called this
one "a failure this script does not recognise" — and a RED is what the
ci-runner bisects, so on 2026-09-25 it reverted a green lane
(stack-safety-json) for it.

- `scripts/gate.sh`: shape C is recognised ONLY together with the
  `Accept timed out` line (a `loadedTestFrameworks` failure without it
  stays RED), and those modules are re-run alone like A and B. A rerun
  that loses its binary the same way, with no `==> X` and no `Failed`
  above zero, prints `gate: KILLED` (exit 137): the box that starved the
  matrix starves one module too at load ~100, and that says nothing
  about the tree. gate-retry retries KILLED; the runner never bisects
  it. The rerun now goes through `GATE_SBT`, so it is testable.
- `scripts/gate-fixtures/`: verbatim lines of the real logs — shape C,
  shape C beside a real `==> X`, `loadedTestFrameworks` without the
  accept timeout, and shape B — and `scripts/fake-sbt-accept-timeout.sh`.
  `scripts/gate-selftest.sh` section 9 reads all four in both directions
  and drives the whole path to KILLED. `--read` over the three saved
  logs of the day (okayChainNative, okayConfNative, okayAsyncNative)
  went from RED to "lost a test process, would re-run".
- `scripts/native-timeline.sh [out.tsv] [secs]` samples every Native
  test binary on the box once a second (pid, state, cpu, rss) and
  `--report` gives each one's lifetime and states — the timeline the
  ledger kept asking for. First run, 24 min beside sibling gates at load
  66–100: 123 binaries, two matrices holding 67 and 52 binaries alive AT
  ONCE, lifetimes up to 430 s, and NO accept kill (no binary died near
  40 s). At most 2 binaries were runnable in any sample — the rest sat
  asleep at ~0.01 s of CPU after connecting. So the binary is not
  CPU-starved while it waits, which weakens "a starved start" as the
  mechanism of C: the 40 s clock may be lost on the ADAPTER's side
  (sbt's JVM, or the launch itself) instead. Open, and the probe is
  what a next sighting needs running beside it.
- The `native-runner-error` ledger is split into its two mechanisms:
  B, exit 0 with nobody killing it (settled 2026-09-09), and C, the
  adapter's own kill of a binary that did not connect within 40 s. Its entries had written
  "nothing changes the settled cause" under readings that named C.
  The runner side of the same incident is ci-runner-revert-confirm's
  (landed alongside): a red with no `==> X` is not bisected at all.
  This lane is the source side — the shape is no longer an unknown red,
  so the gate clears it itself wherever it runs, pre-merge included.
