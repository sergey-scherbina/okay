## ci-native-flake - the nightly family job runs through gate.sh

`.github/workflows/ci.yml`'s nightly `family` job called `sbt
"family <platform>"` directly, so a `native-runner-error` recurrence
(a lost Scala Native test process — nothing in the log but
`RunTerminatedException`/an RPC channel closed) failed the whole
nightly run instead of being told apart from a real failure and
re-run alone, exactly what `scripts/gate.sh` already does for the
push-path `affected` job.

Any command but the `"affected <ref>"` form passes through `gate.sh`
untouched, so `sh scripts/gate.sh "family ${{ matrix.platform }}"` is
a drop-in swap for the bare `sbt` call — the same watchdog and
rerun-alone logic, reused rather than re-taught. Verified locally
that the generic pass-through and exit code work end to end (`sh
scripts/gate.sh "okayLexJVM/test"`: GREEN, exit 0) before wiring it
into a nightly job this session cannot run.
