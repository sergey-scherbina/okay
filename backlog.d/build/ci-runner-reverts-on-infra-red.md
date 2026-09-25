- [ ] ci-runner-reverts-on-infra-red: `scripts/ci-runner.sh` reverted a
      landing on a whole-build RED that contained no failing test.
      2026-09-25 12:34: e0b3f53ee reverted stack-safety-json (ad34a6d11).
      The gate log had zero `==> X`. Its red was infrastructure: a Scala
      Native test binary "interrupted by fatal signal 9", and
      `okayAsyncNative / Test / loadedTestFrameworks` failing with
      "Accept timed out" at load ~100. okay-async does not even depend on
      okay-codec, the module the lane changed. gate.sh itself calls such
      a run "a failure this script does not recognise". The runner then
      reverted the one landing in range without a bisect. What would
      settle it: the runner treats a red with no `==> X` as NO VERDICT
      (as gate.sh's KILLED/STALLED are), re-runs once on a quieter box,
      and reverts only on a red that names a test. The lane was re-landed
      by hand. The same kill recurs: two staged gates of the re-landing
      lane that afternoon lost okay-chain's and okay-conf's Native test
      binaries to signal 9 (then "Accept timed out"), each green when
      re-run alone. A kill of a Native test process is therefore routine
      on this box, and a revert keyed on it will keep firing. (2026-09-25)
