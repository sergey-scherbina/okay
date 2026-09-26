## ci-runner-flake-before-bisect — reproduce a red before hunting a culprit

`scripts/ci-runner.sh` now re-runs, alone on HEAD, exactly the suites a
whole-build red named (`==> X` lines, ANSI stripped, the class part of
each name) before bisecting. Green alone is a flake: no bisect, no
revert, nothing pushed; the next whole-build turn re-tests the range.
Both runner incidents were flakes at HEAD — a 2 h 40 min bisect over
~180 landings (2026-09-25) and the false revert of parquet-codec's first
commit (2026-09-26). ci-runner-selftest case 14 (sh and bash); the
mutant that skips the re-run is caught. ci-runner-confirm-at-culprit is
narrowed to what is left.
