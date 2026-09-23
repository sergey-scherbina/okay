#!/bin/sh
# A "build" that only reports the commands it was handed, one per line,
# so gate-selftest can see how gate.sh split its argument
# (gate-command-chain: `gate.sh "a; b"` used to run `a` alone).
for a in "$@"; do echo "[info] fake-sbt-arg: <$a>"; done
echo "[info] Passed: Total 1, Failed 0, Errors 0, Passed 1"
