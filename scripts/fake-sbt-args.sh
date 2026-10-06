#!/bin/sh
# A "build" that only reports the commands it was handed, one per line,
# so gate-selftest can see how gate.sh split its argument
# (gate-command-chain: `gate.sh "a; b"` used to run `a` alone).
echo "[info] fake-sbt-pid: $$"
if [ -n "${FAKE_SBT_REQUIRE_LOCK:-}" ]; then
  [ -f "$OKAY_CI_LOCK_DIR/pid" ] || { echo '[error] shared CI lock missing'; exit 1; }
  echo "[info] fake-lock-holder: $(cat "$OKAY_CI_LOCK_DIR/pid")"
fi
for a in "$@"; do
  echo "[info] fake-sbt-arg: <$a>"
  if [ -n "${FAKE_SBT_FAIL_COMMAND:-}" ] && [ "$a" = "$FAKE_SBT_FAIL_COMMAND" ]; then
    echo "[error] selected fake stage failed"
    exit 1
  fi
done
echo "[info] Passed: Total 1, Failed 0, Errors 0, Passed 1"
