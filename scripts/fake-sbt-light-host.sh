#!/bin/sh
# A HOST THAT WORKS LIGHTLY (gate-selftest-busyhost-load, 2026-09-27):
# short bursts of CPU and a sleep between them — well under a second of
# CPU per stall window — with an idle child. The watchdog summed CPU in
# WHOLE seconds, so a window in which the host burned 0.3 s read as 0
# and a working host was called STALLED; on a loaded box the fixture of
# case 5 (a starved spin) did the same by accident. This one does it on
# purpose, whatever the load.
echo "[info] welcome to fake sbt (a light host, idle children)"
sleep 600 &
end=$(( $(date +%s) + ${FAKE_SECS:-16} ))
while [ "$(date +%s)" -lt "$end" ]; do
  i=0
  while [ "$i" -lt "${FAKE_BURST:-5000}" ]; do i=$((i + 1)); done
  sleep 1
done
kill $! 2>/dev/null
echo "[info] Passed: Total 1, Failed 0, Errors 0, Passed 1"
