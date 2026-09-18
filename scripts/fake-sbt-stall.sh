#!/bin/sh
# A "build" that goes silent and idle — the shape gate.sh's watchdog
# exists to catch. The `sleep` is a CHILD, so killing this also tests
# that the watchdog walks the tree rather than just the parent.
echo "[info] welcome to fake sbt (stall)"
echo "[info] compiling 1 Scala source to /dev/null ..."
sleep 600
echo "[info] this line is never reached"
