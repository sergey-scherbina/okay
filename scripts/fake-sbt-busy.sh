#!/bin/sh
# A "build" that is silent because it is COMPILING — the control for
# the watchdog. It must survive: silence alone is not a stall.
echo "[info] welcome to fake sbt (busy)"
echo "[info] compiling 500 Scala sources ..."
awk 'BEGIN { x = 0; for (i = 0; i < 60000000; i++) x += i; exit 0 }'
echo "[info] Passed: Total 1, Failed 0, Errors 0, Passed 1"
