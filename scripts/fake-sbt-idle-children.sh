#!/bin/sh
# The hang gate-watchdog-idle-sbt-cpu found (2026-09-23, twice): sbt
# ITSELF keeps burning a little CPU (GC, a reaper thread per child)
# while every child it waits on — node, a Native test binary, a
# forked JVM — sits at 0%. Here the HOST is this shell, spinning in
# builtins only (no fork, so none of its CPU lands on a child), and
# the child is an idle `sleep`. The spin is finite, so the same fake
# is also the control: with the host threshold at 0 the host's CPU
# counts as work, the run survives and reaches its verdict.
echo "[info] welcome to fake sbt (busy host, idle children)"
echo "[info] Test run started (the runner never answers)"
sleep 600 &
i=0
while [ "$i" -lt "${FAKE_SPIN:-3000000}" ]; do i=$((i + 1)); done
kill $! 2>/dev/null
echo "[info] Passed: Total 1, Failed 0, Errors 0, Passed 1"
