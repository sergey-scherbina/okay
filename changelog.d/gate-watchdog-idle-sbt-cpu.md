## gate-watchdog-idle-sbt-cpu - the stall watchdog judges sbt and its children apart

The watchdog in `scripts/gate.sh` missed the hang it was written for
twice on 2026-09-23 (a ForkTests Acceptor waiting on a vanished JVM;
two okay-ui-gtk Native binaries at 0% for 16 minutes): it summed CPU
over the whole tree, and sbt ITSELF burns ~3 s/min at idle (GC, a
process-reaper thread per child) — 26 s per 8-minute window against a
5 s bar — so a tree of idle children read "still working" for ever.
The tree is now split: the HOST (the root and whatever is sbt's own
JVM by its command line, `GATE_HOST_PATTERN`) and the WORKERS (node,
Native test binaries, forked JVMs). A stall is silent AND the workers
under `GATE_STALL_CPU` (5 s) AND the host under `GATE_STALL_HOST_CPU`
(60 s per window — twice its idle overhead, far under a cold compile,
which is the host working with no worker at all and must survive).
`scripts/gate-selftest.sh` gains both shapes with a new fake
(`fake-sbt-idle-children.sh`, a host spinning in shell builtins over
an idle `sleep`): idle children under a busy-ish host are STALLED and
killed by PID; the same shape with the host counted as work survives
to GREEN. Case 4 was run against the OLD watchdog first and never
stopped (the selftest hit its timeout) — the defect reproduced before
the fix. PASS under bash and /bin/sh; a real gate through the new
watchdog green. AGENTS.md's watchdog paragraph says it. Landed as 92698b02.
