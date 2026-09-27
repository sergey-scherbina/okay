## gate-selftest-busyhost-load — the gate's stall watchdog counts CPU below whole seconds

`gate.sh`'s watchdog calls a silent run STALLED only when its process
tree also burned (almost) no CPU in the window, and `cpu_of` summed that
CPU and then CUT it to whole seconds: a host that burned 0.3 s in a
6 s window differenced to 0 and was killed as stalled. That is why
gate-selftest case 5 (a host spinning, meant to survive) went red on a
loaded box — the spin was starved below a second per window. Now
`cpu_of` answers centiseconds, the thresholds are compared in the same
unit, the messages still read seconds (`0.30s`). New case 5b drives a
host that works lightly ON PURPOSE (`fake-sbt-light-host.sh`: short
bursts and sleeps) so the reproduction does not depend on load —
watched STALLED on the old code ("sbt 0s of CPU"), green now; case 4
(a real hang under idle children) still dies. Whole selftest green
under bash and /bin/sh.
