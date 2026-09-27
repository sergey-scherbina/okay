## jmh-lane-fifo — the benchmark lock serves the oldest queued lane first

The lane lock was a race: every queued lane polled `mkdir` every 10 s, so
when the holder released it, whichever lane polled first won. On
2026-09-27 a lane queued since 19:42 lost the freed lock to one queued at
20:16 — its wait had no bound but luck. Now a lane's bench-window request
(`want/<pid>`, filed before the wait) carries its filing time, and a
queued lane takes the lock only when its request is the oldest live one
(ties by pid). A request from a script that predates this is empty and
reads as the oldest, so new lanes wait for old ones rather than
overtaking them. `jmh-lane-selftest.sh` case 14 (two lanes behind a
holder, the newer one polling when the lock frees) was red 3/3 on master
and passes now; both selftests PASS under sh and bash.
