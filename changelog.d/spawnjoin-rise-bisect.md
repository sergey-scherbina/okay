## spawnjoin-rise-bisect — own spawn/join back from 110 to 64 us

`OwnMonitorBenchmark.spawnJoinSeq` had risen from ~64 to ~110 us on master.
`git bisect run` (one `jmh-lane.sh` run a step) named cda0a94b5: the
cancel-interrupts-the-drive fix paid a ThreadLocal get/set, a monitor on
the way out and a `Bind` per late resumption on EVERY slice of every
fiber. Same behaviour, cheaper protocol: the running drive is a field of
our own worker thread, a volatile handshake with `cancel` replaces the
monitor (a cancelled slice still waits out cancel's critical section
before taking its interrupt back), and a late answer resumes inside the
drive's loop. 64.4 us against 64.2 before the regression and 110.6 on
master, same session; TestAsync/SchedulerLaws/OwnMonitor/ManagedBlocking
81/81. specs/spawnjoin-rise-bisect.md.
