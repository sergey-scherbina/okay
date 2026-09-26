## own-scheduler-monitor - a monitor spreads work that waited behind a busy worker

- `Schedulers.own` had one blind spot with two symptoms: a fiber forked
  inside a worker goes on that worker's queue silently, and nothing
  looked at it while the worker was busy. Eight long fibers ran on one
  thread, and fibers that block on `adaptive` reached 2 of 8 threads.
  `TestOwnMonitor` was red on both before the fix.
- The fix is a monitor thread per scheduler (`monitorEvery`, 100 µs;
  `unmonitored` turns it off). A worker whose queue has waited a whole
  look without being touched gets sleeping workers woken to take the
  work; on `adaptive` the monitor also starts overflow workers. It
  sleeps after ~10 ms idle and adds nothing to the per-task path.
- Measured in the five-way harness against master: 8 workers at
  work 64 went from 1 841 to 3 339 ops/s on `own`, and blocking TCP on
  `adaptive` from 5.2 to 51.2. The rest of the TCP gap is `adaptive`'s
  overflow bound, and `watched(overflow = 64)` removes it. Sequential
  spawn/join and the ~30 ns fork/join lanes are unchanged.
- The price: tiny steps over shared state are faster kept on one core
  (workers at work 0: 5 343 to 4 097). `forShortTasks` now turns the
  monitor off for that shape.
- specs/schedulers.md "Two defects" has the design and the rejected
  alternatives; docs/schedulers.md and docs/benchmarks.md §4a have the
  numbers.
