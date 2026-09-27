## own-monitor-burst-load-flake - TestOwnMonitor's burst law stops failing under load

- The law forks eight fibers inside a fiber, takes the fewest threads over
  10 runs, and asserts that the monitor spread them (≥ 2). Its fibers
  were 0.5 ms each, 4 ms of work in all, which is inside the monitor's
  5 ms tick. One late tick in ten runs was a red gate: three sightings
  in whole-build gates on 2026-09-27.
- Measured under 20 CPU burners: 7 of 60 law runs failed at 0.5 ms and
  0 of 60 at 5 ms. The fibers are 5 ms now, with the same law.
