- own-monitor-burst-load-flake — FIXED 2026-09-27: the burst law's fibers
  are 5 ms, not 0.5. Eight 0.5 ms fibers were 4 ms of work, inside the
  monitor's own 5 ms tick. Under 20 CPU burners that failed 7 of 60 law
  runs, and at 5 ms it failed 0 of 60. The law itself is unchanged.
