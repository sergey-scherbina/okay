- [ ] bench-window-demote-measure — stage 2 of specs/bench-window.md:
      does demoting RUNNING gates to the efficiency cores
      (`taskpolicy -b -p <pid>`, background QoS) instead of waiting for
      them leave the performance cores quiet enough to measure? Control
      lane `MergeBenchmark.okaySourceSingleDrain`: (a) alone, (b) beside
      a gate demoted that way, (c) beside the same gate undemoted;
      alternating, two rounds. (b) within (a)'s own error → the window
      demotes instead of draining, and gates never wait; otherwise
      draining stays and the result is recorded as the refutation.
      Memory bandwidth, the shared cache and heat stay shared, which is
      why this is a measurement and not a design. TRIGGER: the first
      window stage 1 opens (right after ready-merge-numbers).
      (2026-09-26, bench-window)
