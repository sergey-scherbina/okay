- [ ] own-monitor-burst-load-flake — `TestOwnMonitor` "own: a burst of
      long fibers forked inside a fiber runs on more than one thread"
      failed twice on 2026-09-27 inside a whole `affected master staged`
      run on a busy box (relay-forward-same-inject's gates; once while
      the gate ran demoted on the E-cores, once not) — "a run used 1
      thread(s) for eight 0.5 ms fibers on four workers" — and passed
      twice alone on that branch and twice alone on master. The law takes
      the MINIMUM over 10 runs, so ONE run in which the 5 ms monitor did
      not spread the burst before the eight 0.5 ms fibers finished on the
      forking worker fails it: 4 ms of work per run is inside the
      monitor's own reaction time, and a loaded box stretches that. Fix
      candidates: fibers long enough that the monitor's tick is well
      inside them (e.g. 5 ms each), or a law over the MEDIAN run; prove it
      under 20 CPU burners (the channel-known-producers recipe) before
      and after. TRIGGER: its next red, or anyone touching the own
      monitor. (2026-09-27, relay-forward-same-inject)
      AGAIN 2026-09-27 in op-map-constructors' whole-build gate
      (okay-gate.3fcpoaWYNc, State-only change, not demoted): "a run used
      1 thread(s) for eight 0.5 ms fibers on four workers".
