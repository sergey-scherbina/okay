- [ ] okay2-split-at-rest-measure — price stage 45 (okay2-split-at-rest)
      against its parent commit. `HandlerBenchmark` has four lanes for
      it: `delimShift`, `produceFold`, `writerMap`, `msplitObserve`.
      Protocol as in okay2-handler-allocs: one lane per `Jmh/run`,
      alternating arms, each lane started on a quiet box and discarded
      if the box was busy at its end, per-lane min of three, and the
      rows written with `scripts/history.sh new`. Not measured on
      2026-09-25: a macOS VM held ~1300% CPU for the hour. (2026-09-25)
