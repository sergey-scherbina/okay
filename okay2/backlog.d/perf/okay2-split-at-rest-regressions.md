- [ ] okay2-split-at-rest-regressions — PRIORITY: LOW. Found by
      okay2-split-at-rest-measure (2026-10-04, history.d okay2-delim-perf):
      stage 45 (`f1520259d`) against its parent read `produceFold` 1.09x
      and `writerMap` 1.05x (+8 KB), `delimShift` 0.98x, `msplitObserve`
      0.99x. ANSWERED FOR produceFold the same day (history.d
      okay2-split-at-rest-producefold): on master, `Producer.fold` with
      the pre-stage-45 split restored is 1.27-1.33x SLOWER (+96 B an
      operation) than its `Split.at` loop, so the split is not that cost —
      the parent comparison carries the whole commit, and master runs the
      lane at ~22.6-23.7 us, the parent's 21.9 within a few percent. LEFT:
      `writerMap`'s +8 KB a run (8 B a tell) — isolate `Writer.mapAt` the
      same way (its old `split` shape restored on master, alternated)
      before anything is changed.
