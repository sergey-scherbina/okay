- [ ] okay2-split-at-rest-regressions — found by okay2-split-at-rest-measure
      (2026-10-04, history.d okay2-delim-perf, min of 3 alternating rounds
      each): stage 45 (`f1520259d`, every handler loop split by `Split.at`)
      against its parent made `produceFold` 1.09x slower (23.8 vs 21.9 us,
      +16 B) and `writerMap` 1.05x (21.9 vs 20.8 us, +8 KB a run), while
      `delimShift` (0.98x, -16 KB) and `msplitObserve` (0.99x) gained. The
      Delim machine's own arm is the win; the Producer.fold and Writer.map
      loops' moves onto `Split.at` are the loss. To find: which of their
      arms grew (allocation profile of the two lanes on master), and
      whether those loops go back to their pre-stage-45 split or keep
      `Split.at` with the cost removed — the stack safety stage 45 gave
      `Producer.each` (StackOverflowError at 200k before) must stay.
      (2026-10-04)
