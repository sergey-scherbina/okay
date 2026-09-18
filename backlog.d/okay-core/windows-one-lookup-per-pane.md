- [x] windows-one-lookup-per-pane — REFUTED BY MEASUREMENT
      (2026-09-10, windows-int-key-panes). The idea: `Windows` holds a
      one-field `Cell` per pane, so folding an element is ONE hash walk
      plus a field store where it was `getOrElse` then `update`, twice
      per pane per element. Written, gated, and then priced on a quiet
      box by `WindowsBenchmark` (which this lane also lands), 16
      iterations per side, minutes apart:
        tumbling 426.8 +-23.0 us with it, 431.4 +-18.6 without
        sliding  911.5 +-124.2 us with it, 874.5 +-69.8 without
      1% better on one lane, 4% WORSE on the other, both inside the
      bars: the change buys nothing. `LongMap`'s get and update are
      cheap next to the rest of `add`, and the Cell's indirection and
      its allocation per pane pay back whatever the second walk cost.
      The code is reverted; the instrument stays.
