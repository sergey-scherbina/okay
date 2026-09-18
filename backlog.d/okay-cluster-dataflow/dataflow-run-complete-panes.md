- [x] dataflow-run-complete-panes — LANDED. The windowed `Wide` node
      has the rule: a partition that can finish a pane alone PRESENTS
      it into a per-bucket buffer, and only the boundary panes go into
      the maps a reducer merges. The buffer road, not the threaded
      terminal — the node never learns what it is folded into.
      Measured back to back on one box: the three-plan road goes from
      647 ms (8.19x the hand-written lane) to 182 (2.25x), 3.6x, with
      every other lane unmoved. And the check is a COUNT, not a clock:
      `Run.merged` is reported by the single-stage road now and
      `TestFlow` asserts it equals the fan's exactly, at 2, 4 and 8
      partitions. Two things fell out — `prepass` answers an `Extent`
      rather than one Long (the road had been computing half of what
      the rule needs), and the LAST partition has no upper bound at
      all, since the two bounds guard two different neighbours and it
      has no later one. That last is why a run at ONE partition now
      merges nothing, asserted.
