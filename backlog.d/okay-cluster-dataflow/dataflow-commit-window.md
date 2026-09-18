- [x] dataflow-commit-window — LANDED as stage 9, and the entry above
      was wrong about the roads: both the ones it named are worse than
      the window, and the one it did not name is what every engine
      does. The engine cannot close it (the write left the engine), so
      it hands the writer `committed(epoch)` and `recovered(epoch)` —
      told BEFORE the journal, so the worst case is a repeated epoch
      rather than a lost one, and a writer that records the epoch
      beside its rows is exactly-once.
