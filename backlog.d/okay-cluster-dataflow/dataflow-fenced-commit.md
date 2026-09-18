- [x] dataflow-fenced-commit — CLOSED, on the READ side rather than
      with a compare-and-set no store here offers. `Folded` carries
      the term, `Checkpoint.newest` takes the highest (term, epoch)
      out of a journal's history, and a stale commit is shadowed for
      ever instead of being read back — which a log can do and a cell
      cannot. The honest half: the rows were right either way, because
      a stale resume costs WORK and not correctness as long as the
      source replays and the writer is keyed. For a source that does
      not replay, the fence is still a check and that window stays
      named.
