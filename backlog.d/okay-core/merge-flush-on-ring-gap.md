- [ ] merge-flush-on-ring-gap — `Source.merge(chunked = true, flushAfter
      = Some(ms))` on the ring (`Merge.Ready`, the default since
      ready-merge-chunk-forward's second landing) read 238.1 ± 13.8 us
      against the shared road's 212.0 ± 7.0 on `okayChunkedFlush` (5 forks
      per arm, one round): 1.12x, over the 1.06x the operator accepted,
      where the unflushed lane read 1.03x. WHERE THE COST IS (corrected
      2026-09-28 — the first text of this item blamed "a flusher per
      side against one", which is wrong: `chunkedMerge` runs a flusher
      per source too): with a window a side has TWO senders, its feed and
      its flusher, so its channel is `forProducers(2)` — two parts, and
      the consumer pays `AdaptiveFifo.popScanning` (O(parts) + a claim
      CAS per pop) on EVERY look at it — where a side without a window
      is the single-producer ring. The shared road has one such 2-part
      channel; the ring road has one PER SIDE, so it pays that scan
      twice. THE LANE: (1) re-measure with 10 forks per arm, arms
      alternating, through `scripts/jmh-lane.sh`, plus `okayChunkedFlushShort`
      on the ring (its shared arm read ±91 us — the 1 ms flusher makes it
      timing-bound; say so or fix the lane); (2) if the gap holds, the
      candidates in order: (a) `Merge.Shared` by default for the flushing
      shape — one line now that `Merge` is a value, two mechanisms by
      the back door; (b) the flusher SIGNALS the feed, the one producer,
      to send its partial chunk, keeping the side SPSC — but a feed
      parked in its source's pull answers late, so the window is not
      honoured; (c) the CONSUMER flushes: the partial chunk sits in the
      side's `TRef[ChunkBuffer]`, and a merge whose ring ran dry takes it
      itself (`takeChunk(full = true)`, under the TRef's CAS, as the
      flusher does) before it waits — the flusher stays only as the
      latency bound while the consumer is busy; SPSC kept, order within
      the side kept (the buffer is the one place unfinished elements
      live). NOT a candidate: the flusher as a second SOURCE on the ring
      — two rings of one side can tell a partial chunk after the next
      full one. Also unmeasured before that landing: the elementwise
      lanes on `Wait.Ladder` (last reading, spin-100: 84.8 / 68.3 / 63.0
      at cap 64 / 256 / 1024) — re-read them here.
      (2026-09-28, ready-merge-chunk-forward)
