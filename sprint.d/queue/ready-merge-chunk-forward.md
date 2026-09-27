- [ ] ready-merge-chunk-forward — the chunked merge roads
      (`Source.merge(chunked = true)`, `flushAfter`, `mergeFlushing`,
      `either`) onto the ring join, so there is ONE merge mechanism —
      a retry of merge-chunked-via-ready (reverted 2026-09-27) AFTER
      `ring-standing-receiver` (sprint.d/doing) lands: ring-chunk-
      bimodal-forks ANSWERED the split (be8035d23/b90b6aa78) — not JIT,
      not placement: the sides' one-shot parked receives (84-217 wakes
      per op vs 11-20), which that lane fixes. BLOCKED on it. WHY: the elementwise ready-merge pays ~25 ns per
      element for the ring, the tell and the walk (specs/ready-merge.md
      Results: pure 68-70 us vs single-drain 44 us per 1000), and a
      per-side `Chunk[A]` of 64 amortises that to under 1 ns;
      `ReadyMerge` is polymorphic in A already, so the road is
      `ReadyMerge[Chunk[A]]` over a chunk channel per side plus
      `Writer.expand` outside — which the reverted cut was, and its
      GOOD forks read at the old road's level (195-210 vs 200-215).
      What the reverted cut kept: `flusherFor`, the failAfterTail fix.
      HOW: rebuild the road from the reverted commit on the branch's
      history (specs/source-merge-via-ready.md names it) on top of the
      standing receiver, read the per-fork wake counters that lane
      added as the regime check, then measure `ChunkFlushBenchmark`
      (`okayChunked`, `okayChunkedFlush`, `okayChunkedFlushShort`, k =
      16/256/1024), 5 forks per arm, arms alternating, through
      `scripts/jmh-lane.sh`. Two matched-pair traps to respect: the
      shared chunk channel's arm goes through `relaxed.parts(2)` and
      pays `AdaptiveFifo.popScanning` (O(parts) + a claim CAS per pop,
      AdaptiveFifo.scala:442) — the ring road does not, so state that
      in the header rather than letting it read as the ring's win; and
      `merge-chunked-flag-fixed-chunk-size` (backlog) means the fused
      flag chunks at 16 only — measure the COMPOSED road at 256/1024 as
      well. `SentinelChannel.receiveManyAsync`'s `wakeSender()` per
      element taken (:425, :434) — leave to ring-standing-receiver if
      it touches those lines; otherwise stop at the first poll that
      wakes nobody. BAR: no arm slower than the shared channel at any k, no
      bimodality by the criterion; then `Source.scala:465+`'s
      `chunkedMerge` and `Channel.mergeChunked`'s shared-channel road
      go, as the elementwise old road went. Spec:
      specs/source-merge-via-ready.md (its "chunked roads wait"
      decision is what this closes). (2026-09-27, perf-plan)
      NOTE 2026-09-27 (ring-standing-receiver, operator decision): the
      chunk-on-ring work is PAUSED ("потом вернемся"), and its blocker is
      back in the backlog. One candidate fix was already REFUTED —
      zero-allocation side wakes did not move the slow regime (6/10
      forks ~250 us) — so "that lane fixes it" above is a hypothesis, not
      a result; the remaining candidate is a standing receiver inside
      `SentinelChannel`, for a reward of parity (~200 us), not a win.

