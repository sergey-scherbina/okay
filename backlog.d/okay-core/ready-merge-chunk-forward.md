- [ ] ready-merge-chunk-forward — the chunked merge roads
      (`Source.merge(chunked = true)`, `flushAfter`, `mergeFlushing`,
      `either`) onto the ring join, so there is ONE merge mechanism —
      a retry of merge-chunked-via-ready (reverted 2026-09-27) ON TOP
      OF `ring-standing-receiver`, which is claimed together with this
      item as its first stage: ring-chunk-bimodal-forks ANSWERED the
      split (be8035d23/b90b6aa78) — not JIT, not placement: the sides'
      one-shot parked receives (84-217 wakes per op vs 11-20). WHY: the elementwise ready-merge pays ~25 ns per
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
      UNPAUSED 2026-09-27 by the operator ("Разблокируй"): the pause
      above is lifted; this item and ring-standing-receiver run as one
      lane, standing receiver first.
      BACK TO THE BACKLOG 2026-09-27 (this lane, stage 1 failed): its
      first stage, `ring-standing-receiver`, was run as a notifying
      receive in `SentinelChannel` and REFUTED — the chunked ring road
      stayed bimodal (6/20 slow forks against the one-shot's 7/20; the
      shared channel 0/10), so stage 2 was not run and nothing moved.
      There is no candidate fix left on the receive side
      (backlog.d/refuted-declined-or-answered/ring-standing-receiver.md).
      The chunked roads stay on the shared channel. TRIGGER: a design
      that changes the relative speed of a chunk side and the merge, or
      the operator deciding one mechanism is worth a bimodal ~1.25x in
      some forks.
      CANDIDATE 2026-09-27 (operator, "Да"): LAZY REGISTRATION —
      poll, then park. Both refuted fixes changed HOW a side wakes
      (cost, thread); neither changed WHEN a side registers. In
      `ReadyMerge.operate`'s `Async.Await` arm (ReadyMerge.scala:186)
      a side whose channel is empty registers its callback AT ONCE,
      whatever the other side holds, and `receiveManyAsync` on an empty
      ring falls to `receiveAsync` (SentinelChannel.scala:434), so the
      producer's next send must hand ONE element over on its own thread
      — the self-sustaining cycle. Already measured and consistent with
      it: in a slow fork the merge's OWN parks are ~0 per op (the spin
      experiment) while side wakes are 84-217 per op, so ~100
      registrations per op are made by a side the merge never parked on
      and would have come back to by itself. The shared channel is in
      one mode because it registers only at a REAL park, when both
      producers' data is gone — that semantics, on per-side channels, is
      the candidate: (1) a non-registering `receiveManyNow` in
      `SentinelChannel` over the existing `popMany` (no channel has a
      non-registering read today); (2) a side's step is `Run(takeNow)`
      answering a chunk or "empty"; "empty" puts the side into an `idle`
      set WITHOUT `reg`; (3) ring and `woken` empty → re-poll the idle
      sides (a volatile read each); one gave a chunk → continue; all
      empty a second consecutive time → only now `reg` on every idle
      side and park. Meets this item's own reopen criterion: it changes
      the RELATIVE speed (the producer stops paying the hand-over), not
      the receive; and it is not the spin experiment, which spun at the
      merge's park after the sides had already registered. FIRST STEP,
      cheap, decides it: one more per-fork counter beside the existing
      ones — "registered while the other side was non-empty". ≈ side
      wakes → the lane is worth running; ≈ 0 → the regime is the
      producer's own speed and this candidate is wrong too. Expected
      reward is parity (~200 us) plus one mechanism, not a win; the
      known cost is one extra poll round before a real park, microseconds
      on a park that happens a few times per op. Then stage 2 as above.
