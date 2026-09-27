- [ ] ring-standing-receiver — the ring merge's sides read their channels
      with ONE-SHOT receives: an empty side parks, and the channel hands
      the next send over as a single element, the wake running on the
      producer's thread (ring-chunk-bimodal-forks, 2026-09-27:
      specs/source-merge-via-ready.md). On the chunked road that made a
      second, self-sustaining regime (~1.25x, 84-217 side wakes per op
      against 11-20). The design to try: a STANDING receiver per side —
      registered once, accumulating sends into the side's own buffer,
      re-armed only when the merge has taken what it holds — so a caught-up
      consumer still receives batches and a producer's send never runs the
      merge's wake work more than once per batch. Then re-measure the
      chunked road (`ChunkFlushBenchmark`, -f 10, the fork counters) and
      the elementwise one (`MergeCapBenchmark`) — the elementwise ring
      did not show the second regime, but nothing guarantees it never
      will at another speed ratio. TRIGGER: the operator's one-mechanism
      ask for the chunked road, or the regime seen on the elementwise
      merge. (2026-09-27, ring-chunk-bimodal-forks)
      PAUSED 2026-09-27 by the operator ("делай пока так, потом еще
      вернемся"), after ONE design tried and refuted: zero-allocation
      side wakes (a `Pending` per registration, an int ring of wake-ups,
      the source's continuation applied by the drive) left the chunk
      road's slow regime exactly as it was (okayChunked 6/10 forks
      ~250 us) and gave the elementwise merge parity, so it was dropped.
      The wake WORK was therefore not the cost: what remains is the
      channel handing a parked receiver ONE element per wake — the
      standing receiver proper, inside `SentinelChannel`. Weigh it
      against its reward first: the chunked road on the ring can reach
      the shared channel's ~200 us (its fast forks do), not beat it.
      Rows: `src/jmh/history.d/…-ring-standing-receiver.tsv`.

