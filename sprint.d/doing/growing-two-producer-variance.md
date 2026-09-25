- [ ] growing-two-producer-variance — `ManyProducersBenchmark.default_chunk`
      at producers=2, consumers=1 on the DEFAULT channel (`growing`)
      never measured under 10% error in ten attempts on 2026-09-25
      (channel-default-adaptive), and read 795 +- 346 us/op in the one
      early run, while every other row of the same A/B, and the same
      lane under `adaptive` (141.6 +- 5.5), was tight. A bimodal cost is
      the likely shape: the run may or may not cross the ring-to-parts
      adoption before its elements are through. Verify before believing
      it: per-iteration scores (`-rf json`) show two clusters or they
      do not; then whether the swap lands at a random point of the run.
      If real, it is a latency cliff in the default channel at exactly
      the producer count `merge` does not cover (a user's two senders).
