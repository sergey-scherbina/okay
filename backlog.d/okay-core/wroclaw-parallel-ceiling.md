- [ ] wroclaw-parallel-ceiling — where okay's merge-parallel lane's
      6.5% serial part actually is, now that the obvious answer is
      REFUTED. The lane scales 1.88x / 3.30x / 5.55x at 2 / 4 / 8
      fibres; Karp-Flatt reads 0.062 / 0.071 / 0.063 — flat, so a
      genuine serial share of ~6.5%, and Amdahl puts the ceiling at
      7.6x on a 14-core box.
      NOT THE PREP PASS (wroclaw-parallel-prep-pass, 2026-09-11).
      `OkayLane.parallel` opens with a serial walk of every event
      (slice maxima + the greatest backwardness), which looked like
      exactly that serial share. Parallelising it as a reduction —
      each slice reduces its own range, the coordinator combines in
      O(lanes) — was measured A/B, both roads alternating INSIDE one
      JVM, five rounds, minimum kept:
        width 2   448 -> 454 ms   (-1.3%)
        width 4   258 -> 257 ms   (+0.4%)
        width 8   154 -> 158 ms   (-2.6%)
      Nothing, and it bounds the scan from above: if 8 threads save
      at most ~4 ms, the whole scan is under 1% of the 855 ms
      single-fibre run. Reverted.
      REMAINING SUSPECTS, unmeasured: the coordinator — `Sink.absorb`
      per slice, `merged` over the partial pane maps, and the
      bunching stitch, all serial and all proportional to the number
      of BOUNDARY panes rather than to the events; and fibre
      spawn/join at width 8. Measure the coordinator's share first by
      timing it separately inside the lane.
