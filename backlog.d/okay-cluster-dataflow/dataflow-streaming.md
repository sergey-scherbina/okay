- [x] dataflow-streaming — LANDED as stage 6, except one half that
      was REFUSED rather than forgotten, and the difference matters to
      whoever reads this next. Landed: the epoch loop, the watermark
      as the minimum over the partitions minus the declared lateness
      (6a), a dying worker's partition replayed on a survivor (6b),
      and exactly-once OUTCOME at a keyed sink with the offers counted
      (6c). NOT landed, and argued against in 6b: keyed state in an
      okay-persist backend. A replacement worker REPLAYS rather than
      restores, because a partition is a recipe and snapshotting an
      operator's insides would make every one of them a wire format
      that has to survive a version change. The COORDINATOR's state
      does go to okay-persist — stage 8 — because that one cannot be
      replayed from anywhere.
