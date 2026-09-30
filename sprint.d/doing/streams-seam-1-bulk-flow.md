- [ ] streams-seam-1-bulk-flow — lane 1 of backlog `streams-seam-arc`
      (operator: "Пиши спеку и делай", 2026-09-30): specs/streams-seam.md
      for the whole arc FIRST, then `Bulk[Flow]` — our engine as a Bulk
      instance in okay-cluster (join = Exchange by key on both sides then
      the local hash join per partition, aggregate = Keyed, cache = the
      materialised partitions, read = one partition per split), so every
      `Tables` program runs on the cluster engine unchanged. Tests: the
      agreement law — a Tables program answers the same on Chunks and on
      Flow at parallelism 4; TestPlan's job; docs.
