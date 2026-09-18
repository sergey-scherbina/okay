- [ ] optics-outside-topology — a dataflow is an arrow. The ONLY one
      of the six that meets the staticness condition the spec sets for
      reaching for `Arrow` at all: the graph must exist as a value
      before it runs, to be drawn, fused, or shipped to a cluster.
      Wants `ArrowChoice` or branches fall out of the static picture.
      okay-flink / okay-kafka / okay-reactive, and dataflow's own plan
      value is the incumbent to compare against.
      MEASURED 2026-09-11, then CORRECTED the same day. The first
      measurement said "nothing here meets the condition" and was
      about `Stage`, not about the tree: `Stage[I, O, A] = A ! (Take %
      I + Writer % O)` is a PROGRAM, everything past the first effect
      lives in a continuation, and a monadic pipeline's shape
      legitimately depends on its values. All true, and not an answer
      to the question asked.
      THE CONDITION IS MET, by `okay.Tables.Plan[A]` — a GADT of the
      plan, whose own comment says why it is not a `Free`: "its
      continuations are functions; the tree can". It has the three
      interpreters an `Arrow` would have been reached for: `show`
      draws it (okay-spark's TestWroclawAlgebra collects rendered
      plans), `optimize` rewrites it — a projection into its `Read`,
      the small join side to the right — under the law "the turned
      join answers exactly what the written one does", and
      `compile(B)` runs it over any `Bulk[D]`, local `Chunks` and
      Spark alike. Drawn, fused, shipped to a cluster: all three.
      SO THE CANDIDATE IS ANSWERED RATHER THAN WAITING. What it asked
      for exists, and it got there with an ordinary GADT and no
      profunctor — which is the same verdict `optics-outside-conf`
      reached by a different road, and the reason this arc keeps
      asking for a consumer before an abstraction. What an `Arrow`
      would add over `Plan` is `ArrowChoice`-shaped branching and
      composition laws nothing has asked for; reopen it if a plan
      needs to branch on a value and still be drawn.
