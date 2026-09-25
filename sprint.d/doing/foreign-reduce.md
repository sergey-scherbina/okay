- [ ] foreign-reduce — the REDUCE of a cluster job in Python or R
      (foreign-map-reduce, 2026-09-25, left out of v1 on purpose). A
      `Wire` is a Scala `Aggregator` whose merge is part of its type —
      that is what makes map-side combine free and lets the coordinator
      merge partials without a user function. A foreign reduce needs the
      far side to declare a MONOID the coordinator can call: `zero`, a
      `step(acc, frame)` over a chunk and a `merge(acc, acc)`, with the
      accumulator crossing the wire by a `Schema` the job names (the
      partial's Schema is already the door check). Design question to
      settle first: whether `merge` runs in the interpreter (a round trip
      per merge on the coordinator) or the foreign side answers a Scala
      `Aggregator` by NAME from a fixed set (sum, count, min/max, top-k,
      histogram — the reductions an analyst actually asks for), which
      needs no protocol at all. Gate: `TestPyMapReduce`'s job with the
      reduce moved to Python, the same answer.
