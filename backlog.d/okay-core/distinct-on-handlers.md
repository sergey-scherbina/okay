- [ ] distinct-on-handlers — operator 2026-09-24: "в скала три тоже нужно
      это исправить". `Distinct[R]` (src/main/scala/Distinct.scala) is
      required only by `Handler.union` and `Handler.flat`, so a
      per-signature handler over a row holding two signatures of one
      class — `State.handle(s)(p)` on `State % Int + State % String`,
      `Reader.run` on `Reader % Int + Reader % String` (the case
      TestRowIdentity demonstrates) — still splits by class and sends
      the second's operations to the first: a ClassCastException at the
      first wrong answer. The lane: require `Distinct[F + G]` on every
      public eliminator that splits one signature out of an open row
      (State, Reader, Writer, Throws, Once, Resource, Choose/Logic,
      Async, Delim, and the kernels `relay`/`translate`/`handle`),
      measure the misroute first, keep the per-handler `Distinct`
      transparent to row-generic code (an abstract part passes, as it
      does now). okay2 does the same in `okay2-distinct-handlers`.
      (2026-09-24)
