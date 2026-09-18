- [x] produce-at-a-wider-row — DONE 2026-09-16 (blob-source-road): the
      answer was already in RowLift — `produce(a).plus[Async]` is a
      zero-cost coerce, not the walk `!.widen` makes — so `produce`'s
      doc says so and names the trap beside it. Was: `produce(a)` is the named injector and
      is typed `A ! Produce` precisely, so a program in
      `Produce + Async` cannot call it. `!.widen` does lift it (that
      is what `Source.of` uses), at the price of a tree-rewriting
      pass a multi-chunk stream should not pay per element, so the
      idiom in practice is the wide `effect[Produce + Async, A](a)` —
      okay's own benchmark spells it that way too
      (`effect[Ask + Produce, Int]`). The point is that at the wide
      row the named safe call is unavailable and `pure` is not:
      reaching for `pure` is exactly the mistake above. A
      `produce[F[+_], A](a): A ! (Produce + F)` would close it; one
      sentence at `produce`'s doc pointing at `!.widen` and at this
      trap would close most of it for nothing.
