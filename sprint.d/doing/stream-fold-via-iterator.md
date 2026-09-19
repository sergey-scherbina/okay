- [ ] stream-fold-via-iterator — `Stream.fold` (Stream.scala ~120)
      walks by `St.uncons(x).runWith` per element: an `Option`, a tuple
      and a program built and run per element, while EVERY `Stream`
      instance in the tree now overrides `iterator` with a direct walk
      (LazyList, List, Producer, `Stream[[A] =>> A ! Produce + G, G]`,
      `feedStream`, `writerStreamIn`). Through this path go
      `Foldable[Producer]` (Generate.scala ~260), the postfix consumers
      `foldLeft/foreach/find/exists/forall/toList` (Stream.scala ~158,
      via `toLazyList`: a memoised cell plus a synchronised lazy state
      per element) and `take(n)` (Generate.scala ~47).
      MEASURED ALREADY, on the G-effectful producer, 10k elements
      (producer-effectful-stream-iterator, 2026-09-19): the default
      `uncons` walk 103.6-114.5 us against the specialised iterator's
      57.3. The pure-producer twin is `FoldConsumersBenchmark.
      streamSpecialized`, last read 120.9 us (history.tsv,
      pwc-elementwise-pure10k) — the lane the number is owed on.
      THE DOC IS STALE WITH IT: `Stream.fold`'s comment declines the
      accumulator dispatch `Chunks.fold` makes because "the step is
      ~150us per 10k and the fold's share is a fraction of a percent"
      (history.tsv:140, 139.9 vs 157.2, bars overlapping) — that step
      is exactly what the iterator removes, so the dispatch is worth
      re-measuring once the walk is the iterator's.
      HOW: `Stream.fold` = `St.iterator(s)` + a while loop; the six
      postfix consumers likewise (no API change — they return values,
      not streams). `filter/drop/zip/++/map/flatMap` return LazyList
      and stay: changing their carrier is an API decision, not this.
      Found in the 2026-09-20 review.
