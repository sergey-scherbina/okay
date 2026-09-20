## stream-fold-via-iterator - the linear consumers walk the linear view

`Stream.fold` stepped by `uncons(_).runWith` per element — an
`Option`, a tuple and a program built and run each time — and the
postfix `foldLeft`, `foreach`, `find`, `exists`, `forall`, `toList`
went through `toLazyList`, a memoised cell and a synchronised lazy
state per element for a value read once, while every `Stream`
instance in the tree already overrode `iterator` with a direct walk
of its carrier. They walk that now; `Foldable[Producer]` follows, and
the `LazyList`-answering combinators (`filter`, `zip`, `take(n)`, …)
are untouched — a memoised stream is their contract.

And `Stream.fold` dispatches on the accumulator's shape as
`Chunks.fold` does, which its own doc had declined: on the old walk
the dispatch bought nothing under the per-element program, on the
iterator it was 40% of what was left — a boxed `Long` per element
through the generic `add`, seen as 24 B/elem and 40 us against a
hand-written `while` over the same iterator.

Measured (spec Results, history.tsv `sfi-*`, three rounds, each pair
beside its old shape written out verbatim as the control row):
`Stream.fold` with `Fold.sumLong` over 10k 133.7 -> 58.3 us and
1 516 561 -> 876 992 B/op (the hand loop to within 88 bytes); the
generic `Fold` 137.3 -> 95.6; `foldLeft` through the iterator against
the `toLazyList` road 209–224 -> 88–92 us, −46% B/op; `Stream.fold`
over the Async-widened producer 114.5 (recorded) -> 64.0.

Also retired: `direct-compileall-split` (backlog.d/okay-core), a
duplicate of the `direct-compiler-phases` lane a sibling had already
claimed.

Files: src/main/scala/Stream.scala, src/test/scala/TestStream.scala
(one agreement test against the bridge as oracle),
compare/src/jmh/scala/okay/FoldConsumersBenchmark.scala (four control
rows), specs/stream-fold-via-iterator.md, src/jmh/history.tsv.
