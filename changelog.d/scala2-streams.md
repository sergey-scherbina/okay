## scala2-streams - okay.scala2.Source: streams for Scala 2.13

The operator asked for streams, fibers and channels in Scala 2.13.
This lane does streams. Fibers and channels as their own types are
still queued, and `merge` already uses both internally.

- `okay.scala2.Source[A]` wraps the core's `okay.Source[A]`
  (`Unit ! (Writer % A + Async)`). Constructors: `Source(...)`,
  `fromIterable`, `range`, `unfold`, `empty`, and `fromEff`, which
  takes a source written as an `Eff[Writer[A] with Async, Unit]`
  for-comprehension. Transformations: `map`, `filter`, `mapConcat`,
  `take`, `takeWhile`, `drop`, `zipWithIndex`, `++`, and `merge`,
  which runs two sources concurrently over okay's `Channel.merge`.
  Running: `runCollect`, `runForeach`, `runFold`, each an
  `Eff[Async, _]`, plus `toEff` to get the program back.
- `take`, `takeWhile`, `drop` and `zipWithIndex` are stages driven by
  `through`. The stages carry `Async` in their own row, so the
  source's Async operations pass through, and a finished stage stops
  pulling, so `take` works on an infinite source.
- okay-scala2 now depends on okay-stream. The 2.13 probe has 28 tests,
  all green on their first run under `-Xlint -Werror`.
- Docs: a Streams section in docs/modules/okay-scala2.md, with
  examples copied from the probe, and a guide sentence. Spec stage 4
  has its boxes checked and its results recorded.
