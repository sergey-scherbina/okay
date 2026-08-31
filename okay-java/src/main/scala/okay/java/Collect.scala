package okay.java

import okay.Aggregator
import java.util.stream.Collector

/**
 * `Aggregator` IS a `Collector`.
 *
 * The JDK's `Collector[T, A, R]` is four things — a supplier, an
 * accumulator, a combiner, a finisher — and okay's
 * `Aggregator[In, Acc, Out]` is the same four: `init`, `add`, `merge`,
 * `present`. Not analogous, the same, which is why this file is a
 * translation rather than a design.
 *
 * That correspondence is the same one okay-spark rests on, and it
 * buys the same thing here: an aggregator written once runs over
 * `Chunks` locally, distributed on Spark, and now on a PARALLEL
 * `java.util.stream` — because `merge` is what a parallel stream
 * needs to combine the halves it split, and it was always there.
 *
 * The one seam is mutability. A `Collector`'s accumulator is a
 * `BiConsumer` that mutates its accumulator in place, while `add`
 * answers a new one, so the value travels in a one-cell box. That box
 * is per SPLIT, not per element — a parallel stream makes one
 * container per thread, exactly as `merge` expects.
 */
object Collect {

  /** the box the JDK's mutating protocol needs around a value */
  final class Cell[A](var value: A)

  /**
   * An aggregator as a Collector. `CONCURRENT` is deliberately NOT
   * claimed: that would tell the JDK one container may be shared
   * across threads without merging, and an `Aggregator` says nothing
   * about its accumulator being thread-safe. `UNORDERED` is not
   * claimed either — plenty of aggregators (first, last, a sliding
   * window) depend on order, and the ones that do not lose nothing by
   * the JDK not knowing.
   */
  def collector[In, Acc, Out](agg: Aggregator[In, Acc, Out])
  : Collector[In, Cell[Acc], Out] =
    Collector.of[In, Cell[Acc], Out](
      () => Cell(agg.init),
      (cell, in) => cell.value = agg.add(cell.value, in),
      (a, b) => { a.value = agg.merge(a.value, b.value); a },
      (cell: Cell[Acc]) => agg.present(cell.value))

  /**
   * The other direction: a Collector as an aggregator.
   *
   * `merge` is where this can be dishonest, so it is not: a Collector
   * whose combiner is the one `Collectors.toList` and friends supply
   * is fine, but a CONCURRENT collector may hand back a combiner that
   * throws, because it was never meant to be called. That is checked
   * and reported rather than discovered at scale.
   */
  def aggregator[In, Acc, Out](c: Collector[In, Acc, Out])
  : Either[String, Aggregator[In, Acc, Out]] =
    if c.characteristics.contains(Collector.Characteristics.CONCURRENT) then
      Left("a CONCURRENT collector shares one container instead of merging; " +
        "its combiner may not be usable as an Aggregator's merge")
    else
      val supplier = c.supplier
      val accumulator = c.accumulator
      val combiner = c.combiner
      val finisher = c.finisher
      val identity =
        c.characteristics.contains(Collector.Characteristics.IDENTITY_FINISH)
      Right(new Aggregator[In, Acc, Out]:
        def init: Acc = supplier.get()
        def add(acc: Acc, in: In): Acc = { accumulator.accept(acc, in); acc }
        def merge(a: Acc, b: Acc): Acc = combiner.apply(a, b)
        def present(acc: Acc): Out =
          if identity then acc.asInstanceOf[Out] else finisher.apply(acc))
}
