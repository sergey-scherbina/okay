package okay.java

import okay.{Aggregator, Pane, Windows}
import java.util.function.{BiConsumer, BinaryOperator, Function as JFunction, Supplier}
import java.util.stream.Collector

/**
 * AN EVENT-TIME WINDOW AS A `Collector`.
 *
 * `Collect.collector` already says an okay `Aggregator` IS a JDK
 * `Collector`. This says the next thing: a WINDOW can be one too — and
 * it is the answer to the one real complaint docs/benchmarks.md §20
 * had about `java.util.stream`. There, the same job over the JDK held
 * every pane of the whole run alive and died with an
 * `OutOfMemoryError`, because `Collectors.groupingBy` has no notion of
 * a group being COMPLETE: a window there is only a key.
 *
 * The complaint is not really about `java.util.stream`. The collector
 * is OUR side of the seam, and an event-time window is a fold with a
 * watermark — so here the accumulator is an `okay.Windows` together
 * with a downstream aggregator's state: each pane is folded into the
 * downstream the moment the watermark CLOSES it, and evicted. What the
 * stream then holds is the live panes, not the history.
 *
 * IT IS SEQUENTIAL, AND THAT IS THE FINDING — not a limitation
 * anyone forgot to lift. A parallel `collect` splits the stream by
 * POSITION and combines partial containers bottom-up, so a container
 * in the middle of the tree holds a contiguous RANGE of the stream and
 * nothing else: its watermark is `max(its own elements) - lateness`,
 * which closes panes that elements of an EARLIER range still belong
 * to. Every such split then reports the same window with a partial
 * value, and no combiner can put them back together, because a folded
 * pane cannot be un-folded. The eviction that bounds the state is only
 * sound where the container knows it holds a PREFIX of the whole
 * stream — which is exactly the coordinator a stream engine has and a
 * `Collector` does not.
 *
 * So the combiner REFUSES rather than returning a plausible wrong
 * answer: a parallel stream fails loudly, and the caller either drops
 * `.parallel()` or keeps `Collectors.groupingBy` and its unbounded
 * state, knowingly. docs/benchmarks.md §20 measures both.
 *
 * TWO REQUIREMENTS ON THE CALLER, both real:
 *
 *   - `into` must not care about the ORDER of panes. Emission order
 *     inside a sweep is unspecified (specs/event-time-windows.md), so
 *     a sum, a count, a max, a `groupBy` are all fine and `first` /
 *     `last` are not.
 *   - the stream must be SEQUENTIAL and ordered — see above. On a
 *     parallel one the combiner throws.
 */
object Windowed {

  /** the accumulator: the operator, and what its closed panes fold into */
  final class State[K, A, Acc, O, S](val windows: Windows[K, A, Acc, O], var folded: S)

  /**
   * @param size     the window's width, in the unit of `at`
   * @param slide    = size for tumbling windows; smaller for sliding
   * @param lateness how far out of order the stream may be
   * @param key      the pane's key
   * @param at       the element's EVENT time
   * @param agg      what one window computes
   * @param into     what the closed panes are folded into — the
   *                 collector's result is this aggregator's
   */
  def collector[A, K, Acc, O, S, R](size: Long, slide: Long, lateness: Long)
                                   (key: A => K)(at: A => Long)
                                   (agg: Aggregator[A, Acc, O])
                                   (into: Aggregator[Pane[K, O], S, R])
  : Collector[A, State[K, A, Acc, O, S], R] =
    type Box = State[K, A, Acc, O, S]

    val supplier: Supplier[Box] = () =>
      new State(new Windows(size, slide, lateness, key, at, agg), into.init)

    val accumulator: BiConsumer[Box, A] = (box, a) =>
      box.windows.add(a)(p => box.folded = into.add(box.folded, p))

    // the combiner is the honest refusal described above: by the time
    // it is called, both sides have already evicted panes against
    // watermarks that were local to their own range, and the partial
    // reports cannot be put back together
    val combiner: BinaryOperator[Box] = (_, _) =>
      throw new UnsupportedOperationException(
        "okay.java.Windowed is a SEQUENTIAL collector: a parallel split evicts panes " +
          "against its own range's watermark, so each split reports the same window with a " +
          "partial value. Drop .parallel(), or window with Collectors.groupingBy and accept " +
          "that its state is the whole history (docs/benchmarks.md §20).")

    val finisher: JFunction[Box, R] = box =>
      box.windows.close()(p => box.folded = into.add(box.folded, p))
      into.present(box.folded)

    Collector.of(supplier, accumulator, combiner, finisher)

  /** the same, tumbling */
  def tumbling[A, K, Acc, O, S, R](size: Long, lateness: Long)
                                  (key: A => K)(at: A => Long)
                                  (agg: Aggregator[A, Acc, O])
                                  (into: Aggregator[Pane[K, O], S, R])
  : Collector[A, State[K, A, Acc, O, S], R] =
    collector(size, size, lateness)(key)(at)(agg)(into)
}
