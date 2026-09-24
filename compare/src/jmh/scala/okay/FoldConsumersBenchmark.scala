package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/**
 * The other three Fold consumers, before and after the dispatch.
 *
 * `Chunks.fold` was specialized first because chunks amortize the tree
 * step and the fold's share of the cost is therefore largest there.
 * `Writer.fold` and `Stream.fold` step the tree once per ELEMENT, so
 * the fold is a smaller fraction — how much smaller is the question
 * these lanes answer, and it decides whether the dispatch was worth
 * writing or is merely harmless.
 *
 * Each lane pairs a specialized accumulator against a deliberately
 * generic one computing the same thing, so the difference is the
 * dispatch and nothing else.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class FoldConsumersBenchmark {

  val n = 10000

  /** a Fold[Long, Long] that is NOT one of the specialized shapes */
  val generic: Fold[Long, Long] = Fold(0L)((s: Long, a: Long) => s + a)

  /** the same arithmetic, accumulator declared primitive */
  val specialized: Fold.OfLong[Long] = Fold.sumLong

  def teller: Unit ! Writer % Long =
    def go(i: Long): Unit ! Writer % Long =
      if i >= n then okay.pure(())
      else Writer.tell(i).flatMap(_ => go(i + 1))
    go(0L)

  def producer: Producer[Long] =
    def go(i: Long): Producer[Long] =
      if i >= n then okay.pure(0L)
      else okay.produce(i).flatMap(_ => go(i + 1L))
    go(0L)

  // ---- Writer.fold

  @Benchmark
  def writerGeneric: Long = !.run(Writer.fold[Long, Long, Unit, Nothing](teller)(using summon)(using summon, generic))._1

  @Benchmark
  def writerSpecialized: Long = !.run(Writer.fold[Long, Long, Unit, Nothing](teller)(using summon)(using summon, specialized))._1

  // ---- Stream.fold, through a plain (un-chunked) Producer

  @Benchmark
  def streamGeneric: Long = Stream.fold(producer)(using generic)

  @Benchmark
  def streamSpecialized: Long = Stream.fold(producer)(using specialized)

  // ---- stream-fold-via-iterator: `Stream.fold` walks `iterator` now;
  // the walk it replaced — `uncons(_).runWith` per element, an Option,
  // a tuple and a program built and run each time — written out here
  // verbatim as the control rows, so each pair alternates in one run.

  private def foldControl[S[_], F[+_], A, B](s: S[A])(using f: Fold[A, B])(using St: Stream[S, F], H: Handler[F]): B = {
    @annotation.tailrec def loop(b: B, x: S[A]): B = St.uncons(x).runWith match
      case None => b
      case Some((a, t)) => loop(f.add(b, a), t)
    loop(f.init, s)
  }

  @Benchmark
  def streamGenericControl: Long = foldControl(producer)(using generic)

  @Benchmark
  def streamSpecializedControl: Long = foldControl(producer)(using specialized)

  // the postfix consumers: `toLazyList` (the old road, a memoised cell
  // and a synchronised lazy state per element) against `iterator`
  @Benchmark
  def foldLeftIterator: Long = producer.foldLeft(0L)(_ + _)

  @Benchmark
  def foldLeftLazyListControl: Long = producer.toLazyList.foldLeft(0L)(_ + _)
}
