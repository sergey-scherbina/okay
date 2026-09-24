package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scala.annotation.tailrec

/**
 * Does the unboxed `FoldUntil` dispatch belong in `Writer.foldUntil`
 * too? (writer-fold-until-unboxed; specs/fold-until.md Results said
 * "the tree step dominates" — a guess.) The same shape as
 * `FoldUntilBoxBenchmark`, on the tree walk instead of the chunk loop:
 * 10 000 Longs told by a writer program, summed into a Long, the stop
 * never firing.
 *
 *   writerFoldUntilBoxed   Writer.foldUntil, a generic FoldUntil             (shipped)
 *   writerFoldUntilOfLong  Writer.foldUntil, FoldUntil.long — the walk IGNORES the shape today,
 *                          so this is the boxed price paid through `final def add`
 *   writerFoldUntilProto   the same walk with addLong/doneLong on a long  (the dispatch's ceiling)
 *   writerFoldSumLong      Writer.fold(Fold.sumLong) — what Fold's dispatch already buys on this walk
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class WriterFoldUntilBoxBenchmark {

  val n = 10000L

  /** the same shape as Source.range: a tell, then the rest */
  def prog: Unit ! Writer % Long =
    def go(i: Long): Unit ! Writer % Long =
      if i >= n then pure(()) else Writer.tell(i).flatMap(_ => go(i + 1))
    pure[Writer % Long, Unit](()).flatMap(_ => go(0L))

  val boxedUntil: FoldUntil[Long, Long, Long] = FoldUntil[Long, Long, Long](0L)(_ + _)(_ => false)(identity)
  val untilOfLong: FoldUntil.OfLong[Long, Long] = FoldUntil.long[Long, Long](0L)(_ + _)(_ => false)(identity)

  /**
   * `Writer.foldUntil`'s walk, specialised to an `OfLong`: the state a
   * `long` in a register, `addLong`/`doneLong`/`endLong` at the
   * primitive. Generic in F for the same reason the library walks are
   * — at `F = Nothing` the inline `split` does not compile.
   */
  def protoWalk[W, A, R, F[+_]](a: A ! Writer % W + F)(K: FoldUntil.OfLong[W, R])
                                (using TypeableK[Writer % W]): R ! F = {
    import !.*
    def _loop(s: Long)(x: A ! Writer % W + F): R ! F = loop(s)(x)
    @tailrec def loop(s: Long)(x: A ! Writer % W + F): R ! F =
      if K.doneLong(s) then Return(K.endLong(s))
      else (x.resume: @unchecked) match
        case Return(_) => Return(K.endLong(s))
        case Inject(e) => split[Writer % W, F](e) {
            case Writer.Say(v) => Return(K.endLong(K.addLong(s, v))): R ! F
          } { e => Inject(e).map(_ => K.endLong(s)) }
        case Bind(Inject(e), k) => split[Writer % W, F](e) { w0 =>
            (w0: @unchecked) match
              case Writer.Say(v) => loop(K.addLong(s, v))(k(()))
          } { e => Inject(e).flatMap(x => _loop(s)(k(x))) }
    loop(K.initLong)(a)
  }

  @Benchmark
  def writerFoldUntilBoxed: Long = !.run(Writer.foldUntil[Long, Long, Unit, Long, Nothing](prog)(using summon)(using summon, boxedUntil))

  @Benchmark
  def writerFoldUntilOfLong: Long = !.run(Writer.foldUntil[Long, Long, Unit, Long, Nothing](prog)(using summon)(using summon, untilOfLong))

  @Benchmark
  def writerFoldUntilProto: Long = !.run(protoWalk[Long, Unit, Long, Nothing](prog)(untilOfLong))

  @Benchmark
  def writerFoldSumLong: Long = !.run(Writer.fold[Long, Long, Unit, Nothing](prog)(using summon)(using summon, Fold.sumLong))._1
}
