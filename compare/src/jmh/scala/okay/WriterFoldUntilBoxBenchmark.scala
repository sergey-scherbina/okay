package okay

import okay.freer.{Fold, FoldUntil}

import okay.std.*
import org.openjdk.jmh.annotations.{State, *}
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
    pure[Unit, Writer % Long](()).flatMap(_ => go(0L))

  val boxedUntil: FoldUntil[Long, Long, Long] = FoldUntil[Long, Long, Long](0L)(_ + _)(_ => false)(identity)
  val untilOfLong: FoldUntil.OfLong[Long, Long] = FoldUntil.long[Long, Long](0L)(_ + _)(_ => false)(identity)

  /**
   * `Writer.foldUntil`'s walk, specialised to an `OfLong`: the state a
   * `long` in a register, `addLong`/`doneLong`/`endLong` at the
   * primitive. On the kernel the walk is `okay.Walk` (okay-freer's tree
   * walk is gone): each `Say` read off the program's head, the rest
   * resumed and walked again.
   */
  def protoWalk[W, A, R](a: A ! Writer % W)(K: FoldUntil.OfLong[W, R]): R = {
    @tailrec def loop(s: Long)(x: A ! Writer % W): R =
      if K.doneLong(s) then K.endLong(s)
      else Walk.view(x) match
        case Walk.Done(_) => K.endLong(s)
        case o: Walk.Op[Writer % W, u, A, Writer % W] => (o.op: @unchecked) match
          case Writer.Say(v) => loop(K.addLong(s, v))(o.resume(()))
    loop(K.initLong)(a)
  }

  @Benchmark
  def writerFoldUntilBoxed: Long = !.run(Writer.foldUntil[Long, Long, Unit, Long, Writer % Long](prog)(using summon, boxedUntil))

  @Benchmark
  def writerFoldUntilOfLong: Long = !.run(Writer.foldUntil[Long, Long, Unit, Long, Writer % Long](prog)(using summon, untilOfLong))

  @Benchmark
  def writerFoldUntilProto: Long = protoWalk(prog)(untilOfLong)

  @Benchmark
  def writerFoldSumLong: Long = !.run(Writer.fold[Long, Long, Unit, Writer % Long](prog)(using summon, Fold.sumLong))._1
}
