package okay.std.bench

import okay.{!, +, %, pure}
import okay.std.*
import okay.given

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * The SHIPPING runners of the classic's SplitBenchmark on the new road: Writer and State each alone over a program
 * of their own operations with `Produce` in the row (never performed), and the mixed State/Writer program under
 * the Writer accumulators. The classic's `*Either`/`*Extract` (benchmark-local loops over freer's `resume` on
 * `<|>` and the old extractor) and `writerMut`/`mixedMut` (a benchmark-local freer loop with a mutable
 * accumulator) measure okay-freer's own dispatch and have no counterpart. The residual `Produce` is answered by
 * `Stream.runWith` (`Answers[Produce]`, the kernel's `Answering`) where the classic called `.runWith`.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class SplitBenchmark {

  final val N = 1000
  type WR = Writer % String + Produce
  type SR = State % Int + Produce

  var wp: Int ! WR = scala.compiletime.uninitialized
  var sp: Int ! SR = scala.compiletime.uninitialized

  def writerProg(i: Int, acc: Int): Int ! WR =
    if i >= N then pure(acc) else Writer.tell("w").at[WR].flatMap(_ => writerProg(i + 1, acc + 1))
  def stateProg(i: Int, acc: Int): Int ! SR =
    if i >= N then pure(acc)
    else if i % 2 == 0 then State.get[Int].at[SR].flatMap(x => stateProg(i + 1, acc + x))
    else State.set[Int](i).at[SR].flatMap(x => stateProg(i + 1, acc + x))

  @Setup
  def up(): Unit =
    mixed = mixedProg(0, 0)
    wp = writerProg(0, 0)
    sp = stateProg(0, 0)

  // ---- Writer: the shipping runner

  @Benchmark
  def writerShip(): Int = okay.std.Stream.runWith(Writer.run[String, Int, WR](wp))._2

  /** the Vector accumulator through the default `Fold[A, Seq[A]]` — same run as `writerShip` */
  @Benchmark
  def writerShipVec(): Int = okay.std.Stream.runWith(Writer.fold[String, Seq[String], Int, WR](wp))._2

  // ---- the MIXED shape at 333 tells (FusionBenchmark's program), both accumulators

  type SW = State % Int + Writer % String
  var mixed: Int ! SW = scala.compiletime.uninitialized
  def mixedProg(i: Int, acc: Int): Int ! SW =
    if i >= N then pure(acc)
    else (i % 3) match
      case 0 => State.get[Int].at[SW].flatMap(x => mixedProg(i + 1, acc + x))
      case 1 => State.set[Int](i).at[SW].flatMap(x => mixedProg(i + 1, acc + x))
      case _ => Writer.tell("w").at[SW].flatMap(_ => mixedProg(i + 1, acc + 1))

  @Benchmark
  def mixedList(): Int = State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, SW](mixed))._2._2

  @Benchmark
  def mixedVec(): Int = State.run[Int, (Seq[String], Int)](0)(Writer.fold[String, Seq[String], Int, SW](mixed))._2._2

  /** the List road WITHOUT the final reverse+map */
  @Benchmark
  def mixedListNoRev(): Int =
    State.run[Int, (List[String], Int)](0)(Writer.foldWith[String, List[String], Int, SW](mixed)(Nil)((s, w) => w :: s))._2._2

  /** Vector through `foldWith` with the step inline (no `Fold` virtual call) */
  @Benchmark
  def mixedVecInline(): Int =
    State.run[Int, (Vector[String], Int)](0)(Writer.foldWith[String, Vector[String], Int, SW](mixed)(Vector.empty)((s, w) => s :+ w))._2._2

  // ---- State: the shipping runner

  @Benchmark
  def stateShip(): Int = okay.std.Stream.runWith(State.handle[Int](0)(sp))._2
}
