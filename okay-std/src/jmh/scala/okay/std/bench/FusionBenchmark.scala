package okay.std.bench

import okay.{!, +, %, pure}
import okay.std.*

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * The SHIPPING-runner lanes of the classic's FusionBenchmark (specs/handler-fusion.md) on the new road: the same
 * N = 1000 get/set/tell programs — left-nested (foldLeft-built), right-nested, and the static 10-op block in a
 * recursion — built once (@Setup) and handled by the std's runners one after another (`nested*`) or by a chain
 * of `handle`s (`handled*`). On this road both are kernel handlers, one machine.
 *
 * The classic's `fused*` and `blockFree*` (okay-freer's `Fused` fixture loops), `ctrl*`, `blockFunc*`,
 * `blockCont*` (handler-passing over freer's `Control` carriers, `Cps`/`Func`) measure okay-freer's fusion and
 * carriers and have no counterpart here.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class FusionBenchmark {

  final val N = 1000

  type SW = State % Int + Writer % String
  type TSW = Throws % String + State % Int + Writer % String

  var sw: Int ! SW = scala.compiletime.uninitialized
  var tsw: Int ! TSW = scala.compiletime.uninitialized
  /** the RIGHT-NESTED twin: the same operations bound by recursion */
  var swR: Int ! SW = scala.compiletime.uninitialized

  def rightSW(i: Int, acc: Int): Int ! SW =
    if i >= N then pure(acc)
    else (i % 3) match
      case 0 => State.get[Int].at[SW].flatMap(x => rightSW(i + 1, acc + x))
      case 1 => State.set[Int](i).at[SW].flatMap(x => rightSW(i + 1, acc + x))
      case _ => Writer.tell("w").at[SW].flatMap(_ => rightSW(i + 1, acc + 1))

  // ---- a STATIC 10-op block inside a recursive loop: the same 1 000 operations as rightSW, ten per iteration,
  // spelled as explicit flatMaps

  final val Iters = N / 10

  def blockFree(i: Int, acc: Int): Int ! SW =
    if i >= Iters then pure(acc)
    else
      State.get[Int].at[SW].flatMap(a =>
      State.set[Int](i).at[SW].flatMap(_ =>
      Writer.tell("w").at[SW].flatMap(_ =>
      State.get[Int].at[SW].flatMap(b =>
      State.set[Int](i + 1).at[SW].flatMap(_ =>
      Writer.tell("w").at[SW].flatMap(_ =>
      State.get[Int].at[SW].flatMap(c =>
      State.set[Int](i + 2).at[SW].flatMap(_ =>
      Writer.tell("w").at[SW].flatMap(_ =>
      State.get[Int].at[SW].flatMap(d =>
        blockFree(i + 1, acc + a + b + c + d)))))))))))

  var blockR: Int ! SW = scala.compiletime.uninitialized

  /** the SHIPPING runners on the prebuilt block */
  @Benchmark
  def nestedBlockR(): Int =
    State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, SW](blockR))._2._2

  @Setup
  def up(): Unit =
    blockR = blockFree(0, 0)
    swR = rightSW(0, 0)
    sw = (0 until N).foldLeft(pure[Int, SW](0)): (m, i) =>
      m.flatMap: acc =>
        (i % 3) match
          case 0 => State.get[Int].at[SW].map(acc + _)
          case 1 => State.set[Int](i).at[SW].map(acc + _)
          case _ => Writer.tell("w").at[SW].map(_ => acc + 1)
    tsw = (0 until N).foldLeft(pure[Int, TSW](0)): (m, i) =>
      m.flatMap: acc =>
        (i % 3) match
          case 0 => State.get[Int].at[TSW].map(acc + _)
          case 1 => State.set[Int](i).at[TSW].map(acc + _)
          case _ => Writer.tell("w").at[TSW].map(_ => acc + 1)

  // ---- two effects

  /** Writer first, then State */
  @Benchmark
  def nestedSW(): Int =
    State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, SW](sw))._2._2

  /** State first, then Writer */
  @Benchmark
  def nestedWS(): Int =
    Writer.run[String, (Int, Int), Writer % String](State.handle[Int](0)(sw)).run._2._2

  // ---- the right-nested twin

  @Benchmark
  def nestedSWr(): Int =
    State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, SW](swR))._2._2

  // ---- three effects, the program never raises

  /** Writer, then State, then Throws */
  @Benchmark
  def nestedTSW(): Int =
    runEither[(Int, (Seq[String], Int)), Throws % String, String](
      State.handle[Int](0)(
        Writer.run[String, Int, TSW](tsw))).run.toOption.get._2._2

  // ---- the same programs through a chain of `handle`s

  @Benchmark
  def handledSW(): Int =
    val w: (Seq[String], Int) ! State % Int = sw.handle(Writer.log[String])
    w.handle(State(0)).run._2._2

  @Benchmark
  def handledSWr(): Int =
    val w: (Seq[String], Int) ! State % Int = swR.handle(Writer.log[String])
    w.handle(State(0)).run._2._2

  /** Writer and State, then Throws */
  @Benchmark
  def handledTSW(): Int =
    val w: (Seq[String], Int) ! (Throws % String + State % Int) = tsw.handle(Writer.log[String])
    val s: (Int, (Seq[String], Int)) ! Throws % String = w.handle(State(0))
    s.handle(Throws.either[String]).run.toOption.get._2._2
}
