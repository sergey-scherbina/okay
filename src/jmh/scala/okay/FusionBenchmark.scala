package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.RowLift.at

/**
 * specs/handler-fusion.md, stage 0 — the gate.
 *
 * What does running a row's handlers ONE AT A TIME cost against
 * running them fused, on the same program? The program is built once
 * per fork (@Setup) and only HANDLING is timed: a Free tree is
 * immutable and `resume` allocates fresh nodes on every walk, so the
 * same tree can be walked by every lane.
 *
 * Read B/op (-prof gc) before us/op. Stage A (split-without-either)
 * moved the fused loops and the shipping runners onto `split`; the
 * `<|>` numbers they replaced are the stage-0 rows in history.tsv. The cost model in the spec says
 * the nested run pays one Bind + one closure per operation per pass
 * that does not own it: on `nestedSW` every State operation once, on
 * `nestedTSW` every State operation once and every Writer operation
 * once more, plus a third walk that owns nothing because the program
 * never raises. The fused lanes should pay none of that. If the µs
 * ratio is under 1.3x the spec's Results record the refutation.
 *
 * N = 1000 operations, get/set/tell in rotation, foldLeft-built (the
 * left-nested shape, so every lane pays the same rotation).
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
  /** the RIGHT-NESTED twin (docs/benchmarks.md lane rules): the same
   * operations bound by recursion, so `resume` has nothing to rotate
   * and the lanes over it price the rebuild alone */
  var swR: Int ! SW = scala.compiletime.uninitialized

  def rightSW(i: Int, acc: Int): Int ! SW =
    if i >= N then pure(acc)
    else (i % 3) match
      case 0 => State.get[Int].at[SW].flatMap(x => rightSW(i + 1, acc + x))
      case 1 => State.set[Int](i).at[SW].flatMap(x => rightSW(i + 1, acc + x))
      case _ => Writer.tell("w").at[SW].flatMap(_ => rightSW(i + 1, acc + 1))

  // ---- stage B: the same right-nested program as Eff (no tree) and as
  // a handler-passing program over a Control carrier

  var effR: Eff[SW, Int] = scala.compiletime.uninitialized

  def rightEff(i: Int, acc: Int): Eff[SW, Int] =
    val E = Effects[Eff]
    if i >= N then E.pure(acc)
    else (i % 3) match
      case 0 => E.flatMap(E.perform[SW, Int](State.Get()))(x => rightEff(i + 1, acc + x))
      case 1 => E.flatMap(E.perform[SW, Int](State.Set(i)))(x => rightEff(i + 1, acc + x))
      case _ => E.flatMap(E.perform[SW, Unit](Writer.Say("w")))(_ => rightEff(i + 1, acc + 1))

  type R = Fused.Answer[Int, String, Int]
  def rightCtrl[C[_, _, _]](h: Interpr[SW, C, R])(using C: Control[C])(i: Int, acc: Int): C[Int, R, R] =
    if i >= N then C.pure(acc)
    else (i % 3) match
      case 0 => C.flatMap(h(State.Get()))(x => rightCtrl(h)(i + 1, acc + x))
      case 1 => C.flatMap(h(State.Set(i)))(x => rightCtrl(h)(i + 1, acc + x))
      case _ => C.flatMap(h(Writer.Say("w")))(_ => rightCtrl(h)(i + 1, acc + 1))

  @Benchmark
  def effSWr(): Int = Fused.runEff(0)(effR)._2

  /** the left-nested twin as Eff: the shape that overflowed before
   * eff-stack-safety; priced beside the right-nested one */
  var effL: Eff[SW, Int] = scala.compiletime.uninitialized

  @Benchmark
  def effSW(): Int = Fused.runEff(0)(effL)._2

  @Benchmark
  def ctrlContSWr(): Int = Fused.runCtrl[Cont, Int, String, Int](0)(h => rightCtrl[Cont](h)(0, 0))._2

  @Benchmark
  def ctrlFuncSWr(): Int = Fused.runCtrl[Func, Int, String, Int](0)(h => rightCtrl[Func](h)(0, 0))._2

  @Setup
  def up(): Unit =
    effR = rightEff(0, 0)
    effL =
      val E = Effects[Eff]
      (0 until N).foldLeft(E.pure[SW, Int](0)): (m: Eff[SW, Int], i) =>
        E.flatMap[SW, Int](m): acc =>
          (i % 3) match
            case 0 => E.flatMap[SW, Int](E.perform[SW, Int](State.Get()))(x => E.pure(acc + x))
            case 1 => E.flatMap[SW, Int](E.perform[SW, Int](State.Set(i)))(x => E.pure(acc + x))
            case _ => E.flatMap[SW, Unit](E.perform[SW, Unit](Writer.Say("w")))(_ => E.pure(acc + 1))
    swR = rightSW(0, 0)
    sw = (0 until N).foldLeft(pure[SW, Int](0)): (m, i) =>
      m.flatMap: acc =>
        (i % 3) match
          case 0 => State.get[Int].at[SW].map(acc + _)
          case 1 => State.set[Int](i).at[SW].map(acc + _)
          case _ => Writer.tell("w").at[SW].map(_ => acc + 1)
    tsw = (0 until N).foldLeft(pure[TSW, Int](0)): (m, i) =>
      m.flatMap: acc =>
        (i % 3) match
          case 0 => State.get[Int].at[TSW].map(acc + _)
          case 1 => State.set[Int](i).at[TSW].map(acc + _)
          case _ => Writer.tell("w").at[TSW].map(_ => acc + 1)

  // ---- two effects

  /** Writer first, then State: State operations rebuilt once */
  @Benchmark
  def nestedSW(): Int =
    State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, State % Int](sw))._2._2

  /** State first, then Writer: Writer operations rebuilt once */
  @Benchmark
  def nestedWS(): Int =
    !.run(Writer.run[String, (Int, Int), Pure](State.handle[Int, Int, Writer % String](0)(sw)))._2._2

  @Benchmark
  def fusedSW(): Int =
    Fused.stateWriter(0)(sw)._2

  // ---- the right-nested twin: no rotation, the rebuild alone

  @Benchmark
  def nestedSWr(): Int =
    State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, State % Int](swR))._2._2

  @Benchmark
  def fusedSWr(): Int =
    Fused.stateWriter(0)(swR)._2

  // ---- three effects, the program never raises

  /** Writer, then State, then Throws: two rebuilds per Writer op's
   * neighbours and a third walk that handles nothing */
  @Benchmark
  def nestedTSW(): Int =
    !.run(runEither[(Int, (Seq[String], Int)), Pure, String](
      State.handle[Int, (Seq[String], Int), Throws % String](0)(
        Writer.run[String, Int, Throws % String + State % Int](tsw)))).toOption.get._2._2

  @Benchmark
  def fusedTSW(): Int =
    Fused.throwsStateWriter(0)(tsw).toOption.get._2
}
