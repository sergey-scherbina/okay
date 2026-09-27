package okay

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{State as JmhState, *}
import okay.Row.at

/**
 * THE SHAPE A PROGRAM IS BUILT IN (left-nested-build-cost, 2026-09-27).
 * The same N operations, built and run: once by `foldLeft` over
 * `flatMap` (left-nested, rotated by `Free.resume` one bind at a time)
 * and once by `!.each` / `!.foldM` (right-nested, never rotated). The
 * build is inside the measured method because every real caller builds
 * per call (a stage telling a chunk, a handler folding its choices).
 */
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@JmhState(Scope.Benchmark)
class BuildShapeBenchmark {
  val N = 1000
  val items: Vector[Int] = Vector.tabulate(N)(identity)

  /** a Writer telling every element, the shape of a Stage telling a chunk */
  @Benchmark
  def tellsFoldLeft(): Int =
    val p = items.foldLeft(pure[Writer % Int, Unit](()))((m, i) => m.flatMap(_ => Writer.tell(i)))
    !.run(Writer.run[Int, Unit, Pure](p))._1.length

  @Benchmark
  def tellsEach(): Int =
    val p = !.each(items)(i => Writer.tell(i))
    !.run(Writer.run[Int, Unit, Pure](p))._1.length

  /** a State counter with an accumulator, the shape of a fold with effects */
  @Benchmark
  def stateFoldLeft(): Int =
    val p = items.foldLeft(pure[State % Int, Int](0))((m, i) => m.flatMap(acc => State.modify[Int](_ + i).map(acc + _)))
    !.run(State.handle[Int](0)(p))._2

  @Benchmark
  def stateFoldM(): Int =
    val p = !.foldM(items)(0)((acc, i) => State.modify[Int](_ + i).map(acc + _))
    !.run(State.handle[Int](0)(p))._2

  /** TWO handlers over a mixed row, FusionBenchmark's `sw` program built
   * per call, the only difference the build: does the 32-vs-13 µs gap of
   * nestedSW/nestedSWr come from the shape when two passes run? */
  type SW = State % Int + Writer % String

  private def op(i: Int, acc: Int): Int ! SW = (i % 3) match
    case 0 => State.get[Int].at[SW].map(acc + _)
    case 1 => State.set[Int](i).at[SW].map(acc + _)
    case _ => Writer.tell("w").at[SW].map(_ => acc + 1)

  @Benchmark
  def rowFoldLeft(): Int =
    val p = items.foldLeft(pure[SW, Int](0))((m, i) => m.flatMap(acc => op(i, acc)))
    State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, State % Int](p))._2._2

  @Benchmark
  def rowFoldM(): Int =
    val p = !.foldM(items)(0)((acc, i) => op(i, acc))
    State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, State % Int](p))._2._2

  /** map-flatmap-pair-cost: the SAME right-nested program as rowFoldM,
   * built per call, but each step is ONE bind (the operation flatMapped
   * straight into the next step, as FusionBenchmark's rightSW) instead of
   * `op.map(acc + _)` and then foldM's flatMap */
  private def oneBind(i: Int, acc: Int): Int ! SW =
    if i >= N then pure(acc)
    else (i % 3) match
      case 0 => State.get[Int].at[SW].flatMap(x => oneBind(i + 1, acc + x))
      case 1 => State.set[Int](i).at[SW].flatMap(x => oneBind(i + 1, acc + x))
      case _ => Writer.tell("w").at[SW].flatMap(_ => oneBind(i + 1, acc + 1))

  @Benchmark
  def rowOneBind(): Int =
    State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, State % Int](oneBind(0, 0)))._2._2

  /** one-bind-hot-steps: stateFoldM's work written by hand with ONE
   * flatMap a step, the ceiling a fused foldM step aims at */
  private def stateOne(i: Int, acc: Int): Int ! State % Int =
    if i >= N then pure(acc) else State.modify[Int](_ + i).flatMap(s => stateOne(i + 1, acc + s))

  @Benchmark
  def stateOneBind(): Int =
    !.run(State.handle[Int](0)(stateOne(0, 0)))._2

  /** op-map-constructors: a step that is `State.update` — before, a get,
   * a set and a map; after, ONE `Update` operation */
  @Benchmark
  def stateUpdate(): Int =
    val p = !.foldM(items)(0)((acc, i) => State.update[Int, Int](s => (s, s + i)).map(acc + _))
    !.run(State.handle[Int](0)(p))._2

  /** reader-asks-op: a step that is `Reader.lift` (and `read`, the same
   * shape) — before, the shared Ask and a map; after, ONE `Asks` */
  @Benchmark
  def readerLift(): Int =
    val p = !.foldM(items)(0)((acc, i) => Reader.lift[Int, Int]((e: Int) ?=> e + i).map(acc + _))
    !.run(Reader.run[Int, Int, Pure](7)(p))
}
