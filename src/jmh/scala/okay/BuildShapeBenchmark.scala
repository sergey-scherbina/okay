package okay

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{State as JmhState, *}

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
}
