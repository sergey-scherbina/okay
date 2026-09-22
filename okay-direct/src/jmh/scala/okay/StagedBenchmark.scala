package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.Direct.*

/**
 * specs/direct-staged.md, the number: the SAME block text three ways
 * — a Free `direct` block run by the shipping runners (what a user
 * has today), the staged `direct` block, and the staged program
 * written by hand (`sw.stage(op)` at every operation — the parity
 * target, 84 568 B/op on core's FusionBenchmark.blockFuncInlineMatchR).
 * 1 000 operations as a static 10-op block inside a 100-iteration
 * loop, the shape staged-block-lanes measured.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class StagedBenchmark {

  final val Iters = 100

  type Row = State % Int + Writer % String
  type R = Stager.Answer[Int, String, Int]
  val sw = Stager.StateWriter[Int, String, Int]()

  def freeBlock(i: Int, acc: Int): Int ! Row =
    if i >= Iters then pure(acc)
    else direct[[A] =>> A ! Row] {
      val a = State.get[Int].!?
      val _ = State.set[Int](i).!?
      Writer.tell("w").!?
      val b = State.get[Int].!?
      val _ = State.set[Int](i + 1).!?
      Writer.tell("w").!?
      val c = State.get[Int].!?
      val _ = State.set[Int](i + 2).!?
      Writer.tell("w").!?
      val d = State.get[Int].!?
      freeBlock(i + 1, acc + a + b + c + d).!?
    }

  def stagedBlock(i: Int, acc: Int): Handled[Row, R, Int] =
    if i >= Iters then Handled.pure(acc)
    else Direct.staged(sw) {
      val a = State.get[Int].!?
      val _ = State.set[Int](i).!?
      Writer.tell("w").!?
      val b = State.get[Int].!?
      val _ = State.set[Int](i + 1).!?
      Writer.tell("w").!?
      val c = State.get[Int].!?
      val _ = State.set[Int](i + 2).!?
      Writer.tell("w").!?
      val d = State.get[Int].!?
      stagedBlock(i + 1, acc + a + b + c + d).!?
    }

  /** the parity target: what the macro should emit, by hand */
  def handBlock(i: Int, acc: Int): Handled[Row, R, Int] =
    if i >= Iters then Handled.pure(acc)
    else
      val M = summon[Monad[Handled[Row, R, *]]]
      M.flatMap(sw.stage(State.Get()))(a =>
      M.flatMap(sw.stage(State.Set(i)))(_ =>
      M.flatMap(sw.stage(Writer.Say("w")))(_ =>
      M.flatMap(sw.stage(State.Get()))(b =>
      M.flatMap(sw.stage(State.Set(i + 1)))(_ =>
      M.flatMap(sw.stage(Writer.Say("w")))(_ =>
      M.flatMap(sw.stage(State.Get()))(c =>
      M.flatMap(sw.stage(State.Set(i + 2)))(_ =>
      M.flatMap(sw.stage(Writer.Say("w")))(_ =>
      M.flatMap(sw.stage(State.Get()))(d => handBlock(i + 1, acc + a + b + c + d)))))))))))

  /** the user's baseline: the Free block, built and run by the shipping runners */
  @Benchmark
  def freeDirectNested(): Int =
    State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, State % Int](freeBlock(0, 0)))._2._2

  @Benchmark
  def stagedDirect(): Int = sw.run(0)(stagedBlock(0, 0))._2

  @Benchmark
  def stagedHand(): Int = sw.run(0)(handBlock(0, 0))._2
}
