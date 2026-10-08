package okay.freer


import okay.{TypeableK, typeableK}

import org.openjdk.jmh.annotations.{State, *}
import java.util.concurrent.TimeUnit
import scala.annotation.tailrec

/**
 * handler-single-pass stage 0 (specs/handler-single-pass.md, "Dispatch"): in ONE fused loop over a stack of
 * `handlers` stepped handlers, how does an operation find its handler? `chain` tests the stack's `TypeableK`s
 * innermost first, the way `split` does today. `table` looks the operation's EXACT class up in a per-stack
 * cache filled from those same tests on a miss. Everything else in the two loops is the same: N operations
 * spread round-robin over the handlers' effects, two operation classes per effect (a State has Get and
 * Update), each step bumping its handler's counter. A prototype; the program's row is erased (`Any`), since
 * only dispatch is measured.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class DispatchBenchmark {
  import DispatchBenchmark.*

  @Param(Array("2", "4", "8"))
  var handlers: Int = 0

  val N = 1000

  private var prog: Int ! Ops = scala.compiletime.uninitialized
  private var tests: Array[TypeableK[?]] = scala.compiletime.uninitialized

  @Setup
  def up(): Unit =
    tests = all.take(handlers).toArray
    // right-nested, the shape of an ordinary recursion: no rotation, dispatch alone
    prog = (0 until N).foldRight(pure[Ops, Int](0)): (i, rest) =>
      val h = i % handlers
      effect[Ops, Unit](op(h, i % 2)).flatMap(_ => rest)

  @Benchmark
  def chain(): Long =
    val counts = new Array[Long](handlers)
    val ts = tests
    def find(e: Any): Int =
      var i = 0
      while i < ts.length && !ts(i).test(e) do i += 1
      i
    run(prog, counts, find)

  @Benchmark
  def table(): Long =
    val counts = new Array[Long](handlers)
    val ts = tests
    // the per-stack cache: operation classes seen, and the handler each one went to
    val classes = new Array[Class[?]](16)
    val index = new Array[Int](16)
    var filled = 0
    def find(e: Any): Int =
      val c = e.getClass
      var j = 0
      while j < filled && (classes(j) ne c) do j += 1
      if j < filled then index(j)
      else
        var i = 0
        while i < ts.length && !ts(i).test(e) do i += 1
        classes(filled) = c
        index(filled) = i
        filled += 1
        i
    run(prog, counts, find)
}

object DispatchBenchmark:
  sealed trait E0[+A]; final case class A0() extends E0[Unit]; final case class B0() extends E0[Unit]
  sealed trait E1[+A]; final case class A1() extends E1[Unit]; final case class B1() extends E1[Unit]
  sealed trait E2[+A]; final case class A2() extends E2[Unit]; final case class B2() extends E2[Unit]
  sealed trait E3[+A]; final case class A3() extends E3[Unit]; final case class B3() extends E3[Unit]
  sealed trait E4[+A]; final case class A4() extends E4[Unit]; final case class B4() extends E4[Unit]
  sealed trait E5[+A]; final case class A5() extends E5[Unit]; final case class B5() extends E5[Unit]
  sealed trait E6[+A]; final case class A6() extends E6[Unit]; final case class B6() extends E6[Unit]
  sealed trait E7[+A]; final case class A7() extends E7[Unit]; final case class B7() extends E7[Unit]

  /** the erased row the prototype's program is typed at */
  type Ops[+A] = Any

  val all: List[TypeableK[?]] = List(
    typeableK[E0](classOf[E0[?]]), typeableK[E1](classOf[E1[?]]), typeableK[E2](classOf[E2[?]]), typeableK[E3](classOf[E3[?]]),
    typeableK[E4](classOf[E4[?]]), typeableK[E5](classOf[E5[?]]), typeableK[E6](classOf[E6[?]]), typeableK[E7](classOf[E7[?]]))

  def op(h: Int, which: Int): Any = (h, which) match
    case (0, 0) => A0() case (0, _) => B0() case (1, 0) => A1() case (1, _) => B1()
    case (2, 0) => A2() case (2, _) => B2() case (3, 0) => A3() case (3, _) => B3()
    case (4, 0) => A4() case (4, _) => B4() case (5, 0) => A5() case (5, _) => B5()
    case (6, 0) => A6() case (6, _) => B6() case (7, 0) => A7() case _ => B7()

  /** the fused loop both lanes share: the handler `find` names takes the operation, a step bumps its count */
  def run(p: Int ! Ops, counts: Array[Long], find: Any => Int): Long =
    @tailrec def loop(x: Int ! Ops): Int = (x.resume: @unchecked) match
      case Free.Return(a) => a
      case Free.Inject(e) => counts(find(e)) += 1; 0
      case Free.Bind(Free.Inject(e), k) => counts(find(e)) += 1; loop(HandleFrames.feed(k, ()))
    loop(p) + counts.sum
