package okay2.bench

import org.openjdk.jmh.annotations.{State => JmhState, _}
import java.util.concurrent.TimeUnit

import okay2._

/** an operation carrying its own answer — the root build's `Ask`, as a
 * Scala 2 signature */
sealed trait Ask extends Row { type Op[+A] = Ask.Op[A] }
object Ask {
  final case class Op[+A](a: A)
  implicit val effect: Effect[Ask] = Effect.of[Ask]
}

/**
 * okay2-bench: the Scala 3 core's HandlerBenchmark (src/jmh/scala/okay/
 * HandlerBenchmark.scala), lane for lane where a lane is named the same —
 * same program shape, same size, same annotations — so that the ratio
 * between a lane here and its twin there prices "no inline" and the
 * Either-per-operation split (backlog okay2-bench, okay2-handler-allocs).
 *
 * The program is built at each signature and widened by subtyping, not
 * by `effect[Ask + Produce, Int]` as in Scala 3: an intersection's `#Op`
 * is its last parent's in scalac 2 (spec stage 8).
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class HandlerBenchmark {

  final val N = 10000

  /** 10k ops, every 100th handled (Ask), the rest forwarded (Produce) */
  def prog: Int ! (Ask + Produce) =
    (1 to N).foldLeft(Free.inject[Ask, Int](Ask.Op(0)): Int ! (Ask + Produce)) { (m, i) =>
      m.flatMap[Ask with Produce, Int] { x =>
        if (i % 100 == 0) Free.inject[Ask, Int](Ask.Op(x + 1))
        else Free.inject[Produce, Int](Produce.Emit(x + 1))
      }
    }

  private val relayAsk: Relay[Ask] = new Relay[Ask] {
    def apply[X, Y](e: Ask.Op[X]): X /> Y = Cont.Pure[X, Y](e.a)
  }

  private val handleAsk: Interpr[Ask, Int ! Produce] = new Interpr[Ask, Int ! Produce] {
    def apply[X](e: Ask.Op[X]): Cont[X, Int ! Produce, Int ! Produce] = Cont.Pure[X, Int ! Produce](e.a)
  }

  @Benchmark
  def relayForward(): Int =
    Effects.relay[Int, Int, Ask, Produce](prog)(pure[Produce, Int](_))(relayAsk).runWith

  @Benchmark
  def buildOnly(): Any = prog

  private var built: Int ! (Ask + Produce) = _

  /** the instrument's own control: every lane answers what the program
   * means, checked once per trial before anything is timed — a lane that
   * returned a believable number for the wrong program would otherwise
   * look like a result */
  @Setup(Level.Trial)
  def buildOnce(): Unit = {
    built = prog
    def check(lane: String, got: Any, want: Any): Unit =
      if (got != want) throw new IllegalStateException(s"$lane answered $got, not $want")
    check("relayPrebuilt", relayPrebuilt(), N)
    check("handlePrebuilt", handlePrebuilt(), N)
    check("relayForward", relayForward(), N)
    check("stateEffect", stateEffect()._1, M.toLong)
  }

  @Benchmark
  def relayPrebuilt(): Int =
    Effects.relay[Int, Int, Ask, Produce](built)(pure[Produce, Int](_))(relayAsk).runWith

  @Benchmark
  def handlePrebuilt(): Int =
    Effects.handleWith[Int, Int, Ask, Produce](built)(pure[Produce, Int](_))(handleAsk).runWith

  final val M = 1000

  @Benchmark
  def stateEffect(): (Long, Long) =
    State.run(0L)(
      (1 to M).foldLeft(pure[State[Long], Long](0L)) { (m, _) =>
        m.flatMap[State[Long], Long](_ => State.get[Long].flatMap(s => State.set[Long](s + 1)))
      })
}
