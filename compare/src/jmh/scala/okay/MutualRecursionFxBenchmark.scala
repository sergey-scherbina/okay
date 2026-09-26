package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.Row.at

/**
 * stack-safe-mutual-recursion, with effects (2026-09-26): the SAME
 * unbounded mutual tail recursion as MutualRecursionBenchmark
 * (isEven/isOdd, N = 1 000 000), now doing work on the way — every
 * level counts itself on a `Metrics`, and every 1 000th level writes a
 * line to a `AppLog`. On the JVM roads those are plain interfaces
 * passed as parameters, the way a service gets its logger and metrics;
 * the calls go INSIDE each road's own suspension (the trampoline's
 * thunk, `Eval.defer`, `IO.defer`, `ZIO.suspendSucceed`, kyo's `IO`,
 * okay's `!.tailcall`), so the machinery stays one suspension per
 * level for everyone and only the interface calls are added.
 *
 * `okayRow` is the idiomatic okay program instead: the count and the
 * log are EFFECTS in its type, `Int ! State % Int + Writer % String`,
 * given their meaning by `State.run` and `Writer.run` — no interface
 * passed anywhere, the handlers are the implementation.
 *
 * Every lane answers (isEven(N), levels counted, lines logged),
 * checked equal before any is timed.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class MutualRecursionFxBenchmark {
  import MutualRecursionFxBenchmark.*

  final val N = 1000000

  @Benchmark
  def stateMachine(): Answer =
    val c = MetricsImpl(); val l = AppLogImpl()
    var n = N
    var even = true
    while n != 0 do
      step(n, c, l)
      n -= 1
      even = !even
    Answer(even, c.count, l.lines)

  @Benchmark
  def handTrampoline(): Answer =
    val c = MetricsImpl(); val l = AppLogImpl()
    Answer(Bounce.run(Bounce.even(N, c, l)), c.count, l.lines)

  @Benchmark
  def scalaTailCalls(): Answer =
    val c = MetricsImpl(); val l = AppLogImpl()
    Answer(TC.even(N, c, l).result, c.count, l.lines)

  @Benchmark
  def catsEval(): Answer =
    val c = MetricsImpl(); val l = AppLogImpl()
    Answer(CatsEval.even(N, c, l).value, c.count, l.lines)

  @Benchmark
  def catsIO(): Answer =
    import cats.effect.unsafe.implicits.global
    val c = MetricsImpl(); val l = AppLogImpl()
    Answer(CatsIO.even(N, c, l).unsafeRunSync(), c.count, l.lines)

  @Benchmark
  def zio(): Answer =
    import _root_.zio.*
    val c = MetricsImpl(); val l = AppLogImpl()
    val b = Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(Zio.even(N, c, l)).getOrThrowFiberFailure())
    Answer(b, c.count, l.lines)

  @Benchmark
  def kyoIO(): Answer =
    import _root_.kyo.*
    import AllowUnsafe.embrace.danger
    val c = MetricsImpl(); val l = AppLogImpl()
    Answer(IO.Unsafe.evalOrThrow(KyoIO.even(N, c, l)), c.count, l.lines)

  @Benchmark
  def bigStackThread(): Answer =
    val c = MetricsImpl(); val l = AppLogImpl()
    val call: java.util.concurrent.Callable[Boolean] = () => Direct.even(N, c, l)
    Answer(bigStack.submit(call).get(), c.count, l.lines)

  @Benchmark
  def exceptionUnwind(): Answer =
    val c = MetricsImpl(); val l = AppLogImpl()
    Answer(Unwind.run(() => Unwind.even(N, 0, c, l)), c.count, l.lines)

  @Benchmark
  def vtSegments(): Answer =
    val c = MetricsImpl(); val l = AppLogImpl()
    Answer(Segments.even(N, 0, c, l), c.count, l.lines)

  @Benchmark
  def okayFree(): Answer =
    val c = MetricsImpl(); val l = AppLogImpl()
    Answer(!.run(OkayFree.even(N, c, l)), c.count, l.lines)

  @Benchmark
  def okayRow(): Answer =
    val (count, (log, b)) =
      State.run[Int, (Seq[String], Boolean)](0)(Writer.run[String, Boolean, State % Int](OkayRow.even(N)))
    Answer(b, count, log.size)

  private val bigStack = java.util.concurrent.Executors.newSingleThreadExecutor { r =>
    val t = new Thread(null, r, "big-stack", 1L << 30)
    t.setDaemon(true)
    t
  }

  @TearDown(Level.Trial)
  def stop(): Unit = bigStack.shutdownNow(): Unit

  @Setup(Level.Trial)
  def check(): Unit =
    val expected = stateMachine()
    if expected != Answer(true, N, N / 1000) then throw new IllegalStateException(s"the floor answers $expected")
    val answers = Map(
      "handTrampoline" -> handTrampoline(), "scalaTailCalls" -> scalaTailCalls(),
      "catsEval" -> catsEval(), "catsIO" -> catsIO(), "zio" -> zio(), "kyoIO" -> kyoIO(),
      "bigStackThread" -> bigStackThread(), "exceptionUnwind" -> exceptionUnwind(),
      "vtSegments" -> vtSegments(), "okayFree" -> okayFree(), "okayRow" -> okayRow())
    for (lane, a) <- answers if a != expected do
      throw new IllegalStateException(s"$lane answers $a, expected $expected")
}

object MutualRecursionFxBenchmark {

  final case class Answer(even: Boolean, counted: Int, logged: Int)

  /** the effects as a service gets them: interfaces, passed in */
  trait Metrics { def inc(): Unit; def count: Int }
  trait AppLog { def log(line: String): Unit; def lines: Int }
  final class MetricsImpl extends Metrics { private var c = 0; def inc(): Unit = c += 1; def count: Int = c }
  final class AppLogImpl extends AppLog {
    private val buf = scala.collection.mutable.ArrayBuffer.empty[String]
    def log(line: String): Unit = { val _ = buf.addOne(line) }
    def lines: Int = buf.size
  }

  /** what every level does before it recurses */
  inline def step(n: Int, c: Metrics, l: AppLog): Unit =
    c.inc()
    if n % 1000 == 0 then l.log(s"level $n")

  object Direct {
    def even(n: Int, c: Metrics, l: AppLog): Boolean = if n == 0 then true else { step(n, c, l); odd(n - 1, c, l) }
    def odd(n: Int, c: Metrics, l: AppLog): Boolean = if n == 0 then false else { step(n, c, l); even(n - 1, c, l) }
  }

  sealed trait Bounce
  object Bounce {
    final case class Done(b: Boolean) extends Bounce
    final case class More(next: () => Bounce) extends Bounce
    def even(n: Int, c: Metrics, l: AppLog): Bounce =
      if n == 0 then Done(true) else More(() => { step(n, c, l); odd(n - 1, c, l) })
    def odd(n: Int, c: Metrics, l: AppLog): Bounce =
      if n == 0 then Done(false) else More(() => { step(n, c, l); even(n - 1, c, l) })
    def run(b: Bounce): Boolean =
      var cur = b
      var answer = false
      var done = false
      while !done do
        cur match
          case Done(x) => { answer = x; done = true }
          case More(k) => cur = k()
      answer
  }

  object TC {
    import scala.util.control.TailCalls.*
    def even(n: Int, c: Metrics, l: AppLog): TailRec[Boolean] =
      if n == 0 then done(true) else tailcall { step(n, c, l); odd(n - 1, c, l) }
    def odd(n: Int, c: Metrics, l: AppLog): TailRec[Boolean] =
      if n == 0 then done(false) else tailcall { step(n, c, l); even(n - 1, c, l) }
  }

  object CatsEval {
    import cats.Eval
    def even(n: Int, c: Metrics, l: AppLog): Eval[Boolean] =
      if n == 0 then Eval.True else Eval.defer { step(n, c, l); odd(n - 1, c, l) }
    def odd(n: Int, c: Metrics, l: AppLog): Eval[Boolean] =
      if n == 0 then Eval.False else Eval.defer { step(n, c, l); even(n - 1, c, l) }
  }

  object CatsIO {
    import cats.effect.IO
    def even(n: Int, c: Metrics, l: AppLog): IO[Boolean] =
      if n == 0 then IO.pure(true) else IO.defer { step(n, c, l); odd(n - 1, c, l) }
    def odd(n: Int, c: Metrics, l: AppLog): IO[Boolean] =
      if n == 0 then IO.pure(false) else IO.defer { step(n, c, l); even(n - 1, c, l) }
  }

  object Zio {
    import _root_.zio.*
    def even(n: Int, c: Metrics, l: AppLog): UIO[Boolean] =
      if n == 0 then ZIO.succeed(true) else ZIO.suspendSucceed { step(n, c, l); odd(n - 1, c, l) }
    def odd(n: Int, c: Metrics, l: AppLog): UIO[Boolean] =
      if n == 0 then ZIO.succeed(false) else ZIO.suspendSucceed { step(n, c, l); even(n - 1, c, l) }
  }

  object KyoIO {
    import _root_.kyo.*
    def even(n: Int, c: Metrics, l: AppLog): Boolean < IO =
      if n == 0 then true else IO { step(n, c, l); odd(n - 1, c, l) }
    def odd(n: Int, c: Metrics, l: AppLog): Boolean < IO =
      if n == 0 then false else IO { step(n, c, l); even(n - 1, c, l) }
  }

  object Unwind {
    final class Resume(val next: () => Boolean) extends RuntimeException(null, null, false, false)
    final val Limit = 1000
    def even(n: Int, d: Int, c: Metrics, l: AppLog): Boolean =
      if n == 0 then true
      else if d >= Limit then throw Resume(() => even(n, 0, c, l))
      else { step(n, c, l); odd(n - 1, d + 1, c, l) }
    def odd(n: Int, d: Int, c: Metrics, l: AppLog): Boolean =
      if n == 0 then false
      else if d >= Limit then throw Resume(() => odd(n, 0, c, l))
      else { step(n, c, l); even(n - 1, d + 1, c, l) }
    def run(start: () => Boolean): Boolean =
      var next = start
      var answer = false
      var done = false
      while !done do
        try { answer = next(); done = true }
        catch case r: Resume => next = r.next
      answer
  }

  object Segments {
    final val Limit = 1000
    def even(n: Int, d: Int, c: Metrics, l: AppLog): Boolean =
      if n == 0 then true
      else if d >= Limit then hop(() => even(n, 0, c, l))
      else { step(n, c, l); odd(n - 1, d + 1, c, l) }
    def odd(n: Int, d: Int, c: Metrics, l: AppLog): Boolean =
      if n == 0 then false
      else if d >= Limit then hop(() => odd(n, 0, c, l))
      else { step(n, c, l); even(n - 1, d + 1, c, l) }
    private def hop(rest: () => Boolean): Boolean =
      val answer = new java.util.concurrent.CompletableFuture[Boolean]()
      Thread.startVirtualThread { () =>
        try answer.complete(rest()): Unit
        catch case e: Throwable => answer.completeExceptionally(e): Unit
      }: Unit
      answer.join()
  }

  object OkayFree {
    def even(n: Int, c: Metrics, l: AppLog): Boolean ! Pure =
      if n == 0 then pure(true) else !.tailcall { step(n, c, l); odd(n - 1, c, l) }
    def odd(n: Int, c: Metrics, l: AppLog): Boolean ! Pure =
      if n == 0 then pure(false) else !.tailcall { step(n, c, l); even(n - 1, c, l) }
  }

  /** the idiomatic okay program: the count and the log are effects in
   * the type, the handlers their implementation */
  object OkayRow {
    type Fx = State % Int + Writer % String
    private def stepFx(n: Int): Unit ! Fx =
      val counted = State.modify[Int](_ + 1).at[Fx]
      if n % 1000 == 0 then counted.flatMap(_ => Writer.tell(s"level $n").at[Fx]) else counted
    def even(n: Int): Boolean ! Fx =
      if n == 0 then pure(true) else stepFx(n).flatMap(_ => odd(n - 1))
    def odd(n: Int): Boolean ! Fx =
      if n == 0 then pure(false) else stepFx(n).flatMap(_ => even(n - 1))
  }
}
