package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * stack-safe-mutual-recursion (2026-09-26): the like-for-like task.
 *
 * The plain loop and the virtual-call lanes (okay-direct
 * StagedBenchmark, docs/benchmarks.md §2c) measure what a handler costs
 * against code that does not need one. This measures a task plain JVM
 * code cannot do at all: UNBOUNDED mutual tail recursion — `isEven`
 * calls `isOdd` calls `isEven`, N = 1 000 000 deep. The JVM has no tail
 * calls, so the direct version overflows the stack (the trial setup
 * proves it on the benchmark thread), and every lane below brings its
 * own machinery to run it in constant stack:
 *
 *   stateMachine    the loop a compiler WITH tail calls would emit —
 *                   the floor, no recursion left to make safe
 *   handTrampoline  Done/More and a while loop, by hand
 *   scalaTailCalls  the standard library's trampoline
 *   catsEval, catsIO, zio, kyoIO   each library's own suspension
 *   okayFree        `!.tailcall` on a Free program, `!.run`
 *   okayCont        `Cont.delay` on a Cont program, `reset`
 *
 * and the JVM roads that are NOT trampolines:
 *
 *   bigStackThread  the direct recursion, on a pooled platform thread
 *                   with a 1 GB stack: bounded by memory, not by -Xss
 *   exceptionUnwind the direct recursion, unwound every 1 000 levels by
 *                   a stackless exception carrying what comes next
 *                   (Baker, "CONS should not CONS its arguments, part
 *                   II: Cheney on the M.T.A.", 1995) — tail calls only
 *   vtSegments      the direct recursion, continued on a fresh virtual
 *                   thread every 1 000 levels while the current one
 *                   parks: the stack as a chain of heap-held segments,
 *                   the same idea as okay's cont-stack switch; works
 *                   for non-tail recursion too
 *   iteratorUnfold  no recursion left: the pair rewritten as a stepped
 *                   state with Iterator.iterate — the state machine
 *                   built from library parts
 *
 * Not measured: jdk.internal.vm.Continuation (not a public API), and
 * compiler tail calls (@tailrec, Kotlin tailrec take self-recursion
 * only; merging the pair by hand IS stateMachine).
 *
 * Every lane answers `isEven(N)`, checked equal before any is timed.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class MutualRecursionBenchmark {
  import MutualRecursionBenchmark.*

  final val N = 1000000

  @Benchmark
  def stateMachine(): Boolean =
    var n = N
    var even = true // which of the two functions is running
    var answer = false
    var done = false
    while !done do
      if n == 0 then { answer = even; done = true }
      else { n -= 1; even = !even }
    answer

  @Benchmark
  def handTrampoline(): Boolean = Bounce.run(Bounce.even(N))

  @Benchmark
  def scalaTailCalls(): Boolean = TC.even(N).result

  @Benchmark
  def catsEval(): Boolean = CatsEval.even(N).value

  @Benchmark
  def catsIO(): Boolean =
    import cats.effect.unsafe.implicits.global
    CatsIO.even(N).unsafeRunSync()

  @Benchmark
  def zio(): Boolean =
    import _root_.zio.*
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(Zio.even(N)).getOrThrowFiberFailure())

  @Benchmark
  def kyoIO(): Boolean =
    import _root_.kyo.*
    import AllowUnsafe.embrace.danger
    IO.Unsafe.evalOrThrow(KyoIO.even(N))

  @Benchmark
  def bigStackThread(): Boolean = bigStack.submit(evenCall).get()

  @Benchmark
  def exceptionUnwind(): Boolean = Unwind.run(() => Unwind.even(N, 0))

  @Benchmark
  def vtSegments(): Boolean = Segments.even(N, 0)

  @Benchmark
  def iteratorUnfold(): Boolean =
    Iterator.iterate((N, true))((n, even) => (n - 1, !even)).dropWhile(_._1 > 0).next()._2

  @Benchmark
  def okayFree(): Boolean = !.run(OkayFree.even(N))

  @Benchmark
  def okayCont(): Boolean = reset(OkayCont.even(N))

  private val bigStack = java.util.concurrent.Executors.newSingleThreadExecutor { r =>
    val t = new Thread(null, r, "big-stack", 1L << 30)
    t.setDaemon(true)
    t
  }
  private val evenCall: java.util.concurrent.Callable[Boolean] = () => Direct.even(N)

  @TearDown(Level.Trial)
  def stop(): Unit = bigStack.shutdownNow(): Unit

  @Setup(Level.Trial)
  def check(): Unit =
    // the task is real: the direct version cannot run at this depth
    val overflowed =
      try { Direct.even(N); false }
      catch case _: StackOverflowError => true
    if !overflowed then throw new IllegalStateException(s"direct mutual recursion did not overflow at $N")
    val expected = stateMachine()
    val answers = Map(
      "handTrampoline" -> handTrampoline(), "scalaTailCalls" -> scalaTailCalls(),
      "catsEval" -> catsEval(), "catsIO" -> catsIO(), "zio" -> zio(), "kyoIO" -> kyoIO(),
      "bigStackThread" -> bigStackThread(), "exceptionUnwind" -> exceptionUnwind(),
      "vtSegments" -> vtSegments(), "iteratorUnfold" -> iteratorUnfold(),
      "okayFree" -> okayFree(), "okayCont" -> okayCont())
    for (lane, a) <- answers if a != expected do
      throw new IllegalStateException(s"$lane answers $a, expected $expected")
}

object MutualRecursionBenchmark {

  /** plain JVM mutual recursion: the control that must overflow */
  object Direct {
    def even(n: Int): Boolean = if n == 0 then true else odd(n - 1)
    def odd(n: Int): Boolean = if n == 0 then false else even(n - 1)
  }

  /** Cheney on the M.T.A.: recurse directly, and every `Limit` levels
   * throw away the stack with an exception that carries the next call.
   * Sound only because every call here is a tail call — the frames
   * thrown away have nothing left to do. */
  object Unwind {
    final class Resume(val next: () => Boolean) extends RuntimeException(null, null, false, false)
    final val Limit = 1000
    def even(n: Int, d: Int): Boolean =
      if n == 0 then true else if d >= Limit then throw Resume(() => odd(n - 1, 0)) else odd(n - 1, d + 1)
    def odd(n: Int, d: Int): Boolean =
      if n == 0 then false else if d >= Limit then throw Resume(() => even(n - 1, 0)) else even(n - 1, d + 1)
    def run(start: () => Boolean): Boolean =
      var next = start
      var answer = false
      var done = false
      while !done do
        try { answer = next(); done = true }
        catch case r: Resume => next = r.next
      answer
  }

  /** the stack as segments: every `Limit` levels the rest of the
   * recursion runs on a fresh virtual thread and this one parks for
   * its answer, so no single stack holds more than `Limit` frames and
   * the parked ones live on the heap. Unlike `Unwind` this keeps every
   * frame, so it works for non-tail recursion as well. */
  object Segments {
    final val Limit = 1000
    def even(n: Int, d: Int): Boolean =
      if n == 0 then true else if d >= Limit then hop(() => odd(n - 1, 0)) else odd(n - 1, d + 1)
    def odd(n: Int, d: Int): Boolean =
      if n == 0 then false else if d >= Limit then hop(() => even(n - 1, 0)) else even(n - 1, d + 1)
    private def hop(rest: () => Boolean): Boolean =
      val answer = new java.util.concurrent.CompletableFuture[Boolean]()
      Thread.startVirtualThread { () =>
        try answer.complete(rest()): Unit
        catch case e: Throwable => answer.completeExceptionally(e): Unit
      }: Unit
      answer.join()
  }

  /** the trampoline a Scala programmer writes by hand */
  sealed trait Bounce
  object Bounce {
    final case class Done(b: Boolean) extends Bounce
    final case class More(next: () => Bounce) extends Bounce
    def even(n: Int): Bounce = if n == 0 then Done(true) else More(() => odd(n - 1))
    def odd(n: Int): Bounce = if n == 0 then Done(false) else More(() => even(n - 1))
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
    def even(n: Int): TailRec[Boolean] = if n == 0 then done(true) else tailcall(odd(n - 1))
    def odd(n: Int): TailRec[Boolean] = if n == 0 then done(false) else tailcall(even(n - 1))
  }

  object CatsEval {
    import cats.Eval
    def even(n: Int): Eval[Boolean] = if n == 0 then Eval.True else Eval.defer(odd(n - 1))
    def odd(n: Int): Eval[Boolean] = if n == 0 then Eval.False else Eval.defer(even(n - 1))
  }

  object CatsIO {
    import cats.effect.IO
    def even(n: Int): IO[Boolean] = if n == 0 then IO.pure(true) else IO.defer(odd(n - 1))
    def odd(n: Int): IO[Boolean] = if n == 0 then IO.pure(false) else IO.defer(even(n - 1))
  }

  object Zio {
    import _root_.zio.*
    def even(n: Int): UIO[Boolean] = if n == 0 then ZIO.succeed(true) else ZIO.suspendSucceed(odd(n - 1))
    def odd(n: Int): UIO[Boolean] = if n == 0 then ZIO.succeed(false) else ZIO.suspendSucceed(even(n - 1))
  }

  object KyoIO {
    import _root_.kyo.*
    def even(n: Int): Boolean < IO = if n == 0 then true else IO(odd(n - 1))
    def odd(n: Int): Boolean < IO = if n == 0 then false else IO(even(n - 1))
  }

  object OkayFree {
    def even(n: Int): Boolean ! Pure = if n == 0 then pure(true) else !.tailcall(odd(n - 1))
    def odd(n: Int): Boolean ! Pure = if n == 0 then pure(false) else !.tailcall(even(n - 1))
  }

  object OkayCont {
    def even(n: Int): Boolean /> Boolean = if n == 0 then Cont.Pure(true) else Cont.delay(() => odd(n - 1))
    def odd(n: Int): Boolean /> Boolean = if n == 0 then Cont.Pure(false) else Cont.delay(() => even(n - 1))
  }
}
