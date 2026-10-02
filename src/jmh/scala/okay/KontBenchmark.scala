package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import okay.Freer.Return

/**
 * The frame machine against the Shift machine and against the rotation
 * (specs/freer-kont.md, the last box). The Shift shapes are
 * DelimBenchmark's own, one to one — `delimGenerator`, `delimPushOnly`,
 * `delimDollarOnly`, `delimDollarResume` — written on `Cont0` and run by
 * `Delimited.runHead`; the two "nested" pairs are pure programs (no
 * delimiter, no capture) run by the machine and by today's
 * `Freer.resume`, which is what code that uses no continuations pays.
 * Same N, same annotations, so a lane here and its twin there are read
 * side by side.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class KontBenchmark {

  val N = 1000

  /** no other effect: the row is `Cont0` alone */
  type Nil[S, R, +X] = Nothing
  type P[S, R, A] = Freer[Cont0.Row[Nil], S, R, A]

  def pure[S, A](a: A): P[S, S, A] = Return(a)

  def run[S, A](p: P[S, S, A]): A = (Delimited.machine[Nil].runHead[S, S, A](p): @unchecked) match
    case Return(a) => a

  /** today's interpreter of a pure program: `resume` alone reaches the value */
  def runResume[S, A](p: P[S, S, A]): A = (p.resume: @unchecked) match
    case Return(a) => a

  // ---- DelimBenchmark.delimGenerator: a shift per emitted value

  type L = List[Int]

  def emit(p: Prompt[L])(a: Int): P[L, L, Unit] =
    Delimited.machine[Nil].shift[L, L, L, L, Unit](Cont0.delimiter(p))(k => k(()).map(a :: _))

  @Benchmark
  def kontGenerator(): Int =
    val p = Cont0.prompt[L]
    def go(i: Int): P[L, L, Unit] =
      if i >= N then pure(())
      else emit(p)(i).flatMap(_ => go(i + 1))
    run(Delimited.machine[Nil].reset[L, L, L](Cont0.delimiter(p))(go(0).map(_ => Nil))).length

  // ---- DelimBenchmark.delimPushOnly: N delimiters, nothing captured

  @Benchmark
  def kontResetOnly(): Int =
    def go(i: Int): P[Int, Int, Int] =
      if i >= N then pure(i)
      else Delimited.machine[Nil].reset[Int, Int, Int](Cont0.delimiter(Cont0.prompt[Int]))(pure(i)).flatMap(_ => go(i + 1))
    run(go(0))

  // ---- DelimBenchmark.delimDollarOnly: N `$` with a return function

  @Benchmark
  def kontDollarOnly(): Int =
    def go(i: Int): P[Int, Int, Int] =
      if i >= N then pure(i)
      else Delimited.machine[Nil].dollar[Int, Int, Int, Int](Cont0.delimiter(Cont0.prompt[Int]))(x => pure(x + 1))(pure(i)).flatMap(_ => go(i + 1))
    run(go(0))

  // ---- DelimBenchmark.delimDollarResume: one shift0 per `$`, resumed once

  @Benchmark
  def kontDollarResume(): Int =
    def go(i: Int): P[Int, Int, Int] =
      if i >= N then pure(i)
      else
        val p = Cont0.prompt[Int]
        Delimited.machine[Nil].dollar[Int, Int, Int, Int](Cont0.delimiter(p))(x => pure(x + 1))(
          Delimited.machine[Nil].shift0[Int, Int, Int, Int, Int](Cont0.delimiter(p))(k => k(i))).flatMap(_ => go(i + 1))
    run(go(0))

  // ---- no continuations at all: what the descent costs against the rotation

  /** `(((pure >>= f) >>= f) >>= f)…`: the shape the rotation re-associates */
  def leftNested: P[Int, Int, Int] =
    var p: P[Int, Int, Int] = pure(0)
    var i = 0
    while i < N do
      p = p.flatMap(x => pure(x + 1))
      i += 1
    p

  @Benchmark
  def leftNestedMachine(): Int = run(leftNested)

  @Benchmark
  def leftNestedResume(): Int = runResume(leftNested)

  /** `pure >>= (_ => pure >>= (_ => …))`: the shape every handler loop walks */
  def rightNested: P[Int, Int, Int] =
    def go(i: Int): P[Int, Int, Int] =
      if i >= N then pure(i) else pure(i).flatMap(_ => go(i + 1))
    go(0)

  @Benchmark
  def rightNestedMachine(): Int = run(rightNested)

  @Benchmark
  def rightNestedResume(): Int = runResume(rightNested)
}
