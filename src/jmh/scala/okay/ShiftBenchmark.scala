package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.Row.at

/**
 * specs/shift-effect.md: `Shift % R` in the core against Cont and Shift on the same shapes (the probe's
 * ShiftFxBenchmark, its (b) lanes). `seq`: N captures in sequence under one delimiter, each body `k(1)`
 * plus 0. `seqDF`: the same with Danvy-Filinski's `shift`. `twoShot`: 100 delimiters, each
 * `shift(k => k(1) + k(10)) * 2`.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ShiftBenchmark {

  final val N = 1000
  type P = okay.Pure

  def seq0(n: Int): Int ! Shift % Int =
    if n == 0 then pure(0)
    else shift0[Int, Int, P](k => k(1).map(_ + 0)).flatMap(x => !.tailcall(seq0(n - 1)).map(_ + x))

  def seqDF(n: Int): Int ! Shift % Int =
    if n == 0 then pure(0)
    else shift[Int, Int, P](k => k(1).map(_ + 0)).flatMap(x => !.tailcall(seqDF(n - 1)).map(_ + x))

  def contSeq(n: Int): Cont[Int, Int, Int] =
    if n == 0 then Cont.Pure(0)
    else Cont.shift[Int, Int, Int](k => k(1) + 0).flatMap(x => Cont.delay(() => contSeq(n - 1)).map(_ + x))

  def delimSeq(p: Prompt[Int])(n: Int): Int ! Shift % ? + P =
    if n == 0 then pure(0)
    else Shift.shift0[Int, Int, P](p)(k => k(1).map(_ + 0)).flatMap(x => !.tailcall(delimSeq(p)(n - 1)).map(_ + x))

  def two: Int ! P =
    reset[Int, P](shift0[Int, Int, P](k => for a <- k(1); b <- k(10) yield a + b).map(_ * 2))

  def delimTwo: Int ! P =
    Shift.reset[Int, P](p => Shift.shift0[Int, Int, P](p)(k => for a <- k(1); b <- k(10) yield a + b).map(_ * 2))

  @Benchmark def shift0_seq(): Int = !.run(reset[Int, P](seq0(N)))
  @Benchmark def shift_seqDF(): Int = !.run(reset[Int, P](seqDF(N)))
  @Benchmark def cont_seq(): Int = Cont.reset(contSeq(N))
  @Benchmark def delim_seq(): Int = !.run(Shift.reset[Int, P](p => delimSeq(p)(N)))

  // resource-abort-releases: an abort through a `try` frame (the piece walked for a finalizer, none found) and one
  // through a `Resource` scope (the piece discontinued, the scope released) — 100 resets each
  given Failing[Shift % ? + P] = new Failing[Shift % ? + P]:
    def guard[X](e: (Shift % ? + P)[X], onFailure: () => Unit): (Shift % ? + P)[X] = e
  def abortTry(p: Prompt[Int]): Int ! Shift % ? + P =
    summon[CanTry[[X] =>> X ! Shift % ? + P]].tryIn(Shift.abort[Int, Int, P](p)(1))(_ => pure(0))
  def abortScope(p: Prompt[Int]): Int ! Shift % ? + P =
    Resource.run[Int, Shift % ? + P](Resource.acquire(1)(_ => ()).at[Resource + Shift % ? + P].flatMap(_ =>
      Shift.abort[Int, Int, P](p)(1).at[Resource + Shift % ? + P]))
  @Benchmark def abort_try(): Int = { var s = 0; var i = 0; while i < 100 do { s += !.run(Shift.reset[Int, P](abortTry)); i += 1 }; s }
  @Benchmark def abort_scope(): Int = { var s = 0; var i = 0; while i < 100 do { s += !.run(Shift.reset[Int, P](abortScope)); i += 1 }; s }

  @Benchmark def shift0_twoShot(): Int = { var s = 0; var i = 0; while i < 100 do { s += !.run(two); i += 1 }; s }
  @Benchmark def cont_twoShot(): Int = { var s = 0; var i = 0; while i < 100 do { s += Cont.reset(Cont.shift[Int, Int, Int](k => k(1) + k(10)).map(_ * 2)); i += 1 }; s }
  @Benchmark def delim_twoShot(): Int = { var s = 0; var i = 0; while i < 100 do { s += !.run(delimTwo); i += 1 }; s }
}
