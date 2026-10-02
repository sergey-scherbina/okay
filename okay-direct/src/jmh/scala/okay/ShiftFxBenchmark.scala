package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * specs/shift-effect.md: one shape on four implementations. `seq`: N
 * captures in sequence under one delimiter, each body `k(1)` plus 0 (an
 * answer-using body, so `Cont`'s macro cannot take it as a value).
 * `twoShot`: 100 delimiters, each `shift(k => k(1) + k(10)) * 2`.
 * `fx*` is `Shift % R` (test sources: ShiftFx), (a) a handler,
 * (b) Delim's machine; `cont*` today's `Cont`; `delim*` today's `Delim`.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ShiftFxBenchmark {

  final val N = 1000
  type P = okay.Pure

  def fxSeq(api: ShiftApi)(n: Int): Int ! Shift % Int =
    if n == 0 then pure(0)
    else api.shift[Int, Int, P](k => k(1).map(_ + 0)).flatMap(x => !.tailcall(fxSeq(api)(n - 1)).map(_ + x))

  def contSeq(n: Int): Cont[Int, Int, Int] =
    if n == 0 then Cont.Pure(0)
    else okay.shift[Int, Int, Int](k => k(1) + 0).flatMap(x => Cont.delay(() => contSeq(n - 1)).map(_ + x))

  def delimSeq(p: Prompt[Int])(n: Int): Int ! Delim + P =
    if n == 0 then pure(0)
    else Delim.shift0[Int, Int, P](p)(k => k(1).map(_ + 0)).flatMap(x => !.tailcall(delimSeq(p)(n - 1)).map(_ + x))

  def fxTwo(api: ShiftApi): Int ! P =
    api.reset[Int, P](api.shift[Int, Int, P](k => for a <- k(1); b <- k(10) yield a + b).map(_ * 2))

  def delimTwo: Int ! P =
    Delim.reset[Int, P](p => Delim.shift0[Int, Int, P](p)(k => for a <- k(1); b <- k(10) yield a + b).map(_ * 2))

  @Benchmark def fxHandled_seq(): Int = !.run(ShiftFx.Handled.reset[Int, P](fxSeq(ShiftFx.Handled)(N)))
  @Benchmark def fxDelim_seq(): Int = !.run(ShiftFx.OnDelim.reset[Int, P](fxSeq(ShiftFx.OnDelim)(N)))
  @Benchmark def cont_seq(): Int = okay.reset(contSeq(N))
  @Benchmark def delim_seq(): Int = !.run(Delim.reset[Int, P](p => delimSeq(p)(N)))

  @Benchmark def fxHandled_twoShot(): Int = { var s = 0; var i = 0; while i < 100 do { s += !.run(fxTwo(ShiftFx.Handled)); i += 1 }; s }
  @Benchmark def fxDelim_twoShot(): Int = { var s = 0; var i = 0; while i < 100 do { s += !.run(fxTwo(ShiftFx.OnDelim)); i += 1 }; s }
  @Benchmark def cont_twoShot(): Int = { var s = 0; var i = 0; while i < 100 do { s += okay.reset(okay.shift[Int, Int, Int](k => k(1) + k(10)).map(_ * 2)); i += 1 }; s }
  @Benchmark def delim_twoShot(): Int = { var s = 0; var i = 0; while i < 100 do { s += !.run(delimTwo); i += 1 }; s }
}
