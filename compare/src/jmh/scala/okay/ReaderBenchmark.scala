package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit


/**
 * The Reader effect across the ecosystem: a chain of N asks
 * (m.flatMap(_ => ask)), then run with the environment 42.
 * okay handles it with the tail-resumptive relay; cats is Kleisli
 * over Eval (Id would build an N-deep stack); ZIO reads its
 * environment; kyo uses Env; atnos-eff its ReaderEffect.
 *
 * SHAPE MATTERS (kyo-fair-lanes, 2026-09-02): foldLeft builds a
 * LEFT-nested chain, ((ask >>= f) >>= f) >>= f. kyo's `map` over a
 * suspension wraps it in a KyoContinue whose apply re-applies the
 * inner continuation, and its handle loop never reassociates — so
 * every resume walks the rest of the chain: O(N²), measured ×109
 * from N=1k to 10k. Real code (a for-comprehension, a direct block,
 * recursion) builds the RIGHT-nested chain, ask >>= (_ => ask >>=
 * ...), which kyo handles linearly. The `*Rec` lanes are that shape,
 * for both libraries; the foldLeft lanes stay as the quadratic-trap
 * measurement, and must not be quoted as kyo's price.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ReaderBenchmark {

  final val N = 10000

  @Benchmark
  def okayReader(): Int =
    val prog = (1 to N).foldLeft(Reader.ask[Int]): (m, _) =>
      m.flatMap(_ => Reader.ask[Int])
    !.run(Reader.run[Int, Int, Nothing](42)(prog))

  /** the same N asks, RIGHT-nested by recursion */
  @Benchmark
  def okayReaderRec(): Int =
    def go(i: Int): Int ! Reader % Int =
      if i == 0 then Reader.ask[Int] else Reader.ask[Int].flatMap(_ => go(i - 1))
    !.run(Reader.run[Int, Int, Nothing](42)(go(N)))

  /** the ctx-fn reader THROUGH THE INSTANCE — at N/10: each
   * flatMap is literally f(fb) and run is application (the compiler
   * is the interpreter, E13/E19), but application is a STACK frame
   * per bind: 10k left-nested binds overflow (measured). The
   * instance serves modest widths (traverse over a config, a page
   * of readers); DEPTH belongs to the row Reader, which trampolines
   * on Cont. The boundary is in capabilities.md. */
  @Benchmark
  def okayCtxMonad(): Int =
    val M = summon[Monad[[X] =>> Int ?=> X]]
    val ask: Int ?=> Int = wire[Int]
    // built by RECURSION: a var loop is a trap — the ctx-closure
    // inserted at `val prev: Int ?=> Int = prog` captures the VAR
    // by reference, making the chain self-referential (E22); and a
    // foldLeft lambda eagerly applies in lambda-result position
    def chain(n: Int): Int ?=> Int =
      if n == 0 then ask
      else M.flatMap(chain(n - 1))((_: Int) => (ask: Int ?=> Int))
    provide(42)(chain(N / 10))

  /** the ctx-fn reader in DIRECT STYLE: no program is built at all
   * — N reads of the ambient environment are N field reads; this is
   * the floor the docs claim */
  @Benchmark
  def okayCtxDirect(bh: org.openjdk.jmh.infra.Blackhole): Unit =
    def body: Int ?=> Unit =
      var i = 0
      while i < N do { bh.consume(wire[Int]); i += 1 }
    provide(42)(body)

  @Benchmark
  def catsKleisli(): Int =
    import cats.data.Kleisli
    val ask: Kleisli[cats.Eval, Int, Int] = Kleisli(e => cats.Eval.now(e))
    (1 to N).foldLeft(ask)((m, _) => m.flatMap(_ => ask)).run(42).value

  @Benchmark
  def zioService(): Int =
    import _root_.zio.*
    val ask = ZIO.service[Int]
    val z = (1 to N).foldLeft(ask)((m, _) => m.flatMap(_ => ask))
      .provideEnvironment(ZEnvironment(42))
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(z).getOrThrowFiberFailure())

  @Benchmark
  def kyoEnv(): Int =
    import _root_.kyo.*
    val k = (1 to N).foldLeft(Env.get[Int]: Int < Env[Int]): (m, _) =>
      m.flatMap((_: Int) => Env.get[Int])
    Env.run(42)(k).eval

  /** kyo Env in its natural, right-nested shape — linear */
  @Benchmark
  def kyoEnvRec(): Int =
    import _root_.kyo.*
    def go(i: Int): Int < Env[Int] =
      if i == 0 then Env.get[Int] else Env.get[Int].flatMap((_: Int) => go(i - 1))
    Env.run(42)(go(N)).eval

  @Benchmark
  def atnosReader(): Int =
    import org.atnos.eff.*
    import org.atnos.eff.all.*
    import org.atnos.eff.syntax.all.*
    type S = Fx.fx1[[X] =>> cats.data.Reader[Int, X]]
    val a = ask[S, Int]
    (1 to N).foldLeft(a)((m, _) => m.flatMap(_ => a)).runReader(42).run
}
