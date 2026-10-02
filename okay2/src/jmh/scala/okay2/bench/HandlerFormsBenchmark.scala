package okay2.bench

import org.openjdk.jmh.annotations.{State => JmhState, _}
import java.util.concurrent.TimeUnit

import okay2._

/**
 * okay2-handler-control-resume: the `control` form against the loops it stands beside, the Scala 3 core's
 * HandlerFormsBenchmark `maybe` lanes in shape (N operations that each resume once, in tail position), on
 * Reader since okay2 has no Maybe.
 *
 * - `reader_builtin`: `Reader(7)`, the bespoke loop
 * - `reader_handle`: `!.handle` with an `Interpr` answering `Cont.Pure` — no capture, the floor for `control`
 * - `reader_shift`: `!.handle` with an `Interpr` answering `Cont.shift(k => k(7))` — what `control` was before
 *   the tail resume, a capture per operation
 * - `reader_control`: `Handler[Reader[Int]].control`, its clause `k(7)`
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class HandlerFormsBenchmark {

  final val N = 1000

  def asks(n: Int): Int ! Reader[Int] =
    if (n == 0) pure[Reader[Int], Int](0) else Reader.ask[Int].flatMap(r => !.tailcall(asks(n - 1)).map(_ + r))

  private val pureAnswer: Interpr[Reader[Int], Int ! Pure] = new Interpr[Reader[Int], Int ! Pure] {
    def apply[X](e: Reader.Op[Int, X]): Cont[X, Int ! Pure, Int ! Pure] = Cont.Pure[X, Int ! Pure](answer[X](7))
  }

  private val shiftAnswer: Interpr[Reader[Int], Int ! Pure] = new Interpr[Reader[Int], Int ! Pure] {
    def apply[X](e: Reader.Op[Int, X]): Cont[X, Int ! Pure, Int ! Pure] =
      Cont.shift[X, Int ! Pure, Int ! Pure](k => k(answer[X](7)))
  }

  private val controlForm = Handler[Reader[Int]].control[Handler.Id](new Handler.Ret[Handler.Id] { def apply[A](a: A): A = a })(
    new Handler.Control[Reader[Int], Handler.Id] {
      def apply[X, A, G <: Row](e: Reader.Op[Int, X], k: X => A ! G): A ! G = k(answer[X](7))
    })

  // Reader's one operation answers `Int`; scalac 2 does not refine `X` from the match, so the answer is asserted
  private def answer[X](x: Any): X = x.asInstanceOf[X]

  @Benchmark def reader_builtin(): Int = asks(N).handle(Reader(7)).run
  @Benchmark def reader_handle(): Int = !.run(!.handle[Reader[Int], Pure](asks(N))(a => pure[Pure, Int](a))(pureAnswer))
  @Benchmark def reader_shift(): Int = !.run(!.handle[Reader[Int], Pure](asks(N))(a => pure[Pure, Int](a))(shiftAnswer))
  @Benchmark def reader_control(): Int = asks(N).handle(controlForm).run
}
