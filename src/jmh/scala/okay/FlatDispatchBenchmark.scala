package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

import !.*

case class E1[+A](a: A) derives Effect
case class E2[+A](a: A) derives Effect
case class E3[+A](a: A) derives Effect
case class E4[+A](a: A) derives Effect

/**
 * specs/handler-fusion.md, the `Handler.flat` box — its CEILING,
 * measured before anything is built (staged-block-lanes, 2026-09-22).
 *
 * `Handler.union` is a nested chain: on `E1 + (E2 + (E3 + E4))` an E4
 * operation passes three `split` tests and three nested `handle` calls
 * before its own handler sees it. `Handler.flat` would assemble the
 * same row inline as ONE match over the four operation classes. The
 * hand-written match below is what that unrolls to, so the difference
 * between the two arms is the whole prize of building it. Both arms
 * run the same prebuilt right-nested tree (no rotation, so the walk
 * is at its cheapest and dispatch is the largest share it can be);
 * position 4 is the number, position 1 the control that the flat
 * form is not slower where the chain answers first.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class FlatDispatchBenchmark {

  final val N = 10000

  type Row = E1 + (E2 + (E3 + E4))

  given Handler[E1] = new Handler[E1]:
    def handle[A](a: E1[A]): A = a.a
  given Handler[E2] = new Handler[E2]:
    def handle[A](a: E2[A]): A = a.a
  given Handler[E3] = new Handler[E3]:
    def handle[A](a: E3[A]): A = a.a
  given Handler[E4] = new Handler[E4]:
    def handle[A](a: E4[A]): A = a.a

  /** the shipping composition: the README's shape, nested in row order */
  val union: Handler[Row] =
    given h34: Handler[E3 + E4] = Handler.union[E3, E4]
    given h234: Handler[E2 + (E3 + E4)] = Handler.union[E2, E3 + E4]
    Handler.union[E1, E2 + (E3 + E4)]

  /** what `Handler.flat` would unroll to: one match, row order, no
   * nested handler call — position 4 pays four class tests and nothing
   * else */
  val flat: Handler[Row] = new Handler[Row]:
    def handle[A](a: Row[A]): A = a match
      case E1(x) => x
      case E2(x) => x
      case E3(x) => x
      case E4(x) => x

  def prog4(i: Int, acc: Int): Int ! Row =
    if i >= N then pure(acc) else effect[Row, Int](E4(i)).flatMap(x => prog4(i + 1, acc + x))

  def prog1(i: Int, acc: Int): Int ! Row =
    if i >= N then pure(acc) else effect[Row, Int](E1(i)).flatMap(x => prog1(i + 1, acc + x))

  var built4: Int ! Row = scala.compiletime.uninitialized
  var built1: Int ! Row = scala.compiletime.uninitialized

  @Setup
  def up(): Unit =
    built4 = prog4(0, 0)
    built1 = prog1(0, 0)

  @Benchmark
  def union4(): Int = built4.runWith(using union)

  @Benchmark
  def flat4(): Int = built4.runWith(using flat)

  @Benchmark
  def union1(): Int = built1.runWith(using union)

  @Benchmark
  def flat1(): Int = built1.runWith(using flat)
}
