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
 *
 * Four forms, because the first ceiling lied by omission: `flat`
 * inlines the handlers' BODIES as well as flattening the dispatch,
 * and a macro over opaque `Handler` givens can only do the second.
 * `flatCalls` is the reachable ceiling; `inlined` is the macro
 * (handler-fusion-flat). Position 4, minima: union 108.4, inlined
 * 100.2, flatCalls 94.9, flat 88.2 µs — bytes identical on all.
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

  /** the same flat match, but CALLING the four handlers (captured once
   * as fields) instead of inlining their bodies — the ceiling the macro
   * can actually reach: `flat` above also inlines the handlers, which
   * no macro over opaque `Handler` givens can do */
  val flatCalls: Handler[Row] =
    val (ha, hb, hc, hd) = (summon[Handler[E1]], summon[Handler[E2]], summon[Handler[E3]], summon[Handler[E4]])
    new Handler[Row]:
      def handle[A](a: Row[A]): A = a match
        case e: E1[A] => ha.handle(e)
        case e: E2[A] => hb.handle(e)
        case e: E3[A] => hc.handle(e)
        case e: E4[A] => hd.handle(e)

  /** the shipped form: the macro's one expression over the row —
   * held to within 10% of `flatCalls` */
  val inlined: Handler[Row] = Handler.flat[Row]

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

  @Benchmark
  def flatCalls4(): Int = built4.runWith(using flatCalls)

  @Benchmark
  def flatCalls1(): Int = built1.runWith(using flatCalls)

  @Benchmark
  def inline4(): Int = built4.runWith(using inlined)

  @Benchmark
  def inline1(): Int = built1.runWith(using inlined)
}
