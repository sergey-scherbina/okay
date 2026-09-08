package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.Direct.*
import okay.AtMacro.{at as atM, given}

/**
 * What does attaching a row COST?
 *
 * Three spellings put an operation into a composite row:
 *
 *   effect[R, A](op)        constructs the node AT R — one Inject, the
 *                           floor, and what `direct` emits too
 *   State.get[Int].at[R]    builds the node at the narrow row and
 *                           moves it — a second Inject, the first
 *                           immediately garbage
 *   State.get[Int] + widen  today's spelling, same shape as `.at` but
 *                           naming the complement
 *
 * The question is whether that second Inject survives to cost
 * anything. It never escapes: it is constructed and destructured
 * within one inlined region, which is the textbook case for HotSpot's
 * escape analysis to scalarise it away. If the bytes agree, the
 * ergonomic spelling is free and the "tree rebuild" objection is
 * theoretical; if they do not, the operation-level spelling is the
 * one to ship.
 *
 * Read B/op (-prof gc) before us/op: allocation does not drift with
 * host load the way time does, and this box is not quiet.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class RowLiftBenchmark {

  final val N = 1000

  type R = State % Int + Writer % String

  // ---- the evidence, minimal (self / left / deeper), as probed
  trait In[F[+_], G[+_]]:
    def inj[A](fa: F[A]): G[A]

  // EVERY instance's inj is identity — the injection is a type-level
  // fact, not a runtime one — so one cached object serves them all.
  // A parameterised `given` is a METHOD and allocates a fresh witness
  // at every use site: that is the +16 B/op the first run charged to
  // both liftAt and at.
  private object IdIn extends In[[A] =>> Any, [A] =>> Any]:
    def inj[A](fa: Any): Any = fa

  trait InLow:
    given self[F[+_]]: In[F, F] = IdIn.asInstanceOf[In[F, F]]

  object In extends InLow:
    given left[F[+_], G[+_]]: In[F, F + G] = IdIn.asInstanceOf[In[F, F + G]]
    given deeper[F[+_], G[+_], H[+_]](using i: In[F, G]): In[F, G + H] =
      IdIn.asInstanceOf[In[F, G + H]]

  /** on the OPERATION: constructs at R, nothing to rebuild */
  extension [F[+_], A](fa: F[A])
    inline def liftAt[G[+_]](using i: In[F, G]): A ! G = effect[G, A](i.inj(fa))

  /** on the PROGRAM: the ergonomic spelling, one extra node per op */
  // NOT inline: it recurses over the tree, and an inline recursion
  // does not terminate at compile time (found by trying)
  extension [A, F[+_]](p: A ! F)
    def at[G[+_]](using i: In[F, G]): A ! G =
      import okay.!.*
      (p.resume: @unchecked) match
        case Pure(a) => Free.Pure(a)
        case Effect(e) => Free.inject(i.inj(e))
        case Bind(Effect(e), k) => Free.inject(i.inj(e)).flatMap(x => k(x).at[G])

  /** the floor: construct at R directly */
  @Benchmark
  def viaEffect(): Int =
    var m: Int ! R = effect[R, Int](State.Get())
    var i = 1
    while i < N do
      m = m.flatMap(_ => effect[R, Int](State.Get()))
      i += 1
    State.run[Int, Int](0)(Writer.run[String, Int, State % Int](m).map(_._2))._2

  /** the operation-level lift */
  @Benchmark
  def viaLiftAt(): Int =
    var m: Int ! R = (State.Get[Int, Int](): State[Int, Int]).liftAt[R]
    var i = 1
    while i < N do
      m = m.flatMap(_ => (State.Get[Int, Int](): State[Int, Int]).liftAt[R])
      i += 1
    State.run[Int, Int](0)(Writer.run[String, Int, State % Int](m).map(_._2))._2

  /** the program-level lift — the ergonomic one under test */
  @Benchmark
  def viaAt(): Int =
    var m: Int ! R = State.get[Int].at[R]
    var i = 1
    while i < N do
      m = m.flatMap(_ => State.get[Int].at[R])
      i += 1
    State.run[Int, Int](0)(Writer.run[String, Int, State % Int](m).map(_._2))._2

  /** today's spelling: widen, naming the complement */
  @Benchmark
  def viaWiden(): Int =
    var m: Int ! R = !.widen[Int, State % Int, Writer % String](State.get[Int])
    var i = 1
    while i < N do
      m = m.flatMap(_ =>
        !.widen[Int, State % Int, Writer % String](State.get[Int]))
      i += 1
    State.run[Int, Int](0)(Writer.run[String, Int, State % Int](m).map(_._2))._2

  /** direct style: the macro reads the row off the block's expected
   * type and emits Free.Inject AT it — no widen, no evidence, no
   * intermediate node. The candidate for "zero cost AND ergonomic". */
  @Benchmark
  def viaDirect(): Int =
    var m: Int ! R = direct { State.Get[Int, Int]().!? }
    var i = 1
    while i < N do
      m = m.flatMap(_ => direct { State.Get[Int, Int]().!? })
      i += 1
    State.run[Int, Int](0)(Writer.run[String, Int, State % Int](m).map(_._2))._2

  /** the macro `.at`: same postfix syntax, but the operation is put
   * into R's Inject at COMPILE time — no intermediate node, no walk.
   * Falls back to the tree walk for a receiver it cannot see through. */
  @Benchmark
  def viaAtMacro(): Int =
    var m: Int ! R = State.get[Int].atM[R]
    var i = 1
    while i < N do
      m = m.flatMap(_ => State.get[Int].atM[R])
      i += 1
    State.run[Int, Int](0)(Writer.run[String, Int, State % Int](m).map(_._2))._2
}
