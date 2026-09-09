package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import scala.annotation.tailrec
import okay.!.{Effect, Bind, resume}
import okay.RowLift.at

/**
 * either-scalarised (runner-floor item 3): which wrapper does WHICH
 * runner actually pay for?
 *
 * Stage A replaced `<|>` (an Either per operation) and the extractor
 * (an Option per test) with `split` in the shipping runners, and the
 * two nestings of the same program saved different amounts: 10.7 KB
 * where 53 KB were expected, 26.7 where 43 were. So the JIT was
 * already scalar-replacing some of those wrappers, per runner, and
 * this file measures which: each runner alone, over a program with
 * only its own operations, in three forms — the shipping loop
 * (`split`), a benchmark-local copy on `<|>` (Either, no Option), and
 * a copy on the extractor (Either AND Option, the pre-stage-A shape).
 * Read B/op (-prof gc); the differences are per-operation wrappers or
 * they are nothing.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class SplitBenchmark {

  final val N = 1000
  type WR = Writer % String + Produce
  type SR = State % Int + Produce

  var wp: Int ! WR = scala.compiletime.uninitialized
  var sp: Int ! SR = scala.compiletime.uninitialized

  def writerProg(i: Int, acc: Int): Int ! WR =
    if i >= N then pure(acc) else Writer.tell("w").at[WR].flatMap(_ => writerProg(i + 1, acc + 1))
  def stateProg(i: Int, acc: Int): Int ! SR =
    if i >= N then pure(acc)
    else if i % 2 == 0 then State.get[Int].at[SR].flatMap(x => stateProg(i + 1, acc + x))
    else State.set[Int](i).at[SR].flatMap(x => stateProg(i + 1, acc + x))

  @Setup
  def up(): Unit =
    mixed = mixedProg(0, 0)
    wp = writerProg(0, 0)
    sp = stateProg(0, 0)

  // ---- Writer: the shipping loop, and the two older shapes

  @Benchmark
  def writerShip(): Int = Writer.run[String, Int, Produce](wp).runWith._2

  /** the pre-stage-A loop on `<|>`: an Either per operation, no Option
   * (`<|>` itself lost the extractor in stage A) */
  @Benchmark
  def writerEither(): Int =
    @tailrec def loop[A](s: Vector[String])(x: A ! WR): (Vector[String], A) = (x.resume: @unchecked) match
      case Free.Pure(a) => (s, a)
      case Effect(e) => <|>[Writer % String, Produce](e) match
        case Left(Writer.Say(v)) => (s :+ v, ())
        case Right(_) => throw new IllegalStateException("no Produce is performed here")
      case Bind(Effect(e), k) => <|>[Writer % String, Produce](e) match
        case Left(Writer.Say(v)) => loop(s :+ v)(k(()))
        case Right(_) => throw new IllegalStateException("no Produce is performed here")
    loop(Vector.empty)(wp)._2

  /** the pre-stage-A KERNEL too: the extractor (an Option per test)
   * feeding an Either — exactly what `<|>` was before stage A. The
   * cast is the old kernel's, copied for the measurement. */
  @Benchmark
  def writerExtract(): Int =
    val T = summon[TypeableK[Writer % String]]
    def old[A](e: (Writer % String)[A] | Produce[A]): Either[(Writer % String)[A], Produce[A]] = e match
      // the extractor's Option, called as a method so the shape is the
      // pre-stage-A kernel's and nothing else
      case x => T.unapply[A](x) match
        case Some(w) => Left(w)
        case None => Right(x.asInstanceOf[Produce[A]])
    @tailrec def loop[A](s: Vector[String])(x: A ! WR): (Vector[String], A) = (x.resume: @unchecked) match
      case Free.Pure(a) => (s, a)
      case Effect(e) => old(e) match
        case Left(Writer.Say(v)) => (s :+ v, ())
        case Right(_) => throw new IllegalStateException("no Produce is performed here")
      case Bind(Effect(e), k) => old(e) match
        case Left(Writer.Say(v)) => loop(s :+ v)(k(()))
        case Right(_) => throw new IllegalStateException("no Produce is performed here")
    loop(Vector.empty)(wp)._2

  /** the OLD accumulator, a Vector appended per tell, through the
   * default `Fold[A, Seq[A]]` — same run as `writerShip`, so the two
   * accumulators are an A/B at 1 000 tells */
  @Benchmark
  def writerShipVec(): Int = Writer.fold[String, Seq[String], Int, Produce](wp).runWith._2

  // ---- the MIXED shape at 333 tells (FusionBenchmark's program), both accumulators

  type SW = State % Int + Writer % String
  var mixed: Int ! SW = scala.compiletime.uninitialized
  def mixedProg(i: Int, acc: Int): Int ! SW =
    if i >= N then pure(acc)
    else (i % 3) match
      case 0 => State.get[Int].at[SW].flatMap(x => mixedProg(i + 1, acc + x))
      case 1 => State.set[Int](i).at[SW].flatMap(x => mixedProg(i + 1, acc + x))
      case _ => Writer.tell("w").at[SW].flatMap(_ => mixedProg(i + 1, acc + 1))

  @Benchmark
  def mixedList(): Int = State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, State % Int](mixed))._2._2

  @Benchmark
  def mixedVec(): Int = State.run[Int, (Seq[String], Int)](0)(Writer.fold[String, Seq[String], Int, State % Int](mixed))._2._2

  /** the List road WITHOUT the final reverse+map: is the loss the reverse? */
  @Benchmark
  def mixedListNoRev(): Int =
    State.run[Int, (List[String], Int)](0)(Writer.foldWith[String, List[String], Int, State % Int](mixed)(Nil)((s, w) => w :: s))._2._2

  /** Vector through `foldWith` with the step inline (no `Fold` virtual call): is the difference `K.add`? */
  @Benchmark
  def mixedVecInline(): Int =
    State.run[Int, (Vector[String], Int)](0)(Writer.foldWith[String, Vector[String], Int, State % Int](mixed)(Vector.empty)((s, w) => s :+ w))._2._2

  // ---- single-shot-row: the PRICE of a mutable accumulator, before
  // any evidence type exists. A benchmark-local Writer runner that
  // appends into a ListBuffer cell instead of threading a List and
  // reversing it — sound ONLY if no continuation it hands out is ever
  // resumed twice, which nothing here checks: this is the probe the
  // spec's gate asks for, not a runner anyone may call.

  private def writerMutLoop[A, F[+_]](a: A ! Writer % String + F)(using TypeableK[Writer % String]): (Seq[String], A) ! F =
    val buf = scala.collection.mutable.ListBuffer.empty[String]
    def _loop(x: A ! Writer % String + F): (Seq[String], A) ! F = loop(x)
    @tailrec def loop(x: A ! Writer % String + F): (Seq[String], A) ! F = (x.resume: @unchecked) match
      case Free.Pure(a) => Free.Pure((buf.toList, a))
      case Effect(e) => split[Writer % String, F](e) {
          case Writer.Say(v) => buf += v; Free.Pure((buf.toList, ())): (Seq[String], A) ! F
        } { e => Effect(e).map(x => (buf.toList, x)) }
      case Bind(Effect(e), k) => split[Writer % String, F](e) { w0 =>
          (w0: @unchecked) match
            case Writer.Say(v) => buf += v; loop(k(()))
        } { e => Effect(e).flatMap(x => _loop(k(x))) }
    loop(a)

  @Benchmark
  def writerMut(): Int = writerMutLoop[Int, Produce](wp).runWith._2

  @Benchmark
  def mixedMut(): Int = State.run[Int, (Seq[String], Int)](0)(writerMutLoop[Int, State % Int](mixed))._2._2

  // ---- State: the same three

  @Benchmark
  def stateShip(): Int = State.handle[Int, Int, Produce](0)(sp).runWith._2

  @Benchmark
  def stateEither(): Int =
    @tailrec def loop[A](s: Int)(x: A ! SR): (Int, A) = (x.resume: @unchecked) match
      case Free.Pure(a) => (s, a)
      case Effect(e) => <|>[State % Int, Produce](e) match
        case Left(State.Get()) => (s, s)
        case Left(State.Set(s2)) => (s2, s2)
        case Right(_) => throw new IllegalStateException("no Produce is performed here")
      case Bind(Effect(e), k) => <|>[State % Int, Produce](e) match
        case Left(State.Get()) => loop(s)(k(s))
        case Left(State.Set(s2)) => loop(s2)(k(s2))
        case Right(_) => throw new IllegalStateException("no Produce is performed here")
    loop(0)(sp)._2

  @Benchmark
  def stateExtract(): Int =
    val T = summon[TypeableK[State % Int]]
    def old[A](e: (State % Int)[A] | Produce[A]): Either[(State % Int)[A], Produce[A]] = e match
      // the extractor's Option, called as a method so the shape is the
      // pre-stage-A kernel's and nothing else
      case x => T.unapply[A](x) match
        case Some(w) => Left(w)
        case None => Right(x.asInstanceOf[Produce[A]])
    @tailrec def loop[A](s: Int)(x: A ! SR): (Int, A) = (x.resume: @unchecked) match
      case Free.Pure(a) => (s, a)
      case Effect(e) => old(e) match
        case Left(State.Get()) => (s, s)
        case Left(State.Set(s2)) => (s2, s2)
        case Right(_) => throw new IllegalStateException("no Produce is performed here")
      case Bind(Effect(e), k) => old(e) match
        case Left(State.Get()) => loop(s)(k(s))
        case Left(State.Set(s2)) => loop(s2)(k(s2))
        case Right(_) => throw new IllegalStateException("no Produce is performed here")
    loop(0)(sp)._2
}
