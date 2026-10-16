package okay.std.bench

import okay.{%, +, pure}
import okay.std.{State, Writer}
import okay.one.Cont
import okay.one.row.{+:, In, Pure, Row}
import okay.one.handler.Stateful
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{State as JmhState, *}

/**
 * PROBE (keyed-root): the cost of the root's program value against the kernel's tree with no layer above it —
 * BuildShapeBenchmark.rowOneBind's program (N steps, one bind a step: a get, a set, a tell in turn) written three
 * ways: `root` the platform's `A ! Es` (a `Run` a node, fitted to paths); `keyed` the kernel's `Cont` at no row, each
 * operation at a place BY KEY (the effect's class), each handler saying the key is its own (`Handler.at`); `pathed`
 * the kernel's `Cont` at the row named, the paths written (the floor: no layer, no key).
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class KeyedProbeBenchmark {
  val N = 1000
  type SW = State % Int + Writer % String

  // ---- root
  private def oneBind(i: Int, acc: Int): okay.![Int, SW] =
    if i >= N then pure(acc)
    else (i % 3) match
      case 0 => State.get[Int].at[SW].flatMap(x => oneBind(i + 1, acc + x))
      case 1 => State.set[Int](i).at[SW].flatMap(x => oneBind(i + 1, acc + x))
      case _ => Writer.tell("w").at[SW].flatMap(_ => oneBind(i + 1, acc + 1))

  @Benchmark
  def root(): Int = State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, SW](oneBind(0, 0)))._2._2

  // ---- keyed: the kernel's tree, places by key
  private val stateAt: In.At[State % Int, Pure] = In.At(classOf[State[?, ?]])
  private val writerAt: In.At[Writer % String, Pure] = In.At(classOf[Writer[?, ?]])

  private final class KState[T <: Row, A](s0: Int) extends Stateful[State % Int, T, Int, A, (Int, A)]:
    private var nextS: Int = s0
    def initial: Int = s0
    def ret(s: Int, a: A) = Cont.pure((s, a))
    def value[X](s: Int, op: State[Int, X]): X = op match
      case _: State.Get[s, ?] => nextS = s; s.asInstanceOf[X]
      case u: State.Update[s, b] => val (b, n) = u.f(s); nextS = n; b.asInstanceOf[X]
    def next[X](s: Int, op: State[Int, X]): Int = nextS
    override def at[F[_], R <: Row](in: In.At[F, R]): In.Same[State % Int, F] =
      if in.ref eq stateAt.ref then In.Same.Refl[State % Int]().asInstanceOf[In.Same[State % Int, F]] else null

  private final class KWriter[T <: Row, A] extends Stateful[Writer % String, T, List[String], A, (Seq[String], A)]:
    def initial: List[String] = Nil
    def ret(st: List[String], a: A) = Cont.pure((st.reverse, a))
    def value[X](st: List[String], op: Writer[String, X]): X = op match { case Writer.Say(_) => () }
    def next[X](st: List[String], op: Writer[String, X]): List[String] = op match { case Writer.Say(w) => w :: st }
    override def at[F[_], R <: Row](in: In.At[F, R]): In.Same[Writer % String, F] =
      if in.ref eq writerAt.ref then In.Same.Refl[Writer % String]().asInstanceOf[In.Same[Writer % String, F]] else null

  private def kOneBind(i: Int, acc: Int): okay.one.![Int, Pure] =
    if i >= N then Cont.pure(acc)
    else (i % 3) match
      case 0 => Cont.perform[State % Int, Pure, Int](State.Get[Int, Int]())(using stateAt).flatMap(x => kOneBind(i + 1, acc + x))
      case 1 => Cont.perform[State % Int, Pure, Int](State.Update[Int, Int](s => (s, i)))(using stateAt).flatMap(x => kOneBind(i + 1, acc + x))
      case _ => Cont.perform[Writer % String, Pure, Unit](Writer.Say("w"))(using writerAt).flatMap(_ => kOneBind(i + 1, acc + 1))

  @Benchmark
  def keyed(): Int =
    val w = Cont.handle(kOneBind(0, 0))(KWriter[Pure, Int]())(using okay.one.row.Removed.Absent[Writer % String, Pure]())
    val s = Cont.handle(w)(KState[Pure, (Seq[String], Int)](0))(using okay.one.row.Removed.Absent[State % Int, Pure]())
    s.run._2._2

  // ---- pathed: the kernel's tree at the row named
  type R2 = Writer % String +: State % Int +: Pure
  private val sIn: In[State % Int, R2] = In.There(In.Here())
  private val wIn: In[Writer % String, R2] = In.Here()

  private def pOneBind(i: Int, acc: Int): okay.one.![Int, R2] =
    if i >= N then Cont.pure(acc)
    else (i % 3) match
      case 0 => Cont.perform[State % Int, R2, Int](State.Get[Int, Int]())(using sIn).flatMap(x => pOneBind(i + 1, acc + x))
      case 1 => Cont.perform[State % Int, R2, Int](State.Update[Int, Int](s => (s, i)))(using sIn).flatMap(x => pOneBind(i + 1, acc + x))
      case _ => Cont.perform[Writer % String, R2, Unit](Writer.Say("w"))(using wIn).flatMap(_ => pOneBind(i + 1, acc + 1))

  @Benchmark
  def pathed(): Int =
    val w = Cont.handle(pOneBind(0, 0))(KWriter[State % Int +: Pure, Int]())
    val s = Cont.handle(w)(KState[Pure, (Seq[String], Int)](0))
    s.run._2._2

  // ---- one effect: stateOneBind's program (a modify a step, one bind), three ways
  private def rOne(i: Int, acc: Int): okay.![Int, State % Int] =
    if i >= N then pure(acc) else State.modify[Int](_ + i).flatMap(s => rOne(i + 1, acc + s))

  @Benchmark
  def stateRoot(): Int = State.handle[Int](0)(rOne(0, 0)).run._2

  private def kOne(i: Int, acc: Int): okay.one.![Int, Pure] =
    if i >= N then Cont.pure(acc)
    else Cont.perform[State % Int, Pure, Int](State.Update[Int, Int](State.Modified(_ + i)))(using stateAt)
      .flatMap(s => kOne(i + 1, acc + s))

  @Benchmark
  def stateKeyed(): Int =
    Cont.handle(kOne(0, 0))(KState[Pure, Int](0))(using okay.one.row.Removed.Absent[State % Int, Pure]()).run._2

  /** the key looked up a step (the root's `effect` today: `ClassValue`) */
  private def cOne(i: Int, acc: Int): okay.one.![Int, Pure] =
    if i >= N then Cont.pure(acc)
    else Cont.perform[State % Int, Pure, Int](State.Update[Int, Int](State.Modified(_ + i)))(
      using okay.KeyOf.of[State % Int](classOf[State[?, ?]]).at[Pure]).flatMap(s => cOne(i + 1, acc + s))

  @Benchmark
  def stateLookup(): Int =
    Cont.handle(cOne(0, 0))(KState[Pure, Int](0))(using okay.one.row.Removed.Absent[State % Int, Pure]()).run._2

  /** the root's handler (`State(0)`) over the kernel's tree with the key a val */
  @Benchmark
  def stateRootHandler(): Int =
    Cont.handle(kOne(0, 0))(State[Int](0).kernel[Pure, Int].keyed(stateAt.ref))(
      using okay.one.row.Removed.Absent[State % Int, Pure]()).run._2
}
