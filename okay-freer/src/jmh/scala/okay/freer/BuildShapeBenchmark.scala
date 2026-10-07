package okay.freer

import okay.given

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{State as JmhState, *}
import okay.freer.Row.at

/**
 * THE SHAPE A PROGRAM IS BUILT IN (left-nested-build-cost, 2026-09-27).
 * The same N operations, built and run: once by `foldLeft` over
 * `flatMap` (left-nested, rotated by `Free.resume` one bind at a time)
 * and once by `!.each` / `!.foldM` (right-nested, never rotated). The
 * build is inside the measured method because every real caller builds
 * per call (a stage telling a chunk, a handler folding its choices).
 */
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@JmhState(Scope.Benchmark)
class BuildShapeBenchmark {
  val N = 1000
  val items: Vector[Int] = Vector.tabulate(N)(identity)

  /** a Writer telling every element, the shape of a Stage telling a chunk */
  @Benchmark
  def tellsFoldLeft(): Int =
    val p = items.foldLeft(pure[Writer % Int, Unit](()))((m, i) => m.flatMap(_ => Writer.tell(i)))
    !.run(Writer.run[Int, Unit, Pure](p))._1.length

  @Benchmark
  def tellsEach(): Int =
    val p = !.each(items)(i => Writer.tell(i))
    !.run(Writer.run[Int, Unit, Pure](p))._1.length

  /** a State counter with an accumulator, the shape of a fold with effects */
  @Benchmark
  def stateFoldLeft(): Int =
    val p = items.foldLeft(pure[State % Int, Int](0))((m, i) => m.flatMap(acc => State.modify[Int](_ + i).map(acc + _)))
    !.run(State.handle[Int](0)(p))._2

  @Benchmark
  def stateFoldM(): Int =
    val p = !.foldM(items)(0)((acc, i) => State.modify[Int](_ + i).map(acc + _))
    !.run(State.handle[Int](0)(p))._2

  /** TWO handlers over a mixed row, FusionBenchmark's `sw` program built
   * per call, the only difference the build: does the 32-vs-13 µs gap of
   * nestedSW/nestedSWr come from the shape when two passes run? */
  type SW = State % Int + Writer % String

  private def op(i: Int, acc: Int): Int ! SW = (i % 3) match
    case 0 => State.get[Int].at[SW].map(acc + _)
    case 1 => State.set[Int](i).at[SW].map(acc + _)
    case _ => Writer.tell("w").at[SW].map(_ => acc + 1)

  @Benchmark
  def rowFoldLeft(): Int =
    val p = items.foldLeft(pure[SW, Int](0))((m, i) => m.flatMap(acc => op(i, acc)))
    State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, State % Int](p))._2._2

  @Benchmark
  def rowFoldM(): Int =
    val p = !.foldM(items)(0)((acc, i) => op(i, acc))
    State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, State % Int](p))._2._2

  /** map-flatmap-pair-cost: the SAME right-nested program as rowFoldM,
   * built per call, but each step is ONE bind (the operation flatMapped
   * straight into the next step, as FusionBenchmark's rightSW) instead of
   * `op.map(acc + _)` and then foldM's flatMap */
  private def oneBind(i: Int, acc: Int): Int ! SW =
    if i >= N then pure(acc)
    else (i % 3) match
      case 0 => State.get[Int].at[SW].flatMap(x => oneBind(i + 1, acc + x))
      case 1 => State.set[Int](i).at[SW].flatMap(x => oneBind(i + 1, acc + x))
      case _ => Writer.tell("w").at[SW].flatMap(_ => oneBind(i + 1, acc + 1))

  @Benchmark
  def rowOneBind(): Int =
    State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, State % Int](oneBind(0, 0)))._2._2

  /** one-bind-hot-steps: stateFoldM's work written by hand with ONE
   * flatMap a step, the ceiling a fused foldM step aims at */
  private def stateOne(i: Int, acc: Int): Int ! State % Int =
    if i >= N then pure(acc) else State.modify[Int](_ + i).flatMap(s => stateOne(i + 1, acc + s))

  @Benchmark
  def stateOneBind(): Int =
    !.run(State.handle[Int](0)(stateOne(0, 0)))._2

  /** op-map-constructors: a step that is `State.update` — before, a get,
   * a set and a map; after, ONE `Update` operation */
  @Benchmark
  def stateUpdate(): Int =
    val p = !.foldM(items)(0)((acc, i) => State.update[Int, Int](s => (s, s + i)).map(acc + _))
    !.run(State.handle[Int](0)(p))._2

  /** reader-asks-op: a step that is `Reader.lift` (and `read`, the same
   * shape) — before, the shared Ask and a map; after, ONE `Asks` */
  @Benchmark
  def readerLift(): Int =
    val p = !.foldM(items)(0)((acc, i) => Reader.lift[Int, Int]((e: Int) ?=> e + i).map(acc + _))
    !.run(Reader.run[Int, Int, Pure](7)(p))

  // ---- map-cost-residual: the ladder from rowOneBind up to rowFoldM
  //
  // rowFoldM's map is ALREADY fused in the direct form (Effects.step),
  // and it still reads ~2x rowOneBind. Each rung below adds ONE of the
  // things foldM does that oneBind does not, so the gap can be named.

  /** rung 1: rowOneBind's loop, but the step is `op(i, acc)` — the
   * `Bind(Inject, Mapped)` that `.map` builds — unwrapped the way
   * `Effects.step` unwraps it. Adds the discarded pair and the run-time
   * match; still one closure a step, still a primitive accumulator.
   * The type claims are `step`'s own. */
  private def unwrap(i: Int, acc: Int): Int ! SW =
    if i >= N then pure(acc)
    else op(i, acc) match
      case b: Freer.Bind[Unary[SW], Unit, Unit, Unit, x, Int] @unchecked => b.f match
        case k: Freer.Mapped[Unary[SW], Unit, x, Int] @unchecked => Free.Bind(b.a, (y: x) => unwrap(i + 1, k.f(y)))
        case _ => Free.Bind(op(i, acc), (y: Int) => unwrap(i + 1, y))
      case m => Free.Bind(m, (y: Int) => unwrap(i + 1, y))

  @Benchmark
  def rowUnwrap(): Int =
    State.run[Int, (Seq[String], Int)](0)(Writer.run[String, Int, State % Int](unwrap(0, 0)))._2._2

  /** boxing control: the same two programs over a `case class`
   * accumulator, so BOTH allocate one accumulator a step. foldM's `B`
   * is erased and boxes an Int on every step; oneBind's `acc: Int`
   * never does. If this pair's ratio is smaller than rowFoldM/rowOneBind,
   * the difference is boxing. */
  final case class Acc(n: Int)

  private def opAcc(i: Int, acc: Acc): Acc ! SW = (i % 3) match
    case 0 => State.get[Int].at[SW].map(x => Acc(acc.n + x))
    case 1 => State.set[Int](i).at[SW].map(x => Acc(acc.n + x))
    case _ => Writer.tell("w").at[SW].map(_ => Acc(acc.n + 1))

  @Benchmark
  def rowFoldMAcc(): Int =
    val p = !.foldM(items)(Acc(0))((acc, i) => opAcc(i, acc))
    State.run[Int, (Seq[String], Acc)](0)(Writer.run[String, Acc, State % Int](p))._2._2.n

  private def oneBindAcc(i: Int, acc: Acc): Acc ! SW =
    if i >= N then pure(acc)
    else (i % 3) match
      case 0 => State.get[Int].at[SW].flatMap(x => oneBindAcc(i + 1, Acc(acc.n + x)))
      case 1 => State.set[Int](i).at[SW].flatMap(x => oneBindAcc(i + 1, Acc(acc.n + x)))
      case _ => Writer.tell("w").at[SW].flatMap(_ => oneBindAcc(i + 1, Acc(acc.n + 1)))

  @Benchmark
  def rowOneBindAcc(): Int =
    State.run[Int, (Seq[String], Acc)](0)(Writer.run[String, Acc, State % Int](oneBindAcc(0, Acc(0))))._2._2.n

  /** fold-each: stateFoldM's work with the step split into the element's
   * program and a pure combine, one bind a step by construction */
  @Benchmark
  def stateFoldEach(): Int =
    val p = !.foldEach(items)(0)(i => State.modify[Int](_ + i))(_ + _)
    !.run(State.handle[Int](0)(p))._2

  // ---- fold-each-residual-split: the ladder from stateFoldEach DOWN to
  // stateOneBind. fold-each named its 1.8x over the hand loop as "the
  // boxed accumulator and the generic calls" without a number for
  // either. Each rung below removes ONE thing the library's foldEach
  // does that stateOne does not.

  /** rung D1: `Effects.foldEach`'s `go` verbatim, with the accumulator
   * and the element's answer fixed to `Int` and only the element type
   * still generic — so `v(i)` still hands `f` the Vector's own boxed
   * element (no re-box the library does not pay), `f` and `combine` are
   * still Function values called through their interfaces, and the
   * closure captures the same six things. What is gone is the erased
   * `B`: no `Integer.valueOf` of the sum a step, no unbox on the way in. */
  private def foldEachInt[X](xs: IndexedSeq[X])(f: X => Int ! State % Int)(combine: (Int, Int) => Int): Int ! State % Int =
    val n = xs.length
    def go(i: Int, acc: Int): Int ! State % Int =
      if i >= n then pure(acc) else f(xs(i)).flatMap(a => go(i + 1, combine(acc, a)))
    Free.delay(() => go(0, 0))

  @Benchmark
  def stateFoldEachInt(): Int =
    val p = foldEachInt(items)(i => State.modify[Int](_ + i))(_ + _)
    !.run(State.handle[Int](0)(p))._2

  /** rung D2: stateOne's loop, the step and the combine written inline
   * (no Function values, no interface calls), but the element still
   * read out of the Vector as foldEach reads it. What is left over
   * stateOneBind is `items(i)`: the radix walk and the unbox. */
  private def stateOneVec(i: Int, acc: Int): Int ! State % Int =
    if i >= N then pure(acc)
    else
      val x = items(i)
      State.modify[Int](_ + x).flatMap(s => stateOneVec(i + 1, acc + s))

  @Benchmark
  def stateOneVecBind(): Int =
    !.run(State.handle[Int](0)(stateOneVec(0, 0)))._2

  /** the road a rung named: `Effects.foldEach` verbatim (erased `B`,
   * Function values, the lot), with the ONE line changed — the
   * elements copied into a flat `ArraySeq` once per program instead of
   * read out of the Vector by its radix walk on every step. The copy is
   * paid inside the measured method, as the library would pay it. */
  private def foldEachArr[X, A, B](xs: Iterable[X])(z: B)(f: X => A ! State % Int)(combine: (B, A) => B): B ! State % Int =
    val v = scala.collection.immutable.ArraySeq.untagged.from(xs)
    val n = v.length
    def go(i: Int, acc: B): B ! State % Int =
      if i >= n then pure(acc) else f(v(i)).flatMap(a => go(i + 1, combine(acc, a)))
    Free.delay(() => go(0, z))

  @Benchmark
  def stateFoldEachArr(): Int =
    val p = foldEachArr(items)(0)(i => State.modify[Int](_ + i))(_ + _)
    !.run(State.handle[Int](0)(p))._2

  // ---- fold-erased-accumulator: the inline road. The library's `go`
  // verbatim (ArraySeq read, erased signature, the lot), but the method
  // and its step functions are `inline`, so `go` is expanded AT THE
  // CALL SITE: compiled with the caller's `B` (`Int` here — no
  // `Integer.valueOf` a step, D1's rung on the library's own road) and
  // with the step lambda beta-reduced into its body, so a `.map`-written
  // step's `Bind` + `Mapped` are built and matched inside ONE compiled
  // unit, where escape analysis can see both ends.

  /** `Effects.foldEach` under `inline` */
  private inline def foldEachInline[X, A, B, F[+_]](xs: Iterable[X])(z: B)(inline f: X => A ! F)(inline combine: (B, A) => B): B ! F =
    val v = scala.collection.immutable.ArraySeq.untagged.from(xs)
    val n = v.length
    def go(i: Int, acc: B): B ! F =
      if i >= n then pure(acc) else f(v(i)).flatMap(a => go(i + 1, combine(acc, a)))
    Free.delay(() => go(0, z))

  @Benchmark
  def stateFoldEachInline(): Int =
    val p = foldEachInline(items)(0)(i => State.modify[Int](_ + i))(_ + _)
    !.run(State.handle[Int](0)(p))._2

  /** `Effects.foldM` under `inline`: the same match that folds a
   * trailing `map` into the next step, now over a node the same method
   * just built */
  private inline def foldMInline[X, B, F[+_]](xs: Iterable[X])(z: B)(inline f: (B, X) => B ! F): B ! F =
    val v = scala.collection.immutable.ArraySeq.untagged.from(xs)
    val n = v.length
    def go(i: Int, acc: B): B ! F =
      if i >= n then pure(acc)
      else f(acc, v(i)) match
        case b: Freer.Bind[Unary[F], Unit, Unit, Unit, x, B] @unchecked => b.f match
          case k: Freer.Mapped[Unary[F], Unit, x, B] @unchecked => Free.Bind(b.a, (y: x) => go(i + 1, k.f(y)))
          case _ => Free.Bind(b, (y: B) => go(i + 1, y))
        case m => Free.Bind(m, (y: B) => go(i + 1, y))
    Free.delay(() => go(0, z))

  @Benchmark
  def stateFoldMInline(): Int =
    val p = foldMInline(items)(0)((acc, i) => State.modify[Int](_ + i).map(acc + _))
    !.run(State.handle[Int](0)(p))._2
}
