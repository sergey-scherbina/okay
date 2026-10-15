package okay

import okay.std.*
import scala.annotation.tailrec
import scala.reflect.ClassTag

/**
 * A `Stage` driven by PUSHING its tells (interop-shared) — how a push-style
 * transformer of another world runs an okay stage: a JDK `Gatherer`
 * (okay-java's `Gather`), a Clojure transducer (okay-clojure's
 * `Transducers`). Its state between two elements is the stage suspended at
 * its next `await`.
 *
 * `emit` receives each tell and answers whether to go on: a refused push
 * (a downstream that wants nothing more) ends the drive, and the stage's
 * continuation past that tell is never built.
 */
object Push {

  /** where a driven stage stands between two calls from outside */
  enum Pos[I, O, A]:
    /** not started: it may tell before its first await */
    case Fresh(stage: Stage[I, O, A])
    /** suspended at an await; the next element resumes it */
    case Waiting(k: Option[I] => Stage[I, O, A])
    /** answered, or a push was refused: nothing more to do */
    case Done()

  /** the stage walked to its next await (`Walk.next`), its tells on the way walked out one by one (`Walk.view`) */
  private type Awaited[I, O, A] = Walk.Step[Take % I, A, Take % I + Writer % O]
  private def walked[I, O, A](p: Stage[I, O, A]): Walk.Step[Writer % O, Awaited[I, O, A], Writer % O] =
    Walk.view[Writer % O, Awaited[I, O, A]](Walk.next[Take % I, A, Take % I + Writer % O](p))

  /** run until an await or the answer, emitting every tell; `ended` answers
   * every await with None (the finisher's, the completion arity's, mode) */
  def drive[I, O, A](p: Stage[I, O, A], emit: O => Boolean, ended: Boolean): Pos[I, O, A] =
    step(walked(p), emit, ended)

  @tailrec private def step[I, O, A](w: Walk.Step[Writer % O, Awaited[I, O, A], Writer % O], emit: O => Boolean,
      ended: Boolean): Pos[I, O, A] = w match
    case Walk.Done(at) => at match
      case Walk.Done(_) => Pos.Done()
      case o: Walk.Op[Take % I, x, A, Take % I + Writer % O] => o.op match
        case Take.Await() =>
          if ended then step(walked(o.resume(None)), emit, ended) else Pos.Waiting[I, O, A](oi => o.resume(oi))
    case o: Walk.Op[Writer % O, x, Awaited[I, O, A], Writer % O] =>
      val (v, rest) = told(o)
      if emit(v) then step(Walk.view(rest), emit, ended) else Pos.Done[I, O, A]()

  /** a tell: its value, and the rest past it */
  private def told[O, X, R](o: Walk.Op[Writer % O, X, R, Writer % O]): (O, R ! Writer % O) = o.op match
    case Writer.Say(v) => (v, o.resume(()))

  /** hand the stage one element: a fresh stage first runs to its first
   * await (its header's tells go out), then the element resumes it */
  def offer[I, O, A](pos: Pos[I, O, A], i: I, emit: O => Boolean): Pos[I, O, A] =
    val at = pos match
      case Pos.Fresh(p) => drive(p, emit, ended = false)
      case other => other
    at match
      case Pos.Waiting(k) => drive(k(Some(i)), emit, ended = false)
      case other => other

  /** the input is over: the stage runs to its answer, flushing */
  def end[I, O, A](pos: Pos[I, O, A], emit: O => Boolean): Pos[I, O, A] = pos match
    case Pos.Fresh(p) => drive(p, emit, ended = true)
    case Pos.Waiting(k) => drive(k(None), emit, ended = true)
    case done => done
}

/**
 * A program written in ANOTHER language as data — an answer, or one
 * operation and a function from its answer to the rest — walked as an okay
 * program or stage (interop-shared): okay-clojure's `okay.core` programs,
 * okay-frege's `Prog`. Each step is one okay program node (the walk is
 * trampolined), each operation runs under the okay program's handlers,
 * and the continuation is the other language's function — a multi-shot
 * handler calls it per branch.
 *
 * A `View` reads a node without allocating one per step: its kind as an
 * Int, its payload, its resumption.
 */
object Foreign {

  inline val Done = 0
  inline val Await = 1
  inline val Tell = 2
  inline val Perform = 3
  inline val Lift = 4

  /** how to read one language's program nodes */
  trait View[P]:
    /** Done, Await, Tell, Perform or Lift */
    def kind(p: P): Int
    /** Done: the answer; Tell: the value; Perform: the operation */
    def payload(p: P): AnyRef
    /** Lift: run the lifted action of that language, answering its value */
    def lift(p: P): AnyRef
    /** the rest of the program, given this step's answer */
    def resume(p: P, answer: AnyRef): P
    /**
     * Lift: the lifted action as an OPERATION a cancel can interrupt, when
     * the language can make one (interop-lift-cancellation). The walker
     * performs it when the program's row accepts it — a row with `Async` —
     * and runs the action in place otherwise. None by default.
     */
    def liftAsOperation(p: P): Option[AnyRef] = None
    /** the prefix of this language's refusals, e.g. "okay.clojure" */
    def who: String

  /** a value from the other side, as the type the okay side declared */
  def as[T](x: Any, what: String, who: String)(using ct: ClassTag[T]): T = x match
    case ct(t) => t
    case other => throw IllegalArgumentException(
      s"$who: $what expected ${ct.runtimeClass.getName}, got " +
        (if other == null then "null" else other.getClass.getName))

  /**
   * An erased value handed to the other language, which takes `Object`:
   * at run time it already IS one (a primitive arrives boxed), so this
   * matches every value but null and casts nothing.
   */
  def obj(a: Any): AnyRef = a match
    case r: AnyRef => r
    case _ => null

  /**
   * A CALLBACK call from another JVM language's program (foreign-one-ops):
   * the operation a generated Frege or Clojure binding makes. The row does
   * not answer it; the callbacks the caller offered to `run` do, the same
   * `Foreign.callback`s that serve every wire language.
   */
  final case class Call(name: String, arg: AnyRef)

  /**
   * Whether a value is an operation of the effects F, and that operation
   * PERFORMED — for an operation that arrives UNTYPED, as an `Object` from
   * another language (okay-clojure's `Program`, okay-frege's `Prog`). A
   * single signature's is FOUND (its `TypeableK`); a sum is BUILT with `|`
   * (`Member.of[Reader % Long] | Member.of[State % Long]`), each side
   * tested in turn and its operation performed at its own place.
   */
  trait Member[F[_]]:
    def test(x: Any): Boolean
    /** `x`, which the test said IS an operation of F, performed; its value as the other side takes it */
    def perform(x: AnyRef): AnyRef ! F
    /** the operation performed, or None when `x` is not one of F's */
    final def operation(x: AnyRef): Option[AnyRef ! F] = if test(x) then Some(perform(x)) else None
    /** this signature and another, side by side */
    def |[G[_]](g: Member[G]): Member[F + G] =
      val f = this
      new Member[F + G]:
        def test(x: Any): Boolean = f.test(x) || g.test(x)
        def perform(x: AnyRef): AnyRef ! F + G =
          if f.test(x) then f.perform(x).at[F + G] else g.perform(x).at[F + G]

  object Member:
    given one[F[_]](using t: TypeableK[F]): Member[F] = new Member[F]:
      def test(x: Any): Boolean = t.test(x)
      def perform(x: AnyRef): AnyRef ! F = effect[F, Any](asOperation[F](x)).map(obj)
    /** no effects: nothing is an operation of them */
    given pure: Member[Pure] = new Member[Pure]:
      def test(x: Any): Boolean = false
      def perform(x: AnyRef): AnyRef ! Pure = throw IllegalArgumentException(s"not an operation: $x")
    def of[F[_]](using t: TypeableK[F]): Member[F] = one[F]

  /** THE ONE CAST of the interop bridges: a value the signature's `TypeableK` said is an operation of F is an
   * `F[X]` for the X the other side will receive as an `Object`, so `F[Any]` (its answer goes back erased) */
  private def asOperation[F[_]](x: AnyRef): F[Any] = x.asInstanceOf[F[Any]]

  /** the callbacks a program was offered, as the walker asks them: the
   * program answering `name` applied to `arg`, or None where none has it */
  trait Calls[F[_]]:
    def apply(name: String, arg: AnyRef): Option[AnyRef ! F]
    /** the names offered, for a refusal */
    def names: Seq[String]

  object Calls:
    /** no callbacks: a `Call` is refused by name */
    def none[F[_]]: Calls[F] = new Calls[F]:
      def apply(name: String, arg: AnyRef): Option[AnyRef ! F] = None
      def names: Seq[String] = Nil

  private def described(x: Any): String = if x == null then "null" else x.getClass.getName

  /**
   * The program as a stage that may also perform operations of F —
   * okay-stream's effectful stage row, so it composes through `through`.
   */
  def stageWith[I, O: ClassTag, F[_], P](prog: => P, name: String)
                                        (using v: View[P], m: Member[F]): Unit ! (Take % I + (Writer % O + F)) =
    type R = Take % I + (Writer % O + F)
    def go(p: P): Unit ! R = v.kind(p) match
      case Done => pure(())
      case Await => effect[Take % I, Option[I]](Take.Await()).at[R].flatMap(in => go(v.resume(p, in.fold(null)(obj))))
      case Tell => Writer.tell(as[O](v.payload(p), s"$name's tell", v.who)).at[R].flatMap(_ => go(v.resume(p, null)))
      case Lift => v.liftAsOperation(p).flatMap(m.operation) match
        case Some(o) => o.at[R].flatMap(x => go(v.resume(p, x)))
        case None => defer(go(v.resume(p, v.lift(p))))
      case _ =>
        val raw = v.payload(p)
        m.operation(raw) match
          case Some(o) => o.at[R].flatMap(x => go(v.resume(p, x)))
          case None => throw IllegalArgumentException(
            s"${v.who}: $name performed ${described(raw)}, which is not an operation of this " +
              "stage's row (a stage's own are await and tell; stageWith[I, O, F] adds F)")
    defer(go(prog))

  /** the program as `A ! F`; its await and tell belong to a stage, and a
   * `Call` it performs is answered by `calls` */
  def run[F[_], A: ClassTag, P](prog: => P, name: String, calls: Calls[F] = Calls.none[F])
                                (using v: View[P], m: Member[F]): A ! F =
    def go(p: P): A ! F = v.kind(p) match
      case Done => pure(as[A](v.payload(p), s"$name's answer", v.who))
      case Lift => v.liftAsOperation(p).flatMap(m.operation) match
        case Some(o) => o.flatMap(x => go(v.resume(p, x)))
        case None => defer(go(v.resume(p, v.lift(p))))
      case Perform => v.payload(p) match
        case Call(n, arg) => calls(n, arg) match
          case Some(answer) => answer.flatMap(x => go(v.resume(p, x)))
          case None => throw IllegalArgumentException(
            s"${v.who}: $name called '$n', which no callback offered to it answers " +
              s"(offered: ${calls.names.mkString("[", ", ", "]")})")
        case raw => m.operation(raw) match
          case Some(o) => o.flatMap(x => go(v.resume(p, x)))
          case None => throw IllegalArgumentException(
            s"${v.who}: $name performed ${described(raw)}, which is not an operation of this program's row")
      case k => throw IllegalStateException(
        s"${v.who}: $name used ${if k == Await then "await" else "tell"} outside a stage; " +
          "the language's stage runs a program that awaits and tells")
    defer(go(prog))
}
