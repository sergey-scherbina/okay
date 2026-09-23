package okay

import okay.Free.{Bind, Inject, Return}
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

  /** run until an await or the answer, emitting every tell; `ended` answers
   * every await with None (the finisher's, the completion arity's, mode) */
  @tailrec def drive[I, O, A](p: Stage[I, O, A], emit: O => Boolean, ended: Boolean): Pos[I, O, A] =
    (p.resume: @unchecked) match
      case Return(_) => Pos.Done()
      case Inject(e) => split[Take % I, Writer % O](e)
        { case Take.Await() => Pos.Done[I, O, A]() }
        { case Writer.Say(o) => emit(o): Unit; Pos.Done[I, O, A]() }
      case Bind(Inject(e), k) => split[Take % I, Writer % O](e)
        { case Take.Await() =>
            if ended then drive(k(None), emit, ended) else Pos.Waiting[I, O, A](k) }
        { w0 => (w0: @unchecked) match
            case Writer.Say(o) =>
              if emit(o) then drive(k(()), emit, ended) else Pos.Done[I, O, A]() }

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

  private def described(x: Any): String = if x == null then "null" else x.getClass.getName

  /**
   * The program as a stage that may also perform operations of F —
   * okay-stream's effectful stage row, so it composes through `through`.
   */
  def stageWith[I, O: ClassTag, F[+_], P](prog: => P, name: String)
                                        (using v: View[P], m: Member[F]): Unit ! (Take % I + (Writer % O + F)) =
    type R = Take % I + (Writer % O + F)
    def go(p: P): Unit ! R = v.kind(p) match
      case Done => pure(())
      case Await => effect[R, Option[I]](Take.Await()).flatMap(in => go(v.resume(p, in.fold(null)(obj))))
      case Tell => effect[R, Unit](Writer(as[O](v.payload(p), s"$name's tell", v.who))).flatMap(_ => go(v.resume(p, null)))
      case Lift => Free.delay(() => go(v.resume(p, v.lift(p))))
      case _ =>
        val raw = v.payload(p)
        m.operation(raw) match
          case Some(o) => effect[R, Any](o).flatMap(x => go(v.resume(p, obj(x))))
          case None => throw IllegalArgumentException(
            s"${v.who}: $name performed ${described(raw)}, which is not an operation of this " +
              "stage's row (a stage's own are await and tell; stageWith[I, O, F] adds F)")
    Free.delay(() => go(prog))

  /** the program as `A ! F`; its await and tell belong to a stage */
  def run[F[+_], A: ClassTag, P](prog: => P, name: String)(using v: View[P], m: Member[F]): A ! F =
    def go(p: P): A ! F = v.kind(p) match
      case Done => pure(as[A](v.payload(p), s"$name's answer", v.who))
      case Lift => Free.delay(() => go(v.resume(p, v.lift(p))))
      case Perform =>
        val raw = v.payload(p)
        m.operation(raw) match
          case Some(o) => effect[F, Any](o).flatMap(x => go(v.resume(p, obj(x))))
          case None => throw IllegalArgumentException(
            s"${v.who}: $name performed ${described(raw)}, which is not an operation of this program's row")
      case k => throw IllegalStateException(
        s"${v.who}: $name used ${if k == Await then "await" else "tell"} outside a stage; " +
          "the language's stage runs a program that awaits and tells")
    Free.delay(() => go(prog))
}
