package okay2

import scala.annotation.{tailrec, unused}
import scala.reflect.ClassTag
import Free.{Return, Inject, Bind}

/**
 * CONDITIONS AND RESTARTS (okay2-condition-repair; the Scala 3 core's `okay.Condition`, specs/condition.md): Common
 * Lisp's three-way decision, as an effect. Code that meets trouble SIGNALS a condition; the code that knows what to
 * do about it offers named RESTARTS by wrapping regions in frames; a POLICY, chosen at run time, decides: resume at
 * the signal point with a value, invoke a restart (unwind to its frame, which recovers with the value), or fail.
 * The signal point does not decide, the frame does not decide — the policy does, after seeing both.
 *
 * Scala 2's differences from the core: the row is the trait `Condition`, its operations `Condition.Op` (okay2's
 * shape for every effect); `frame`'s restart is an explicit parameter where the core's is a context function; the
 * core's direct-style `frame` (a `direct` block as the body) has no twin, okay2 having no `direct`.
 */
sealed trait Condition extends Row { type Op[+A] = Condition.Op[A] }

object Condition {

  /** what the policy answers for a signal */
  sealed abstract class Decision
  object Decision {
    /** continue AT the signal point, the condition answered with `value` */
    final case class Resume(value: Any) extends Decision
    /** unwind to the nearest frame named `restart`, which recovers with `value` */
    final case class Invoke(restart: String, value: Any) extends Decision
    /** escalate: `Unhandled`, naming the condition and the restarts declined */
    case object Fail extends Decision
  }

  sealed trait Op[+A]
  /** a condition raised; `accept` checks the policy's resume value against the answer type */
  final case class Signal[A](condition: Any, accept: Any => A) extends Op[A]
  /** a region offering the restart `name` (identified by `id`), its body a program in the handler's row */
  final case class Within[A, V](name: String, id: AnyRef, body: Any, recover: V => A, accept: Any => V) extends Op[A]
  /** a restart invoked by its handle: unwind to ITS frame */
  final case class Leave[V](handle: Restart[V], value: V) extends Op[Nothing]

  implicit val effect: Effect[Condition] = Effect.of[Condition]

  final case class Unhandled(condition: Any, menu: Vector[String])
    extends RuntimeException(
      s"unhandled condition: $condition (restarts on offer: ${if (menu.isEmpty) "none" else menu.mkString(", ")})")

  final case class NoSuchRestart(restart: String, menu: Vector[String])
    extends RuntimeException(
      s"no restart '$restart' on the menu (${if (menu.isEmpty) "none" else menu.mkString(", ")})")

  /** the policy resumed a signal with a value of the wrong type: the policy's bug, named */
  final class BadResume(condition: Any, value: Any, expected: String)
    extends RuntimeException(s"the policy resumed $condition with $value: not a $expected")

  /** raise `condition`, answered by the policy with an `A` */
  def signal[A](condition: Any)(implicit tag: ClassTag[A]): A ! Condition =
    Free.inject[Condition, A](Signal[A](condition, checked(condition, tag)))

  private def checked[A](condition: Any, tag: ClassTag[A]): Any => A = {
    case tag(a) => a
    case other => throw new BadResume(condition, other, tag.runtimeClass.getName)
  }

  /** the answer type a condition type `C` is resumed at */
  trait Answers[C, A] { def tag: ClassTag[A] }
  object Answers {
    def of[C, A](implicit t: ClassTag[A]): Answers[C, A] = new Answers[C, A] { def tag: ClassTag[A] = t }
    // A solved from the evidence: Scala 2 does not read it off a bound `C <: Of[A]` as the core's given does
    implicit def fromOf[C, A](implicit @unused ev: C <:< Of[A], t: ClassTag[A]): Answers[C, A] = of[C, A]
  }

  /** raise a typed condition: its answer type is the one `Answers` gives it */
  def raiseC[C, A](c: C)(implicit ev: Answers[C, A]): A ! Condition = signal[A](c)(ev.tag)

  /** a frame's own handle, given only to the body of THAT frame: no frame, no handle, no invoke */
  final class Restart[V] private[Condition] (private[Condition] val name: String) {
    def invoke[X](v: V): X ! Condition = Free.inject[Condition, X](Leave(this, v))
  }

  /** a region offering a restart `name` whose handle the body gets; invoking it unwinds to THIS frame (by identity,
   * so two frames of one name cannot alias) and `recover`s with the value */
  def frame[A, V, F <: Row](name: String)(body: Restart[V] => A ! (Condition + F))(recover: V => A)
                          (implicit tag: ClassTag[V]): A ! (Condition + F) = {
    val handle = new Restart[V](name)
    Free.inject[Condition, A](Within[A, V](name, handle, body(handle), recover, checked(name, tag))).plus[F]
  }

  /** a region offering a restart `name` to the POLICY (by name): `Invoke(name, v)` unwinds here */
  def within[A, F <: Row](name: String)(body: A ! (Condition + F))(recover: Any => A): A ! (Condition + F) =
    Free.inject[Condition, A](Within[A, Any](name, new Object, body, recover, identity)).plus[F]

  private final case class Frame(name: String, id: AnyRef)

  /** a region's outcome: its value, or a restart on its way to the frame that owns it */
  private sealed trait Out[+X]
  private final case class Done[X](x: X) extends Out[X]
  private final case class Escape(target: AnyRef, value: Any) extends Out[Nothing]

  /**
   * Run the conditions of `prog` under `policy`, which sees each condition and the restarts on offer (innermost
   * first). Other effects are forwarded. A nested frame's region is a program the outer one continues into, its
   * walk DEFERRED (`Free.delay`), so frames nested to any depth — a recursive program opening one per level — take
   * no host frame each (the core recurses into it directly).
   */
  def run[A, F <: Row](policy: (Any, Vector[String]) => Decision)(prog: Free[Condition with F, A]): A ! F = {
    val Mine = Split.at[Condition]

    def loop[X](p0: Free[Condition with F, X], menu: List[Frame]): Out[X] ! F = {
      // only a signal or an invoke reads the menu's names: built per frame up front, nesting n frames cost n^2
      lazy val names = menu.map(_.name).toVector

      def step(op: Op[Any], k: Any => Free[Condition with F, X]): Either[Free[Condition with F, X], Out[X] ! F] = op match {
        case Leave(handle, v) =>
          if (!menu.exists(_.id eq handle)) throw NoSuchRestart(handle.name, names)
          Right(pure[F, Out[X]](Escape(handle, v)))
        case Signal(c, accept) =>
          policy(c, names) match {
            case Decision.Resume(v) => Left(k(accept(v)))
            case Decision.Invoke(name, v) =>
              if (!names.contains(name)) throw NoSuchRestart(name, names)
              Right(pure[F, Out[X]](Escape(name, v)))
            case Decision.Fail => throw Unhandled(c, names)
          }
        case w: Within[a, v] =>
          // THE CLAIM, the core's: a frame's body is a program in this handler's row, erased at the operation
          // because the row's other half F is not the operation's to name
          val body = w.body.asInstanceOf[Free[Condition with F, a]]
          Right(Free.delay[F, Out[a]](() => loop[a](body, Frame(w.name, w.id) :: menu)).flatMap {
            case Done(b) => loop(k(b), menu)
            case Escape(t, x) if t == w.name || (t eq w.id) => loop(k(w.recover(w.accept(x))), menu)
            case e: Escape => pure[F, Out[X]](e) // an outer frame's
          })
      }

      @tailrec def walk(p: Free[Condition with F, X]): Out[X] ! F = {
        val next: Either[Free[Condition with F, X], Out[X] ! F] = Free.resume(p) match {
          case Return(x) => Right(pure[F, Out[X]](Done(x)))
          case Inject(e) => Left(Bind(Inject[Condition with F, X](e), (x: X) => Return[Condition with F, X](x)))
          case Bind(Inject(Mine(op)), k) => step(op, k)
          case Bind(Inject(g), k) => Right(Inject[F, Any](g).flatMap(x => loop(k(x), menu)))
          case other => throw new IllegalStateException("resume left a non-head form: " + other)
        }
        next match {
          case Left(p2) => walk(p2)
          case Right(out) => out
        }
      }
      walk(p0)
    }

    loop(prog, Nil).map {
      case Done(a) => a
      case Escape(t, _) => throw new IllegalStateException(s"restart '$t' escaped every frame")
    }
  }

  /** a condition that names its answer type: `object HowMany extends Of[Int]`, raised by `HowMany.signal` */
  trait Of[A]

  implicit final class OfOps[A](private val c: Of[A]) extends AnyVal {
    def signal(implicit tag: ClassTag[A]): A ! Condition = raiseC[Of[A], A](c)(Answers.of[Of[A], A])
  }

  /** a policy's typed resume of an `Of[A]`: a wrong-typed value does not compile */
  def resume[A](c: Of[A])(v: A): Decision = { val _ = c; Decision.Resume(v) }
}
