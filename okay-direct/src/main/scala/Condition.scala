package okay


import okay.core.{Row, Split}
import okay.one.handler.{Stateful, Handler as OneHandler}
import okay.Direct.DirectCtx

/**
 * Conditions: resumable exceptions (specs/condition.md) — the road
 * between throwing and damage-as-data. `signal` raises WITHOUT
 * unwinding: the policy runs while the signal point's continuation
 * is live, so "continue from where you were, with this value" is
 * an answer and not a wish. Named restarts are frames the policy
 * can unwind TO — skip this element, use a default, retry — each
 * knowing how to continue from ITS place. The policy sees the
 * condition and the MENU of restarts on offer, and answers a
 * Decision; an unanswered condition throws, naming both.
 *
 * Additive by the operator's rule: `Throws` and the damage-as-data
 * readers stay exactly what they are; a program that never signals
 * never pays. Resumption itself is not new machinery — it is what
 * every effect operation already does (the handler answers, the
 * continuation continues); what this file adds is the RESTART
 * frame: a region whose escape is an error raised where the
 * restart was taken and caught by the frame's own guard (the
 * kernel's `recover`), the menu a state of the one handler.
 */
object Condition {

  /** what the policy answers */
  enum Decision:
    case Resume(value: Any)
    case Invoke(restart: String, value: Any)
    case Fail

  enum Op[+A] derives Effect:
    /** raise; the answer is what the policy resumed with, passed
     * through `accept` — the operation carries its own check of the
     * policy's untyped Resume (a ClassTag test), so the machine hands
     * the continuation an A without claiming anything */
    case Signal[A](condition: Any, accept: Any => A) extends Op[A]
    /** a frame entered: on the menu from here, innermost first */
    case Enter(frame: Condition.Frame) extends Op[Unit]
    /** a frame left, by any road: it and every frame inside it off the menu */
    case Exit(id: AnyRef) extends Op[Unit]
    /** the LEXICAL invoke (condition-caps): unwind to the handle's
     * OWN frame — by identity, not by name, so two frames sharing
     * a name cannot alias (a policy Invoke stays by name: innermost
     * wins, that is the dynamic menu's contract). Only a Restart
     * can construct it, and only a frame hands one out */
    case Leave[V](handle: Restart[V], value: V) extends Op[Nothing]

  /** the condition had no answer: the policy declined (Fail), and
   * the menu it declined is part of the report */
  final case class Unhandled(condition: Any, menu: Vector[String])
    extends RuntimeException(
      s"unhandled condition: $condition (restarts on offer: ${
        if menu.isEmpty then "none" else menu.mkString(", ")})")

  /** the policy invoked a restart that is not on the menu — a bug
   * in the policy, named as such */
  final case class NoSuchRestart(restart: String, menu: Vector[String])
    extends RuntimeException(
      s"no restart '$restart' on the menu (${
        if menu.isEmpty then "none" else menu.mkString(", ")})")

  /** raise a condition; resumes with the policy's value, checked
   * against A at the resume (BadResume names a wrong one): the
   * ClassTag is the evidence, so `signal[Int]` needs none written
   * and a generic caller passes its own */
  def signal[A](condition: Any)(using tag: scala.reflect.ClassTag[A]): A ! Op =
    effect(Op.Signal[A](condition, checked(condition, tag)))

  /** the policy's Any, admitted as an A or refused named */
  private def checked[A](condition: Any, tag: scala.reflect.ClassTag[A]): Any => A =
    case tag(a) => a
    case other => throw BadResume(condition, other, tag.runtimeClass.getName)

  /** the condition C is answered with A — declared once, where C
   * is; the ClassTag is the evidence the resume check stands on */
  trait Answers[C, A]:
    def tag: scala.reflect.ClassTag[A]
  object Answers:
    /** the usual way to declare one: Answers.of[HowMany, Int] */
    def of[C, A](using t: scala.reflect.ClassTag[A]): Answers[C, A] = new:
      def tag = t
    /** a condition that EXTENDS Of[A] has declared its answer already:
     * the instance is derived, raiseC takes it without a given */
    given fromOf: [A: scala.reflect.ClassTag, C <: Of[A]] => Answers[C, A] = of[C, A]

  /** a Resume whose value does not conform to the instance's answer
   * type: the POLICY's bug, caught where it acts, named */
  final class BadResume(condition: Any, value: Any, expected: String)
    extends RuntimeException(
      s"the policy resumed $condition with $value: not a $expected")

  /** the typed door (condition-typed): the answer type comes from
   * the Answers instance — no annotation at the site, and a
   * non-conforming Resume is refused named rather than cast */
  def raiseC[C, A](c: C)(using ev: Answers[C, A]): A ! Op =
    signal[A](c)(using ev.tag)

  /**
   * The capability a frame hands its body (condition-caps, the
   * ctx-prompts pattern): in-scope code invokes THIS restart
   * lexically — a nonexistent restart is not a runtime miss but a
   * COMPILE error, because the only way to hold a Restart is to be
   * inside its frame (the constructor is private). The dynamic
   * policy menu stays the floor: signals still see every frame.
   */
  final class Restart[V] private[Condition] (private[Condition] val name: String):
    /** unwind to this frame with v — the body's remaining work is
     * abandoned, the frame answers recover(v) */
    def invoke[X](v: V): X ! Op =
      effect[Op, X](Op.Leave(this, v))   // Op[Nothing] is an Op[X]: the answer never comes

  /**
   * `within`, handing the body its restart as a CAPABILITY: the
   * typed twin — recover receives the V the handle's invoke was
   * given. (A policy Invoke on the same name still arrives as Any;
   * the frame is one frame either way.)
   */
  def frame[A, V, F[_]](name: String)
                       (body: Restart[V] ?=> A ! Op + F)
                       (recover: V => A)(using tag: scala.reflect.ClassTag[V]): A ! Op + F =
    val handle = Restart[V](name)
    region[A, F](Frame(name, handle), body(using handle), x => recover(checked(name, tag)(x)))

  /** establish a restart around `body`: if the policy invokes
   * `name` (with a value), the frame's result is `recover(value)`
   * and execution continues after the frame */
  def within[A, F[_]](name: String)(body: A ! Op + F)(recover: Any => A): A ! Op + F =
    region[A, F](Frame(name, new Object), body, recover)

  /** the frame on the menu around `body`; an escape to it (by its name or its identity) its `recover`, any other
   * error off the menu and on */
  private def region[A, F[_]](f: Frame, body: A ! Op + F, recover: Any => A): A ! Op + F =
    val enter: Unit ! Op + F = effect[Op, Unit](Op.Enter(f)).at[Op + F]
    val exit: Unit ! Op + F = effect[Op, Unit](Op.Exit(f.id)).at[Op + F]
    val guarded = Direct.tryProgram[A, Op + F](body.flatMap(a => Run.map(exit)(_ => a))(using Join.same)) {
      case e: Escape if e.target == f.name || (e.target eq f.id) => Run.map(exit)(_ => recover(e.value))
      case e => exit.flatMap[A, Op + F](_ => throw e)(using Join.same)
    }
    enter.flatMap(_ => guarded)(using Join.same)

  /** an established frame: its menu name and its identity */
  final case class Frame private[Condition] (name: String, id: AnyRef)

  /** an unwinding to a frame — its name (a policy's Invoke) or its identity (a Restart) — raised where the restart
   * was taken, caught by the frame's guard; no stack trace, it is control */
  private final class Escape(val target: AnyRef, val value: Any) extends RuntimeException(null, null, false, false)

  /**
   * The machine: interprets signals through the policy, keeps the
   * menu of frames (a `Stateful` handler, wherever `Op` is among the
   * program's effects). The menu accumulates inner-first; an Invoke
   * unwinds to the INNERMOST frame of that name.
   */
  def run[A, Es[_]](policy: (Any, Vector[String]) => Decision)
                   (prog: A ! Es)(using s: Split[Es, Op]): A ! s.Rest =
    prog.handle(handler(policy))

  /** the menu a state of the machine: a signal answered in place, or an escape raised where it was */
  private def handler(policy: (Any, Vector[String]) => Decision): Handler[Op, [A] =>> A] = new Handler[Op, [A] =>> A]:
    def kernel[T <: Row, A]: OneHandler[Op, T, A, A] = new Stateful[Op, T, List[Frame], A, A]:
      def initial = Nil
      def ret(menu: List[Frame], a: A) = okay.one.Cont.pure(a)
      def value[X](menu: List[Frame], op: Op[X]): X = op match
        case Op.Signal(c, accept) =>
          val names = menu.map(_.name).toVector
          policy(c, names) match
            case Decision.Resume(v) => accept(v)
            case Decision.Invoke(name, v) =>
              if !names.contains(name) then throw NoSuchRestart(name, names)
              throw Escape(name, v)
            case Decision.Fail => throw Unhandled(c, names)
        case Op.Leave(handle, v) =>
          // a handle that leaked out of its frame: the frame is gone
          if !menu.exists(_.id eq handle) then throw NoSuchRestart(handle.name, menu.map(_.name).toVector)
          throw Escape(handle, v)
        case Op.Enter(_) => ()
        case Op.Exit(_) => ()
      def next[X](menu: List[Frame], op: Op[X]): List[Frame] = op match
        case Op.Enter(f) => f :: menu
        case Op.Exit(id) => menu.dropWhile(_.id ne id).drop(1)
        case _ => menu

  /**
   * The direct-style door (specs/condition.md, Direct style): the
   * restart frame's body as a direct block — two lines, forwarding
   * to `within` over `direct`, per the door recipe of
   * docs/capabilities.md. The explicit `within` stays the floor.
   */
  inline def frame[A, F[_]](name: String)
    (inline body: DirectCtx[[X] =>> X ! Op + F] ?=> A)
    (recover: Any => A): A ! Op + F =
    within[A, F](name)(Direct.direct[[X] =>> X ! Op + F](body))(recover)

  /**
   * Typed signals, the NOMINAL spelling (specs/condition.md, Typed
   * signals): `object HowMany extends Of[Int]` declares the answer
   * type in the condition itself, and that declaration IS an Answers
   * instance (Answers.fromOf) — so `HowMany.signal` is `raiseC` with
   * the evidence derived, one typed door behind two spellings: the
   * checked resume of raiseC applies, and a wrong-typed `resume`
   * stops compiling. The machine stays erased (the header's
   * discipline); the Any floor stays the floor.
   */
  trait Of[A]

  extension [A: scala.reflect.ClassTag](c: Of[A])
    /** the typed signal edge: HowMany.signal : Int ! Op */
    def signal: A ! Op = raiseC[Of[A], A](c)

  /** the typed resume for policies: resume(c)(v) checks v against
   * c's answer type — `case c: HowMany.type => resume(c)(41)` */
  def resume[A](@scala.annotation.unused c: Of[A])(v: A): Decision = Decision.Resume(v)

  /** the Shift/Resource precedent: splitting a row on Op is a
   * total test — one class carries the whole signature */
}
