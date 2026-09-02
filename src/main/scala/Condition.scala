package okay

import okay.!.*

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
 * frame, in the Delim discipline: an operation's payload is a
 * program in the same row, erased at the operation and re-typed
 * inside the one machine that owns the frames and the menu.
 */
object Condition {

  /** what the policy answers */
  enum Decision:
    case Resume(value: Any)
    case Invoke(restart: String, value: Any)
    case Fail

  enum Op[+A]:
    /** raise; the answer is what the policy resumed with */
    case Signal(condition: Any) extends Op[Any]
    /** the payload is a program in the same row — the Delim
     * discipline: erased here, re-typed inside the machine */
    case Within(name: String, body: Any, recover: Any => Any) extends Op[Any]
    /** the LEXICAL invoke (condition-caps): unwind to the named
     * frame directly, no policy round-trip — only a Restart handle
     * can construct it, and only a frame hands one out */
    case Leave(name: String, value: Any) extends Op[Nothing]

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
      s"no restart '$restart' on the menu (${menu.mkString(", ")})")

  /** raise a condition; resumes with the policy's value */
  def signal[A](condition: Any): A ! Op =
    effect(Op.Signal(condition)).asInstanceOf[A ! Op]

  /** the condition C is answered with A — declared once, where C
   * is; the ClassTag is the evidence the resume check stands on */
  trait Answers[C, A]:
    def tag: scala.reflect.ClassTag[A]
  object Answers:
    /** the usual way to declare one: Answers.of[HowMany, Int] */
    def of[C, A](using t: scala.reflect.ClassTag[A]): Answers[C, A] = new:
      def tag = t

  /** a Resume whose value does not conform to the instance's answer
   * type: the POLICY's bug, caught where it acts, named */
  final class BadResume(condition: Any, value: Any, expected: String)
    extends RuntimeException(
      s"the policy resumed $condition with $value: not a $expected")

  /** the typed door (condition-typed): the answer type comes from
   * the Answers instance — no annotation at the site, and a
   * non-conforming Resume is refused named rather than cast */
  def raiseC[C, A](c: C)(using ev: Answers[C, A]): A ! Op =
    signal[Any](c).map {
      case ev.tag(a) => a
      case other => throw BadResume(c, other, ev.tag.runtimeClass.getName)
    }

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
      effect(Op.Leave(name, v)).asInstanceOf[X ! Op]

  /**
   * `within`, handing the body its restart as a CAPABILITY: the
   * typed twin — recover receives the V the handle's invoke was
   * given. (A policy Invoke on the same name still arrives as Any;
   * the frame is one frame either way.)
   */
  def frame[A, V, F[+_]](name: String)
                        (body: Restart[V] ?=> A ! (Op + F))
                        (recover: V => A): A ! (Op + F) =
    within[A, F](name)(body(using Restart[V](name)))(v => recover(v.asInstanceOf[V]))

  /** establish a restart around `body`: if the policy invokes
   * `name` with v, this whole `within` answers `recover(v)` — the
   * body's remaining work is abandoned, everything outside
   * continues; a body that completes normally makes the frame
   * invisible */
  def within[A, F[+_]](name: String)(body: A ! (Op + F))(recover: Any => A): A ! (Op + F) =
    effect[Op + F, Any](Op.Within(name, body, recover.asInstanceOf[Any => Any]))
      .asInstanceOf[A ! (Op + F)]

  private enum Out[X]:
    case Done(x: X)
    case Escape(name: String, value: Any)

  /**
   * The machine: interprets signals through the policy, owns the
   * restart frames, forwards every other effect (the Resource.run
   * shape). The menu accumulates inner-first; an Invoke unwinds to
   * the INNERMOST frame of that name.
   */
  def run[A, F[+_]](policy: (Any, Vector[String]) => Decision)
                   (prog: A ! (Op + F)): A ! F = {
    def loop[X](p: X ! (Op + F), menu: List[String]): Out[X] ! F =
      (p.resume: @unchecked) match
        case Pure(x) => pure(Out.Done(x.asInstanceOf[X]))
        case Effect(e) => <|>[Op, F](e) match
          case Left(op) => handle(op, (a: Any) => Free.Pure(a).asInstanceOf[X ! (Op + F)], menu)
          case Right(f) => Effect(f).map(x => Out.Done(x.asInstanceOf[X]))
        case Bind(Effect(e), k) => <|>[Op, F](e) match
          case Left(op) => handle(op, k.asInstanceOf[Any => X ! (Op + F)], menu)
          case Right(f) => Effect(f).flatMap(x => loop(k(x), menu))

    def handle[X](op: Op[?], k: Any => X ! (Op + F), menu: List[String]): Out[X] ! F =
      op match
        case Op.Leave(name, v) =>
          if !menu.contains(name) then throw NoSuchRestart(name, menu.toVector)
          pure(Out.Escape(name, v))
        case Op.Signal(c) =>
          policy(c, menu.toVector) match
            case Decision.Resume(v) => loop(k(v), menu)
            case Decision.Invoke(name, v) =>
              if !menu.contains(name) then throw NoSuchRestart(name, menu.toVector)
              pure(Out.Escape(name, v))
            case Decision.Fail => throw Unhandled(c, menu.toVector)
        case Op.Within(name, body, recover) =>
          loop(body.asInstanceOf[Any ! (Op + F)], name :: menu).flatMap {
            case Out.Done(b) => loop(k(b), menu)
            case Out.Escape(n, v) if n == name => loop(k(recover(v)), menu)
            case Out.Escape(n, v) => pure(Out.Escape(n, v)) // an outer frame's
          }

    loop(prog, Nil).map {
      case Out.Done(a) => a
      case Out.Escape(n, _) =>
        // unreachable: Invoke is menu-checked at the signal
        throw IllegalStateException(s"restart '$n' escaped every frame")
    }
  }

  /**
   * The direct-style door (specs/condition.md, Direct style): the
   * restart frame's body as a direct block — two lines, forwarding
   * to `within` over `direct`, per the door recipe of
   * docs/capabilities.md. The explicit `within` stays the floor.
   */
  inline def frame[A, F[+_]](name: String)
    (inline body: Direct.DirectCtx[[X] =>> X ! (Op + F)] ?=> A)
    (recover: Any => A): A ! (Op + F) =
    within[A, F](name)(Direct.direct[[X] =>> X ! (Op + F)](body))(recover)

  /**
   * Typed signals (specs/condition.md, Typed signals): an ADDITIVE
   * edge vocabulary — a condition declaring its answer type gets a
   * typed signal and a typed resume, and a wrong-typed resume stops
   * compiling. The machine stays erased (the header's discipline);
   * the Any floor stays the floor.
   */
  trait Of[A]

  extension [A](c: Of[A])
    /** the typed signal edge: HowMany.signal : Int ! Op */
    def signal: A ! Op = Condition.signal[A](c)

  /** the typed resume for policies: resume(c)(v) checks v against
   * c's answer type — `case c: HowMany.type => resume(c)(41)` */
  def resume[A](c: Of[A])(v: A): Decision = Decision.Resume(v)

  /** the Delim/Resource precedent: splitting a row on Op is a
   * total test — one class carries the whole signature */
  given TypeableK[Op] = typeableK(classOf[Op[?]])
}
