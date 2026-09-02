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
    case Within(name: String, id: AnyRef, body: Any, recover: Any => Any) extends Op[Any]
    /** the LEXICAL invoke (condition-caps): unwind to the handle's
     * OWN frame — by identity, not by name, so two frames sharing
     * a name cannot alias (a policy Invoke stays by name: innermost
     * wins, that is the dynamic menu's contract). Only a Restart
     * can construct it, and only a frame hands one out */
    case Leave(handle: Restart[?], value: Any) extends Op[Nothing]

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
      effect(Op.Leave(this, v)).asInstanceOf[X ! Op]

  /**
   * `within`, handing the body its restart as a CAPABILITY: the
   * typed twin — recover receives the V the handle's invoke was
   * given. (A policy Invoke on the same name still arrives as Any;
   * the frame is one frame either way.)
   */
  def frame[A, V, F[+_]](name: String)
                        (body: Restart[V] ?=> A ! (Op + F))
                        (recover: V => A): A ! (Op + F) =
    val handle = Restart[V](name)
    effect[Op + F, Any](Op.Within(name, handle, body(using handle), recover.asInstanceOf[Any => Any]))
      .asInstanceOf[A ! (Op + F)]

  /** establish a restart around `body`: if the policy invokes
   * `name` with v, this whole `within` answers `recover(v)` — the
   * body's remaining work is abandoned, everything outside
   * continues; a body that completes normally makes the frame
   * invisible */
  def within[A, F[+_]](name: String)(body: A ! (Op + F))(recover: Any => A): A ! (Op + F) =
    effect[Op + F, Any](Op.Within(name, new Object, body, recover.asInstanceOf[Any => Any]))
      .asInstanceOf[A ! (Op + F)]

  /** an established frame: its menu name and its identity */
  private final case class Frame(name: String, id: AnyRef)

  private enum Out[X]:
    case Done(x: X)
    /** target is a name (policy Invoke: innermost of that name) or
     * a frame identity (lexical invoke: exactly that frame) */
    case Escape(target: AnyRef, value: Any)

  /**
   * The machine: interprets signals through the policy, owns the
   * restart frames, forwards every other effect (the Resource.run
   * shape). The menu accumulates inner-first; an Invoke unwinds to
   * the INNERMOST frame of that name.
   */
  def run[A, F[+_]](policy: (Any, Vector[String]) => Decision)
                   (prog: A ! (Op + F)): A ! F = {
    // the Resume path is a WHILE loop, not a recursion (the
    // Resource.run shape): a decode loop signalling once per record
    // resumes a hundred thousand times in a row, and every one of
    // them would otherwise be a JVM frame. Frames (Within) nest by
    // lexical depth, which is bounded; forwarded effects suspend
    // under flatMap, so their recursion lives in closures.
    def loop[X](p0: X ! (Op + F), menu: List[Frame]): Out[X] ! F =
      val names = menu.map(_.name).toVector
      var p = p0
      while true do
        val (op, k): (Op[Any], Any => X ! (Op + F)) = (p.resume: @unchecked) match
          case Pure(x) => return pure(Out.Done(x.asInstanceOf[X]))
          case Effect(e) => <|>[Op, F](e) match
            case Left(op) => (op, (a: Any) => Free.Pure(a).asInstanceOf[X ! (Op + F)])
            case Right(f) => return Effect(f).map(x => Out.Done(x.asInstanceOf[X]))
          case Bind(Effect(e), k) => <|>[Op, F](e) match
            case Left(op) => (op, k.asInstanceOf[Any => X ! (Op + F)])
            case Right(f) =>
              val cont = k.asInstanceOf[Any => X ! (Op + F)]
              return Effect(f).flatMap(x => loop(cont(x), menu))
        op match
          case Op.Leave(handle, v) =>
            // a handle that leaked out of its frame: the frame is gone
            if !menu.exists(_.id eq handle) then throw NoSuchRestart(handle.name, names)
            return pure(Out.Escape(handle, v))
          case Op.Signal(c) =>
            policy(c, names) match
              case Decision.Resume(v) => p = k(v)
              case Decision.Invoke(name, v) =>
                if !names.contains(name) then throw NoSuchRestart(name, names)
                return pure(Out.Escape(name, v))
              case Decision.Fail => throw Unhandled(c, names)
          case Op.Within(name, id, body, recover) =>
            return loop(body.asInstanceOf[Any ! (Op + F)], Frame(name, id) :: menu).flatMap {
              case Out.Done(b) => loop(k(b), menu)
              case Out.Escape(t, v) if t == name || (t eq id) => loop(k(recover(v)), menu)
              case Out.Escape(t, v) => pure(Out.Escape(t, v)) // an outer frame's
            }
      throw IllegalStateException("unreachable")

    loop(prog, Nil).map {
      case Out.Done(a) => a
      case Out.Escape(t, _) =>
        // unreachable: both invokes are menu-checked at the operation
        throw IllegalStateException(s"restart '$t' escaped every frame")
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

  /** the Delim/Resource precedent: splitting a row on Op is a
   * total test — one class carries the whole signature */
  given TypeableK[Op] = typeableK(classOf[Op[?]])
}
