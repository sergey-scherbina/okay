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

  /** the Delim/Resource precedent: splitting a row on Op is a
   * total test — one class carries the whole signature */
  given TypeableK[Op] = typeableK(classOf[Op[?]])
}
