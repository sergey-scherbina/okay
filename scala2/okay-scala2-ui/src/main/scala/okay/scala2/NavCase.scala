package okay.scala2

import okay.ui.{Event, Nav, Screen}

/**
 * okay-ui's `Nav`, AS A SCALA 2.13 CALLER MATCHES IT (one-bridge's follow-up, specs/scala2-facade.md).
 *
 * A 2.13 match on `okay.ui.Nav` makes scalac read every case's constructor, and `Nav.Run` holds a program —
 * `Event ! Async`, `Free` at its bridge `Unary`, a match type the 2.13 TASTy reader refuses (a crash with no
 * position). The screen is made here instead, in the facade, so okay-ui keeps its own shape: a 2.13 caller matches
 * this mirror, whose `Run` holds the program behind `NavProgram` — a reference to that class is not a reading of
 * its constructor (`ProgBody`'s rule). Building a `Nav` from 2.13 (`Nav.Stay(this)`) reads only that case and needs
 * no mirror.
 */
sealed abstract class NavCase

object NavCase {
  final case class Stay(s: Screen) extends NavCase
  final case class Push(next: Screen) extends NavCase
  case object Pop extends NavCase
  final case class To(s: Screen) extends NavCase
  /** stay on `s` and launch a program: the program out of 2.13's sight */
  final case class Run(program: NavProgram, s: Screen) extends NavCase
  final case class PopTo[A](key: Nav.Key[A], answer: A) extends NavCase

  /** the case a step decided, for a 2.13 match */
  def of(n: Nav): NavCase = n match {
    case Nav.Stay(s) => Stay(s)
    case Nav.Push(next) => Push(next)
    case Nav.Pop => Pop
    case Nav.To(s) => To(s)
    case Nav.Run(prog, s) => Run(new NavProgram(prog), s)
    case p: Nav.PopTo[a] => PopTo[a](p.key, p.answer)
  }
}

/** a `Nav.Run`'s program, held where a 2.13 caller never reads its type */
final class NavProgram private[scala2] (private[scala2] val program: okay.![Event, okay.Async]) extends AnyVal
