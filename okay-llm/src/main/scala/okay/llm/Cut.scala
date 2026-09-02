package okay.llm

import okay.*
import okay.given

/**
 * Streaming validation that cuts generation (specs/llm-agentic.md,
 * llm-streaming-cut): a validator stands IN the token stream and, on
 * a violation, ABORTS TO A NAMED PROMPT installed over the
 * generation — the one thing specialised effects cannot do: a
 * non-local exit across the streaming boundary, no Option threading
 * on the stages between, no poisoned partial output flowing further.
 * The Scope precedent (okay-ui), applied to the model's own mouth.
 *
 * ADDITIVE per the adoption doctrine: `guarded` wraps a streaming
 * generation; the unguarded path is untouched. A passing stream
 * never captures — the guard costs the prompt push, not the capture
 * price.
 */
object Cut {

  /** the violation is a VALUE: the rule, the token position, what
   * was seen — the caller retries, reprompts, or surfaces it */
  final case class Violation(rule: String, at: Int, seen: String)

  /** the streaming row under a cut boundary */
  type Guarded[A] = Writer % String + (Delim + Async)

  /** install the boundary; the body streams tokens and may abort to
   * the prompt with a named violation */
  def guarded[A](gen: Prompt[Either[Violation, A]] => A ! (Writer % String + (Delim + Async)))
  : Either[Violation, A] ! (Writer % String + Async) =
    val p = Delim.prompt[Either[Violation, A]]
    Delim.run(Delim.push(p)(gen(p).map(Right(_))))

  /** abort the generation with a violation — the non-local exit */
  def cut[A, X](p: Prompt[Either[Violation, A]])(v: Violation)
  : X ! (Writer % String + (Delim + Async)) =
    Delim.abort[Either[Violation, A], X, Writer % String + Async](p)(Left(v))

  /**
   * The recurring shape: pull a token source, CHECK each token, emit
   * what passes, abort on the first violation — and after the abort
   * the source is never pulled again (the cut stops the pull, which
   * a scripted stream can observe).
   */
  def checked[A](p: Prompt[Either[Violation, A]],
                 tokens: Unit ! (Writer % String + Async))
                (check: (Int, String) => Option[Violation])
  : Unit ! (Writer % String + (Delim + Async)) =
    def go(src: Unit ! (Writer % String + Async), i: Int)
    : Unit ! (Writer % String + (Delim + Async)) =
      !.widen[Either[Unit, (String, Unit ! (Writer % String + Async))],
              Async, Writer % String + Delim](
        Writer.uncons[String, Unit, Async](src)).flatMap {
        case Left(_) => pure(())
        case Right((t, rest)) => check(i, t) match
          case Some(v) => cut[A, Unit](p)(v)
          case None =>
            effect[Writer % String + (Delim + Async), Unit](Writer(t))
              .flatMap(_ => go(rest, i + 1))
      }
    go(tokens, 0)

  // ── the capability door (specs/context-functions.md, ctx-prompts)
  // ADDITIVE: guarded/cut/checked stay. The prompt becomes ambient;
  // a validator holds no name, and nesting cuts to the NEAREST guard.

  /** the boundary with an ambient prompt */
  def guard[A](gen: Prompt[Either[Violation, A]] ?=> A ! (Writer % String + (Delim + Async)))
  : Either[Violation, A] ! (Writer % String + Async) =
    guarded[A](p => gen(using p))

  /** `checked` against the NEAREST guard — the prompt is ambient
   * (the ctx-prompts door; the explicit form stays) */
  def checked[A](tokens: Unit ! (Writer % String + Async))
                (check: (Int, String) => Option[Violation])
                (using p: Prompt[Either[Violation, A]])
  : Unit ! (Writer % String + (Delim + Async)) =
    checked(p, tokens)(check)

  /** abort to the nearest guard — no prompt in hand */
  def violation[A, X](v: Violation)(using p: Prompt[Either[Violation, A]])
  : X ! (Writer % String + (Delim + Async)) =
    cut[A, X](p)(v)

  /** `checked`, prompt ambient */
  def watched[A](tokens: Unit ! (Writer % String + Async))
                (check: (Int, String) => Option[Violation])
                (using p: Prompt[Either[Violation, A]])
  : Unit ! (Writer % String + (Delim + Async)) =
    checked[A](p, tokens)(check)

  // ── the repair door (specs/condition.md): between passing a token
  // and cutting the stream there is REPAIRING it. ADDITIVE:
  // checked/watched still cut hard; a stream that never violates
  // never signals.

  /** the screened row: conditions over the guarded row */
  type Screened = Condition.Op + (Writer % String + (Delim + Async))

  /**
   * `checked`, repairable: a violating token SIGNALS the Violation
   * instead of cutting, and the policy at `Condition.run` answers
   * per incident — `Resume(t: String)` emits `t` in the token's
   * place and the stream continues; `Invoke("drop", _)` makes the
   * token vanish; `Invoke("cut", v: Violation)` is the old hard cut
   * to the nearest guard. The menu at the signal is
   * `["drop", "cut"]` — mechanism in the stream, policy at the edge.
   */
  def screened[A](tokens: Unit ! (Writer % String + Async))
                 (check: (Int, String) => Option[Violation])
                 (using p: Prompt[Either[Violation, A]])
  : Unit ! Screened =
    type R = Writer % String + (Delim + Async)
    def emit(t: String): Unit ! Screened =
      effect[Screened, Unit](Writer(t))
    def go(src: Unit ! (Writer % String + Async), i: Int): Unit ! Screened =
      !.widen[Either[Unit, (String, Unit ! (Writer % String + Async))],
              Async, Condition.Op + (Writer % String + Delim)](
        Writer.uncons[String, Unit, Async](src)).flatMap {
        case Left(_) => pure(())
        case Right((t, rest)) => check(i, t) match
          case None => emit(t).flatMap(_ => go(rest, i + 1))
          case Some(v) =>
            Condition.within[Option[String], R]("drop")(
              !.widen[String, Condition.Op, R](Condition.signal[String](v))
                .map(Some(_)))(_ => None).flatMap {
              case Some(t2) => emit(t2).flatMap(_ => go(rest, i + 1))
              case None => go(rest, i + 1)
            }
      }
    // the typed frame: an Invoke("cut", v) reaches recover as a
    // Violation or is refused named (the ClassTag door), not cast
    Condition.frame[Option[Violation], Violation, R]("cut")(go(tokens, 0).map(_ => None))(
      v => Some(v)).flatMap {
      case Some(v) =>
        !.widen[Unit, Writer % String + (Delim + Async), Condition.Op](
          violation[A, Unit](v))
      case None => pure(())
    }
}
