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
}
