package okay.llm

import okay.*

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
 * generation; the unguarded path is untouched — and `TestCut` pins
 * that a passing stream gives the same answer as the unguarded run.
 *
 * ── WHAT THE GUARD COSTS, MEASURED (delim-guard-per-op, 2026-09-17),
 * because the sentence that stood here was wrong. It said the guard
 * "costs the prompt push, not the capture price". Half of that is
 * right and the half that matters is not.
 *
 * A passing stream really never captures, and the push really is
 * nothing — a thousand of them cost 28.4 µs, so one is about 0.03 µs.
 * But entering `Delim + Async` puts EVERY operation of the body
 * through the delimited-control machine, and that is per token:
 *
 *     writerTell            15.098 / 18.012 µs   N tells, no machine
 *     writerTellUnderDelim  30.376 / 40.847 µs   one push, same N,
 *                                                under the machine
 *                                    ratio 2.01x then 2.27x
 *
 * Two rounds at load 9–10; the absolutes moved 20–35% between them
 * and the ratio held. `DelimBenchmark.writerTellUnderDelim` is the
 * benchmark, added by that lane because neither of the two that
 * already existed measures this shape — `delimPushOnly` counts N
 * pushes and `delimGenerator` counts N captures, so the claim read
 * as measured while the numbers beside it answered other questions.
 *
 * SO THE DECISION IT INFORMS: a guard roughly DOUBLES the cost of
 * whatever runs inside it. For token streams that is usually far
 * below the model's own latency and worth paying; for a hot inner
 * loop it is not, and the boundary belongs around the smallest span
 * that needs it rather than the whole generation. The same applies
 * to any guard of this shape, `okay.ui.Scope` included.
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
  : Either[Violation, A] ! Writer % String + Async =
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
                 tokens: Unit ! Writer % String + Async)
                (check: (Int, String) => Option[Violation])
  : Unit ! (Writer % String + (Delim + Async)) =
    def go(src: Unit ! Writer % String + Async, i: Int)
    : Unit ! (Writer % String + (Delim + Async)) =
      !.widen[Either[Unit, (String, Unit ! Writer % String + Async)],
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

  /**
   * The boundary with an ambient prompt — and the evidence is
   * `Delim.Prompted`, not `Prompt` (delim-doors-are-prompted,
   * 2026-09-18). A `Prompt` is one line to make, so asking for one as
   * a GIVEN proves nothing: a `violation` outside any guard compiled
   * and then failed at runtime with `NoPrompt`. `Prompted`'s
   * constructor is private to `Delim`, so holding one means being
   * inside the guard that installed it. The explicit forms
   * (`guarded`, `cut`, `checked(p, …)`) are unaffected.
   */
  def guard[A](gen: Delim.Prompted[Either[Violation, A]] ?=> A ! (Writer % String + (Delim + Async)))
  : Either[Violation, A] ! Writer % String + Async =
    // `Delim.scope` is the only door that hands out the evidence —
    // its constructor is private to `Delim`, which is exactly what
    // makes the evidence worth asking for
    Delim.run(Delim.scope[Either[Violation, A], Writer % String + Async](gen.map(Right(_))))

  /** `checked` against the NEAREST guard — the prompt is ambient
   * (the ctx-prompts door; the explicit form stays) */
  def checked[A](tokens: Unit ! Writer % String + Async)
                (check: (Int, String) => Option[Violation])
                (using p: Delim.Prompted[Either[Violation, A]])
  : Unit ! (Writer % String + (Delim + Async)) =
    checked(p.prompt, tokens)(check)

  /** abort to the nearest guard — no prompt in hand */
  def violation[A, X](v: Violation)(using p: Delim.Prompted[Either[Violation, A]])
  : X ! (Writer % String + (Delim + Async)) =
    cut[A, X](p.prompt)(v)

  /** `checked`, prompt ambient */
  def watched[A](tokens: Unit ! Writer % String + Async)
                (check: (Int, String) => Option[Violation])
                (using p: Delim.Prompted[Either[Violation, A]])
  : Unit ! (Writer % String + (Delim + Async)) =
    checked[A](p.prompt, tokens)(check)

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
  def screened[A](tokens: Unit ! Writer % String + Async)
                 (check: (Int, String) => Option[Violation])
                 (using p: Delim.Prompted[Either[Violation, A]])
  : Unit ! Screened =
    type R = Writer % String + (Delim + Async)
    def emit(t: String): Unit ! Screened =
      effect[Screened, Unit](Writer(t))
    def go(src: Unit ! Writer % String + Async, i: Int): Unit ! Screened =
      !.widen[Either[Unit, (String, Unit ! Writer % String + Async)],
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
