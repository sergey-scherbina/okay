package okay

import scala.annotation.implicitNotFound

/**
 * THE DISCIPLINE, AS A TYPE (dialogue-replay-discipline, stage 1 of
 * specs/durable-workflow.md).
 *
 * A paused program outlives its process because the ANSWERS are
 * written down and the place is re-derived by running the program
 * again over them. That is exact under one sentence:
 *
 *     EVERYTHING THE OUTSIDE WORLD TELLS THE PROGRAM
 *     ENTERS THROUGH `pause`.
 *
 * Until now that sentence was in a document, and `TestDelimPersist`
 * measured what breaking it does: a `Writer` between two pauses says
 * the same thing again on every replay, and a clock read between two
 * pauses reads a different time. `Replayable[F]` is the sentence as a
 * constraint — the row of a program you intend to REPLAY may only
 * hold effects whose re-execution nobody can observe.
 *
 * WHAT IS IN, AND WHY EACH: `State` and `Reader` are re-threaded from
 * the same answers, so a replay produces the same values; `Throws`
 * raises the same error at the same place; `Delim` is the machine
 * doing the replaying. Absent, deliberately: `Async` and anything
 * reaching outside (replay performs it again), `Writer` (replay tells
 * it again — measured), `Resource` (replay acquires again), `Uid`
 * (a fresh id per run is the definition of not replayable).
 *
 * HOW IT IS SPELT, and this is the part that was not obvious. The
 * natural form — an inductive instance over the row,
 * `given union[F, G](using Replayable[F], Replayable[G]):
 * Replayable[F + G]` — DOES NOT RESOLVE. `F + G` is
 * `[A] =>> F[A] | G[A]`, and matching a concrete row against it asks
 * the compiler to invert a union into halves; it leaves both
 * unsolved and reports every instance as ambiguous for both. What
 * works is the same trick that closed the second-machine hole in
 * `Delim.OneMachine`: state the property as SUBTYPING with the
 * concrete row on the LEFT. `A | B <: C | D` decomposes the left
 * side, which the compiler does happily — and it keeps the whole
 * question out of `orDominator`, where `Row.In` over an abstract
 * row crashes dotty 3.9 outright.
 *
 * The consequence worth knowing: an ABSTRACT row is not proved and
 * not refuted, it PROPAGATES — a helper written over `F[+_]` takes
 * the obligation and hands it to its caller, where the row is
 * usually concrete. That is the same behaviour `OneMachine` has and
 * the same reason.
 */
@implicitNotFound("""this row holds an effect that REPLAY WOULD PERFORM AGAIN, so the program is not a pure function of its journal and a restart would not land where the first run stood.
Everything a durable program is told by the outside world must enter through `pause`, whose answers the journal remembers.
Replayable: Pure, State, Reader, Throws, Delim.  NOT replayable: Async, Writer, Resource, Uid, anything that reaches outside.
If you mean to replay a program that breaks this on purpose (a test of the limit, a migration), say so: `Replayable.unchecked`.""")
sealed trait Replayable[F[+_]]

object Replayable:

  /** the operations a replay may re-run without anybody noticing */
  type Safe = Delim[Any] | State[?, Any] | Reader[?, Any] | Throws[?, Any]

  private val ev: Replayable[Nothing] = new Replayable[Nothing] {}
  private def of[F[+_]]: Replayable[F] = ev.asInstanceOf[Replayable[F]]

  /** every operation the row can hold is one a replay may re-run */
  given replayable[F[+_]](using F[Any] <:< Safe): Replayable[F] = of

  /**
   * DELIBERATELY REPLAYING A PROGRAM THAT BREAKS THE DISCIPLINE.
   *
   * It exists because two honest uses do: a test that MEASURES what
   * a breach costs (`TestDelimPersist` watches a `Writer` log say the
   * same thing twice across two runs, and that test is the reason
   * anybody believes the rule), and a migration that knowingly
   * re-runs. It is a method rather than a given so that it cannot be
   * summoned by accident, and its name is what a reviewer sees.
   */
  def unchecked[F[+_]]: Replayable[F] = of
