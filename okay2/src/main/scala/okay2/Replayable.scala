package okay2

import scala.annotation.implicitNotFound

/**
 * THE DISCIPLINE, AS A TYPE. A paused dialogue outlives its process
 * because the ANSWERS are written down and the place is re-derived by
 * running the program again over them (`Delim.replay`). That is exact
 * under one sentence:
 *
 *     EVERYTHING THE OUTSIDE WORLD TELLS THE PROGRAM
 *     ENTERS THROUGH `pause`.
 *
 * `Replayable[F]` is the sentence as a constraint: the row of a program
 * you intend to replay may only hold effects whose re-execution nobody
 * can observe. `State` and `Reader` are re-threaded from the same
 * answers, `Throws` raises the same error at the same place, `Delim` is
 * the machine doing the replaying. Absent, deliberately: `Async` and
 * anything reaching outside, `Writer` (replay tells it again —
 * measured, TestDelimPersist), `Resource` (replay acquires again),
 * `Once` (a fresh store per run).
 *
 * The Scala 3 core states this as subtyping into a union, because its
 * rows are unions the compiler cannot take apart. Here a row is a
 * nominal `F + G`, so the instance is the INDUCTIVE one the Scala 3
 * core could not write: both sides replayable, the union replayable.
 */
@implicitNotFound("this row holds an effect that REPLAY WOULD PERFORM AGAIN, so the program is not a pure function of its journal and a restart would not land where the first run stood.\nEverything a durable program is told by the outside world must enter through `pause`, whose answers the journal remembers.\nReplayable: Pure, State, Reader, Throws, Delim.  NOT replayable: Async, Writer, Resource, Once, anything that reaches outside.\nIf you mean to replay a program that breaks this on purpose (a test of the limit, a migration), say so: `Replayable.unchecked`.")
sealed trait Replayable[F <: Row]

object Replayable {
  private val ev: Replayable[Pure] = new Replayable[Pure] {}
  private def of[F <: Row]: Replayable[F] = ev.asInstanceOf[Replayable[F]]

  implicit val pure: Replayable[Pure] = of
  implicit def state[S]: Replayable[State[S]] = of
  implicit def reader[R]: Replayable[Reader[R]] = of
  implicit def throws[E]: Replayable[Throws[E]] = of
  implicit val delim: Replayable[Delim] = of
  /** a union is replayable when both sides are */
  implicit def union[F <: Row, G <: Row](implicit f: Replayable[F], g: Replayable[G]): Replayable[F + G] = {
    val _ = (f, g); of
  }

  /** DELIBERATELY replaying a program that breaks the discipline: a
   * test that measures what a breach costs, a migration that knowingly
   * re-runs. A method rather than an implicit, so it cannot be summoned
   * by accident, and its name is what a reviewer sees. */
  def unchecked[F <: Row]: Replayable[F] = of
}
