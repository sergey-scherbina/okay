package okay2

import scala.annotation.implicitNotFound
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

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
 * The Scala 3 core states this as subtyping into a union. Here a row
 * is an INTERSECTION (stage 8), and scalac 2 cannot take one apart by
 * implicit search: a rule `Replayable[F + G]` matches every type
 * (`S =:= S with S`), and the search diverges even at low priority —
 * measured on two parts, three and four. So the instance is DERIVED by
 * a blackbox macro that reads the row's parents and admits it when
 * every one is on the list below: a whitelist, as before, so a user's
 * own effect that reaches outside is refused too.
 */
@implicitNotFound("this row holds an effect that REPLAY WOULD PERFORM AGAIN, so the program is not a pure function of its journal and a restart would not land where the first run stood.\nEverything a durable program is told by the outside world must enter through `pause`, whose answers the journal remembers.\nReplayable: Pure, State, Reader, Throws, Delim.  NOT replayable: Async, Writer, Resource, Once, anything that reaches outside.\nIf you mean to replay a program that breaks this on purpose (a test of the limit, a migration), say so: `Replayable.unchecked`.")
sealed trait Replayable[F <: Row]

object Replayable {
  private val ev: Replayable[Pure] = new Replayable[Pure] {}
  private[okay2] def of[F <: Row]: Replayable[F] = ev.asInstanceOf[Replayable[F]]

  /** every signature of F is one replay cannot observe running twice */
  implicit def derive[F <: Row]: Replayable[F] = macro ReplayableMacro.derive[F]

  /** DELIBERATELY replaying a program that breaks the discipline: a
   * test that measures what a breach costs, a migration that knowingly
   * re-runs. A method rather than an implicit, so it cannot be summoned
   * by accident, and its name is what a reviewer sees. */
  def unchecked[F <: Row]: Replayable[F] = of
}

/** the derivation: flatten the row's intersection, test each parent's
 * class against the whitelist, abort with the discipline's message */
object ReplayableMacro {
  /** the signatures replay may run again unobserved — and `Row`, which
   * is `Pure`, the empty requirement */
  private val allowed = Set("okay2.State", "okay2.Reader", "okay2.Throws", "okay2.Delim", "okay2.Row")

  def derive[F: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._
    def parts(t: Type): List[Type] = t.dealias match {
      case RefinedType(ps, _) => ps.flatMap(parts)
      case other => List(other)
    }
    val row = weakTypeOf[F]
    val bad = parts(row).filterNot(p => allowed.contains(p.typeSymbol.fullName))
    if (bad.nonEmpty)
      c.abort(c.enclosingPosition,
        "this row holds an effect that REPLAY WOULD PERFORM AGAIN: " + bad.mkString(", ") +
          ".\nEverything a durable program is told by the outside world must enter through `pause`, whose answers the journal remembers." +
          "\nReplayable: Pure, State, Reader, Throws, Delim.  NOT replayable: Async, Writer, Resource, Once, anything that reaches outside." +
          "\nIf you mean to replay a program that breaks this on purpose (a test of the limit, a migration), say so: `Replayable.unchecked`.")
    q"_root_.okay2.Replayable.of[$row]"
  }
}

