package okay

import scala.util.NotGiven

/**
 * THE ROW-MEMBERSHIP CRASH, MINIMISED (row-membership-crash,
 * 2026-09-17). Kept compiling so that the next Scala upgrade says, by
 * turning red or green, whether it is still there.
 *
 * `RowLift.In` is membership as a witness, and its inductive given
 * (`deeper[F, G, H](using In[F, G]): In[F, G + H]`) asks the compiler
 * to solve `?G + ?H =:= X` for the target. When X is a CONCRETE row
 * that is easy. When X is an ABSTRACT type constructor, dotty 3.9
 * dies:
 *
 *     java.lang.AssertionError: Failure to join alternatives F and G
 *       at dotty.tools.dotc.core.TypeOps$.orDominator(TypeOps.scala:403)
 *
 * It has decided three designs in this repository in one day:
 *
 *   delim-safety      `NotGiven[In[Delim, F]]` could not be used, so
 *                     the guard is `NotGiven[Delim[Any] <:< F[Any]]`
 *   dialogue-replay   `Replayable` is `F[Any] <:< Safe` for the same
 *                     reason, after the inductive form was refuted
 *   workflow-activity `In[F, G]` for the driver's row crashed, so the
 *                     row is written as the complement `F + E`
 *
 * THE RULE THAT FALLS OUT, and it is the practical one: an obligation
 * over a row is CARRIED AS A PARAMETER, never searched for at an
 * abstract row. `Delim.answer`, `replay` and `drive` all take their
 * `OneMachine` rather than summoning it, and that is why the core
 * compiles at all.
 */
object ProbeRowCrash:

  /** CONCRETE targets are fine — this is the everyday case and it
   * resolves */
  val concrete: RowLift.In[State % Int, State % Int + Async] = summon

  /** and so is the identity */
  val self: RowLift.In[Async, Async] = summon

  /**
   * THE CRASH, one line, commented out because it does not fail — it
   * takes the COMPILER down, and a repository cannot hold a file that
   * cannot be compiled. Uncomment to check whether a new Scala still
   * dies here:
   *
   *     def crashes[F[+_], G[+_]](using RowLift.In[F, G]): Int = 0
   *     val boom = crashes[Async, Async]
   *
   * (the definition alone is fine; it is the SEARCH at the call site
   * with both rows abstract that dies)
   */
  def documented: String = "see the comment above"

  /**
   * THE SHAPE THAT DOES NOT CRASH, and the one three lanes ended up
   * using: membership as SUBTYPING. A union on the right of a `<:<`
   * needs no join, so a concrete row resolves and an abstract one
   * simply FAILS — which is what a well-behaved implicit should do.
   */
  type Sub[F[+_], G[+_]] = F[Any] <:< G[Any]

  val subConcrete: Sub[State % Int, State % Int + Async] = summon
  val subSelf: Sub[Async, Async] = summon

  /** and the negative direction, which is what the guards use */
  val subAbsent: NotGiven[Sub[Async, State % Int]] = summon

  /**
   * WHAT `Sub` IS NOT. True membership is `∀X. F[X] <: G[X]`; this
   * tests it at `Any` only. For the rows this library builds — unions
   * of effect signatures applied pointwise — the two coincide, and
   * the cast it licenses is exactly the one `In` licenses. It is an
   * approximation, and saying so is the difference between a
   * shortcut and a lie.
   */
  def caveat: String = "tested at Any, not for all X"
