package okay

import scala.util.NotGiven

/**
 * THE ROW-MEMBERSHIP CRASH, MINIMISED (row-membership-crash,
 * 2026-09-17). Kept compiling so that the next Scala upgrade says, by
 * turning red or green, whether it is still there.
 *
 * `Row.In` is membership as a witness, and its inductive given
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
 * ── REPORTED AND DIAGNOSED UPSTREAM (2026-09-17).
 * **scala/scala3#27096** is the bug; **#27097** is a fix with tests,
 * waiting for review. It is not a 3.9 regression: 3.3.6 LTS, 3.7.3,
 * 3.9.0 and main all die the same way.
 *
 * THE CAUSE, because knowing it changes what you try next: two
 * definitions of "same" disagree. `orDominator` guards its merge with
 * `tycon1 =:= tycon2`, and for two UNINSTANTIATED type variables that
 * comparison returns true BY CONSTRAINING THEM to each other — the
 * question creates its own answer. `mergeRefinedOrApplied` then
 * compares the very same tycons by object identity (`tp1 == tp2`),
 * sees two different `TypeVar`s, and asserts. The fix makes the merge
 * accept a partner the current constraint has already equated, using
 * `frozen_=:=` so that asking cannot create the answer.
 *
 * WHAT THAT MEANS HERE: when the fix lands, the workaround below stops
 * being necessary and `In[F, G]` can be searched at an abstract row —
 * so this file is the thing to re-run on the next Scala upgrade, and
 * the three designs it lists are the ones worth revisiting then.
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
  val concrete: Row.In[State % Int, State % Int + Async] = summon

  /** and so is the identity */
  val self: Row.In[Async, Async] = summon

  /**
   * THE CRASH, one line, commented out because it does not fail — it
   * takes the COMPILER down, and a repository cannot hold a file that
   * cannot be compiled. Uncomment to check whether a new Scala still
   * dies here:
   *
   *     def crashes[F[+_], G[+_]](using Row.In[F, G]): Int = 0
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
