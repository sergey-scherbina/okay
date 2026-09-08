package okay

import scala.annotation.implicitNotFound
import okay.Rowlift.{In, at}

/**
 * A step a program may DECLINE — which is what a refutable pattern on
 * the left of `<-` asks for, and what an `if` guard asks for too.
 *
 *   for case Some(old) <- find(id)
 *       _              <- save(id, to)
 *   yield old
 *
 * Scala desugars both into `withFilter`, so neither is about patterns
 * or about booleans: the question each one asks is whether this
 * program may stop early. A plain `A ! F` may not — nothing in `Free`
 * declines to answer — and it must not pretend to, because a silently
 * skipped step is a bug that reads like a feature. So the witness
 * below is the whole design, and everything else follows from which
 * effect supplies it.
 *
 * TWO EFFECTS CAN FAIL, AND THEY MEAN DIFFERENT THINGS.
 *
 *   Choose   the BRANCH dies and the search goes on — `guard`, under
 *            the syntax people reach for first
 *   Abort    the PROGRAM stops and `runOption` answers None — a lookup
 *            with nothing to look up, which has no other branch to
 *            continue into
 *
 * A row carrying `Choose` gets the search meaning: where a program
 * searches, `guard` already means prune, and one syntax must not mean
 * two things in one row.
 *
 * WHY MEMBERSHIP AND NOT `MonadPlus`. The obvious evidence is
 * `MonadPlus[[X] =>> X ! F]`, and it was shipped that way first. It
 * does not work: the instance for `Choose + F` never matches a
 * concrete row, because unifying `[A] =>> Choose[A] | F[A]` against
 * `[A] =>> Choose[A] | (Writer % String)[A]` is a higher-order
 * unification the compiler declines — it finds the given and reports
 * "does not match". Measured, not assumed: the first version bound
 * patterns in the bare `Choose` row and nowhere else, which is the
 * row nobody writes.
 *
 * What DOES resolve is membership by shape, the witness `.at` already
 * uses. So the evidence for "this row may drop a step" is that the row
 * CONTAINS an effect that can fail, and nothing larger.
 */
@implicitNotFound("""this row cannot drop a step, so a refutable pattern (`case Some(x) <- p`) and an `if` guard have no meaning in it: ${F}
Both desugar to withFilter, which needs somewhere for the dropped step to GO.
Put a failing effect in the row and it works: Abort (the program stops, runOption answers None) or Choose (the branch dies, the search goes on).
Or keep the row as it is and write the branch yourself: `old.fold(pure(()))(...)` says the same thing and hides nothing""")
trait CanFail[F[+_]]:
  def fail[A]: A ! F

trait CanFailLow:
  /** stop: the rest of the program does not run */
  given viaAbort: [F[+_]] => In[Abort, F] => CanFail[F] = new:
    def fail[A]: A ! F = abort[A].at[F]

object CanFail extends CanFailLow:
  /** prune: this branch dies, the search continues — and it wins over
   * Abort where a row carries both, because in a searching row `guard`
   * already means prune */
  given viaChoose: [F[+_]] => In[Choose, F] => CanFail[F] = new:
    def fail[A]: A ! F = effect[Choose, A](Choose(Seq.empty)).at[F]

extension [A, F[+_]](p: A ! F)
  /** the desugaring target of a pattern bind and of an `if` guard */
  def withFilter(q: A => Boolean)(using C: CanFail[F]): A ! F =
    p.flatMap(a => if q(a) then pure[F, A](a) else C.fail[A])
