package okay

/**
 * The nondeterminism effect: choose one of several values, and let
 * the handler explore every branch. The handler is MULTI-SHOT — it
 * invokes the captured continuation once per alternative, which is
 * delimited continuations doing what neither a relay (exactly-once
 * by parametricity) nor an ordinary exception-style handler can.
 * Each nesting level of choose costs stack at run time.
 */
case class Choose[+A](as: Seq[A])

/** The class IS the whole identity: Choose has no parameter but its
 * (erased) answer type, so splitting a row on it is a TOTAL test —
 * said once here, rather than as a "cannot be checked at runtime"
 * warning at every use site of a test that is in fact complete. */
given TypeableK[Choose] = typeableK(classOf[Choose[?]])


/** one of the given alternatives */
inline def choose[A](as: A*): A ! Choose = effect(Choose(as))

/**
 * Nondeterminism is the canonical MonadPlus: no alternatives is
 * failure (the handler prunes the branch), append chooses between two
 * whole computations. Note the overlap: Monad[Free[F, *]] also covers
 * Choose programs — summon MonadPlus explicitly where empty/append
 * are needed.
 */
given MonadPlus[[A] =>> A ! Choose] with
  override def pure[A](a: A): A ! Choose = okay.pure(a)
  override def empty[A]: A ! Choose = effect(Choose(Seq.empty))
  extension [A](x: A ! Choose)
    override def flatMap[B](f: A => B ! Choose): B ! Choose = x.flatMap(f)
    override def append(y: A ! Choose): A ! Choose =
      effect[Choose, A ! Choose](Choose(Seq(x, y))).flatMap(identity)

/**
 * A row CONTAINING Choose is a MonadPlus too — which is what lets
 * `guard` prune inside an effectful search (the model is asked, the
 * answer is judged, the branch dies or lives).
 */
given [F[+_]]: MonadPlus[[A] =>> A ! (Choose + F)] with
  override def pure[A](a: A): A ! (Choose + F) = okay.pure(a)
  override def empty[A]: A ! (Choose + F) = effect(Choose(Seq.empty))
  extension [A](x: A ! (Choose + F))
    override def flatMap[B](f: A => B ! (Choose + F)): B ! (Choose + F) = x.flatMap(f)
    override def append(y: A ! (Choose + F)): A ! (Choose + F) =
      effect[Choose + F, A ! (Choose + F)](Choose(Seq(x, y))).flatMap(identity)

/**
 * A REFUTABLE PATTERN on the left of `<-`, which is the same thing as
 * failure wearing different syntax.
 *
 *   for case Some(n) <- choose(Some(1), None, Some(3)) yield n
 *
 * Scala desugars that into `withFilter` plus a total match, so the
 * question it asks is not about patterns at all: may a step be
 * DROPPED? A plain `A ! F` cannot drop one — nothing in `Free`
 * declines to answer — and it should not pretend to: a silently
 * skipped step is a bug that reads like a feature. Where `Choose` is
 * in the row, dropping already has a meaning (the branch dies), and
 * that is exactly what MonadPlus evidence says. So the pattern binds
 * precisely there, and everywhere else the compiler explains what the
 * row is missing.
 *
 * The `if` guard of a for-comprehension goes through the same method,
 * so it is `guard` under its usual syntax.
 */
extension [A, F[+_]](p: A ! F)(using M: MonadPlus[[X] =>> X ! F])
  def withFilter(q: A => Boolean): A ! F =
    p.flatMap(a => if q(a) then pure[F, A](a) else M.empty[A])

/** all the results of all the branches, forwarding the effects F */
def runChoice[A, F[+_]](a: A ! Choose + F): Seq[A] ! F =
  Effects[Free].handle[Choose, F, A, Seq[A]](a)(x => pure(Seq(x))):
    [X] => c => shift: k =>
      c.as.foldLeft(pure[F, Seq[A]](Seq.empty)): (acc, x) =>
        acc.flatMap(s => k(x).map(s ++ _))
