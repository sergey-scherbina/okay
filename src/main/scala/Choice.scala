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

/** all the results of all the branches, forwarding the effects F */
def runChoice[A, F[+_]](a: A ! Choose + F): Seq[A] ! F =
  Effects[Free].handle[Choose, F, A, Seq[A]](a)(x => pure(Seq(x))):
    [X] => c => shift: k =>
      c.as.foldLeft(pure[F, Seq[A]](Seq.empty)): (acc, x) =>
        acc.flatMap(s => k(x).map(s ++ _))
