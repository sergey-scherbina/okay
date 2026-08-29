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

/** one of the given alternatives */
inline def choose[A](as: A*): A ! Choose = effect(Choose(as))

/** all the results of all the branches, forwarding the effects F */
def runChoice[A, F[+_]](a: A ! Choose + F): Seq[A] ! F =
  Effects[Free].handle[Choose, F, A, Seq[A]](a)(x => pure(Seq(x))):
    [X] => c => shift: k =>
      c.as.foldLeft(pure[F, Seq[A]](Seq.empty)): (acc, x) =>
        acc.flatMap(s => k(x).map(s ++ _))
