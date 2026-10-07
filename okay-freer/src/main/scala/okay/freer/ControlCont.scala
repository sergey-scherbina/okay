package okay.freer

import okay.{Control}

/** the stack-safe data instance */
given Control[Cps] with
  override inline def pure[A, R](a: A): A />> R = Cps.Pure(a)
  // the leaf, not the macro: a macro expanded here cycles with ContMacro
  override inline def shift[A, S, R](f: (A => S) => R): Cps[A, S, R] = Cps.shiftLeaf(f)
  extension [A, S, R](m: Cps[A, S, R])
    // prefix form: `m / k` here would resolve to this override
    override inline infix def /(k: A => S): R = Cps.run(m)(k)
    override inline def flatMap[B, S2](f: A => Cps[B, S2, S]): Cps[B, S2, R] =
      Cps.bind(m)(f)
    // overridden: the default `map` builds a `pure` per element
    override inline def map[B](f: A => B): Cps[B, S, R] = Cps.mapped(m)(f)
  override def isAnswer[A, S](m: Cps[A, S, S]): Boolean = Cps.onAnswer(m)(_ => true)(false)
  override def answerOf[A, S](m: Cps[A, S, S]): A = Cps.onAnswer(m)(a => a)(throw IllegalStateException("not an answer"))

