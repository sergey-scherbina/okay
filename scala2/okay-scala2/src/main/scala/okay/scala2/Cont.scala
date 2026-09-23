package okay.scala2

/**
 * The continuation paramonad for Scala 2.13 (specs/scala2-facade.md,
 * stage 2): `Cont[A, S, R]` delivers an `A` into a continuation
 * answering `S`, and answers `R` itself — so `shift` may CHANGE the
 * answer type (Danvy & Filinski's answer-type modification).
 *
 * It is okay's own `Cont` underneath, so it is stack-safe for the same
 * reason; this class only gives it a signature Scala 2 can read. The
 * Scala 3 one is an opaque type with extension methods, and scalac
 * 2.13's TASTy reader can call neither. The Scala 3 `Cont` is spelled
 * `okay.Cont` in this file: this package's own names win inside it.
 */
final class Cont[A, S, R] private (private val body: ContBody[A, S, R]) {

  def flatMap[B, S2](f: A => Cont[B, S2, S]): Cont[B, S2, R] =
    Cont.of(body.c.flatMap(a => f(a).body.c))

  def map[B](f: A => B): Cont[B, S, R] = Cont.of(body.c.map(f))

  /** feed the continuation `k` and answer */
  def run(k: A => S): R = body.c / k
}

/** the Scala 3 program, held out of `Cont`'s constructor for the
 * reason `ProgBody` gives in Prog.scala */
private[scala2] final class ContBody[A, S, R](val c: okay.Cont[A, S, R]) extends AnyVal

object Cont {

  private[scala2] def of[A, S, R](c: okay.Cont[A, S, R]): Cont[A, S, R] = new Cont(new ContBody(c))

  def pure[A, R](a: A): Cont[A, R, R] = of(okay.Cont.Pure(a))

  /** capture the continuation up to the nearest `reset` */
  def shift[A, S, R](f: (A => S) => R): Cont[A, S, R] = of(okay.Cont.shift(f))

  /** delimit: run with the identity continuation */
  def reset[A, R](c: Cont[A, A, R]): R = c.run(identity)
}
