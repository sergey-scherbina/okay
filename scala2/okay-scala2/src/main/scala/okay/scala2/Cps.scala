package okay.scala2

/**
 * The continuation paramonad for Scala 2.13 (specs/scala2-facade.md,
 * stage 2): `Cps[A, S, R]` delivers an `A` into a continuation
 * answering `S`, and answers `R` itself — so `shift` may CHANGE the
 * answer type (Danvy & Filinski's answer-type modification).
 *
 * It is okay's own `Cps` underneath, so it is stack-safe for the same
 * reason; this class only gives it a signature Scala 2 can read. The
 * Scala 3 one is an opaque type with extension methods, and scalac
 * 2.13's TASTy reader can call neither. The Scala 3 `Cps` is spelled
 * `okay.freer.Cps` in this file: this package's own names win inside it.
 */
final class Cps[A, S, R] private (private val body: ContBody[A, S, R]) {

  def flatMap[B, S2](f: A => Cps[B, S2, S]): Cps[B, S2, R] =
    Cps.of(body.c.flatMap(a => f(a).body.c))

  def map[B](f: A => B): Cps[B, S, R] = Cps.of(body.c.map(f))

  /** feed the continuation `k` and answer */
  def run(k: A => S): R = body.c / k
}

/** the Scala 3 program, held out of `Cps`'s constructor for the
 * reason `ProgBody` gives in Prog.scala */
private[scala2] final class ContBody[A, S, R](val c: okay.freer.Cps[A, S, R]) extends AnyVal

object Cps {

  private[scala2] def of[A, S, R](c: okay.freer.Cps[A, S, R]): Cps[A, S, R] = new Cps(new ContBody(c))

  def pure[A, R](a: A): Cps[A, R, R] = of(okay.freer.Cps.Pure(a))

  /** capture the continuation up to the nearest `reset` */
  def shift[A, S, R](f: (A => S) => R): Cps[A, S, R] = of(okay.freer.Cps.shift(f))

  /** delimit: run with the identity continuation */
  def reset[A, R](c: Cps[A, A, R]): R = c.run(identity)
}
