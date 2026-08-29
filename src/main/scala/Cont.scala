package okay

import scala.annotation.tailrec

/**
 * Final tagless interface of delimited control: the parameterised
 * continuation monad (ParaMonad) with the shift operator of Danvy
 * and Filinski, with answer-type modification. M[A, S, R] means
 * (A => S) => R, which `/` (run) eliminates.
 */
trait Control[M[_, _, _]] extends ParaMonad[M]:
  def shift[A, S, R](f: (A => S) => R): M[A, S, R]
  extension [A, S, R](m: M[A, S, R])
    infix def /(k: A => S): R
  inline def reset[A, R](m: M[A, A, R]): R = m / identity

/**
 * Staging via final tagless (Carette–Kiselyov–Shan, the partial
 * evaluation half): in an `inline def` program, `val C = Control[M]`
 * summons the instance at its precise type, so the instance's inline
 * operations resolve statically and the tagless dispatch evaporates
 * at compile time — at the Func carrier the program partially
 * evaluates to plain nested closures.
 */
transparent inline def Control[M[_, _, _]]: Control[M] =
  compiletime.summonInline[Control[M]]

infix type />[A, R] = Cont[A, R, R]
infix type ^[A, R] = Cont[A, A, R]
inline def shift[A, S, R](f: (A => S) => R): Cont[A, S, R] = Cont.Shift(f)
inline def reset[A, R](c: A ^ R): R = c / identity

/**
 * The parameterised continuation monad, defunctionalized (cf. Free):
 * Cont[A, S, R] computes A and, applied by `/` to a continuation A => S,
 * makes an answer R, i.e. it means (A => S) => R.
 * Bind is a data node, and `/` rebalances the left-nested binds in a
 * tail-recursive loop, so running a flatMap chain is stack-safe.
 */
enum Cont[A, S, R] {
  case Pure[A, R](a: A) extends Cont[A, R, R]
  case Shift[A, S, R](f: (A => S) => R) extends Cont[A, S, R]
  private case Bind[A, B, S, T, R](a: Cont[A, T, R],
                                   f: A => Cont[B, S, T]) extends Cont[B, S, R]

  inline def flatMap[B, S2](f: A => Cont[B, S2, S]): Cont[B, S2, R] = Bind(this, f)
  inline def map[B](f: A => B): Cont[B, S, R] = flatMap(a => Pure(f(a)))

  /** apply to a continuation, as the function (A => S) => R it means */
  final def apply(k: A => S): R = this / k

  @tailrec final infix def /(k: A => S): R = this match
    case Pure(a) => k(a)
    case Shift(f) => f(k)
    case Bind(Bind(a, f), g) => Bind(a, f(_).flatMap(g)) / k
    case Bind(Pure(a), f) => f(a) / k
    case Bind(Shift(s), f) => s(f(_)(k))
}

given Control[Cont] with
  override inline def pure[A, R](a: A): A /> R = Cont.Pure(a)
  override inline def shift[A, S, R](f: (A => S) => R): Cont[A, S, R] = Cont.Shift(f)
  extension [A, S, R](m: Cont[A, S, R])
    override inline infix def /(k: A => S): R = m / k
    override inline def flatMap[B, S2](f: A => Cont[B, S2, S]): Cont[B, S2, R] =
      m.flatMap(f)

/**
 * The function encoding is the reference implementation of Control.
 * It is not stack-safe: flatMap nests closures (Cont is the safe one).
 * The choice mirrors Free vs Eff one level up: data for tools and
 * safety, functions for speed.
 */
type Func[A, S, R] = (A => S) => R

given Control[Func] with
  override inline def pure[A, R](a: A): Func[A, R, R] = _(a)
  override inline def shift[A, S, R](f: (A => S) => R): Func[A, S, R] = f
  extension [A, S, R](m: Func[A, S, R])
    override inline infix def /(k: A => S): R = m(k)
    override inline def flatMap[B, S2](f: A => Func[B, S2, S]): Func[B, S2, R] =
      k => m(f(_)(k))
