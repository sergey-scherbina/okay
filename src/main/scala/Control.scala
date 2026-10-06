package okay

/**
 * THE CONTROL INTERFACE, common to every monad of delimited continuations: Danvy–Filinski's `shift` and `/`
 * (`reset` is `/ identity`) over a parameterised monad `M[A, S, R]`, `(A => S) => R`. The instances: `Cont`
 * (okay-freer: the freer tree read as continuations, data, stack-safe), `Func` (closures, the reference, not
 * stack-safe), and what a machine of its own brings (okay-cont). The interface is the core's; the data instance is
 * the module's, and the core says how it is a `Control` here, beside the closures'.
 */
trait Control[M[_, _, _]] extends ParaMonad[M]:
  def shift[A, S, R](f: (A => S) => R): M[A, S, R]
  extension [A, S, R](m: M[A, S, R])
    infix def /(k: A => S): R
  inline def reset[A, R](m: M[A, A, R]): R = m / identity

/** summons the instance at its precise type, so its inline operations resolve statically (Carette-Kiselyov-Shan staging) */
transparent inline def Control[M[_, _, _]]: Control[M] =
  compiletime.summonInline[Control[M]]


/** the stack-safe data instance */
given Control[Cont] with
  override inline def pure[A, R](a: A): A /> R = Cont.Pure(a)
  // the leaf, not the macro: a macro expanded here cycles with ContMacro
  override inline def shift[A, S, R](f: (A => S) => R): Cont[A, S, R] = Cont.shiftLeaf(f)
  extension [A, S, R](m: Cont[A, S, R])
    // prefix form: `m / k` here would resolve to this override
    override inline infix def /(k: A => S): R = Cont.run(m)(k)
    override inline def flatMap[B, S2](f: A => Cont[B, S2, S]): Cont[B, S2, R] =
      Cont.bind(m)(f)
    // overridden: the default `map` builds a `pure` per element
    override inline def map[B](f: A => B): Cont[B, S, R] = Cont.mapped(m)(f)

/** closures: the reference instance, fast, not stack-safe */
type Func[A, S, R] = (A => S) => R

given Control[Func] with
  override inline def pure[A, R](a: A): Func[A, R, R] = _(a)
  override inline def shift[A, S, R](f: (A => S) => R): Func[A, S, R] = f
  extension [A, S, R](m: Func[A, S, R])
    override inline infix def /(k: A => S): R = m(k)
    override inline def flatMap[B, S2](f: A => Func[B, S2, S]): Func[B, S2, R] =
      k => m(f(_)(k))
    // composed directly, no `pure` per element
    override inline def map[B](f: A => B): Func[B, S, R] = k => m(x => k(f(x)))
