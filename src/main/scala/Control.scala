package okay

/**
 * THE CONTROL INTERFACE, common to every monad of delimited continuations: Danvy–Filinski's `shift` and `/`
 * (`reset` is `/ identity`) over a parameterised monad `M[A, S, R]`, `(A => S) => R`. The instances: `Cont`
 * (okay-freer: the freer tree read as continuations, data, stack-safe), `Func` (closures, the reference, not
 * stack-safe), and the machine's `Carrier` (okay-cont). The interface and every instance are the core's: the two
 * modules know nothing of each other, nor of this.
 */
trait Control[M[_, _, _]] extends ParaMonad[M]:
  def shift[A, S, R](f: (A => S) => R): M[A, S, R]
  extension [A, S, R](m: M[A, S, R])
    infix def /(k: A => S): R
  inline def reset[A, R](m: M[A, A, R]): R = m / identity
  /** is `m` an answer already — a `pure`, nothing captured? then `answerOf` has it, and a handler's loop goes on
   * with a tail call instead of a continuation (`Effects.handle`'s tail answer). Closures cannot tell: never */
  def isAnswer[A, S](m: M[A, S, S]): Boolean = false
  def answerOf[A, S](m: M[A, S, S]): A = throw IllegalStateException("not an answer: ask isAnswer first")

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
  override def isAnswer[A, S](m: Cont[A, S, S]): Boolean = Cont.onAnswer(m)(_ => true)(false)
  override def answerOf[A, S](m: Cont[A, S, S]): A = Cont.onAnswer(m)(a => a)(throw IllegalStateException("not an answer"))

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

/** THE MACHINE (okay-cont) as a Control: `shift` is `Shift0` at the top level, `/` the delimiter around a `Bind`, `flatMap` a `Bind`. The `k` a body
 * is given is STRICT — a run of its own, on the host stack (as `Func`'s is; the machine's own `k` is a
 * program, `okay.cont.shift0`) */
given Control[cont.Carrier] with
  def pure[A, R](a: A): cont.Carrier[A, R, R] = cont.Cont.Return(a)
  def shift[A, S, R](f: (A => S) => R): cont.Carrier[A, S, R] =
    cont.Cont.Shift0[EmptyTuple, EmptyTuple, EmptyTuple, S, R, A](k => cont.Cont.Return(f(a => cont.Machine.value(k(a)))))
  extension [A, S, R](m: cont.Carrier[A, S, R])
    infix def /(k: A => S): R = cont.Machine.value(cont.Cont.Reset[EmptyTuple, EmptyTuple, S, R](cont.Cont.Bind(m, (a: A) => cont.Cont.Return(k(a)))))
    def flatMap[B, S2](f: A => cont.Carrier[B, S2, S]): cont.Carrier[B, S2, R] = cont.Cont.Bind(m, f)
    override def map[B](f: A => B): cont.Carrier[B, S, R] = cont.Cont.Bind(m, (a: A) => cont.Cont.Return(f(a)))
  override def isAnswer[A, S](m: cont.Carrier[A, S, S]): Boolean = m match
    case cont.Cont.Return(_) => true
    case _ => false
  override def answerOf[A, S](m: cont.Carrier[A, S, S]): A = m match
    case cont.Cont.Return(a) => a
    case _ => throw IllegalStateException("not an answer")
