package okay

/**
 * Extensible effects: THE INTERFACE (specs/freer-min.md, stage 45). `Effects[M]` is what every encoding of a
 * program implements — the classic freer tree (`okay.freer`, with its `!`, `pure`, `effect` and toolkit) and the
 * machine (`okay.cont`, `Prog` here) — over the carrier `C` it folds into, a `Control`. The core holds the
 * interface and what both encodings share: `Control`, `Answers`, `Interpr`, the type classes.
 */

/** the union of two signatures: F + G */
infix type +[F[+_], G[+_]] = [A] =>> F[A] | G[A]

/** the empty signature: a computation over it is pure, with nothing to perform; the zero of `+` */
type Pure[+A] = Nothing

/** fix the parameter of a binary signature: State % S, Throws % E */
infix type %[F[_, _], S] = F[S, *]

/**
 * A partial function, infix: `Request |=> Response ! Async`. The spelling is forced by `!`: an infix type's
 * precedence comes from its first character, and anything binding tighter than `!` (`~>`, `-?>`, `=?>`)
 * parses `A ~> B ! F` as `(A ~> B) ! F`. A union on the left binds first, so `Get | Post |=> Res` reads as
 * it looks.
 */
infix type |=>[A, B] = PartialFunction[A, B]

/** Final tagless interface of extensible effects: `M[F, A]` computes `A` performing the signature `F`. Its
 * meaning is its image in a continuation paramonad (`foldCont`), on which `run` and `handle` are founded — the
 * encoding's own CARRIER `C`, a `Control`: `Cont` for the tree encodings, so that a handler `F !> S` is what it
 * always was; a machine of its own may bring its own (specs/freer-min.md, stage 32). Code written over any
 * `M: Effects` speaks to the carrier through `control` — `control.shift`, `control./` — and never names `Cont` */
trait Effects[M[_[+_], _]]:
  /** the continuation carrier `foldCont` folds into: `(A => S) => R` as the encoding has it */
  type C[_, _, _]
  /** the carrier's `shift` and `/` */
  def control: Control[C]

  def pure[F[+_], A](a: A): M[F, A]
  def perform[F[+_], A](e: F[A]): M[F, A]
  /** a bind whose left side is deferred, forced only when the encoding's interpreter reaches it, so that
   * mutually recursive functions returning `M[F, A]` call each other in tail position with no JVM frame each */
  def defer[F[+_], A, B](thunk: () => M[F, A])(f: A => M[F, B]): M[F, B]
  /** a tail call to a mutually recursive function, for code written over any `M: Effects` (`!.tailcall` on `Free`) */
  def tailcall[F[+_], A](thunk: => M[F, A]): M[F, A] = defer(() => thunk)(pure)

  extension [F[+_], A](m: M[F, A])
    def flatMap[B](f: A => M[F, B]): M[F, B]
    inline def map[B](f: A => B): M[F, B] = m.flatMap(a => pure(f(a)))
    /** `foldMap` into the carrier: the program's fold, each operation answered by
     * `h` as a continuation (`Static.foldMap` is the same fold into any
     * `Selective`). The result is still waiting for its LAST continuation:
     * `/ identity` when `S` is the answer (`runWith`), `/ ret` to finish
     * into `S` (`handle`). TestFoldCont and docs/contract.md show three `S`.
     * At `C = Cont` the handler is `F !> S` and the fold `A /> S` */
    def foldCont[S](h: Interpr[F, C, S]): C[A, S, S]
    /** run all the effects by a comonadic Answers (the foldCont definition; encodings may override with an equivalent fast path) */
    def runWith(using Answers[F]): A = control./(m.foldCont(interpr[C, F, A](using control, summon[Answers[F]])))(identity)
  /** handle the effect F by h (and the values by ret), forwarding the
   * effects G; for mass tail-resumption prefer !.relay (measured) */
  def handle[F[+_], G[+_]](using TypeableK[F])[A, B](m: M[F + G, A])
                          (ret: A => M[G, B])
                          (h: Interpr[F, C, M[G, B]]): M[G, B] =
    control./(m.foldCont[M[G, B]]([X] => e => split[F, G](e)(e => h(e))(e => control.shift(k => perform(e).flatMap(k)))))(ret)


/** the freer monad, the initial encoding: `Inject` is a suspended shift, given its meaning by `foldCont`.
 * Choose it when the program is a thing — to step, inspect or relay it — stack-safe on any bind shape */

object Effects {
  /** an encoding WITH ITS CARRIER NAMED: what an instance's given declares (`given Effects.Aux[Free, Cont]`), so
   * that `foldCont`'s handler type is concrete wherever the instance is reached by its type, not only by the
   * given's own object */
  type Aux[M[_[+_], _], C0[_, _, _]] = Effects[M] { type C = C0 }

  /** level 1, any encoding in direct style: `M[F, *]` as a monad, for `direct[[A] =>> M[F, A]]` over `Effects[M]` */
  def monad[M[_[+_], _], F[+_]](using E: Effects[M]): Monad[[A] =>> M[F, A]] = new Monad[[A] =>> M[F, A]]:
    def pure[A](a: A): M[F, A] = E.pure(a)
    extension [A](a: M[F, A])
      def flatMap[B](f: A => M[F, B]): M[F, B] = E.flatMap(a)(f)

  /** the staging entry for effect programs: `Effects[Free]`, `Effects[Eager]`, or any `M` with an instance
   * in scope; with `trait Effects` it forms one door, as a class and its companion do. Summoned WITH ITS
   * CARRIER: the pattern binds `c` to what the instance declares (`Effects.Aux`), so `Effects[Free].handle(…)(h)`
   * takes the handler at `Cont` — `summonInline[Effects[M]]` answered at `Effects[M]`, the carrier unknown — and
   * `summonFrom` still defers the search to where an inline program is expanded (`sprog[Free]`, TestEffects) */
  transparent inline def apply[M[_[+_], _]] =
    compiletime.summonFrom { case e: Effects.Aux[M, c] => e }

}
