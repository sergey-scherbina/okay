- [ ] cont-variance — `opaque type Rep[+A, -S, +R] = Free[Shift, A]`
      (src/main/scala/Cont.scala, the one line, plus `type Cont` and
      `/>`; `^[A, R] = Cont[A, A, R]` comes out invariant in A by
      itself). specs/freer-base.md's "Variance is out" refused it
      because the runner typed `k(a): R` from the GADT equality
      `S = R` that matching `Pure` gave; after cont-on-free that
      equality does not exist — the `Return` branch already goes
      through `pinned` — so variance no longer turns a checked line
      into a cast: the cast is there either way. The RHS is `Free[F[+_],
      +A]`, covariant in A, and S/R do not occur in it, so the opaque
      alias's variance check should pass. A HYPOTHESIS, proved by
      compiling, nothing else. What it buys: `Cont[Nothing, S, R]` —
      an abort written once fits every branch; `if c then pure(1)
      else abort` lubs without an annotation; a `Cont[Dog, S, R]`
      where a `Cont[Animal, S, R]` is expected. Honest note: no
      `shift[Nothing, …]` exists in the repository today, so this is
      API room, not a measured saving.
      Bounds: (1) inference — covariance lets dotty take a lub where
      today it errors; inside the companion `Rep` is transparent, so
      `Leaf`/`Reentry` are untouched, and the 200-odd `Cont.Pure` call
      sites plus TestCont are what compiling checks; (2) `Control[M[_,
      _, _]]` takes a variant constructor (an invariant higher-kinded
      parameter accepts any variance); (3) `Prog` is NOT touched — an
      upcast between indexes is a hole in typestate, and its spec
      says so; (4) `okay.scala2.Cont` is an invariant wrapper and is
      unaffected. Changes a type signature: the gate is the full
      `affected master staged`. Update the "Variance is out" decision
      in specs/freer-base.md to say what superseded it.
