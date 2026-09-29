- [ ] cont-variance — the FACADE's variance, now that the base has half
      of it (freer-base-step-extractor, 2026-09-29): `Freer[G, S, +R,
      +A]` is covariant in `A` and `R` (the Variance decision in
      specs/freer-base.md says why `+R` came, and why `S` stays
      invariant — contravariance would let a wrong-typed continuation
      into a bind by upcast, and the GADT runner types `k(a): S` as an
      `R` from `S <: R`, not from `S = R`). `opaque type Rep[A, S, R] =
      Freer[Shift, S, R, A]` (src/main/scala/Cont.scala) is still
      invariant in all three, so nothing outside the companion sees the
      base's variance: `Cont[Nothing, S, R]` does not fit every branch,
      `if c then pure(1) else abort` does not lub, a `Cont[Dog, S, R]`
      is not a `Cont[Animal, S, R]`. THE LANE, one line and a compile:
      `Rep[+A, S, +R]` (matching the RHS; `-S` is refused above), plus
      `type Cont` and `/>`; `^[A, R] = Cont[A, A, R]` comes out
      invariant in A by itself. Bounds: (1) inference — covariance lets
      dotty take a lub where today it errors; the 200-odd `Cont.Pure`
      call sites plus TestCont are what compiling checks; (2)
      `Control[M[_, _, _]]` takes a variant constructor; (3) `Prog` is
      NOT touched — an upcast between indexes is a hole in typestate;
      (4) `okay.scala2.Cont` is an invariant wrapper and is unaffected.
      Honest note: no `shift[Nothing, …]` exists in the repository
      today, so this is API room, not a measured saving. Changes a type
      signature: the gate is the full `affected master staged`.
