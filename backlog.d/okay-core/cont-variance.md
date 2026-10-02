- [ ] cont-variance — the FACADE's variance in `A` only. REWRITTEN
      2026-09-30 after freer-consumed-index: the base is
      `Freer[G[_, _, +_], S, R, +A]`, INVARIANT in both indexes (a
      consumed index needs them so, and `+R`'s two readers were
      replaced: `Cont.tailAt` with its `S <:< R` evidence, `noProgram` as
      a throwing `Delay`), covariant in the answer `A` alone.
      `opaque type Rep[A, S, R] = Freer[Shift, S, R, A]`
      (src/main/scala/Cont.scala) is invariant in `A` too, so a
      `Cont[Dog, S, R]` is not a `Cont[Animal, S, R]` and
      `if c then pure(1) else abort` does not lub on the value. THE
      LANE, one line and a compile: `Rep[+A, S, R]`, plus `type Cont`
      and `/>`; `^[A, R] = Cont[A, A, R]` stays invariant in A by
      itself. `+R` is NOT available any more (the index is invariant on
      the base, on purpose). Bounds: inference — the 200-odd `Cont.Pure`
      sites and TestCont are what compiling checks; `Control[M[_, _,
      _]]` takes a variant constructor; `okay.scala2.Cont` is an
      invariant wrapper and is unaffected.
