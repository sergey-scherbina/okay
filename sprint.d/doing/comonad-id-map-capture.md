- [ ] comonad-id-map-capture — PRIORITY: HIGH. `okay.given`'s `Comonad[Id]`
      (Monad.scala) carries `extension [A](a: A) def map`, which puts
      a lexical `.map` on EVERY type; in package `okay` and in any
      file with `import okay.given` it is closer than a facade's
      companion extension and types the lambda's argument as the
      receiver itself. It has now bitten three facades — `Static`
      became a class for it (lexical-extension-beats-companion), `Cont`
      and `Prog` document an `import okay.Prog.{flatMap, map}` beside
      the given — and its own comment admits it "hijacked kyo's .map in
      benchmarks". THE LANE: take `map` off the universal instance —
      keep `Comonad[Id]`'s `extract`/`coflatMap`, and give its `fmap`
      no extension (a `Functor[Id]` user calls `summon[Functor[Id]].
      fmap`), or move the instance out of `okay.given` into an
      explicit import; then delete the three workarounds' imports and
      the `Static` note, gated across the family (the throws-union
      `.map` contest is the other known casualty). A usability defect
      met four times is not a documentation item. (2026-09-23)
