- [ ] effects-foldmap — `p.foldMap(nt)` into any okay `Monad` G, derived
      from `foldCont` beside `runWith` (operator ask, 2026-10-02): the one
      fold programs lacked that `Static` and `Proc` have. Stack-safe when
      G's flatMap defers; an eager G's depth is the operation count,
      written down. Tests: core (lazy and eager G), okay-cats (into IO).
