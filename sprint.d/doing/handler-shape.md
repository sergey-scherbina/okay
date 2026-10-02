- [ ] handler-shape — the operator, 2026-10-02 ("Да"): with the effect
      named, the macro picks per effect the type its cases are matched
      against, BEFORE they are typed. `F[Any]` if the effect has an
      operation whose answer is a field's type (`Put(k, v: V) extends KV[V,
      V]`) and none whose caller chooses the answer, `F[Answer]` otherwise.
      Through a `given` derived from F, in `apply`'s signature, with one
      implementation still. An effect with both kinds keeps `.poly`, refused
      by name. After state-get-update. (2026-10-02)
