- [ ] prog-focus-product-indices — PRIORITY: LOW (trigger). The
      operator asked whether `Prog` needs the optics `PState` has
      (2026-09-23). NOT the profunctor reading: `PState.zoom` is
      content because the state is a VALUE threaded through the answer
      type and a lens reads and writes it, while `Prog`'s indexes are
      phantoms — a `dimap` over them can only be `transition`, i.e. a
      cast dressed as an optic, and would let any move be claimed.
      Refused on that ground; do not build a phantom `zoom`. What IS
      content: PRODUCT indexes, Atkey's product of parameterised
      monads — when two typed protocols compose on one program (pg's
      connection `Closed -> Ready` beside `Tx`'s `Idle -> Open`), a
      step of one lifts to the tuple with the others unchanged:
      `Prog.focus[I]`: `Prog[F, A, S, R]` -> `Prog[F, A, Put[St, I, S],
      Put[St, I, R]]` for a tuple `St` of protocol states, with a
      match type `Put`/`Replace` and a membership witness — which
      `Delim.Stacked.Has[S <: Tuple, P]` already is for the prompt
      stack (the first structured index in the library). So the lane is
      a generalisation of `Has` from "is on the tuple" to "at which
      position, replace it", not a new optic. TRIGGER: the first module
      composing two typed protocols on one `Prog` — okay-pg's
      connection lifecycle with `Tx` is the candidate; until then `Tx`
      alone has nothing to focus. (2026-09-23)
