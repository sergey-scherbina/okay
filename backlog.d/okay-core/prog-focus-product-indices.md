- [ ] prog-focus-product-indices — PRIORITY: LOW (trigger). REWRITTEN
      2026-09-30: `Prog` is deleted (indexed-effects stage 8); the
      question now belongs to the indexed tree. PRODUCT indexes,
      Atkey's product of parameterised monads: when two typed protocols
      compose on one program (okay-pg's connection `Closed -> Ready`
      beside `Tx`'s `Idle -> Open`), each signature's step must lift to
      a tuple index with the others unchanged — `focus[I]`:
      `Freer[G, S, R, A]` -> `Freer[G', Put[St, I, S], Put[St, I, R], A]`
      for a tuple `St` of protocol states, with a match type `Put` and
      a membership witness that says at WHICH position, which is
      `Delim.Stacked.Has[S <: Tuple, P]` generalised from "is on the
      tuple" to "at this position, replace it". A handler of one
      protocol then holds its own state and forwards the rest with the
      tuple (`State.handleIndexed`'s shape over `+~`). Unlike the old
      phantom facade, the indexes are on the nodes now, so a `zoom` is
      content here, not a disguised cast. TRIGGER: the first module
      composing two typed protocols on one program — okay-pg's
      connection lifecycle with `Tx`, or a resource's open/closed
      beside a transaction.
