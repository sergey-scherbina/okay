- [ ] cont-state-cost — WHAT IS DONE (stage 42, 2026-10-07): the `Answer`
      arm feeds the next frame, the handler resolved once per capability —
      `stateAnswering` 17.6 µs against the classic's 18.2, `writerTell`
      12.5 against 27.7, `handlePrebuiltAnswering` 66.8. MEASURED NEXT (stage
      43): a row program `Free[Ask +: Tick +: Pure, Int]`, lanes
      `rowAnswering` 47.8 µs and `rowGeneral` 64.5 for 2000 operations —
      24 and 32 ns an operation against the context form's 6.7 and 16.5.
      The `Target` per operation is a fraction of the general path's extra
      8 ns; the 17 ns both pay is STRUCTURE: `flatMap` joins rows (`R ++
      R2`) and `run` splits the capabilities by the row's shape at every
      node (`Shape.split`, an `HCons` list rebuilt), one `Free` object a
      bind. THE DECISION, the operator's (taken up by effects-rows): a FIXED-ROW `Free` — `flatMap`
      at one row, one `Has` a run, `inject` polymorphic in the row by the
      expected type or an explicit argument as the classic's `effect[F, A]`
      is, `for` over mixed effects needing the program's row declared, as
      the classic does — then the `Target` cache on top if it still shows.
