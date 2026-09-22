- [ ] direct-staged — road 2, NOW WITH ITS NUMBER (staged-block-lanes,
      2026-09-22, specs/continuations-roadmap.md "Measured"): a static
      10-op block inside a recursive loop, the ceiling (binds AND
      handler static, each operation compiled to its shift arm) is
      **1.55x** over the fused Free fixture and **1.99x** over the
      shipping `State.run(Writer.run(_))` on the same tree, 82 968 vs
      123 712 B/op. The build threshold (1.5x) is cleared. But the lane
      beside it decides the DESIGN: the same block over Func with the
      handler passed as a value is **0.89x** — static binds buy
      nothing; the whole win is the handler being known per operation
      at compile time, so no `split` runs. So the emission target is
      NOT "`direct` over Control with an opaque `h`" (refuted) but
      "`direct` over Control with the handler inlined per operation":
      the macro must see, for each mark, the handler arm for that
      operation's class — an `inline` `Interpr` whose `split` reduces
      on a statically known operation, or per-effect inline arms the
      macro selects. Needs: (1) a library-side runner of
      `Fused.runCtrl`'s shape (`Fused` is a test fixture since
      fused-out-of-core); (2) a way to declare handlers the macro can
      read; (3) the stack-safety contract stated (Func is not
      stack-safe on a left-nested chain — the block is static, the loop
      is the user's recursion, and a 1M-iteration loop must still be
      a test). Parity target: `blockFuncStagedR` to the byte.
