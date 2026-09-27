- [ ] direct-loop-then-rest — PRIORITY: LOW (measured small). Found by
      direct-one-bind-steps (2026-09-27, TestDirectShape). The `direct`
      emitter writes one bind a step for straight-line code, branches and
      dropped values (0 left-nested heads in 12 steps). The exception is a
      `for x <- xs do !op` statement FOLLOWED by more of its block. The
      loop is right-nested (foreachLoop recurses), but the block binds the
      whole loop before the rest, so every iteration's head is
      `Bind(Bind(op, loopK), rest)` and gets rotated (3 of 4 steps in the
      pinned case). The fix: pass the rest into `foreachLoop` as its base
      case in place of `M.pure(())`, the way direct-tail-fusion already
      threads a tail into loop BODIES (`compileTail`). That is macro code
      (quotes, owners). left-nested-build-cost measured the shape at
      ≤1.1x time and 17-27% memory, so this waits for a consumer whose
      loop-then-rest block is hot.
