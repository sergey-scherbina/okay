- [ ] shift-prompt-key — specs/shift-merge.md stage 3: `Shift.Stacked` read
      as `Shift % p.type`: the prompt stack in the ROW (a `reset(p)` handles
      `Shift % p.type`; `shift0(p)`'s body is typed at the row outside it, so
      a shift to the consumed prompt does not type; an escaped prompt leaves
      `Shift % p.type` unhandled and `!.run` refuses it), replacing the
      tuple-indexed `Under`/`Has`; `Lexical.Stacked` and `Layered.Stacked`
      move with it. (2026-10-02, after shift-stacked-key)
