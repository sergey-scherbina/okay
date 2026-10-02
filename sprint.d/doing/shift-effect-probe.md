- [ ] shift-effect-probe — level 1 of the API (operator, 2026-10-02): the
      user knows `A ! F`, `pure`, `perform`, `shift`, `reset`, `handle`
      and nothing else, so a continuation is an EFFECT in the row,
      `Shift[R]`, and `reset` is its handler. Probe, additive, no core
      change: (a) `Shift[R]` as an ordinary effect with a deep handler over
      `!.handle`, (b) the same API on Delim's machine; one suite on both
      (shift/reset laws, with and without State, multi-shot with Choose,
      nested resets of different and equal answer types, direct style,
      depth 100 000); level 2's `Cont[A, S ! F, R ! F]` beside it —
      `.cont`/`.!`/`lift`, the round trip on the diagonal, one ATM example
      with an effect. Then a JMH lane each: (a) vs (b) vs Cont/Delim.
      specs/shift-effect.md. (2026-10-02)
