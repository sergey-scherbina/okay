- [ ] shift-effect-typed — the probe's second round (operator,
      2026-10-02: "Добавляй. А если shift'ы будут разных типов?").
      (1) `shift` with Danvy-Filinski's body, under its `reset` (`R !
      Shift % R + F`), beside `shift0` (the body `R ! F`, the first round's);
      (2) captures of different answer types in one program: each `Shift`
      carries a key of its answer type, made at compile time, its
      `TypeableK` `ByValue`, so `Distinct` passes `Shift % Int + Shift %
      String` and each `reset` takes only its own; (3) on (b) a `reset`
      whose row still holds a `Shift` pushes its prompt on the machine
      that is running instead of starting a second one. Both
      implementations, one suite. specs/shift-effect.md. (2026-10-02)
