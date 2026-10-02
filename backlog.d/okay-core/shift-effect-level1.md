- [ ] shift-effect-level1 — PRIORITY: LOW, the start of a machine per small
      `reset` (shift-effect-core, 2026-10-02, specs/shift-effect.md;
      operator: "Запиши в беклог", design first). 100 resets with a
      two-shot capture in each take 10.8 µs on `Shift % R`, against 7.3 µs
      for the probe's handler (a), which started no machine. That bounds
      the start at ~35 ns a reset. Two candidates, each to be MEASURED
      alone before either lands:
      (1) one delimiter instead of two. A `reset` that runs its own
      machine installs its prompt AND `Delim.run`'s barrier
      (`Stacked.bounded`); its own prompt can be the barrier, because a
      capture of another type that finds no delimiter still fails at the
      bottom. One install and one pop less a reset.
      (2) a fast path for a body with no capture in it (a `pure(v)`, or a
      tree with no Shift/Delim node): answer it without a machine.
      ((1) of the old list, nesting, landed as reset-nesting-room; (3), the
      type arguments, as shift-in-scope.)
      TRIGGER: a consumer that runs a `reset` per element of a stream, or a
      profile with `Frames.run`'s start in it.
