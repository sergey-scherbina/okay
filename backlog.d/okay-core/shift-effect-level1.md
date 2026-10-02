- [ ] shift-effect-level1 — what is left of `Shift % R` after it reached
      the core (shift-effect-core, 2026-10-02, specs/shift-effect.md).
      (1) nested resets of the SAME answer type start a machine each:
      3 000-10 000 deep, depending on the JIT. (2) the machine's start per
      small `reset`: `twoShot` 10.3 µs against the probe's handler (a) at
      7.29. ((3), the type arguments, landed as shift-in-scope.)
      TRIGGER: a consumer writing either shape in a loop, or the level-1
      docs page (effects-shift-reset's step 5).
