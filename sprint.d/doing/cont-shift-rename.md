- [ ] cont-shift-rename — level 1 step 1 (operator, 2026-10-02: "Да. Да.
      Делай"): the top-level `okay.shift`/`okay.reset` are Cont's and take
      the names level 1 needs for `Shift % R`. They become `Cont.shift` /
      `Cont.reset` (level 2), with no change in behaviour. Every call site
      moves (about 50 files outside okay2; okay2 is its own build and a
      later step), and so do the docs that quote them. Gate: `affected
      master staged`. specs/shift-effect.md. (2026-10-02)
