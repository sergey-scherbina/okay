- [x] classic-package — DECIDED (the operator, 2026-10-07: "Классика
      переезжает в okay.freer") and DONE in lane classic-to-freer
      (specs/freer-min.md, stage 45): the classic Free-tree library is
      module `okay-freer`, package `okay.freer`, ABOVE the core; the core
      `okay` is the interface (Effects, Control, Answers, Monad, Prog) on
      okay-cont alone; `Classic[M]` the classic's typeclass, `Classic`/`!`
      its toolkit; every satellite takes `import okay.freer.*`. Next lane
      effects-rows: the interface over nominal rows, `A ! R` the machine's
      program in the core (bang-is-cont), the classic an instance through
      `Union[R, *]`.
