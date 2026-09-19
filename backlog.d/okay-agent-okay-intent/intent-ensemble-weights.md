- [ ] intent-ensemble-weights — `NoModel` blends the probe with the
      pattern tier using ONE fitted weight from a six-point grid,
      because sixty rows cannot support a fitted second-level model.
      When the corpus grows (see distillation), replace the grid with a
      real stacking model and measure whether it beats the blend.
      GATED, and the gate is now measured (2026-09-07): the corpus did
      not grow honestly — intent-distil-dose found the distilled rows'
      gain to be one split's, intent-distil-static found them worth
      nothing to the static table, intent-distil-diversity found them
      a third as diverse as the fixture — so a second-level model
      trained on them would learn the generator's register. Opens when
      the human fixture passes ~200 rows (the review queue is the
      source); the six-point grid stays until then.
