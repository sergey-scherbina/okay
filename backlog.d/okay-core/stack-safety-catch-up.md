- [ ] stack-safety-catch-up — sixteen stack recursions landed AFTER the
      2026-09-25 inventory and were named by the stage-9 guard's first
      run (stack-safety-guard): fourteen in okay-arrow, `ArrowFrames.cells`
      in okay-py, `ContMacro.rewrite` in the core. They are rows marked
      UNAUDITED in specs/stack-safety-okay.tsv, so the guard holds, but
      nobody has decided what bounds them. THE SUSPECT is
      `OkayArrow.parseField` (and `dictFields`, `field`, `column` beside
      it): it recurses per level of the SCHEMA it reads out of an Arrow
      file — input a writer chose, the same class as the WireCbor defect
      stage 2c fixed. `Fb.table`/`tables` are the writer and recurse on a
      schema this process built; `ContMacro.rewrite` is compile time
      (stage 7). Take each the usual way: red first on a small stack at
      a depth an input can reach, then an explicit stack, or a written
      bound replacing UNAUDITED. (2026-09-25)
