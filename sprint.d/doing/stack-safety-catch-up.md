- [ ] stack-safety-catch-up — stack recursions that landed AFTER the
      2026-09-25 inventory, named by the stage-9 guard's first run
      (stack-safety-guard). The fourteen in okay-arrow are CLOSED
      (stack-safety-arrow): the reader recursed per level of a schema read
      from the stream and overflowed on a 100 000-deep one; every door a
      type comes in by now refuses past Arrow's own limit of 64, and the
      rows carry that bound. `ArrowFrames.cells` (okay-py) is BOUNDED by the same limit, since it
      walks a Table's columns (stack-safety-query). LEFT, the one UNAUDITED
      row in specs/stack-safety-okay.tsv: `ContMacro.rewrite` in the core,
      which runs at compile time over a user's tree (stage 7). (2026-09-25)
