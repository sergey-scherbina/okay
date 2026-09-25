- [ ] stack-safety-catch-up — stack recursions that landed AFTER the
      2026-09-25 inventory, named by the stage-9 guard's first run
      (stack-safety-guard). The fourteen in okay-arrow are CLOSED
      (stack-safety-arrow): the reader recursed per level of a schema read
      from the stream and overflowed on a 100 000-deep one; every door a
      type comes in by now refuses past Arrow's own limit of 64, and the
      rows carry that bound. LEFT, as UNAUDITED rows in
      specs/stack-safety-okay.tsv: okay-py's `ArrowFrames.cells`, which
      recurses per level of a column's type on the way to Python (so it
      is likely under the same 64 once its input is an okay-arrow Table:
      check, then write the bound); and `ContMacro.rewrite` in the core,
      which runs at compile time over a user's tree (stage 7). (2026-09-25)
