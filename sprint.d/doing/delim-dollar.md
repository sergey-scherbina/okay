- [ ] delim-dollar — stage 1 of specs/shift0-dollar.md: `Delim.dollar(p)(ret)(body)`
      in the machine (a return function on the delimiter, taken into a
      0-capture's segment by the `$/S0` rule). Laws `($v)`, `($/S0)`,
      `reset0 = pure $`; APLAS 2012's macro-expression of `$` checked
      against the primitive with `Bisim.check`; DelimBenchmark
      push-only and generator lanes before/after. AFTER
      shift0-dollar-probe. (2026-09-24)
