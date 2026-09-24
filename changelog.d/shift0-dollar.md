## shift0-dollar - research and plan: `$`, stacked shift0, layered reflection

Operator, 2026-09-24: shift0 and `$` must be researched here and done
properly, and so must reflection over a stack of monads. This lane is
the research and the plan, and ships no code.

- specs/shift0-dollar.md. λ$'s `$` (Materzok & Biernacki, APLAS 2012)
  is NOT `push(p)(e).flatMap(v)`: a shift0 captures `v $ K` together
  with `v`, so the two differ whenever a 0-capture does not resume
  exactly once. The spec covers the ICFP 2011 typing of shift0 (the
  body typed under the stack with the top removed) and what transfers
  to our named prompts, and the FSCD 2019 correspondence (deep
  handlers with shift0, shallow with control0, `$` as the return
  clause). Stages 0-4.
- specs/layered-reflection.md. Filinski 1999 plus Brachthäuser,
  Boruch-Gruszecki & Odersky (2020): native multi-prompt control gives
  layered reflection directly, and `Delim.Prompted` is already the
  capability. Stage 0 runs on today's `Delim`.
- Sprint queue: shift0-dollar-probe, delim-dollar, stacked-shift0
  (promoted), handlers-as-dollar, and monadic-reflection-stacked
  (promoted).
