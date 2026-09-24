## delim-dollar - `$` as a primitive of the Delim machine (λ$, APLAS 2012)

specs/shift0-dollar.md stage 1. `Delim.dollar(p)(ret)(body)`: run the
body under the delimiter, and when it returns, leave the delimiter
through `ret`. A shift0 captures the delimiter WITH `ret`, which is the
`$/S0` rule. As in λ$ it is the primitive, and `push` is
`dollar(p)(pure)`, keeping its plain frame on the hot path.

- The machine has a `Segs.Ret` frame (body `R0` to prompt `R`). The cut
  is `Plain | AtRet | NotFound`, with the existential `P0` a type
  member, and no cast was added. Control-captures to a dollar are
  refused at run time, because their bare continuation answers `R0`.
  Stage 3 (typed shallow handlers) is where that gets decided.
- TestDollar (11): `($v)`, `($/S0)` (k dropped, k twice),
  re-installation, shift under a dollar, agreement with APLAS's macro
  and `reset0 = pure $` on six bodies, `R0 ≠ R`, ICFP 2011's example
  with rets, the control refusal, and 100 000 nested dollars. A mutant
  found a gap in the first list (re-installation), which is closed now.
- Measured, gated at load < 4. The primitive is 1.61x faster than the
  macro with nothing captured and 1.42x with one resume, and uses 176 to
  232 B less per dollar. `dollar` against `push` is 1.03. The first
  cut of the capture path cost the generator lane +104 B and 7-9% per
  capture; the final one matches master byte for byte and is within
  2% on time. history.tsv has the rounds.
- docs/continuations/11-four-captures.md: "A fifth word: `dollar`",
  with APLAS 2012 and FSCD 2019.
