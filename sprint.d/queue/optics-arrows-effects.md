- optics-arrows-effects — what optics, profunctors and arrows do with
  the monads, applicatives, effects and continuations already here
  (specs/optics.md stage 12, the operator's question 2026-09-18). The
  survey LANDED as the spec section: the tree already runs one
  traversal at `Validated`/`Par`/`Static`/an effect row, `PState.zoom`
  is the lens-meets-continuation seam, `Arrow` has one instance. Five
  lanes are in BACKLOG under `optics-arrows-effects`;
  `optics-guide-page` LANDED 2026-09-18 (docs/optics.md, five pairs
  run by `TestOpticsGuide`, and it found `optic-law-rewrites` cited as
  filed in two places and filed in neither). Next is
  `optics-arrow-instances` (its law suite is SHARED with
  static-workflow stage 1 — one suite, two lanes, first to land writes
  it). RE-CHECKED the same day against the plans: `ArrowChoice`'s
  trigger is pulled by `Proc` (specs/static-workflow.md), not by
  `Tables.Plan`; `optics-prism-selective` WAITS on static-workflow
  stage 3, which answers it for a term; indexed optics have three
  hand-written seats named in BACKLOG and still no second interpreter;
  `optics-cont-profunctor` LANDED 2026-09-18 — half instance, half
  refutation: `Strong` exists and `PState.zoom` is now one line
  through it, `Choice` cannot exist at all (parametricity: the absent
  case has no `X` to answer with), and `PState.zoomCase` is the door
  that prices itself in its type. Left on the optics side:
  `optics-arrow-instances` (waits on the shared `arrow-laws` suite)
  and the two gated items.
