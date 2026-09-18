- optics-arrows-effects — what optics, profunctors and arrows do with
  the monads, applicatives, effects and continuations already here
  (specs/optics.md stage 12, the operator's question 2026-09-18). The
  survey landed as the spec section; of the five lanes it named, FOUR
  ARE CLOSED and the fifth was never one:
  - `optics-guide-page` — docs/optics.md, five pairs run by
    `TestOpticsGuide`; it also found `optic-law-rewrites` cited as
    filed in two places and filed in neither.
  - `optics-cont-profunctor` — half instance, half refutation:
    `Strong` exists and `PState.zoom` is one line through it, `Choice`
    CANNOT exist (parametricity — the absent case has no `X` to answer
    with), and `PState.zoomCase` is the door that prices itself in its
    type.
  - `optics-arrow-instances` — `Arrow[Function1]` as a WIDENING of the
    existing given (a second one would make `Strong[Function1]`
    ambiguous everywhere) and the Kleisli at `Optic.Star`, both
    instantiating the shared `arrow-laws` suite in three lines.
  - `optics-prism-selective` — CLOSED by its own gate: static-workflow
    stage 3 answers it for a TERM, and nobody wants the `Star[F]` road
    for `Static`. Reopen when a consumer appears.
  - `ArrowChoice` was never this board's: its trigger was pulled by
    `Proc` on the workflow side.
  WHAT IS LEFT under `backlog.d/optics-arrows-effects/`, all genuinely
  open and none blocking anything: `optic-law-rewrites` (teach `Fuse`
  the two rewrites its own measurement priced), `optics-field-fuse`
  (the by-name lens is the one constructor the planner cannot read —
  MEASURE before choosing), and `gate-fanout-what-is-left`. Two files
  there are RECORDS rather than lanes and are named as such.
