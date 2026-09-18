- static-workflow — the durable spine without `ArrowApply`
  (specs/static-workflow.md). STAGE 1 LANDED 2026-09-18 (30f57b4b) and
  so did its notation (`proc-notation`: the block, branches, loops, the
  spellings, auto-colouring) and the shared `arrow-laws` suite. What
  the queue is for now is WHAT IS LEFT, which is stages 2-4 of the
  spec, thirteen behaviour boxes, in this order:
  - `static-workflow-strands` (stage 2) — the deploy check that runs
    BEFORE a deploy (`Proc.strands(topic, term)`: which live runs would
    this term strand, and where), and the EXHAUSTIVE cut — crash before
    and after the append at every leaf, resume, compare with the
    uninterrupted run, count activities per `(id, position)`. A finite
    term makes that a property rather than a sample, which is most of
    why the shape exists. NEXT, and the biggest of the three.
  - `static-workflow-optics` (stage 3) — a lens and a prism applied to
    a STEP. The instances already exist, so this is tests and an
    example rather than machinery; it also closes the optics board's
    `optics-prism-selective`, which waits on it.
  - `static-workflow-render` (stage 4) — `render` draws the term as
    Mermaid with the run's position marked. `Proc.nodes` already hands
    every node its path, so this is a second consumer of one walk.
  Stage 5 is gated with triggers written where it stands; do not build
  it without one.
