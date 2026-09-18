- static-workflow — the durable spine without `ArrowApply`
  (specs/static-workflow.md). **CLOSED 2026-09-18, every stage that was
  in scope**, all on the same day: stage 1 the type, the bridge and the
  laws (30f57b4b); stage 2 the deploy check and the exhaustive cut
  (7a1427bd); stages 3 and 4 optics on a step and a term that draws
  itself (cae24773). Its notation closed with it —
  specs/proc-notation.md: the block, branches and loops, the spellings,
  auto-colouring — and the shared `arrow-laws` suite came out of the
  same plan (specs/arrows-plan.md, whose workflow side is done).
  WHAT THE ARC LEFT BEHIND, for whoever picks it up:
  - STAGE 5 IS GATED, not forgotten: parallel branches, compensation
    as structure, and the cursor chapter each carry a trigger written
    where they stand. Building one without its trigger is building it
    for the spec rather than for a consumer.
  - THE DEPLOY CHECK IS TWO QUESTIONS and only one of them is a term's:
    `strands` sees the SHAPE of a journal, the envelope's `program`
    field sees who wrote it. Neither alone is enough, and that is
    stated in the spec's stage-2 Results rather than left to be found.
  - `proc-form-consumer` (backlog.d/okay-ui) is the box this arc
    declined to tick: a form as a durable procedure needs a CONSUMER,
    which is a UI lane's to write.
  - docs/static-workflows.md is the page to hand somebody, and
    Appendix A of the book carries the refutation of its own argument
    against this shape (book-vs-proc).
  Delete this line at the next queue rewrite; it stays one cycle so the
  next agent does not go looking for the stages.
