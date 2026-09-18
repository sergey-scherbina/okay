- arrows-plan — ONE plan across static-workflow, proc-notation and
  optics stage 12 (specs/arrows-plan.md, LANDED 2026-09-18). Four
  decisions: `Proc` is over a SIGNATURE beside `Static`, the workflow
  is the signature `Wf.Question` (static-workflow's Interface amended);
  the arrow LAWS are their own first lane `arrow-laws` (generic suite
  in test scope, `TestMealy`'s statements lifted out, instantiated at
  `Mealy` only — unblocks both `static-workflow-proc` and
  `optics-arrow-instances`, which each then add one line); the order
  is 1 arrow-laws → 2 static-workflow-proc (stage 3's optics tests
  folded in) → 3 proc-notation-road on the workflow side, 4
  optics-arrow-instances and 6 optics-cont-profunctor on the optics
  side, neither side waiting on the other after lane 1; and whether
  `leaves`/`walk`/`render` are ONE path-indexed fold is a sentence
  stage 1 must write (the fourth indexed-optics seat, or not).
  LANE 1 (`arrow-laws`) LANDED 2026-09-18: 15 laws in
  `okay.laws.ArrowLaws`, `Mealy` instantiates in three lines, and its
  `right` is tested for the first time.
  LANE 2 (`static-workflow-proc`) LANDED 2026-09-18: `okay.Proc` over
  a signature, `Wf.Question`/`Wf.Proc.program`/`Wf.Proc.walk`, 37 new
  tests, okay-persist unchanged. Decision 4 answered: `leaves` and
  `render` ARE one path-indexed fold (`Proc.nodes`), `walk` is not —
  filed as BACKLOG `optics-indexed-fourth-seat`.
  LANE 3 (`proc-notation-road`) LANDED 2026-09-18: `Proc.direct`
  compiles the straight-line block to a term (the environment threaded
  in `Arr`s, `app` refused by name), `Direct.scala` untouched, and
  docs/static-workflows.md is the page for the whole arc. STAGE 2 OF
  THIS PLAN IS REFUSED FOR NOW with its evidence: the third road
  shares twenty lines with the other two, so one IR would be two IRs
  with one name. Follow-ups filed: `proc-notation-branches` (if/loops
  in the notation) and `proc-notation-liveness`.
  LANE 3.1 (`proc-notation-branches`) LANDED 2026-09-18: an `if` with
  questions in its branches is `OnRight`, a `while` is `Iter`, and an
  assignment is a REBUILD of the environment rather than a mutation.
  THE PLAN'S WORKFLOW SIDE IS DONE. What is left of the plan is the
  optics side's own lanes (4 `optics-arrow-instances`, 6
  `optics-cont-profunctor`), which do not wait on anything here.
