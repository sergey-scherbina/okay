- schema-fold — LANDED, all three stages, 2026-09-11 (1d56db5d, 95b1013a,
  e72d44c8; specs/schema-fold.md Results): `Schema.fold`/`Algebra`/`Edge`,
  `Step`/`Folded`, four value doors moved and measured faster, `Validate`.
  What it left in BACKLOG under okay-codec: schema-typed-paths (a checked
  lens on `A`, a macro), form-errors-on-validate (a UI lane). Delete this
  line at the next queue rewrite; it stays one cycle so the next agent
  does not go looking for the stages.
  THE MIDDLE SHA WAS CORRECTED 2026-09-18: it had named that commit's
  PRE-REBASE object, which still exists locally so nothing ever looked
  wrong, and which is not on master. Found the hour
  `check-citations.sh` was taught to read the boards' directories —
  the first time anything had checked SPRINT at all. The old sha is
  deliberately not quoted: the checker greps every 8-hex word and
  cannot tell a citation from a mention of one.
