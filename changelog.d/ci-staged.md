## ci-staged (stage A) - your modules first, then their dependents; re-gate only for your own

Operator (2026-09-25): the local box collapses because every lane runs
the family-sized gate at once and `land.sh` sends every lane back to a
fresh one whenever any sibling lands a source commit. The shape asked
for, stated twice: before the merge, the tests of the modules that
changed, then the tests of the modules that depend on them, one list
in that order; the whole build only in master, before the push. This
lane is stage A of specs/ci-staged.md; the post-merge runner that runs
the whole build and pushes is stage B, filed as `ci-runner`.

`affected <ref> <task> <platform> staged` (project/Affected.scala): the
same project set `affected` always ran, as TWO sbt commands queued in
order — the changed projects' `test`, then the dependents' — so sbt
stops at the first red and a lane that broke its own module never pays
for its dependents. A build-file change collapses to one stage (every
project changed). `--plan` and `--files=a,b,c` print the stages without
running or diffing, which is how `scripts/affected-selftest.sh` checks
six shapes in one sbt start: okay-lex main is 3 projects then 125,
okay-lex test-only 3 then none, the core 3 then 169, `build.sbt` 182 as
one stage, `closed` the same 128 it was, a bad order refused.
`scripts/gate.sh "affected master staged"` is the pre-merge gate: the
order rides on both JVM-first phases (`gate-selftest.sh` case 8 — a first
cut's suffix match would have mangled the four-argument form).

`land.sh` step 2 re-gates only when master's gained source lies in a
MODULE this lane touched (first path component; `project/` and the root
`*.sbt` are the build, on either side) — a sibling's disjoint landing is
rebased onto, and the runner checks the two together before the push.
Also: the landing sha comes from the branch tip, not master's HEAD after
the merge (a sibling claim has slipped between the two), and the
release-claim commit no longer carries an attribution trailer.

Recorded in the spec's Decisions so it is not re-proposed: a first draft
had the dependents COMPILED pre-merge (`Test/compile`) rather than
tested; the operator overruled it. The win wanted is the order, the
narrowed re-gate and one whole build per batch — not a smaller set.

AGENTS.md "Before merging" rewritten; the PUSH rule is untouched until
the runner lands.
