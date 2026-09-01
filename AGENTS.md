# Working in this repository (agents)

SPRINT: SPRINT.md
BACKLOG: BACKLOG.md
CHANGELOG: CHANGELOG.md

Several agents commit to one `master` from one machine. The rules in
force, all already practiced, none previously written down:

## Coordination
- The protocol is the `multi-agent` skill; this file only fixes the
  repo-specific facts. The branch is `master` (not `main`), there is
  no remote sync step — claims and merges are local.
- Claims live in `.work/active/<slug>.claim`, committed to `master`.
  One claim is one task; release it (`git rm` + commit) when the task
  lands, naming the landing commit.
- All work happens on a `feature/<slug>` branch in a worktree OUTSIDE
  the repo (`../okay-wt-<slug>` — `.worktrees/` inside also occurs and
  is gitignored). The main checkout is for reading state, claims, and
  fast-forward merges only. Never `git stash`/`reset --hard` in the
  main checkout: another agent's uncommitted work lives there.
- Before merging: rebase the branch on `master`, run `sbt test`, then
  `git merge --ff-only` — and READ the merge output; git refuses a
  fast-forward over a sibling's uncommitted files, and the refusal
  scrolls past a `tail -1`.
- Coordination room: rozum. Announce landings; flag files you both
  touch (`build.sbt`, `src/jmh/history.tsv` — append-only, expect
  tail conflicts, resolve by keeping both sides).

## Boards
- `SPRINT.md` is what agents pick from (claim before working);
  `BACKLOG.md` is where found-but-deferred work goes THE MOMENT it is
  found; `CHANGELOG.md` is append-only, newest first, one entry per
  landed task naming the commits. Lifecycle: promote backlog -> sprint
  -> claim -> land -> DELETE from sprint, prepend to changelog.

## Specs
- The `spec-dev` skill, with `specs/` as the spec directory (no global
  SPEC.md). Write or extend the feature's spec and COMMIT it before
  implementation; check `- [ ]` behavior items off as tests cover
  them; record findings and refuted alternatives in the spec's
  Decisions/Results — that history is why the specs exist.

## Build facts that bite
- `sbt test` runs everything, JVM + JS + Native. The core suite forks
  (see build.sbt for why); `.jvmopts` gives sbt 6g.
- Live suites (`TestLive` in okay-agent and okay-mcp) hit a local
  model endpoint and npx respectively; they SKIP where those are
  absent, so a red TestLive usually means the endpoint died, not the
  code.
- Benchmarks: measure before optimizing, record in
  `src/jmh/history.tsv` (TABS, eight columns — literal `\t` has
  slipped in before and breaks parsing), keep refuted experiments.
- `organization` is `dev.okay` (build.sbt is the decision in force).
