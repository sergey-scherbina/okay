# Working in this repository (agents)

SPRINT: SPRINT.md
BACKLOG: BACKLOG.md
CHANGELOG: CHANGELOG.md

## Skills
- Skills live in the `.agents/plugins` submodule (fresh clone:
  `git submodule update --init`). Read `.agents/plugins/AGENTS.md` — it
  indexes every skill; when a task matches one, read
  `.agents/plugins/<name>/commands/<name>.md` and follow it. In a
  worktree the submodule is checked out only in the main repo — read
  skills from there, never `submodule update --init` in the worktree.

Several agents commit to one `master` from one machine. The rules in
force, all already practiced, none previously written down:

## Coordination
- The protocol is the `multi-agent` skill
  (`.agents/plugins/multi-agent/commands/multi-agent.md`); this file
  only fixes the repo-specific facts. The branch is `master` (not `main`), there is
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
  scrolls past a `tail -1`. HARDENED after three incidents: the merge
  runs ALONE (its own command, from the main checkout, exit code
  printed), and only after reading exit 0 do worktree removal, branch
  deletion, boards and the claim release run. A `;` after a failed
  merge has twice deleted an unmerged branch and pushed a release
  entry for work that had not landed.
- Coordination room: rozum (etiquette: the `rozum` skill). Announce
  landings; flag files you both
  touch (`build.sbt`, `src/jmh/history.tsv` — append-only, expect
  tail conflicts, resolve by keeping both sides).

## Boards
- The protocol is the `scrumban` skill: write the plan into the board
  BEFORE executing. `SPRINT.md` is what agents pick from (claim before working);
  `BACKLOG.md` is where found-but-deferred work goes THE MOMENT it is
  found; `CHANGELOG.md` is append-only, newest first, one entry per
  landed task naming the commits. Lifecycle: promote backlog -> sprint
  -> claim -> land -> DELETE from sprint, prepend to changelog.

## Specs
- The `spec-dev` skill
  (`.agents/plugins/spec-dev/commands/spec-dev.md`), with `specs/` as
  the spec directory (no global
  SPEC.md). Write or extend the feature's spec and COMMIT it before
  implementation; check `- [ ]` behavior items off as tests cover
  them; record findings and refuted alternatives in the spec's
  Decisions/Results — that history is why the specs exist.

## Code rules the operator has set
- NO CAST WITHOUT A REAL NECESSITY (operator, 2026-09-02). An
  `asInstanceOf`, an `@unchecked` pattern, an `Any` where a type
  parameter would do, is a claim the compiler cannot check — and
  "the first draft was easier that way" is not a reason. Before
  writing one, try the typed route: a polymorphic method (`def
  perform[X](op: Tx[X]): X` instead of `Tx[Any]`), GADT matching on
  the freer tree (`case Bind(Effect(e), k)` types e and k), a
  helper class holding the typed pair (`Held[X]`), a decision taken
  at construction instead of a type test per value (`TRef.bare`).
  When a cast truly cannot go — a heterogeneous map keyed by
  identity, an erased type behind a wildcard — isolate it in ONE
  function and say in its comment why the type is right. Incident:
  Stm.scala's first cut had a dozen erasure casts in the
  interpreter; all but one were removable (stm-typed-interpreter).

## Build facts that bite
- A guess about the build is a HYPOTHESIS, not a fact: check it
  before acting on it. Incident (2026-09-02): an agent decided a
  sibling's sbt run "held a lock" and waited on it — there was no
  lock; a `ps`/`ls` would have shown it in a second. If you think
  "X is probably the reason", run the one command that would show
  X, and only then believe it (operator directive, 2026-09-02).
- A DISCARDED PROGRAM is a compile ERROR (build.sbt, -Wconf): an
  `A ! F` value in statement position, as a Unit def's body, or
  eta-expanded into a Unit function builds a program and drops it —
  `c.send(x)` alone sends nothing. From plain code write
  `c.offer(x): Unit`; inside a program, flatMap/map the send. The
  compiler cannot see `xs.foreach(c.send)` or `for x <- xs do
  c.send(x)` (foreach takes any result) — those two shapes are on
  you. Found by channel-callback (2026-09-02): ten silent discards
  across ui/jetty/netty/chatweb before the lint existed.
- **POLICY: no warnings, ever — main or test, any platform** (2026-09-03,
  reinforcing tidy-warnings' 255 → 0). A landing that introduces a
  warning is not done; fix the code or, for a warning the compiler
  cannot actually resolve correctly either way (see the `-Wconf`
  entries in build.sbt, each with its own dated comment explaining
  why), silence that exact message with a comment saying why — never
  change code shape just to please the linter, and never suppress a
  category wholesale. A discarded Java/JS result is `x: Unit` (or
  `val _ = x` for a js.Dynamic, which `: Unit` does not silence), an
  unused pattern type is `?`, an unused parameter that an API forces
  is `@unused`. Verify with `clean; Test/compile` (add `-feature
  -deprecation` via `set ThisBuild / scalacOptions += ...` when
  hunting — they are not on by default, so warnings under them can
  hide) — an incremental compile hides warnings in files it did not
  touch. A `@nowarn` that only fires a real warning on one platform
  (e.g. a JVM-only erasure check) is expected to look "unused" on the
  others; that specific message is silenced project-wide rather than
  chasing it per-platform (see `-Wconf:msg=@nowarn annotation...`).
- **POLICY: no flaky tests in the default gate — only in `integrationTest`**
  (2026-09-03). Anything whose result depends on something `sbt test`
  cannot control — a live model gateway, docker, network timing — is
  `Live`-tagged (see below) and excluded by default, however solid it
  usually is; a landing's gate must not depend on external timing.
  Flakiness discovered in an already-untagged suite gets the same
  tag, not a retry loop or a widened assertion.
- `sbt test` runs everything, JVM + JS + Native. The core suite forks
  (see build.sbt for why); `.jvmopts` gives sbt 6g.
- Live suites (`TestLive` in okay-agent and okay-mcp, the LIVE tests
  in okay-demo's TestChatDemo) hit a local model endpoint and npx
  respectively; they SKIP where those are absent — and since
  live-skip-on-gateway-loss (2026-09-02) also when the endpoint
  drops the connection MID-test (an IOException anywhere in the
  cause chain: `okay.llm.Live.wireDropped`, the `liveTest` helper).
  A red live test therefore means a wrong ANSWER, not a dead
  endpoint; write new live tests with `liveTest`, not `test`.
- **Every suite reaching outside the JVM — a live model gateway,
  docker (kafka/mongo/pg/redis/s3), an external tool (openssl,
  python3) — is tagged `Live` and OUT of `sbt test` by default**
  (integration-test-gate, 2026-09-03): `sbt integrationTest` runs
  them. This is not the availability skip above (`assume`/
  `munitIgnore` — unchanged, still fires inside these suites when
  the service is absent); it exists because even a PRESENT service
  can flake on its own timing (this session alone: TestChatDemo's
  LIVE suite failed identically on untouched master, twice, under
  live-model load), and a landing's gate should not depend on that.
  Tag a new live/docker-dependent suite the same way: `override def
  munitTests(): Seq[Test] = super.munitTests().map(_.tag(new
  munit.Tag("Live")))` (whole suite), or thread it through a
  `liveTest`-style per-test helper (TestChatDemo's own) where a
  suite mixes live and non-live tests. specs/integration-test-gate.md.
- Benchmarks: the `performance` skill is the protocol — measure
  before optimizing, record in
  `src/jmh/history.tsv` (TABS, eight columns — literal `\t` has
  slipped in before and breaks parsing), keep refuted experiments.
- `organization` is `dev.okay` (build.sbt is the decision in force).
