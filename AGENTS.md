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
- Two of them bind to tools rozum serves for THIS project on the same
  MCP connection as the meeting room, so they are worth naming here
  rather than leaving to the index — what an index cannot tell you is
  WHEN:
  - `rag.search` (the `rag` skill): semantic search over this repo's
    own code and docs. Reach for it when you do not know the token,
    symbol or path — a concept, a symptom, an unfamiliar area whose
    shape you need first. Do NOT reach for it when you do know: grep
    and Read are exact, instant and never stale, and a hit here is a
    pointer to open, not an answer. It earned its keep in this repo
    already: it is how the `journal-versions` design error was found,
    by asking the other repo what it had built for the same problem.
  - `state.get` / `state.update` / `state.reset` (the `task-state`
    skill): a durable per-project JSON fact store that does not live
    in the conversation, so `/clear` and a fresh session cannot lose
    it. Read it at the start of a task and after any compaction;
    write a fact the moment you learn it, rather than trusting the
    transcript to still be carrying it later.
- Both are bound to THIS project by `.mcp.json`, whose URL carries
  `?project=` — do not drop it. rozum serves every project from one
  daemon and picks the project from that query; without it the daemon
  falls back to ITS OWN launchd default, and the failure is silent:
  measured 2026-09-03, `state.update` from a session here wrote into
  the rozum repo's state file and `rag.search` answered out of the
  rozum index, which is why its hits kept being Rust. The index is
  per project too and is not automatic — `rozum rag index --root .`
  builds it (incremental, under a second here), and a stale index
  reports its own age in every result. After the first build, keeping
  it fresh is the hooks' job: `sh scripts/githooks/install.sh` once
  per clone points `core.hooksPath` at the tracked hooks, which
  re-index the MAIN checkout after a merge, a commit or a checkout.
  They never fail the git operation that called them and do nothing
  at all when `rozum` is absent — an index is a convenience, a commit
  is not.

Several agents commit to one `master` from one machine. The rules in
force, all already practiced, none previously written down:

## Coordination
- The protocol is the `multi-agent` skill
  (`.agents/plugins/multi-agent/commands/multi-agent.md`); this file
  only fixes the repo-specific facts. The branch is `master` (not `main`).
  Claims and merges are LOCAL — no lane needs the network to land, and
  none should wait for it. Pushing is a SEPARATE, deliberate act by
  whoever the operator asks; it is not part of landing a lane and not
  part of the claim procedure. (2026-09-08: `origin` was 60 commits
  behind and was fast-forwarded to `eca8877e` on the operator's
  instruction. Before that nothing had pushed for days.)
- **NEVER `reset` or `merge` to `origin/*`.** Not because origin is
  always stale — since 2026-09-08 it is sometimes current — but
  because it is current only in the moments just after somebody
  pushes, and NOTHING in the landing procedure pushes. So at any
  instant `origin/master` is master-minus-every-lane-landed-since-the-
  last-push, and that number is usually not zero. `git log --oneline
  origin/master..master` tells you what it is; do not guess, and do
  not assume a fresh `git fetch` made it zero. The
  skill's claim procedure literally says `git fetch origin` and
  `git merge --ff-only origin/main`; followed in this repo that
  discards every lane landed since the last push. INCIDENT
  2026-09-03 23:22: a `reset: moving to origin/master` in the main
  checkout moved master back four commits and dropped three landed
  lanes (gate-honesty, flakes-integration, ring-channel) off the
  branch pointer. They came back only because an unrelated ff-merge
  26 seconds later happened to carry them. If it happens again:
  `git reflog show master` names the old tip, and
  `git reset --hard <tip>` restores it — the reflog is local and only
  yours, so ask in the room before anyone commits on the wrong base.
- **The one case where you DO integrate `origin`: when it is genuinely
  AHEAD.** The prohibition above exists because a `merge --ff-only
  origin/master` DISCARDS lanes when origin is behind. It says nothing
  about the opposite, which happened on 2026-09-09: five README edits
  were made on GitHub during a session, so both sides had commits the
  other lacked and no fast-forward existed in either direction.
  `--force` was never an option — it would have destroyed the five.
  MERGE, do not rebase, and the reason is specific rather than
  stylistic: a rebase rewrites the shas of every unpushed commit, and
  CHANGELOG.md and BACKLOG.md cite landed commits BY SHA ("landed as
  51adaf00", "fixed-in 3f09bd9c"). Seventeen such references pointed
  into the unpushed range that day. A ledger citing commits that no
  longer exist is a worse defect than a merge commit on an otherwise
  linear branch. Gate the merged tree before pushing it: the merge is
  a tree nobody has tested, however trivial the incoming diff looks.
- Claims live in `.work/active/<slug>.claim`, committed to `master`.
  One claim is one task; release it (`git rm` + commit) when the task
  lands, naming the landing commit.
- NEVER `pkill -f sbt`, `pkill java` or `killall java` — and the 143
  is NOT one of those, see "THE 143, SOLVED" below. MEASURED 2026-09-06:
  a full matrix died at 1635 tests with sentinels watching — the
  sentinel in its OWN session carrying "sbt-launch" in its command
  line died with it, while a sentinel in the gate's own process GROUP
  and one with a neutral name both lived. Only a kill by NAME does
  that; `matrix-kill-by-process-group` had blamed a suite killing a
  process group for two days, and the setsid fix it prescribed would
  have fixed nothing. This entry first read the killer as a sibling
  agent tidying up — wrong, and corrected the same evening: it is the
  scalascript launchd pair. The rule stands for humans and agents all
  the same: kill by PID, and check whose pid it is first — `ps -p <pid>
  -o args=` before any signal. `scripts/gate-sentinels.sh` runs a gate
  with the trap set and a `ps` recorder, which is how the killer's
  `grep -E 'sbt-launch|xsbt\.boot|sbt\.script|…|org\.openjdk\.jmh'` was
  finally caught on the line two seconds before a build died.
  The same check before believing "the box is busy": a JMH fork that
  HANGS keeps `/var/folders/.../T/jmh.lock` for everyone. 2026-09-06
  one sat 37 minutes at 0.0% CPU, state S, and blocked a sibling's
  benchmarks for half an hour of polite waiting. `pgrep -f
  org.openjdk.jmh.runner.ForkedMain`, then `ps -o pcpu,etime,stat -p`:
  a fork at 0% for minutes is asleep, not measuring, and running past
  its lock with `-jvmArgs -Djmh.ignoreLock=true` perturbs nothing.
  Do not kill it — it is someone's diagnostic state — and do not run
  past a lock whose holder is actually burning CPU.
  The orphans are the idle reaper's leftovers — "THE 143, SOLVED"
  below names both agents, their log, and the fix.
- NEVER `git add -A`/`git add .` in the main checkout — stage the
  explicit paths you wrote. 2026-09-06: a `git add -A` beside a claim
  swept a sibling's in-progress 275-line benchmark into a commit
  titled `claim: backlog-audit` and pushed it. The file was
  syntactically whole so master still built, and it was left in place
  rather than reverted — a revert deletes the file from the working
  tree the sibling is still typing into. `git add -p` is unavailable
  here, so a shared file another agent is editing cannot be staged
  hunk-wise at all: that is the second reason the lane belongs in a
  worktree, not just the merge discipline below.
- All work happens on a `feature/<slug>` branch in a worktree OUTSIDE
  the repo (`../okay-wt-<slug>` — `.worktrees/` inside also occurs and
  is gitignored). The main checkout is for reading state, claims, and
  fast-forward merges only. Never `git stash`/`reset --hard` in the
  main checkout: another agent's uncommitted work lives there.
- **Run `scripts/check-citations.sh` immediately before
  `git merge --ff-only`.** It reads every 8-hex word out of
  CHANGELOG.md and BACKLOG.md, keeps the ones that are commits, and
  fails on any that is not an ancestor of HEAD — run it FROM THE
  WORKTREE, where HEAD is the tip about to become master, so the
  lane's own citations count. It exists because
  the hand-run version of this check failed THREE TIMES in one session
  — not from carelessness but from checking the wrong thing: the sha
  in hand rather than the sha in the file. The first run of the script
  found sixteen dangling citations, only two of them from that
  session's lanes, so the mechanism has been quietly costing the
  ledger for a long time. A check that requires remembering what to
  check is not a check.
- **Why they go stale at all: a rebase rewrites every sha on the
  branch**, and the boards cite landed work BY sha, so hexes written
  during a lane are wrong the moment the branch moves again. The
  window is exactly the gap between gating and merging.
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
- **Lane rules for comparison benchmarks** (docs/benchmarks.md, "Lane
  rules"): before a competitor's number is quoted, (1) a lane built by
  `foldLeft` gets a right-nested twin — kyo's left-nested shape read
  ~1000x and was quoted as the library's price for a week; (2) only
  lanes sharing a granularity and memoisation compare — five of six
  "ZIO ahead" rows were mismatched pairs; (3) the competitor is priced
  from the source its author intended (`ZStream.range`, `Stream.emits`,
  kyo `Stream.range`), the per-element source beside it as the worst
  case, never alone. A lane's header answers all three or the lane
  lands without a number.
- **POLICY: no warnings, ever — main, test AND Jmh, any platform**
  (2026-09-03, reinforcing tidy-warnings' 255 → 0). A landing that introduces a
  warning is not done; fix the code or, for a warning the compiler
  cannot actually resolve correctly either way (see the `-Wconf`
  entries in build.sbt, each with its own dated comment explaining
  why), silence that exact message with a comment saying why — never
  change code shape just to please the linter, and never suppress a
  category wholesale. A discarded Java/JS result is `x: Unit` (or
  `val _ = x` for a js.Dynamic, which `: Unit` does not silence), an
  unused pattern type is `?`, an unused parameter that an API forces
  is `@unused`. **`x: Unit` is not a universal off switch**, and
  reading this line as one put five warnings on master: four
  `a.tell(m).runWith: Unit` in okay-actor's tests still raised E175,
  because build.sbt:41 escalates a discarded `!` PROGRAM to an error
  and says nothing about a discarded plain value. When a discarded
  value is one OUR code chose to return, the fix is usually to use it
  — `tell` answers whether the mailbox took the message, so
  `assert(a.tell(m).runWith)` is a real assertion where the ascription
  was a comment (okay-actor-tell-warnings, 2026-09-06). Verify with `clean; Test/compile` (add `-feature
  -deprecation` via `set ThisBuild / scalacOptions += ...` when
  hunting — they are not on by default, so warnings under them can
  hide) — an incremental compile hides warnings in files it did not
  touch. A `@nowarn` that only fires a real warning on one platform
  (e.g. a JVM-only erasure check) is expected to look "unused" on the
  others; that specific message is silenced project-wide rather than
  chasing it per-platform (see `-Wconf:msg=@nowarn annotation...`).
  **`Test/compile` does NOT reach Jmh sources** — they are their own
  configuration, which is how three warnings sat unnoticed in
  `compare/src/jmh` through the sweep that took main+test to zero
  (jmh-warnings, 2026-09-03). Check them with `compare/Jmh/compile`
  and `okayJVM/Jmh/compile`, after `rm -rf <project>/target` — the
  JMH generator caches hard, and a stale cache reports success
  without recompiling (it will also fail `Jmh/run` with "Unable to
  find the resource: /META-INF/BenchmarkList", whose fix is the same
  full `rm -rf`, not deleting `src_managed`/`jmh-classes` alone).
  Where a comparison benchmark warns because it is written in a
  COMPETITOR's idiom, the suppression is scoped to the `compare`
  project, never `ThisBuild`: a benchmark rewritten to please our
  linter measures the rewrite, not the library, but the main tree
  must keep the lint that would catch a real defect.
- **POLICY: no flaky tests in the default gate — only in `integrationTest`**
  (2026-09-03). Anything whose result depends on something `sbt test`
  cannot control — a live model gateway, docker, network timing — is
  `Live`-tagged (see below) and excluded by default, however solid it
  usually is; a landing's gate must not depend on external timing.
  Flakiness discovered in an already-untagged suite gets the same
  tag, not a retry loop or a widened assertion. As of nio-port-scope
  (2026-09-03) every suite that BINDS a real port is tagged, found by
  survey rather than one flake at a time — the survey is
  `grep -E '\.(serve|listen)\(0\)|ServerSocketChannel\.open|new ServerSocket'`
  over the test tree, and a new binding suite is expected to tag
  itself. And tagging is not a substitute for understanding: the same
  lane found that suite's assertion was testing something unassertable
  on a shared machine (a released ephemeral port is immediately
  re-bindable by a neighbour) and fixed the assertion too.
- `scripts/gate.sh` runs `sbt test` and TELLS THE TWO REDS APART: a
  real failure (any `==> X`) is final and printed; the one false red
  this repository is known to produce — a Native module reporting
  `Failed 0, Errors 1` because its test process was lost, which alone
  passes — is re-run for exactly those modules and reported either
  way. `scripts/gate.sh --read <log>` says what it would have done
  with a gate log you already have. The evidence, and what is ruled
  out, is in BACKLOG's `native-runner-error`.
- **The gate now reads two more things, both added 2026-09-11 after
  they cost something the same day.** A SIGNAL is not a verdict: sbt
  exiting 143/137 with no `==> X` prints `gate: KILLED`, because it
  used to print "RED — a failure this script does not recognise" and
  `gate-retry.sh`, which never retries a red, therefore refused the
  one case it was written for. And WARNINGS are checked: any
  `[warn] -- [Exxx]` in a run that actually compiled something is red,
  because "no warnings, ever" had no enforcement and four unused
  imports had ridden through every green gate. A WARM run compiles
  nothing and the script says so rather than letting silence look
  like cleanliness — which is another reason a lane gates in a fresh
  worktree.
  One E198 false positive is known and recorded in the script: a
  RENAMED import used only as an extension method reads as unused, and
  deleting it fails with E008. Drop the rename, not the import.
- **`scripts/gate-retry.sh <worktree> <log> [attempts]` is how a long
  gate is actually run here.** It waits for a quiet box, runs
  `gate.sh`, and starts over when the run produced NO VERDICT — which
  is what the RAM guard's kill looks like from outside. It never
  retries a `gate: RED`: a run that reached a verdict has said
  something about the tree and its exit code is passed straight
  through, because a loop that re-rolls a red is a machine for landing
  broken trees. Waiting for quiet is not enough on its own — seven
  gates died in one session on 2026-09-11 and every one had started on
  an idle machine; the spike arrives after the run does.
  `scripts/gate-retry.sh --probe` prints the box reading the wait uses;
  `--read <log>` says what the loop would do with a gate log you
  already have, which is how the three branches are checked without
  waiting for a kill — the same function the loop calls.
- `sbt test` runs everything, JVM + JS + Native. The core suite forks
  (see build.sbt for why); `.jvmopts` gives sbt 6g.
- **The full matrix PASSES: 2422 tests, 81 module runs, 0 failures, 83
  seconds warm** (matrix-143, 2026-09-06). It was believed broken for
  three days and everyone gated on scoped subsets instead, so nobody's
  green was the repository's green. Run the whole thing.
- **THE 143, SOLVED (2026-09-06 evening, adversarial-lanes).** Not an
  agent, not a pkill in anyone's turn, not a process group. Two launchd
  agents from the operator's SCALASCRIPT project run for every session
  on this Mac: `~/Library/LaunchAgents/io.scalascript.build-ram-guard.plist`
  (every 20 s; under memory pressure — available < 3 GB with pageouts —
  kills the heaviest JVM matching `sbt-launch|xsbt.boot|sbt.script|
  sbt/standalone|bloop|scala-cli|…|org.openjdk.jmh`; a clean matrix
  crosses that line 60–90 s in) and `io.scalascript.kill-stale-builders
  .plist` (hourly, `--idle 30 --kill`: no CPU in a 20 s window and up
  > 30 min — a JMH host JVM waiting on its fork is "idle" by that rule,
  dies, and leaves the fork holding `$TMPDIR/jmh.lock`; that is every
  orphan we found). Its log, `~/Library/Logs/kill-stale-builders.log`,
  lists kills by pid and cwd. Proof: a bait with "sbt-launch" in its
  argv died five times, a control without it never; no transcript in
  any project ran a kill by name; the pattern string is in
  scalascript's scripts. The fix is theirs or the operator's
  (`launchctl unload` both while okay builds run); nothing in this
  repository is at fault. If a 143 recurs, read that log first.
- **Exit 143 is SIGTERM — somebody killed the build, no test failed.**
  Two senders found. (1) A run started as `nohup sbt ... &` INSIDE a
  tool call dies when that call's shell exits: the log stops mid-compile
  and nothing says why. Make sbt the background call's OWN command.
  (2) Under a second full matrix on the same box a tracked run still
  took a SIGTERM at 1607 tests; the same tree alone four minutes later
  was green. Mechanism unidentified — if you hit it, you are sharing
  the machine, not looking at a defect.
- **`pgrep -f 'bin/java.*sbt-launch'` MATCHES NOTHING.** sbt's command
  line begins with a plain `java`, so that pattern answers "the box is
  quiet" every time, including while two matrices are running. Use
  `pgrep -f sbt-launch`. This one broken check is most of why 143 was
  blamed on the test suite for three days (matrix-143, 2026-09-06).
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
- **A competitor lane's SOURCE is checked in the competitor's own
  code before the lane is named `_chunk_` or called "native"**
  (benchmark-fairness-audit + fs2-chunked-merge-lanes, 2026-09-06).
  The `_elem_`/`_chunk_` naming rule of 5 Sep caught four mismatches
  on OUR side and missed the fifth because it never looked at the
  other side: `fs2.Stream.range` is `emit(o) ++ go(o + step)` in
  3.10.2 — a singleton per element — so every "chunk-native fs2" row
  was a fold over one-element chunks (44 443 → 109, which is ahead
  of ZIO); `ZStream.iterate` is per-element too (628 → 36.7 from
  `ZStream.range`). Open the constructor you feed a competitor in
  the sources jar in the Coursier cache, note the chunking with a
  file:line in the lane's comment, and keep the old lane renamed as
  the diagnostic it is. Also: every cats lane pays `unsafeRunSync`'s
  7.6 µs thread handoff (§0 of docs/benchmarks.md) and no okay lane
  does — quote it where a cats number is close.
- `organization` is `dev.okay` (build.sbt is the decision in force).
