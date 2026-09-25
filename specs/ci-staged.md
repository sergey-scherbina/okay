# ci-staged — gate your own change before the merge, gate everything once before the push

## Overview

Every lane today lands through the same shape: rebase, `scripts/gate.sh
"affected master"`, `--ff-only`, push. `affected` closes the lane's diff
over its DEPENDENTS (project/Affected.scala), so a lane that touches the
core — or `build.sbt` — runs the whole family, in a worktree of its
own, at the same time as every other lane doing the same. With N agents
landing that is N full gates on one 14-core box, each pushing the others
toward the RAM guard's kill (AGENTS.md, THE 143), each kill a retry,
each retry another full gate. `land.sh` then refuses to land any lane
whose base master has moved by a SOURCE commit, so a sibling's disjoint
one-line change sends everyone back to a fresh full gate. The box
collapses under work that is almost entirely duplicated: the same
dependents, tested N times, for N changes that do not touch each other.

The operator's ask (2026-09-25): **before the merge, test the modules
the lane changed; run the whole build once, after the merges into
master and BEFORE the push.** Three stages:

- **A — pre-merge, per lane.** The lane's OWN projects run their tests;
  their dependents are COMPILED (`Test/compile`), not tested. A broken
  signature shows there, and a dependent's tests would mostly re-check
  what its compile just proved. The build files changing is still the
  whole family — nothing smaller is true.
- **B — post-merge, pre-push, serial, once for everyone.** ONE runner,
  one lock, gates `origin/master..master` with the full dependent
  closure — the closure a lane pays today, paid once per batch of
  landings — and PUSHES on green. Nobody else pushes. Origin only ever
  receives a tree the whole gate has seen.
- **C — red is a revert, not a hunt.** A red range is bisected with the
  same scoped gate; the first bad landing commit is reverted on master
  by the runner, named in the room and in `changelog.d/`, and the next
  turn gates and pushes the range with the revert in it. The author
  re-lands with the fix. Master's health is the runner's job, not each
  author's — and origin never carries the fault at all.

What changes for a lane: its gate shrinks to its own modules, and it
stops pushing. What does not change: the gate still runs before the
merge, the merge is still its own command, and the push still happens
within minutes of landing — by the runner, after the one full gate.

## Interface

### Stage A — `affected <ref> <task> <platform> self`

`project/Affected.scala`, the `affected` command, gains a fourth
argument, the SCOPE:

    affected <git-ref> [task] [jvm|js|native|rest|all] [self|closed]

- `closed` (the default, and today's meaning): `<task>` on the changed
  projects AND every project that depends on them.
- `self`: `<task>` on the changed projects only; on the dependent
  closure minus those, `Test/compile` — so a dependent that no longer
  compiles against the lane's main sources fails the gate, and a
  dependent that does is not tested here. A lane whose diff touched
  only TEST sources compiles no dependents at all (ci-affected-tests-only
  already established that a test cannot break a dependent).
- The BUILD changing (`buildChanged`: root `*.sbt`, `project/`, the
  meta-build's sources) ignores the scope: it is the whole gate, `test`,
  as today. `self` cannot make that smaller and does not pretend to.

The log line names both halves: `affected: 3 file(s) changed since
master: 1 project(s) directly (test), 12 dependents (Test/compile)`.

`scripts/gate.sh "affected master self"` is the pre-merge spelling. The
two-phase JVM-first split (gate-jvm-first) applies to it exactly as to
`affected <ref>`: `affected <ref> test jvm self` then `affected <ref>
test rest self`.

### Stage A — `land.sh`'s re-gate check narrows, and it stops pushing

`scripts/land.sh` step 2 today refuses to land when master gained ANY
source commit since the lane's base. It refuses instead only when the
gained source files INTERSECT the lane's own scope: a file under one of
the lane's direct projects' source directories, or a build file. A
sibling's source change in a disjoint module is rebased onto and
landed; stage B is what checks that the two lanes agree.

Step 8 (`git push origin master`) becomes `scripts/ci-runner.sh kick`.
The runner pushes.

`land.sh` also (a) takes the landing sha from the BRANCH (`git rev-parse
--short feature/<slug>`), never from master after the merge — a sibling
claim has slipped between the two before (memory: landing-sha-from-branch);
and (b) drops the attribution trailer it hard-codes into the
release-claim commit, which the operator's own instructions forbid.

### Stage B — `scripts/ci-runner.sh`

    scripts/ci-runner.sh once          gate origin/master..master, push on green, exit
    scripts/ci-runner.sh loop          `once`, then wait for a kick, repeat
    scripts/ci-runner.sh kick          wake a running loop, or start `once` detached
    scripts/ci-runner.sh status        the lock holder, what is unpushed, pending kicks
    scripts/ci-runner.sh --read <log>  what `once` would do with a gate log

State lives under `.work/ci/` (gitignored, local — the runner is this
box's, as the claims are):

- `.work/ci/lock/` — a directory, taken with `mkdir` (O_EXCL on every
  filesystem here), holding the runner's pid. A second runner reads the
  pid, checks it is alive (`ps -p`), and exits saying so; a dead holder's
  lock is stale and is taken over — named in the log.
- `.work/ci/kick` — touched by `kick`; the loop removes it before a run,
  so a landing DURING a run leaves a kick behind and the loop runs again.
  No landing is lost to a run already in flight.
- `.work/ci/log/<UTC>-<from>..<to>.log` — one gate log per run.

There is no `last-green` file: **what is pushed is what was green.**
`origin/master` is the base, by construction.

`once`:
1. take the lock or exit;
2. `git fetch origin`; `from=origin/master`, `to=$(git rev-parse master)`.
   Equal → nothing to do, exit 0. `from` NOT an ancestor of `to` (origin
   genuinely ahead — edits made on GitHub) → `git merge origin/master`
   on master, MERGE not rebase (AGENTS.md: the boards cite unpushed
   commits by sha), and `to` is the merge; the merged tree is then gated
   like any other range;
3. a range whose diff is board-only (`.work`, `sprint.d`, `backlog.d`,
   `changelog.d`, `docs`, `specs` — `land.sh`'s own list) is pushed at
   once: nothing to gate;
4. otherwise `scripts/gate.sh "affected $from..$to"` — the RANGE form
   `changedSince` already reads, closed over dependents, JVM first. If
   the range touches `okay2/`, `cd okay2 && ../scripts/gate.sh test`
   follows (the separate build has no `affected`; its suite is 80 s warm);
5. GREEN → `git push origin master`; room: `ci: pushed <from>..<to>
   (N landings)`. A REJECTED push means origin moved during the run:
   back to step 2, same turn.
   RED → stage C. KILLED/STALLED → `gate-retry.sh`'s rule: no verdict,
   try again, up to the same attempt bound; the log names each try.

`kick` with no loop running starts `once` DETACHED — a double-fork
subshell, `(nohup sh scripts/ci-runner.sh once </dev/null >log 2>&1 &)`,
because a background job started inside a tool call's shell dies with
that shell (AGENTS.md, "Exit 143 is SIGTERM", sender 1). The runner runs
from the MAIN checkout: it reads master and pushes it; it never edits a
worktree and never touches anything uncommitted there.

### Stage C — bisect and revert

On RED over `from..to` with more than one LANDING commit in the range
(a landing = a commit whose diff is not board-only — the same filter as
step 3):

1. `git bisect start $to $from` in a DETACHED WORKTREE of its own
   (`../okay-ci-bisect`), never in the main checkout — a bisect there is
   a `checkout` in the checkout everybody merges into; `git bisect run
   sh scripts/gate.sh "affected $from..HEAD"`;
2. the first bad commit `C` is reverted on master: `git revert --no-edit
   C`; the revert commit's message names `C`, its lane (from the
   `release-claim` that follows it, when one does) and the runner's log;
3. `changelog.d/ci-revert-<slug>.md` is written with the same facts and
   committed with the revert; the room gets `ci: RED <from>..<to> —
   reverted <C> (<slug>); re-land with the fix`;
4. the loop's next turn gates `from..<revert>` and pushes it green. The
   culprit and its revert reach origin TOGETHER, or not at all.

One landing commit in the range needs no bisect: it is the culprit.

A red that `gate.sh` already knows to be false (`native-runner-error`'s
lost process) is re-run alone by `gate.sh` itself before the verdict;
a red that survives is a red. The runner never widens an assertion and
never retries a `gate: RED`, exactly as `gate-retry.sh` does not.

### The push rule

AGENTS.md's "PUSH WHAT YOU LAND, IMMEDIATELY" becomes "LAND, THEN KICK
THE RUNNER — the runner pushes". The reason the old rule was written
(origin 60 commits behind, submodule consumers blocked) is served
better, not worse: the runner pushes within one gate of every landing
and pushes only what the whole gate has seen. A `claim:` commit is not
pushed by hand either — the next turn's board-only range pushes it at
once (step 3), and a claim is local coordination in the first place.
The one push a human still makes by hand is none.

## Behavior

Stage A — `self` scope (`project/Affected.scala`; `TestAffectedSelf` in
okay-deploy beside `TestDocSnippets`, running `sbt "affected <ref> test
all self"` against a fixture worktree and reading the log line, since
the plugin is the meta-build's and cannot be loaded into a test JVM):

- [ ] a diff touching only `okay-lex/src/main` runs `okayLexJVM/test`
      (and JS/Native) and `Test/compile` on okay-parse, okay-codec and
      the rest of okay-lex's dependent closure — no dependent's `test`
- [ ] a diff touching only `okay-lex/src/test` runs okay-lex's own
      tests and compiles NO dependent
- [ ] a diff touching `build.sbt` runs `test` on the whole gate under
      `self` exactly as under `closed`
- [ ] a diff touching the core (`src/main/scala`) under `self` runs the
      core's own tests and `Test/compile` on every module — the whole
      family compiled, not tested
- [ ] `closed` (and no fourth argument) behaves exactly as `affected`
      does today — the nightly and CI's push job are unchanged
- [ ] the log line names the direct count with its task and the
      dependent count with `Test/compile`
- [ ] `scripts/gate.sh "affected master self"` runs the JVM arm first
      and the rest only on green, as `affected master` does
- [ ] `land.sh` rebases and lands when master's gained source files lie
      outside the lane's direct projects and the build; refuses, naming
      the files, when one lies inside
- [ ] `land.sh`'s `landed as` names the branch tip; the release-claim
      commit carries no attribution trailer; step 8 kicks and does not
      push

Stage B — the runner (`scripts/ci-runner-selftest.sh`, in the style of
`gate-selftest.sh`: a fixture repo with a bare `origin`, a fake
`gate.sh` that answers what the test says, under both `sh` and `bash`):

- [ ] `once` with `origin/master == master` does nothing and exits 0
- [ ] `once` on a board-only range pushes without running the gate
- [ ] `once` on GREEN pushes exactly the gated `to` — not a master that
      moved during the run; the moved-past commits wait for the next turn
- [ ] `once` on RED pushes nothing
- [ ] `once` with origin genuinely ahead merges (a merge commit, no
      rebase), gates the merged tree, pushes it
- [ ] a second `once` while the first holds the lock exits without
      running, naming the holder's pid
- [ ] a lock whose pid is dead is taken over, and the log says so
- [ ] a `kick` during a run leaves the loop a second run; two kicks
      during a run leave one
- [ ] `kick` with no loop running starts `once` detached: the kicking
      shell exits at once and the run reaches a verdict after it
- [ ] a range touching `okay2/` also gates okay2's own build; one that
      does not, does not

Stage C — bisect and revert (the same selftest, a fixture history of
landings with one the fake gate calls bad):

- [ ] a red range with one landing commit reverts it without a bisect
- [ ] a red range with several landing commits reverts exactly the first
      bad one and no other
- [ ] the revert commit names the culprit sha and its lane's slug;
      `changelog.d/ci-revert-<slug>.md` exists and names the same
- [ ] nothing is pushed on the red turn; the next turn pushes the range
      with the revert, culprit and revert together
- [ ] the bisect runs in its own detached worktree; the main checkout's
      HEAD never leaves master

Policy (AGENTS.md, this lane edits it):

- [ ] AGENTS.md "Before merging" names `scripts/gate.sh "affected master
      self"` as the pre-merge gate, the runner as the pre-push one, the
      revert as the runner's answer to red, and rewrites the PUSH rule
      as "land, then kick" — saying why (this section's numbers)

## Out of scope

- GitHub Actions. The push job there already runs `affected before..sha`
  closed over dependents on a runner of its own; it is a second full
  gate for what the local runner already pushed. This spec is the LOCAL
  box, where the collapse is.
- Making the runner a launchd agent. `loop` in a terminal the operator
  owns, or `kick`'s detached `once`, is enough to measure with; a
  launchd plist is a follow-up once the loop has run a day.
- Auto-re-landing a reverted lane. The author does that, with the fix.
- Changing what `gate.sh` counts as a verdict, a warning, a stall or a
  kill. The runner calls `gate.sh` and reads its lines; it adds nothing
  to their meaning.
- `okay2`'s own `affected`. Its whole suite is 80 s warm; scoping it
  buys nothing yet. Filed when it does.

## Design

**Why `Test/compile` and not nothing for the dependents.** The thing a
lane can break downstream without touching downstream is a signature —
a renamed method, a changed type, a removed given. That is a compile
error in the dependent, and `Test/compile` catches it in the dependent's
own tests too (a test calling the removed method). What `Test/compile`
does not catch is a BEHAVIOUR change under an unchanged signature, and
that is exactly what stage B exists for — once, for the batch, before
anything is pushed.

**Why one runner and a lock, not a queue service.** The claims are
files in `.work/active/`; the lock is a directory in `.work/ci/`. Same
box, same filesystem, same `mkdir` atomicity the claims already lean
on (memory: okay-claim-discipline, O_EXCL claims). Nothing to install,
nothing to keep running that `ps` cannot show.

**Why `origin/master` is the base and there is no `last-green`.** The
runner is the only pusher, so what origin holds is exactly what the
runner gated green — one fact in one place, kept by git. A state file
would be a second copy of it that could disagree.

**Why bisect with the scoped gate.** `affected $from..HEAD` at each
bisect step gates only what the range up to that step touched, so a
bisect over five disjoint lanes costs five scoped gates, not five full
ones — the same economy stage A buys, applied to the hunt.

**Why the runner reverts and does not just report.** A red master that
waits for its author is a red master every other lane rebases onto,
and every one of them reads the same red as its own. A revert within
one runner turn keeps master something a lane can rebase onto without
inheriting a fault it did not write — and, because the push waits for
green, keeps the fault off origin entirely.

**What a lane loses.** A cross-module interaction — lane X changes the
core's behaviour under an unchanged signature, lane Y's module depends
on the old behaviour — used to fail in X's own pre-merge gate (Y was in
X's closure). Now it fails in the runner, after X has merged, and X is
reverted before it is pushed. X's author learns the same fact one merge
later; Y's author never waited for X's gate; origin never saw X.

## Decisions

- **`self` compiles dependents' TESTS (`Test/compile`), not only their
  mains** — chosen because a dependent's test is where a removed given
  or a renamed method it relied on is called most; `compile` alone would
  pass a lane that broke every downstream test's compile. Rejected:
  `compile` only (misses exactly that); `test` on direct dependents only
  (an arbitrary depth; the boundary the operator drew is "mine vs not").
- **A fourth argument, not a new command** — chosen so `gate.sh`'s
  two-phase logic and CI's `affected before..sha` keep one code path.
  Rejected: an `affected-self` command (two spellings of one graph walk).
- **The full gate runs BEFORE the push, and the runner is the only
  pusher** — the operator's refinement over a first draft that gated
  after the push and kept a `last-green` file: a red never reaches
  origin, and the base needs no state of its own. Rejected: gate after
  push with revert (origin carries the fault for a turn; consumers
  bumping the submodule in that window get it).
- **The runner gates a RANGE (`origin/master..master`), not the tip** —
  `changedSince` already has the `a..b` form and it is what "what did
  these landings touch" means. Rejected: the working-tree form (the main
  checkout carries siblings' uncommitted files, which the runner must
  not read).
- **Revert on red** — per the trade-off above. Rejected: leave red and
  page the author (every other lane inherits the red until they wake);
  auto-re-run once (a machine for landing broken trees, `gate-retry.sh`'s
  own reason for never retrying a RED).

## Results

_Filled after each lane lands: the pre-merge gate's size before and
after for a core lane and a leaf lane (projects tested, projects
compiled, wall time at the same load); the runner's first week (turns,
pushes, reds, reverts, the longest time from a landing to its push);
and the collapse the operator described measured against it (load
average during landings, gates killed by the RAM guard per day)._
