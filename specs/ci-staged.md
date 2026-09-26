# ci-staged — your modules first, then their dependents, before the merge; the whole build once, before the push

## Overview

Every lane today lands through the same shape: rebase, `scripts/gate.sh
"affected master"`, `--ff-only`, push. `affected` closes the lane's diff
over its DEPENDENTS (project/Affected.scala) and runs the closure as ONE
`all`, in whatever order sbt's scheduler takes — so a lane learns that
its own module is red only after the dependents it dragged in have
compiled and started. Every lane does this in a worktree of its own, at
the same time as every other lane, and `land.sh` then refuses to land any
lane whose base master has moved by a SOURCE commit, so a sibling's
disjoint one-line change sends everyone back to a fresh full gate. With
N agents landing that is N family-sized gates on one 14-core box, most
of them re-checking each other, each pushing the others toward the RAM
guard's kill (AGENTS.md, THE 143), each kill a retry, each retry another
gate. The box collapses under duplicated work.

The operator's shape (2026-09-25, stated twice, the second time after a
draft of this spec had the dependents only compiled): **before the merge,
run the tests of the modules that changed, THEN the tests of the modules
that depend on them — one list, in that order. Run the whole build in
master once, before master is pushed.** Three stages:

- **A — pre-merge, per lane, ordered.** The lane's changed projects run
  their tests first; their dependents run theirs second, as a separate
  sbt command — a red in what the lane wrote stops the run before one
  dependent is paid for. The set is what `affected` runs today; the
  order is new. And a sibling's landing in a module the lane never
  touched no longer forces a re-gate: that interaction is stage B's.
- **B — post-merge, pre-push, serial, once for everyone.** ONE runner,
  one lock, runs the WHOLE build on master (`family all`, plus okay2's
  suite when the range touched it) and PUSHES on green. Nobody else
  pushes. Origin only ever receives a tree the whole gate has seen.
- **C — red is a revert, not a hunt.** A red run is bisected over the
  landings since the last push, with the scoped `affected` gate; the
  first bad landing is reverted on master by the runner, named in the
  room and in `changelog.d/`, and the next turn gates and pushes the
  range with the revert in it. The author re-lands with the fix.
  Master's health is the runner's job, not each author's — and origin
  never carries the fault at all.

What changes for a lane: its gate fails fast on its own modules, it
re-gates only for neighbours in its own modules, and it stops pushing.
What does not change: the gate still runs before the merge, the merge is
still its own command, and the push still happens within minutes of
landing — by the runner, after the one whole build.

## Interface

### Stage A — `affected <ref> <task> <platform> staged`

`project/Affected.scala`, the `affected` command, gains a fourth
argument, the ORDER:

    affected <git-ref> [task] [jvm|js|native|rest|all] [staged|closed]

- `closed` (the default, and today's meaning): one `all` over the
  changed projects and every project that depends on them.
- `staged`: the same set as TWO sbt commands queued in sequence — `all`
  over the changed projects, then `all` over the dependents minus them.
  sbt stops at the first command that fails, so the second never runs
  on a red first. A lane whose diff touched only TEST sources has no
  second stage (ci-affected-tests-only: a test cannot break a dependent).
- The BUILD changing (`buildChanged`: root `*.sbt`, `project/`, the
  meta-build's sources) collapses the two into one: every project
  changed, nothing is "first".

The log line names both stages: `affected: 3 file(s) changed since
master: 1 project(s) directly, then 12 dependents`, and each stage is
announced as it runs: `running test on 3 changed project(s): …`, then
`running test on 12 dependent project(s): …`.

Two flags, for the selftest and for reading a plan without paying for
it: `--plan` prints the stages and runs nothing; `--files=a,b,c` takes
the changed files from the argument instead of git.

`scripts/gate.sh "affected master staged"` is the pre-merge spelling. The
two-phase JVM-first split (gate-jvm-first) applies to it as to `affected
<ref>`: `affected <ref> test jvm staged` then `affected <ref> test rest
staged` — so the order on the box is changed-JVM, dependents-JVM,
changed-rest, dependents-rest.

### Stage A — `land.sh`'s re-gate check narrows, and it stops pushing

`scripts/land.sh` step 2 today refuses to land when master gained ANY
source commit since the lane's base. It refuses instead only when a
gained source file lies in a MODULE the lane's own diff touched, or is a
build file (on either side). A sibling's source change in a module the
lane never touched is rebased onto and landed; stage B is what checks
that the two lanes agree. "Module" is the first path component
(`okay-lex/…`), the core being `src/`, and `project/` plus the root
`*.sbt` being the build — one module per top-level directory is this
repository's layout, the same seam `Affected.scala` reads through the
build's own directories.

Step 8 (`git push origin master`) becomes `scripts/ci-runner.sh kick`
once the runner exists (lane 2). Until then it pushes, as today.

`land.sh` also (a) takes the landing sha from the BRANCH (`git rev-parse
--short feature/<slug>`), never from master after the merge — a sibling
claim has slipped between the two before (memory: landing-sha-from-branch);
and (b) drops the attribution trailer it hard-codes into the
release-claim commit, which the operator's own instructions forbid.

### Stage B — `scripts/ci-runner.sh`

    scripts/ci-runner.sh once          whole build on master, push on green, exit
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
4. otherwise the WHOLE build: `scripts/gate.sh "family all"` — the
   nightly's set, all platforms, on `to` — and, if the range touches
   `okay2/`, `cd okay2 && ../scripts/gate.sh test` after it (the separate
   build; 80 s warm). The whole build and not `affected from..to`,
   because the operator asked for the whole build and because this is
   the one run that pays for everything once instead of N times;
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

**Zero — a RED with no `==> X` names no test at all**
(ci-runner-reverts-on-infra-red, 2026-09-25): `stack-safety-json` was
reverted for a Native test binary killed by signal 9 and an
`okayAsyncNative` accept timeout — `gate.sh`'s own words for this shape
are "a failure this script does not recognise", and the reverted module
did not even depend on the one the lane touched. Checked with the exact
test `gate.sh` itself uses (`grep -q "==> X"` on the run's own log)
BEFORE anything below runs: no match means this is a SIGNAL, not a
verdict — treated like `gate: KILLED`/`gate: STALLED`, no bisect, no
revert, retried on the next kick.

Otherwise, on RED over `from..to` with more than one LANDING commit in
the range (a landing = a commit whose diff is not board-only — the same
filter as step 3):

1. `git bisect start $to $from` in a DETACHED WORKTREE of its own
   (`../okay-ci-bisect`), never in the main checkout — a bisect there is
   a `checkout` in the checkout everybody merges into; `git bisect run
   sh scripts/gate.sh "affected $from..HEAD"` — the SCOPED gate here,
   because a bisect over five disjoint landings then costs five scoped
   gates and not five whole builds;
2. **CONFIRM before reverting** (ci-runner-revert-needs-confirmation,
   2026-09-25 — two real reverts landed on a real red that never
   repeated: `TestSignals` under load, a Native runner killed by signal
   9). Wait for quiet (the SAME `quiet()` gate-retry.sh and
   `jmh-lane.sh` use — a re-run on the same noisy box just repeats the
   same false red), then run `sh scripts/gate.sh "affected $from..C"`
   ONCE MORE, alone. GREEN here means a flake: log it, do NOT revert
   and do NOT push — the next whole-build turn re-tests `from..to`
   fresh, on (hopefully) a quieter box, and if the range really is fine
   nothing was lost by not reverting. Only a SECOND red confirms `C`;
3. the first bad commit `C` is reverted on master: `git revert --no-edit
   C`; the revert commit's message names `C`, its lane (from the
   `release-claim` that follows it, when one does) and the runner's log;
4. `changelog.d/ci-revert-<slug>.md` is written with the same facts and
   committed with the revert; the room gets `ci: RED <from>..<to> —
   reverted <C> (<slug>); re-land with the fix`;
5. the loop's next turn runs the whole build on `<revert>` and pushes it
   green. The culprit and its revert reach origin TOGETHER, or not at all.

One landing commit in the range needs no bisect: it is the culprit,
straight into step 2 (confirm) above.

A red that `gate.sh` already knows to be false (`native-runner-error`'s
lost process) is re-run alone by `gate.sh` itself before the verdict;
step 2 above is the identical instinct applied to the CULPRIT a bisect
names, generically, rather than to one known failure shape. A red that
survives BOTH the original run and the confirmation is a red. The
runner never widens an assertion and never retries a confirmed
`gate: RED`, exactly as `gate-retry.sh` does not.

### The push rule

AGENTS.md's "PUSH WHAT YOU LAND, IMMEDIATELY" becomes, when the runner
lands, "LAND, THEN KICK THE RUNNER — the runner pushes". The reason the
old rule was written (origin 60 commits behind, submodule consumers
blocked) is served better, not worse: the runner pushes within one whole
build of every landing and pushes only what that build has seen. A
`claim:` commit is not pushed by hand either — the next turn's board-only
range pushes it at once (step 3). The one push a human still makes by
hand is none.

### Stage D — the same lock discipline for benchmarks (`scripts/jmh-lane.sh`)

The operator's own generalization (2026-09-25): the shape is not really
about tests, it is about a SHARED BOX, and a JMH benchmark loads that box
exactly as a test gate does — with the extra failure mode that
contention does not fail the run, it makes the NUMBER wrong, silently.
`scripts/quiet.sh` factors `gate-retry.sh`'s own `quiet()`/`kill_tree`
out into a file both it and `jmh-lane.sh` source, so the one threshold
set (`busy-sbt=0`, `load<15`, `free>=10GB`) never drifts between the two
consumers. `jmh-lane.sh "<sbt Jmh/run command>" [attempts]` takes its
OWN lock (`.work/jmh/lock`, separate from the runner's `.work/ci/lock` —
`quiet()` itself is what keeps a lane and a whole-build gate from
overlapping, since a live gate is a busy-sbt process either one sees),
waits for quiet before EVERY attempt (not only the first — a
contamination-triggered retry must not blindly re-run into the same
busy box), and — the half a test gate never needs — checks quiet AGAIN
right after the run; a busy reading there discards the result and
retries the same lane, up to 5 attempts by default. `scripts/gate.sh` is
NOT used for the sbt invocation itself: it parses a test summary line
and a JMH result table is not one, matching `scripts/ab-defaults.sh`'s
own existing precedent of a bare `$SBT` call for `Jmh/run`.
`scripts/jmh-lane-selftest.sh` (6 cases, fixture-based, a fake
`quiet.sh` popping scripted quiet/busy answers off a queue file) found
the one real bug: the first draft waited for quiet only ONCE, before the
whole retry loop, so a contamination-triggered retry re-ran immediately
into whatever the box was doing, never re-checking.

## Behavior

Stage A — `staged` order (`project/Affected.scala`;
`scripts/affected-selftest.sh` runs six `--plan --files=…` commands in
one sbt start and reads the log, since the plugin is the meta-build's and
cannot be loaded into a test JVM):

- [x] a diff touching only `okay-lex/src/main` runs stage 1 `test` on
      the three okay-lex projects and stage 2 `test` on okay-lex's
      dependent closure (125 projects, 2026-09-25)
- [x] a diff touching only `okay-lex/src/test` runs the three and has no
      second stage
- [x] a diff touching `build.sbt` runs `test` on the whole gate as ONE
      stage under `staged` exactly as under `closed`
- [x] a diff touching the core (`src/main/scala`) under `staged` runs the
      core's three first and every module (169) second
- [x] `closed` (and no fourth argument) behaves exactly as `affected`
      does today — the nightly and CI's push job are unchanged
- [x] an order that is neither `staged` nor `closed` is refused, named
- [x] `scripts/gate.sh "affected master staged"` runs the JVM arm first
      and the rest only on green, as `affected master` does, the order
      riding on both phases; a four-argument form is passed through
      (`gate-selftest.sh` case 8)
- [x] `land.sh` rebases and lands when master's gained source files lie
      outside the lane's own modules and the build; refuses, naming the
      modules, when one lies inside (dry-run on this lane against real
      master, 2026-09-25: refused naming BUILD — master had gained
      `build.sbt`, this lane touches `project/`; the classifier checked
      on eight paths)
- [x] `land.sh`'s `landed as` names the branch tip; the release-claim
      commit carries no attribution trailer (ci-staged's own commit
      used this and landed clean; land.sh's step 8 is now `ci-runner.sh
      kick`, see Stage B)

Stage B — the runner (`scripts/ci-runner-selftest.sh`, in the style of
`gate-selftest.sh`: a fixture repo with a bare `origin`, fake
`gate.sh`/`gate-retry.sh` that answer what the test says, under both
`sh` and `bash`, 11 cases):

- [x] `once` with `origin/master == master` does nothing and exits 0
- [x] `once` on a board-only range pushes without running the gate —
      found and fixed a real bug getting here: a git PATHSPEC built
      from a shell variable (`':!changelog.d'`) does not work, because
      word-splitting a variable's value does not strip the quotes it
      contains; `is_board_only_files` filters the name LIST in the
      shell instead
- [x] `once` on GREEN pushes exactly the gated `to` — by construction
      (`to` is captured once, before the gate runs, and is the only
      value ever pushed); not separately raced against a landing
      arriving mid-gate
- [x] `once` on RED pushes nothing
- [x] `once` with origin genuinely ahead merges (a merge commit, no
      rebase), gates the merged tree, pushes it
- [x] a second `once` while the first holds the lock exits without
      running, naming the holder's pid
- [x] a lock whose pid is dead is taken over, and the log says so
- [ ] a `kick` during a run leaves the loop a second run; two kicks
      during a run leave one — NOT covered: needs a `loop` process
      actually mid-gate to kick against, which the fixture's synchronous
      fake gate does not produce. Filed as a follow-up, not blocking.
- [x] `kick` with no loop running starts `once` detached: the kicking
      shell returns before the push happens; the test polls origin for
      up to 10s and finds it updated
- [ ] a range touching `okay2/` also gates okay2's own build; one that
      does not, does not — implemented (see Interface) but not
      exercised by the fixture, which has no okay2/. Filed as a
      follow-up.

Stage C — bisect and revert (the same selftest, a fixture history of
four landings — two plain, one that introduces a marker file, one
after it — with the fake gate red exactly when the marker is present
in the tree, so a REAL `git bisect run` has something to find):

- [x] a red with no `==> X` anywhere in the log (infrastructure noise —
      a killed Native process, an accept timeout) is never bisected or
      reverted; the next turn re-tests the SAME range and, once
      genuinely green, pushes the original commit unchanged
- [x] a red range with one landing commit reverts it without a bisect
- [x] a red range with several landing commits reverts exactly the
      first bad one and no other (4 landings, the marker-introducing
      one alone reverted; the two innocent ones survive)
- [x] the revert commit names the culprit sha and its lane's slug;
      `changelog.d/ci-revert-<slug>.md` exists and names the same
- [x] nothing is pushed on the red turn; the next turn pushes the range
      with the revert, culprit and revert together
- [x] the bisect runs in its own detached worktree; the main checkout's
      HEAD never leaves master, and the bisect worktree is removed after
- [x] the culprit (sole or bisected) is confirmed on its OWN scoped gate,
      alone, before the revert — a fixture case (10b) where the
      confirmation comes back GREEN reverts nothing, pushes nothing, and
      the next (genuinely green) turn pushes the ORIGINAL, never-guilty
      commit unchanged
- [x] the confirmation waits for a quiet box first, the same `quiet()`
      as `gate-retry.sh`/`jmh-lane.sh` — verified by the shared source,
      not a second copy of the threshold

Policy (AGENTS.md):

- [x] "Before merging" names `scripts/gate.sh "affected master staged"`
      and the narrowed re-gate rule (landed with stage A); the PUSH
      rule is rewritten as "land, then kick" (this lane), with a
      documented fallback for a checkout that predates the runner

Stage D — benchmarks (`scripts/jmh-lane-selftest.sh`, 6 cases, a fixture
directory with a fake `quiet.sh` popping scripted answers off a queue
file and a fake `sbt` that always succeeds):

- [x] quiet throughout: one attempt, exit 0
- [x] the box gets busy DURING the run: that attempt's result is
      discarded and reported as contaminated; the SAME lane is retried
      and its own clean result is trusted
- [x] the box never stays quiet through a whole lane: gives up at the
      attempt cap (default 5, tested at 3) and exits 99, distinct from
      a real sbt failure
- [x] a second lane while one holds `.work/jmh/lock` refuses, naming
      the holder's pid — a SEPARATE lock from `.work/ci/lock`
- [x] a lock whose pid is dead is taken over, and it says so
- [x] the lock is released whether the lane succeeded or not
- [x] `quiet()`/`kill_tree` live in one file (`scripts/quiet.sh`),
      sourced by both `gate-retry.sh` and `jmh-lane.sh` — verified by
      running `gate-retry.sh --probe` and `quiet.sh --probe` from both
      the main checkout and a worktree and reading identical numbers

- [x] the bisect tests LANDING TIPS only — commits a `release-claim:
      …, landed as <sha>` names; every other commit of a lane is skipped
      (exit 125), and a range with no release-claim bisects every commit
      as before (ci-runner-bisect-intermediate-commits, 2026-09-26;
      selftest case 12). Found by a false revert: parquet-codec's FIRST
      commit, red on the docs index its own later commit added, was
      bisected, "confirmed" and reverted while the lane's tip was green;
      the whole-build red was two load flakes elsewhere
- [x] a revert that conflicts ABORTS itself and says so: the main
      checkout is never left mid-revert (selftest case 13) — the false
      revert above conflicted with the lanes built on it and blocked every
      sibling's `merge --ff-only` until a human ran `git revert --abort`
- [ ] NOT YET: the confirmation re-runs the culprit's scoped gate on the
      MAIN checkout's tree (HEAD), not the culprit's; for a culprit that
      changed build.sbt that set is everything, so a flake at HEAD can
      "confirm" it. And a landing is reverted as its one tip commit, not
      as the lane's commits. Both stay in backlog
      (ci-runner-confirm-at-culprit)

## Out of scope

- GitHub Actions. The push job there already runs `affected before..sha`
  closed over dependents on a runner of its own; it is a second gate for
  what the local runner already pushed. This spec is the LOCAL box, where
  the collapse is.
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

**Why the order buys something when the set is the same.** A lane's own
module is where its red lives nine times in ten; under one unordered
`all` that red arrives after sbt has compiled and started the dependents
it scheduled alongside. Two commands make the first red the LAST thing
the box pays for. And what the box was actually collapsing under was not
one lane's closure but N lanes each re-gating the family for each
other's disjoint landings — that is the `land.sh` narrowing, which needs
no change to the set at all.

**Why the runner runs the WHOLE build and not `affected from..to`.**
The operator asked for the whole build before the push, and it is the
one place that is right: the runner is serial, so the family is paid
once per batch of landings however many there were, and "the family is
green at this sha" is a fact the nightly can then skip re-proving.

**Why one runner and a lock, not a queue service.** The claims are
files in `.work/active/`; the lock is a directory in `.work/ci/`. Same
box, same filesystem, same `mkdir` atomicity the claims already lean
on (memory: okay-claim-discipline, O_EXCL claims). Nothing to install,
nothing to keep running that `ps` cannot show.

**Why `origin/master` is the base and there is no `last-green`.** The
runner is the only pusher, so what origin holds is exactly what the
runner gated green — one fact in one place, kept by git. A state file
would be a second copy of it that could disagree.

**Why the runner reverts and does not just report.** A red master that
waits for its author is a red master every other lane rebases onto,
and every one of them reads the same red as its own. A revert within
one runner turn keeps master something a lane can rebase onto without
inheriting a fault it did not write — and, because the push waits for
green, keeps the fault off origin entirely.

**What a lane loses.** A cross-module interaction — lane X changes a
module's behaviour, lane Y lands beside it in a module X never touched,
and the two disagree — used to be caught by whichever landed second
being forced to re-gate over the first. Now it is caught by the runner,
after both have merged, and the culprit is reverted before it is
pushed. Both authors learn the same fact one merge later; neither
waited for the other's gate; origin never saw it.

## Decisions

- **Dependents are TESTED pre-merge, in a second stage — not compiled
  only.** The first draft of this spec had `self`: the changed projects
  tested, the dependents `Test/compile`d, on the reasoning that a
  behaviour change under an unchanged signature is what stage B is for.
  The operator overruled it, twice, with the shape above: the same set,
  ordered. Recorded so the next agent does not re-propose it: the
  operator's win is the fail-fast order plus the narrowed re-gate plus
  one whole build per batch, not a smaller pre-merge set.
- **A fourth argument, not a new command** — so `gate.sh`'s two-phase
  logic and CI's `affected before..sha` keep one code path. Rejected: an
  `affected-staged` command (two spellings of one graph walk).
- **The whole build runs BEFORE the push, and the runner is the only
  pusher** — the operator's refinement over a draft that gated after the
  push and kept a `last-green` file: a red never reaches origin, and the
  base needs no state of its own. Rejected: gate after push with revert
  (origin carries the fault for a turn; consumers bumping the submodule
  in that window get it).
- **The runner runs `family all`, the bisect runs `affected from..HEAD`**
  — the whole build where one run covers everybody, the scoped gate
  where a run per candidate would otherwise be a whole build each.
- **Revert on red** — per the trade-off above. Rejected: leave red and
  page the author (every other lane inherits the red until they wake);
  auto-re-run once (a machine for landing broken trees, `gate-retry.sh`'s
  own reason for never retrying a RED).

## Results

Stage A, 2026-09-25, `--plan` on this box: an okay-lex main change is
3 projects then 125 dependents; a test-only change 3 then none; the
core 3 then 169; `build.sbt` 182 as one stage; `closed` 128 as one —
the same 128 `affected master` ran before. The pre-merge gate for THIS
LANE (build files changed — `project/Affected.scala`, `scripts/`)
was, correctly, the whole family under `staged`: 7441 tests, 330
module compiles, GREEN.

Stage B/C, `ci-runner-selftest.sh`, 11 cases, fixture repo (never the
real okay repo): every case in the spec's Behavior list passed except
two explicitly filed as follow-ups (a live `loop` process kicked mid-run;
an `okay2/`-touching range) which the synchronous fixture cannot
produce. One real bug found and fixed by the selftest itself: a git
pathspec built from a shell variable does not work (word-splitting
does not strip the quotes the variable's value contains), which would
have sent every board-only landing through a full, wasted gate.

The runner's first week in production, and the collapse measured
against it (load average during landings, RAM-guard kills per day):
filled once `ci-runner.sh loop` has actually run that long.
