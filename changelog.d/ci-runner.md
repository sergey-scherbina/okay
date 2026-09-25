## ci-runner (stage B/C) - the one serial, whole-build, pre-push gate

Stage A (ci-staged) shrank the pre-merge gate to a lane's own modules,
but the box was still collapsing under N lanes each pushing directly:
`land.sh` step 8 was `git push origin master`, so every landing risked
putting an untested-together tree on `origin`. Stage B closes it:
`scripts/ci-runner.sh` is now the ONE process that pushes.

`once`: takes a `mkdir` lock under `.work/ci/` (gitignored — local
state, like the claims are local coordination but TRACKED; this is
not), fetches, and reads `origin/master..master`. Nothing to do →
exit 0. Board-only (`.work`, `sprint.d`, `backlog.d`, `changelog.d`,
`docs`, `specs` — `land.sh`'s own list) → pushed at once, no gate.
Origin genuinely ahead (edits made on GitHub) → MERGED, never rebased
(AGENTS.md: the boards cite unpushed commits by sha) — then gated like
any other range. Otherwise: `scripts/gate.sh "family all"` via
`scripts/gate-retry.sh` (extended with an optional fourth `[cmd]`
argument so the runner gets the same quiet-wait/stall-watchdog/
kill-retry any lane already gets, for free), plus okay2's own suite
when the range touches `okay2/`. GREEN pushes exactly the gated
commit; a rejected push (a concurrent push) re-reads and retries the
same turn. `loop` repeats `once` on a `kick`; `kick` wakes a running
loop or starts one detached run — the double-fork-and-nohup shape
AGENTS.md's "Exit 143 is SIGTERM" already documents, so the kicking
shell (a tool call, a hook, `land.sh` itself) is never the process
that has to survive the whole gate.

`land.sh` step 8 is now `sh scripts/ci-runner.sh kick`, not a push —
with a documented fallback (push it yourself) for a checkout that
predates this lane. AGENTS.md's PUSH rule is rewritten: "land, then
kick — the runner pushes."

Stage C: a RED run bisects the landings since the last push with the
SCOPED `affected from..HEAD` gate (a bisect over five disjoint lanes
costs five scoped gates, not five whole builds), in a worktree of its
own — the main checkout's `master` never moves for it. One landing
commit in the range skips the bisect. The first bad commit is
reverted, `changelog.d/ci-revert-<slug>.md` is written and folded into
the same commit, and the NEXT turn gates and pushes the range with the
revert already in it — origin never sees the culprit at all.

`scripts/ci-runner-selftest.sh`: a fixture repo with a bare `origin`
and fake `gate.sh`/`gate-retry.sh` (never the real okay repo, never
sbt), 11 cases covering the spec's Behavior list. It found a real bug
before landing: `git diff -- $PATHSPEC` where `$PATHSPEC` is an
unquoted shell variable containing literal quote characters
(`':!changelog.d'`) does not work — word-splitting a variable's value
does not strip quotes, which only mean something at PARSE time — so
every board-only landing was running the whole gate instead of pushing
at once. Fixed by filtering the changed-file list in the shell instead
of building a pathspec. Two Behavior items are explicitly unexercised
(a `loop` kicked mid-run; an `okay2/`-touching range) and filed as
follow-ups rather than faked.
