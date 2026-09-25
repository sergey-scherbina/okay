#!/bin/sh
# ci-runner.sh — the ONE serial, pre-push gate (specs/ci-staged.md,
# stage B/C). Run from the MAIN checkout, never a worktree: it reads
# LOCAL master, gates it, and — only on green — pushes it. Nobody else
# pushes; `land.sh` (ci-staged, stage A) already stopped, and this is
# what starts instead.
#
#   scripts/ci-runner.sh once          gate origin/master..master, push on green, exit
#   scripts/ci-runner.sh loop          `once`, then wait for a kick, repeat
#   scripts/ci-runner.sh kick          wake a running loop, or start `once` detached
#   scripts/ci-runner.sh status        the lock holder, what is unpushed, pending kicks
#   scripts/ci-runner.sh --read <log>  what `once` would have done with a gate log you have
#
# WHY THE WHOLE BUILD AND NOT `affected`: the operator's own words,
# twice — the pre-merge gate (stage A, `affected … staged`) is scoped
# to the lane; this runner is where the WHOLE build runs, once, for
# however many lanes landed since the last push. It calls
# `scripts/gate-retry.sh` for the same reason any lane does: a full
# build on a shared box meets the RAM guard, and the retry-on-KILLED,
# wait-for-quiet, stall-watchdog logic there is not this script's to
# re-invent.
#
# STATE is under .work/ci/ (gitignored — local to this box, like the
# claims are local coordination but TRACKED; this is not):
#   .work/ci/lock/          a directory (mkdir is O_EXCL everywhere
#                            here), holding lock/pid — the runner's pid
#   .work/ci/kick           touched by `kick`; a run in progress leaves
#                            it for the next turn instead of losing it
#   .work/ci/log/<UTC>-<from>..<to>.log   one gate log per run
#
# There is no "last-green" file. What is PUSHED is what was green:
# origin/master is the base, always, so the fact and its record are
# the same place.
set -u
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/.." && pwd)"
cd "$root"
LOCKDIR="$root/.work/ci/lock"
KICK="$root/.work/ci/kick"
LOGDIR="$root/.work/ci/log"
mkdir -p "$LOGDIR"

# ---- the board-only test: land.sh's own list (ci-staged) --------------
# A range whose diff touches none of these has nothing a gate could
# catch — pushed at once, no run started. A git PATHSPEC (`-- ':!dir'`)
# built from a shell VARIABLE does not work: word-splitting a variable's
# value does not strip the quote characters it contains (quoting is a
# parse-time thing, not a runtime one), so `':!changelog.d'` reaches git
# as a literal five-character pathspec starting with a quote mark and
# matches nothing — found by this file's own selftest, case 2, where a
# changelog-only range ran the whole gate instead of pushing at once.
# Filtering the file LIST in the shell has no such trap.
is_board_only_files() {
  [ -z "$1" ] && return 0
  printf '%s\n' "$1" | grep -vE '^(\.work|sprint\.d|backlog\.d|changelog\.d|docs|specs)/' | grep -q . && return 1
  return 0
}
is_board_only_range() {
  # two dots: by the time this is called, $1 is a real ancestor of $2
  # (the merge above guarantees it), so this is exactly what $2 added
  is_board_only_files "$(git diff --name-only "$1..$2" 2>/dev/null)"
}

# a single commit's own diff, same test — used to tell a "landing" from
# a claim/release-claim/board commit when picking a bisect target
is_board_only_commit() {
  is_board_only_files "$(git diff --name-only "$1^..$1" 2>/dev/null)"
}

# ---- the lock: mkdir, holding the pid, taken over from a dead holder -
take_lock() {
  if mkdir "$LOCKDIR" 2>/dev/null; then
    echo $$ > "$LOCKDIR/pid"
    return 0
  fi
  holder=$(cat "$LOCKDIR/pid" 2>/dev/null || echo "")
  if [ -n "$holder" ] && ps -p "$holder" >/dev/null 2>&1; then
    echo "ci-runner: lock held by pid $holder — another run is in progress"
    return 1
  fi
  echo "ci-runner: lock dir exists but its pid ($holder) is dead — taking it over"
  rm -rf "$LOCKDIR"
  mkdir "$LOCKDIR" 2>/dev/null || { echo "ci-runner: lost the race for the lock"; return 1; }
  echo $$ > "$LOCKDIR/pid"
  return 0
}

release_lock() { rm -rf "$LOCKDIR"; }

# ---- stage C: bisect (a DETACHED worktree, never the main checkout) --
# and revert the first bad LANDING commit
bisect_and_revert() {
  from="$1"; to="$2"; log="$3"
  landings=$(git rev-list --reverse "$from..$to" | while read -r c; do
    is_board_only_commit "$c" || echo "$c"
  done)
  n=$(printf '%s\n' "$landings" | grep -c . || true)
  if [ "$n" -eq 0 ]; then
    echo "ci-runner: RED over $from..$to names no landing commit at all — leaving red, alerting the room" | tee -a "$log"
    return 1
  fi
  if [ "$n" -eq 1 ]; then
    culprit="$landings"
    echo "ci-runner: one landing commit in range — reverting it without a bisect: $culprit" | tee -a "$log"
  else
    bwt="$root/../okay-ci-bisect"
    [ -d "$bwt" ] && git worktree remove --force "$bwt" 2>/dev/null
    git worktree add --detach "$bwt" "$to" >>"$log" 2>&1 || { echo "ci-runner: could not create the bisect worktree" | tee -a "$log"; return 1; }
    (
      cd "$bwt"
      git bisect start "$to" "$from" >>"$log" 2>&1
      # THE SCOPED gate here, not the whole build: a bisect over N
      # disjoint landings costs N scoped gates, not N whole builds —
      # the same economy stage A buys, applied to the hunt. Verified
      # (2026-09-25): `gate.sh` exits nonzero on a genuine RED and 0 on
      # GREEN — plain enough for `bisect run` to read.
      git bisect run sh scripts/gate.sh "affected $from..HEAD" >>"$log" 2>&1
      git rev-parse HEAD
    ) > "$bwt.culprit" 2>>"$log"
    culprit=$(tail -1 "$bwt.culprit")
    rm -f "$bwt.culprit"
    (cd "$bwt" && git bisect reset "$to" >>"$log" 2>&1)
    git worktree remove --force "$bwt" 2>>"$log"
    if [ -z "$culprit" ]; then
      echo "ci-runner: bisect over $n landing(s) named no commit — leaving red, alerting the room" | tee -a "$log"
      return 1
    fi
    echo "ci-runner: bisect over $n landing(s) names $culprit" | tee -a "$log"
  fi
  culprit_subject=$(git log -1 --format=%s "$culprit")
  slug=$(printf '%s\n' "$culprit_subject" | sed -n 's/^\([a-zA-Z0-9-]*\):.*/\1/p')
  [ -z "$slug" ] && slug="unnamed"
  git revert --no-edit "$culprit" >>"$log" 2>&1 || {
    echo "ci-runner: revert of $culprit CONFLICTED — leaving it for a human; the tree is mid-revert" | tee -a "$log"
    return 1
  }
  cat > "changelog.d/ci-revert-$slug.md" <<EOF
## ci-revert-$slug - reverted by ci-runner

Culprit \`$culprit\` ("$culprit_subject", lane \`$slug\`) failed the
whole-build gate over \`$from..$to\`. Reverted so master stays
something the next lane can rebase onto; re-land with the fix. Runner
log: \`$log\`.
EOF
  git add "changelog.d/ci-revert-$slug.md"
  git commit --amend -m "$(cat <<AMENDMSG
Revert "$culprit_subject"

This reverts commit $culprit, which the whole-build gate over
$from..$to failed (lane: $slug). changelog.d/ci-revert-$slug.md
and the runner log ($log) have the failure.
AMENDMSG
)" >>"$log" 2>&1
  echo "ci-runner: reverted $culprit ($slug); changelog.d/ci-revert-$slug.md" | tee -a "$log"
  return 0
}

# ---- one turn ----------------------------------------------------------
run_once() {
  take_lock || return 1
  trap release_lock EXIT INT TERM

  git fetch origin >/dev/null 2>&1
  from=$(git rev-parse origin/master)
  to=$(git rev-parse master)
  if [ "$from" = "$to" ]; then
    echo "ci-runner: origin/master == master, nothing to do"
    release_lock; trap - EXIT INT TERM
    return 0
  fi
  # ORIGIN GENUINELY AHEAD (edits on GitHub, AGENTS.md's own exception):
  # no fast-forward either way. MERGE, never rebase — the boards cite
  # unpushed commits by sha (AGENTS.md, "Why they go stale at all").
  if ! git merge-base --is-ancestor "$from" "$to" 2>/dev/null; then
    echo "ci-runner: origin/master is not an ancestor of master — merging (never rebasing)"
    if ! git merge origin/master --no-edit >/dev/null 2>&1; then
      echo "ci-runner: merge of origin/master CONFLICTED — leaving it for a human"
      release_lock; trap - EXIT INT TERM
      return 1
    fi
    to=$(git rev-parse master)
  fi

  ts=$(date -u +%Y%m%dT%H%M%SZ)
  log="$LOGDIR/$ts-${from}..${to}.log"

  if is_board_only_range "$from" "$to"; then
    echo "ci-runner: $from..$to is board-only — pushing without a gate" | tee -a "$log"
    if git push origin master >>"$log" 2>&1; then
      echo "ci-runner: pushed $from..$to (board-only)"
      release_lock; trap - EXIT INT TERM
      return 0
    else
      echo "ci-runner: push REJECTED — origin moved during this turn; the next turn re-reads it"
      release_lock; trap - EXIT INT TERM
      return 1
    fi
  fi

  echo "ci-runner: gating $from..$to (whole build)" | tee -a "$log"
  sh scripts/gate-retry.sh "$root" "$log" 6 "family all"
  rc=$?
  touches_okay2=0
  git diff --name-only "$from..$to" -- okay2 2>/dev/null | grep -q . && touches_okay2=1
  if [ "$rc" -eq 0 ] && [ "$touches_okay2" -eq 1 ]; then
    echo "ci-runner: range touches okay2/ — gating its own build too" | tee -a "$log"
    ( cd okay2 && sh ../scripts/gate-retry.sh "$PWD" "$log.okay2" 6 test )
    rc=$?
    cat "$log.okay2" >> "$log" 2>/dev/null
    rm -f "$log.okay2"
  fi

  case "$rc" in
    0)
      echo "ci-runner: GREEN — pushing $from..$to" | tee -a "$log"
      if git push origin master >>"$log" 2>&1; then
        echo "ci-runner: pushed $from..$to"
        release_lock; trap - EXIT INT TERM
        return 0
      else
        echo "ci-runner: push REJECTED after a green gate — origin moved mid-turn; the next turn re-reads it" | tee -a "$log"
        release_lock; trap - EXIT INT TERM
        return 1
      fi
      ;;
    99)
      echo "ci-runner: no verdict after gate-retry's attempts — the box took it; not pushing, will retry on the next kick" | tee -a "$log"
      release_lock; trap - EXIT INT TERM
      return 1
      ;;
    *)
      echo "ci-runner: RED (exit $rc) — bisecting $from..$to" | tee -a "$log"
      bisect_and_revert "$from" "$to" "$log"
      release_lock; trap - EXIT INT TERM
      return 1
      ;;
  esac
}

# ---- entry points -------------------------------------------------------
cmd="${1:-once}"
case "$cmd" in
  once)
    run_once
    exit $?
    ;;
  loop)
    echo "ci-runner: looping (Ctrl-C to stop)"
    while :; do
      rm -f "$KICK"
      run_once
      while [ ! -f "$KICK" ]; do sleep 5; done
    done
    ;;
  kick)
    holder=$(cat "$LOCKDIR/pid" 2>/dev/null || echo "")
    if [ -n "$holder" ] && ps -p "$holder" >/dev/null 2>&1; then
      touch "$KICK"
      echo "ci-runner: kicked the running loop (pid $holder)"
    else
      touch "$KICK"
      echo "ci-runner: no loop running — starting a detached run"
      # a background job started INSIDE a tool call's shell dies with
      # that shell (AGENTS.md, "Exit 143 is SIGTERM", sender 1) — the
      # double subshell and </dev/null/nohup keep this one alive past it
      ( nohup sh "$0" once </dev/null >>"$LOGDIR/kick-detached.log" 2>&1 & )
    fi
    ;;
  status)
    holder=$(cat "$LOCKDIR/pid" 2>/dev/null || echo "")
    if [ -n "$holder" ] && ps -p "$holder" >/dev/null 2>&1; then
      echo "lock: held by pid $holder"
    else
      echo "lock: free"
    fi
    git fetch origin >/dev/null 2>&1
    n=$(git rev-list --count origin/master..master 2>/dev/null || echo "?")
    echo "unpushed: $n commit(s) (origin/master..master)"
    [ -f "$KICK" ] && echo "kick: pending" || echo "kick: none"
    ;;
  --read)
    log="${2:?usage: ci-runner.sh --read <log>}"
    if grep -q "^ci-runner: pushed" "$log"; then echo "$log: pushed — done"
    elif grep -q "^ci-runner: reverted" "$log"; then echo "$log: reverted the culprit — the next turn pushes the revert"
    elif grep -q "no verdict after gate-retry" "$log"; then echo "$log: no verdict — retry"
    elif grep -q "board-only — pushing" "$log"; then echo "$log: board-only push"
    else echo "$log: no recognised outcome — read it"
    fi
    ;;
  *)
    echo "usage: ci-runner.sh once|loop|kick|status|--read <log>" >&2
    exit 2
    ;;
esac
