#!/bin/sh
# ci-lock.sh — the ONE lock over a whole build in a checkout, sourced by
# scripts/ci-runner.sh and scripts/gate.sh (ci-runner-lock-bypass,
# 2026-09-28; policy P-6: one vocabulary on both sides, or it is two
# guards, not one).
#
# WHY IT LEFT ci-runner.sh: `.work/ci/lock` stopped a second
# `ci-runner.sh` and nothing else. A hand-run "the whole build, right
# now" (`gate-retry.sh … "family all"`) in the same checkout raced a
# legitimate `ci-runner.sh once` mid-run: two sbt processes writing one
# `target/` tree, which read as a real RED (`scala2probe`:
# `NoClassDefFoundError` on core classes) and was not (2026-09-25). The
# missing piece was enforcement OUTSIDE the runner — so the lock is
# taken by whoever starts a whole build, and the runner's own gate,
# a descendant of the runner holding it, recognises the lock as its own.
#
# The lock is a DIRECTORY (mkdir is O_EXCL everywhere here) holding the
# owner's pid. A holder that is dead is taken over; a holder that is a
# live ANCESTOR of the caller is the caller's own run (the runner above
# its gate-retry above its gate.sh); any other live holder wins.
#
#   . scripts/ci-lock.sh
#   CI_LOCK_WHO=gate ci_lock_take <dir>    0 taken (release it when done)
#                                          2 already ours, held above us
#                                          1 held by another live run
#   ci_lock_release <dir>
#   ci_lock_holder <dir>                   the pid in it, or nothing
#   ci_lock_is_ours <pid>                  0 when <pid> is us or an ancestor

ci_lock_holder() { cat "$1/pid" 2>/dev/null; }

ci_lock_alive() { [ -n "$1" ] && ps -p "$1" > /dev/null 2>&1; }

# walks the parent chain from this process up to init; bounded by the
# depth of the process tree, and a pid that reads back as 0 or empty
# ends it (an orphan's parent is 1, whose parent reads as 0)
ci_lock_is_ours() {
  _p=$$
  while [ -n "$_p" ] && [ "$_p" -gt 1 ] 2>/dev/null; do
    [ "$_p" = "$1" ] && return 0
    _p=$(ps -o ppid= -p "$_p" 2>/dev/null | tr -d ' ')
  done
  return 1
}

ci_lock_take() {
  _d="$1"; _who="${CI_LOCK_WHO:-ci-lock}"
  mkdir -p "$(dirname "$_d")" 2>/dev/null
  if mkdir "$_d" 2>/dev/null; then
    echo $$ > "$_d/pid"
    return 0
  fi
  _holder=$(ci_lock_holder "$_d")
  if ci_lock_alive "$_holder"; then
    if ci_lock_is_ours "$_holder"; then return 2; fi
    echo "$_who: lock held by pid $_holder — another run is in progress"
    return 1
  fi
  echo "$_who: lock dir exists but its pid ($_holder) is dead — taking it over"
  rm -rf "$_d"
  mkdir "$_d" 2>/dev/null || { echo "$_who: lost the race for the lock"; return 1; }
  echo $$ > "$_d/pid"
  return 0
}

ci_lock_release() { rm -rf "$1"; }
