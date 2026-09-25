#!/bin/sh
# ci-runner-selftest.sh — specs/ci-staged.md's Behavior list for stage
# B/C, against a FIXTURE repo (a throwaway git init + a bare "origin"),
# never the real okay repo. Two fakes stand in for the real build:
#
#   scripts/gate.sh <cmd>          verdict from $CI_TEST_VERDICT (or,
#                                   for the bisect case, from whether a
#                                   marker file is present in the tree)
#   scripts/gate-retry.sh <wt> <log> [n] [cmd]
#                                   runs the fake gate.sh ONCE, no
#                                   quiet-wait, no retry — this selftest
#                                   must not depend on the REAL host's
#                                   load average (the exact thing
#                                   AGENTS.md's "no flaky tests" rule
#                                   forbids depending on)
#
# Run under both /bin/sh and bash (sh-not-bash's rule: a script this
# repo runs as `sh` must be tested as `sh`).
#
#   sh scripts/ci-runner-selftest.sh
set -u
here="$(cd "$(dirname "$0")" && pwd)"
REAL_RUNNER="$here/ci-runner.sh"
fail=0
say() { printf '%s\n' "$1"; }
ok()  { say "  ok   — $1"; }
bad() { say "  FAIL — $1"; fail=1; }

new_fixture() {
  tmp=$(mktemp -d)
  bare="$tmp/origin.git"
  work="$tmp/work"
  git init -q --bare "$bare"
  git init -q -b master "$work" 2>/dev/null || { git init -q "$work" && (cd "$work" && git checkout -q -b master); }
  ( cd "$work"
    git config user.email t@t.test; git config user.name selftest
    mkdir -p scripts changelog.d .work/active
    cp "$here/ci-runner.sh" scripts/ci-runner.sh
    cat > scripts/gate.sh <<'EOF'
#!/bin/sh
# fake gate.sh: green unless CI_TEST_VERDICT=red, OR (for the bisect
# case) a BAD_MARKER file is present in the tree
if [ -f BAD_MARKER ] || [ "${CI_TEST_VERDICT:-green}" = red ]; then
  echo "gate: RED — tests failed:"; echo "==> X fake.Test.thing"; exit 1
fi
echo "gate: GREEN"; exit 0
EOF
    cat > scripts/gate-retry.sh <<'EOF'
#!/bin/sh
WT="${1:?}"; LOG="${2:?}"; CMD="${4:-test}"
( cd "$WT" && sh scripts/gate.sh "$CMD" ) >> "$LOG" 2>&1
exit $?
EOF
    chmod +x scripts/*.sh
    echo base > base.txt
    git add -A; git commit -q -m "base"
    git remote add origin "$bare"
    git push -q origin master
  )
}

run() { ( cd "$work" && CI_TEST_VERDICT="${CI_TEST_VERDICT:-green}" sh scripts/ci-runner.sh "$@" ); }
sha() { ( cd "$work" && git rev-parse "$1" ); }
origin_sha() { git -C "$bare" rev-parse master 2>/dev/null || echo none; }
commit_file() { # <name> <content>
  ( cd "$work" && echo "$2" > "$1" && git add "$1" && git commit -q -m "$1: fixture landing" )
}

say "1. nothing to do: origin/master == master"
new_fixture
out=$(run once); rc=$?
[ "$rc" -eq 0 ] && ok "exit 0" || bad "exit $rc"
[ "$(origin_sha)" = "$(sha master)" ] && ok "origin unchanged" || bad "origin moved"
rm -rf "$tmp"

say "2. a board-only range pushes without running the gate"
new_fixture
( cd "$work" && mkdir -p changelog.d && echo x > changelog.d/note.md && git add -A && git commit -q -m "changelog: note" )
before=$(origin_sha)
out=$(CI_TEST_VERDICT=red run once); rc=$?
[ "$rc" -eq 0 ] && ok "exit 0" || bad "exit $rc: $out"
[ "$(origin_sha)" = "$(sha master)" ] && ok "pushed despite a RED fake verdict (never gated)" || bad "did not push"
printf '%s\n' "$out" | grep -q "board-only" && ok "said board-only" || bad "did not say board-only: $out"
rm -rf "$tmp"

say "3. GREEN pushes exactly the gated commit"
new_fixture
commit_file src.txt one
target=$(sha master)
out=$(CI_TEST_VERDICT=green run once); rc=$?
[ "$rc" -eq 0 ] && ok "exit 0" || bad "exit $rc: $out"
[ "$(origin_sha)" = "$target" ] && ok "origin now at the gated commit" || bad "origin at $(origin_sha), wanted $target"
rm -rf "$tmp"

say "4. RED pushes nothing"
new_fixture
commit_file src.txt one
before=$(origin_sha)
out=$(CI_TEST_VERDICT=red run once); rc=$?
[ "$rc" -ne 0 ] && ok "nonzero exit" || bad "exit 0 on a red run"
[ "$(origin_sha)" = "$before" ] && ok "origin unchanged" || bad "origin moved on RED"
rm -rf "$tmp"

say "5. origin genuinely ahead: merged (not rebased), gated, pushed"
new_fixture
# a second clone pushes a commit ORIGIN never saw from this work tree
other="$tmp/other"
git clone -q "$bare" "$other" >/dev/null 2>&1
( cd "$other" && git config user.email o@o.test && git config user.name other \
  && echo remote > remote.txt && git add -A && git commit -q -m "remote: from elsewhere" && git push -q origin master )
commit_file src.txt local
out=$(CI_TEST_VERDICT=green run once); rc=$?
[ "$rc" -eq 0 ] && ok "exit 0" || bad "exit $rc: $out"
( cd "$work" && git log --oneline -1 --merges | grep -q . ) && ok "a merge commit exists" || bad "no merge commit found"
[ "$(origin_sha)" = "$(sha master)" ] && ok "pushed the merged tree" || bad "origin not updated to the merge"
rm -rf "$tmp"

say "6. a second run while the first holds the lock refuses, naming the pid"
new_fixture
mkdir -p "$work/.work/ci/lock"; echo $$ > "$work/.work/ci/lock/pid"
out=$(run once); rc=$?
[ "$rc" -ne 0 ] && ok "refused (nonzero exit)" || bad "ran anyway"
printf '%s\n' "$out" | grep -q "held by pid $$" && ok "named the holder's pid" || bad "did not name the pid: $out"
rm -rf "$tmp"

say "7. a lock whose pid is dead is taken over"
new_fixture
deadpid=99999
while kill -0 "$deadpid" 2>/dev/null; do deadpid=$((deadpid + 1)); done
mkdir -p "$work/.work/ci/lock"; echo "$deadpid" > "$work/.work/ci/lock/pid"
commit_file src.txt one
out=$(CI_TEST_VERDICT=green run once); rc=$?
[ "$rc" -eq 0 ] && ok "ran and exited 0" || bad "exit $rc: $out"
printf '%s\n' "$out" | grep -q "taking it over" && ok "said it took the dead lock over" || bad "did not say so: $out"
rm -rf "$tmp"

say "8. kick with a lock held just touches the kick file (does not start a second run)"
new_fixture
mkdir -p "$work/.work/ci/lock"; echo $$ > "$work/.work/ci/lock/pid"
out=$(run kick)
[ -f "$work/.work/ci/kick" ] && ok "kick file exists" || bad "no kick file"
printf '%s\n' "$out" | grep -q "kicked the running loop" && ok "said it kicked the loop" || bad "said: $out"
rm -rf "$tmp"

say "9. kick with no loop running starts a detached run that pushes"
new_fixture
commit_file src.txt one
target=$(sha master)
CI_TEST_VERDICT=green run kick >/dev/null
w=0
while [ "$(origin_sha)" != "$target" ] && [ "$w" -lt 20 ]; do sleep 0.5; w=$((w + 1)); done
[ "$(origin_sha)" = "$target" ] && ok "the detached run pushed within 10s" || bad "origin at $(origin_sha) after waiting, wanted $target"
rm -rf "$tmp"

say "10. RED with ONE landing commit: reverted without a bisect, nothing pushed on the red turn"
new_fixture
commit_file src.txt one
before_origin=$(origin_sha)
out=$(CI_TEST_VERDICT=red run once); rc=$?
[ "$rc" -ne 0 ] && ok "nonzero exit on the red turn" || bad "exit 0"
[ "$(origin_sha)" = "$before_origin" ] && ok "nothing pushed on the red turn" || bad "origin moved on red"
( cd "$work" && git log --oneline -3 ) | grep -qi "revert" && ok "a revert commit exists" || bad "no revert commit"
ls "$work"/changelog.d/ci-revert-*.md >/dev/null 2>&1 && ok "changelog.d/ci-revert-*.md exists" || bad "no revert changelog"
say "    (the next turn, green, pushes the revert)"
out2=$(CI_TEST_VERDICT=green run once); rc2=$?
[ "$rc2" -eq 0 ] && ok "next turn exits 0" || bad "next turn exit $rc2: $out2"
[ "$(origin_sha)" = "$(sha master)" ] && ok "the revert reached origin" || bad "revert not pushed"
rm -rf "$tmp"

say "11. RED with SEVERAL landing commits: bisect finds the one that introduced BAD_MARKER"
new_fixture
commit_file a.txt one
commit_file b.txt two
( cd "$work" && echo bad > BAD_MARKER && git add BAD_MARKER && git commit -q -m "culprit: introduces the marker" )
commit_file c.txt three
before_origin=$(origin_sha)
# CI_TEST_VERDICT is not read here — the fake gate checks BAD_MARKER's
# presence in the tree, which is what makes a REAL bisect meaningful
out=$(run once); rc=$?
[ "$rc" -ne 0 ] && ok "nonzero exit" || bad "exit 0"
[ "$(origin_sha)" = "$before_origin" ] && ok "nothing pushed on the red turn" || bad "origin moved"
printf '%s\n' "$out" | grep -q "bisect over 4 landing(s) names" && ok "bisect ran over all 4 landings (a, b, culprit, c)" || bad "did not bisect: $out"
( cd "$work" && git log --oneline -5 ) | grep -qi "revert \"culprit:" && ok "reverted the CULPRIT commit by name, not a or c" || bad "reverted the wrong commit"
( cd "$work" && [ -f BAD_MARKER ] ) && bad "BAD_MARKER still present after the revert" || ok "BAD_MARKER gone after the revert"
( cd "$work" && [ -f a.txt ] && [ -f c.txt ] ) && ok "the innocent landings (a.txt, c.txt) survive" || bad "an innocent landing was lost"
[ "$(cd "$work" && git symbolic-ref HEAD)" = "refs/heads/master" ] && ok "the main checkout's HEAD never left master" || bad "HEAD moved off master"
[ ! -d "$tmp/okay-ci-bisect" ] && ok "the bisect worktree was cleaned up" || bad "the bisect worktree is still there"
rm -rf "$tmp"

say ""
if [ "$fail" -eq 0 ]; then say "ci-runner-selftest: PASS"; else say "ci-runner-selftest: FAIL"; fi
exit "$fail"
