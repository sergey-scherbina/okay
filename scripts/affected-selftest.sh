#!/bin/sh
# affected-selftest.sh — the shapes specs/ci-staged.md names for the
# `staged` order, and the `closed` control, checked in ONE sbt start.
#
# `affected --plan --files=…` prints what it would run and runs nothing,
# and takes the changed files from the argument instead of git, so the
# cases need no worktrees and no edits: one sbt, six commands, a grep of
# the log. Run it from a checkout of this repo (a worktree is fine); it
# takes as long as loading the build does.
#
#   sh scripts/affected-selftest.sh            # exit 0 = every shape held
#
# What "held" means, per case (specs/ci-staged.md, Behavior, stage A):
#   leaf main   okay-lex/src/main   → stage 1: test on the 3 okay-lex
#                                     projects; stage 2: test on >0 dependents
#   leaf test   okay-lex/src/test   → stage 1 the 3, NO second stage
#   build       build.sbt           → "the BUILD changed", one stage, all
#   core        src/main/scala      → stage 1 okayJS/JVM/Native only,
#                                     stage 2 every module
#   closed      okay-lex/src/main   → today's line, ONE stage
#   docs        docs/tutorial.md    → stage 1: okayDeploy alone, NO second stage
#   shared      okay-stream's TestChannelLaws (a JVM test source)
#                                   → stage 1 okayStreamJVM; stage 2 its
#                                     test->test dependents, okayClojure among them
#   bogus order                     → refused, named
set -e
cd "$(dirname "$0")/.."
log=$(mktemp -t okay-affected-selftest)
F=okay-lex/src/main/scala/okay/lex/X.scala
T=okay-lex/src/test/scala/okay/lex/X.scala
C=src/main/scala/X.scala
D=docs/tutorial.md
S=okay-stream/src/test/scala-jvm/TestChannelLaws.scala
# gate.sh so the run is the repo's one sbt path; its RED here is the
# deliberate bad order at the end, not a verdict on the tree
GATE_LOG="$log" sh scripts/gate.sh \
  "affected master test all staged --plan --files=$F; affected master test all staged --plan --files=$T; affected master test all staged --plan --files=build.sbt; affected master test all staged --plan --files=$C; affected master test all closed --plan --files=$F; affected master test all staged --plan --files=$D; affected master test all staged --plan --files=$S; affected master test all bogus --plan --files=build.sbt" \
  >/dev/null 2>&1 || true
plan=$(sed 's/\x1b\[[0-9;]*m//g' "$log" | grep -E '^\[(info|error)\] affected: ')
fail=0
check() { # <name> <grep -E pattern>
  if printf '%s\n' "$plan" | grep -qE "$2"; then echo "ok   $1"
  else echo "FAIL $1 — wanted /$2/"; fail=1; fi
}
check "leaf main: the changed three first, then dependents" \
  'affected: 1 file\(s\) changed since master: 3 project\(s\) directly, then [1-9][0-9]* dependents'
check "leaf main: stage 1 is okay-lex's three" \
  'plan — test on 3 changed project\(s\): okayLexJS okayLexJVM okayLexNative'
check "leaf main: stage 2 is the dependents" \
  'plan — test on [1-9][0-9]* dependent project\(s\): '
check "leaf test: no second stage" \
  'affected: 1 file\(s\) changed since master: 3 project\(s\) directly, then 0 dependents'
check "build: the whole gate, one stage" \
  'affected: 1 file\(s\) changed since master — the BUILD changed, so every project is'
check "core: the core's three first" \
  'plan — test on 3 changed project\(s\): okayJS okayJVM okayNative'
check "core: every module second" \
  'affected: 1 file\(s\) changed since master: 3 project\(s\) directly, then 1[0-9][0-9] dependents'
check "closed: today's line, unchanged" \
  'affected: 1 file\(s\) changed since master: 3 project\(s\) directly, [0-9]+ with dependents'
# affected-docs-run-no-doc-tests: a docs-only lane ran 0 tests; the doc
# suites are okay-deploy's, so a page maps there and to no dependent
check "docs: a page is okayDeploy's, with no dependents" \
  'affected: 1 file\(s\) changed since master: 1 project\(s\) directly, then 0 dependents; a doc, board or ledger file changed, so okayDeploy'
check "docs: stage 1 is okayDeploy alone" \
  'plan — test on 1 changed project\(s\): okayDeploy$'
# affected-test-only-shared-suites: a changed shared suite reaches the
# project whose tests extend it (okayClojure, test->test on okayStream.jvm)
check "shared suite: the changed project first" \
  'plan — test on 1 changed project\(s\): okayStreamJVM$'
check "shared suite: its test->test dependents second, okayClojure among them" \
  'plan — test on [1-9][0-9]* dependent project\(s\): .*okayClojure'
check "bogus order refused" \
  "affected: 'bogus' is not an order \(staged, closed\)"
# exactly three second stages print (leaf main, core, shared suite);
# build, closed and docs are one stage each, leaf test has none
n=$(printf '%s\n' "$plan" | grep -c 'plan — test on [0-9]* dependent project' || true)
if [ "$n" = 3 ]; then echo "ok   exactly three dependent stages (leaf main, core, shared suite)"
else echo "FAIL dependent stages: $n, wanted 3"; fail=1; fi
[ "$fail" = 0 ] && echo "affected-selftest: every shape held (log: $log)" || echo "affected-selftest: FAILED (log: $log)"
exit "$fail"
