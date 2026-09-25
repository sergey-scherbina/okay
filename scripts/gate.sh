#!/usr/bin/env bash
#
# The full matrix, with the one false failure it is known to produce
# told apart from a real one.
#
# TWICE on 2026-09-09 a matrix ended red with NO failed test: a Native
# module reported `Error: Total 110, Failed 0, Errors 1` and named the
# suite that happened to be running, having run fewer tests than that
# module has. Alone, the module passed. Both times the box was paging
# under sibling builds. It is a LOST TEST PROCESS, and the runner says
# nothing else about it: no exception, no output, no stack.
#
# What is ruled out, so nobody re-runs these (native-runner-error):
#   - the RAM guard: `killed=0` in its log at both minutes
#   - an OS kill: the kernel's memorystatus log for that window has
#     only idle-exit of system daemons, nothing of ours
#   - CPU pressure alone: 13 Native modules in parallel under 42
#     burners, four rounds, all green; and 5 modules under 28, green
#
# It takes two shapes, both handled below; the third occurrence was of
# the second, and this script missed it until it was taught.
#
# So this script does not pretend to fix it. It makes the gate SAY it:
# a failure that carries this signature and nothing else is re-run for
# the affected projects ALONE, and the outcome is printed either way.
#
# It never hides a real failure. A single `==> X` anywhere, or a
# failure whose module reported `Failed` above zero, is final: no
# rerun, and the exit code stands.
#
# Usage: scripts/gate.sh [sbt-command]          (default: test)
#        scripts/gate.sh "a; b; c"              several commands, run in
#          sequence in one sbt, stopping at the first that fails
#        scripts/gate.sh --read <log>           read a gate log that
#          already exists and say what it would have done — which is
#          how the three branches below are tested without waiting for
#          the failure to happen again
set -uo pipefail


# ---------------------------------------------------------------- THE WATCHDOG
#
# A GATE THAT HANGS USED TO HANG FOR EVER. Measured 2026-09-18: a run
# sat 57 minutes with its log frozen mid-sentence, and only a human
# asking "how is the gate?" found it. The diagnosis, from a jcmd dump
# kept at the time:
#
#   main                          parked in sbt.Execute.next
#   sbt.ForkTests$Acceptor$1$.run blocked in Net.accept — NO TIMEOUT
#   165 child processes           100 node + 65 Scala Native binaries,
#                                 every one at 0.0% CPU, all spawned in
#                                 the first 20 seconds, on 14 cores
#   java forks among them         NONE
#
# `ForkTests` opens a socket, starts a forked JVM and waits for it to
# connect back. The fork was not there, and `accept()` has no alarm —
# so sbt waited for a task that could never finish. (Scala Native's
# runner has a 40 s accept timeout, which is why THAT failure shows up
# as exit 137 instead of silence: same family, opposite symptom.)
#
# WHAT THIS DETECTS, and why it is two signals and not one. Silence
# alone is not a stall: a cold compile of one big module is silent for
# minutes. Idleness alone is not a stall either: sbt is briefly idle
# between tasks. A stall is SILENT **AND** IDLE — no new output for
# GATE_STALL_SECS while the whole process tree burned less than
# GATE_STALL_CPU seconds of CPU in that window. That is exactly what
# the dump above showed and exactly what a slow compile is not.
#
# WHAT IT DOES: takes the evidence FIRST (a jcmd thread dump and a `ps`
# of the tree, beside the log), then kills the run BY PID — never by
# name, which AGENTS.md forbids and which would reach a sibling's
# build — and prints `gate: STALLED`. That is deliberately not RED and
# not KILLED: `scripts/gate-retry.sh` treats a run with no verdict as
# retryable, and a stall says nothing about the tree.
#
# TESTING IT: `GATE_SBT` replaces the command, so the whole path is
# exercised in seconds rather than in an hour —
#   GATE_SBT=scripts/fake-sbt-stall.sh GATE_STALL_SECS=6 GATE_TICK_SECS=2 \
#     scripts/gate.sh test
# is a run that goes silent on purpose, and `scripts/gate-selftest.sh`
# is that plus the assertions.
GATE_SBT="${GATE_SBT:-sbt}"
# PIN THE AMBIENT JDK TO WHAT .sdkmanrc DECLARES (jdk-local-default,
# 2026-09-22), without touching sdkman's global `current` — the
# ambient JVM is what launches sbt and therefore what compiles
# everything (in-process dotc; specs/jdk-compatibility.md), and it is
# controlled by the SHELL's PATH, not by anything in build.sbt. A
# machine whose global default has drifted to the wrong major (17 was
# seen mid-session: JEP 444 virtual-thread APIs vanish, `Thread
# .startVirtualThread`/`.ofVirtual` fail with "is not a member of
# object Thread") should still build correctly from this repo, so
# every sbt invocation resolves its OWN JDK from the project's own
# pin rather than trusting whatever the box's default happens to be
# right now. Silently does nothing if sdkman or the pinned candidate
# is absent — this is a convenience, not a hard requirement.
if [ "${GATE_JDK_AUTOPIN:-1}" != "0" ]; then
  sdk_java="$(sed -n 's/^java=//p' .sdkmanrc 2>/dev/null | head -1)"
  sdk_java_home="$HOME/.sdkman/candidates/java/${sdk_java}"
  if [ -n "$sdk_java" ] && [ -d "$sdk_java_home" ]; then
    export JAVA_HOME="$sdk_java_home"
    export PATH="$JAVA_HOME/bin:$PATH"
  fi
fi
# TWO LAYERS, AND THE INNER ONE MUST FIRE FIRST. `gate-retry.sh` has
# had a stall watchdog since before this one: it polls the log's SIZE
# once a minute and kills the tree after GATE_STALL_MIN (10) minutes
# of no growth. That layer stays — it is the backstop for the case
# where this script itself is wedged — but it takes no evidence, and
# whichever fires first is the one that gets to look. So this default
# sits BELOW it: 8 minutes here, 10 there.
#
# The CPU test is what makes 8 minutes safe. Silence alone at 8
# minutes would kill honest cold compiles; silence with essentially no
# CPU in the whole process tree is a build that has stopped being a
# build.
stall_secs="${GATE_STALL_SECS:-480}"     # 8 minutes of silence...
stall_cpu="${GATE_STALL_CPU:-5}"         # ...with under 5s of CPU in the WORKERS
# ...and under 60s in the HOST (gate-watchdog-idle-sbt-cpu, 2026-09-23).
# The host is sbt's own JVM; the workers are everything under it —
# node, Native test binaries, forked test JVMs. One sum over the whole
# tree missed the hang this watchdog was written for, TWICE in a day:
# sbt at idle burns ~3 s/min (GC, a process-reaper thread per child),
# 26 s per window, five times the 5 s bar, so a tree of 0%-CPU children
# read "still working" for ever. The host's bar is its own: 60 s per 8
# minutes is ~12% of one core, twice its idle overhead and far under a
# cold compile, which is the HOST working (dotc is in-process) with no
# worker at all — the case that must survive.
stall_host_cpu="${GATE_STALL_HOST_CPU:-60}"
host_pattern="${GATE_HOST_PATTERN:-sbt-launch|sbt[.]script|xsbt[.]boot}"
tick_secs="${GATE_TICK_SECS:-30}"

# the log's mtime, on BSD (this box) and on GNU. Without the second
# arm a Linux run would read 0 for ever, which reads as "never
# changed" — a watchdog that kills every gate is worse than none.
mtime_of() { stat -f %m "$1" 2>/dev/null || stat -c %Y "$1" 2>/dev/null || echo 0; }

# every descendant of a pid, deepest last; pgrep -P is one level, so
# this walks it. Used for both the CPU sum and the kill.
descendants() {
  for c in $(pgrep -P "$1" 2>/dev/null); do
    echo "$c"
    descendants "$c"
  done
}

# the tree split in two: the HOST (the root pid, and whatever in the
# tree is sbt's own JVM by its command line) and the WORKERS (the rest).
# `host_pids`/`worker_pids` print pids; `cpu_of` sums their CPU seconds.
host_pids() {
  echo "$1"
  descendants "$1" | while read -r p; do
    if ps -o command= -p "$p" 2>/dev/null | grep -Eq "$host_pattern"; then echo "$p"; fi
  done
}
worker_pids() {
  hosts=$(host_pids "$1")
  descendants "$1" | while read -r p; do
    if ! printf '%s\n' "$hosts" | grep -qx "$p"; then echo "$p"; fi
  done
}
# CPU SECONDS of a set of pids. `ps -o pcpu` is the average over a
# process's whole life and says nothing about now (it read 5.8% for a
# JVM that had been idle for an hour), so this reads cumulative CPU
# TIME and the caller differences two samples.
cpu_of() {
  sort -u | while read -r p; do
    ps -o time= -p "$p" 2>/dev/null
  done | awk -F: '
    { n=NF; s=0; m=1
      for (i=n; i>=1; i--) { s += $i * m; m *= 60 }
      total += s }
    END { printf "%d\n", total+0 }'
}

stall_evidence() {
  # the dump is the whole point: without it a stall is a shrug
  local root="$1" out="$2" jv jc
  ps -o pid,ppid,pcpu,etime,stat,command -p "$root" > "$out.ps" 2>/dev/null
  descendants "$root" | while read -r p; do
    ps -o pid,ppid,pcpu,etime,stat,command -p "$p" 2>/dev/null | tail -1
  done >> "$out.ps"
  # NO `case` HERE. Inside a command substitution that spans lines,
  # `;; esac` on one line is a syntax error under /bin/sh — and this
  # script is invoked as `sh scripts/gate.sh` everywhere in AGENTS.md,
  # so a bash-only body would have broken the watchdog in exactly the
  # invocation that matters. Caught by running the selftest both ways.
  jv=$({ echo "$root"; descendants "$root"; } | while read -r p; do
         if [ "$(ps -o comm= -p "$p" 2>/dev/null | sed 's#.*/##')" = "java" ]; then
           echo "$p"
         fi
       done | head -1)
  if [ -n "$jv" ]; then
    jc="$(/usr/libexec/java_home 2>/dev/null)/bin/jcmd"
    [ -x "$jc" ] || jc=jcmd
    "$jc" "$jv" Thread.dump_to_file -format=json -overwrite "$out.json" >/dev/null 2>&1 \
      && echo "gate: thread dump of the stalled JVM ($jv): $out.json"
  fi
  echo "gate: the process tree it was waiting on: $out.ps"
}

# run sbt with the watchdog watching its log. Sets nothing global but
# the exit status it returns.
# sbt_run <log> <sbt-command>...  — several commands run IN SEQUENCE
# inside ONE sbt, and sbt stops at the first that fails, which is what
# makes the JVM-first split below cost nothing when it is green
sbt_run() {
  local l="$1" pid quiet base now mt last hbase hnow
  shift
  : > "$l"
  # shellcheck disable=SC2086
  $GATE_SBT "$@" > "$l" 2>&1 &
  pid=$!
  last=$(mtime_of "$l")
  quiet=0
  base=$(worker_pids "$pid" | cpu_of); hbase=$(host_pids "$pid" | cpu_of)
  while kill -0 "$pid" 2>/dev/null; do
    sleep "$tick_secs"
    kill -0 "$pid" 2>/dev/null || break
    mt=$(mtime_of "$l")
    if [ "$mt" != "$last" ]; then
      last="$mt"; quiet=0
      base=$(worker_pids "$pid" | cpu_of); hbase=$(host_pids "$pid" | cpu_of); continue
    fi
    quiet=$((quiet + tick_secs))
    [ "$quiet" -lt "$stall_secs" ] && continue
    now=$(worker_pids "$pid" | cpu_of); hnow=$(host_pids "$pid" | cpu_of)
    if [ $((now - base)) -gt "$stall_cpu" ] || [ $((hnow - hbase)) -gt "$stall_host_cpu" ]; then
      # silent but working: a long compile (the host) or a long test
      # (a worker). Say it once per window and keep waiting, with both
      # baselines moved forward.
      echo "gate: quiet for ${quiet}s but the workers burned $((now - base))s and sbt $((hnow - hbase))s of CPU — still working"
      quiet=0; base="$now"; hbase="$hnow"; continue
    fi
    echo "gate: STALLED — no output for ${quiet}s; the workers burned $((now - base))s and sbt $((hnow - hbase))s of CPU in that window"
    stall_evidence "$pid" "$l.stall"
    echo "gate: killing the run BY PID ($pid and its tree); this is NOT a verdict about the tree"
    descendants "$pid" | while read -r p; do kill "$p" 2>/dev/null; done
    kill "$pid" 2>/dev/null
    sleep 5
    descendants "$pid" | while read -r p; do kill -9 "$p" 2>/dev/null; done
    kill -9 "$pid" 2>/dev/null
    wait "$pid" 2>/dev/null
    return 124
  done
  wait "$pid"
}

replay=""
if [ "${1:-}" = "--read" ]; then replay="${2:?--read needs a log}"; fi

if [ -n "$replay" ]; then
  log="$replay"
  # A log does not carry the exit status of the process that wrote it,
  # and one of the branches below is keyed on exactly that (a signal).
  # `GATE_STATUS=143 scripts/gate.sh --read <log>` is how that branch
  # is exercised without waiting to be killed again.
  status="${GATE_STATUS:-$(grep -cE "^\[error\]" "$log" > /dev/null && echo 1 || echo 0)}"
  echo "gate: reading $log (no sbt run)"
else
  cmd="${1:-test}"
  log="${GATE_LOG:-$(mktemp -t okay-gate)}"

  # JVM FIRST, AND THE OTHER PLATFORMS ONLY IF IT IS GREEN
  # (gate-jvm-first, 2026-09-18). MEASURED: sampling one full gate's
  # own descendants every 4 s, `node` is 684 of 944 samples — 72% of
  # a matrix is Scala.js runners, and the Native binaries are most of
  # the rest. The JVM arm is where a logic error shows; JS and Native
  # mostly re-check that the same suites compile and run there.
  #
  # sbt runs the commands it is given IN SEQUENCE and stops at the
  # first that fails, so a red JVM never pays for the other two. One
  # sbt, one JVM start, one log — the phases are two commands inside
  # it, not two invocations.
  #
  # ONLY for the `affected <ref>` form, which is what AGENTS.md tells
  # every lane to run. Anything else (`test`, an explicit task, a
  # `family` call) is passed through untouched: this is a faster road
  # to the same verdict, not a new meaning for the argument.
  # `affected <ref> staged` is the PRE-MERGE gate (ci-staged, 2026-09-25,
  # specs/ci-staged.md): the lane's changed projects' tests first, their
  # dependents' second, as two sbt commands. Same two phases, same
  # JVM-first order — the order is the fourth sbt argument, so it rides
  # on the end of both: changed-JVM, dependents-JVM, changed-rest,
  # dependents-rest.
  phase2=""
  case "$cmd" in
    "affected "*)
      ref="${cmd#affected }"
      scope=""
      case "$ref" in
        *" staged") case "${ref% staged}" in
                      *" "*) : ;;        # `<ref> <task> <platform> staged`: the caller means it
                      *) scope=staged; ref="${ref% staged}" ;;
                    esac ;;
      esac
      if [ -n "$scope" ]; then
        cmd="affected $ref test jvm staged"; phase2="affected $ref test rest staged"
      else
        case "$ref" in
          *" "*) : ;;                      # a task or platform was given: the caller means it
          *) cmd="affected $ref test jvm"; phase2="affected $ref test rest" ;;
        esac
      fi ;;
  esac
  # A ";"-CHAIN IS SEVERAL COMMANDS (gate-command-chain, 2026-09-23).
  # Handed to sbt as ONE argument, `"a; b"` ran `a` and dropped `b`
  # without a word — twice in one day, and "0 test results" in the
  # verdict line was the only tell. sbt's own spelling for a sequence
  # is one argument per command, so the chain is split into exactly
  # that: in order, trimmed, empty parts dropped, and sbt still stops
  # at the first that fails. The JVM-first split above applies only to
  # a single `affected <ref>`; a chain is passed as the caller wrote it.
  chained=""
  case "$cmd" in
    *";"*)
      chained=1
      set --
      rest="$cmd"
      while :; do
        part=$(printf '%s' "${rest%%;*}" | sed 's/^[[:space:]]*//; s/[[:space:]]*$//')
        [ -n "$part" ] && set -- "$@" "$part"
        case "$rest" in *";"*) rest="${rest#*;}" ;; *) break ;; esac
      done
      # zero commands: real sbt with no argument opens its INTERACTIVE
      # shell and the gate would wait on it for ever
      if [ "$#" -eq 0 ]; then
        echo "gate: \"$cmd\" names no command — refusing to start an interactive sbt" >&2
        exit 2
      fi
      phase2="" ;;
  esac
  if [ -n "$chained" ]; then
    echo "gate: sbt$(for c in "$@"; do printf ' "%s"' "$c"; done)  (log: $log)"
    sbt_run "$log" "$@"
  elif [ -n "$phase2" ]; then
    echo "gate: sbt \"$cmd\" \"$phase2\"  (log: $log)"
    sbt_run "$log" "$cmd" "$phase2"
  else
    echo "gate: sbt $cmd  (log: $log)"
    sbt_run "$log" "$cmd"
  fi
  status=$?
fi
# ONE stripped copy, then plain greps over the FILE. Not a pipeline:
# `set -o pipefail` plus `grep -q` reports failure even on a match,
# because grep exits early and the writer takes a SIGPIPE — which is
# how the first cut of this script called every real failure
# "unrecognised" (native-runner-error, 2026-09-09).
clean="${log}.clean"
perl -pe 's/\e\[[0-9;]*m//g' "$log" > "$clean"
strip() { cat "$clean"; }

tests=$(awk '/Passed: Total|Error: Total/ {for (i=1;i<=NF;i++) if ($i=="Total") {gsub(",","",$(i+1)); s+=$(i+1)}} END {print s+0}' "$clean")
echo "gate: sbt exited $status, $tests test results"

# ORDER MATTERS HERE, and each step earns its place.
#
#   1  a failed test is final, whatever else happened afterwards
#   2  a SIGNAL is not a verdict, and its log is TRUNCATED — so it is
#      answered before anything reads that log for absences
#   3  warnings, which only a log of a finished run can be trusted on
#   4  green

# 1. A REAL TEST FAILURE ENDS IT, whatever the exit status was: a
# suite that failed and was then killed is red, not killed.
if grep -q "==> X" "$clean"; then
  echo "gate: RED — tests failed:"
  grep "==> X" "$clean" | head -20
  exit "${status:-1}"
fi

# 2. A SIGNAL IS NOT A VERDICT.
#
# 143 is SIGTERM and 137 is SIGKILL, and on this box they come from
# launchd's RAM guard and idle reaper, not from anything about the
# tree (AGENTS.md, "THE 143, SOLVED"; scripts/gate-sentinels.sh tells
# an external signal from a process-group kill by blast radius).
#
# This mattered more than a wrong word. `scripts/gate-retry.sh` retries
# a run that produced NO VERDICT and passes a `gate: RED` straight
# through, deliberately — a loop that re-rolls a red is a machine for
# landing broken trees. Calling a kill RED therefore disabled the
# retry in exactly the case it was written for: measured 2026-09-11, a
# matrix died at 147 module compiles with zero `==> X`, zero `[error]`
# lines and the log simply stopping mid-suite, and the loop reported it
# as a failure of the tree.
#
# Nothing else may read this log for an ABSENCE, which is why this
# comes before the warning check: half a matrix that warned about
# nothing has not told you the tree is clean.
if [ "$status" -eq 143 ] || [ "$status" -eq 137 ]; then
  echo "gate: KILLED — sbt took signal $((status - 128)) and no test failed"
  echo "gate: this is NOT a verdict about the tree; run it again on a quiet box"
  exit "$status"
fi

# 2b. A STALL IS NOT A VERDICT EITHER, and for the same reason: the
# watchdog above killed the run, so this log stops mid-sentence and
# nothing may read it for an absence. 124 is the watchdog's own code
# (`timeout`'s convention), and it is deliberately NOT spelled RED or
# KILLED — `gate-retry.sh` keys on those two strings, and a stall is a
# run that said nothing about the tree, which is exactly what its
# retry loop exists for.
if [ "$status" -eq 124 ]; then
  echo "gate: the stall's evidence is beside the log ($log.stall.json, $log.stall.ps)"
  echo "gate: read the dump before re-running; a second stall in the same place is a finding"
  exit "$status"
fi

# 3. WARNINGS, which this script did not look at until 2026-09-11 and
# which AGENTS.md has required all along ("no warnings, ever"). Three
# unused imports in okay-openapi and one in the core's own tests had
# ridden through every green gate.
#
# Two facts decide the shape. A warning is a COMPILE diagnostic, so a
# warm run emits none and its silence is not evidence — the script
# says which case it is in rather than letting a warm pass look like a
# clean one. And a lane's gate runs in a fresh worktree, where nothing
# is compiled yet, so the run that decides a landing is exactly the run
# that sees them.
#
# The signature is dotty's own diagnostic header, `[warn] -- [Exxx]`,
# and not any line sbt happens to call a warning: a resolution note or
# "multiple main classes" is not what the rule is about.
#
# ONE FALSE POSITIVE IS KNOWN, and deleting the import it names breaks
# the build. E198 "unused import" fired on `import okay.RowLift.{at as
# liftAt, plus}` in the core's own tests, where `liftAt` IS used —
# removing it failed with E008 "value liftAt is not a member of". A
# RENAMED import reached only in extension-selection position is not
# counted as used. The fix is to drop the RENAME, not the import
# (`{at, plus}` and `.at[...]`, which compiles clean), and it is
# written here so the next person does not delete a line the compiler
# pointed at and then wonder why nothing builds.
warns=$(grep -cE "^\[warn\] -- " "$clean")
compiled=$(grep -cE "^\[info\] compiling " "$clean")
if [ "$compiled" -eq 0 ]; then
  echo "gate: warnings NOT checked — nothing was compiled (a warm run says nothing about them)"
elif [ "$warns" -gt 0 ]; then
  echo "gate: RED — $warns compile warning(s) over $compiled module compile(s); 'no warnings, ever' (AGENTS.md):"
  grep -E "^\[warn\] -- " "$clean" | sed 's/^/  /' | head -20
  exit 1
else
  echo "gate: no compile warnings ($compiled module compile(s) looked at)"
fi

# 4. GREEN
[ "$status" -eq 0 ] && { echo "gate: GREEN"; exit 0; }

# THE TWO SHAPES A LOST TEST PROCESS TAKES (native-runner-error).
#
#   A  the process ran some tests and then went: the module reports
#      `Error: Total N, Failed 0, Errors 1` and names the suite that
#      was in flight
#   B  it went before it said anything at all, so there is no report
#      to speak of — only `(<m> / Test / executeTests)` carrying
#      scala-native's `RunTerminatedException` (its RPC channel closed
#      under it). Filed by the `failing-over` gate, 2026-09-09 18:55,
#      after this script called it "unrecognised" and re-ran nothing.
#
#   C  it never CONNECTED (native-accept-timeout, 2026-09-25): the
#      binary did not dial back within the adapter's hard-coded 40 s
#      (`ComRunner`, `ServerSocket.setSoTimeout(40000)` in test-runner
#      0.5.12), the adapter logged `Force close … Accept timed out` and
#      killed it (`destroyForcibly`: exit 137, "fatal signal 9"), and
#      sbt reports `(<m> / Test / loadedTestFrameworks)` carrying the
#      same `RPCCore$ClosedException` — before any test of the module
#      ran. Accepted ONLY with the `Accept timed out` line in the log:
#      a loadedTestFrameworks failure without it is not this mechanism.
#      Measured occurrences: okayOpticsNative 2026-09-22, okayAsyncNative
#      2026-09-23 and 2026-09-25 (the ci-runner run that reverted a green
#      lane for it), okayChainNative and okayConfNative 2026-09-25.
#
# Conservative on purpose: a project that failed in NONE of the shapes
# means we do not understand this red, so nothing is re-run.
lost_a=$(grep -E "^\[error\] \(.*Test / test\) sbt.TestsFailedException" "$clean" \
      | sed -E 's/^\[error\] \(([^ ]+) \/ Test \/ test\).*/\1/' | sort -u)
lost_b=$(grep -E "^\[error\] \(.*Test / executeTests\).*(RunTerminatedException|RPCCore\$ClosedException)" "$clean" \
      | sed -E 's/^\[error\] \(([^ ]+) \/ Test \/ executeTests\).*/\1/' | sort -u)
# shape A must carry its own report line, or it is not shape A
if [ -n "$lost_a" ] && ! grep -qE "^\[error\] Error: Total [0-9]+, Failed 0, Errors [1-9]" "$clean"; then
  lost_a=""
fi
lost_c=""
if grep -qE "Accept timed out" "$clean"; then
  lost_c=$(grep -E "^\[error\] \(.*Test / loadedTestFrameworks\).*(RunTerminatedException|RPCCore\$ClosedException)" "$clean" \
        | sed -E 's/^\[error\] \(([^ ]+) \/ Test \/ loadedTestFrameworks\).*/\1/' | sort -u)
fi
lost=$(printf '%s\n%s\n%s\n' "$lost_a" "$lost_b" "$lost_c" | grep -v '^$' | sort -u)
failed_projects=$(grep -E "^\[error\] \([^)]*Test / (test|executeTests|loadedTestFrameworks)\)" "$clean" \
      | sed -E 's/^\[error\] \(([^ ]+) \/ Test \/ (test|executeTests|loadedTestFrameworks)\).*/\1/' | sort -u)
# comm needs two FILES, and `<(...)` process substitution is a
# bashism — this script is invoked as both `scripts/gate.sh` (its own
# bash shebang) and `sh scripts/gate.sh` (plain POSIX sh) around this
# repo, and the second broke here (single-path-verification's own
# bench.sh smoke test, 2026-09-22): "syntax error near unexpected
# token `('" instead of a verdict, on the very first RED this line
# ever had to handle. Temp files work under both.
failed_f="${log}.failed_projects"
lost_f="${log}.lost_projects"
printf '%s\n' "$failed_projects" | grep -v '^$' > "$failed_f"
printf '%s\n' "$lost" | grep -v '^$' > "$lost_f"
unknown=$(comm -23 "$failed_f" "$lost_f")
rm -f "$failed_f" "$lost_f"
if [ -z "$lost" ] || [ -n "$unknown" ]; then
  echo "gate: RED — a failure this script does not recognise; read $log"
  [ -n "$unknown" ] && { echo "gate: these failed in no known shape:"; echo "$unknown" | sed 's/^/  /'; }
  grep -E "^\[error\]" "$clean" | head -20
  exit $status
fi

echo "gate: no test failed, and these modules lost a test process:"
echo "$lost" | sed 's/^/  /'
if [ -n "$replay" ]; then
  echo "gate: (--read) would re-run those alone and report the outcome"
  exit 0
fi
echo "gate: re-running exactly those, alone, to say which it was"
rerun=""
for p in $lost; do rerun="$rerun $p/test"; done
rlog="${log}.rerun"
# shellcheck disable=SC2086
$GATE_SBT $rerun > "$rlog" 2>&1
rstatus=$?
if [ $rstatus -eq 0 ]; then
  echo "gate: GREEN AFTER RERUN — the matrix's only failure was a lost process in:"
  echo "$lost" | sed 's/^/  /'
  echo "gate: (rerun log: $rlog) — record the recurrence in BACKLOG's native-runner-error entry"
  exit 0
fi
perl -pe 's/\e\[[0-9;]*m//g' "$rlog" > "${rlog}.clean"
# A rerun that lost its process to the SAME accept timeout has still
# said nothing about the tree: the box that starved the matrix's binary
# for 40 s can starve one module's too (load ~100 on 14 cores, measured
# 2026-09-25). Reported as KILLED, which gate-retry retries and
# ci-runner never bisects — a red here is what reverted a green lane.
if ! grep -q "==> X" "${rlog}.clean" \
   && grep -q "Accept timed out" "${rlog}.clean" \
   && ! grep -qE "Failed [1-9]" "${rlog}.clean"; then
  echo "gate: KILLED — the rerun lost its test process the same way (Accept timed out) and no test failed"
  echo "gate: this is NOT a verdict about the tree; run it again on a quieter box"
  exit 137
fi
echo "gate: RED — the rerun failed too, so this is not the known signature:"
grep -E "==> X|^\[error\]" "${rlog}.clean" | head -20
exit $rstatus
