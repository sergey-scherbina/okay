#!/usr/bin/env bash
# Build an optimized native binary of okay.demo.StateMcp with GraalVM
# native-image: a two-stage PGO build (instrument, run a
# representative workload, rebuild against the profile) plus
# -march=native, since the binary is meant to run on the machine that
# built it, spawned per Claude Code session where startup latency is
# the whole cost that matters.
#
# Usage:
#   GRAAL_HOME=/path/to/graalvm ./native-image-state-mcp.sh [output-path]
#
# Needs a JDK with native-image (GraalVM for JDK 21+; Oracle GraalVM
# for -O3/PGO, GraalVM CE works with -O2 — drop -march=native and the
# PGO stage if the toolchain does not support them).
set -euo pipefail

GRAAL_HOME="${GRAAL_HOME:?set GRAAL_HOME to a GraalVM install with native-image}"
OUT="${1:-$(pwd)/okay-state-mcp}"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

cd "$(dirname "$0")/../.."   # the repo root, wherever this script lives under okay-demo/scripts

echo "== exporting okayDemo's runtime classpath ==" >&2
CP="$(sbt -batch --error "export okayDemo/Runtime/fullClasspath" | tail -1)"

echo "== instrumented build (for PGO) ==" >&2
"$GRAAL_HOME/bin/native-image" -cp "$CP" okay.demo.StateMcp "$WORK/instrumented" \
  --no-fallback --pgo-instrument --gc=serial

echo "== running a representative workload through it ==" >&2
python3 - "$WORK" <<'PY'
import json, subprocess, sys, tempfile
work = sys.argv[1]
def workload():
    msgs = [
        {"jsonrpc":"2.0","id":1,"method":"initialize",
         "params":{"protocolVersion":"2024-11-05","capabilities":{},
                    "clientInfo":{"name":"pgo","version":"1"}}},
        {"jsonrpc":"2.0","method":"notifications/initialized","params":{}},
        {"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}]
    i = 3
    for k in range(200):
        msgs.append({"jsonrpc":"2.0","id":i,"method":"tools/call",
            "params":{"name":"update_state","arguments":{"step":k,"note":"x"*20}}}); i += 1
        msgs.append({"jsonrpc":"2.0","id":i,"method":"tools/call",
            "params":{"name":"get_state","arguments":{}}}); i += 1
    msgs.append({"jsonrpc":"2.0","id":i,"method":"tools/call",
        "params":{"name":"reset_state","arguments":{}}})
    return "\n".join(json.dumps(m) for m in msgs)
text = workload()
for _ in range(3):
    with tempfile.TemporaryDirectory() as d:
        subprocess.run([f"{work}/instrumented", f"{d}/state.json"], cwd=work,
            input=text, text=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
PY

echo "== final build: PGO profile + -march=native ==" >&2
"$GRAAL_HOME/bin/native-image" -cp "$CP" okay.demo.StateMcp "$OUT" \
  --no-fallback --pgo="$WORK/default.iprof" -march=native --gc=serial \
  -R:MaxHeapSize=64m -H:+ReportExceptionStackTraces

echo "== built: $OUT ==" >&2
"$OUT" --version < /dev/null > /dev/null 2>&1 || true
