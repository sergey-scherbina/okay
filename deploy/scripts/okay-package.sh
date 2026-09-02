#!/usr/bin/env bash
# The scaffold's one utility (specs/deploy.md): build an Okay
# service's fat jar and, if a Docker daemon answers, its image too.
#
#   deploy/scripts/okay-package.sh okayDemo [tag]
#
# No Docker daemon: still builds and proves the jar with `java -jar`
# (the actual hard part — classpath, merge conflicts, one main), so
# this works exactly where the container step cannot (a laptop that
# just stopped docker to free memory, a CI runner without a daemon).
set -euo pipefail

MODULE="${1:?usage: $0 <sbt-module-id> [tag]}"
TAG="${2:-local}"
IMAGE="okay/${MODULE,,}:${TAG}"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"

echo "== building ${MODULE}/assembly"
sbt -batch "${MODULE}/assembly"

JAR="$(find . -path '*/target/scala-*/app.jar' | head -1)"
if [ -z "$JAR" ]; then
  echo "no app.jar produced — does ${MODULE} set assembly/assemblyJarName := \"app.jar\"?" >&2
  exit 1
fi
echo "== jar: $JAR"

if docker info >/dev/null 2>&1; then
  echo "== building image ${IMAGE}"
  docker build --build-arg "MODULE=${MODULE}" -t "${IMAGE}" -f deploy/Dockerfile .
  echo "== built ${IMAGE} — run with: docker run --rm -p 8080:8080 ${IMAGE}"
else
  echo "== no Docker daemon reachable — image build skipped; the jar above is the artifact"
  echo "== smoke-test it yourself: java -jar \"$JAR\""
fi
