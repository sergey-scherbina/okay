#!/usr/bin/env bash
# okay-deploy's one utility (specs/deploy.md): build a module's fat jar
# and, if a Docker daemon answers, its image from the module's OWN
# rendered deploy/Dockerfile. Nothing here knows any application.
#
#   okay-deploy/bin/okay-package.sh <sbt-module-id> <module-dir> [image:tag]
#   okay-deploy/bin/okay-package.sh okayDemo okay-demo okay/demo:v1
set -euo pipefail
MODULE="${1:?usage: $0 <sbt-module-id> <module-dir> [image:tag]}"
DIR="${2:?usage: $0 <sbt-module-id> <module-dir> [image:tag]}"
IMAGE="${3:-okay/${DIR}:local}"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"
echo "== ${MODULE}/assembly"
sbt -batch "${MODULE}/assembly"
JAR="$(ls "${DIR}"/target/scala-*/app.jar 2>/dev/null | head -1)"
[ -n "$JAR" ] || { echo "no ${DIR}/target/scala-*/app.jar — does ${MODULE} carry OkayDeploy.deployable(...)?" >&2; exit 1; }
echo "== jar: $JAR"
if docker info >/dev/null 2>&1; then
  echo "== image ${IMAGE} from ${DIR}/deploy/Dockerfile"
  docker build -t "${IMAGE}" -f "${DIR}/deploy/Dockerfile" .
else
  echo "== no Docker daemon — image step skipped; the jar is the artifact: java -jar \"$JAR\""
fi
