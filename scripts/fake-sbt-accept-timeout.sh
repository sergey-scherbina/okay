#!/bin/sh
# A "build" whose every run loses its Native test binary to the
# adapter's accept timeout (shape C, scripts/gate-fixtures/): the
# matrix AND the rerun of that module alone. gate-selftest checks that
# gate.sh then says KILLED — a busy box, not a verdict — instead of RED,
# which is what made ci-runner revert a green lane (2026-09-25).
cat "$(dirname "$0")/gate-fixtures/shape-c-accept-timeout.log"
exit 1
