- [ ] ci-selftest-darwin-session — P2 / test harness: session isolation
      selftest compares zero sess values on this Darwin host.
      REPRO 2026-10-06: sh scripts/ci-runner-selftest.sh, case 9c alone
      reports runner shares kicker session 0; all other assertions pass.
      ps -p $$ -o pid,pgid,sess shows SESS 0 for the invoking shell too.
      ci-runner.sh/selftest are unchanged by build-platform-processes.
      HOW: use a supported POSIX getsid probe (optional helper/fallback)
      or explicitly report session observability as unavailable; do not
      treat two zero sentinels as evidence that sessions are shared.
      Preserve owned-PID/group signal tests, lock release and push checks.
