- [ ] jmh-lane-term-exits — `scripts/jmh-lane.sh` traps INT/TERM with a
      handler that does not exit (`trap bw_unwant EXIT INT TERM`, and
      the same after the lock), so under `sh` a SIGTERM runs the
      handler and the script CONTINUES its queue loop: a queued lane
      needed `kill -KILL` (ring-head-tail-padding, 2026-09-27). Fix:
      EXIT keeps the cleanup, INT/TERM exit (130/143) so EXIT runs it.
      Proof: a lane queued behind a held lock dies on one SIGTERM and
      leaves no want token, red first. (2026-09-27, ring-head-tail-padding)
