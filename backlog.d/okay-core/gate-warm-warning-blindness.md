- [ ] gate-warm-warning-blindness — PARTLY ANSWERED 2026-09-17 by
      gate-stall-watchdog: a GREEN whose warning check was blind now
      SAYS SO in the log ("the WARNING CHECK WAS BLIND — this worktree
      was already built"), so silence no longer reads as cleanliness.
      What is still open is making it impossible rather than visible:
      `gate.sh` reporting how many of its module compiles actually
      compiled something, or the gate removing test-classes first.
      The measured incident is in that lane's CHANGELOG entry.
