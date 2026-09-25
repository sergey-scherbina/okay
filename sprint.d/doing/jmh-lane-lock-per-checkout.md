- [ ] jmh-lane-lock-per-checkout — `scripts/jmh-lane.sh` takes its lock
      at `.work/jmh/lock` INSIDE the checkout it runs from, so two
      worktrees (an A/B's `mine` and `ref`, or two agents' lanes) do not
      exclude each other at all; what caught them on 2026-09-25 was
      JMH's own `$TMPDIR/jmh.lock` — 4 of 18 lanes of cont-stack's A/B
      died with "Another JMH instance might be running", and one lane
      of the next series retried through it. The lock belongs to the
      BOX, not the checkout: `$TMPDIR/okay-jmh-lane.lock` (or the main
      checkout's `.work/jmh`, found the way `land.sh` finds it), with
      the pid file as now. `jmh-lane-selftest` covers the lock; its
      fixture sets the lock path, so the change is one variable.
