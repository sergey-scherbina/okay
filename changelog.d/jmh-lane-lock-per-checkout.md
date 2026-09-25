## jmh-lane-lock-per-checkout — one JMH lane at a time means one per BOX

`scripts/jmh-lane.sh` took its lock at `.work/jmh/lock` inside the
checkout it ran from, so lanes from two worktrees — an A/B's `mine` and
`ref` arms, or two agents' — never saw each other; what stopped them
on 2026-09-25 was JMH's own `$TMPDIR/jmh.lock`, and 4 of 18 lanes of
cont-stack's A/B died with "Another JMH instance might be running".

- The lock lives at `${TMPDIR:-/tmp}/okay-jmh-lane.lock`, beside JMH's
  own; `JMH_LANE_LOCK` overrides it. The pid file, the dead-holder
  takeover and the release on exit are as before.
- `jmh-lane-selftest.sh`'s fixture sets `JMH_LANE_LOCK` to its own
  directory, so the six cases run against a fixture lock, never the
  box's; PASS under `sh` and `bash`. (The first cut forgot the lock's
  parent directory, and the selftest caught it: "lost the race for
  the lock" on every case.)
