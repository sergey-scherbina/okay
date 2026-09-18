- [ ] workflow-timers-index — what the durable-workflow arc left
      behind, moved here from the sprint when the arc closed
      (backlog-audit-0918, 2026-09-18), because a closed arc's sprint
      entry is the first thing the next agent reads and the worst place
      for open work. `Timers.due`, `Signals.next` and `Retire.census`
      SCAN a topic — docs/durable-workflows.md says "honest for
      thousands of runs and wrong for millions" — and there is no
      scheduler process: `tick(now)` is a call you make from your own
      loop. That is the gap between the flagship ROADMAP P13 names
      (durable workflows ahead of dataflow) and an engine somebody runs
      at scale. Two pieces, each its own lane when picked: an INDEX of
      due timers and waiting signals (a topic keyed by due time, or a
      compacted projection the tick reads instead of the history), and
      a loop that calls `tick` so an adopter does not write one — with
      the lease it already has, so two loops collide harmlessly.
      TRIGGER: the first adopter past a few thousand live runs, or the
      flagship being SOLD as one — whichever comes first. Not before:
      the scan is the honest answer at the size anything here runs.
