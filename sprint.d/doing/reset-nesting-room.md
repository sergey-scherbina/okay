- [ ] reset-nesting-room — backlog shift-effect-level1's (1) (operator:
      "Продолжай", 2026-10-02): nested resets of the SAME answer type start
      a machine each inside its parent's, and the JVM stack runs out at
      3 000-10 000. `reset` counts its nested runs per thread and, past the
      room, runs the next level on a fresh stack (`StackSwitch.fresh`, as
      Cont's strict `k` does). Test: 100 000 nested resets. Behaviour
      otherwise unchanged. (2026-10-02)
