## reset-nesting-room - nested resets of one answer type, any depth

Backlog shift-effect-level1's (1) (operator: "Продолжай", then
"Приземляй"; specs/shift-effect.md).

- A `reset` that runs its own machine runs it inside what forced it, so
  nested resets of one answer type overflowed the JVM stack at 3 000 to
  10 000. `Shift.run` now counts these runs per thread (a `ThreadLocal`)
  and, past the room, runs the next on a fresh stack (`StackSwitch.fresh`,
  as Cont's strict `k` does). The room can be set with
  `-Dokay.shift.room`.
- TestShift has a new test: 100 000 nested resets. It was watched to fail
  with the switch off (StackOverflowError).
- Cost: 100 small resets in 10.8 µs against 10.3, the bytes equal. The
  alternatives without a `ThreadLocal`, and why each failed, are in the
  spec.
