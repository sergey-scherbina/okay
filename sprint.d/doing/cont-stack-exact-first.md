- [ ] cont-stack-exact-first — PRIORITY: MEDIUM (operator, 2026-10-04;
      specs/cont-stack.md, road 3). What still waits on the host stack
      (an opaque body answering a plain value) should know its bound
      exactly: read the stack pointer (StackRoom's FFM reader, JDK 22+
      with native access) instead of counting levels at a guessed size.
      The count, sized by cont-stack-cold-bytes-per-level, stays as the
      fallback (JDK 17–21, no native access, Native). cont-core-design
      took the reader out of the runner (`StackSwitch.more`, the gauge),
      and its cost was the reason: a read is ~7 µs before it looks
      (Results). Price that against today's count before bringing it back:
      read once per room, not per level.
