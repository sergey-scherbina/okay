## stack-host-three: Native reads its stack; the inventory audited under the host-stack rule; four stale items closed

- Scala Native reads the stack at the end of every room: the runtime's `ThreadInfo` gives the bounds and
  the guard page, and a `stackalloc` gives the pointer, a few ns with no system call. The grant follows
  the JVM's rule (half of what is left over a 64 KB margin, at a cold level's size), and a fresh 1 GB stack
  starts from a small room that is then read. TestContStackNative has the layout guard back and a
  grant/refuse test.
- specs/stack-safety.md, "Audit under the host-stack rule". The 369 inventory rows are sorted by the kind
  of their bound. 59 are bounded only by a value the program builds at run time, which a loop can make as
  deep as it likes. They are listed by module and priority as backlog okay-core/stack-program-built-depth.
- Closed as moot (their machines are gone): cont-core-remaining-costs, cont-frames-register-pressure
  (kept as a lesson), cont-stack-statepara-time-residual. shift-effect-level1 now carries
  machine-start-cost's number.
