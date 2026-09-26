## handlers-vs-virtual-calls - the handler as an ordinary interface, measured

- `StagedBenchmark` has the State+Writer handler as a plain class
  hierarchy: one subclass (inlined) and four identical subclasses
  rotated per invocation (megamorphic: the JIT log reads "failed to
  inline: virtual call" at every call site), over the persistent
  `Vector` log and a mutable buffer.
- One round, stopped by the operator: a megamorphic call adds under a
  nanosecond an operation over the plain loop (1.43 against 0.73 us
  for 1 000 operations), where the staged handler costs 7.8.
  docs/benchmarks.md §2c.
