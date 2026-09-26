- [ ] handlers-vs-virtual-calls — the rung between the plain loop and
      the staged handler: the SAME 1 000 State+Writer operations through
      an ordinary abstract class (`get`/`set`/`tell` as virtual methods),
      once monomorphic and once MEGAMORPHIC (four identical subclasses
      rotated per invocation, so C2 sees four receivers at every call
      site and cannot inline), with the Vector log and with a mutable
      buffer. Measured beside plainLoopBuffer, plainLoopVector and
      stagedDirect in the same series; docs/benchmarks.md §2c.
      (2026-09-26, operator ask)
