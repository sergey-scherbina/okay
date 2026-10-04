- [ ] cont-leaf-by-platform — PRIORITY: MEDIUM (2026-10-04, from
      cont-leaf-forms). MEASURED 2026-10-04 on the JVM (ContDepthBenchmark,
      history.d cont-leaf-depth), contAnswer's body `k(x + 1) + 1`:
      | depth | lazy (macro) | strict (`shiftLeaf`) | ratio |
      | 1 000 | 28.7 µs | 24.3 µs | 0.85x |
      | 100 000 | 7.84 ms | 2.27 ms | 0.29x |
      | 1 000 000 | 115.0 ms | 28.8 ms | 0.25x |
      The strict leaf stays linear (23–29 ns a level, a StackSwitch at 1e6
      included). The lazy one grows (28 → 78 → 115 ns a level) while its
      bytes stay linear (376 B a level, strict 216). GC is about 24% of an
      op in both, so GC is not the growth. Likely (not measured apart): the
      lazy `k`'s pending rests form a heap chain walked cold, where the
      strict leaf's sit on the stack.
      NEXT, if taken: ContMacro picks the strict leaf on JVM and Native and
      keeps the CPS transform for JS. The macro runs on the JVM for every
      platform, so the choice has to come from something the platform's
      source set provides (a given the macro summons). Two things to settle
      first: (1) Native measured the same way; it has no JMH here, so this
      needs a timed loop. (2) Past a switch, a strict body's rest runs on
      another thread: a ThreadLocal or a lock held across `k(…)` sees it.
      That is already true of every opaque body, but an answer-using body
      the macro could read never had it.
