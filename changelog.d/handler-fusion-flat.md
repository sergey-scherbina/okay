## handler-fusion-flat - Handler.flat[R], the row handler as one dispatch expression

`Handler.flat[R]` (core, Handler.scala): a macro that reads the row's
members off the APPLIED type (Distinct's trick — only a union's body is
an `OrType`, whatever the spelling), summons each member's `Handler`
and `TypeableK`, binds them to vals outside the handler object, and
emits `handle` as `if t1.test(a) then h1.handle(a) else … else hk.handle(a)`
— the tests the nested `Handler.union` chain already performs, minus
the k−1 handler objects and virtual calls between a test and its
answer. A `%`-shaped member (`Tag.Of[K, F][Any]` is `Tag[K, F, Any]`)
is read back to its constructor as a lambda over the last argument. A
member without a `Handler` in scope is a compile error naming it. The
one cast per member is `split`'s claim, stated in the macro once.

Laws (`TestFlat`, 5): agreement with `union` on a mixed program by
answer AND by every handler's trace (identity handlers would agree on
the answer even if every operation went to the wrong one); each of the
four positions alone; a left-nested spelling; a `Tag.Of` member; the
refusal. `TestReadmes.run` and `TestStepper` build their agent rows
with `flat`; typepedia and okay-agent.md say so.

Measured (`FlatDispatchBenchmark`, two rounds × two forks, minima,
bytes identical on all eight lanes): position 4 — union 108.4, **flat
100.2 (1.08x)**, the hand-written flat match that CALLS the handlers
94.9, the one that inlines their bodies 88.2; position 1 — parity
(95.4 / 94.8). So the 1.24x ceiling staged-block-lanes measured was
two levers, and a macro over opaque givens has only one of them: the
reachable ceiling is 1.14x and the macro is within 5.6% of it. The
first cut spliced the givens straight into `handle` — a `given x: T =
…` in a class body is a lazy val, so every operation paid an accessor,
and it measured no faster than the chain (106.5 vs 109.1) and slower
at position 1. The 5.6% that remains is `Class.isInstance` through a
field against a constant-class `instanceof`, under every `split` in
the library — filed as `typeablek-instanceof`.
