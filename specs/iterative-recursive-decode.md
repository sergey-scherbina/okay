# Iterative recursive decode: a threshold, not a rewrite

## Overview

`lower-maxdepth-real-margin` (2026-09-10) bought real stack margin back
by lowering `Codecs.maxDepth` to 64, at a real cost: documents between
65 and 256 levels that decoded before now refuse, and `TestVector`'s
own recursion test already sits a quarter of the way to the new limit.
The cause is `Json.decode` and `Cbor.get` costing real JVM stack PER
LEVEL of a RECURSIVE schema (~4-8 KB, `specs/codecs.md` "The margin,
measured") — every level is a native call frame, so raising the limit
back up directly re-opens the risk the previous lane closed.

**Scope is smaller than the BACKLOG entry that opened this spec
claimed.** Reading `Staged.scala` before writing a line of code here:
both staged generators (compile-time and run-time) already bottom out
to the INTERPRETED decoder — `Json.decode`/`Cbor.get` — for a schema
they have already seen in the current expansion (recursion) or cannot
Mirror. They are bounded, unrolled code for everything else. So there
are exactly TWO recursive roots to fix, not four: the
`Json.decode`/`Cbor.get` pair (one fold, two entry points) and
`JsonStrict.Reader.get`. Fixing them fixes every door, staged included,
for free.

## Design

**A threshold, matching a shape this codebase already has twice.**
`Cont.Fuse` fuses binds into native closures up to 128, then spills to
a heap-backed `Bind` the trampoline runs at O(1) stack. `Cbor.In` and
`JsonStrict.Reader` already carry an `enter`/`leave` counter. The fix
here is a third instance of the same idea: recurse NATIVELY (today's
code, today's speed) up to a small threshold, then switch the
remaining descent to a stack-safe trampoline.

**The trampoline is `Cont`, reused, not reinvented.** The ONE place a
recursive-schema decoder calls itself one level deeper is wrapped in
`Cont.defer`:

```scala
// today (Cbor.get, sketch):
case p: Schema.SProduct[A] => /* … */ get(in, fieldSchema) /* recurses here */

// past the threshold:
case p: Schema.SProduct[A] if depth >= threshold =>
  Cont.defer(() => getC(in, fieldSchema, depth + 1))(fieldResult => /* combine */)
```

`Cont.defer`'s thunk is forced inside `/`'s own `@tailrec` loop, one
level per iteration (`eff-stack-safety.md` proved this exact mechanism
for `Eff`'s left-nested binds: "the inward applications happen inside
`/`'s loop… with `Bind(Defer(t,f), g)` rotated… exactly as
`Bind(Bind(a,f),g)` is today"). A SPIKE (2026-09-10, deleted, not
committed — `SpikeContDecode.scala`, a hand-rolled binary-tree CBOR
decoder bypassing `Schema` entirely so only the recursion MECHANISM was
measured) confirmed the mechanism on THIS shape of problem: a
`Cont.defer`-based decode built a value from a 500 000-level chain
where the plain recursive decoder overflowed at the default stack size,
to the identical value.

**Below the threshold, the decoder must be indistinguishable from
today's.** This is the part that makes the fix safe to land: a program
that never nests deeper than `threshold` never touches `Cont` at all —
same code shape, same allocations, same speed. Only a document past
the threshold pays for the trampoline, and it is paying to not crash.

**The threshold is small and independent of `Codecs.maxDepth`.**
Candidate: 24-32 — deep enough that no realistic document (this arc's
own repo-wide grep found no consumer nesting real data past a handful
of levels) ever leaves the fast path, shallow enough that the native
frames below it cost nothing worth measuring (a few hundred KB at
worst, per `specs/codecs.md`'s own per-level figures). Once this
lands, `Codecs.maxDepth` can go back up — the number stops being a
stack budget and becomes a policy choice again (a message this
deeply nested is probably not a message, rather than a message this
stack cannot afford).

## Interface

Nothing public changes shape: `Json.decode`, `Cbor.read`,
`Json.readStrict`, `Staged.json/cbor/strict` all keep their signatures.
Internal:

- `Cbor.get` (and `JsonStrict.Reader.get`) thread a `depth: Int` (they
  already have `Cbor.In.open` / `JsonStrict.Reader.open` for the
  budget — the SAME counter drives the native/trampoline switch, no
  second counter).
- A `getC`/`decodeC`-shaped twin returning a `Cont`-wrapped result,
  used only past the threshold; the public `get`/`decode` call it and
  `reset` the result at the point they cross into it, so a caller
  above the threshold never sees `Cont` in its type.

## Behavior

- [ ] a recursive schema nested past `Codecs.maxDepth` — raised back up
      once this lands — decodes on a default 1 MB JVM thread, to a
      depth this lane's own benchmark states (target: comparable to
      the interpreted CST builder's 100 000, not a new small ceiling)
- [ ] a document that never nests past the threshold decodes through
      the EXACT same code path as before this lane (no `Cont`
      allocation reachable below `threshold`) — asserted by a test that
      counts allocations or by construction (a guard, not a hope)
- [ ] `Json.decode`, `Cbor.get`, and `JsonStrict.Reader.get` agree with
      each other and with the staged generators on every existing
      corpus (`TestCompat`, `TestUnknownFields`, `TestInputDepth`,
      `TestStackBytes`/`TestStackMargin` all still green, values
      unchanged)
- [ ] MEASURED, JMH not a hand-timed loop (`compare`'s own convention,
      `docs/benchmarks.md`): the shallow/common-case cost regression is
      at most what `eff-stack-safety.md` measured for one `Defer` node
      on `Eff`'s fast path (+11% B/op, +14% time) — and ideally
      ZERO, since the hybrid design means the fast path should not
      allocate a `Defer` at all below `threshold`. A regression on the
      SHALLOW case is the one outcome that would sink this lane; a
      cost on the deep/pathological case is the price this lane exists
      to introduce.
- [ ] `TestVector`'s recursion test, and `Codecs.maxDepth` itself, get
      to move back toward their pre-lower-maxdepth-real-margin shape
      (a decision for the lane that lands this, not assumed here)

## Out of scope

- The two raw-JSON-nesting walks (`JsonValue`'s fast parser,
  `Json.value`'s CST-to-semantic projection) cost much less per level
  (~1-2 KB, `specs/codecs.md`) and are a separate, smaller,
  mechanically different fix (a classic explicit-stack recursive-
  -descent-to-iterative conversion, no `Cont` needed since there is no
  monadic combination to preserve — just container assembly). Not
  blocking: at any threshold-based `maxDepth` decided here, they are
  already safe at today's numbers.
- Making the interpreted decoder as fast as the staged one, or any
  other performance work unrelated to depth.
- Removing `Codecs.maxDepth` entirely. A wire contract still needs ONE
  number two services can agree on (`specs/codecs.md`,
  input-depth-both-wires) — this lane changes what that number costs,
  not whether one exists.

## Decisions

- **`Cont.defer`, not a bespoke explicit-frame stack.** A hand-rolled
  frame-stack interpreter (sketched and rejected before writing this
  spec) would likely cost less per level than `Cont`'s generic
  `Shift`/`Bind` machinery, but it is new, untested machinery solving a
  problem `Cont` already solves and already has tests and a JMH history
  for. Reuse is the smaller change and the better-understood risk;
  revisit ONLY if the JMH gate in Behavior fails and a bespoke stack
  measurably closes the gap.
- **A threshold, not "always trampoline".** The spike's own numbers
  argued for this directly: three unscientific timing rounds of a
  pure-`Cont` decode at realistic depth (5-50 levels) swung from 0.8x
  to 1.6x of native — noisy, but never confidently free, and
  `eff-stack-safety.md`'s properly-measured +11%/+14% for one `Defer`
  node on a hot path is the trustworthy number in the same direction.
  A tax on every decode, for a hazard only pathological input creates,
  is the wrong trade for a module whose whole pitch is 1.5-2.5x circe.

## Results

Not yet built. The spike that informed this spec (SpikeContDecode.scala,
2026-09-10, deleted — not a benchmark, a feasibility check) found:

| check | result |
|---|---|
| `Cont.defer`-based decode of a 500 000-level chain | succeeds, correct value (native overflows at default stack) |
| hybrid (native to 24, then `Cont`) at depth 5/20 (below threshold) | ~native speed, noise-level difference |
| hybrid past the threshold, 500 000 levels | succeeds, correct value |
| pure-`Cont` decode at depth 5/20/50, 3 independent timing rounds | 0.8x, 1.6x, 1.0x of native — too noisy to trust the number, not too noisy to see it is not free |

A real JMH benchmark, run the way `compare`'s lanes are (forks, GC
profiling, a load reference), is Behavior's unchecked box and the gate
before this lands.
