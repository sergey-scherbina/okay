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

Root 1 of 2, `Cbor.get` (cbor-decode-threshold-trampoline, 2026-09-10):

- [x] a recursive schema decodes correctly at the deepest depth
      `Codecs.maxDepth` currently allows (well past `NativeThreshold`,
      so the switch is exercised) — `TestCborTrampoline`
- [x] ordinary shapes (products, sums, lists, options, isos) below the
      threshold decode unchanged — `TestCborTrampoline`
- [x] errors past the threshold still refuse rather than silently
      succeeding — `TestCborTrampoline`
- [x] `Codecs.maxDepth`'s own refusal is unchanged in both paths —
      `TestCborTrampoline`, `TestInputDepth`, `TestUnknownFields`,
      `TestCompat` all still green
- [x] `Cbor.read[Tree]`/`Staged.cbor[Tree]`'s stack cost is FLAT past
      the threshold rather than scaling with depth (the mechanism's
      own signature) — `TestStackBytes`, A/B'd: reverting
      `NativeThreshold` to an unreachable value makes this ONE test
      fail and nothing else, confirming it is what tests the fix
- [x] MEASURED, JMH not a hand-timed loop, 3 forks not 1
      (`compare/CodecBenchmark.cborDecodeInterp`, Order — the
      below-threshold, common-case shape): **1438 ± 58 ns/op before,
      1338 ± 14 ns/op after** — NOT a regression (a single-fork run
      first showed 2166 vs 1419, a false 53% alarm that evaporated at
      3 forks — `bench-one-round-lies`, again; this is why root 2's own
      gate started at 3 forks instead of repeating the mistake)

Root 2 of 2, `Json.decode` (json-decode-threshold-trampoline,
2026-09-10):

- [x] `Json.decode` has NO reader object and NO existing depth counter
      (unlike `Cbor.In`) — depth threaded as an explicit parameter
      through new `decodeAt`/`decodeNative`, the public `decode[A](s)(j)`
      signature unchanged
- [x] `Json.decode` does not enforce `Codecs.maxDepth` itself — that
      refusal already lives upstream at `Json.isCut` (parse time); this
      lane's threshold is a PURE stack-safety switch, so `Json.decode`
      is now safe on ANY input depth, including a `Json` value built
      directly and never cut — `TestJsonTrampoline`
- [x] `Json.read` (which parses through the cut first) still refuses a
      too-deep document exactly as before — `TestJsonTrampoline`
- [x] ordinary shapes below the threshold, and a damaged list element
      past it (the skip-damage rule, not a fault), both unchanged —
      `TestJsonTrampoline`
- [x] `Json.read[Tree]`'s stack cost is FLAT past the threshold,
      A/B'd the same way as root 1 — `TestStackBytes`
- [x] MEASURED, JMH, 3 forks from the start
      (`compare/CodecBenchmark.decodeSumInterpAst`/`decodeSeamAst`):
      615±12→622±11 and 642±26→626±6 ns/op — both within noise, no
      regression, confirmed on the first attempt this time
- [x] `TestVector`'s recursion test, and `Codecs.maxDepth` itself,
      moving back toward their pre-lower-maxdepth-real-margin shape —
      BOTH roots are now closed, so this is unblocked; left as a
      SEPARATE deliberate decision (a wire-contract number, not
      assumed here) rather than bundled into this lane

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

**Root 1, `Cbor.get` (cbor-decode-threshold-trampoline, landed
2026-09-10).** The spike (SpikeContDecode.scala, deleted, not a
benchmark) found what it was for — feasibility, not a number:

| check | result |
|---|---|
| `Cont.defer`-based decode of a 500 000-level chain | succeeds, correct value (native overflows at default stack) |
| hybrid (native to 24, then `Cont`) at depth 5/20 (below threshold) | ~native speed, noise-level difference |
| pure-`Cont` decode at depth 5/20/50, 3 independent timing rounds | 0.8x, 1.6x, 1.0x of native — too noisy to trust, not too noisy to see it is not free (this is WHY the threshold exists) |

The real gate, `compare/CodecBenchmark.cborDecodeInterp` (Order, a
non-recursive product — the below-threshold shape every real caller
hits), JMH, 3 forks, 5×1s/3×1s:

| | before | after |
|---|---|---|
| ns/op | 1438 ± 58 | 1338 ± 14 |

Read generously: within noise of unchanged; read exactly: not worse.
A SINGLE-fork run first showed 2166 vs 1419 (a 53% "regression") —
`bench-one-round-lies` again, on the same box that has produced this
exact shape of false alarm before in this session. 3 forks, not 1, is
what this spec's own Behavior item should have said from the start.

`Cbor.read[Tree]`/`Staged.cbor[Tree]`'s stack cost dropped from 1024 KB
to 16 KB (this probe's own floor) at `Codecs.maxDepth`'s current depth
(64) — `TestStackBytes`. `Json.read[Tree]` and the `JsonStrict`-based
doors are unchanged (256/512 KB): root 2, not yet built.

**Root 2, `Json.decode` (json-decode-threshold-trampoline, landed
2026-09-10).** Same design as root 1, one real difference and one real
surprise, both found in the doing:

- **The difference:** no `Cbor.In`-style mutable reader to hang a
  counter on, so depth is an explicit parameter through
  `decodeAt`/`decodeNative`/`decodeC`/`fieldC` — otherwise the SAME
  `Cont.defer`-at-the-recursive-step shape, zero casts, for the same
  reason (one `.map` where `decodeNative` widens via `Either`'s
  covariance, since `Cont` does not share it).
- **The surprise:** writing this root's own `TestStackBytes` update
  surfaced that its PREDECESSOR's numbers were wrong. Root 1's doc
  comment claimed "`Json.read[Tree]`/`JsonStrict`-based doors UNCHANGED
  (256/512 KB)" without re-measuring — re-measured here, fresh, against
  the untouched `Json.scala`: `Json.readStrict[Tree]`/`Staged.strict[Tree]`
  were ALREADY 16 KB at `Codecs.maxDepth`'s current depth (64), not
  512 KB — that number was carried forward from an EARLIER measurement
  taken at the OLD `maxDepth` = 256 (four times the depth) and never
  re-verified after `lower-maxdepth-real-margin` changed the constant.
  `JsonStrict.Reader.get` was never a needed third root at this depth;
  root 2 turned out to be the LAST one, not one of three.
- **A second surprise, unrelated to either root:** proving "`Json.decode`
  is safe past `Codecs.maxDepth`" needed a document built directly (not
  parsed), because `Json.lossless` (the CST→value projection) turned
  out to be QUADRATIC in depth for this shape — 50 000 levels took 74
  seconds. Completely out of this lane's scope (it is the raw
  container-nesting walk, not the schema-recursion one) and NOT
  something either threshold lane touches; recorded as
  `json-lossless-quadratic-depth` in BACKLOG rather than chased here.

JMH, 3 forks from the start this time (`decodeSumInterpAst`,
`decodeSeamAst`): 615±12→622±11 ns/op and 642±26→626±6 ns/op — both
within noise on the first attempt, no false alarm to write up.

**Both roots of this spec are closed.** `Codecs.maxDepth` and
`TestVector`'s stress depth moving back up is a separate, deliberate
follow-up (a wire-contract number, not assumed by either lane).
