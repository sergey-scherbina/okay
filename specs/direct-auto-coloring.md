# direct-auto-coloring — v2: no marks, gated by capability and marker

## Overview

Auto-coloring lets a `direct` block use monadic values and effect
operations as plain values with NO marks at all: the typer inserts a
Conversion where an `F[A]` stands in an `A` position, and the macro
rewrites those conversion calls with the machinery direct-macro v1
already has. The entire design is about WHERE the conversion is
allowed to fire, and both gates were named by the user (2026-09-01):

1. **The capability gate**: the block is a context function
   `DirectCtx[F] ?=> A`; the conversions require `using DirectCtx[F]`
   — outside a direct block the capability does not exist, the
   conversion does not resolve, and `F[A]`-as-`A` stays the compile
   error it always was. Compile-time refusal, stronger than the
   phantom marks' runtime throw. (dotty-cps-async's CpsMonadContext
   pattern.)
2. **The marker gate**: operation types color only when a
   `Direct.Effect[G]` instance exists — arbitrary `G[A]`s never
   silently color; registering an effect signature for auto-coloring
   is an explicit, one-line, per-project decision.

Explicit marks (`.?`, `.reflect`, `.!?`) keep working in the same
block and remain the recommended default; auto-coloring is the
opt-in ergonomic layer.

## Interface

```scala
object Direct:
  /** the capability: exists ONLY inside a direct block */
  final class DirectCtx[F[_]] private[Direct] ()

  /** marker: G's operations may auto-color inside direct blocks */
  trait Effect[G[_]]

  // the phantom conversions — never run; the macro rewrites every call
  /** the block's own monadic values: F[A] as A */
  given selfColor[F[_], A](using DirectCtx[F]): Conversion[F[A], A]
  /** marked operations: G[A] as A, membership in the block's row
   * checked by the macro exactly as for .!? */
  given opColor[F[_], G[_], A](using DirectCtx[F], Effect[G]): Conversion[G[A], A]

  // DirectApply.apply now takes a context function — plain blocks
  // still adapt automatically, v1 call sites unchanged:
  inline def apply[A](inline block: DirectCtx[F] ?=> A)(using inline M: Monad[F]): F[A]
```

Use sites need `import Direct.{*, given}` (a bare `*` does not bring
givens in Scala 3) and `import scala.language.implicitConversions`.
Naming note: `Direct.Effect` shadows `!.Effect` if both are imported
unqualified — rare, and the qualified names disambiguate.

## Behavior

- [ ] `val x: Int = m` colors in an ascribed position; `f(m)` colors
  in an argument position; `m + 1` colors on member selection —
  each equal to the `.?` spelling of the same block
- [ ] operations color via the marker: with
  `given Effect[Writer % String]`, a bare `Writer("a")` in an
  Int-expected position... does not arise; in practice ops surface
  through selections/arguments/ascriptions the same way — the
  op-conversion call is rewritten as `.!?` is, row membership
  checked, wrong-row refused with the row named
- [ ] **the discard guard**: a statement of monadic/marked type with
  no conversion (statements have no expected type — auto-coloring
  CANNOT fire there) is a COMPILE error naming the fix (`.?` /
  `.!?`); a silently dropped effect is the one classic auto-coloring
  bug and it must not compile
- [ ] `val x = m` (no ascription) infers `F[A]` and does NOT color —
  documented, not fought: inference sees the monadic value; ascribe
  to color
- [ ] outside a direct block nothing colors: no DirectCtx, normal
  type errors
- [ ] a G with no `Effect[G]` instance never colors: normal type
  errors name the mismatch
- [ ] explicit `.?` and `.!?` still work in an auto block, mixed
  freely
- [ ] every v1 test still passes with the context-function signature
  (plain blocks adapt)

## Out of scope

- Auto-coloring under lambdas — same refusal as v1, same reason.
- A `Conversion` that RUNS (non-phantom fallback) — the conversion
  outside macro rewriting is meaningless (it would need to run the
  monad); the phantom throws with a message naming the macro.
- Effect instances in the core — the marker is the user's register;
  the core ships none (additive doctrine).

## Design

- The macro strips the context-function wrapper
  (`Block(List(DefDef(anonfun, ctx :: _, body)), Closure)`) and
  compiles `body`; references to the ctx parameter occur only inside
  conversion calls, which are rewritten away whole.
- Conversion-call detection joins mark detection: a call whose
  callee chain roots at `selfColor`/`opColor`'s symbol is a mark;
  its converted operand is the receiver. selfColor operands go the
  `.?` route (reflect), opColor operands the `.!?` route
  (row-membership check, Free.Inject at the row, reflect).
- The discard guard runs in compileBlock: a pure statement whose
  type is `F[_]`, or `G[_]` with an Effect[G] instance in scope, is
  refused with the fix named.

## Decisions

(fill as they are made)

## Results

(fill after verify)
