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

Explicit marks (`.?`, `.reflect`) keep working in the same
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
   * checked by the macro exactly as for a .? on an op */
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

- [x] `val x: Int = m` colors in an ascribed position; `f(m)` colors
  in an argument position; `m + 1` colors on member selection —
  each equal to the `.?` spelling of the same block
- [x] operations color via the marker: with
  `given Effect[Writer % String]`, a bare `Writer("a")` in an
  Int-expected position... does not arise; in practice ops surface
  through selections/arguments/ascriptions the same way — the
  op-conversion call is rewritten as `.!?` is, row membership
  checked, wrong-row refused with the row named
- [x] **do-notation statements** (v2.1, user ask 2026-09-01): a bare
  statement of the block's F or row-operation type RUNS — the `_ <-`
  reading, an implicit `.?` with the value dropped. `Writer("a")` on
  its own line tells; `direct[Option] { None; 2 }` is None; a bare
  List statement re-runs the rest per element (honest do-notation
  multi-shot). This subsumes the old discard-guard error for the
  block's own types: running is what the statement means, and
  building-without-running was dead code in every reading
- [x] **the narrowed guard**: a statement of a FOREIGN marked type
  (an Effect[G]-registered G that is neither this block's F nor in
  its row) still refuses to compile — it can be neither run nor
  meaningfully dropped. Unmarked foreign types stay the compiler's
  own unused-value warning, as everywhere else
- [x] `val _ = op` and `val x = op` keep the VALUE un-run — binding
  is explicit consent to hold the program as a value (building
  sub-programs inside a block is legitimate); only bare statement
  position carries the do-notation reading
- [x] `val x = m` (no ascription) infers `F[A]` and does NOT color —
  documented, not fought: inference sees the monadic value; ascribe
  to color
- [x] outside a direct block nothing colors: no DirectCtx, normal
  type errors
- [x] a G with no `Effect[G]` instance never colors: normal type
  errors name the mismatch
- [x] explicit `.?` marks still work in an auto block, mixed
  freely
- [x] every v1 test still passes with the context-function signature
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
  its converted operand is the receiver; markTerm then dispatches
  by type (F[T] reflects; a row operation is injected, then
  reflected).
- The discard guard is a traversal of the whole block (marks or
  not, lambdas skipped): a discarded statement whose type is
  `F[_]`, Free-derived, or `G[_]` with an Effect[G] instance in
  scope is refused with the fix named.

## Decisions

- **One mark, one conversion route** — mark dispatch and conversion
  dispatch converge on the same by-TYPE decision (markTerm): F[T]
  reflects, a row operation injects then reflects. selfColor and
  opColor stay separate GIVENS (different gates: the capability
  alone vs capability + Effect marker) but their calls are one mark
  kind to the macro.
- **Unit-typed operations cannot auto-color, by typer physics** —
  statement position has no expected type, and a Unit ascription is
  VALUE DISCARD, which preempts conversion search. So `tell`-like
  ops surfaced the whole question. v2.1 answers it: statement
  position is rewritten by the MACRO (no conversion involved), so
  Unit ops need no mark at all — the statement is the mark.
- **Auto-coloring resolves at the DECLARED type** — a smart
  constructor typed at the trait (`def ask: Reader[Int, Int]`)
  colors; a raw case constructor's precise type (Reader.Ask[Int,
  Int]) defeats G inference and does not. Documented, not fought.

## Results

- v2.1 (do-notation statements): 11 tests in TestDirectAuto — bare
  statements of the block's monad run (None short-circuits, a bare
  List statement re-runs the rest per element, runs counted), bare
  ops tell with no mark, `val` keeps a program un-run (binding is
  consent to hold the value), foreign marked types still refuse.
  The None.type wrinkle paid for once: a singleton carries no type
  arguments, so runnableElem also consults the base type at the
  block's monad (Option[Nothing]).
- v2: 8 initial tests + the 16 v1 tests unchanged (plain
  blocks adapt to the context-function signature with no edits).
  Ascribed/argument/selection positions color; ops color via the
  Effect marker at declared types; the discard guard catches the
  dropped-statement case in blocks with and without marks; nothing
  colors outside a block or without a marker; marks mix freely.
- The v1 machinery was reused whole: the entire v2 delta is the
  capability + two phantom givens + conversion-call detection folded
  into asMark + the discard guard traversal.
