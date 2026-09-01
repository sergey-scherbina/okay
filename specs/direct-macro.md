# direct-macro — the flat block, v1 (scoped)

## Overview

`direct[F] { block }` lets a plain block use monadic values as plain
values — `val x = m.?` with no for-comprehension — by rewriting the
block at compile time into the Cont binds that
specs/monadic-reflection.md established as the semantic floor. The
macro adds SYNTAX only: every program it emits is one the user could
have written with `Monadic.reflect`/`reify` by hand, so semantics
(multi-shot, short-circuit, the stack discipline of the reflected
monad) are inherited, not re-implemented. v1 is deliberately scoped:
the general transform exists (dotty-cps-async) but costs years of
re-typing machinery; the scoped one is a few hundred lines because it
refuses the hard corner (marks under lambdas) instead of solving it.

## Interface

```scala
object Direct:
  /** the mark: typechecks as A so the block typechecks BEFORE macro
   * expansion; never executes — the macro rewrites every call.
   * Outside a direct block it throws at runtime by design. */
  extension [F[_], A](m: F[A])
    def ? : A
    inline def reflect: A   // the named spelling of the same mark

  /** rewrite block: marks become Monadic binds, the result is F[A] */
  transparent inline def direct[F[_]](inline block: Any)(using Monad[F]): F[?]
```

The mark is `Direct.?`, NOT `Monadic.?` — the two return different
types (A vs Cont) because they live on different sides of expansion.
One imports `Direct.*` for flat blocks or `Monadic.*` for
for-comprehensions; mixing both imports in one scope is an ambiguity
the compiler will name.

**Effects are the first-class case** (user directive 2026-09-01):
a `direct` block over the program monad `!` must work exactly as the
same program written monadically — operations reflected (`Writer
.tell("a").?`), the block's value an `A ! Row` that handlers run
afterwards, rows and `+` untouched. The Monad instance is the
existing `Monad[Free[Row, *]]`; nothing effect-specific enters the
macro. Since `[A] =>> A ! Row` is noisy at a call site, F is
inferred from the EXPECTED type where one is given —
`val p: Int ! W = direct { ... }` names no F at all; `direct[F]`
stays for expression positions.

## Behavior

- [ ] `direct[Option] { val x = mx.?; val y = my.?; x + y }` ==
  the for-comprehension equivalent, on every TestMonadic scenario
  (Option short-circuit, Either error channel, List multi-shot)
- [ ] EFFECTS work as through `!` itself: a direct block over
  `[A] =>> A ! (Writer % String)` reflecting `Writer.tell` answers
  the same (log, value) under `Writer.run` as the monadic program —
  and a two-effect row (`Writer % String + Reader % Int`) reflects
  `tell` and `ask` in one block, handlers peeling as always
- [ ] `val p: Int ! W = direct { ... }` — F inferred from the
  expected type, no type argument written
- [ ] marks in SUBEXPRESSIONS are hoisted in evaluation order:
  `f(a.?, b.?)` binds a before b, exactly left-to-right
- [ ] `if`/`match` with marks in condition/scrutinee and branches:
  only the taken branch's effects run
- [ ] a mark under a lambda is a COMPILE error naming the position
  and the workaround (bind to a val before the lambda)
- [ ] `while`/`try` containing marks: compile error, "v2" named
- [ ] a mark outside any direct block: the phantom throws with a
  message naming the macro
- [ ] a block with NO marks still compiles: `direct[F] { 42 }` ==
  `F.pure(42)`
- [ ] multi-shot inside the flat block: a reflected List re-runs the
  REST OF THE BLOCK per element (vars shared across runs — the
  documented footgun, asserted by a test, not hidden)

## Out of scope (v2 roads, recorded not promised)

- **Auto-coloring** (no marks at all): the Conversion trick —
  a scoped `given Conversion[F[A], A]` lets the block typecheck,
  the macro rewrites the conversion calls. Works (dotty-cps-async's
  automatic mode) but must be locked to the block's scope and
  degrades error messages; explicit marks first.
- **`while`/`try`** — recursion-encoding for while, error-channel
  reification for try (Throws.scala is the seam).
- **Answer-type modification inside a block** — the block is the
  DIAGONAL (one F, answers F[A] throughout); that fixed answer type
  is exactly what makes the scoped macro cheap (no re-typing tower).

## Design

The rewrite is statement-level monadic normalization (ANF for marks):

1. Type the block (the compiler already did — inline macro).
2. Walk statements; in each, hoist every mark call out of
   subexpressions into fresh vals, left-to-right (evaluation order
   preserved by construction).
3. Fold the statement list right-to-left:
   `val x = mark(m); rest` → `Monadic.reflect(m).flatMap(x => rest)`;
   pure statements ride inside the continuation unchanged; the final
   expression becomes `Cont.Pure(_)`; `reify` closes the block.
4. Every Cont in the emitted tree is `Cont[·, F[A], F[A]]` — the
   diagonal, ONE answer type per block: retyping is Expr-level
   quoting, no Tasty surgery.
5. Guards run before rewriting: any mark under a Lambda/by-name node
   → positioned compile error; while/try with marks → the v2 error.

## Decisions

- **A phantom mark, not Monadic's operators** — the macro's block is
  typed BEFORE expansion, so inside it `m.?` must have type A; the
  Monadic operators return Cont and cannot be reused. The phantom
  throws outside a block rather than compiling to nothing, so a
  stray mark fails loudly at the first run, not silently.
- **Diagonal only** — one F per block, fixed answer F[A]. This is
  what "direct style" means to a caller, and it is the entire
  difference between a few hundred lines and dotty-cps-async.
- **Refuse lambdas instead of coloring them** — the continuation
  cannot cross a function boundary the macro does not rewrite;
  rewriting HOFs generically is the expensive half of the general
  problem. A clear error with a workaround beats a wrong capture.

## Results

(fill after verify)
