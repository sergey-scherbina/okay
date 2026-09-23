# direct-macro — the flat block, v1 (scoped)

## Overview

`direct[F] { block }` lets a plain block use monadic values as plain
values — `val x = m.!?` with no for-comprehension — by rewriting the
block at compile time into the monad's own plain flatMap binds
(direct-flatmap-emission, 2026-09-02, in Decisions; the first cut
emitted the Cont binds that specs/monadic-reflection.md established
as the semantic floor, and Monadic stays that floor and the no-macro
API). The macro adds SYNTAX only: every program it emits is one the
user could have written with flatMap by hand, so semantics
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
    def reflect: A   // the word
    def !? : A       // the symbol
    def ? : A        // the glyph (unwrap-glyph, 2026-09-17)
    def unary_! : A  // the prefix, for rows: `!prog`
  // ONE mark, four spellings: each serves monadic values AND raw
  // operations — the macro dispatches by type (an F[T] reflects; an
  // operation of the block's row is injected, then reflected)
  //
  // THIS BLOCK USED TO SHOW `.?` ALONE, AND WAS WRONG FOR A YEAR:
  // Direct.scala retired that spelling (it collided with Throws' and
  // with the row peek) while this section kept advertising it. The
  // contradiction with the Decisions entry below cost an hour in the
  // applicative-do lane — a block written with `.?` compiled, ran and
  // answered correctly through auto-coloring while the glyph did
  // nothing at all. specs/unwrap-glyph.md removed both collisions and
  // gave the glyph back; the Decisions entry records the history.

  /** rewrite block: marks become Monadic binds, the result is F[A].
   * direct[F] names only the monad (partial type application via
   * DirectApply); with an expected type both infer:
   * val p: Int ! W = direct { ... } */
  inline def direct[F[_]]: DirectApply[F]
  final class DirectApply[F[_]] extends AnyVal:
    inline def apply[A](inline block: A)(using inline M: Monad[F]): F[A]
```

The mark is `Direct.!?`, NOT `Monadic.!?` — the two return different
types (A vs Cont) because they live on different sides of expansion.
One imports `Direct.*` for flat blocks or `Monadic.*` for
for-comprehensions; mixing both imports in one scope is an ambiguity
the compiler will name.

**Effects are the first-class case** (user directive 2026-09-01):
a `direct` block over the program monad `!` must work exactly as the
same program written monadically — operations reflected (`Writer
.tell("a").!?`), the block's value an `A ! Row` that handlers run
afterwards, rows and `+` untouched. The Monad instance is the
existing `Monad[Free[Row, *]]`; nothing effect-specific enters the
macro. Since `[A] =>> A ! Row` is noisy at a call site, F is
inferred from the EXPECTED type where one is given —
`val p: Int ! W = direct { ... }` names no F at all; `direct[F]`
stays for expression positions.

## Behavior

- [x] `direct[Option] { val x = mx.!?; val y = my.!?; x + y }` ==
  the for-comprehension equivalent, on every TestMonadic scenario
  (Option short-circuit, Either error channel, List multi-shot)
- [x] EFFECTS work as through `!` itself: a direct block over
  `[A] =>> A ! (Writer % String)` reflecting `Writer.tell` answers
  the same (log, value) under `Writer.run` as the monadic program —
  and a two-effect row (`Writer % String + Reader % Int`) reflects
  `tell` and `ask` in one block, handlers peeling as always
- [x] `val p: Int ! W = direct { ... }` — F inferred from the
  expected type, no type argument written
- [x] marks in SUBEXPRESSIONS are hoisted in evaluation order:
  `f(a.!?, b.!?)` binds a before b, exactly left-to-right; and a
  PURE sibling that precedes a mark is hoisted to a val before it
  (audit-fixes, 2026-09-02: `g({log += "a"; 1}, xs.reflect)` used to
  evaluate the pure argument after the effect and once per
  continuation under multi-shot — "aaa" for a List of three; now
  once, first). Pure siblings after the last mark stay in place and
  run per continuation, as the source reads.
- [x] `if`/`match` with marks in condition/scrutinee and branches:
  only the taken branch's effects run
- [x] a mark under a lambda is a COMPILE error naming the position
  and the workaround (bind to a val before the lambda)
- [x] `try` containing marks — SHIPPED (direct-try, see Out of
  scope below for the seam); `while`/foreach/map graduated too
  (specs/direct-loops.md)
- [x] `try` inside `direct[[X] =>> E ?=> X]` — SHIPPED (direct-try-ctx,
  2026-09-03): catches a throw from the body, deferred to the
  context function's APPLICATION (`provide`), not its construction —
  a repeated `provide` over the same produced value catches
  correctly each time, not just once
- [x] a marked val KEEPS ITS SYMBOL (audit-fixes, 2026-09-02): the
  val is re-bound to the continuation's parameter rather than
  substituted away, so a local `def` after it still refers to it,
  and a `var` bound from a mark can be reassigned — both were
  compile errors ("used outside the scope where it was defined",
  "Reassignment to val v") found by the audit
- [x] an `import` is a statement a block may contain (cont-in-direct,
  2026-09-17): it binds nothing and runs nothing, so it rides along
  into the built tree. It used to be "an unsupported statement",
  which made a scoped spelling — `import Cont.direct.*`, `import
  State.modify` — unusable inside a block
- [x] a mark outside any direct block: the phantom throws with a
  message naming the macro
- [x] a block with NO marks still compiles: `direct[F] { 42 }` ==
  `F.pure(42)`
- [x] multi-shot inside the flat block: a reflected List re-runs the
  REST OF THE BLOCK per element (vars shared across runs — the
  documented footgun, asserted by a test, not hidden)
- [x] ONE mark for values and operations (user ask 2026-09-01,
  refined the same day): `Writer("a").!?` in a row block — the macro
  finds Row from F = A ! Row, checks membership, emits
  Free.Inject[Row, A](op) reflected; a mark that is neither F[T]
  nor a row operation is refused with both possibilities named.
  The separate `.?` was REFUTED as redundant: the type already
  says which case it is, so the user should not have to

## Deep recursion (deep-recursive-direct, 2026-09-15)

The operator asked whether "Deep recursion in Scala 3" (Kozak) — a
macro `deepRecursive` that rewrites a self-recursive body into
`TailRec`'s `tailcall`/`flatMap`/`done` — can be had here, and then
that it be `direct` itself rather than a second entry. It is, and her
`TailRec` is this library's `Free`: `tailcall` is `Delay`, `flatMap`
is `Bind`, `done` is `Pure`, `.result` is `!.run`.

```scala
import okay.Direct.{*, given}
import scala.language.implicitConversions

def fib(n: Int): Long ! Pure = direct:
  if n < 2 then n.toLong else fib(n - 1) + fib(n - 2)

def sum(n: Int): Long ! Pure = direct:          // 1 + sum(n - 1): not tail
  if n == 0 then 0L else 1L + sum(n - 1)
```

The rules: inside a `direct` block a call to the ENCLOSING def, at the
block's own program type, is deferred wherever it is marked or
auto-coloured; and a call to ANOTHER def at that type is deferred in
the block's TAIL position (direct-tail-defer, 2026-09-16), which is
what makes mutual recursion need no word — `fib(n - 1)` under `.reflect`, or under `selfColor`
when `Direct.given` is imported, becomes `Free.delay(() => fib(n -
1))` under the same mark. The DEFERRAL is the whole trick: a
self-call evaluated at construction is the native recursion the
block was written to avoid (on master before this lane, the coloured
`sum(1_000_000)` overflowed a 512 KB stack at construction; with the
rule it answers). The lowering of `a + b` with two calls, `if`,
`match` and blocks is `direct`'s own. Never under a lambda (v1 does
not look there): a self-call used as a value is left alone. Mutual
recursion needs no word at all in tail position, and
`!.tailcall(other(n))` where a mutual call stands anywhere else —
which the article's macro cannot do either way.

Behavior (TestDirectDeep):
- [x] fib(25) = 75025 with two bare self-calls in one expression;
      `1 + sum(n - 1)` at 1 000 000 on the suite's default stack; a
      self-call inside `match` with two parameters; a real row whose
      tells interleave with the recursion, in order.
- [x] the marked spelling defers the same way; mutual recursion
      through `!.tailcall`; a self-call under a lambda untouched.

Decisions on the tail rule (direct-tail-defer, 2026-09-16):
- **Why the position, and not the callee.** The enclosing def's symbol
  is knowable at expansion; a mutual cycle is not — it spans files and
  the other def may not be typed yet. The tail position is the one
  piece of evidence the macro has, and it is the position where a node
  costs one allocation and saves a frame. Before the rule, `else
  isOdd(n - 1)` compiled, answered at small `n` and overflowed the
  stack at depth; the failing test was written first and watched.
- **Only a call, never a value.** A tail-position program VALUE (`else
  p`) is left alone: naming a program builds nothing.
- **Never twice.** A term that already defers — `!.tailcall`,
  `Free.delay`, `Free.defer` — is skipped. Getting this wrong was not
  theoretical: the first cut read no callee name through the
  `Inlined`-with-bindings node `!.tailcall` expands into and wrapped
  the node twice, which the `-Xprint:inlining` dump caught.
- **The cost, measured.** `compare/DirectBenchmark`, two alternating
  rounds: allocation IDENTICAL TO THE BYTE on `okayDirect`,
  `okayDirectRec` and the `okayFlatMap` control, so the rule added no
  node to any of them; time inside the lane's own noise (master's two
  rounds swung 127.0 → 106.5 µs while the control moved 4% the other
  way).
- **"Defer every program-typed call" is the DEFAULT, with an import to
  opt out** (direct-defer-default, 2026-09-16, the operator's call
  after seeing the price; rows `da-*` measured it, `dd-*` are the
  shipped pair). It is the only rule that also covers a mutual call
  OUTSIDE tail position — a cycle the macro cannot see may be closed
  from any position — and the operator chose safety by default:
  a recursive block that compiles, answers small and overflows deep is
  the failure a library must not hand its users, and the price is
  visible and recoverable where it matters. One run, `okayFlatMap` as
  the control (101.2 µs):

  | lane | time | allocation |
  |---|---|---|
  | `okayDirect` (default) | 179.3 µs | 2 238 113 B |
  | `okayDirectEager` (`import Direct.eagerCalls.given`) | 113.6 µs | 1 598 113 B |

  +640 000 B is exactly 64 bytes — a `Delay` plus its thunk — for each
  of the 10 000 marked calls that lane makes, and the opt-out gives
  back the allocation to the digit. Both lanes are published so the
  trade is in the numbers rather than in a sentence.
- **The knob is a `using` parameter, not a summoned marker.** An
  import whose only reader is a macro is an "unused import" to the
  compiler, and that warning would land in every user's build; passed
  as `using d: Deferral` the typer uses it, so the import counts. Two
  ways to spell the default were wrong and both were caught by
  compiling: a default ARGUMENT (`d: Deferral = All`) makes
  `apply$default$N` take the inline block again, duplicating the whole
  body — with a nested `direct` block inside it, `TreePickler` crashes
  with `assertion failed: method $anonfun`; and declaring the givens at
  type `Deferral` rather than at their singleton types hands the macro
  a type that says nothing, so the import resolved and changed nothing
  (the expansion dump showed both modes deferring).
- **Two shapes are never deferred, in either mode.** A call already
  wrapped in `!.tailcall`/`Free.delay`/`Free.defer` (else it pays for
  two nodes), and a call that CARRIES DEFINITIONS — a lambda, a local
  val or def, a nested block's context function — because moving such
  a tree under a thunk moves symbols that are owned where they stand.
  The thunk is also built under the owner at the rewrite site rather
  than the splice owner, which is invisible while the rule only fires
  at the top of a block and fatal once it fires inside one.

Decisions:
- **`direct`, not a second entry.** A first cut added
  `Direct.deepRecursive` for a def with a PLAIN result type (the
  article's signature: `def fib(n: Int): Long`), generating a
  `loop$deep: Long ! Pure` and `!.run`ning it — 58 tests green — and
  the operator asked for the recursion to work with `direct` at the
  program type instead. It does, once the block's own values colour:
  `selfColor` already applies to `A ! Row` (it looked as if it did not
  — three probes refused `val x: Long = f(n - 1)` — until the
  imports the auto-colouring tests carry were noticed: `Direct.given`
  and `scala.language.implicitConversions`; a wildcard import does
  not bring givens). A `rowColor` given written for the supposed gap
  was removed the same hour. What `deepRecursive` offered on top was
  a value-typed signature, and that is the article's constraint
  (a macro runs after the typer), not a need of this library.
- **The deferral is a mark, not a new node.** `Direct.reflect(Free
  .delay(...))` and `selfColor.apply(Free.delay(...))` are shapes the
  pipeline already lowers; nothing learned a new case. The self-call
  detector runs once over the block before `compile`, keyed on the
  enclosing def's symbol (`Symbol.spliceOwner` walked up to the first
  `isDefDef`).
- **`.?` was not a mark, and is one again** (unwrap-glyph,
  2026-09-17). It was retired because two other things answered `?` on
  a program: `Throws.?`, which through the `into` conversion answered
  it on ANY value and did nothing at all, and the row peek `!.?`. Both
  are gone — the Throws glyphs moved into their type's companion,
  where a converted receiver cannot reach them, and the peek took the
  word `peek`, which is what a method that RUNS operations through a
  Handler should have been called. The Interface block above showed
  `.?` throughout the retirement, and that contradiction is what made
  the incident in specs/unwrap-glyph.md possible. All four spellings
  now work: `.reflect`, `.!?`, `.?`, prefix `!p`.

## Out of scope (v2 roads, recorded not promised)

- **Auto-coloring** (no marks at all): the Conversion trick —
  a `given Conversion[F[A], A]` lets the block typecheck, the macro
  rewrites the conversion calls. The scoping answer is a CAPABILITY:
  the block becomes `DirectCtx[F] ?=> A` and the conversion requires
  `using DirectCtx[F]`, so it can fire ONLY inside a direct block
  (dotty-cps-async's CpsMonadContext pattern; compile-time refusal
  outside, better than the phantom's runtime throw). Which types
  auto-color is itself gated by a marker typeclass (user sketch
  2026-09-01): e.g. only `G` with a `given Effect[G]` instance
  converts, so arbitrary F[A]s never silently color — the row
  membership check the macro already does for `?` becomes the
  conversion's own evidence. Cost that stays: the
  implicitConversions language import and degraded error messages
  inside blocks; explicit marks remain the default.
- **`try`** — SHIPPED (direct-try, 2026-09-02): a try body with
  marks is its own sub-block, reified at the try's joined type
  through the recursive pipeline, and the whole try becomes ONE mark
  over `CanTry[F].tryIn` (Throws.scala is the seam, as recorded):
  strict monads catch at construction — full coverage, since their
  computation IS the construction; Free rows guard construction and
  every continuation step, so a pure segment throwing between
  effects lands in the catch while a throw inside an effect's
  HANDLER stays that handler's business (stated). Finalizers and
  marks inside catch bodies remain refused, named. A body ending in
  throw types Nothing: upcast through the monad, not variance.
  `while` and the foreach/map loops SHIPPED via specs/direct-loops.md.
  CanTry's instances are NAMED (audit-fixes, 2026-09-02): the first
  cut had a catch-all strict given, under which a LAZY monad (a Cont
  diagonal, Eff) tried the construction and never the run — the
  catch silently never fired. Now Option/Either/List/Vector/Try and
  Free rows have instances, a strict monad of your own declares
  `given CanTry[M] = CanTry.strict`, and a lazy one is a compile
  error that says why. Context functions (`E ?=> X`) have an instance
  too (direct-try-ctx, 2026-09-03): the FIRST attempt reused the
  strict shape and crashed dotty 3.7.4 at erasure ("bad adapt for
  M$proxy2.pure(a)") — but strict was also the WRONG semantics for a
  context function, which is lazy in its environment (a closure, not
  run until applied): a try wrapped around merely CONSTRUCTING the
  closure would never see a throw from inside its body. `ctxFn`
  (Throws.scala) defers the try to APPLICATION time instead — the
  honest counterpart to the Free row's per-step guard — and that
  different generated-code shape sidesteps the crash too; no Scala
  version bump needed. Stated limitation, over a condition
  frame: a direct-try whose body reflects a `within`/`frame` does
  not catch a throw from a PURE segment inside that frame's body —
  Condition.run's Within case runs the frame body through its own
  `loop`, outside the guarded continuation steps of the enclosing
  try; the throw reaches the run as an exception. Catch inside the
  frame (a direct-try in its body), or use a restart for that path.
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

## Structure (direct-compiler-phases, 2026-09-20)

The pipeline was one method — `compileAll`, 1460 lines of nested defs
sharing a closure over the Quotes, the monad and the two mode flags —
which meant no phase had a name a test could call. It is now a class,
`DirectCompiler[F]`, mixed from one trait per phase, each in its own
file under `src/main/scala/macros/` (`package okay.macros`; the facade
`okay.Direct` stays where it was) with its dependencies stated by what
it extends:

| phase | file | decides | reaches the knot |
|---|---|---|---|
| marks | DirectMarks | what the reader wrote: mark spellings, colouring conversions, symbol uses | no |
| row | DirectRow | what F is: the row, runnable values, silent drops, the lifts | no |
| emit | DirectEmit | the monad's words: `pure`, `flatMap`, `fmap`, the upcast | no |
| defer | DirectDefer | the pre-pass: `Free.delay` around calls that would recurse on the stack | no |
| vals | DirectVals | `val` runs here, `lazy val` is the Once cell, `def` is by name | nested def |
| loops | DirectLoops | for-do, for-yield, the fused statement tail | body |
| parallel | DirectParallel | which leading vals spawn together; spawn-all-then-join-all | leaf fallback |
| core | DirectCompiler | `compile`, `compileMarked`, ANF over spines, the block fold | is the knot |

The knot — `compile`/`compileBlock` — is abstract in `DirectPhase`, the
base every phase extends, so a phase that calls back into the compiler
does so through those two names and a reader sees it in the column
above. Emission is unchanged by construction: the split moved code and
renamed nothing that emits; every TestDirect* suite is the check.

The seam is the `Expr`: `q.reflect.Term` belongs to one Quotes path, so
what enters the class (the block, a try body's sub-pipeline, a probe)
enters as an `Expr[Any]` and is `asTerm`ed under the instance's own `q`.
Inside, the given is declared at `q.type` — not `Quotes` — so a
dependent method a phase calls (`Direct.stripped`,
`DirectCompiler.pipeline`) binds to this `q` and its Term is ours.

A phase is asserted ON ITS OWN through `src/test/scala/DirectProbe.scala`,
test-side macros that run one phase over a block and answer with plain
data, before any bind is emitted and without running anything:

- [x] `DirectProbe.deferred` — the block after the defer pre-pass,
  shown: a marked call at the program type carries `Free.delay` by
  default and not under `eagerCalls`; a tail call carries it under
  both; a call already under `!.tailcall` is wrapped once, not twice;
  and a block with no enclosing def (one in a class body) is left as
  written — the pass finds the def a self-call would name by walking
  up from the splice owner, and stops when there is none
- [x] `DirectProbe.marks` — how many marks the mark analysis finds,
  counting an auto-colouring conversion as the mark it is
- [x] `DirectProbe.runnable` — the element type a value could RUN at
  as a bare statement of a block over F: `Some(Unit)` for an operation
  of the row and for a program of it, `None` for a foreign monad
- [x] `DirectProbe.dropped` — whether a statement of that type would
  be a silent drop (the error's own predicate); and that the predicate
  asks for `Direct.Effect`, the auto-colouring marker — the one name
  that resolved differently outside `object Direct` (`okay.Effect` is
  the narrower type, and the compiler said nothing); red with the bare
  name, green qualified
- [x] `DirectProbe.independentRun` — the names of the leading vals the
  parallel analysis would spawn together; a dependent leaf ends the run
- [x] `DirectProbe.slots` — the value slots an application spine
  hoists: receiver and arguments, the elements of a varargs

## Decisions

- **The compiler is a class of phase traits, not a closure of nested
  defs** (direct-compiler-phases, 2026-09-20). `compileAll` had grown
  to 1460 lines of ~70 nested defs, all correct and all unnameable: a
  phase could be tested only by running the whole macro and reading
  the program's behaviour. The closure became `DirectCompiler[F]` and
  the defs became members of one trait per phase (the Structure
  section above), so a test-side macro can instantiate the class and
  call one phase. `using` parameters on the class were refused: they
  would have put two `Quotes` givens in the class body (the parameter
  and the phases' inherited alias), so the class takes plain `val`s
  and the base trait owns the one given, at `q.type`.
- **The phases live in `okay.macros`, and not in `okay.direct`**
  (direct-phases-package, 2026-09-20). The facade stays `okay.Direct`;
  the nine phase files are machinery every file in `package okay`
  would otherwise see, so they moved to a subpackage. The natural
  name was measured and refused: a package is a term name too, and a
  probe compile of the three import shapes the repository uses showed
  `okay.direct` breaking exactly one of them — `import okay.*` beside
  `import okay.Direct.*`, which seven files here write and any user
  would — with E049 "Reference to direct is ambiguous: imported by
  okay._ and by okay.Direct._". (`package okay` with `import
  Direct.*`, and `import okay.Direct.*` alone, both compiled: the
  first draft of this entry said "shadowing", and that was the wrong
  mechanism.) `macros` names what the package holds, compile-time
  machinery, and collides with no term of the API.

- **A lambda whose body ends at the block's program type is compiled,
  not refused** (direct-program-lambda, 2026-09-16). The general
  lambda refusal stands; this is the same narrow exception `try` and a
  nested `def` already have, and it is sound for the same reason — the
  body ALREADY answers at the program type, so binding the marks
  inside it changes neither the lambda's type nor where it is
  evaluated. The body compiles through the ordinary `compile` (it
  reads the block's own locals) and is flattened by one `flatMap`,
  since an expression answering a program compiles to `F[F[T]]`. Only
  the block's OWN row: another row would need its `Monad` summoned and
  its type carried into the pipeline, and the shape that wants this —
  a `Delim` continuation handler — answers at the row it was written
  in. What it buys: `Delim.shift(p) { k => "x".tell; k(n) }` with no
  inner `direct` block (`TestDelim`).

- **A call whose ARGUMENTS carry marks is not deferred**
  (direct-marked-args, 2026-09-16). `!f(!f(5))` was refused as "a mark
  under a lambda", and the lambda was the macro's own: the
  defer-every-call rule wraps a call in `Free.delay(() => …)` before
  anything is compiled, so a mark in an argument landed under that
  thunk and the general lambda refusal fired, naming a lambda the user
  never wrote. The arguments bind first and the call is built inside
  the continuation, where there is nothing left to defer; deep
  recursion through such a call is `!.tailcall`'s job, as it is under
  `eagerCalls`. Found writing `Delim.shift(p)(k => direct { !k(!k(5)) })`,
  the natural spelling of a continuation invoked twice — with it,
  `shift` and `reset` are written inside a `direct` block, handler and
  all (`TestDelim`).

- **A nested parameterless `def` at the block's program type is
  compiled, not refused** (direct-nested-def, 2026-09-16). The general
  refusal stands — a mark inside a nested definition would need the
  definition's signature rewritten — but this shape needs no rewrite:
  the def already ENDS at the program type, so binding the marks
  inside its body leaves its meaning alone, and `def` keeps being by
  name (a bind per use). The body compiles in the same pass, since it
  reads the block's own locals, and is flattened the way
  `colourlessVal` flattens a val's: a body ending in a program is bound
  and its answer marked. A fresh symbol carries it, because inference
  gives `def plan = effect(...)` the precise `Free.Inject[R, A]` while
  the compiled body is a `Free[R, A]`; the uses are rewritten onto it,
  a coloured use as a mark and a bare use as the program. Defs with
  parameters keep the refusal. This is what makes the three words
  comparable on one function: `val` 3 calls per request, `def` 0/1/3/8,
  `lazy val` 0/1/2/3 (docs/direct-style.md).

- **`lazy val` with a mark is the `Once` effect, not a cell in the
  tree** (direct-once, 2026-09-16; the operator's design). The first
  cut was `Free.once(p)`: a `Delay` whose thunk checks a mutable
  cell. Refused before it compiled, for three reasons the effect
  answers at once: a mutable field makes the same program answer
  differently on its second run (a replaying handler, a persisted
  journal, replays a stale value); "once" under a multi-shot handler
  needs a policy the macro cannot check, so an import flag would
  have been a promise nobody verifies; and a concurrent or abandoned
  first run is undetectable from inside the thunk. As an effect the
  cells are `Once.run`'s threaded state, the tree stays data, and
  the multi-shot policy is handler order — `runChoice(Once.run(p))`
  backtracks the cells, `Once.run(runChoice(p))` shares one store —
  which the compiler checks. The handle carries no program so that
  `Once`'s type does not name its row; the program stays with
  `!.once`, which is `Once.at` (the row-generic form the macro emits,
  the two operations injected by the caller) at `Once + F`. A lazy
  val with a mark and no `Once` in the row is refused with the effect
  named; a self-referring one is refused (a knot at run time, a
  dangling symbol after the rewrite); a demand while the program is
  running throws. `Logic.once` (the cut) first kept its name, and a
  file importing both `!.*` and `Logic.*` found the two ambiguous
  within the hour; it is `Logic.cut` (logic-cut, the same day).
- **Tail fusion for loop bodies** (direct-tail-fusion, 2026-09-02;
  the road direct-flatmap-emission recorded): a loop BODY compiles
  against an explicit tail term — `compileTail(t, tail)` returns an
  F-term that runs t's effects and continues with `tail` — so the
  sequencing bind (`bodyF.flatMap(_ => loop())`) merges into the
  body's own last bind: `while i < N do { x = step(x).reflect;
  i += 1 }` emits `step(x).flatMap(v => { x = v; i += 1; loop() })`
  — one bind per iteration, the hand-written recursion shape, where
  the unfused emission paid two (measured at 2.0x, §1b). Fused
  shapes: statement blocks (vals, assigns, pure statements, bare
  runnable ops), each threading the same tail inward; everything
  else (if/match, nested loops, try) falls back to
  `flatMap(asF(compiled))(_ => tail)` — the pre-fusion behavior —
  because duplicating a tail into branches duplicates code, and the
  fallback is correct by construction. The tail is always a nullary
  call (`loop()`/`loop(tl)`), so the fused closure captures nothing
  extra. Semantics unchanged by monad law (associativity + pure-bind)
  — the gate is every TestDirect* suite green unchanged.

- **`?`, not `!`, for the operation mark** — REFUTED by the
  compiler (2026-09-01): an extension named `!` imported via
  `import Direct.*` SHADOWS `object !` as an identifier, and every
  `!.run(...)` in the importing file stops compiling ("value run is
  not a member of Any => Any" — the bare `!` eta-expands to the
  extension). The two-character mark is the price of keeping the
  program-runner namespace intact.

- **A phantom mark, not Monadic's operators** — the macro's block is
  typed BEFORE expansion, so inside it `m.!?` must have type A; the
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

- **Plain flatMaps, not Cont, as the emission target**
  (direct-flatmap-emission, 2026-09-02; filed by bench-direct): the
  macro now compiles every node to an `F[T]` term — binds are
  `fa.flatMap(v => rest)` on `Monad[F]` directly, the pure tail is
  `M.pure(_)`, loops are the same recursive defs with `F[Unit]`
  bodies, and `Monadic.reify` disappears from the emitted tree. The
  original target (`Cont[·, F[A], F[A]]` closed by reify) was priced
  by the bench-direct lane at 3.3x over hand-written flatMaps
  (docs/benchmarks.md §1b): every mark paid a shift closure and a
  Cont trampoline step on top of the flatMap it wrapped. The Cont
  layer bought NOTHING semantic in the macro's hands — `reflect(m)`
  is `shift(k => m.flatMap(k))`, so multi-shot and short-circuit
  were always F's own flatMap calling the continuation, and the
  stack discipline was always inherited (a strict monad's flatMap
  invokes k inline in BOTH encodings — a while over Option was
  never trampolined; a lazy carrier defers k in both). The macro
  builds the continuation syntactically, which is the one job
  runtime reflection existed to do dynamically. `Monadic.reflect`/
  `reify` remain public API for hand-written Cont style; only the
  macro's target changed. Two law-based fusions ride along: a pure
  while condition emits a plain `if` (no bind), and a statement-
  position `Assign` with a marked rhs binds once into the assignment
  (not bind-into-pure-then-bind-again). Gate: every TestDirect*
  suite green unchanged — the suites pin the semantics the rewrite
  must preserve.

## Results

- direct-tail-fusion, measured (2026-09-02, quiet box; docs/
  benchmarks.md §1b): 10k binds while+var — 189µs -> **101µs**
  (2.0x -> **1.06x, matched within noise** against the 95µs
  hand-written chain). Recursion untouched (55µs, 0.58x) — it
  doesn't go through While/foreach, so tail fusion has nothing to
  merge there. Every TestDirect* suite green unchanged (65/65),
  full sbt test green. The generic statement-tail compiler
  (`compileTail`/`stmtsTail`) folds vals, marked assigns, pure
  statements and bare runnable ops into the supplied tail; if/match/
  nested-loop/try and any bare mark fall back to one sequencing bind
  — duplicating a tail into branches would duplicate code, and the
  fallback is the pre-fusion emission, correct by construction.
- direct-flatmap-emission, measured (2026-09-02, quiet box, spike
  watcher clean; docs/benchmarks.md §1b): 10k binds — while+var
  313µs -> **189µs** (3.3x -> 2.0x over the 95µs hand chain, and the
  remaining 2.0x is exactly the loop's sequencing bind per iteration,
  two binds against the chain's one); recursion 410µs -> **56µs**
  (4.3x -> **0.59x — faster than the hand-written foldLeft chain**,
  because the macro emits right-nested binds where foldLeft builds
  left-nested ones the Free interpreter must reassociate — the same
  mechanism the table credits zio-direct for, and level with kyo's
  hand-written 56µs). Every TestDirect* suite green unchanged, full
  sbt test green. Paid for once, in the rewrite: a nested quote
  inside a splice referencing a type param bound outside the
  enclosing quote does not pickle (the loops keep flatMap in the
  outer quote and precompute the body's element type from types
  alone); anonymous `(_: Type[T])` poly-lambda params do not pickle
  once quotes deepen — named; the hoisted Monad val is built by hand
  (Symbol.newVal) so the term stays in one Quotes context. Recorded
  road, not promised: a continuation-passing sequencing pass could
  merge the while loop's two binds per iteration into one (~1x).
- 16 tests in TestDirect, all green: vals, subexpression hoisting
  (order asserted), if/match, multi-shot (List, 6 continuation runs
  counted), effects over `!` (single row and a two-effect
  Reader+Writer row, handlers peeling as always), expected-type
  inference (`val p: Int ! F = direct { ... }` — no type argument),
  compile errors for lambda/while/try/by-name, the phantom's runtime
  throw. The macro is ~300 lines.
- Findings, each paid for once:
  - `isInstanceOf[ByNameType]` on quotes-reflect types is ALWAYS true
    (abstract types erase to TypeRepr) — by-name detection must
    pattern-match through the API's TypeTest.
  - Boolean `&&`/`||` are intrinsics: method type by-value, the
    short-circuit is compiler magic — the macro desugars marked
    `a && b` to `if a then b else false` (dually `||`) and recurses,
    keeping the short-circuit; hoisting their operands would have
    broken it silently.
  - `Lambda(...)` IS `Block(DefDef :: Nil, Closure)` — the Lambda
    case must precede the Block case or the error message degrades.
  - A narrow-row operation in a wide-row block (`Writer.tell` where
    the block's F is a union row) is refused with the fix named:
    spell it `effect[Row, A](op).!?` — the same spelling the monadic
    style needs, so parity with `!` holds exactly.
  - Application spines: only VALUE slots (receiver, arguments) are
    hoisted; callee structure (Selects, TypeApplies, curried lists)
    is rebuilt — a partially applied method is not a value.

## Once across fibres (once-across-fibres, 2026-09-23)

`Once.run` threads its cells, so a fibre forked inside a program
takes a snapshot and two fibres demanding one handle run it twice,
each in its own store. `SharedOnce` (okay-async/SharedOnce.scala) is
the shared-cell reading: one store, a demand that WAITS while the
program is in flight.

```scala
final class SharedOnce:
  def run[A](a: A ! (Once + Async)): A ! Async
  def runIn[A, F[+_]](a: A ! (Once + (Async + F))): A ! (Async + F)
```

### Behavior — once across fibres

- [x] one `!.once` value demanded from two fibres under one
      `SharedOnce` runs its program ONCE; the second fibre waits (an
      `Async` await resumed by the store) and gets the same answer
      (TestSharedOnce, JVM: `par` needs a Scheduler)
- [x] for contrast, pinned: the same value under `Once.run` per fibre
      runs twice
- [x] a demand after the store is filled answers from the store; the
      first store wins
- [x] `runIn` forwards the rest of the row (a `Writer` tell inside the
      program happens once, with its one run)

### Decisions — once across fibres

- **A `translate` handler, not a second loop.** Each `Once` operation
  answers a program in `Async + F`; the waiting demand is an ordinary
  `await`, so it suspends the fibre as any await does and the machine
  needs nothing new. `Once.run` keeps its threaded loop untouched.
- **One cast, the same as `Once.stored`'s**, isolated in `cell(h)`: a
  heterogeneous map keyed by handle identity; the cell's `A` is the
  handle's.
- **A knot is a HANG, documented, not detected.** The fibre that would
  store is the one waiting; `Async` has no fibre identity to tell a
  self-demand from a sibling's, and a thread heuristic would misfire
  when a continuation resumes on another thread. `Once.run` is where
  a knot is possible; the shared store is for handles that cross
  fibres, and its doc says so.
- **Not replayable, said out loud.** The cells live outside the tree,
  which is exactly what the threaded reading refused; a program under
  `SharedOnce` is run, not replayed.

