# 8. Direct style: reflection, elaboration, and the two gates

## Three presentations of one monad

A monadic computation has been written three ways since the
beginning. Moggi's metalanguage sequences it explicitly with `let`
\[[Moggi 1991](#ref-moggi-1991)\]; Wadler's do-notation sugars the
`let` into a comprehension \[[Wadler 1992](#ref-wadler-1992), [1995](#ref-wadler-1995)\]
— Scala's `for` is exactly this; and **direct style** removes the
sequencing syntax altogether, writing the computation as if its
values were ordinary values. The third presentation is not a
convenience heaped on the second: it has its own theorem. Andrzej
Filinski proved \[[Filinski 1994](#ref-filinski-1994)\] that in a
language with delimited control, every monad can be **reflected** —
`μ : F[A] → A` delivers a monadic value as a direct value, and its
inverse **reification** `[·] : A → F[A]` delimits a direct block
back into the monad — such that the direct program and the monadic
program it abbreviates are equal. Chapter 2 called this theorem the
justification of the whole tower ("shift and reset can express any
monadic effect"); this chapter is the same theorem read in the other
direction: *because* Okay has shift/reset at the bottom, direct
style is not a feature to build but a corollary to state.

The construction is two lines, and answer-type modification (chapter
3) is what types it precisely:

```scala
// Cont.scala (Monadic), 401–409
inline def reflect[B]: Cont[A, F[B], F[B]] = shift(k => m.flatMap(k))
inline def reify[F[_], A, B](p: Cont[A, F[A], F[B]])(using M: Monad[F]): F[B] =
  p / (a => M.pure(a))
```

`Cont[A, F[B], F[B]]` reads "*A now, F[B] eventually*": the reflected
value's answer type carries the debt the block owes its monad, and
`reify` settles it with `pure`. Danvy and Filinski's typing
discipline \[[Danvy & Filinski 1990](#ref-danvy-1990)\], made
polymorphic by Asai and Kameyama \[[Asai & Kameyama 2007](#ref-asai-2007)\],
is precisely what lets a *library* state this type; untyped accounts
of reflection have to trust the programmer with it. `shift` hands
the entire rest of the block to the monad's own `flatMap`, and the
monad decides what a continuation is worth: `None` drops it,
`Either` short-circuits it, `List` runs it once per element. That
last point deserves emphasis, because a competing implementation
strategy genuinely loses it — see §Multi-shot below.

Two facts about the construction, both established by test before
they were written into the specs
([specs/monadic-reflection.md](../../specs/monadic-reflection.md)):

**The stack is the reflected monad's, not Cont's.** A strict
`flatMap` invokes the continuation in place, so each `reflect`
costs a stack frame; `Option` chains of a thousand binds are
comfortable and a hundred thousand are not. A trampolined monad —
Okay's own `A ! F`, in Bjarnason's data-not-closures sense
\[[Bjarnason 2012](#ref-bjarnason-2012)\] — returns a node instead of
calling, and 100 000 reflected binds run flat. The practical rule
falls out of the runners, not out of doctrine.

**One monad per block.** The answer type fixes one `F` per `reify`;
mixing two monads in one block does not typecheck, and that is the
honest restatement of the fact that monads do not compose \[[Moggi
1991](#ref-moggi-1991)\]. Blocks *nest* — Filinski's own treatment of
layering \[[Filinski 1999](#ref-filinski-1999)\] — and composition
proper is what the effect rows of chapter 5 are for: one block over
`A ! F + G` reflects a row, not a monad stack. For monads the library
does not own, `Layered` gives each its own delimiter — reify as
`η $ e`, reflect as `shift0` \[[Materzok & Biernacki
2012](#ref-materzok-2012)\] — so the nesting of the blocks is the order
of the layers. The problem, transformers on a real cats stack, and both
answers are told step by step in the book's
[chapter 16b](../continuations/16b-two-monads-at-once.md).

## Elaboration: the macro as a normalization proof

Reflection gives direct style *relative to Cont* — the
for-comprehension over `Cont` remains. Removing it is an
**elaboration** problem: rewrite a plain block into the reflect/reify
chain it means. The target normal form is administrative normal form
\[[Flanagan, Sabry, Duba & Felleisen 1993](#ref-flanagan-1993)\]:
every effectful subterm hoisted to a `let` (here: a `flatMap`
binder), evaluation order made syntactic. The `direct` macro
(Direct.scala) is exactly an ANF pass over typed Scala trees —
statement folding, value-slot hoisting in application spines
(`Direct.scala:293`), branch-wise treatment of `if`/`match` — and
its correctness argument is inherited rather than invented: each
rewrite step is an instance of the monad laws plus the
Kameyama–Hasegawa axioms for shift/reset \[[Kameyama & Hasegawa
2003](#ref-kameyama-2003)\], so the emitted program is one the user
could have written by hand with `reflect`/`reify`, and the specs'
phrase "the macro adds SYNTAX only" is a theorem-shaped claim, not a
slogan.

The design's one real decision is **scope**. The fully general
transform for Scala 3 exists — dotty-cps-async \[[Shevchenko
2022](#ref-shevchenko-2022)\] — and its cost is concentrated in one
corner: rewriting *under lambdas* (automatic coloring of
higher-order arguments), which forces re-typing machinery through
every library signature it meets. Okay's macro refuses that corner
with a positioned error (`Direct.scala:234`) and stays ~300 lines.
The precedent for a scoped transform is exactly Scala's own
scala-async \[[Haller & Zaugg, SIP-22](#ref-sip22)\], which lived for
years with the same restriction; F#'s computation expressions
\[[Syme, Petricek & Lomov 2011](#ref-syme-2011)\] made the same
trade at the language level — elaborate the block form, do not chase
the lambda. Two typed-tree lessons are recorded in the spec's
graveyard because each was paid for by a failing compile:
quotes-reflect types erase, so `isInstanceOf` tests on them are
vacuous and only `TypeTest` patterns mean anything; and Boolean
`&&`/`||` are intrinsics whose method types are by-value while their
semantics is not — the only correct rewrite is the `if` they mean.

**One mark, dispatched by type.** The surface has a single mark
(`.?`); the macro's `markTerm` (`Direct.scala:178`) decides by type
whether the marked value is the block's `F[T]` (reflect) or an
operation of its row (inject into the row program, then reflect —
the row is extracted from `F = A ! Row`). An earlier two-mark design
was refuted as redundant: the type already carries the distinction,
so the syntax should not. This is the effect-handler reading of
chapter 5 meeting reflection: an *operation* is not a monadic value,
but `Free.Inject` is a monad morphism away, and elaboration is where
the coercion belongs.

One surface note earns its citation: Okay's prefix mark `!prog`
(the glyph of the program type `A ! F` performing it) is the point
Idris reached with **bang-notation** — `!expr` inside a do-block
lifts an effectful subexpression to a bind \[[Brady 2013](#ref-brady-2013)\]
— and Frank reached with `!` for command invocation \[[Lindley,
McBride & McLaughlin 2017](#ref-frank-2017)\]; the elaboration
mechanism differs (their compilers, our macro), the reading is the
same: perform, here.

## The two gates: coloring as a capability

Removing the marks entirely turns direct style into an implicit
conversion problem, and the theory that keeps it sound is
**capability passing** \[[Brachthäuser, Schuster & Ostermann
2020](#ref-effekt-2020)\]: a permission is a value with a scope, and
what may happen *here* is decided by what capabilities exist here.

```scala
// Direct.scala:73–88
final class DirectCtx[F[_]] private[Direct] ()
trait Effect[G[_]]
given selfColor[F[_], A](using DirectCtx[F]): Conversion[F[A], A]
given opColor[F[_], G[_], A](using DirectCtx[F], Effect[G]): Conversion[G[A], A]
// Free.scala:61 — the block's own programs, found through the source type's companion
given directColor[R[+_], A](using Direct.DirectCtx[[X] =>> Free[R, X]]): Conversion[Free[R, A], A]
```

One consequence of Scala's rules deserves a sentence, because it
decides what a file has to import. Applying any `Conversion` is a
*feature* the language wants consent for — `import
scala.language.implicitConversions`, per file — and that consent is
not removable by `into` (which lifts it in parameter positions only,
while a block colours at ascriptions and receivers) and was, for one
day, replaced by a build-wide flag that the repository took out again:
`TestThrows` proves `throws`'s `into` by the *absence* of that import,
and a global flag makes the absence prove nothing. So the language
import stays per colouring file; what a file no longer needs is
`Direct.given`, because `directColor` lives in `Free`'s companion, the
implicit scope of the conversion's source type. And for the reader
who wants no implicit conversion at all, the marks are the answer:
prefix `!` on a program is one glyph and involves no `Conversion`.

The block is a context function `DirectCtx[F] ?=> A`, so the
capability exists only inside it — outside, the conversions cannot
resolve and `F[A]`-as-`A` remains a type error; the refusal is
compile-time, one grade *stronger* than the marks' runtime phantom.
(dotty-cps-async's `CpsMonadContext` is the same move; Effekt makes
it the whole language design.) The second gate is a marker
typeclass: operation signatures color only where the user has
written `given Effect[G]` — registration is an explicit per-project
act, so no foreign type ever colors silently. The conversions
themselves never run; elaboration finds their calls by symbol and
routes them through the same `markTerm` dispatch. What remains is
typer physics, documented rather than fought: conversions fire only
against an expected type, so un-ascribed `val`s keep the monadic
value (a feature — that is how a program is *held*), and Unit
ascription is value discard, which preempts conversion search
entirely. The last fact is why the fourth layer exists.

## Recursion in a block: the tree is the trampoline

A direct block that calls its own def — `fib(n - 1) + fib(n - 2)` at
the program type `Long ! Pure` — has a problem the marks alone do not
solve: the self-call is a *program*, and evaluating it at construction
is the native recursion the block was written to avoid. Kozak's
`deepRecursive` macro for Scala 3 rewrites such a body into `TailRec`'s
`tailcall`/`flatMap`/`done`; here the rewrite is one rule in front of
the lowering, and the target is chapter 11's tree. Inside a block, a
call to the enclosing def at the block's program type is deferred
wherever it is marked or coloured — `fib(n - 1)` becomes `Free.delay(()
=> fib(n - 1))` under the same mark (`Direct.scala:864`) — and the
recursion then trampolines through `resume`'s `Delay` case instead of
the JVM stack. A second rule covers the cycle the first cannot see: a
call to ANOTHER def at the block's program type is deferred when it
stands in the block's TAIL position, which is what makes mutual
recursion need no word (direct-tail-defer). The asymmetry is not
aesthetic — a macro expanding one def cannot know that the def it
calls calls back, since the cycle spans compilation units, so the
syntactic position is the only evidence available at expansion time.
Covering the remaining case means deferring every program-typed call
wherever it stands, and that is what the default now does — at a
measured 64 bytes per marked call, which a block that is hot and
provably not recursive buys back with one import
(`Direct.eagerCalls.given`, direct-defer-default). The knob is a
`using` parameter of the block rather than a summoned marker, because
an import whose only reader is a macro is an unused import to the
compiler and the warning would land in every user's build. `TailRec`'s `tailcall` is `Delay`, its `flatMap` is
`Bind`, its `done` is `Pure`, its `.result` is `!.run`; what the macro
of the article refuses — a self-call inside `match`, a real row
interleaving effects with the recursion, mutual recursion — this
lowering already handles, mutual recursion by one explicit
`!.tailcall(other(n))` — needed only where a mutual call is NOT in
tail position, since a mark is not a deferral. `TestDirectDeep` runs `1 + sum(n - 1)`
a million deep on the suite's default stack. The zero-annotation form
is possible only at the program type with colouring on, because a
macro runs after the typer; with the prefix mark it is
`!fib(n - 1) + !fib(n - 2)` and no conversion is involved.

## Statements run: the do-notation reading

In Haskell's do-notation, a bare statement *is* a bind: `op; rest`
means `op >>= \_ -> rest` \[[Wadler 1995](#ref-wadler-1995); made
precise as the monadic metalanguage's `let _ = …` in [Moggi
1991](#ref-moggi-1991)\]. Languages that build direct-style effects
in — Frank's "do be do be do" \[[Lindley, McBride & McLaughlin
2017](#ref-frank-2017)\], Koka \[[Leijen 2017](#ref-leijen-2017)\],
Effekt — all adopt the same reading: an effectful statement
executes. Okay's `direct` blocks do too (`Direct.scala:334,404`): a
bare statement whose type is the block's `F[T]` or a row operation
is elaborated as an implicit `.?` with the value dropped, which is
what makes `Writer("env=41")` on its own line *tell*. The guard
narrows to what genuinely cannot be meant: a statement of a foreign
*marked* type (registered via `Effect`, but neither this block's
monad nor in its row) can be neither run nor sensibly dropped, and
refuses to compile. `val` binding, by contrast, holds the program
un-run — binding is consent to have the value — which keeps
program-as-value construction (chapter 4's whole point) available
inside a direct block.

## Call-by-need is an effect

`Delay` is call-by-name — Plotkin's distinction \[[Plotkin 1975](#ref-plotkin-1975)\]:
the thunk is re-run at every use, and two uses of one shared node are
two runs. Call-by-need adds sharing: one cell remembers the first
answer. In a pure language the two are observationally equivalent and
the cell is the optimisation lazy evaluation is named after; for a
computation with effects they differ in the log, which is why Haskell
sequences `IO` with `>>=` and keeps laziness for values, and why
`unsafeInterleaveIO` is a separate, marked word. Okay draws the same
line and puts the cell where a semantics goes: in the row. `Once` is
an effect with two operations (`Force` reads a handle's cell or marks
it running, `Store` fills it and answers the first value stored), the
handle is an identity with no program inside — so the effect's type
does not mention the row it lives in — and `Once.run` is a handler
whose state is the cells, threaded through its loop as `State.handle`
threads `S` (direct-once, 2026-09-16). The consequence is that the
tree stays data: no mutable field, so a program run twice replays the
same trace, and multi-shot needs no flag because it is decided where
every other stateful effect decides it, by handler order —
`runChoice(Once.run(p))` backtracks the cells with the search,
`Once.run(runChoice(p))` shares one store across the branches. In a
block the word is `lazy val`: the macro compiles the right-hand side
to a program under a fresh handle and turns every use into a mark on
it, so the effects run in the position of the first demand, once;
`val` runs now, a bare mark runs at every use. `TestDirectOnce` holds
both handler orders.

## Multi-shot, and the road not taken

There is a second, seemingly cheaper way to get direct style:
suspend a real thread of control instead of rewriting the program —
Loom's virtual threads on the JVM, or runtime effect handlers as
retrofitted onto OCaml \[[Sivaramakrishnan et al. 2021](#ref-ocaml-2021)\].
The price is stated plainly in the OCaml paper: runtime
continuations are **one-shot** (resumable at most once), because a
stack segment cannot be re-run without being copied. Reflection and
elaboration both represent the continuation as a pure closure, which
is trivially multi-shot — and Okay's `Logic`, `Choice`, simulation
and stepper machinery (chapter 7) are exactly the consumers that
invoke a continuation many times. An earlier note in this repository
claimed "direct style would forfeit multi-shot"; the correction,
recorded in specs/context-functions.md, is that *Loom-style* direct
style forfeits it, and the closure-based roads forfeit nothing —
which is why they are the ones Okay walks. The remaining ATM
frontier — one block whose answer type *changes* along its length,
in the manner of Kobori, Kameyama and Kiselyov's prompt-passing
translation \[[Kobori, Kameyama & Kiselyov 2016](#ref-kobori-2016)\]
— is recorded as an open road in the spec, not taken because the
diagonal (one `F[A]` per block) is what direct style means to its
users today.

## References

- <a id="ref-moggi-1991"></a>Eugenio Moggi.
  *Notions of computation and monads.* Information and Computation
  93(1), 1991.
- <a id="ref-wadler-1992"></a>Philip Wadler.
  *The essence of functional programming.* POPL 1992.
- <a id="ref-wadler-1995"></a>Philip Wadler.
  *Monads for functional programming.* Advanced Functional
  Programming, LNCS 925, 1995.
- <a id="ref-danvy-1990"></a>Olivier Danvy, Andrzej Filinski.
  *Abstracting control.* LISP and Functional Programming, 1990.
- <a id="ref-filinski-1994"></a>Andrzej Filinski.
  *Representing monads.* POPL 1994.
- <a id="ref-materzok-2012"></a>Marek Materzok, Dariusz Biernacki.
  *A Dynamic Interpretation of the CPS Hierarchy.* APLAS 2012.
- <a id="ref-filinski-1999"></a>Andrzej Filinski.
  *Representing layered monads.* POPL 1999.
- <a id="ref-kameyama-2003"></a>Yukiyoshi Kameyama, Masahito
  Hasegawa. *A sound and complete axiomatization of delimited
  continuations.* ICFP 2003.
- <a id="ref-asai-2007"></a>Kenichi Asai, Yukiyoshi Kameyama.
  *Polymorphic delimited continuations.* APLAS 2007.
- <a id="ref-flanagan-1993"></a>Cormac Flanagan, Amr Sabry, Bruce
  Duba, Matthias Felleisen. *The essence of compiling with
  continuations.* PLDI 1993.
- <a id="ref-bjarnason-2012"></a>Rúnar Óli Bjarnason.
  *Stackless Scala with free monads.* Scala Days 2012.
- <a id="ref-plotkin-1975"></a>Gordon Plotkin.
  *Call-by-name, call-by-value and the λ-calculus.* Theoretical
  Computer Science 1(2), 1975.
- <a id="ref-kobori-2016"></a>Ikuo Kobori, Yukiyoshi Kameyama, Oleg
  Kiselyov. *Answer-type modification without tears: prompt-passing
  style translation for typed delimited-control operators.* WoC 2015
  / EPTCS 212, 2016.
- <a id="ref-syme-2011"></a>Don Syme, Tomas Petricek, Dmitry Lomov.
  *The F# asynchronous programming model.* PADL 2011.
- <a id="ref-sip22"></a>Philipp Haller, Jason Zaugg.
  *SIP-22: async/await for Scala* (scala-async), 2013 — the scoped
  CPS-transform precedent.
- <a id="ref-shevchenko-2022"></a>Ruslan Shevchenko.
  *Embedding generic monadic transformer into Scala*
  (dotty-cps-async). TFP 2022.
- <a id="ref-effekt-2020"></a>Jonathan Immanuel Brachthäuser,
  Philipp Schuster, Klaus Ostermann. *Effekt: capability-passing
  style for type- and effect-safe, extensible effect handlers in
  Scala.* Journal of Functional Programming 30, 2020.
- <a id="ref-brady-2013"></a>Edwin Brady. *Idris, a
  general-purpose dependently typed programming language: design
  and implementation.* JFP 23(5), 2013 — §bang-notation.
- <a id="ref-frank-2017"></a>Sam Lindley, Conor McBride, Craig
  McLaughlin. *Do be do be do.* POPL 2017.
- <a id="ref-leijen-2017"></a>Daan Leijen. *Type directed
  compilation of row-typed algebraic effects.* POPL 2017.
- <a id="ref-ocaml-2021"></a>K.C. Sivaramakrishnan, Stephen Dolan,
  Leo White, Tom Kelly, Sadiq Jaffer, Anil Madhavapeddy.
  *Retrofitting effect handlers onto OCaml.* PLDI 2021.
