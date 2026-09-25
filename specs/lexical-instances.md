# lexical-instances — a handler instance is a prompt; every strategy is a primitive you can name

## Overview

A row routes an operation to a handler by the operation's CLASS
(docs/many-instances.md). That is why a row holds one `State % Int`,
why two of a kind misroute (`Distinct` refuses them), and why a
handler forwarding an operation it does not own cannot tell whose it
was ("accidental handling", Zhang & Myers, POPL 2019). The fourth
route in many-instances.md, "a fresh prompt", has been a paragraph
with no API behind it.

This spec builds that API. A handler INSTALLATION is a prompt, and the
program reaches it through a value the installation hands its body.
This is the design of Biernacki, Piróg, Polesiuk & Sieczkowski,
"Binders by day, labels by night: effect instances via lexically
scoped handlers" (POPL 2020): a lexical binder in the types, and a
fresh runtime label in the machine.

```scala
Lexical.State.deep(0) { a =>
  Lexical.State.deep(10) { b =>
    for x <- a.get; y <- b.get; _ <- a.set(x + y) yield ()   // two State[Int], no row, no key
  }
}
```

THE OPERATOR'S RULE for this arc (2026-09-25): every way of running a
handler is a separate, explicit primitive or combinator that a caller
can pick by name. A DEFAULT strategy is built from them afterwards,
and it never removes the manual choice.

## The strategies (each one a primitive)

| strategy | how an operation reaches the handler | k | what it costs | when it is the right one |
|---|---|---|---|---|
| `row` | class test in a row (today's handlers) | the program's own `Bind` closure | the floor (`State.handle`: 35 µs / 1000 get+set) | one of a kind per row, the common case |
| `deep` | `shift0` to the installation's prompt, which is a `dollar` with the return clause | re-installs the handler (FSCD 2019: deep ↔ shift0) | 3.8x time, 7.4x bytes (handlers-as-dollar) | many instances, multi-shot, a clause that stores or drops k |
| `shallow` | `control0` to the prompt | bare; the clause re-installs the handler around k if it wants | 4.7x / 7.8x | a clause that changes handler between operations |
| `tail` | the operation calls the clause IN PLACE through the instance (evidence passing, Xie et al. ICFP 2020) | none captured: the clause's answer is the resumption value | expected near `row` (stage 1 measures it) | tail-resumptive clauses: State, Reader, Writer |

`row` exists. `deep` and `shallow` exist as test code (TestHandlersAsDollar)
and become combinators here. `tail` is new. It is only sound when no
capture crosses the instance between an operation and its answer.
Stage 1 states that precondition in the type or in the documentation.

## Interface (stage 0, as built)

```scala
object Lexical:
  final class Inst[F[+_], R, G[+_]]:                 // made only by an installation
    val prompt: Prompt[R]
    def perform[X](e: F[X]): X ! Delim + G           // the same call for every strategy
  trait Ops[F[+_], R, G[+_]]:            def op[X](e: F[X], k: X => R ! Delim + G): R ! Delim + G
  trait Clauses[F[+_], A, R, G[+_]] extends Ops[F, R, G]: def ret(a: A): R ! Delim + G
  trait ShallowClauses[F[+_], A, R, G[+_]]:
    def ret(a: A): R ! Delim + G
    def op[X](e: F[X], k: X => R ! Delim + G, again: (R ! Delim + G) => R ! Delim + G): R ! Delim + G

  def deep[F[+_], A, R, G[+_]](c: Clauses[F, A, R, G])(body: Inst[F, R, G] => A ! Delim + G): R ! Delim + G
  def shallow[F[+_], A, R, G[+_]](c: ShallowClauses[F, A, R, G])(body: Inst[F, R, G] => A ! Delim + G): R ! Delim + G

  object State:
    type Ans[S, A, G[+_]] = S => (S, A) ! Delim + G
    def deep[S, A, G[+_]](s0: S)(body: Inst[okay.State % S, Ans[S, A, G], G] => A ! Delim + G): (S, A) ! Delim + G
    def shallow[S, A, G[+_]](s0: S)(body: …): (S, A) ! Delim + G
    extension [S, R, G[+_]](i: Inst[okay.State % S, R, G]) def get: S ! Delim + G; def set(s: S): S ! Delim + G
```

The body gets its instance as a LAMBDA PARAMETER, not a given. Two
instances of one type are then two names, as in the POPL 2020 calculus,
and not an ambiguous implicit.

## Behavior

- [x] Stage 0: two `State[Int]` instances in one program, each operation
      reaching its own installation. The same program on a row needs
      `Tag` and is refused by `Distinct` without it.
- [x] Stage 0: no accidental handling. An inner instance of the SAME
      effect does not catch an operation addressed to the outer one.
- [x] Stage 0: one instance, `deep` and `shallow`, is `Bisim`-equivalent
      to `State.handle` on the residual row.
- [x] Stage 0: the generic `deep`/`shallow` combinators with user
      clauses: a non-tail-resumptive handler (collect every answer of a
      multi-shot choice), which `tail` cannot run.
- [x] Stage 1: `tail`, with its precondition CHECKED (a guard that
      throws `MultiShotAcrossTail`), and priced against `row` and `deep`
      (Results).
- [x] Stage 2: stacked `deep` and `tail` instances (`Lexical.Stacked`),
      so an instance used outside its installation is a compile error.
- [x] Stage 3: `Lexical.handle` picks by clause kind (TailClauses →
      tail, Clauses → deep, ShallowClauses → shallow), and
      `Lexical.State(s0)` is tail. Every strategy stays callable by name.

## Out of scope

- Replacing the row. `row` stays the default for one of a kind. This
  arc adds routes and never removes one.
- Typed shallow handlers with `k: X => R0` (a dollar's body type).
  handlers-as-dollar showed that the shallow State handler does not
  need it.

## Decisions

- **The instance is a lambda parameter, and `Inst` carries the
  operation, not the clauses' types (stage 0).** `Inst[F, R, G]` holds
  its prompt and a polymorphic `run: [X] => F[X] => X ! Delim + G`
  built at installation. The strategy is fixed where the instance is
  made, so `perform` is the same call for every strategy. The body's
  answer type `A` appears only in the return clause and never in
  `Inst`.
- **No row test anywhere.** `F` is a phantom of the instance: the
  operation travels as a `Delim` capture, so `F` needs no `TypeableK`,
  the program needs no `Distinct`, and a user effect is a plain `enum`
  (TestLexical's `Flip`).

- **`Inst[F, G]` lost its answer type (stage 1).** Stage 0 had
  `Inst[F, R, G]`, where `R` differed by strategy (a state-passing
  function for deep, a pair for tail). A body written against one
  strategy then did not type against another. Without `R`, changing
  strategy is changing one word at the installation, which is the
  point of naming them.
- **`tail` is guarded, not trusted (stage 1).** A cell and a
  continuation-carried state disagree in exactly one shape: a capture
  from OUTSIDE the installation that resumes its body more than once.
  A multi-shot capture INSIDE the body threads the cell through its
  branches in order, and so does `deep`, whose state capture crosses
  that inner prompt (pinned: both give `(6, List(0, 1, 3))`). The
  installation is a `dollar` on its own prompt, and its return
  function runs once per resumption, so the second run throws
  `MultiShotAcrossTail` with the way out in its message. That costs one
  delimiter per installation, not per operation. The cell and the flag
  are made per run of the program, so running a program twice is not a
  multi-shot (pinned).
- **Stacked instances ARE their delimiter (stage 2).** `Delim.Stacked.In`
  became an open class, and `Lexical.Stacked.Deep` and `.Tail` extend it,
  so `i.p` is the singleton on the stack and `perform` asks the same
  `Has` a stacked `shift0` does. A wrapper holding an `In` would have
  two singletons, `i.in.p.type` and `d.p.type`, that the compiler cannot
  relate: the same trap `Layered.Stacked` avoided by using the `In`
  itself. `shallow` is not stacked, for control0's reason.
- **The default is by clause KIND, and the row is not a candidate
  (stage 3).** Tail-resumptiveness is declared by the type of the
  clauses (`TailClauses` can only answer in place), so the choice is
  static and free. `tail` is safe as a default only because its one
  unsafe shape fails loudly. The row stays the library's default for
  one handler of a kind. `Lexical` is for when that is not enough.

- **Pay as you go (lexical-tail-allocs, 2026-09-25).** `Inst[F, G]`
  now works over the WHOLE row the body uses. `deep` and `shallow`
  require evidence that `G` has `Delim`. `tail` chooses how to close by
  the row, at compile time (`Closing`): a row without `Delim` cannot
  capture, so it gets no guard and no machine, the program stays
  `A ! G` and is run by whatever runs `G`. A row with `Delim` gets the
  guard. `Delim + G` read as `G` uses a witness built without a cast:
  `ev.liftCo[[x] =>> x | G[Any]]` turns `Delim ⊆ G` into
  `(Delim | G) ⊆ G`.
- **The unguarded close WALKS the body; it does not map over it.** The
  first cut ended with `body.map(finish)`, a `Bind` over the whole
  body, and `resume` re-associated every step under it. That cost +36 B
  per operation, which made "no guard, no machine" MORE expensive than
  the guarded path (438 304 against 366 656 B). The walk is
  `State.handle`'s loop with nothing to handle.

STAGE 0, 2026-09-25 (TestLexical 7):

- Two `State[Int]` instances in one program, deep and shallow, give
  `(10, (10, 10))`, worked by hand. On a row the same pair is refused
  by `Distinct` ("cannot be told apart in one row"). The test first
  asserted the word "Distinct", which the message does not contain.
  Reading the actual refusal text caught that.
- No accidental handling: `outer.get` inside an inner `State[Int]`
  answers 0 and `inner.get` answers 10. There is no row program to
  compare it with, because a row cannot address past the innermost
  handler of one signature.
- One instance, deep and shallow, is `Same(1, 0)` against
  `State.handle` on the Writer row.
- A user effect (`Flip`, a plain enum) with a multi-shot clause gives
  all four answers of two coin flips, which is what `tail` will not be
  able to run. 10 000 get/set run through one deep instance in constant
  stack.

STAGES 1-3, 2026-09-25 (TestLexicalTail 6, TestLexicalStacked 2, TestLexicalDefault 2):

- tail: `Bisim`-equal to `State.handle` on the Writer row. It mixes
  with deep in one program (deep outer, tail inner, the stage-0 answer).
  Multi-shot inside gives `(6, List(0, 1, 3))`, the same as deep. Multi-shot
  across: deep gives `List((1, 0), (2, 0), (3, 0))`, and tail throws.
  100 000 operations run in constant stack.
- WATCHED FAILING: the guard mutated to never throw turned the "across"
  test red.
- Stacked: a tail instance outside a deep one gives `(5, 10)` (by hand).
  A tail instance kept past its installation is refused with "not on the
  prompt stack".
- Default: `Lexical.State(0)` trips the guard across a multi-shot, and
  `Lexical.State.deep` is the named way out. `Lexical.handle` runs
  general clauses deep (a coin's two answers) and tail clauses in place
  (`(14, 14)`).

PRICE, 2026-09-25 (DelimBenchmark stateHandle / stateLexTail / stateLexDeep,
one run, load 27 → 75: time noisy, bytes exact):

- Bytes per 1000 get/set: row 222 040, tail 366 590 (1.65x), deep
  1 511 046 (6.8x). Time: row 70 ± 15 µs, tail 154 ± 55 (about 2x),
  deep 1117 ± 401 (not usable; the clean stage-3 run of handlers-as-dollar
  gave deep 3.8x).
- THE SPEC'S EXPECTATION WAS WRONG: "tail near row". It is a quarter of
  deep's cost, not the row's. The +72 B per operation are the
  `Free.delay` thunk that keeps the cell access lazy, the `(S, X)` pair
  a `TailClauses` answer returns, the polymorphic `run`, and a
  `Return`. Backlog `lexical-tail-allocs` has the ways to take them out,
  each to be measured. The strategies and the default do not change.

PAY AS YOU GO, 2026-09-25 (bytes per 1000 get/set; time too noisy to quote):

- Row 222 040. Tail with the guard 366 659. Tail on a row without Delim,
  first cut 438 304. The same program in the Delim machine gave 438 364,
  which ruled out the runner as the cause. After the walk fix: 366 224.
- REFUTED: a `Cell` the clause writes instead of an `(S, X)` pair.
  366 224 against 366 224: escape analysis had already removed the pair.
  Reverted.
- What is left, +72 B per operation over the row, is a `Delay` node,
  its thunk and a `Return`, which keep an instance's operation lazy.
  That is the price of instance identity by evidence instead of routing
  by class. The row stays the zero-overhead default for one handler of a
  kind. Backlog `lexical-tagged-walk` has the one design that could
  close most of it, and the semantic reason it is not the default.
