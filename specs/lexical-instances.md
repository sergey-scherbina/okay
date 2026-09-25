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
- [ ] Stage 1: `tail`, priced against `row` and `deep`, with its
      precondition stated.
- [ ] Stage 2: stacked instances (`Delim.Stacked`), so an instance used
      outside its installation is a compile error.
- [ ] Stage 3: a DEFAULT combinator that picks a strategy from what
      the clauses are (tail-resumptive or not). Every strategy stays
      callable by name.

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

## Results

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
