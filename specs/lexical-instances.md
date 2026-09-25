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

## Interface (stage 0)

```scala
object Lexical:
  /** an installed handler of F whose answer is R: the value the body
   * gets, which its operations go through */
  final class Inst[F[+_], R] private[Lexical] (...):
    def perform[X, G[+_]](e: F[X]): X ! Delim + G

  /** a handler's clauses: the return clause and one clause per operation */
  trait Clauses[F[+_], A, R, G[+_]]:
    def ret(a: A): R ! Delim + G
    def op[X](e: F[X], k: X => R ! Delim + G): R ! Delim + G

  /** strategy primitives: the same clauses, two ways to run them */
  def deep[F[+_], A, R, G[+_]](c: Clauses[F, A, R, G])(body: Inst[F, R] => A ! Delim + G): R ! Delim + G
  def shallow[F[+_], A, R, G[+_]](c: ShallowClauses[F, A, R, G])(body: Inst[F, R] => A ! Delim + G): R ! Delim + G

  object State:   // the worked instance
    def deep[S, A, G[+_]](s0: S)(body: Inst[State % S, …] => A ! Delim + G): (S, A) ! Delim + G
    def shallow[S, A, G[+_]](s0: S)(body: …): (S, A) ! Delim + G
    extension [S](i: Inst[State % S, ?]) def get: S ! Delim + G; def set(s: S): S ! Delim + G
```

The body gets its instance as a LAMBDA PARAMETER, not a given. Two
instances of one type are then two names, as in the POPL 2020 calculus,
and not an ambiguous implicit.

## Behavior

- [ ] Stage 0: two `State[Int]` instances in one program, each operation
      reaching its own installation. The same program on a row needs
      `Tag` and is refused by `Distinct` without it.
- [ ] Stage 0: no accidental handling. An inner instance of the SAME
      effect does not catch an operation addressed to the outer one.
- [ ] Stage 0: one instance, `deep` and `shallow`, is `Bisim`-equivalent
      to `State.handle` on the residual row.
- [ ] Stage 0: the generic `deep`/`shallow` combinators with user
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

## Results
