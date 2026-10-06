# okay-cont

A monad of delimited continuations typed by two stacks of answer types,
`Cont[I, O, A]` — Danvy–Filinski's `shift0`/`reset` with answer-type
modification, handlers as delimiters, operations as one capture to their
handler through the delimiters between — and a machine that runs it: seven
nodes, no cast, no prompt, no row (specs/freer-min.md, stages 1–30). It
has no dependency and runs on the JVM, Scala.js and Scala Native; the
core depends on it, beside okay-freer, and the two know nothing of each
other.

| | |
|---|---|
| `Cont[I, O, A]` | a program of `A` which, with a continuation answering `I`, answers `O` — `(A => I) => O` with the answer types grown into two stacks, a level each |
| `reset[S](body)`, `shift0[X](k => …)` | the delimiter and the capture; the shift learns its delimiter from the context the body is written in |
| `Handler[E, A, Ans]`, `handle(h)(body)` | a handler is a delimiter: deep, its clauses outside it; `Answering` answers in place, no capture |
| `perform(op)` | the operation, to its handler found in the context's types at compile time: none in scope, no program |
| `Free[R, A]` | a program over a nominal row `Ask :+: Say :+: RNil`, built with no handler in sight, run under them |
| `Carrier[A, S, R]` | the machine as a `Control` carrier: a program one level over the top, `(A => S) => R`; the instance `Control[Carrier]` and `Prog`, the machine as an `Effects` encoding, are the core's (Control.scala, Prog.scala) — this module knows nothing of the core |
| `Machine.run`, `Machine.value` | a run to its typed end, `Head`: a value, or a capture handed out for a machine outside |

## Using it

A handler is a delimiter, and an operation reaches it through the context:

```scala
def reader[A](n: Int): Handler[Ask, A, A] = new Handler[Ask, A, A]:
  def ret(a: A): A = a
  def apply[X, Oc <: Ctx](using o: Oc)(op: Ask[X], k: X => Cont[o.Here, o.Here, A]): Cont[o.Here, o.Here, A] = op match
    case Ask.Number => k(n)
```

The same program over any `Effects` encoding, the machine's chosen by its given:

```scala
def prog[M[_[+_], _]](using E: Effects[M]): M[Produce, Int] =
  E.perform[Produce, Int](1).flatMap(x => E.perform[Produce, Int](x + 1).map(y => x + y))
assertEquals(summon[Effects[Prog]].runWith(prog[Prog]), 3)
```

Measured against the core's handlers (stage 28): general clauses 1.29x,
answering handlers at the core's, tail calls 0.85x.
