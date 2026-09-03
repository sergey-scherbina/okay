# The theory of Okay — the map

*(The short pretty index is [index.md](index.md); this page is the map
with the notation and the reading order.)*

Okay is a small library built out of large ideas, and its code cites
them in passing — `Cont.scala` opens by naming Danvy and Filinski,
`Logic.scala` names LogicT, the staging comment names
Carette–Kiselyov–Shan. This book is the connected account: which
theories the library stands on, who established them, where to read
them, and — for each design decision — why *this* point in the design
space, argued from the cited work or from a measurement the repository
keeps.

It teaches the theory through one concrete library. Every claim about
Okay carries a `file:line` or a quoted signature from the tree as it
stands; every theoretical claim carries author, title, venue, year.
Refuted designs are kept, with their refutations — the six encodings
tried against the Writer cast and the benchmark attributions that
turned out wrong are worked examples of the method, not embarrassments.

## The chapters

| | | the papers |
|---|---|---|
| [1. Monads and functors](01-monads.md) | why effects want a monad, and what `A ! F` is | Moggi 1989/1991 · Wadler 1992/1995 |
| [2. Continuations and delimited control](02-continuations.md) | `Cont`, shift/reset, prompts — the foundation everything else stands on | Felleisen 1988 · Danvy & Filinski 1990 · Filinski 1994 |
| [3. Parameterised monads](03-parameterised.md) | answer-type modification, typestate, `M[A, S, R]` | Atkey 2009 · Asai & Kameyama 2007 |
| [4. Free and freer](04-free-freer.md) | programs as data, the left-nested-bind problem, and whether it is real here | Swierstra 2008 · Kiselyov & Ishii 2015 · Voigtländer 2008 |
| [5. Algebraic effects and handlers](05-effects-handlers.md) | operations, rows as unions, three shapes of handler | Plotkin & Power 2003 · Plotkin & Pretnar 2009 · Kiselyov, Sabry & Swords 2013 |
| [6. Final tagless and staging](06-tagless-staging.md) | two ways to make abstraction free | Carette, Kiselyov & Shan 2009 · Taha & Sheard 1997/2000 |
| [7. Logic, streams and sketches](07-logic-streams.md) | msplit, codata, the fold algebra, approximation with stated error | Kiselyov, Shan, Friedman & Sabry 2005 · Wadler 1985 · Flajolet et al. 2007 |

## The notation, once

These five spellings appear on nearly every page, all from
`Effects.scala`:

```scala
// Effects.scala:40 — "A, computed with effects F"
infix type ![A, F[+_]] = Free[F, A]
// Effects.scala:28 — fix a signature's first parameter
infix type %[F[_, _], S] = F[S, *]
// Effects.scala:34 — the empty effect row
type Pure = Nothing
// F + G is a union of signatures; a row is built with % and +
// Effects.scala:59 — a Cont-valued handler
infix type !>[F[_], S] = Interpr[F, Cont, S]
```

So `A ! (State % Int + Throws % String)` reads: *a program computing
`A`, which may use integer state and may throw strings*. `Pure` being
`Nothing` is not a trick spelling — chapter 5 explains why the empty
row is the empty type, and what that buys.

## How to read

Chapters 1–2 are the foundation and are worth reading in order:
chapter 2 explains the sentence that justifies the whole architecture
(Filinski's theorem that delimited continuations express every monadic
effect — which is why `Cont` is the bottom of Okay's tower and
everything else is layered on it). After that the chapters are
independent. Each ends with its references and with pointers into the
rest of `docs/` — the [typepedia](../typepedia.md) for what each type
is, [benchmarks](../benchmarks.md) for every number quoted, and
[existentials](../existentials.md) for the longest worked example of
the house method: measure, refute, keep the refutation.

---

[Contents](index.md) · [1 · Monads and functors](01-monads.md) →
