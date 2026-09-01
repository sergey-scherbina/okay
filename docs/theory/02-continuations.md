# 2. Continuations and delimited control

## The continuation, and why undelimited is not enough

A continuation is "the rest of the computation", reified as a function.
The classical control operator `call/cc` captures *all* of it — from
here to the end of the program — which makes it both powerful and
unusable compositionally: a captured continuation never returns, so
there is no answer to hand back and no way to run a captured piece
*inside* a larger program.

Delimited control fixes this by marking where "the rest" stops.
Matthias Felleisen introduced the **prompt** \[[Felleisen 1988](#ref-felleisen-1988)\]: a
delimiter installed on the stack, and a control operator capturing only
up to it. The captured piece is then an ordinary function — it returns
— so captured continuations compose. Olivier Danvy and Andrzej Filinski
gave the now-standard pair **shift/reset** and, crucially, its typing
discipline \[[Danvy & Filinski 1990](#ref-danvy-1990); [the typing with answer types is in
their 1989 report, and its polymorphic account is Asai & Kameyama
2007](#ref-asai-2007)\]: `reset` delimits, `shift f` captures the continuation `k` up to
the nearest `reset` and runs `f(k)` in its place.

The reason this chapter comes before everything else is Andrzej
Filinski's theorem \[[Filinski 1994](#ref-filinski-1994)\]: **shift and reset can express any
monadic effect.** State, exceptions, nondeterminism — each is a
particular way of invoking (or not invoking, or twice invoking) the
captured continuation. A language, or a library, that has delimited
control at the bottom does not need to *build in* any other effect; it
needs only to program them. That is Okay's architecture in one
sentence.

## What this is in Okay: `Cont`

```scala
// Cont.scala:39 — capture the current continuation
// (Danvy–Filinski, with answer-type modification)
inline def shift[A, S, R](f: (A => S) => R): Cont[A, S, R] = Cont.Shift(f)
// Cont.scala:41 — delimit: run with the identity continuation
inline def reset[A, R](c: A ^ R): R = c / identity
```

`Cont[A, S, R]` *means* `(A => S) => R` — chapter 3 explains the three
parameters — and `A /> R` (`Cont.scala:35`) is its diagonal
`Cont[A, R, R]`, the ordinary continuation monad. The interesting
engineering is that `Cont` is **defunctionalized**: rather than being
the function type it means, it is a data type (`Pure`, `Shift`,
`Bind` — `Cont.scala:54–61`) with one interpreter, `/`. Two
consequences, both load-bearing:

**Stack safety.** A directly-encoded continuation monad overflows the
stack on long `flatMap` chains — the classic problem Rúnar Bjarnason
treated for Scala with trampolines \[[Bjarnason 2012](#ref-bjarnason-2012)\]. Okay's answer is
the same normalization move chapter 1 showed for `Free`: `Bind` is a
node, and `/` rebalances left-nested binds in a tail-recursive loop.

**Fusion under a budget.** Pure defunctionalization pays a node per
bind. `Cont.flatMap` (`Cont.scala:68–70`) therefore *fuses* into the
`Shift` closure — `Shift(k => s(f(_)(k)))` — while a depth budget
(`Cont.Fuse`, default 128) lasts, and only then spills into `Bind`
data. Short chains run as plain nested closures at closure speed; long
chains switch to the stack-safe interpreter. The budget is the
compromise between the two encodings, chosen by measurement rather
than doctrine.

## Prompts as an effect: `Delim`

`shift`/`reset` capture to the *nearest* delimiter. Multi-prompt
control — capture to a *named* delimiter, possibly across intervening
ones — is strictly more expressive, and Okay implements it in the
shape of Dybvig, Peyton Jones and Sabry's monadic framework \[[Dybvig,
Peyton Jones & Sabry 2007](#ref-dybvig-2007)\]: a **prompt is a first-class tag carrying
the delimiter's answer type** (`Delim.scala:74`), `push` installs one,
and `shift(p)` captures up to the prompt `p` (`Delim.scala:90`).

Two decisions are documented in `Delim.scala`'s header because both
were "arrived at the hard way", and they are worth restating as
theory-meets-implementation:

- **`push` is an operation, not a handler application.** Capturing
  *across* an intervening delimiter is the point of multi-prompt, and
  nested handlers cannot do it — an inner handler forwarding a shift
  it does not own forwards it opaquely, leaving its own frames out of
  the captured continuation. One machine must own the whole prompt
  stack, so both `push` and `shift` reach it as operations of a single
  `Delim` signature.
- **Tags are what let several answer types share one row.** Okay's
  union splitting is by runtime class (chapter 5); a signature
  parameterised by its answer type would erase two different prompts
  to the same class. With the answer type riding inside the tag, one
  `Delim` signature suffices and tags keep prompts apart.

The generators are the everyday face of this machinery: `Generate.scala`
builds `LazyList`, `Producer` and `Teller` from one delimited-control
unfold (`Loop`/`take`/`put`), which is `shift` capturing "the rest of
the enumeration" at each element.

## Why Cont is the bottom of the tower

Filinski's theorem says delimited control *suffices*; Okay's
`Effects.scala` makes it the actual foundation: a Cont-valued handler
`F !> S = F ==> ([X] =>> X /> S)` (`Effects.scala:59`) interprets each
operation as a continuation-manipulating program, and the comment at
`Effects.scala:424–432` places the three handler shapes — comonadic
`F ==> Id`, translating `F ==> ([X] =>> X ! G)`, and Cont-valued —
"on one line". The first two are the degenerate points (never touch
the continuation; touch it only to re-inject), the third is the
general case, and it is general *because* of Filinski. Chapter 5 walks
the three shapes; the point here is that they are not three features
but one theorem, specialized twice.

## References

- <a id="ref-felleisen-1988"></a>Matthias Felleisen. *[The theory and practice of first-class
  prompts.](https://doi.org/10.1145/73560.73576)* POPL 1988.
- <a id="ref-danvy-1989"></a>Olivier Danvy, Andrzej Filinski. *A functional abstraction of typed
  contexts.* DIKU report 89/12, 1989.
- <a id="ref-danvy-1990"></a>Olivier Danvy, Andrzej Filinski. *[Abstracting control.](https://doi.org/10.1145/91556.91622)* LISP and
  Functional Programming 1990.
- <a id="ref-filinski-1994"></a>Andrzej Filinski. *[Representing monads.](https://doi.org/10.1145/174675.178047)* POPL 1994.
- <a id="ref-asai-2007"></a>Kenichi Asai, Yukiyoshi Kameyama. *[Polymorphic delimited
  continuations.](https://doi.org/10.1007/978-3-540-76637-7_16)* APLAS 2007.
- <a id="ref-dybvig-2007"></a>R. Kent Dybvig, Simon Peyton Jones, Amr Sabry. *[A monadic framework
  for delimited continuations.](https://www.cs.indiana.edu/~dyb/pubs/monadicDC.pdf)* JFP 17(6):687–730, 2007.
- <a id="ref-bjarnason-2012"></a>Rúnar Bjarnason. *[Stackless Scala with free monads.](http://blog.higher-order.com/assets/trampolines.pdf)* 2012.

---

← [1 · Monads and functors](01-monads.md) · [Contents](index.md) · [3 · Parameterised monads](03-parameterised.md) →

*Shipped consumers of this chapter (2026-09-01):* `Delim` carries
Dialog's cancellable scopes (`Scope`), the streaming cut (`Cut`),
the agent stepper's pause-and-fork, and the sim scheduler's
captured-continuation feeding — the operator's adoption doctrine
(specs/delimited-control.md, Adoption) states when it is the
mechanism and when an option.
