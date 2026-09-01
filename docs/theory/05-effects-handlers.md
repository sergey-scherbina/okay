# 5. Algebraic effects and handlers

## Operations, then handlers

Gordon Plotkin and John Power reframed Moggi's monads from the inside:
an effect is not a type constructor first but a set of **operations**
with equations — nondeterminism *is* `choose`, state *is* `get`/`put` —
and the monad arises from the algebraic theory [Plotkin & Power 2002,
2003]. The payoff of thinking operation-first is separation: a program
*performs* operations; what they mean is decided elsewhere.

"Elsewhere" got its name with Plotkin and Matija Pretnar's **handlers**
[Plotkin & Pretnar 2009]: a handler gives each operation an
interpretation that receives the operation's payload *and the
continuation of the program at that point*. Exceptions discard the
continuation; state threads a value through it; nondeterminism calls it
several times. Handlers generalize `try/catch` from "catch a throw" to
"catch any operation", and they compose: unhandled operations pass
through to an outer handler.

The third ingredient is making the *set of pending effects* extensible
and inferable. Kiselyov, Sabry and Swords' extensible effects
[Kiselyov, Sabry & Swords 2013] replaced monad-transformer stacks with
a single monad over an open union of signatures — the design Okay
follows, with chapter 4's freer monad as the carrier.

## Rows as unions, and the trusted kernel

Okay's effect row is a *type-level union of signatures*:

```scala
A ! (State % Int + Throws % String + Async)
```

`+` is genuine union, not a coproduct functor — which is why `Pure`,
the empty row, can be `Nothing` (`Effects.scala:34`): the union with
nothing added is the row itself, and a pure program `A ! Pure`
coerces into any row for free by covariance (`F[Nothing] <: F[X]`,
noted at `Effects.scala:213`).

A handler for `F` inside a row `F + G` must *split* the union: given an
operation, is it mine or the residue's? That is `<|>`
(`Effects.scala:231`), and it rests on `TypeableK[F]`
(`Effects.scala:157`) — a runtime class test for "is this value an
`F`-operation". The split's soundness argument is written where the one
cast lives (`Effects.scala:234`): *the trusted kernel, sound by the
excluded middle of the union* — a value of type `F[A] | G[A]` that the
`F`-test rejects **is** a `G[A]`, provided the two signatures' erasures
are distinguishable. That proviso is a real obligation, not a
formality: the Writer story below exists because an identity-encoded
signature erased to *its element's* class and could collide with
anything.

## Three shapes of handler, one line

`Effects.scala:424–439` states the design in a comment worth quoting
almost whole. With `F ==> H` meaning a natural transformation from the
signature to a carrier:

| shape | meaning | limits |
|---|---|---|
| `F ==> Id` — `Handler[F]` | comonadic: every operation is *answered on the spot* | cannot suspend — "Id is exactly where a suspension cannot go", so no I/O on a platform with no thread to park |
| `F ==> ([X] =>> X ! G)` — `!.translate` | tail-resumptive: answer with *more program* in another row | one walk, no `Cont`; cannot abort or resume twice |
| `F !> S = F ==> ([X] =>> X /> S)` | Cont-valued: the operation receives its delimited continuation | full Plotkin–Pretnar power — abort, multi-shot — at the price of going through `Cont` |

This is chapter 2's theorem specialized twice: the Cont-valued form is
the general handler Filinski's theorem promises; the other two are the
fast degenerate points where the continuation is used exactly once and
immediately, and Okay makes them separate constructs *because they cost
less* — `translate` forwards `G` in a single tail-recursive walk, and
`Handler[F]` is what `runWith` consumes. Choosing the weakest shape
that suffices is the library's standing advice, and the platform
constraint enforces it once: on JS there is no `CanBlock`, so a
comonadic `Handler[Async]` does not exist there *by type*, and code
must peel to `Async.runAsync` instead (`cross-platform-async.md`).

## The worked example: Writer's six encodings

`docs/existentials.md` is this chapter's laboratory. The Writer effect
wants a free `tell` — no allocation per emitted element — which the
freer carrier permits via an identity signature. The price surfaced as
one unprovable equation (the operation's answer type), and six
encodings were tried against it: naming the existential (two forms of
[the mapK newtype]), minted `=:=` evidence, an intersection type, a
match type as the alias, a match type inside the alias. Five failed —
four for the same mechanical reason, *erasure follows the
representation* — and the bytecode of each failure is in that page. The
sixth, a one-constructor GADT (`case Say(w: W) extends Writer[W, Unit]`),
succeeded by giving up the identity representation, and the measurement
that authorized it (no detectable cost on the real benchmark) closed
the question. Read it as this chapter's moral told concretely: in an
extensible-effects system the *signature's runtime representation* is
part of the semantics, because the row split and the handler's
refinement both read it.

## References

- Gordon Plotkin, John Power. *Notions of computation determine
  monads.* FoSSaCS 2002.
- Gordon Plotkin, John Power. *Algebraic operations and generic
  effects.* Applied Categorical Structures 11(1):69–94, 2003.
- Gordon Plotkin, Matija Pretnar. *Handlers of algebraic effects.*
  ESOP 2009.
- Oleg Kiselyov, Amr Sabry, Cameron Swords. *Extensible effects: an
  alternative to monad transformers.* Haskell Symposium 2013.
- Oleg Kiselyov, Hiromi Ishii. *Freer monads, more extensible
  effects.* Haskell Symposium 2015.
