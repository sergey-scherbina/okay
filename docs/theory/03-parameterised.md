# 3. Parameterised monads

## Atkey's generalization

Robert Atkey asked what happens when a monad's type is indexed not by
one type but by a *pair* — a "before" and an "after" \[[Atkey 2009](#ref-atkey-2009)\]. A
**parameterised monad** `M[A, S, R]` is a computation of a value `A`
that moves an index from `S` to `R`; `pure` sits on the diagonal
(`S = R`, it moves nothing), and `flatMap` composes indices like
function composition:

```scala
// Monad.scala:11
trait ParaMonad[M[_, _, _]] {
  // identity: R -> R
  def pure[A, R](a: A): M[A, R, R]
  extension [A, S, R](m: M[A, S, R])
    // composition: (S -> R) o (S2 -> S) = S2 -> R
    def flatMap[B, S2](f: A => M[B, S2, S]): M[B, S2, R]
}
```

Okay's `Monad.scala` cites the paper in its header and states Atkey's
own slogan as a comment: the computation "is indexed by an arrow
`S -> R` in a category of states". Two of Atkey's example
instantiations are exactly the two Okay uses.

## Instance one: the continuation monad with answer-type modification

Chapter 2's `Cont[A, S, R]` means `(A => S) => R`. Read through
Atkey's lens: the "state" being moved is the **answer type** — the
type of what the surrounding `reset` will produce. `shift` may hand
back an answer of a different type than its continuation returns, and
the indices track that honestly. This is answer-type modification
\[[Danvy & Filinski 1989](#ref-danvy-1989); [Asai & Kameyama 2007](#ref-asai-2007)\], and the parameterised
monad is its natural typing: `Cont` is not "a monad with two extra
parameters bolted on" but the paramonad whose arrows are
answer-transformations.

The bridge back to ordinary monads is Atkey's too: **every diagonal of
a parameterised monad is a monad**. Okay spells it as an instance
(`Monad.scala:36–43`):

```scala
final class DiagonalMonad[M[_, _, _], R](val P: ParaMonad[M])
  extends Monad[[A] =>> M[A, R, R]]
given [M[_, _, _] : ParaMonad as P, R]: Monad[[A] =>> M[A, R, R]] = ...
```

which is how `A /> R = Cont[A, R, R]` gets its `Monad` and how the
whole effect machinery of chapter 5 — built on `/>`-valued handlers —
rides on ordinary monadic code while the general three-parameter form
stays available underneath.

(An implementation footnote recorded at `Monad.scala:29`: the diagonal
instance is a *named class with a public `P`*, not an anonymous
`given … with`, because an `inline` method reaching a privately
captured given makes the compiler synthesize an accessor with an
unstable name, breaking downstream binaries on a mere recompile. Theory
chose the shape; binary compatibility chose the spelling.)

## Instance two: typestate

Atkey's other flagship example is state whose *type* changes as the
program runs — a file handle that is `Open` before `close` and
`Closed` after, with the indices making misuse a type error. Okay's
`PState` (`State.scala:69–75`) is this instance: its `flatMap`
"already composes the transitions `S -> S2 -> S3` (typestate)", so a
protocol's stages become index transitions and skipping a stage fails
to compile.

The price was measured rather than assumed: the typestate variant
costs about 1.7x the plain `State` handler (`docs/benchmarks.md`), so
Okay keeps both — `State % S` for the common case where the type never
changes, `PState` where the protocol is the point. This is the
recurring house pattern: the more general theory is present, and the
specialized fast path exists *because a benchmark said so*, not
instead of the theory.

## Why a paramonad at the foundation

Chapter 2 ended with Filinski: delimited control expresses every
monadic effect. The typing that makes this *safe* is precisely the
parameterised structure — without answer-type indices, `shift`'s
ability to change the answer type is either forbidden (losing
expressiveness) or unchecked (losing safety). So Okay's tower has a
paramonad at the bottom out of necessity, and Atkey's diagonal theorem
is the ramp back down to the ordinary monads everything else is
written in. One trait (`ParaMonad`), one theorem (the diagonal), two
instances (`Cont`, `PState`) — the chapter is short because the design
followed the paper closely enough that there is little else to say.


**The production consumer.** The two-state degenerate form of this
chapter's typestate ships in okay-sql: `Typed.region` (Typed.scala)
carries the transaction protocol in a phantom index — `Db[Tx.No]` in,
`Db[Tx.Yes]` for the body, a nested region a compile error where
`transact` could only refuse at runtime. The full answer-type
threading stays here as the theory; the phantom form is what the
price/benefit analysis actually bought.

## References

- <a id="ref-atkey-2009"></a>Robert Atkey. *[Parameterised notions of computation.](https://bentnib.org/paramnotions-jfp.html)* JFP
  19(3–4):335–376, 2009. (The paper `Monad.scala` links.)
- <a id="ref-danvy-1989"></a>Olivier Danvy, Andrzej Filinski. *A functional abstraction of typed
  contexts.* DIKU report 89/12, 1989.
- <a id="ref-asai-2007"></a>Kenichi Asai, Yukiyoshi Kameyama. *[Polymorphic delimited
  continuations.](https://doi.org/10.1007/978-3-540-76637-7_16)* APLAS 2007.

---

← [2 · Continuations and delimited control](02-continuations.md) · [Contents](index.md) · [4 · Free and freer](04-free-freer.md) →
