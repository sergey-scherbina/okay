# Are these two programs the same program?

You rewrote a program: fused two stages, inlined a handler, moved a
filter in front of a map. The tests still pass on the inputs you
tried. `Bisim.check` asks a stronger question: **is there any handler
that could tell the two programs apart?**

## The idea

An okay program is a tree that stops at every operation and waits for
an answer. A handler sees nothing except those operations, and decides
nothing except the answers. So if two programs perform the same
operation, and then the same next operation for every answer, and
return equal values in the end, **no handler can tell them apart**.
This is normal-form bisimilarity, the equivalence Biernacki, Lenglet
and Polesiuk give for effects and handlers \[[FSCD 2020](#ref-bisim-2020)\],
and it is equality of interaction trees \[[Xia et al. 2020](#ref-itrees-2020)\].

`Bisim.check` walks both trees in lockstep. The one practical change is
that the answers come from a finite **sample** you give per operation,
instead of from every possible value:

```scala
trait Answers[F[+_]]:
  def apply[X](op: F[X]): List[X]
```

## A difference is found, with its path

```scala
given Answers[State % Int] = Answers.state(0, 1, 2)
val p = State.get[Int].flatMap(s => State.set(s))
val q = State.get[Int].flatMap(s => State.set(if s == 2 then 0 else s))
Bisim.check(p, q) match
  case Verdict.Differ(path, l, r) =>
    assertEquals(path, List("Get() -> 2"))
    assertEquals((l, r), ("performed Set(2)", "performed Set(0)"))
```

The two verdicts carry different weight:

- `Differ` is a **proof**. It gives the path, the operations and the
  answers that lead to the point where the programs part.
- `Same(paths, cut)` is **evidence**. Every sampled path agreed:
  `paths` of them ended, and `cut` were still running at the depth
  bound. The sample decides what is covered, so with the answers
  `(0, 1)` the pair above is `Same`. A `Same` over zero ended paths
  says nothing, which is why it counts.

## Rows of several effects

`+` combines answers for each member of a row. Each operation is
answered by the member whose runtime test accepts it, the same test a
runner uses:

```scala
given Answers[Gen.Row[Int]] = Answers.writer[Int] + Answers.stop
```

`Answers.stop` answers with nothing: `Stop.Now` is a leaf, so both
programs must stop at the same place.

## Laws and models: checking a rewrite

The generator stages (`map`, `filter`, `take`, `drop`, …) are checked
two ways in `TestBisim`, on the programs they materialise. A LAW
relates a stage to itself:

```scala
val v = Bisim.check(l.program, r.program, depth = 64)
law("map . map = map (f andThen g)")(src.map(f).map(g), src.map(f andThen g))
```

A MODEL relates a stage to what it means, the same operation on a
`List`:

```scala
model("take")(src.take(3), xs.take(3))
```

You need both. A law can pass when a stage is wrong in the same way on
both sides: `take` counting down by two keeps `take(5).take(3)` and
`take(3)` equal. Only the model fails in that case. Both checks were
measured against deliberate mutants of the library's own stage code
(specs/handler-equivalence-oracle.md, Results).

## Coherence: two roads to a wider row

A program at row `F` reaches `F + G` by a coercion, `!.widen`, or by a
walk that rebuilds the tree, `!.normalize`. What the program means must
not depend on which road it took. That property is coherence of effect
subtyping \[[Biernacki & Polesiuk 2018](#ref-coherence-2018)\]. A row
member is found by a runtime test on the operation's value (its class,
a `Tag`'s key, a `Writer.byValue` element class), so the two roads agree
exactly when neither changes an operation. `TestRowCoherence` states it
as a law over every core signature and over mixed rows, including two
`Tag` keys over one signature:

```scala
    Bisim.check(!.widen[A, F, Added](p), !.normalize[A, F, Added](p), depth) match
      case Verdict.Same(paths, _) => assert(paths > 0, s"$name: no sampled path ended")
    coherent("two keys", keyed, keyedAnswers)
```

A coercion that swapped the two keys fails it with a path: `Differ at
the start: left performed Tag(big,Get()), right performed
Tag(small,Get())`.

## What it does not check

`Bisim.check` compares programs across **all** handlers, so it does not
accept a rewrite that is valid only under one handler's equations.
`get; get` and `get` differ (a handler may answer the two gets
differently), even though `State.run` treats them the same. Check such
a rewrite AFTER that effect's handler, on the row that is left:

```scala
assert(!Bisim.check(twice, once).same, "a handler may answer the two gets differently")
same(Bisim.check(State.handle[Int](7)(twice), State.handle[Int](7)(once)), "under State.handle")
```

Operations are compared with `==`, so an operation that carries a
function (a `Delim` shift) cannot be compared yet.

## References

- <a id="ref-coherence-2018"></a>Dariusz Biernacki, Piotr Polesiuk.
  *Logical relations for coherence of effect subtyping.* Logical
  Methods in Computer Science, 2018 (first at TLCA 2015). Why a
  program's meaning must not depend on the derivation that widened its
  effects.

- <a id="ref-bisim-2020"></a>Dariusz Biernacki, Sergueï Lenglet, Piotr
  Polesiuk. *A complete normal-form bisimilarity for algebraic effects
  and handlers.* FSCD 2020, LIPIcs 167, 7:1–7:22.
- <a id="ref-itrees-2020"></a>Li-yao Xia, Yannick Zakowski, Paul He,
  Chung-Kil Hur, Gregory Malecha, Benjamin C. Pierce, Steve Zdancewic.
  *Interaction trees: representing recursive and impure programs in
  Coq.* POPL 2020.
