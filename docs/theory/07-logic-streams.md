# 7. Logic, streams and sketches

Three smaller theories close the book — nondeterminism as a searchable
effect, streams as codata, and aggregation as algebra — each with its
own literature and each visible as one file.

## Nondeterminism, and the one primitive that tames it

Philip Wadler's "list of successes" [Wadler 1985] is the founding
observation: a nondeterministic computation *is* the lazy list of its
answers, and failure is the empty list. What the plain list cannot do
is search fairly or cut: interleave two infinite branches, commit to a
first answer, run an else-branch only when there is *no* answer.
Kiselyov, Shan, Friedman and Sabry's LogicT [Kiselyov, Shan, Friedman
& Sabry 2005] showed that one primitive restores all of it —
**msplit**, which observes a nondeterministic program as either
nothing, or its first answer *plus a program producing the rest*.

`Logic.scala` announces itself as exactly this, "LogicT … rebuilt on
Choose", and its header is the paper's table of contents restated:
`once` (the cut), `ifte` (the soft cut — negation-as-failure in one
line), `interleave` (the fair or), `>>-` (the fair bind — a productive
branch cannot starve its siblings), `observe` (first *n* answers of a
possibly infinite search), every one derived from `msplit`
(`Logic.scala:41`). The Okay twist is where the answers live:
alternatives are a `Seq`, "and a LazyList IS a Seq — so infinite choice
points cost nothing to construct" — the 1985 paper's data structure
serving the 2005 paper's operators.

## Streams as codata

A data type is what you can build; a **codata** type is what you can
observe. A stream is the canonical codata: its one observation is
`uncons` — the next element and the rest — and Okay's `Stream`
typeclass (`Stream.scala:5–10`) is that observation made *effectful*:
`uncons` answers in the stream's own effect `F`, so a pure generator, a
channel and a socket satisfy one interface. The fold/unfold duality
underneath — consumers are algebras, producers are coalgebras — is the
recursion-schemes tradition [Meijer, Fokkinga & Paterson 1991]; Okay
keeps both directions honest by *naming* them: `Fold[A, S]` is the
algebra (`init`/`add`), generators are unfolds built from chapter 2's
delimited control, and `Writer.uncons` (chapter 5's stream-with-result)
is the richer observation `Either[A, (W, rest)]` — codata "with the
answer carried at the end" (`Stream.scala:22`).

The tradition has names worth knowing. The recursion-schemes paper
gave the schemes their birds-and-bananas notation — a fold is a
**catamorphism**, an unfold an **anamorphism**, their composition a
hylomorphism — and Jeremy Gibbons' "origami programming" [Gibbons
2003] made the discipline explicit: write no explicit recursion;
express every traversal as a fold or an unfold, and the program's
structure becomes a theorem about it (fusion laws, deforestation). Okay
is origami in that sense wherever it streams: a `Chunks` pipeline is a
hylomorphism — an unfold at the source (`Chunks.generate`, `range`,
`fromIterator`), chunk-to-chunk arrows in the middle, a catamorphism
at the sink (`Chunks.fold`) — and the `Pipeline` optimizer of chapter
6 is the fusion laws applied as rewrites: map fusion IS the functor
law, filter fusion and take-pushdown are the fold-fusion family, each
property-tested rather than assumed.

The tradition has names worth knowing. The recursion-schemes paper
gave the schemes their birds-and-bananas notation — a fold is a
**catamorphism**, an unfold an **anamorphism**, their composition a
hylomorphism — and Jeremy Gibbons' "origami programming" [Gibbons
2003] made the discipline explicit: write no explicit recursion;
express every traversal as a fold or an unfold, and the program's
structure becomes a theorem about it (fusion laws, deforestation). Okay
is origami in that sense wherever it streams: a `Chunks` pipeline is a
hylomorphism — an unfold at the source (`Chunks.generate`, `range`,
`fromIterator`), chunk-to-chunk arrows in the middle, a catamorphism
at the sink (`Chunks.fold`) — and the `Pipeline` optimizer of chapter
6 is the fusion laws applied as rewrites: map fusion IS the functor
law, filter fusion and take-pushdown are the fold-fusion family, each
property-tested rather than assumed.

Two engineering notes the theory predicts and the benchmarks confirm.
Chunking (`Chunks[A] = Producer[Chunk[A]]`) amortizes the tree step of
chapter 4 over a batch, which is the whole arithmetic of the streaming
runtimes it is compared against. And the fold algebra being *first
order* — a start and a step, no combine — is what lets `Aggregator`
extend it with `merge` into Spark's `(zero, seqOp, combOp)` triple
exactly (`Aggregate.scala:7–9`), so one aggregator runs locally,
distributed, and as a `java.util.stream.Collector` unchanged.

## Sketches: approximation with stated error

Some aggregations are impossible exactly in bounded space — distinct
count, frequencies, quantiles — but each has a *sketch*: a small
summary that is wrong by a stated, bounded amount, and that **merges
associatively**, which is what makes it distributable. `Sketch.scala`
says it was "written fresh from the papers" and names them
(`Sketch.scala:13–14`): HyperLogLog for cardinality [Flajolet, Fusy,
Gandouet & Meunier 2007], Count-Min for frequencies [Cormode &
Muthukrishnan 2005], and the t-digest for quantiles [Dunning & Ertl
2019] — whose buffered, merge-then-compress shape "the one Dunning
describes" (`Sketch.scala:176`) replaced a per-element insertion
measured 580× slower (`docs/benchmarks.md` §13). Exact one-pass
statistics ride the same algebra: `variance` is Welford's update
[Welford 1962] merged by Chan–Golub–LeVeque [Chan, Golub & LeVeque
1983] (`Aggregate.scala:195–196`) — the merge form being, again, what
makes it chunk-parallel and distribution-safe.

The moral that joins the three sections: in each case the *algebraic
laws* (msplit's equations, uncons as final coalgebra, associative
merge) are not decoration on the API — they are the exact property the
distributed or infinite setting demands, and each file cites the paper
where that property was established.

## References

- Philip Wadler. *[How to replace failure by a list of successes.](https://doi.org/10.1007/3-540-15975-4_33)*
  FPCA 1985.
- Oleg Kiselyov, Chung-chieh Shan, Daniel P. Friedman, Amr Sabry.
  *[Backtracking, interleaving, and terminating monad transformers
  (functional pearl).](https://okmij.org/ftp/papers/LogicT.pdf)* ICFP 2005.
- Erik Meijer, Maarten Fokkinga, Ross Paterson. *[Functional
  programming with bananas, lenses, envelopes and barbed wire.](https://maartenfokkinga.github.io/utwente/mmf91m.pdf)*
  FPCA 1991.
- Jeremy Gibbons. *[Origami programming.](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/origami.pdf)* In The Fun of Programming,
  Palgrave, 2003.
- Philippe Flajolet, Éric Fusy, Olivier Gandouet, Frédéric Meunier.
  *[HyperLogLog: the analysis of a near-optimal cardinality estimation
  algorithm.](https://algo.inria.fr/flajolet/Publications/FlFuGaMe07.pdf)* AofA 2007.
- Graham Cormode, S. Muthukrishnan. *[An improved data stream summary:
  the count-min sketch and its applications.](https://doi.org/10.1016/j.jalgor.2003.12.001)* Journal of Algorithms
  55(1):58–75, 2005.
- Ted Dunning, Otmar Ertl. *[Computing extremely accurate quantiles
  using t-digests.](https://arxiv.org/abs/1902.04023)* arXiv:1902.04023, 2019.
- B. P. Welford. *[Note on a method for calculating corrected sums of
  squares and products.](https://doi.org/10.1080/00401706.1962.10490022)* Technometrics 4(3):419–420, 1962.
- Tony F. Chan, Gene H. Golub, Randall J. LeVeque. *[Algorithms for
  computing the sample variance: analysis and recommendations.](https://doi.org/10.1080/00031305.1983.10483115)* The
  American Statistician 37(3):242–247, 1983.
