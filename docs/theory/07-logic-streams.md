# 7. Logic, streams and sketches

Three smaller theories close the book — nondeterminism as a searchable
effect, streams as codata, and aggregation as algebra — each with its
own literature and each visible as one file.

## Nondeterminism, and the one primitive that tames it

Philip Wadler's "list of successes" \[[Wadler 1985](#ref-wadler-1985)\] is the founding
observation: a nondeterministic computation *is* the lazy list of its
answers, and failure is the empty list. What the plain list cannot do
is search fairly or cut: interleave two infinite branches, commit to a
first answer, run an else-branch only when there is *no* answer.
Kiselyov, Shan, Friedman and Sabry's LogicT \[[Kiselyov, Shan, Friedman
& Sabry 2005](#ref-kiselyov-2005)\] showed that one primitive restores all of it —
**msplit**, which observes a nondeterministic program as either
nothing, or its first answer *plus a program producing the rest*.

`Logic.scala` announces itself as exactly this, "LogicT … rebuilt on
Choose", and its header is the paper's table of contents restated:
`cut`, `ifte` (the soft cut — negation-as-failure in one
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
recursion-schemes tradition \[[Meijer, Fokkinga & Paterson 1991](#ref-meijer-1991)\]; Okay
keeps both directions honest by *naming* them: `Fold[A, S]` is the
algebra (`init`/`add`), generators are unfolds built from chapter 2's
delimited control, and `Writer.uncons` (chapter 5's stream-with-result)
is the richer observation `Either[A, (W, rest)]` — codata "with the
answer carried at the end" (`Stream.scala:22`).

The tradition has names worth knowing. The recursion-schemes paper
gave the schemes their birds-and-bananas notation — a fold is a
**catamorphism**, an unfold an **anamorphism**, their composition a
hylomorphism — and Jeremy Gibbons' "origami programming" \[[Gibbons
2003](#ref-gibbons-2003)\] made the discipline explicit: write no explicit recursion;
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
Chunking (`Chunks[A] = Feed[Chunk[A]]`) amortizes the tree step of
chapter 4 over a batch, which is the whole arithmetic of the streaming
runtimes it is compared against. And the fold algebra being *first
order* — a start and a step, no combine — is what lets `Aggregator`
extend it with `merge` into Spark's `(zero, seqOp, combOp)` triple
exactly (`Aggregate.scala:7–9`), so one aggregator runs locally,
distributed, and as a `java.util.stream.Collector` unchanged.

A third note the theory also predicts, discovered by asking why the
amortization is not the whole answer: batching a representation
amortizes the STEP cost over a batch, and that only helps when a batch
exists to amortize over. Forced to batches of one — the shape a
genuinely per-element source (an LLM's tokens, one SSE event at a
time) hands any array-native representation — `Chunks.merge` costs
33x its own default-sized run (measured, chunk-size-representation and
chunks-size-one, 2026-09-03), because an array-of-chunks pays a chunk
allocation per *production*, independent of how many elements land in
it. The number that matters is what happens next: at that same forced
size, `Chunks` and `Source` (the Free-tree representation of chapters
4–5, no array anywhere) land within a few percent of each other —
780.7µs against 819.6µs on 2×2000 elements — while `ZStream`, ZIO's
one representation, forced the same way costs 9985µs, 12x worse than
either. The amortization is real, but it is a property of the BATCH,
not of any one representation; a system with only the batched
representation has nowhere to go when the batch collapses to one,
which is the concrete, measured reason this library keeps two
representations of a stream rather than one.

## Iteratees: the consumer as a program

The fold above is an algebra: a start and a step, and something else
walks the input and feeds it. Oleg Kiselyov's **iteratee**
\[[Kiselyov 2012](#ref-kiselyov-2012)\] turns the consumer inside out: it
is a *program* that asks for its next element, suspended until one
arrives, and the producer — his *enumerator* — is whatever answers the
ask. His motivation was lazy I/O's three failures (a handle held open
by a thunk nobody forced, an exception raised far from the read that
caused it, no way to stop reading early); the design that fixes them is
a consumer that owns its own control flow and knows nothing about
where elements come from. A transformer of iteratees, an
*enumeratee*, is then a consumer on one side and a producer on the
other, and the whole pipeline is composed from those three parts.

Okay has all three, under the names of chapter 5. `Take.Await`
(`Pipe.scala:22–28`) is the ask: a consumer is a program `B ! Take % W`.
The producer is chapter 5's stream-with-result, `A ! Writer % W` — a
`tell` is the dual of an `await`. `pipe(producer)(consumer)`
(`Pipe.scala:39–48`) is the pairing, and it is chapter 2's delimited
control doing the work: each `await` transfers control to the producer
for exactly one element, no queue and no buffer in between, the
consumer drives, and a finite consumer therefore ends an infinite
producer with only the asked elements ever computed. The enumeratee is
`Stage[I, O, A] = A ! Take % I + Writer % O` (`Pipe.scala:58`), a
program that awaits on one side and tells on the other; `through`
composes stages demand-driven, and `Stage.transduce(z)(step, end)` is
the state-step-flush skeleton that every stage in the library — the
lexer's scanner, SSE framing, `chunked`, the demo's stream join —
turned out to be an instance of; `Stage.transduceUntil` is the
enumeratee that may finish on its own (`Right`), the iteratee's early
`Done` at the stage level.

What the encoding buys is visible in the pair below. By hand, the
consumer owns the source, so it can be run against an `Iterator` and
nothing else; as an iteratee it owns only its questions, so the same
program runs against a file, a socket, or a test's list:

```scala
// by hand: the loop holds the source, and only an Iterator will do
def firstBlank(it: Iterator[String]): Int =
  var n = 0
  while it.hasNext do
    if it.next().isEmpty then return n
    n += 1
  n

// as an iteratee: a program that only knows how to ask
val firstBlank: Int ! Take % String =
  !.loop[Int, Int, Take % String](0) { n =>
    Take.await[String].map {
      case Some(s) if s.nonEmpty => Left(n + 1)
      case _                     => Right(n)
    }
  }

pipe(lines)(firstBlank)   // lines: Unit ! Writer % String — a file, a socket, a List
```

Two things differ from Kiselyov's Haskell. His iteratee is a monad
*transformer* over a base monad `m`, and every effect the consumer
performs — a log line, an async read — is lifted through it. Here
`Take` is one entry in an effect row, so a consumer that also logs and
sleeps is `B ! Take % W + Writer % String + Async` and lifts nothing;
`Writer.uncons`'s forwarding arm (`Writer.scala:340`) hands the other
effects out unchanged while it steps the tells. The law that pins the
interaction is stated in `TestFoldUntilStreams`: an effect the
producer performs *before* the element that satisfies the consumer is
performed, and one *after* it is not — counted, not read off the code.

The second difference is a specialisation the theory predicts. When
the consumer is a left fold with a stop — `find`, `take(n)`, "read
until the header has parsed" — the continuation is redundant: the
state already says what to do next. `FoldUntil[A, S, R]`
(`Fold.scala:222`) is that iteratee as *data*, `add`/`done`/`end` with
no `Bind` per element, and the same instance runs over every carrier
— `Stream.foldUntil`, `Chunks.foldUntil`, `Writer.foldUntil`,
`Source.runFoldUntil` — with `done` asked before the first element so
that `take(0)` pulls nothing. It is to the iteratee what `Fold` is to
a hand-written loop: the same consumer, with the recursion taken away
because it carried no information.

Its presentation has names too. Read as an automaton, `FoldUntil` is a
**Moore machine** \[[Moore 1956](#ref-moore-1956)\]: a state `S`, a
transition `add`, and outputs read OFF the state — `done: S =>
Boolean` and `end: S => R` — rather than a `Done` constructor answered
per element, which is exactly why it allocates nothing where the
iteratee's per-element answer is an object. Its `init`/`add`/`end`
triple is the "fold as a value" of the `foldl` tradition
\[[Gonzalez 2013](#ref-gonzalez-2013)\] with the halting predicate
added; the caller's own shape, a step answering `Either[S, R]`, is
`FoldUntil.until` over it. And its dual on the producing side is
`Source.unfold`, the anamorphism whose case for being a first-class
combinator Gibbons and Jones made in "The under-appreciated unfold"
\[[Gibbons & Jones 1998](#ref-gibbons-1998)\] — the same "state
decides when to stop" form that `!.loop` (chapter 4) gives a program
and `Proc.Iter` gives a free arrow.

The generator on the other side of `pipe` is the same coin. Kiselyov,
Peyton Jones and Sabry \[[Kiselyov, Peyton Jones & Sabry 2012](#ref-kiselyov-2012-yield)\]
showed `yield` — a producer that suspends after each element — to be
the dual of the iteratee's `await`, and that a lazy stream, a
generator and an iteratee are three surfaces of one control transfer.
Here that is literal: `Writer.tell` is `yield`, `Take.await` is the
iteratee's ask, `pipe` is the transfer, and chapter 8's direct blocks
let a generator be written as a loop that tells — and, since
direct-loops v3, a *consumer* as a loop that reads: `for x <-
Take.each[I] do tell(f(x)).!?` inside a block is a `Stage` written
straight, and `for x <- Pull.of(s) do …` reads any `Stream` carrier
the same way, one `step` bound per element (docs/direct-style.md,
"A loop over a source"). The iteratee side of a direct block is no
longer spelled `!.loop`; `Pull.loop(f)` is that program by name for
the places a block is not.

The generator now has its name. `Gen[W]` (Gen.scala; specs/generators.md)
is a value class over `Unit ! Writer % W + Stop` — the program that
tells, with `Stop` as one more member of the row for the early end —
and a `generator[W] { … }` block (okay-direct) is a direct block over
that row in which `for … yield` emits. Two papers fix what the words
mean. Python's PEP 255 \[[Schemenauer, Peters & Hetland 2001](#ref-pep-255)\]
gave the everyday semantics — a `yield` suspends the body, the body
runs again only when the next value is asked for, and the generator
ends when the body returns, is closed, or is left — and James and
Sabry \[[James & Sabry 2011](#ref-james-2011)\] showed that this
`yield` IS a delimited continuation: the operator captures the rest of
the body up to the generator's boundary, and a reader that stops is a
continuation dropped. Here that is the implementation, not an analogy:
`Gen.iterator` hands out the `w` of `Bind(Inject(Say(w)), k)` and holds
`k`, applying it on the NEXT `next()` — so the code between two yields
runs when the second value is asked for — and every stopping reader
is a `FoldUntil` whose law is that `k` is not called once it is done.
The first cut applied `k` eagerly and was one yield ahead of Python; a
step counter caught it. A chain of `map`/`filter`/`take` over a `Gen`
is not three walks: the stages are DATA — a transducer in Hickey's
sense \[[Hickey 2014](#ref-hickey-2014)\], a transformer of the
reader, with the state each stage adds carried as a type member
(`Xf.St[S]`: `take` wraps `(Int, S)`, `map` adds nothing) — and a
stopping reader walks the source once, applying every stage inside
`add` — `flatMap` and `++` included, an inner generator read from
where the reader stands; `program` materialises the same chain as the
walks when a road needs a program (specs/gen-chain-fusion.md).

```scala
enum Tree { case Leaf(v: Int); case Node(l: Tree, r: Tree) }
import Tree.*

def leaves(t: Tree): Gen[Int] = generator[Int] {     // chapter 2's tree walk, as a Gen
  t match
    case Leaf(v)    => Gen.emit(v).!?
    case Node(l, r) => leaves(l).!?; leaves(r).!?
}
val it = leaves(Node(Node(Leaf(1), Leaf(2)), Leaf(3))).iterator
it.next()                                            // 1 — the walk ran to its first leaf and holds the rest
```

The enumeratee now has a JDK name. Java 24's stream **gatherers**
\[[Klang 2024](#ref-klang-2024)\] are user-defined intermediate
operations of four parts — an initializer, an integrator
`(state, element, downstream) → boolean`, an optional combiner, a
finisher — and they are the enumeratee written the other way round:
*pushed* one element at a time by the stream, where an iteratee
*asks*. That push form is the one Hickey's Clojure **transducers**
\[[Hickey 2014](#ref-hickey-2014)\] made popular — a transformation
of the reducing step, with early termination (`reduced`) and a
completion arity for the flush — and a gatherer's `false` and its
finisher are exactly those two. Because okay's `Stage` is a program,
the two forms translate by pure mechanics: the gatherer's state is
the stage *suspended at its next* `await`, the integrator resumes it
with the element and pushes whatever it `tell`s on the way to the next
`await`, and a stage that answers is an integrator returning `false`
(okay-java's `Gather`, both directions, law-tested against each
other). What neither side can give the other is a combiner: a
suspended program is a position in the stream, and positions do not
merge — which JEP 485 accommodates by running a combiner-less gatherer
sequentially even inside a parallel stream. And because the
transducer is the same enumeratee, the translation goes to Clojure
too: okay-clojure's `Transducers` makes a stage a transducer and a
transducer a stage, the stage's answer being `reduced` and its last
tells the completion arity, law-tested against Clojure's own `into`.

Frege, a Haskell for the JVM, shows the other half of the chapter's
argument. The tempting bridge there is lazy IO — a pure `[a] -> [b]` fed
a list whose thunks pull from okay — and it fails exactly as the iteratee
paper says lazy IO fails: the function, not the consumer, decides when
to read, and a forced thunk cannot suspend. Haskell's own answer to
effects in a lazy language was to make them a MONAD rather than an
evaluation order \[[Peyton Jones & Wadler 1993](#ref-pj-wadler-1993)\],
and okay-frege takes it literally: the okay tree is written in Frege
(`okay.frege.Prog`, a freer monad over `await`, `tell`, `perform` and
`liftIO`), okay's handlers interpret it, and because its continuations
are Frege functions a multi-shot handler resumes them per branch — the
freer construction \[[Kiselyov & Ishii 2015](#ref-kiselyov-ishii-2015)\]
crossing a language boundary with nothing but data.

## Sketches: approximation with stated error

Some aggregations are impossible exactly in bounded space — distinct
count, frequencies, quantiles — but each has a *sketch*: a small
summary that is wrong by a stated, bounded amount, and that **merges
associatively**, which is what makes it distributable. `Sketch.scala`
says it was "written fresh from the papers" and names them
(`Sketch.scala:13–14`): HyperLogLog for cardinality \[[Flajolet, Fusy,
Gandouet & Meunier 2007](#ref-flajolet-2007)\], Count-Min for frequencies \[[Cormode &
Muthukrishnan 2005](#ref-cormode-2003)\], and the t-digest for quantiles \[[Dunning & Ertl
2019](#ref-dunning-1902)\] — whose buffered, merge-then-compress shape "the one Dunning
describes" (`Sketch.scala:176`) replaced a per-element insertion
measured 580× slower (`docs/benchmarks.md` §13). Exact one-pass
statistics ride the same algebra: `variance` is Welford's update
\[[Welford 1962](#ref-welford-1962)\] merged by Chan–Golub–LeVeque \[[Chan, Golub & LeVeque
1983](#ref-chan-1983)\] (`Aggregate.scala:195–196`) — the merge form being, again, what
makes it chunk-parallel and distribution-safe.

The moral that joins the three sections: in each case the *algebraic
laws* (msplit's equations, uncons as final coalgebra, associative
merge) are not decoration on the API — they are the exact property the
distributed or infinite setting demands, and each file cites the paper
where that property was established.

## References

- <a id="ref-wadler-1985"></a>Philip Wadler. *[How to replace failure by a list of successes.](https://doi.org/10.1007/3-540-15975-4_33)*
  FPCA 1985.
- <a id="ref-kiselyov-2005"></a>Oleg Kiselyov, Chung-chieh Shan, Daniel P. Friedman, Amr Sabry.
  *[Backtracking, interleaving, and terminating monad transformers
  (functional pearl).](https://okmij.org/ftp/papers/LogicT.pdf)* ICFP 2005.
- <a id="ref-kiselyov-2012"></a>Oleg Kiselyov. *[Iteratees.](https://doi.org/10.1007/978-3-642-29822-6_15)*
  FLOPS 2012, LNCS 7294.
- <a id="ref-kiselyov-2012-yield"></a>Oleg Kiselyov, Simon Peyton Jones, Amr Sabry. *[Lazy v. Yield:
  incremental, linear pretty-printing.](https://doi.org/10.1007/978-3-642-35182-2_14)* APLAS 2012, LNCS 7705.
- <a id="ref-pep-255"></a>Neil Schemenauer, Tim Peters, Magnus Lie Hetland. *[PEP 255 — Simple Generators.](https://peps.python.org/pep-0255/)*
  Python Enhancement Proposals, 2001.
- <a id="ref-james-2011"></a>Roshan P. James, Amr Sabry. *[Yield: mainstream delimited continuations.](https://legacy.cs.indiana.edu/~sabry/papers/yield.pdf)*
  Theory and Practice of Delimited Continuations (TPDC) 2011.
- <a id="ref-pj-wadler-1993"></a>Simon Peyton Jones, Philip Wadler. *[Imperative functional programming.](https://doi.org/10.1145/158511.158524)*
  POPL 1993.
- <a id="ref-kiselyov-ishii-2015"></a>Oleg Kiselyov, Hiromi Ishii. *[Freer monads, more extensible effects.](https://doi.org/10.1145/2804302.2804319)*
  Haskell 2015.
- <a id="ref-klang-2024"></a>Viktor Klang. *[JEP 485: Stream Gatherers.](https://openjdk.org/jeps/485)*
  OpenJDK, final in JDK 24 (2024; previews JEP 461, 473).
- <a id="ref-hickey-2014"></a>Rich Hickey. *[Transducers are coming.](https://clojure.org/news/2014/08/06/transducers-are-coming)*
  Clojure news, 2014; and the talk *Transducers*, Strange Loop 2014.
- <a id="ref-meijer-1991"></a>Erik Meijer, Maarten Fokkinga, Ross Paterson. *[Functional
  programming with bananas, lenses, envelopes and barbed wire.](https://maartenfokkinga.github.io/utwente/mmf91m.pdf)*
  FPCA 1991.
- <a id="ref-gibbons-2003"></a>Jeremy Gibbons. *[Origami programming.](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/origami.pdf)* In The Fun of Programming,
  Palgrave, 2003.
- <a id="ref-gibbons-1998"></a>Jeremy Gibbons, Geraint Jones. *[The under-appreciated unfold.](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/unfold.ps.gz)*
  ICFP 1998.
- <a id="ref-moore-1956"></a>Edward F. Moore. *[Gedanken-experiments on sequential machines.](https://doi.org/10.1515/9781400882618-006)*
  In Automata Studies, Princeton, 1956.
- <a id="ref-gonzalez-2013"></a>Gabriel Gonzalez. *[Composable, streaming, and efficient left folds.](https://www.haskellforall.com/2013/08/composable-streaming-and-efficient-left.html)*
  2013 (the `foldl` library).
- <a id="ref-flajolet-2007"></a>Philippe Flajolet, Éric Fusy, Olivier Gandouet, Frédéric Meunier.
  *[HyperLogLog: the analysis of a near-optimal cardinality estimation
  algorithm.](https://algo.inria.fr/flajolet/Publications/FlFuGaMe07.pdf)* AofA 2007.
- <a id="ref-cormode-2003"></a>Graham Cormode, S. Muthukrishnan. *[An improved data stream summary:
  the count-min sketch and its applications.](https://doi.org/10.1016/j.jalgor.2003.12.001)* Journal of Algorithms
  55(1):58–75, 2005.
- <a id="ref-dunning-1902"></a>Ted Dunning, Otmar Ertl. *[Computing extremely accurate quantiles
  using t-digests.](https://arxiv.org/abs/1902.04023)* arXiv:1902.04023, 2019.
- <a id="ref-welford-1962"></a>B. P. Welford. *[Note on a method for calculating corrected sums of
  squares and products.](https://doi.org/10.1080/00401706.1962.10490022)* Technometrics 4(3):419–420, 1962.
- <a id="ref-chan-1983"></a>Tony F. Chan, Gene H. Golub, Randall J. LeVeque. *[Algorithms for
  computing the sample variance: analysis and recommendations.](https://doi.org/10.1080/00031305.1983.10483115)* The
  American Statistician 37(3):242–247, 1983.

---

← [12 · Applicative, Selective, Monad](12-applicative-static.md) · [Contents](index.md)

**A fourth engineering note: a batched API over an unbatched
primitive.** The third note said amortization is a property of the
batch, not of a representation. There is a sharper form of the same
mistake, and it is harder to see because the batch is real. Our
chunked receive genuinely delivered batches — an average of 13.4
elements, 4000 handshakes reduced to 299 — and bought 4%. Everything
the batch touched did get cheaper by 13x; the batch simply did not
touch the expensive part, because the bulk receive was a loop over the
single receive, and the ring's per-element CAS survived inside it. An
amortization argument is only as good as the inventory of what is
paid per element. Counting the batch is not the same as counting the
work.
