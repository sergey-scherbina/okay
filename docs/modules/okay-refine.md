# okay-refine

> Reading anything out of anything, one pattern at a time: a typed
> hierarchy of patterns that recognise a document level by level —
> bytes, format, document, instrument — where a pattern is a prism
> whose read may decline, a path reads and writes back, and a choice
> runs every alternative and says `Unclear` rather than first-wins.

Depends on: `okay` (core), `okay-codec`, `okay-optics`. Pure Scala —
cross-built for JVM, JS and Native. Spec: `specs/refine.md`.

## Guide

**The problem it is for.** A file arrives and nothing is known about
it: not its format, not what it describes, not which of the forty
shapes of "an interest-rate swap" it is. Every question about it is
the same question at a different level — *is this one of these?* — and
every answer makes the next question askable. A parser for a
programming language does this over a grammar fixed in advance; here
the grammar is the sum of every pattern anyone has registered, and it
grows without touching what was there. The design bet, from a bank
project that failed at this twice for cognitive load: **the patterns
are the components.** Each is a value; the hierarchy is their
composition; the document model is what the composition produces.

**A pattern is a prism.** `Refine.step(name)(read)(write)`: `read`
is partial and says why it declines, `write` is total. Composing along
a path (`andThen`) reads *and* writes back, so a document read through
one branch and written through another is a conversion — lawful, for
free. `<|>` is a choice at one level.

**The answer is a `Verdict`, never a bare value.** `Took(value, path,
declined)` names the path of patterns that produced it and every
sibling that declined with its own reason; `Unclear(candidates, …)`
when two branches took the input; `Declined(tried)` with every path
and why. A hierarchy's whole worth over a hand-written `match` is
that the reader sees what was considered — for a risk system, a
silent first-wins is the defect this module exists to remove.

## Tutorial

Three steps, a path and a choice:

```scala
val int   = Refine.step[String, Int]("int")(s => s.toIntOption.toRight(s"'$s' is not an integer"))(_.toString)
val even  = Refine.step[Int, Int]("even")(n => if n % 2 == 0 then Right(n) else Left(s"$n is odd"))(identity)
val small = Refine.step[Int, Int]("small")(n => if n < 10 then Right(n) else Left(s"$n is not small"))(identity)
val evenOrSmall = int andThen (even <|> small)
val seven = evenOrSmall.run("7")    // Took(7, int/small, Vector(int/even: 7 is odd))
val four  = evenOrSmall.run("4")    // Unclear(Vector((int/even, 4), (int/small, 4)), Vector())
val bad   = evenOrSmall.run("x")    // Declined(Vector(int: 'x' is not an integer))
val seven2 = evenOrSmall.write(7)   // Right("7")
```

The first level shipped here is FORMAT — cbor, json, xml, yaml over
the codecs' own lossless trees, so a detector cannot drift from the
parser it stands for:

```scala
val verdict = Format.detect.run("""{"a": [1, 2]}""".getBytes(UTF_8))
val path = verdict match
  case Verdict.Took(_, by, _) => by.toString               // "text/json"
  case other => other.toString
val tried = verdict.reasons.map(_.at.toString)              // Vector("cbor", "text/xml", "text/yaml")
val back = verdict.toOption.flatMap(doc => Format.detect.write(doc).toOption)
val text = back.map(new String(_, UTF_8))                   // Some("""{"a": [1, 2]}""")
```

Into a sum: each alternative learns one case, `write` takes that case
only, and an `Or` asks its alternatives in order:

```scala
val num: Refine[String, AnyVal] = int.widen[AnyVal] <|> decimal.widen[AnyVal]
val half = num.run("4.5").toOption     // Some(4.5)
val i42  = num.write(42)               // Right("42")
val no   = num.write(true)             // Left("int|decimal: no alternative writes this value")
```

The DOCUMENT level (stage 2): a derived `Schema` is a pattern, so
bytes → format → value → instrument is one path; read through YAML and
written back through the same path it comes out as JSON, which is what
makes a path a conversion; and `search` makes any pattern a `Choose`
program, so `Logic.ifte` writes "if this is a swap then … else …":

```scala
final case class Swap(id: String, notional: Double, fixedRate: Double)
given Schema[Swap] = Schema.derived
val swap: Refine[Json, Swap] = Refine.schema[Swap]("swap")
val fromBytes: Refine[Array[Byte], Swap] = Format.detect andThen Format.value andThen swap
val read = fromBytes.run("id: s1\nnotional: 1000000.0\nfixedRate: 0.03\n".getBytes(UTF_8))
val where = read match
  case Verdict.Took(_, by, _) => by.toString                // "text/yaml/value/swap"
  case other => other.toString
val asJson = fromBytes.write(Swap("s1", 1000000.0, 0.03)).map(new String(_, UTF_8))
// Right("{\"id\":\"s1\",\"notional\":1000000,\"fixedRate\":0.03}") — read from YAML, written as JSON: a conversion
val readings = !.run(runChoice[Swap, okay.Pure](swap.search(Json.parse("""{"id": "s1", "notional": 1.0, "fixedRate": 0.03}"""))))
// Seq(Swap("s1", 1.0, 0.03)) — a pattern is a search: Unclear is a choice point, Declined an empty one
```

The public prover — ISDA's two FpML 5.10 examples read end to end —
lives in the module's tests (`okay.refine.fpml`, `TestFpmlProver`): a
vanilla swap and an FX forward, `text/xml/value/dataDocument/trade/swap`
and `…/fxForward`, each verdict naming the other branch's refusal. The
patterns are the SHAPE of domain work, not the domain: every further
product and version is a private module's.

## Writing a document level

What a whole domain built on this looks like — the private repository
that reads ISDA's FpML and CDM did it in a day and this is its shape,
recorded here because the shape is the mechanism's, not the domain's:

- **One pattern per document element, named after it.** A product is
  `traded(element, terms)`: a step that reads the trade's header, hands
  `terms` the product ELEMENT, and is named `element` — so the verdict's
  path reads `dataDocument/trade/swap` and every refusal names the
  element it was about. The terms of an element are their own pattern,
  REUSED where the element recurs (a swaption's underlying `swap`, a
  credit option's `creditDefaultSwap`).
- **The smallest value that says what the document is.** A swap is its
  legs; a leg is a notional and a rate or an index. Fields are added the
  day a document in hand needs them, never ahead: `Option` where a
  document may not say, a refusal in the element's name where it must.
- **`req` and `opt`.** `req(step, json, what)` turns a step's refusal
  into "no `what` (its reason)"; `opt` reads an optional part, and
  `None` is not a refusal. Every product is a `for` over these.
- **The registry is `Refine.first(a, b, c, …)`.** One more product is
  one more alternative, in order, nothing existing edited — and the
  alternatives at one level must be DISJOINT by construction, because
  every one of them runs. The corpus run below is what shows when they
  are not: the one `Unclear` that domain ever met was a named basket
  taken by both a `basket` and an unnamed-`pool` alternative, fixed by
  making `pool` decline a name.
- **Events beside instruments.** A message that carries no trade — a
  termination naming the trade by id — is not a product; it is a
  second level `<|>`-ed beside the first, disjoint because a
  termination that carries its trade is the instrument's.

**The corpus method.** Vendor the standard's own published examples,
verbatim, pinned to a commit, with the licence beside them; write ONE
test that reads every file through the whole path and prints the table
— took / declined by document element, and for an element you have a
pattern for, why it still declined — and asserts `declined == 0` once
that is true. Then the table is the measure of coverage, every new
pattern's effect is a number, and a document that stops reading is a
regression with its reasons in the diagnosis. On 801 FpML documents the
first run read 123, the same evening 801; two of the refusals on the
way were defects of okay's XML dialect that no unit test had seen — the
XML declaration read as an unclosed tag, HTML's void elements applied
to XML — and both are landed here, because a domain corpus is the
sharpest test a dialect gets.

**The cross-format law.** When a second format describes the same
things (CDM's `TradeState` JSON beside FpML's XML), read it to the SAME
value type, and hold `readB.run(docB) == readA.run(docA)` on the pairs
the standard itself publishes. It is the prism law one level up: a
document read through one level and written through the other is a
conversion, and the law says the conversion loses nothing that the
value keeps.

**What lives where.** A combinator, a Json step, a dialect fix, a
verdict shape — here. What a swap IS — in the domain's repository.
The test of the boundary: a file that mentions no term of the domain
belongs here.

## The algebra: glyphs, classes, effects, streams

**Two names each, and one new combinator.** `>>>` is `andThen`; `or` is
`<|>` — every alternative runs, two takers are `Unclear`. `orElse` is NOT
another name for `<|>`: it keeps the meaning Scala gives the word in
Option, Either and PartialFunction — the second pattern is consulted only
when the first DECLINES, so it can never make the answer `Unclear`, and
the refusal that sent the read to the fallback stays in the verdict:

```scala
assert((even or small).run(4).isInstanceOf[Verdict.Unclear[?]])
assertEquals((even orElse counting).run(4), Verdict.Took(4, Path("even"), Vector.empty))
assertEquals((even orElse counting).run(7), Verdict.Took(7, Path("small"), Vector(Refusal(Path("even"), "7 is odd"))))
```

**What a pattern IS an instance of**, each law run by
`TestRefineAlgebra` on Took, Unclear and Declined inputs:

- a **category** — `Refine.id` (which adds no name to the path, so
  `id >>> r` answers exactly what `r` does) and `>>>`, associative;
  `given Refine.category` is okay-optics' `Optic.Category`;
- a **monoid under `or`**, and another **under `orElse`**, both with
  `Refine.empty` (declines everything, writes nothing) as the unit;
- an **invariant functor** — `map(name)(to, from)` is `imap`: a pattern
  can only learn a new type it can also write back;
- **monoidal, two ways** — `***` runs two patterns on the halves of a
  pair, `+++` on the two sides of an `Either`; and `and` reads a RECORD
  from one input and writes it back by merging the two skeletons
  (`Refine.Merge`, given for `Json`) — Rendel and Ostermann's
  `ProductFunctor`, for trees:

```scala
val money = (field("amount") >>> num) and (field("currency") >>> str)
assertEquals(money.run(j), Verdict.Took((5.0, "EUR"), Path("amount", "number", "currency", "string"), Vector.empty))
assertEquals(money.write((5.0, "EUR")).map(Json.print), Right("""{"amount":5,"currency":"EUR"}"""))
```

**The category's fold, named.** `Refine.path(a, b, c)` is `a >>> b >>> c`
for steps that keep one type, and `Refine.path()` is `id`;
`Refine.json.at(…)` is its commonest case, a descent through fields,
each a step of the verdict's path:

```scala
same(Refine.path(even, double, half), even >>> double >>> half, ints, ints)
assertEquals(at("a", "b").write(Json.JStr("x")).map(Json.print), Right("""{"a":{"b":"x"}}"""))
```

**What it cannot be, and why** — each is the way back refusing:

- not a `Functor`, `Applicative` or `Monad`: `B` is the read's OUTPUT and
  the write's INPUT, so a plain `B => C` has no way back; `pure(b)` has no
  input to write `b` into; and a `flatMap` choosing the next pattern from
  the value read leaves the write not knowing which pattern to write
  through;
- not a `Profunctor` (so not `Strong`/`Choice` in the optics sense): `A`
  is the read's input and the write's output — the same invariance on the
  other side;
- not an `Arrow` or `ArrowChoice`: `arr(f)` would need a way back for an
  arbitrary function. A pattern is a PARTIAL ISOMORPHISM — a category with
  products and sums, and no `arr`;
- `or` is not commutative: the readings and refusals come back in the
  order written, though whether the answer is `Unclear` does not depend on
  it.

The read half on its own IS a Kleisli arrow — of the `Verdict` monad
(readings with a log of refusals) — which is why `>>>` is associative.

**Effects.** A read is pure on purpose: a read that performed effects
could not be written back or replayed. A pattern enters a program three
ways — `run` inside it (a value), `orRaise` (the value, or the whole
non-`Took` verdict through `Throws`, handed back by `runEither`), and
`search` (a `Choose` program: `Unclear` is a choice point, `Declined` an
empty one):

```scala
val both = for a <- int.orRaise("4"); b <- int.orRaise("five") yield a + b
assertEquals(readings("4"), Seq(4, 4))
```

**Streams.** `verdicts` is a `Stage` with one verdict per input and
nothing dropped; `taken` emits only the values and ANSWERS what it did
not take, so a stream that read nine of ten documents cannot pass for one
that read ten:

```scala
assertEquals(values, Seq(7, 200))
assertEquals(missed, Refine.Missed(declined = 2, unclear = 1))
```

## Routing: one stream in, a stream per kind out

A `Router` is a value — a pattern and a routing table, read top to
bottom like a `match` — and `run` sends every document of a `Source` to
the channel of its kind:

```scala
val routed = Router(any)
.route[Swap](swaps)
.route[Fx](fxs)
.route { case c: Cds if c.ccy == "EUR" => c }(eurCds)
.otherwise(rejected)
.run(Source(docs*)).runWith
assertEquals(routed, Router.Routed(Vector("Swap" -> 2, "Fx" -> 2, "case #3" -> 1), rejected = 2))
```

- `route[X](channel)` takes every recognised value of type `X` — a class,
  a case, or a union: `route[Swap | Cds](rates)` takes exactly swaps and
  CDSs. The test is the compiler's `TypeTest`; a `ClassTag` would have
  been the union's least upper bound and taken every sibling as well
  (found by the first run of TestRouter).
- `route { case … }(channel)` routes by pattern matching — a guard, a
  union of cases, a projection to another type.
- `byName("fxForward")(channel)` routes by the pattern that TOOK the
  document, for kinds that share a value type.
- `tap(channel)` gets a copy of everything recognised; `routeAs(name,
  channel)` names a route for the `Routed` count.
- `otherwise(channel)` gets every `Rejected(input, verdict, why)`: a
  document that declined, one that was `Unclear` (never routed — the
  pattern could not decide what it is), and a recognised value no rule
  fits ("no route for …"). Without an `otherwise` they are still counted.

The first rule that fits wins, as in a `match`: the TABLE is the
author's, written in order. The RECOGNITION is not — that is still the
pattern's `Verdict`, and an `Unclear` document is rejected, not routed by
whichever rule comes first. When the input ends, `run` closes every
channel it was given, once each (two rules may share one); when the
input fails, it fails them with the same error, so no consumer waits for
a stream that will not come. The capacity of the channels is the
backpressure policy: a bounded one slows the router, and every route
with it.

`decide(a)` says where one document would go without running anything,
and `r.routed(key)` is the synchronous twin — one `Stage`, each element
tagged with its key, the rest `Left` with why:

```scala
assertEquals(r.decide("fx:f1,EURUSD"), Right("Fx"))
assertEquals(out.collect { case Right((k, _)) => k }, Seq("Swap", "Fx", "Cds", "Cds", "Swap", "Fx"))
```

## Routes: one table, any platform

`Router` binds each rule to a channel as it is written. `Routes` keeps
the TABLE apart from where its results go: a table is an `object` whose
LANES are typed handles, declared like the cases of a `match`, and the
same table runs over any `Bulk` — `Chunks` in one JVM, `SparkBulk` on a
cluster — or into channels:

```scala
object Kinds extends Routes(fromBytes):
val swaps = route[Swap]
val rates = route[Fx | Cds]
val usdSwaps = route("usdSwaps") { case s: Swap if s.ccy == "USD" => s }
```

`split` runs the table over ANY CARRIER that has a `Routable` — the
typeclass of what can be routed: a `Vector`, any `Bulk` collection
(`Chunks` in one JVM, `SparkBulk`'s rows on a cluster), a `Source`
stream. It asks exactly what routing needs — every input tagged once,
each lane's values handed out as the carrier's own kind, the counts — so
the table is written once and one call routes every carrier:

```scala
val v = Kinds.split(inputs)
val c = Kinds.split(localBulk.of(inputs))
val s = Kinds.split(Source(inputs*))
assertEquals(all(c(Kinds.swaps)), swapsV)
assertEquals(swapsS, swapsV)
assertEquals(routed, v.counts)
```

Over a `Bulk`, each document is recognised once and CACHED; a lane is a
filter on an `Int`, and the counts are one aggregate. `Documents.files`
reads a directory as (name, bytes), one split per file, wherever the
split lands. On Spark it is the same call — TestSparkRoutes runs 400
documents through `local[4]` and one JVM and asserts they agree:

```scala
val s = { given Bulk[Rows] = onSpark; Kinds.split(onSpark.read(folder, Documents.files)) }
assertEquals(s.counts, l.counts)
```

Over a `Source`, read once, every lane is a channel and `counts` is the
PROGRAM that reads, tags and sends; unbounded lanes (the default) let it
run first, and `Routable.stream(capacity)` bounds them, when the readers
must run beside it:

```scala
val s = Kinds.split(Source(inputs*))(using Routable.stream(capacity = 4))
Async.par(s.counts, s(Kinds.swaps).runCollect),
```

What `out(lane)` does, per carrier: the input was tagged ONCE, by
`split` (a Vector: at once; a `Bulk`: at the first action, then cached;
a `Source`: while `counts` runs), and a lane only takes its share of that
tagging and types it — the pattern is never run again. A Vector's lanes
are grouped on first use, so a lane is a lookup. A `Bulk`'s tagging stays
pinned (on Spark, persisted) until `out.release()`. A STREAM's lane is a
channel with one reader, so it is read ONCE; a second run is refused by
name rather than left to split the channel's elements between two
readers:

```scala
val again = intercept[IllegalStateException](lane.runCollect.runWith)
s.release()
```

A new carrier — a Kafka topic, a Flink stream — is one more
`Routable` instance (`fan`, `select`, `done`), and no table changes. The
typeclass is on the carrier VALUE, `Routable[Source[A]]`, not on a type
constructor, so an alias (`Source`, `Chunks`) or an opaque type
(`SparkBulk.Rows`) is found by the type as written.

Into channels, a lane binds with `~>`; a lane left unbound rejects its
values as "not bound here", so a partial binding drops nothing:

```scala
val r = Kinds.run(src)(Kinds.swaps ~> swapsCh, Kinds.rejected ~> dead).runWith
```

A pattern is `Serializable` (so is a `Merge`, and a lane — a `TypeTest`
is too, so `route[Fx | Cds]`'s exact test travels to an executor);
declare the table as an `object`, which an executor re-creates by
reference instead of copying.

## API reference

| | |
|---|---|
| `Refine.step(name)(read)(write)` | a pattern: `A => Either[String, B]` and `B => A` |
| `r andThen s` | a path; the verdict's path is both names |
| `r <\|> s`, `Refine.first(a, b, …)` | a choice; every alternative runs |
| `r.map(name)(to, from)` | an iso on what is learnt |
| `r.widen[C]` | into a sum; `write` accepts this branch's case only |
| `r.run(a): Verdict[B]` | `Took` / `Unclear` / `Declined`, each with its `Refusal`s |
| `r.write(b): Either[String, A]` | the way back along the path |
| `Refine.Step(…).prism` | the step as an optics `Prism`, for the laws |
| `Refine.schema[A](name)` | a derived `Schema[A]` as a `Refine[Json, A]`: decode declines in the codec's words, encode writes |
| `r.search(a): B ! Choose` | the pattern as a search: Took one answer, Unclear a choice point, Declined an empty one |
| `r >>> s`, `r or s` | `andThen` and `<\|>` by other names |
| `Refine.path(steps*)`, `Refine.json.at(names*)` | the fold of `>>>` over same-typed steps (`id` when empty); a descent through fields |
| `r orElse s` | a fallback: `s` only when `r` declines; never `Unclear` from `s` |
| `Refine.id`, `Refine.empty` | the category's unit (no name in the path); the unit of `or` and `orElse` |
| `r *** s`, `r +++ s` | on the halves of a pair; on the sides of an `Either` |
| `r and s` | a record: both over one input, written back through `Refine.Merge` |
| `r.orRaise(a): B ! Throws % Verdict[B]` | the read as an effect |
| `r.verdicts`, `r.taken` | `Stage`s: every verdict; the values, answering `Missed(declined, unclear)` |
| `Router(r).route[X](c)`, `.route { case … }(c)`, `.byName(n)(c)`, `.tap(c)`, `.otherwise(c)`, `.run(source)` | routing: a stream per kind; `Routed` counts; every channel closed (or failed) at the end |
| `r.routed(key)` | the synchronous twin: one `Stage` of `Either[Rejected, (K, B)]` |
| `object T extends Routes(r)`: `route[X]`, `route(name){ case … }`, `byName`, `routeAs` | a routing table as a value; lanes are typed handles |
| `T.split(c)`: `out(lane)`, `out.rejected`, `out.counts` | the table over any `Routable` carrier: Vector, Bulk (Chunks, SparkBulk), Source |
| `Routable[C]`: `fan`, `select`, `done`; `Routable.stream(capacity)` | what can be routed; a new carrier is one instance |
| `out.release()` | let go of the tagging (Spark: unpersist); a stream lane reads once |
| `T.run(source)(lane ~> c, T.rejected ~> c)` | the same table into channels |
| `Documents.files` (JVM) | a directory of whole files as a `Bulk.Format` |
| `Format.value` | `Refine[Doc, Json]`: JSON, YAML and XML (`Xml.value`: elements as objects, `@attr`, repeats as arrays) project to a value, CBOR declines; writes JSON — for XML text, `Xml.fromValue` on the written value |
| `Refine.json.field(name)`, `.str`, `.num`, `.each(name)` | the steps a document-level pattern is written in; a path of them names itself in the verdict |
| `Format.detect` | `Refine[Array[Byte], Doc]`: `cbor <\|> (text andThen (json <\|> xml <\|> yaml))` |
| `Doc.Json / Xml / Yaml / Cbor` | the detected document, as the dialect's own tree (or the bytes) |

## Gotchas

- **A dialect declines by its first character before it parses.** JSON
  needs `{` or `[`, XML `<`, the block YAML dialect anything but `{`, `[`,
  `<?`, `<!` — necessary conditions, so the verdicts are the ones the full
  parse would give, only sooner: every alternative runs on every
  document, and before this each declining dialect read the whole file
  (60% of detection on okay-fin's corpora). Text before an XML root
  element is declined (`begins with 'h', not <`): it is not well-formed.
- **A bare scalar is no document.** `hello` and `42` are declined by
  every text format: a level whose answer is "a string" has learnt
  nothing about what to ask next. Write the step if you want scalars.
- **YAML's block dialect and JSON.** `{"a": 1}` is a YAML flow mapping
  by the spec, but okay-codec's dialect is block-only and reads `{` as
  a scalar beside a mapping; `Format.yaml` declines a root-level scalar
  for exactly that reason. When flow style lands, the same input
  becomes `Unclear` naming both — by design.
- **The XML declaration** was read by the XML dialect as an unclosed
  tag until `xml-processing-instruction` (2026-09-29): every real FpML
  file was declined with `unclosed`. `<?…?>` and `<!DOCTYPE …>` are one
  token each now, and `TestFormat` reads a declared document as
  `text/xml`.
- **HTML's void elements are not XML's.** `Format.xml` reads STRICT
  XML (`Xml.cst(s, Xml.strict)`, since `xml-strict-void`): under the
  HTML default `<source>Coal</source>` never opened, because `source`
  is an HTML void, and its close "closed nothing". An HTML page's `<br>`
  now declines as "never closed" here — the right answer for a data
  document detector. Found by a domain corpus, not a unit test.
- **`field` on a repeated element hands back an array.** Two
  `versionedTradeId`s under one identifier declined a trade twice in a
  day; `each(name)` reads all, and a `first(name)` over it is the step
  for "the first of".
- **There is no registry type.** An `Or` is flat, so `Refine.first(a, b,
  c)` IS the registry: one more alternative is one more element, in
  order, and nothing existing is edited. A `Judge` that re-orders the
  alternatives waits for a second orderer to exist (specs/refine.md).
- **`write` is `Either` on a tree.** A step's prism review is total; an
  `Or` into a sum cannot know which alternative a case belongs to
  without asking, so the tree's `write` is partial and says so.

## Literature

- Pickering, Gibbons, Wu, *Profunctor Optics: Modular Data Accessors*
  (Programming 2017) — the prism as partial getter + total constructor;
  composition along a path is what makes a read-and-write-back pair.
- Kiselyov, Shan, Friedman, Sabry, *Backtracking, Interleaving, and
  Terminating Monad Transformers* (ICFP 2005) — the soft cut a later
  stage's "if this is a swap then … else …" is spelled with.
- Wirth, *Program Development by Stepwise Refinement* (CACM 1971) — the
  word, and the discipline: what is known narrows one decision at a time.
- Hutton, Meijer, *Monadic Parser Combinators* (1996) — parsers as
  values composed by choice and sequence, the shape this borrows for a
  grammar that is not fixed in advance.
- Rendel, Ostermann, *Invertible Syntax Descriptions: Unifying Parsing
  and Pretty Printing* (Haskell Symposium 2010) — partial isomorphisms,
  and why a reader-writer pair is an invariant functor with products and
  choice rather than an applicative: `map`, `and`, `or` here.
- Hughes, *Generalising Monads to Arrows* (SCP 2000) — the `>>>` glyph,
  and the `arr` a pattern cannot have.
- ISDA, *FpML* (Financial products Markup Language) and *Common Domain
  Model* — the two public corpora the private domain modules read; this
  module's format level is what they stand on.
