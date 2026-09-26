# okay with other JVM languages: Java streams, Clojure, Frege

okay is a Scala 3 library, but the JVM has other languages, and each has
its own way of transforming a stream and of doing effects. This guide is
about using them TOGETHER with okay: a JDK stream running an okay stage,
a Clojure transducer inside an okay pipeline, a Frege program asking
okay's `Reader` and sleeping on okay's timer.

Every Scala, Clojure and Frege example below is copied from a test that
runs in this repository's gate — `okay-java/…/TestDocExamplesGather.scala`,
`okay-clojure/…/TestDocExamplesClojure.scala`,
`okay-frege/…/TestDocExamplesFrege.scala`, and the Clojure and Frege
source those suites load — so they compile and pass as written; and a
gated test (`TestDocSnippets`) fails if one of them stops being a
verbatim copy. The build configuration in §8 is marked as the one block
a test cannot pin.

Contents:

1. [One idea, three bridges](#1-one-idea-three-bridges)
2. [Java: `java.util.stream`](#2-java-javautilstream)
3. [Clojure](#3-clojure)
4. [Frege](#4-frege)
5. [Which bridge, for what](#5-which-bridge-for-what)
6. [The rules the bridges keep](#6-the-rules-the-bridges-keep)
7. [What it costs](#7-what-it-costs)
8. [Setting up the build](#8-setting-up-the-build)
9. [Literature](#9-literature)

## 1. One idea, three bridges

Two things cross between okay and another language, and each has one
shape on the okay side.

**A stream transformation is a `Stage`.** okay's `Stage[I, O, A]` is a
program that awaits `I` and tells `O` — a transducer written as a
program (guide §5). Java's stream *gatherer* (JDK 24) and Clojure's
*transducer* are the same thing written as a push-style state machine:
a step that may emit any number of outputs and may say "stop", and a
flush at the end. So each translates to and from a `Stage` by pure
mechanics — the other side's state is the stage suspended at its next
`await`, a `tell` is an emit, a stage that answers is a "stop"
(theory ch. 7 has the argument; Hickey's transducers and JEP 485 are the
references).

**An effect crosses as DATA, never as a lazy value.** A lazy language
tempts you to let a lazy list pull from okay — the element computed when
a thunk is forced. That is lazy IO, and it fails the way Kiselyov's
iteratee paper says it fails: the consumer, not you, decides when to
read; a thunk outlives the resource it reads; a failure surfaces where a
thunk happened to be forced; and a forced thunk cannot *suspend*, so an
okay effect that waits cannot run inside one. So an okay effect enters
Clojure or Frege as an explicit operation of a small program-as-data
library written in THAT language (`okay.core`, `okay.frege.Prog`) — okay's
own freer tree, re-spelled — and okay walks it. Its continuations are the
other language's functions, so a handler may call them twice.

| | a transformation | effects from the other side | lazy data |
|---|---|---|---|
| Java | `Gather`: Stage ⇄ `Gatherer`; `Collect`: Aggregator ⇄ `Collector` | — (Java code calls okay directly) | `Streams`: `Chunks` ⇄ `Stream` |
| Clojure | `Transducers`: Stage ⇄ transducer; `CoreAsync`: core.async channel = okay `Channel` | `okay.core` + `Program` | `Program`: seq ⇄ `Chunks` |
| Frege | a `Prog` stage | `okay.frege.Prog` + `Frege` | `Frege`: list ⇄ `Chunks` |

## 2. Java: `java.util.stream`

A stage runs inside a JDK stream as an intermediate operation, and a
stage that ANSWERS stops even an infinite stream:

```scala
// a stage that answers after three: the JDK's integrator returns false
val firstThree: Stage[Int, Int, Unit] =
  Stage.transduceUntil[Int, Int, Int, Unit](0)(
    (n, i) => Stage.tell[Int, Int](i * 10).map(_ => if n + 1 >= 3 then Right(()) else Left(n + 1)),
    _ => ())

val out = Stream.iterate(1, _ + 1).gather(Gather.gatherer(firstThree)).toList   // [10, 20, 30]
```

and any JDK gatherer — the JDK's own `Gatherers`, a library's — runs in
an okay pipeline:

```scala
val windows = through(lines("a", "b", "c"))(Gather.stage(Gatherers.windowFixed[String](2)))
// Writer.run(windows) — (Seq([a, b], [c]), ()); run it again: the same
```

An event-time window is a gatherer too, and unlike a `Collector` it
hands each pane on the moment the watermark closes it:

```scala
val panes = events
  .gather(Windowed.gatherer[Ev, String, Long, Long](10, 10, 0)(_.key)(_.ts)(sum))
  .toList                                   // [Pane(0,10,a,6), Pane(10,20,a,2), Pane(20,30,a,7)]
```

Gatherers need JDK 24 at run time; okay-java still loads on 17 and 21
(a JVM links a class only when it is called). docs/modules/okay-java.md.

## 3. Clojure

Calling Clojure is its own Java API with names refused honestly:

```scala
val upper = Clj.fn("clojure.string", "upper-case")          // Right(#'clojure.string/upper-case)
```

A stage is a transducer — composed with Clojure's own by `comp`:

```scala
val xf = comp.invoke(odd, Transducers.of(runningSum))        // (comp (filter odd?) <stage>)
val out = into.invoke(PersistentVector.EMPTY, xf, Clj.eval("(range 10)").toOption.get)
// [1 4 9 16 25]
```

and a Clojure transducer is a stage:

```scala
val pairs = Transducers.stage[String, AnyRef](fn(Clj.eval("(comp (dedupe) (partition-all 2))").toOption.get))
val windows = through(lines("a", "a", "b", "c", "c", "d"))(pairs)
// Writer.run(windows) — (Seq([a b], [c d]), ())
```

**okay's effects from Clojure** go through `okay.core`, a namespace in
the okay-clojure jar. A program is data; `mlet` reads like `do`:

```clojure
(def reader-state
  "Reader and State, in mlet order: env * 1000 + the state after +1"
  (ok/mlet [env (ok/perform (Ops/ask))
            s   (ok/perform (Ops/get))
            _   (ok/perform (Ops/set (inc s)))
            s2  (ok/perform (Ops/get))]
    (ok/done (+ (* env 1000) s2))))
```

```scala
val prog = Program.run[Reader % Long + State % Long, java.lang.Long](value("reader-state"))(
  using summon, Program.Row.of[Reader % Long] | Program.Row.of[State % Long])
val answer = !.run(State.handle(5L)(Reader.run(7L)(prog)))   // (6, 7006)
```

A stage that uses `ok/await` and `ok/tell` is `Program.stage`, and one
that also performs is `Program.stageWith[I, O, F]`. **Lazy seqs** cross
both ways, as lazily as they are:

```scala
val c = Program.chunks[java.lang.Long](range)            // a Clojure (range), infinite, as okay Chunks
val s = Program.seq(Chunks.map(Chunks.range(0, 5))(Long.box))   // okay Chunks as a Clojure lazy seq
```

**core.async channels** are okay `Channel`s through `CoreAsync`, checked
by the same law battery as okay's own — so a Clojure `go` block and an
okay stream share one channel, and an okay stage can be the channel's
own transducer:

```scala
val ch = fn("clojure.core.async", "chan").invoke(Long.box(10L), Transducers.of(runningSum))
val c = CoreAsync.of[java.lang.Long](ch)
```

**Clojure data** crosses as EDN: okay-codec's `Edn` writes what
`clojure.edn/read-string` reads (keyword keys, exact integers, a variant
as a `#Sum/Case` tag) and reads what Clojure's `pr-str` prints, through
the same `Schema` as JSON and CBOR — docs/modules/okay-codec.md.

docs/modules/okay-clojure.md.

## 4. Frege

Frege is a Haskell for the JVM, and COMPILED: `.fr` sources become
classes before your Scala calls them (§8). A Frege program that uses
okay is written in `okay.frege.Prog` — okay's tree in Frege — with
`await`, `tell`, `perform` and `liftIO`:

```haskell
--- a running sum, iteratee style: await until Nothing, tell each sum
runningSum :: Long -> Prog ()
runningSum acc = do
  m <- await
  case m of
    Nothing -> return ()
    Just x  -> tell (acc + x) >> runningSum (acc + x)
```

```scala
val sums = through(numbers(1, 2, 3, 4))(Frege.stage[Long, java.lang.Long](P.runningSum(Thunk.`lazy`(0L)).call()))
// Writer.run(sums) — (Seq(1, 3, 6, 10), ())
```

okay's effects are TYPED operations: the native that makes one names
its answer, so using it at another type is a Frege type error:

```haskell
pure native askOp    okay.frege.Ops.ask     :: () -> Operation Long
pure native getOp    okay.frege.Ops.get     :: () -> Operation Long
pure native setOp    okay.frege.Ops.set     :: Long -> Operation Long

readerState :: Prog Long
readerState = do
  env <- perform (askOp ())
  s   <- perform (getOp ())
  _   <- perform (setOp (s + 1))
  s2  <- perform (getOp ())
  return (env * 1000 + s2)
```

Because a continuation is a Frege function, a multi-shot handler simply
calls it per branch:

```haskell
choosing :: Prog Long
choosing = do
  a <- perform (choose2 1 2)
  b <- perform (choose2 10 20)
  return (a + b)
```

```scala
val all = !.run(runChoice(Frege.run[Choose, java.lang.Long](P.choosing.call())))   // 11, 12, 21, 22
```

Existing Frege `IO` code enters by `liftIO`, as one step. It never
calls back into okay, so nothing has to be suspended. Where the
program's row carries `Async`, the step runs on a thread of its own, and
cancelling the fiber INTERRUPTS it whatever the scheduler. Without that
thread, a pool-threaded scheduler reported the fiber finished while a
blocking `liftIO` ran on (interop-lift-cancellation). Clojure's blocking
step is `(ok/lift f)`, and it gets the same treatment. Frege lists and okay `Chunks` convert lazily both ways:

```scala
// an INFINITE Frege list, read partially by okay
val firstFive = Chunks.take(Frege.chunks[java.lang.Long](P.squares.call()))(5)      // 1, 4, 9, 16, 25

// an INFINITE okay source, handed to a Frege function that takes 10
val sum = P.sumFirst(10, Frege.list(countedNats(produced, 16)))              // 45, and okay produced ≤ 16
```

docs/modules/okay-frege.md.

## 5. Which bridge, for what

- **Reuse a transformation someone wrote** — the JDK's `Gatherers`, a
  Clojure library's transducer — in an okay pipeline: `Gather.stage`,
  `Transducers.stage`.
- **Offer an okay stage to code in another language** — a lexer, a
  window, a protocol framer — as the thing that language already
  composes: `Gather.gatherer` for `stream.gather`, `Transducers.of` for
  `into`/`sequence`/`comp`.
- **Write logic in the other language that needs okay's effects** —
  reading config, state, failure, time, choice — as a program okay runs:
  `okay.core` + `Program.run`, `okay.frege.Prog` + `Frege.run`; as a
  stage that also performs: `stageWith[I, O, F]`.
- **Hand lazy data across** — a Clojure `(range)`, a Frege infinite
  list, an okay source — `chunks` one way, `seq`/`list` the other.

## 6. The rules the bridges keep

- **Lazy data is PURE by type.** `Program.seq` and `Frege.list` accept
  only `Chunks` — an okay source with no other effect. An effectful
  source forced from inside a lazy value is lazy IO; it belongs in a
  program, as a `perform`.
- **A built pipeline is a value, even over the other side's mutable
  state.** `through(p)(stage)` starts the stage when the program is
  RUN, once per run (it used to drive the stage to its first output at
  build time, and the built program then held that run's state —
  found by a doc snippet, fixed in `through` itself,
  windows-stage-rerun-loses-pane), so a JDK gatherer's state or a
  Clojure transducer's `volatile!` is made afresh by every run. What
  neither can do is be snapshotted: a continuation from INSIDE a run,
  resumed again after that run finished, is REFUSED by name by
  `Gather.stage` and `Transducers.stage` before the spent state is
  touched. (`Gather.gatherer` and `Transducers.of` start afresh per
  JDK evaluation and per application to a reducing function.)
- **Types are checked at the seam, and refused by name.** A value
  arriving as `Object` is tested against the type the okay side declared
  (a `ClassTag`; Clojure's integers are `java.lang.Long`); an operation
  is tested against the program's row (`Row`, which is the core's
  `okay.Member` under each bridge's own name). A union row is spelled
  once — `Row.of[F] | Row.of[G]` — because the compiler cannot infer
  the two sides of a union type.
- **No bridge claims what it cannot keep.** No combiner from a stage (a
  suspended program is a position, and positions do not merge — a
  combiner-less gatherer is evaluated in order even in a parallel
  stream); no multi-shot through a thread (there is no thread).

Python and R are not JVM languages, and each runs in its own process. They
get the same depth by a different road: typed calls, and callbacks into
okay's effects by name. See [okay with Python and R](python-and-r.md).

### Where each language names okay's effects

okay's Scala programs carry their effects in their type, `A ! F`. How
much of that reaches the other language differs:

| language | in the language's own types | where the row is stated |
|---|---|---|
| Scala 3 | the whole row, `A ! Reader % Long + State % Long` | the type |
| Scala 2 ([okay-scala2](scala2.md)) | the whole row, `A ! (State[S] + Writer[W])`; `!.run` takes only `A ! Pure` | the type |
| TypeScript | the operations and their signatures, `Prog<T, ShopOps>` / `effects<ShopOps>()`, generated from the Scala callbacks by `Ts.ops` ([details](typescript.md#a-programs-effects-in-its-type)) | the type, checked by tsc |
| Frege | each operation's ANSWER, `perform :: Operation a -> Prog a`; `Prog a` has no row; the caller's callbacks typed argument and answer, generated by `Jvm.frege` ([below](#the-callers-own-operations-declared-once)) | `Frege.run[F, A]` in Scala, checked per operation at run time |
| Clojure | nothing; a program is data; the caller's callbacks as `defn`s generated by `Jvm.clojure` | `Program.run[F, A]` in Scala, checked per operation at run time |
| Haskell (GHC worker) | the effects and their operations, `Eff '[Shop] Double` with `send (PriceOf sku)`; the effect's GADT generated from the Scala callbacks by `Hs.ops` ([details](python-and-r.md#haskell-programs-typed-by-their-effects)); untyped `perform` by name still works | the type, checked by GHC |
| Go (worker) | each operation's argument and answer, `okay.Op[float64]` with `okay.Send`, generated by `Go.ops` ([details](go.md#typed-operations)); no type-level lists, so not the set | the callbacks the Scala side offers; the operations' types checked by `go build` |
| Python, R | nothing; an operation is a name | the callbacks the Scala side offers, checked by name at run time |

Frege has no type-level lists, so the typed answer is as far as it can
go.

### The caller's own operations, declared once

okay's own effects (`Reader`, `State`, `Choose`, …) a Frege or Clojure
program performs directly, through `okay.frege.Ops` and `okay.clojure.Ops`.
An operation of YOURS is declared once, as the `Foreign.callback`s that
serve Python, TypeScript, Go, Rust, Haskell and R:

```scala
val priceOf = Foreign.callback[String, Double]("price_of")(sku => Reader.ask[Map[String, Double]].map(_(sku)))
val discount = Foreign.callback[Double, Double]("discount")(a => Reader.ask[Map[String, Double]].map(m => a * m("rate")))
val cbs = Foreign.callbacks(priceOf, discount)
```

`Jvm.frege` writes a Frege module from them, one operation per callback,
typed by its Schemas — so `priceOf 42` is a Frege type error, not a run-time
surprise:

```scala
assertEquals(Jvm.frege("okay.frege.Shop", cbs), checkedIn)
```

```haskell
priceOf :: String -> Operation Double
priceOf a = callOp "price_of" a
```

A Frege program performs them like any operation, and `run` is handed the
callbacks that answer them — each run under the caller's handlers, here
its Reader:

```haskell
quote :: String -> Long -> Prog Double
quote sku qty = do
  price <- perform (priceOf sku)
  perform (discount (price * qty.double))
```

```scala
val quote = Frege.run[Reader % Map[String, Double], java.lang.Double](Quote.teaForThree.call(), calls = Jvm.calls(cbs))
assertEquals(!.run(Reader.run(prices)(quote)).doubleValue, 6.0)
```

Clojure gets the same from `Jvm.clojure` — a namespace of `defn`s
(`(shop/price-of sku)`) — and `Program.run(…, calls = Jvm.calls(cbs))`;
there a wrong argument is refused by name, by the callback's own Schema. A
call nobody offered is refused by name too, with the names that were. The
suites are `TestFregeCallbacks` and `TestClojureCallbacks`; the checked-in
`Shop.fr` and `shop.clj` are compared with the generator's output, so a
changed callback that was not regenerated fails the build.

### Bridging a language this guide does not cover

The three bridges share one copy of everything that is not a matter of
the other language's syntax, so a fourth (Kotlin, Groovy, a Lisp, a
Scheme) is mostly a matter of reading that language's values:

- `okay.Push` drives a `Stage` by pushing its tells, and answers where
  the stage stands between two calls from outside (`Fresh`, `Waiting`,
  `Done`). `Gather.gatherer` and `Transducers.of` are each a few lines
  over it: the first hands it the JDK's `Downstream.push`; the second
  hands it a function that calls `rf` and threads the accumulator.
- `okay.Foreign` walks a program that the other language wrote as data
  (an answer, or an operation plus a function from its answer to the
  rest) as an okay stage or program. A bridge supplies a `Foreign.View`
  that says which of five kinds a node is (Done, Await, Tell, Perform,
  Lift), reads its payload, and resumes it with an answer.
  `Program.stageWith` and `Frege.stageWith` are each one line over it.
- `okay.Member[F]` tests whether an `Object` from the other side is an
  operation of the row F.
- `okay.Operations` holds the core effects' operations as values for
  the other language to hand back: `ask`, `get`, `set`, `raise`,
  `choose`, `sleep`.

This is the free monad's separation of a program's description from its
interpretation \[Swierstra 2008; Kiselyov & Ishii 2015\], applied across a
language boundary. The foreign language builds the description, and the
view is the only part of the interpreter that needs to know how that
language spells it.

## 7. What it costs

Measured in the modules' own suites (`PRICE` lines), on this repository's
build machine:

| | per step |
|---|---|
| a Frege `Prog` step through the driver | 0.27 µs |
| a Frege program run on its own thread, each operation a handoff — the design built first and dropped | ~10.5 µs |
| a hundred thousand Clojure `okay.core` steps | 0.33 s in all, on the default stack |

The drivers are trampolined by okay's interpreter, so program length is
bounded by memory, not by the stack.

## 8. Setting up the build

- **Java**: `okay-java`. Gatherers need a JDK 24+ runtime; nothing else
  does.
- **Clojure**: `okay-clojure` brings `org.clojure:clojure` 1.12;
  `okay.core` is inside the jar — `(require '[okay.core :as ok])`.
- **Frege**: `okay-frege` brings the Frege runtime; the sbt plugin
  `okay-frege-sbt` compiles your `.fr` sources:

<!-- not-a-test: sbt build configuration -->
```scala
// project/plugins.sbt
addSbtPlugin("dev.okay" % "okay-frege-sbt" % okayVersion)

// build.sbt
lazy val app = project
  .enablePlugins(OkayFrege)
  .settings(libraryDependencies += "dev.okay" %% "okay-frege" % okayVersion)
  .settings(OkayFrege.before(Compile))     // src/main/frege, read by src/main/scala
```

  The Frege compiler targets JDK 17 (`fregeTarget`), and a Frege warning
  fails the build (`fregeFailOnWarnings`); acknowledge one that is right
  to keep with `--- nowarn: <message>` in its source.

## 9. Literature

- Rich Hickey. *[Transducers are coming.](https://clojure.org/news/2014/08/06/transducers-are-coming)* 2014.
- Viktor Klang. *[JEP 485: Stream Gatherers.](https://openjdk.org/jeps/485)* OpenJDK, final in JDK 24.
- Oleg Kiselyov. *[Iteratees.](https://doi.org/10.1007/978-3-642-29822-6_15)* FLOPS 2012 — why lazy IO fails, and the consumer-as-program answer.
- Wouter Swierstra. *[Data types à la carte.](https://doi.org/10.1017/S0956796808006758)* JFP 18(4), 2008 — a program as a value, its interpretation supplied separately.
- Oleg Kiselyov, Hiromi Ishii. *[Freer monads, more extensible effects.](https://doi.org/10.1145/2804302.2804319)* Haskell 2015 — the tree `okay.core` and `Prog` re-spell.
- Simon Peyton Jones, Philip Wadler. *[Imperative functional programming.](https://doi.org/10.1145/158511.158524)* POPL 1993 — effects as a monad in a lazy language.
- Gordon Plotkin, Matija Pretnar. *[Handling algebraic effects.](https://doi.org/10.2168/LMCS-9(4:23)2013)* LMCS 2013 — handlers that call a continuation more than once.

Theory ch. 7 (docs/theory/07-logic-streams.md) carries the argument
that a gatherer and a transducer are the push-form enumeratee.
