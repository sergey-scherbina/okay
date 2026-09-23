# okay-clojure

> Clojure from okay, and okay's stages as Clojure's transducers — both
> ways. JVM; `org.clojure:clojure` 1.12.6 (runs on JDK 8+).

| | |
|---|---|
| `Clj` | calling Clojure through its own Java API (`clojure.java.api.Clojure`): `fn(ns, name)` loads the namespace and answers the var's `IFn`, `require(ns)`, `eval(source)` — each a `Left` NAMING what is missing (an unbound var, a namespace that does not load, source that does not read) instead of Clojure's later "Attempting to call unbound fn" |
| `Transducers.of(stage)` | an okay `Stage` as a Clojure transducer: runs in `into`, `transduce`, `sequence`, `eduction`, a core.async channel; composes with `comp` against Clojure's own, in either order |
| `Transducers.stage(xf)` | any Clojure transducer — `(map inc)`, `(partition-all 3)`, `(dedupe)`, a library's own — as a `Stage` in an okay pipeline (`through`, `pipe`) |
| `okay.core` (Clojure, in the jar) | okay's effects FROM Clojure: a program is data — `done`, `step`, `bind`, `perform`, `await`, `tell`, and `mlet`, a monadic let |
| `Program.stage` / `stageWith` / `run` | a Clojure `okay.core` program as an okay `Stage` (that may also perform) or as `A ! F` — multi-shot, no threads |
| `Program.chunks` / `Program.seq` | a Clojure seq as okay `Chunks` and okay `Chunks` as a Clojure lazy seq, both LAZY — infinite either way; `seq` takes only pure `Chunks` |
| `Ops` | the core effects' operations for Clojure to perform (Reader, State, Throws, Choose, Async sleep) |
| `CoreAsync.channel` / `CoreAsync.of` | a core.async channel as an okay `Channel` — the same law battery as every okay channel; Clojure sees an ordinary core.async channel |

## A stage is a transducer

Hickey's transducer ([Hickey 2014](https://clojure.org/news/2014/08/06/transducers-are-coming))
is a transformation of a reducing function with three arities — init,
step `(acc x)`, completion `(acc)` — where a step may answer
`(reduced acc)` to stop the process and a stateful transducer flushes
in its completion arity. okay's `Stage[I, O, A]` is that as a program:
a `tell` is a call of the reducing function, a stage that ANSWERS is a
step answering reduced, and what the stage tells after its last
`await` answered `None` is the flush. The translation is the one
okay-java's `Gather` makes for the JDK's gatherers; here Clojure's
accumulator is threaded through `rf` where the JDK had a `Downstream`.

```scala
// an okay stage: a running sum, one output per input
val runningSum: Stage[Long, Long, Long] =
  Stage.mapAccumulate[Long, Long, Long](0L)((s, i) => (s + i, s + i))

val into = Clj.fn("clojure.core", "into").toOption.get
val comp = Clj.fn("clojure.core", "comp").toOption.get
val odd = Clj.eval("(filter odd?)").toOption.get

val xf = comp.invoke(odd, Transducers.of(runningSum))        // (comp (filter odd?) <stage>)
val out = into.invoke(PersistentVector.EMPTY, xf, Clj.eval("(range 10)").toOption.get)
// [1 4 9 16 25]
```

and the other way, Clojure's transducers in an okay pipeline:

```scala
val pairs = Transducers.stage[String, AnyRef](fn(Clj.eval("(comp (dedupe) (partition-all 2))").toOption.get))
val windows = through(lines("a", "a", "b", "c", "c", "d"))(pairs)
// Writer.run(windows) — (Seq([a b], [c d]), ())
```

Both directions are law-tested against Clojure's own
`(into [] xf coll)` (`TestTransducers`), and each claim that matters has
a test that fails without it: a stage that answers stops `(range 1000)`
after 3 elements, a downstream `(take 2)` stops a stage mid-element
(1000 tells per element, at most 3 made), a `(take 3)` stops an okay
producer pulling.

**Types at the seam.** Clojure hands `Object`s. An element is tested
against the stage's input class (a `ClassTag`; Clojure's integers are
`java.lang.Long`, which `ClassTag[Long]` accepts) and refused by name
when it is something else, rather than failing later as a
`ClassCastException` inside the stage.

**A built pipeline is a value.** `through` starts a stage when the
program is run, so a pipeline BUILT over `Transducers.stage` applies
the transducer afresh on every run. A Clojure transducer's `volatile!`
cannot be snapshotted, though: a continuation from INSIDE one run,
resumed again after that run finished, is refused by name — before
the spent state is touched. `Transducers.of` starts a fresh process
per application to a reducing function, as it does for Clojure's own.

## okay's effects from Clojure: `okay.core`

The transducer is the part of Clojure that is a transformation; a
Clojure program that needs okay's EFFECTS — ask a Reader, touch State,
raise, sleep, choose — goes through `okay.core`, a namespace shipped in
this jar (`(require '[okay.core :as ok])`, no AOT). It is okay's freer
tree written in Clojure: a program is `(done v)` or `(step op k)`, and
`mlet` — cats' monadic let — reads like `do`:

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

okay's driver (`Program`) walks the data: each operation runs under the
okay program's handlers, and the continuation `k` is a Clojure function —
so `Choose` resumes it once per branch (all four sums of `(Ops/choose
[1 2])` and `(Ops/choose [10 20])` come back), a hundred thousand steps
run on the default stack, and no thread is involved. A stage is the same
program using `ok/await` and `ok/tell`; `Program.stageWith[I, O, F]` lets
it perform `F` too. This is okay-frege's design (docs/modules/okay-frege.md
says why it is not lazy IO), and the two drivers are the same walk.

### Blocking code: `ok/lift`

Blocking Clojure or Java code is one explicit step, `(ok/lift f)`:

```clojure
(defn mark-after [key ms]
  (ok/lift (fn [] (Thread/sleep (long ms)) (System/setProperty key "woke") "woke")))
```

Where the okay program's row carries `Async`, the step runs on a thread
of its own, and cancelling the fiber interrupts it whatever the
scheduler (interop-lift-cancellation, TestClojureCancel).

## Lazy seqs, both ways

```scala
val c = Program.chunks[java.lang.Long](range)            // a Clojure (range), infinite, as okay Chunks
val s = Program.seq(Chunks.map(Chunks.range(0, 5))(Long.box))   // okay Chunks as a Clojure lazy seq
```

A Clojure seq is realised only as far as okay pulls it (counted: five
elements read in chunks of eight realise one 32-element Clojure block of
a thousand), and an infinite okay source under `(take 10 …)` produces at
most one chunk. `Program.seq` takes only pure `Chunks` — an effectful
source realised from inside a lazy seq is lazy IO; it is a `perform` in
an `okay.core` program instead.

## core.async channels as okay Channels

`CoreAsync.channel[A](capacity)` makes a core.async channel and hands it
to okay as an okay `Channel` — `sendBlocking`, `receive`, `Source`s,
`merge`, actors all work on it — while Clojure on the other end sees an
ordinary core.async channel. `CoreAsync.of[A](chan)` takes one Clojure
made, with its own buffer and its own transducer, which can be an okay
stage:

```scala
val ch = fn("clojure.core.async", "chan").invoke(Long.box(10L), Transducers.of(runningSum))
val c = CoreAsync.of[java.lang.Long](ch)
(1L to 5L).foreach(i => assert(c.sendBlocking(Long.box(i))))
c.close()
```

— and draining `c` gives 1, 3, 6, 10, 15, the running sum computed by
the okay stage INSIDE the core.async channel.

okay's `Channel` promises more than a queue: close is two-phase, the end
comes only after the buffer, an accepted element is always delivered.
The view keeps those promises over core.async's callbacks — one `take!`
in flight with okay's receivers queued (a cancelled receive loses
nothing: a stash), one `put!` in flight with sends queued (a queued send
can be withdrawn), `close!` only after the queued sends finish — and it
is checked by the SAME law battery every okay channel answers for
(`TestCoreAsyncChannelLaws`, okay-stream's `ChannelLawsSuite`).

No AOT, no `gen-class`: the transducer is a Scala `AFn`, and Clojure
sees an ordinary `IFn` — the module has no Clojure build step.
