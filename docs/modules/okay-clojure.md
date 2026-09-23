# okay-clojure

> Clojure from okay, and okay's stages as Clojure's transducers — both
> ways. JVM; `org.clojure:clojure` 1.12.6 (runs on JDK 8+).

| | |
|---|---|
| `Clj` | calling Clojure through its own Java API (`clojure.java.api.Clojure`): `fn(ns, name)` loads the namespace and answers the var's `IFn`, `require(ns)`, `eval(source)` — each a `Left` NAMING what is missing (an unbound var, a namespace that does not load, source that does not read) instead of Clojure's later "Attempting to call unbound fn" |
| `Transducers.of(stage)` | an okay `Stage` as a Clojure transducer: runs in `into`, `transduce`, `sequence`, `eduction`, a core.async channel; composes with `comp` against Clojure's own, in either order |
| `Transducers.stage(xf)` | any Clojure transducer — `(map inc)`, `(partition-all 3)`, `(dedupe)`, a library's own — as a `Stage` in an okay pipeline (`through`, `pipe`) |

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

**One-shot pipelines.** A Clojure transducer's `volatile!` cannot be
snapshotted, and `through` runs a stage eagerly to its first output,
so a pipeline BUILT with `through` over `Transducers.stage` runs once
and refuses a second run by name — before the spent state is touched.
Build it again to run it again. `Transducers.of` has no such limit:
each application to a reducing function is a fresh process, as it is
for Clojure's own.

No AOT, no `gen-class`: the transducer is a Scala `AFn`, and Clojure
sees an ordinary `IFn` — the module has no Clojure build step.
