# okay-frege — Frege programs as okay programs, okay effects in Frege's IO

## Overview

Frege (frege-lang.org) is a Haskell for the JVM: lazy, pure, type
classes, `IO` as a monad. State checked 2026-09-23: development resumed
in 2025 after a pause (37 commits since, 32 of them one contributor
automating the Java FFI, reviewed by the language's author);
`org.frege-lang:frege:3.25.153` on Maven Central, published 2026-08-23,
one 9 MB jar with no dependencies. Probed here: the compiler runs on
JDK 25 (1.2 s for a module), `-target 17` emits major 61 that runs on
17, 21 and 25 unchanged; the runtime itself is major 52.

Frege is COMPILED — a `.fr` source becomes Java and then classes — so
unlike okay-clojure there is no `eval`: Frege sources are compiled by
an sbt task before the Scala that calls them.

## The seam: okay's effects LIFTED into Frege's IO (operator, 2026-09-23)

The obvious bridge — a pure Frege function `[a] -> [b]` fed an input
list whose tail thunks pull from okay — is Haskell's lazy IO, with its
failures: the function, not okay, decides when to read (a `reverse`
forces the whole input), a thunk outlives the resource it reads, an
upstream failure surfaces wherever a thunk happened to be forced, and
— the decisive one — forcing a thunk is a synchronous JVM call that
cannot SUSPEND, so an okay upstream that awaits or is `Async` cannot
be stepped from inside one.

So okay's effects enter Frege as explicit `IO` actions instead, and
the Frege program's own sequencing (`>>=`) orders them:

```haskell
native await   okay.frege.Io.await   :: () -> IO (Maybe a)   -- Take
native tell    okay.frege.Io.tell    :: b  -> IO ()          -- Writer
native perform okay.frege.Io.perform :: Op -> IO a           -- any operation of the row

runningSum :: Long -> IO ()
runningSum acc = await () >>= maybe (return ()) (\x -> tell (acc + x) >> runningSum (acc + x))
```

**The mechanism: a thread is the continuation.** The Frege `IO`
program runs on its own virtual thread; each `await`/`perform` is a
handoff to the okay DRIVER, which is an ordinary okay program: it
performs the operation in its own row (`effect`), under whatever
handlers run it — so an `Async` upstream, `Throws`, `Reader`, `State`
all work — and hands the answer back, the Frege thread parked until
then. Strict alternation: at any moment exactly one side runs, so it
is a coroutine, not concurrency. Monadic reflection across a thread
boundary.

Probed 2026-09-23 (scratchpad, Java driver): correct results; a
recursive `IO` loop over 1e6 elements with no stack overflow; an
interrupt from the driver ends a Frege thread parked in `await`;
~10.5 µs per element with two handoffs per element (await and tell).

## Behavior

- [ ] build: `project/Frege.scala` compiles `src/{main,test}/frege/**.fr`
      with the Frege compiler FORKED (it may exit the JVM), `-target 17`,
      against the configuration's classpath, into a managed class dir on
      that configuration's classpath; incremental by source timestamps;
      a Frege error fails the build with the compiler's message
- [ ] `okayFrege` (JVM) depends on core + okay-stream and
      `org.frege-lang:frege:3.25.153`; root aggregate; module page, row
- [ ] `Frege.stage[I, O](io)`: a Frege `IO ()` using `await`/`tell` as
      a `Stage[I, O, Unit]` — law: equals the same stage written in
      okay, over empty, one, ten, a thousand elements
- [ ] tells are BUFFERED on the Frege side and handed over at the next
      `await` or at the end: one handoff per element, not two —
      measured before and after
- [ ] a Frege stage that returns early stops the pipeline pulling (a
      1000-element producer pulled no further than the stage read)
- [ ] `Frege.run[F](io)`: a Frege `IO a` as `A ! F`: every `perform op`
      is tested against the row (`TypeableK[F]`) and performed as an
      operation of F — Reader, State, Throws, Async (a sleep) each
      shown from Frege; an operation outside the row is refused BY NAME
- [ ] an okay failure (`Throws` handled outside, a thrown exception)
      reaches Frege at the `perform` that caused it, and a Frege
      exception reaches okay as a failure naming the Frege frame
- [ ] ONE-SHOT only: a thread is a one-shot continuation; running the
      Frege program under a multi-shot handler (`Choice` resuming
      twice) is refused by name at the second resumption, not
      deadlocked
- [ ] abandonment: `Frege.scoped` ties the Frege thread to a
      `Resource` region (interrupted at the region's end, observed by a
      test); a `Cleaner` interrupts a thread whose driver became
      unreachable (best effort, documented as such)
- [ ] pure data both ways: Frege `[a]` <-> okay `Chunks` lazily (an
      infinite Frege list taken partially from okay; an okay producer
      as a Frege list only when it is PURE — `Writer % A` alone — by
      type), `Maybe` <-> `Option`, `Either` <-> `Either`
- [ ] docs: module page with the lazy-IO argument, guide paragraph,
      theory ch. 7 (or 8) paragraph with references (Frege; Launchbury
      & Peyton Jones on `ST`/`IO`; Filinski's reflection); every
      snippet in a gated test

## Decisions

- **Effects as IO, not lazy IO** — the operator's call and the
  argument above. The pure part of a Frege program stays lazy and that
  laziness is SAFE, because no effect hides in a thunk.
- **A thread per program run, virtual where available.** The only way
  a synchronous JVM call inside Frege can wait for an okay effect.
  Below JDK 21 a platform thread (the `Schedulers.hasVirtualThreads`
  guard okay-platform uses).
- **The operation crossing is ONE cast, in one function.** Frege hands
  the driver an `Object`; `TypeableK[F].test` proves it an operation of
  F, and because `F` is covariant an `F[X]` IS an `F[Any]` — so the
  refinement after the test is the only cast, and the answer needs
  none (it goes back to Frege as an `Object`).
- **Refused: sbt-frege** (the 2015 plugin, unmaintained, sbt 0.13).

## Results
