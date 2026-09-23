# okay-frege — Frege programs as okay programs

## Overview

Frege (frege-lang.org) is a Haskell for the JVM: lazy, pure, type
classes, `IO` as a monad. State checked 2026-09-23: development resumed
in 2025 after a pause (37 commits since, 32 of them one contributor
automating the Java FFI, reviewed by the language's author);
`org.frege-lang:frege:3.25.153` on Maven Central, published 2026-08-23,
one 9 MB jar with no dependencies. Probed: the compiler runs on JDK 25
(1.2 s for a module); `-target 17` emits major 61 that runs on 17, 21
and 25 unchanged; the runtime itself is major 52.

Frege is COMPILED — a `.fr` source becomes Java and then classes — so
unlike okay-clojure there is no `eval`: `.fr` sources are compiled by
an sbt plugin (okay-frege/sbt-plugin, `OkayFrege`) before the Scala
that calls them.

## The seam: a thin Frege monad over okay's effects (operator, 2026-09-23)

The obvious bridge — a pure Frege `[a] -> [b]` fed an input list whose
tail thunks pull from okay — is Haskell's lazy IO: the function decides
when to read (`reverse` forces the whole input), a thunk outlives the
resource it reads, an upstream failure surfaces wherever a thunk
happened to be forced, and forcing a thunk cannot SUSPEND, so an
upstream that awaits or is `Async` cannot be stepped from inside one.

okay's effects therefore enter Frege explicitly, and — the operator's
second call — through OUR thin wrapper, not Frege's `IO`: `okay.frege.Prog`
(src/main/frege) is okay's freer tree written in Frege,

```haskell
data Op     = Await | Tell Obj | Perform Obj | Lift (IO Obj)
data Prog a = Done a | Step Op (Obj -> Prog a)      -- with Functor/Applicative/Monad

await   :: Prog (Maybe a)       -- a stage's input (okay's Take)
tell    :: b -> Prog ()         -- a stage's output (okay's Writer)
perform :: Obj -> Prog a        -- any operation of the okay program's row
liftIO  :: IO a -> Prog a       -- existing Frege IO, as ONE step
```

and the okay DRIVER (`okay.frege.Frege`, Scala) walks the tree: each
`Step` is one okay program node, so the okay interpreter trampolines
the walk, every operation runs under whatever handlers run the okay
program, and each continuation is a Frege FUNCTION — which a multi-shot
handler may call twice. Existing Frege `IO` code enters by `liftIO`:
the driver runs the action, which never calls back into okay, as one
step — so it needs nothing suspended, and no thread.

## Behavior

- [x] build: `project/Frege.scala` (since stage 3 the okay-frege-sbt plugin) compiles `.fr` with the Frege
      compiler FORKED, `-target 17`; `Frege.before(Compile)` for sources
      the configuration's own Scala reads (the `Prog` library — its
      classes are a product AND are mapped into the jar), `Frege.in(Test)`
      for sources that call the Scala side; a Frege error fails the build
      with the compiler's message
- [x] `okayFrege` (JVM) depends on core + okay-stream and
      `org.frege-lang:frege:3.25.153`; root aggregate; module page, row
- [x] `Frege.stage[I, O](prog)`: a Frege `Prog ()` using `await`/`tell`
      as a `Stage[I, O, Unit]` — law: equals the same stage written in
      okay, over empty, one, ten, a thousand elements
- [x] tells before the first await and after the last are emitted, the
      empty input included
- [x] a Frege stage that returns early ends the stage: a 1000-element
      producer pulled no further than the stage read (3)
- [x] a hundred thousand Frege steps run on the default stack
- [x] `liftIO`: existing Frege IO runs as one step — told from a stage,
      answered from a run
- [x] `Frege.run[F](prog)`: `perform op` performed as an operation of F
      — Reader and State in the order Frege's `>>=` asks; a Throws raised
      from Frege reaches `runEither` as a `Left`
- [x] MULTI-SHOT: `Choose` × `Choose` performed from Frege gives all four
      branches — the continuation is a Frege function, called per branch
- [x] refused by name: an operation outside the row; `await`/`tell` in a
      run; `perform` in a stage
- [x] a Frege `error` surfaces at the step that forced it, with Frege's
      message
- [x] docs: module page with the lazy-IO argument and both designs,
      guide paragraph, theory ch. 7 paragraph with references; every
      snippet in a gated test

## Stage 2 (frege-effects-lists, 2026-09-23)

- [x] `Frege.stageWith[I, O, F](prog)`: a Frege stage that also
      PERFORMS — row `Take % I + (Writer % O + F)`, so it runs through
      okay-stream's effectful `through`; a Frege filter that asks its
      threshold from `Reader` is the test
- [x] `Ops.sleep(ms)`: `Async` from Frege — an `Operation Long` (answers
      the milliseconds slept, not `()`, whose Java form is a `short`);
      two sleeps of 20 ms take at least 40 ms under `Async.run`
- [x] `Frege.chunks(xs)`: a Frege list as okay `Chunks`, lazily — an
      infinite Frege list read partially, forced only as far as okay
      pulled (counted on the Frege side is impossible, so: an infinite
      list completes the test at all, and a bounded `take` answers)
- [x] `Frege.list(chunks)`: an okay pure `Chunks` as a Frege lazy list —
      a Frege function over an INFINITE okay source takes only what it
      needs: the elements okay produced are counted, and bounded by the
      chunk the last demanded element sat in
- [x] `Frege.option`/`Frege.maybe`: `Maybe` <-> `Option`, both ways
- [x] PURE by type: `list` takes `Chunks` (`Unit ! Writer % Chunk[A]`,
      no other row), so an effectful okay source cannot become a Frege
      list — the lazy-IO argument, enforced by the signature
- [x] docs: the module page's list section, guide unchanged

## Stage 3: the build half as a plugin (frege-sbt-plugin, 2026-09-23)

- [x] `okay-frege/sbt-plugin`: a SOURCE sbt plugin, `okay-frege-sbt`
      (`OkayFrege`, `noTrigger`), the okay-deploy-sbt precedent; the
      repository's project/plugins.sbt depends on it and okay-frege
      enables it — project/Frege.scala is gone
- [x] settings a user can change: `fregeTarget` (17), `fregeJavaOptions`,
      `fregeFailOnWarnings` (true); `before(c)` / `in(c)`
- [x] incremental on the sources AND the compiler's classpath and
      options: a change in the Scala a native binds to recompiles (the
      first cut tracked source timestamps only and said so)
- [x] the full matrix green with okay-frege built through the plugin
      (the repository IS the plugin's first user)

## Decisions

- **Effects explicit, never in a thunk** — the lazy-IO argument above.
  The pure part of a Frege program stays lazy, and that laziness is
  safe because no effect hides in it.
- **Our monad, not Frege's IO, carries okay's effects** (operator). The
  first cut lifted okay's operations into Frege's `IO` as natives and
  ran the Frege program on its own (virtual) thread, the thread being
  the continuation: correct — a 1e6-element recursive IO loop, an
  interrupt ending a parked thread — but ONE-SHOT (a thread cannot be
  copied, so `Choice` had to be refused), ~10.5 µs per element over two
  synchronous handoffs, and it needed abandonment machinery (a
  `Cleaner`, a scope) for a stage the downstream stopped pulling. As a
  tree walked by the driver: multi-shot, 0.27 µs per step, no thread to
  abandon. Existing IO code loses nothing: `liftIO` runs it as a step.
- **A union row is BUILT, not found.** `Frege.Row[F]` tests whether an
  `Object` from Frege is an operation of F (the core's `TypeableK` tests
  one signature; `split` needs no more because it excludes the other
  side, and an untyped value has no other side). A single signature's
  `Row` is a given; for `F + G` dotty does not infer F and G from the
  union type lambda (it answered `Nothing` for both, measured), so a
  union is spelled once: `Row.of[F] | Row.of[G]`.
- **The operation crossing is ONE cast, in one function** — after
  `Row.test` has said the `Object` is an operation of F; `F` is
  covariant, so `F[X]` is `F[Any]` and the answer needs none.
- **Operations are typed by their answer** (frege-typed-operations,
  2026-09-23). `data Operation a = pure native java.lang.Object {}` — a
  phantom, the `{}` saying the Java type takes no parameters (without it
  Frege emits `Object<A>`) — and `perform :: Operation a -> Prog a`, so
  the native that makes an operation fixes its answer type the way
  `effect[F, A](op: F[A])` does in okay: using an `Operation Long`'s
  answer as a `String` is a Frege TYPE error (checked), where the first
  cut's `perform :: Obj -> Prog a` let the caller pick and failed at run
  time.
- **The one cast is private and acknowledged, not hidden.** An answer
  crosses as `Object` and is read at its type by `fromObj :: Obj -> a`, a
  Java cast native. Frege's check (`Typecheck.checkReturn`) warns "will
  diverge" for a result that is a bare type variable absent from the
  arguments — and that shape is the ONLY one for which Frege emits a
  generic `(A)(Object)x` (an `Answer a -> a` detour came out as a plain
  `(Object)x` javac refused, measured). So `fromObj` is `private` (not
  resolvable outside `Prog`, checked) and carries Frege's own
  acknowledgement, `--- nowarn: application of fromObj will diverge` —
  the mechanism the Frege Prelude uses for `error :: String -> u`.
  `null` is `Nothing` for `await` (`java.util.Objects.isNull`), so the
  driver knows no Frege Prelude type.
- **Frege warnings fail the build** (frege-typed-operations): the
  compiler's `W <file>:<line>:` lines were logged at debug and the gate
  never saw them — the "will diverge" sat there from the first landing.
  `fregeCompile` fails on any; one that is right to keep is acknowledged
  in the source with a `nowarn` doc comment.
- **Refused: sbt-frege** (the 2015 plugin, unmaintained, sbt 0.13).

## Results

Landed 2026-09-23 (okay-frege). 12 tests in `TestFrege`, 3 in
`TestDocExamplesFrege`; 0.269 µs per Frege step through the driver
(PRICE line, 200 000 tells) against ~10.5 µs per element for the thread
cut. Two mutants — the row test disabled, a stage pulling one element
past a returned Frege program — each failed exactly its own test.

**Found on the way.** `products` does not feed sbt's `packageBin`: the
first jar carried the Scala driver and none of the 13 `Prog*` classes it
walks; they are mapped in by name now. `externalDependencyClasspath`
includes `unmanagedClasspath`, which is where the Frege output goes —
the task reads `managedClasspath` (sbt named the cycle). Scala 2's
nested comments made a `src/<config>/frege/**.fr` in a scaladoc an
unclosed comment in the build definition.

**Stage 2 (frege-effects-lists).** `stage` became `stageWith` at the
empty row — `Pure` is `Nothing`, so `Writer % O + Pure` IS `Writer % O`
and dotty accepted the equality: one walker instead of two. 24 tests in
the module; two new mutants — a Frege list built EAGERLY over an
infinite okay source (a StackOverflowError in both laziness tests) and a
sleep that did not sleep (15 ms against the 40 asserted) — each failed
its own test. The Async answer is the milliseconds slept, because
Frege's `()` is a Java `short` and a boxed Unit would not cast.

**Stage 3 (frege-sbt-plugin).** project/Frege.scala is now the source
plugin okay-frege/sbt-plugin (`okay-frege-sbt`, `OkayFrege`), which the
repository's project/plugins.sbt depends on — the okay-deploy-sbt
precedent — and which okay-frege enables like any user would. Checked,
not assumed: a cold `okayFrege/test` through the plugin is green with
the Frege warning check on; a rerun with nothing changed leaves the
compiled Frege classes untouched (same timestamp); and a one-line change
to `Ops.scala` — the Scala the test natives bind to — recompiles them,
which the first cut (source timestamps only) could not do.
