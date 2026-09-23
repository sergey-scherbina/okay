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
an sbt task (project/Frege.scala) before the Scala that calls them.

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

- [x] build: `project/Frege.scala` compiles `.fr` with the Frege
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
- **Answers cross as `Object`, typed by the Frege signature** — a Java
  cast native (`"(Object)"`), which the Frege compiler warns "will
  diverge" (a polymorphic result of `Obj -> a`); it is a cast and it
  returns. `null` is `Nothing` for `await` (`java.util.Objects.isNull`),
  so the driver knows no Frege Prelude type.
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
