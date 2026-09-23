# okay-frege

> Frege — a Haskell for the JVM — programs as okay programs. A thin
> Frege monad, `Prog`, whose operations are okay's; an okay driver walks
> it. Multi-shot handlers work, and no thread is involved. JVM;
> `org.frege-lang:frege` 3.25.153.

| | |
|---|---|
| `okay.frege.Prog` (Frege, src/main/frege) | `await`, `tell`, `perform op`, `liftIO io` — okay's freer tree written in Frege, with `Functor`/`Applicative`/`Monad`, so a Frege programmer writes ordinary `do`; `Operation a` is an okay operation typed by its answer |
| `Frege.stage(prog)` | a Frege `Prog ()` that awaits and tells, as an okay `Stage` |
| `Frege.run[F](prog)` | a Frege `Prog a` as `A ! F`: each `perform` runs as an operation of the row `F`, under whatever handlers run it |
| `Frege.Row` | whether a value from Frege is an operation of `F` — found for one signature, built with `|` for a union |
| `Ops` | the core effects' operations as values for Frege to `perform` (Reader, State, Throws, Choose) — one pure native each |
| project/Frege.scala | compiles `.fr` sources (forked, `-target 17`) before the Scala that reads them, or after the Scala they call |

## Why not lazy IO

The obvious bridge is a pure Frege function `[a] -> [b]` fed a list whose
tail thunks pull from okay. That is Haskell's lazy IO, and it fails the
way Kiselyov's iteratees paper says lazy IO fails: the FUNCTION decides
when to read (a `reverse` forces the whole input before the first
output), a thunk can outlive the resource it reads, an upstream failure
surfaces wherever a thunk happened to be forced — and forcing a thunk is
a synchronous JVM call that cannot suspend, so an okay upstream that
awaits or is `Async` cannot be stepped from inside one.

So okay's effects enter Frege as explicit operations of `Prog`, sequenced
by Frege's own `>>=`. The pure part of the Frege program stays lazy, and
that laziness is safe: no effect hides in a thunk.

## A stage, a program

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

okay's own effects, performed from Frege — each operation a value the
Frege module binds as one pure native (`okay.frege.Ops`, or your own
effect's the same way), TYPED by its answer: `Operation Long` is an
operation answering a `Long`, and `perform :: Operation a -> Prog a`
answers exactly that — using the answer as a `String` is a Frege type
error, not a `ClassCastException` at run time:

```haskell
pure native askOp okay.frege.Ops.ask :: () -> Operation Long
pure native getOp okay.frege.Ops.get :: () -> Operation Long
pure native setOp okay.frege.Ops.set :: Long -> Operation Long

readerState :: Prog Long
readerState = do
  env <- perform (askOp ())
  s   <- perform (getOp ())
  _   <- perform (setOp (s + 1))
  s2  <- perform (getOp ())
  return (env * 1000 + s2)
```

```scala
val prog = Frege.run[Reader % Long + State % Long, java.lang.Long](P.readerState.call())(
  using summon, Frege.Row.of[Reader % Long] | Frege.Row.of[State % Long])
val answer = !.run(State.handle(5L)(Reader.run(7L)(prog)))   // (6, 7006)
```

A single signature's row is found by itself; a union is spelled once
with `|`, because dotty does not infer the two sides of a union type
lambda (it answers `Nothing` for both).

## Multi-shot, because the continuation is a function

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

`Choose` resumes the program once per branch, and each resumption is a
call of a Frege function — nothing is shared between branches but the
pure program itself.

## Existing Frege IO: `liftIO`

```haskell
greetIO :: String -> IO String
greetIO who = return ("hello, " ++ who)

usesIO :: Prog String
usesIO = do
  g <- liftIO (greetIO "okay")
  tell g
  return (g ++ "!")
```

The driver runs a lifted action as ONE step. It never calls back into
okay, so nothing has to be suspended inside it — which is why no thread
is needed anywhere, and why a lifted action inside a multi-shot branch
simply runs once per branch, as the branch asks.

## What was built first, and dropped

The first cut lifted okay's operations into Frege's own `IO` as natives
and ran the Frege program on its own virtual thread, the thread being
the continuation (each `await`/`perform` a handoff to the driver). It
worked — a million-element recursive `IO` loop, an interrupt ending a
parked program — but it was one-shot (a thread cannot be copied, so
`Choice` had to be refused), ~10.5 µs per element, and a stage the
downstream stopped pulling left a parked thread that needed a `Cleaner`
to reclaim. `Prog` walked by the driver: multi-shot, 0.27 µs per step
(`TestFrege`'s PRICE line), nothing to reclaim.

## Building Frege sources

project/Frege.scala compiles `.fr` with the Frege compiler forked (it is
a whole compiler with its own statics), `-target 17` — Frege generates
Java and calls javac, which would otherwise emit the running JDK's major
(69 on the 25 this build compiles on) and refuse to load on 17 or 21.
`Frege.before(Compile)` for sources the configuration's own Scala reads
(the `Prog` library: its classes are a product and are mapped into the
jar), `Frege.in(Test)` for sources that call the Scala side.
