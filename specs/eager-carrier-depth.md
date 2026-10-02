# eager-carrier-depth — `tailRecM` and `foldMap` with no stack overflow, in principle

Status: in progress, 2026-10-02. Owner lane: `eager-carrier-depth`.
Operator's bar: "полный трамплининг — чтобы не было никакого переполнения
в принципе". Supersedes the stack claims of effects-foldmap and
monad-tailrecm.

## What was wrong (measured)

monad-tailrecm DERIVED `tailRecM` for every okay `Monad` through `Cont`:
each iteration a `Cont.shift` whose body is `M.flatMap(f(s))(k)`. For an
EAGER carrier (`Option`, `Either`, a strict box) `flatMap` calls `k`
before it returns, so every iteration runs INSIDE the previous one's
`flatMap`. The earlier "a million through Option" ran on sbt's `-Xss8m`
or a forked main thread, where `Cont` moves to a fresh stack when the
current one runs out (specs/cont-stack.md). On a 128 KB JVM thread it
overflows at 1 000 iterations; on Scala.js, which has no fresh stack,
between 300 and 1 000. `foldMap` into an eager carrier had the same
shape.

## Why no generic derivation can be fixed

With only `pure` and `flatMap`, an eager carrier's `flatMap` may need
its continuation's RESULT to build its own (an eager `Writer` appends
its log after `k` returns; a `List` runs `k` many times). No wrapper can
turn that into a loop without the carrier's help. That is Phil Freeman's
point in *Stack Safety for Free* (2015) and the reason PureScript has
`MonadRec` and cats makes `tailRecM` part of every `Monad`.

## Design

- **`TailRecM[F]` is provided by the CARRIER, never derived from
  `flatMap`.** The generic given is removed: a monad with no `TailRecM`
  is a compile error naming the class, never an overflow at run time.
- **Instances that loop in constant stack:** `Option`, `Either[E, *]`,
  `LazyList` (an explicit stack, lazily), the context monad `E ?=> *`
  (a while loop under the context), programs `A ! F` (`!.loop`, whose
  recursion sits in a `Bind` the interpreter runs). In the interop
  modules: `IO`/`Eval`/any cats `Monad` (their own `tailRecM`), ZIO and
  ZStream (their `flatMap` defers to their run loop), kyo (`Loop`).
- **`TailRecM.deferring`** writes the obvious `flatMap` recursion, for
  a carrier whose `flatMap` does NOT call its continuation before
  returning — an explicit claim at the instance, checked by a test.
- **`foldMap` is `tailRecM` of the target**, as cats' `Free.foldMap`
  is: each step resumes the tree once and answers `Left(rest)` or
  `Right(a)`. So a fold into G is exactly as stack-safe as `TailRecM[G]`.
- `M.tailRecM(a)(f)` stays as spelled: it asks the `TailRecM[F]`.

## Behavior

- [ ] every claimed carrier: a million iterations on a 128 KB JVM thread
      (`SmallStack(128)`), and on Scala.js and Native (scala-cross)
- [ ] `foldMap` into `Option` and into a program: a million operations
      on `SmallStack(128)` and on JS
- [ ] a strict monad with no `TailRecM` does not compile `tailRecM`/
      `foldMap`/`ToCats.monad`, and the error names `TailRecM`
- [ ] cats' laws (`MonadTests`, its stack-safety law) on `ToCats.monad`
      from `Monad[Option]`, on all three platforms

## Decisions

## Results
