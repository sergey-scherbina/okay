# eager-carrier-depth — `tailRecM` and `foldMap` with no stack overflow, in principle

Status: done, 2026-10-02. Owner lane: `eager-carrier-depth`.
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

- [x] every claimed carrier: a million iterations on a 128 KB JVM thread
      (`Option`, `Either`, `LazyList`, the context monad, a program, ZIO,
      kyo pure and under `Env`; ZStream a hundred thousand), and core's
      on Scala.js and Native (TestStackSafeLoops)
- [x] `foldMap` into `Option`: a million operations, left-nested and
      non-tail, on a 128 KB thread and on Scala.js/Native
- [x] a strict monad with no `TailRecM` does not compile `tailRecM`, and
      the error names `TailRecM` (compileErrors)
- [x] cats' laws (`MonadTests`, its stack-safety law) on `ToCats.monad`
      from `Monad[Option]`, on all three platforms; an eager okay monad
      that brings its own loop: a million through cats' `tailRecM` on JS

## Decisions

- **The kyo instance is `Loop`, not `deferring`.** A pure kyo value maps
  at once (`<` is `A | Kyo`), so kyo's `flatMap` is eager on pure
  values; `Loop` is kyo's own constant-stack loop. Its inline expansion
  carries kyo's E221 ("recursive call used a default argument"),
  silenced on that one method with the reason.
- **What stays bounded, deliberately out of this lane:** `Cont`'s runner
  itself nests the host stack for nested OPAQUE bodies and on Scala.js
  has no fresh stack (specs/cont-stack.md, "JS: the bound"). TailRecM and
  foldMap no longer go through it for an eager carrier.

## Results

- core: TestTailRecM 7, TestFoldMap 4 (SmallStack 128 KB), and
  TestStackSafeLoops 2 on JVM, Scala.js and Native; okay-cats JVM 323,
  JS 253, Native 253 (TestToCatsDepth back in the cross set); okay-zio
  TestZioTailRecM 2; okay-kyo TestKyoTailRecM 2.
- Mutant: `TailRecM[Option]` as `deferring` (the flatMap recursion)
  fails both 128 KB tests with StackOverflowError.
