# monad-tailrecm — `tailRecM` for every okay Monad

Status: done, 2026-10-02. Owner lane: `monad-tailrecm`.

## Goal

okay's `Monad` had no `tailRecM`, so a loop over an arbitrary monad was
only as stack-safe as its `flatMap`, and `ToCats` could not give cats'
`Monad` (which demands it). Programs had their own (`!.loop`).

effects-foldmap measured that a continuation resumed by `Cont`'s data
machine adds no host frames even when an EAGER `flatMap` calls it at
once. So `tailRecM` can be DERIVED for every carrier:

```scala
def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B]
```

each iteration a `Cont.shift` whose body is `flatMap(f(s))(k)`, the
`Left` case looping inside `Cont`'s `flatMap`, closed by `/ pure`.

## Behavior

- [x] no instance has to write it: the class `TailRecM[F]` (Monad.scala)
      has a given for every okay `Monad` in its companion, delegating to
      the extension `M.tailRecM(a)(f)` in Effects.scala
- [x] a million iterations through an EAGER carrier (`Option`) and a
      deferring one (a program), right answer, no StackOverflowError
- [x] short-circuit: an `Option` loop stops at the first `None`
- [x] `ToCats.monad`: cats' `Monad` from any okay `Monad`, `tailRecM`
      the derived one; cats-laws `MonadTests` (its tailRecM stack-safety
      law included) hold on it, and a million iterations through an
      eager okay monad cats never heard of

## Decisions

- **A class in Monad.scala, the implementation in Effects.scala**
  (operator, mid-lane). The first cut was a default method on `Monad`,
  which put `Cont` into Monad.scala. Monad.scala now holds only
  `trait TailRecM[F[_]]` and a one-line companion given; the loop
  through `Cont` is an extension on a `Monad` instance beside `convert`,
  `!.loop` and `foldMap`.
- **The given in the companion, not top-level.** Top-level in
  Effects.scala it needed `import okay.given`, and `ToCats.monad` failed
  to resolve in a suite without that import. In `object TailRecM` it is
  in the implicit scope of `TailRecM[F]`, found with no import.

## Results

- TestTailRecM (4) and the cats suites (154, the laws included), green.
  The mutant that writes `tailRecM` as the obvious `flatMap` recursion
  fails both `Option` tests with a StackOverflowError; the program
  case passes under it, as it should, since a program's `flatMap`
  defers.

- **SUPERSEDED by eager-carrier-depth (2026-10-02).** The derivation
  through `Cont` is gone: it held a host frame per iteration on an eager
  carrier (128 KB JVM thread: overflow at 1 000; Scala.js: 300-1 000),
  and the million measured here ran on an 8 MB stack. `TailRecM` is now
  provided by each carrier, never derived; the class and `ToCats.monad`
  stay.
