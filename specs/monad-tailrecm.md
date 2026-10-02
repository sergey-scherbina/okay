# monad-tailrecm — `tailRecM` for every okay Monad

Status: in progress, 2026-10-02. Owner lane: `monad-tailrecm`.

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

- [ ] a default method on `Monad`: no instance has to write it, an
      instance with a native loop may override it
- [ ] a million iterations through an EAGER carrier (`Option`) and a
      deferring one (a program), right answer, no StackOverflowError
- [ ] short-circuit: an `Option` loop stops at the first `None`
- [ ] `ToCats.monad`: cats' `Monad` from any okay `Monad`, `tailRecM`
      the derived one; cats-laws `MonadTests` (its tailRecM stack-safety
      law included) hold on it

## Decisions

## Results
