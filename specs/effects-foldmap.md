# effects-foldmap — a program folded into any monad

Status: done, 2026-10-02. Owner lane: `effects-foldmap`.

## Goal

`Static` has `foldMap` into any `Selective`, `Proc` into any `Monad`; a
program `A ! F` had only `foldCont`, into `Cont`. Add the missing one:

```scala
extension [F[+_], A](m: M[F, A])
  def foldMap[G[_]](nt: F ==> G)(using G: Monad[G]): G[A]
```

on every `Effects` encoding, DERIVED (not a primitive): `foldCont` with
`S = G[A]`, each operation `G.flatMap(nt(e))(k)`, closed by `/ G.pure` —
`convert`'s shape with a monad in place of an encoding.

## Behavior

- [x] the operations land in G in program order, the answer through `pure`
- [x] a G that defers its continuation (another program, cats' `IO`)
      folds a 100 000-operation program without growing the stack
- [x] an eager G (`Option`) is correct and short-circuits on `None` —
      and is stack-safe too (see Results; the expected bound was wrong)
- [x] okay-cats: a program's operations interpreted straight into `IO`

## Decisions

- **Derived, in the extension block beside `runWith`** — a default
  method, so every encoding has it and none must implement it.
- **Stack: expected bounded by the carrier — REFUTED.** The plan was to
  write a depth bound for an eager G, whose `flatMap` calls the
  continuation at once. Measured first: no bound exists to write.

## Results

- A probe before the bound was written: `Option` and `Either` (an
  eager `Monad` declared in the probe) folded 20 000, 50 000, 100 000
  and 1 000 000 operations, left-nested binds and non-tail recursion
  (`effect(op).flatMap(x => go(i - 1).map(_ + x))`), all without a
  StackOverflowError. The continuation `k` handed to `Cont.shift` is
  resumed by `Cont`'s data machine, so an eager `flatMap` calling it
  at once adds no host frames per operation. TestFoldMap pins the
  million in both shapes; the method's comment says so instead of a
  bound.
