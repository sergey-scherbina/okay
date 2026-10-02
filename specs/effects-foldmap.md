# effects-foldmap — a program folded into any monad

Status: in progress, 2026-10-02. Owner lane: `effects-foldmap`.

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

- [ ] the operations land in G in program order, the answer through `pure`
- [ ] a G that defers its continuation (`A ! F` itself, cats' `IO`/`Eval`)
      folds a 100 000-operation program without growing the stack
- [ ] an eager G (`Option`) is correct, short-circuits on `None`, and its
      depth bound is written on the method
- [ ] okay-cats: a program's operations interpreted straight into `IO`

## Decisions

- **Derived, in the extension block beside `runWith`** — a default
  method, so every encoding has it and none must implement it.
- **Stack: the bound is the carrier's.** For an eager G every operation
  calls its continuation inside `flatMap`, so the host stack grows by a
  few frames per operation performed; okay's `Monad` has no `tailRecM`
  to escape that, and cats' answer (a required `tailRecM`) is what
  `ToCats` refused for the same reason. Written on the method, measured
  in the test.

## Results
