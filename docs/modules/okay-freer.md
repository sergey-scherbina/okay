# okay-freer

The freer monad on its own: the tree every `okay` program is, below the
core, with no dependency, on the JVM, Scala.js and Scala Native. The core
(`okay`) is the library over it — handlers, rows, `Effects` — and names
the tree at its door, so a program written with `import okay.*` sees
`Free`, `Freer` and `Unary` as before; the module is where they live
(specs/freer-min.md, stage 29).

| | |
|---|---|
| `okay.freer.Freer[G, S, R, A]` | the indexed tree (Kiselyov–Ishii, with Atkey's indexes): `Return`, `Inject`, `Bind`, `Delay`; `resume` rotates it to a head form in constant stack |
| `okay.freer.Free[F, A]` | the effect tree at the unary signature `F`, every index `Unit`; the four names at that arity, `fold`, `loop` |
| `okay.freer.Unary` | the bridge from a unary effect into an indexed row, and its extractor |
| `okay.freer.DirectCtx` | the evidence a `direct` block installs; the colouring conversion is in `Freer`'s companion |
| `okay.Monad`, `ParaMonad`, `TailRecM`, … | the type classes the tree instantiates (Monad.scala), package `okay`, in this module |

## Using it

Nothing changes for a program: the core's names are the module's.

```scala
def prog[M[_[+_], _]](using E: Effects[M]): M[Produce, Int] =
  E.perform[Produce, Int](1).flatMap(x => E.perform[Produce, Int](x + 1).map(y => x + y))
assertEquals(prog[Free].runWith, 3)
```

A macro or a tool that names the symbols by their path names them at
their home, `okay.freer.Free` and `okay.freer.Freer`, as `okay-direct`'s
do.
