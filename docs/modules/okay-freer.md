# okay-freer

The freer monad on its own: the indexed tree every `okay` program is,
below the core, with no dependency, on the JVM, Scala.js and Scala Native.
The core (`okay`) is the library over it — the effect tree `Free` at the
unary signature, handlers, rows, `Effects` — and names the tree at its
door, so a program written with `import okay.*` sees `Freer` as before;
the module is where it lives (specs/freer-min.md, stage 29).

| | |
|---|---|
| `okay.freer.Freer[G, S, R, A]` | the indexed tree (Kiselyov–Ishii, with Atkey's indexes): `Return`, `Inject`, `Bind`, `Delay`; `resume` rotates it to a head form in constant stack; `Suspended`, `Mapped`, `defer`, `delay`; its `ParaMonad` |
| `okay.Monad`, `ParaMonad`, `TailRecM`, … | the type classes it instantiates (Monad.scala), package `okay`, in this module |

## Using it

Nothing changes for a program: the core's names are the module's.

```scala
def prog[M[_[+_], _]](using E: Effects[M]): M[Produce, Int] =
  E.perform[Produce, Int](1).flatMap(x => E.perform[Produce, Int](x + 1).map(y => x + y))
assertEquals(prog[Free].runWith, 3)
```

A macro or a tool that names the tree by its path names it at its home,
`okay.freer.Freer`, as `okay-direct`'s do; `okay.Free` and `okay.Unary`
are the core's.
