# okay-freer

The freer monad on its own, and its reading as continuations: the indexed
tree every `okay` program is, the CPS `Cont` that is the same tree, and
the machine that runs it — below the core, with no dependency, on the JVM,
Scala.js and Scala Native. The core (`okay`) is the library over it — the
effect tree `Free` at the unary signature, handlers, rows, `Effects`,
`Control` — and names the tree at its door, so a program written with
`import okay.*` sees everything as before (specs/freer-min.md, stages 29
and 31).

| | |
|---|---|
| `okay.freer.Freer[G, S, R, A]` | the indexed tree (Kiselyov–Ishii, with Atkey's indexes): `Return`, `Inject`, `Bind`, `Delay`; `resume` rotates it to a head form in constant stack; `Suspended`, `Mapped`, `defer`, `delay`; its `ParaMonad` |
| `okay.Cont[A, S, R]` | `(A => S) => R` as data: the tree at the shift signature, `shift` (a macro that reads its body), `/` runs it; the modes, `safe`, `direct`, `Monadic` |
| `okay.Delimited` | the machine: the stack of continuations typed per reset installation, nested runs, the strict `k` by re-execution or a fresh stack (`StackSwitch`, `StackRoom`, `StackPool`, the JDK 22 variant in this module's multi-release jar) |
| `okay.DirectCtx` | the evidence a `direct` block installs; shared by the direct DSL and both monads |
| `okay.Monad`, `ParaMonad`, `TailRecM`, … | the type classes the tree instantiates (Monad.scala), package `okay`, in this module |

Not here, on purpose: `Control`, the interface of `shift` and `/` every
monad of delimited continuations implements, with its `Cont` and `Func`
instances — the core's, common to this module's `Cont` and to `okay-cont`'s
machine. The machine's `Run` never names the core's `Free`: how an
operation leaves a run is the caller's `Delimited.Leaving[F, O]`, the
identity at `Unary[F]`.

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
