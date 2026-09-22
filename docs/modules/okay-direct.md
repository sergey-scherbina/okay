# okay-direct

The optional direct syntax — `direct[F] { ... }` — and its
compile-time macro implementation: the row-auto-coloring compiler
(`DirectCompiler`), the loop/emit/parallel/phase rewrites, and
`Condition`, the guard language the macro compiles against.

Nothing in the core or in `okay-async` depends on this module. A
program written against `!`/`flatMap` works without it; `direct`
exists to let ordinary-looking Scala code stand for one, at the cost
of a macro doing the coloring at compile time.

## Using it

```scala
lazy val myModule = (project in file("my-module"))
  .dependsOn(okay.jvm, okayDirect.jvm)
```

Reach for it when a program reads more naturally as straight-line
code than as an explicit `flatMap` chain. Everything it expands to is
ordinary `okay` — nothing here is a separate runtime.
