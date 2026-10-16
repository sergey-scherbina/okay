# okay-caps

Tracked capabilities — Scala 3's capture checking — measured on okay, in a module of its own
(specs/okay-caps.md).

A capability is a value through which code may act: a transaction, a file system. Capture checking
(`import language.experimental.captureChecking`) tracks in a type which capabilities a value holds, and refuses one
that outlives its scope. The first one here is a transaction: `Tx.transaction(db)(tx => ...)`, or with the
transaction given, `Tx(db) { Tx.execute("...") }`. Inside the bracket a statement runs; a body whose result holds
the transaction does not compile — so a statement after the commit cannot be written, the hole
[typepedia](../typepedia.md) and specs/context-functions.md name for a phase left in scope.

What capture checking refuses, and what it does not, each measured by running the compiler on a snippet in
`TestTxCapability` (its errors come after the typer, where `compiletime.testing` cannot see them):

| the transaction carried out of its bracket by | refused |
|---|---|
| returning it | yes: "Capability `tx` outlives its scope" |
| a closure over it | yes |
| a `var` or a field keeping it | yes |
| a list of closures, an `Iterator` over it | yes |
| a `LazyList` over it | no — the standard library's `LazyList.map` does not track it |
| a kernel program (`A ! R`) holding it | no — okay-core is not capture-checked |
| a caller compiled without capture checking | no — the check is the caller's to switch on |

The module needs no `-experimental` on 3.9.0, and a dependent compiles as before. The kernel line is the one that
matters most for okay, whose programs are values run later: the guarantee reaches them only when the kernel's
types carry capture sets (backlog `okay-caps-next`).

## The guard: what the checker misses fails loud

A transaction is closed when its bracket ends, by commit or by rollback; a statement on it after that throws
`Tx.Closed`. So the three leaks the checker does not refuse still cannot run a statement outside a transaction —
they fail at the leak:

```scala
    val f = Tx.transaction(log)(tx => () => tx.execute("late"))
    assertEquals(intercept[Tx.Closed](f()).getMessage,
```

The compiler where it can, the guard always.

## The checker for a whole module

One compiler option instead of an import in every file — measured to refuse what the import refuses:

```
scalacOptions += "-language:experimental.captureChecking"
```

## The rule: keep a capability an effect

A capability kept an EFFECT cannot leave its bracket at all. okay-tx's connection is an operation of the bracket's
row (`Tx.resource`): a program that performs it outside has that operation in its row and no handler for it — a
type error, with or without the checker (okay-tx's `TestTxEscape`: the resource read outside a bracket — "No given
instance of type Path[Reader % Conn, T]"; a program performing it carried out and run — its row the bracket's, not
the one outside). The leak this module measures begins where a capability becomes a VALUE
held by a closure: there the checker guards it, and where the checker cannot see, the guard. So: an effect where
you can, a value with the checker where you must.
