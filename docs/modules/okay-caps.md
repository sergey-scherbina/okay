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
