# okay-caps: tracked capabilities (Scala 3 capture checking), measured

`specs/context-functions.md` (E3) and `docs/capabilities.md` name a hole: a transaction's phase given in scope
stays in scope after its commit — use-after-commit compiles — "discipline, not types, until capture checking".
Capture checking (`import language.experimental.captureChecking`) tracks in a type which capabilities a value
holds and refuses one that outlives its scope. This lane measures what it gives okay on Scala 3.9.0, in an
optional module of its own, before anything else is built on it.

## The first capability: a transaction

`Tx`, a `caps.SharedCapability`; `Tx.transaction(db)(tx => ...)` / `Tx(db) { ... }` (the context-function form)
the bracket — begin, the body, commit, rollback on failure. Inside, `tx.execute(sql)`. A body whose result holds
the transaction does not compile.

## Questions, and how they are answered

1. The flags it needs on 3.9.0 — by compiling the module.
2. Whether it infects a dependent — a caller compiled without capture checking, against the module's classes.
3. Which leaks it refuses, which it does not — each a snippet run through the compiler in a test (capture checking
   runs after the typer: `compiletime.testing.typeCheckErrors` never sees its errors, so the tests run
   `dotty.tools.dotc.Driver` on the snippet, the module's classes on its classpath).
4. Its seam with the kernel — a program `A ! R` built inside the bracket, holding the transaction, returned.

## Behavior

- [x] a body using the transaction compiles and commits; a failure rolls back
- [x] refused: the transaction returned, a closure holding it, a field or var keeping it, a list of closures, an
      iterator over it
- [x] the holes, pinned as compiling (a test that flips when a compiler closes one): a caller without capture
      checking, a `LazyList`, a kernel program
- [x] the context-function form: `Tx(db) { Tx.execute("...") }`

## Decisions

- An optional module, JVM only: its tests run the compiler. The module's own code needs no `-experimental`
  (measured): a dependent compiles as before.

## Results (Scala 3.9.0)

1. Flags: none beyond the import in the files that use it; no `-experimental`.
2. Infection: none — a snippet compiled WITHOUT capture checking against the module's classes compiles, and leaks
   freely: the guarantee is the caller's to switch on, file by file.
3. Refused, "Capability `tx` outlives its scope: it leaks into outer capture set … owned by value …": the
   transaction returned; a closure over it (plain and context-function forms); a list of closures; an
   `Iterator.map` over it. A `var` keeping it: refused ("a field `keep` with `any` in its type need to be put in an
   object that extends Capability"). The standard library's List and Iterator track captures.
4. Not refused: `LazyList.from(1).map(_ => tx...)` — LazyList does not track its function's captures; a kernel
   program, `Cont.pure(1).map(_ => tx.execute(...))`, returned from the bracket — okay-core is compiled without
   capture checking, so `Cont`'s closures carry no capture set and the program looks pure. For okay this is the
   line that decides: programs are values run later, exactly the case the checker must see.

Next (backlog `okay-caps-next`): capture sets on the kernel's types, a probe in okay-core first; a handler
providing a capability in direct style; an agent sandbox (okay-agent) — a snippet typed by its row of effects,
capture-checked.
