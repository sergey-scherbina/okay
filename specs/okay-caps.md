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

## The guard (okay-caps-guard)

What the checker misses must not pass silently. Three parts:

- A transaction is CLOSED when its bracket ends — commit or rollback — and every use after that throws
  `Tx.Closed` ("transaction closed"): the leaks measured above (a LazyList, a kernel program, a caller without the
  checker) fail loud at the leak, not as a statement run outside any transaction.
- The checker for a whole module: `scalacOptions += "-language:experimental.captureChecking"` instead of the import
  in every file — measured to refuse what the import refuses.
- The rule, written where capabilities are explained: a capability kept an EFFECT (okay-tx's `Tx.resource`, an
  operation of the bracket's row) cannot leave its bracket — a program performing it outside has the operation in
  its row and no handler for it, a type error; a capability turned into a VALUE needs the checker, or this guard.

- [x] use after commit, after rollback: `Tx.Closed`, through a closure, a LazyList, a kernel program
- [x] the module-wide option refuses as the import does
- [x] the rule in docs/modules/okay-caps.md and docs/capabilities.md

Results: TestTxCapability 6 — the guard catches the closure, the LazyList (at `head`), the kernel program (at
`run`) and a transaction kept past a rollback; `-language:experimental.captureChecking` refuses the closure leak
with the same message as the import, and compiles the honest body. The AtomicBoolean of the guard does not
trouble the checker (a field of a `SharedCapability`).

## The kernel (okay-caps-core): okay-core's Cont capture-checked, measured

A probe on okay-core's branch `capture-checking` (fb69ca6; not merged): `import language.experimental.captureChecking`
in `Cont.scala` alone. What it took, and what it gives:

- Thirteen errors at first, each the checker seeing what the kernel hid: `Bind^{f}` — a node holds its function.
  Closed by a capturing self type (`this: Cont[R, I, O, A]^`), node parameters `Cont^` (`Bind`, `Map`, `Defer`,
  `Inject`, `Lift`, `Dollar`, `Lower`), results `^{this, f}` on `map`, `bound`, `down`, `defer`, `shift0`, `dollar`,
  and two lambdas' result ascribed. Nothing else in okay-core needs the checker.
- The bytecode: the same, instruction for instruction, in all 430 classes (`javap -c -p`); only the debug line
  numbers moved, by the import's lines. The machine is untouched; no benchmark can move.
- okay-core's suites all green (core 251, words 45, async 19, the Scala 2 module 6 — it reads the annotated TASTy —,
  cats 2, tofu 9).
- From okay-caps against it: a kernel program holding the transaction, carried out of the bracket by `map` or
  `defer`, is REFUSED ("Capability `tx` outlives its scope"). By `flatMap` — and so by a `for` — NOT: `flatMap` is
  `inline`, and a capture set in an inline method's signature is inlined into every caller as `@retains`, an
  experimental annotation — every user of the kernel would need `-experimental` (measured: okay-core's own
  `Control.scala` stops compiling). `transparent inline`, which would carry the body's type instead, breaks the
  `inline match` on `Lifts` and the private `down` it calls: a redesign of the kernel's most delicate method.
- A capture-checked caller pays: `.run` and an ascription `Int ! Pure` want a pure program, so an honest
  `p.map(_ => tx.execute(...)).run` is refused until `run` takes `(A ! Pure)^` and the caller writes `(Int ! Pure)^`.

Verdict: the kernel CAN be capture-checked at no run-time cost, but the guarantee stops at `flatMap`, the word
programs are written with — partial, and a partial static guarantee is not one to sell. Not merged. The guard
(`Tx.Closed`) stays the answer for kernel programs; the probe is kept on its branch, to be taken up when `@retains`
is no longer experimental (then `flatMap`'s signature carries `^{this, f}` directly) — backlog `okay-caps-next`.

## The rule, tested (okay-caps-rule)

okay-tx's `TestTxEscape`: the bracket's resource performed outside any bracket does not compile (no `Path` to a
reader of it in the row outside); a program performing it, carried out of the bracket and bound there, does not
compile (its row is the bracket's); the resource read into a value and closed over leaves — the closure answers
the connection after its commit — the boundary the checker or the guard covers.

## In okay-tx (okay-tx-session)

The guard where transactions are used: okay-tx's JDBC bracket. `Jdbc` hands the bracket's `Connection` out raw; a
closure keeping it runs after the commit on a connection closed — or, from a pool, given back to it and maybe in
another bracket's hands. `Sessions` beside it: the same transactor, the resource a `Session` — a
`caps.SharedCapability` wrapping the connection, `execute`/`queryInt` its words — closed when the bracket commits
or rolls back, before the connection is: a statement after it is `Session.Closed`, naming the leak.

- [ ] a session's statements in its bracket commit; a session kept past the commit, past a rollback: `Session.Closed`
- [ ] nested brackets one session; a retry a fresh one
- [ ] `Jdbc` unchanged
