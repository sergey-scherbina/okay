## freer-base-stage2 - `Prog`: the index on a program; `NoPrompt` a compile error; okay-sql's transaction order in the types

Road 3 of specs/continuations-roadmap.md, built as the probe (4af08745)
said it could be. Additive throughout: `A ! F`, `Delim.reset`/`shift`
and every caller keep their spelling.

- `okay.Prog[F, A, S, R]` (src/main/scala/Prog.scala): an opaque
  facade over `Free[F, A]` with two phantom indexes — `diag` enters
  at any index, `transition` is the one named claim, `flatMap` joins
  end to end, `free` unlifts the diagonal only. Zero cost, asserted
  structurally (`diag(p).free eq p`, the same `Bind`). The opaque
  type sits INSIDE the companion (`Prog.Rep`), as `Cont`'s does: a
  top-level one is transparent to its package.
- `Delim.Stacked`: the prompt stack as a lexical given (`Stack`, `In`,
  `Has`), `delimited` (root, runs) / `reset` (nested, installs) /
  `shift` / `control` / `abort` over the real machine. TestProg: the
  probe's five positives answer the laws' values through the machine;
  a shift with NO reset, to a FOREIGN prompt, and to a prompt that
  ESCAPED its reset are `compileErrors` with a message naming the
  stack. `shift0`/`control0` stay unstacked (their index is the stack
  below the prompt — unpriced, said in the spec).
- okay-sql `Tx` (Tx.scala): `begin: Idle -> Open`, `commit`/`rollback:
  Open -> Idle`, statements at any index, `Tx.run` for `Idle -> Idle`
  only. TestTx (JVM — an `Async` run needs `CanBlock`): a recording
  fake `Sql` logs the steps in the promised order; nested `begin`
  (PgSql's `IllegalStateException`), orphan `commit`, a program left
  open and `begin().free` do not compile.
- The trap, found twice and documented: `okay.given`'s `Comonad[Id]`
  puts a lexical `.map` on everything, closer than the facade's —
  `import okay.Prog.{flatMap, map}` beside it (Prog.scala's doc,
  docs/guide.md, TestTx on purpose).
- Docs with literature: docs/guide.md "Typestate on a program"
  (Atkey 2009), docs/continuations-in-practice.md "The stack in the
  type" (Gunter–Rémy–Riecke 1995, Dyvbig–Peyton Jones–Sabry 2007,
  Kiselyov–Shan 2007 — DOIs checked against Crossref; the remembered
  Kiselyov–Shan DOI was another paper's), docs/modules/okay-sql.md.
  specs/freer-base.md boxes and "Stage 2 — BUILT"; roadmap road 3
  ticked; specs/delim-safety.md's stage-2 boxes answered by this
  road. Also filed on the way: backlog `widen-split` (the operator's
  question, from windows-stage-rerun-loses-pane).
