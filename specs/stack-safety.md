# Stack safety — no unbounded stack recursion

## Overview

Operator rule, 2026-09-25 (AGENTS.md, "NO UNBOUNDED STACK RECURSION"):
a recursive method in okay or okay2 must be one of three things.

- TAIL: the compiler turns it into a loop, and `@tailrec` makes the
  compiler CHECK that. An unannotated loop is a loop only until the
  next edit.
- TRAMPOLINED: the recursive call is deferred, so the stack does not
  grow. It can be deferred into a `Free`/`Cont` node, a program's
  `flatMap`, a thunk or lazy cell, or a callback run later. Or the
  walk carries its own explicit stack or worklist on the heap.
- BOUNDED: the depth has a limit WRITTEN DOWN beside the method: a
  constant (`PullBudget`), a fixed arity, or a structure whose depth is
  fixed by construction. "Usually shallow" is not a bound. Nor is "the
  user would not build one that deep": a `where` folded out of ten
  thousand filters is an ordinary program.

Why: a JVM thread has a stack of roughly 10^4 frames. okay2's
`Producer.each` threw StackOverflowError at 200 000 productions. The
same text in okay was safe only because okay's `split` is `inline` and
put the call in tail position (producer-each-stack, okay2-split-at-rest).
The difference was invisible in the source.

## How recursion is found — the bytecode, not the source

Two tools, in `scripts/`. Both read JVM main classes after a compile
(`scripts/gate.sh "family jvm compile"`; for okay2,
`cd okay2 && ../scripts/gate.sh compile`).

- `scripts/tailscan.py <root>` finds methods scalac ALREADY compiled
  to a loop: tail-call elimination leaves a `goto 0` in both Scala 2
  and Scala 3. Each is mapped back to its `def` by line number and
  checked for `@tailrec`.
- `scripts/recscan.py <root> [skip-prefix]` finds recursion that is
  STILL on the stack. It builds a call graph per class, and a lambda is
  an edge from the method that creates it to its body. That edge is
  labelled by the call that consumes the lambda, which is the next
  invoke after its `invokedynamic`. An edge whose consumer defers the
  body is dropped:
  - a program's `flatMap`/`map`, or `Free.Bind`/`Delay` built inline;
  - `Cont`;
  - `Safepoint.defer`;
  - a `Waiter` callback.
  Every cycle left is stack recursion: direct (the method calls
  itself), lambda (through a closure run now, like `children.map(go)`),
  or mutual.

A source-level survey could not have found this. `each` read the same
in both cores and differed only in whether `split` was inlined.

## Inventory (2026-09-25, recscan over the two builds)

- okay: 332 methods. 132 direct, 73 through a lambda, 127 mutual.
  `specs/stack-safety-okay.tsv`.
- okay2: 63 methods. 9 direct, 24 through a lambda, 30 mutual.
  `specs/stack-safety-okay2.tsv`.

The largest groups in okay:

| module | rows |
|---|---|
| okay-codec | 78 |
| okay-direct | 34 |
| okay-ui | 31 |
| okay-py | 18 |
| okay-workflow | 16 |
| okay-js | 16 |
| okay-staging | 15 |
| core (`src`) | 14 |
| okay-sql | 13 |
| okay-stream | 12 |

The rest have single digits each.

The inventory still contains false positives, and each stage checks
its rows before it moves anything:
- a consumer that defers but is not yet on the tool's list (okay2's
  `Writer.effect`/`Take.effect`, ZIO `flatMap`, the fs2 `++`);
- a constructor that only stores the lambda.

35 okay rows are recursion inside a quoted macro splice
(`QuoteUnpickler`). They run at compile time over the user's source
tree. Whether that counts as BOUNDED is Decision 2 below.

## Stages

Each stage takes one group in BOTH cores where both have it, in this
order:
1. A test at a depth the stack cannot hold: 100 000 levels, or a
   structure built by a fold. The test must FAIL first.
2. The conversion: a loop with `@tailrec`, an explicit worklist, or
   the walk moved onto `Free`/`Cont`/`Eval`.
3. The rows removed from the inventory file, and a re-run of
   `recscan` showing they are gone.

A module that is bounded by construction gets its bound written beside
the method, and its rows are marked BOUNDED in the file instead of
deleted.

- [x] Stage 0 — tailrec-audit (2026-09-25): the rule, the two tools,
      `@tailrec` on the 68 okay methods scalac already looped (10 with
      an `again` wrapper for the call from inside `flatMap` or a thunk),
      and this inventory.
- [x] Stage 0b — okay2 (stack-safety-okay2-core, 2026-09-25):
      tailscan listed 25 methods; 18 now carry `@tailrec`, and 11 of
      those needed an `again` wrapper:
      - the loops that resume from inside a `flatMap`: Choice,
        Effects.translate, Produce.streamIn, State.zoomAt,
        Writer.expand, Source, Take.into, the Stm simulation, zio;
      - fs2, whose `++` takes its right side by name;
      - Json's merge-patch.
      The other 7 are not loops: 3 `while` loops the scanner misread,
      and 4 tree walks only partly in tail position (sql
      `collect`/`eval`/`fits`, `Tables.estimate`), left for stages 3
      and 4.
- [x] Stage 1a — okay core (stack-safety-core, 2026-09-25). Each test
      below ran RED on master first (StackOverflowError on a 128 KB
      stack, `SmallStack` in src/test/scala):
      - `Aggregate.topK`'s insertion is a `@tailrec` loop over a
        reversed kept prefix; before, it was k frames deep.
      - `Delim.split` walks the segment chain as a loop and wraps the
        captured part from a type-aligned `Wrap` of polymorphic frames,
        with no cast. A shift under 20 000 nested prompts had
        overflowed.
      - `Static.foldMap` is ONE loop over a type-aligned continuation
        (`Args`: `More`, `AppTo`, `Mapped`, `SelectE`, `SelectF`). All
        three nesting axes (a select's condition, an application's
        argument, a select's function side) overflowed at 3 000.
      - `Effects.reflect` is TRAMPOLINED by its target's `flatMap`:
        20 000 operations into `Eager` passed on master.
      - `sliding` is lazy.
      - `Distinct`, `Handler` and `Provide` are quoted macros (Stage 7).
- [x] Stage 1b — okay2 core (stack-safety-okay2-core, 2026-09-25):
      the same three as okay, in Scala 2, each RED first on a 128 KB
      stack (`src/test/scala-jvm/okay2/TestStackSafetyCore`):
      - `topK`'s insertion is a loop;
      - `Delim.cut` is a loop over a type-aligned `Wrap`. Scala 2 refines
        no method type parameter by a match, and `@tailrec` refuses a
        call with changed type arguments, so each step is a METHOD on
        the node (`Top`/`On` → `Unwound`) and the walk's changing type
        sits in the existential of `Walk`. No cast.
      - `Static.foldMap` is one loop over `Down`/`Up` with
        `AppTo`/`SelectE`/`SelectF` frames. It is still at `Any`,
        under the method's one existing claim.
      `Gen`'s loop (a `new Free.Bind`) and `package.go` (a `LazyList`)
      are deferred, not recursion.
- [ ] Stage 1c — `Cont` in both cores: its own spec, specs/cont-stack.md
      (compile-time trampolining of what the macro can see, a room
      carried as a value, a fresh stack for opaque bodies; measured and
      not yet landed).
- [ ] Stage 2 — codecs: okay-codec (78) and okay2-codec (15). Json is
      already `Cont`-trampolined; the other formats, Schema walks,
      Compat, Stubs and Policy are not.
- [ ] Stage 3 — streams and STM: okay-stream, okay-stm, okay2-stream,
      okay2-stm.
- [ ] Stage 4 — data codecs over values: okay-py, okay-r, okay-sql,
      okay-pg, okay-jdbc, okay-r2dbc. Two known suspects:
      - `PyCodec.enc`/`dec` and `RCodec.enc`/`dec` recurse per level
        of a recursive VALUE;
      - `Query.eval`/`collect` recurse per `And`/`Or` of a predicate
        built by a fold.
- [ ] Stage 5 — workflow: `Proc.go`/`nodes`, `Wf.go`, recursing per
      `Then` of a composed arrow.
- [ ] Stage 6 — UI trees: okay-ui, okay-ui-gtk, okay-js.
- [ ] Stage 7 — macros and staging: okay-direct, okay-staging,
      okay-optics `Fuse`, `ProcMacro`, per Decision 2.
- [ ] Stage 8 — the remaining single-digit modules.
- [ ] Stage 9 — the guard: a check that the inventory only SHRINKS
      (the `docs/snippet-debt.txt` discipline), run where the gate
      already compiles.

## Decisions

1. Deferred edges count as TRAMPOLINED only when the consumer really
   defers. The tool's list is the whole claim, and a consumer added to
   it needs its reason in the tool's comment.
3. A `Select`'s function side is folded before `select` is called
   (Stage 1a). Folding only builds a `G`, so the value is the same. A
   `G` whose `select` skips that side now receives it already folded,
   which is extra work and never a different answer. Evaluating it
   lazily would put the fold back on the stack inside `G`'s own
   `select`.
2. OPEN: recursion in a macro over the user's source. scalac itself
   recursed over the same tree to typecheck it, so our depth is at most
   the compiler's own, and a tree that deep had already stopped the
   compiler. That is a bound, but a borrowed one. Stage 7 either writes
   it down as the bound or converts the walks.
