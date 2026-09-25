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
- [x] Stage 2 — codecs: okay-codec (78) and okay2-codec (15). Json is
      already `Cont`-trampolined; the other formats, Schema walks,
      Compat, Stubs and Policy are not.
      - [x] 2a — the JSON family, both cores (stack-safety-json,
        2026-09-25). One real defect: past the threshold the STRICT
        reader skipped each unknown field by a direct call back into its
        field loop, so an object 40 deep with 200 000 fields the schema
        does not name threw StackOverflowError (TestJsonStrict, red
        first in okay and okay2). The skip is a loop now; the two rows
        are gone from both files. Every other JSON row is marked
        BOUNDED with its bound: one open container per call below
        `Codecs.NativeThreshold`, Cont past it, a product's field count
        for the absent-field step, the schema for an Option/iso chain.
      - [x] 2b — the CST projections outside the JSON family
        (cst-walks-remaining, 2026-09-25): `Yaml.values` (okay-codec),
        and okay-rag's `Split.tokens`, `Split.structural`'s descent,
        `Symbols.of`'s walk and `span`'s token run, each red first on a
        256 KB stack (TestYamlDepth at 5 000 levels, TestCodeDepth at
        3 000 nested definitions) and each now an explicit stack; the
        five rows are gone.
      - [x] 2c — Cbor, Edn and the wire (stack-safety-cbor-edn-wire,
        2026-09-25). THREE real defects, all in okay-codec's Wire.scala,
        each red first on a 256 KB stack at 20 000 levels (TestWireDepth):
        `WireCbor.decode` recursed per level of the bytes a worker sent,
        and caught only `IllegalStateException`, so a deep message was a
        StackOverflowError that escaped the decoder and took the reading
        thread; `WireCbor.encode` recursed per level of the tree; and
        `WireJson.whole`'s `damaged` walked the repaired tree of a damaged
        line recursively after a parse that takes any depth. All three are
        explicit stacks now (the decoder's frame is one open container:
        its remaining count and what it has read; a declared count past
        the bytes left is refused as damage), and the three rows are gone.
        Cbor and Edn were already threshold-plus-`Cont`: their ten rows are
        BOUNDED, and TestCborEdnDepth runs encode and decode of both at
        20 000 levels on the same small stack as the control. okay2 has
        neither format.
      - [x] 2d — the rest of okay-codec (stack-safety-codec-rest,
        2026-09-25): 38 rows, one hole. `JsonOptic.path` and
        `Policy.hide` descend the schema once per SEGMENT of a key a
        caller hands over, and through a sum at every level that descent
        is a search over the cases, not a loop — so on a recursive enum a
        100 000-segment key was a StackOverflowError (TestJsonOpticDepth,
        red first on a 256 KB stack; the product/Option chain alone had
        already been compiled to a loop). A key is bounded rather than
        walked: `JsonOptic.MaxSegments` = 64, past which `path` names
        nothing and `hide` refuses the key by name. Everything else is a
        written bound: Staged's fifteen walks run at COMPILE time over
        the user's type, a type met again cut by `seen`; Stubs' printers
        declare a product once (`Writer.declare`) and its key table has a
        `left` budget; TsTypes' recursive descent reads a declaration
        file a person wrote; Compat and Digest carry their own `seen`
        guards and stop at a product's name; `Schema.fold`'s `edge` is a
        lazy val answered by identity. Stage 2 is closed.
      - [x] okay-arrow (stack-safety-arrow, 2026-09-25): the stream reader's
        `parseField` recursed per level of the schema it read, and a
        100 000-deep schema overflowed on a 256 KB stack (TestArrowDepth).
        Fixed by a BOUND rather than a stack, because Arrow's reference
        implementation has one: C++ `IpcReadOptions::max_recursion_depth`,
        `kMaxNestingDepth` = 64. `Column.MaxNesting` = 64 is checked at
        every door a type comes in by: the schema in `parseField`, `Table`'s
        constructor, and Arrow Java's schema in `ApacheArrow.fromRoot`
        (on an explicit stack). The fourteen walks behind those doors
        are BOUNDED rows. Found beside it: `fromRoot` threw
        IndexOutOfBoundsException on any EMPTY root with a list column,
        because Arrow Java allocates a list's offsets with its first value.
        Fixed and tested.
- [x] Stage 3 — streams and STM: okay-stream, okay-stm, okay2-stream,
      okay2-stm (stack-safety-stream-stm, 2026-09-25). One real hole, in
      both cores: the flushing chunked feed (`Channel.feedFlushing`)
      continues DIRECTLY into the next step whenever a step sends
      nothing. A told element does that at most `Source.ChunkSize` times
      before a send's flatMap breaks the descent — but an EMPTY flush
      sends nothing, and a poller that flushes after every empty poll
      descended once per poll: 200 000 empty flushes were a
      StackOverflowError (TestFlushDepth, red first in okay-stream and
      okay2-stream). The feed counts its direct steps now and every
      `FlushBudget` (256) of them goes through a `pure(()).flatMap` node,
      the budget idiom Pipe's `PullBudget` already is. Everything else in
      the two modules is a written bound, mirrored across the cores: the
      Pipe/Take loops carry `PullBudget`; STM's `perform`/`runWithLog`
      nest per `orElse` in the transaction's own text (the code says so),
      its `attempt`s retry through a thunk `park` runs from a waker's
      frame, the Sim handler's `finish`/`loop` reach a `Sim.yieldNow` bind
      at the first operation; `Pipeline.once`/`chunks`/`depth` and
      `Tables.show`/`estimate`/`optimize`/`compile` walk a plan the
      program built by applying operators, its own text and never a
      peer's data.
- [ ] Stage 4 — data codecs over values: okay-py, okay-r, okay-sql,
      okay-pg, okay-jdbc, okay-r2dbc. Two known suspects:
      - `PyCodec.enc`/`dec` and `RCodec.enc`/`dec` recurse per level
        of a recursive VALUE;
      - [x] `Query.eval`/`collect` recurse per `And`/`Or` of a predicate
        built by a fold (stack-safety-query, 2026-09-25). CONFIRMED in both
        cores: a predicate made by `reduce(_ and _)` of 200 000 conditions
        overflowed in `render`, `collect` and `eval` (TestQueryDepth, red
        first in okay-sql and okay2-sql). All three are explicit stacks now.
        `render` writes left to right into one builder after a post-order
        pass marks the subtrees that render as nothing, so the `True`
        elimination is unchanged (pinned case by case). That also drops the
        old render's quadratic copy of the string built below each level.
        `eval` is a small continuation machine that keeps `&&`/`||`
        short-circuiting.
      - [x] `Typed.shapeOf` on a RECURSIVE row type (stack-safety-okay2-
        catch-up, 2026-09-25), found while auditing okay2's `Typed.fits`:
        a derived schema of a recursive type is a cycle of lazy thunks,
        and `shapeOf` matched the tree directly, so `Query.field[Tree, …]`
        was a StackOverflowError in BOTH cores where "not row-shaped" was
        the promise (TestTypedRecursive, red first in okay-sql and
        okay2-sql). A product met again on its own path is refused by
        name now, and every other Typed walk (`tpe`, `fits`, `decode`,
        `encode`) is over the finite Shape that builds. The same lane
        wrote the bounds of okay2's other catch-up rows, and of their
        Scala 3 twins: jdbc's `valueOf`/`arrayOf` per DIMENSION of a
        database array (declared in the DDL, Postgres MAXDIM = 6),
        `jdbcOf` per level of a parameter the program built,
        SparkSchema's four walks per level of a `ColType` whose recursive
        products Columns already cuts to one Json column. The fs2/zio
        interop `again` loops were never stack recursion: they recurse
        through fs2 `++`/`flatMap` and `ZIO.flatMap`, the library's own
        lazy bind, and recscan now knows those (and an implicit evidence
        fetched between a thunk and its call, `NotGiven.default`, is no
        longer read as the thunk's consumer).
      - [x] 4a — the SQL family (stack-safety-sql-family, 2026-09-25):
        okay-sql's Typed and Row, okay-pg, okay-r2dbc. One real hole:
        okay-pg's `parseArray` recursed per `{` of a literal read OFF THE
        SOCKET, and a 100 000-deep one was a StackOverflowError in the
        connection reader (TestPgArrayDepth, red first on a 256 KB stack).
        Fixed by the bound Postgres itself has — MAXDIM = 6
        (src/include/utils/array.h): a server never sends a deeper
        literal, so one past `PgSql.MaxDim` is refused by name as damage
        on the socket. `Row.toParams`' walk over the HMap tuple is a loop
        (its row is paid). Every other row is a written bound: Typed's
        walks are over the finite Shape (a recursive product refused,
        4-catch-up above); pg's `colType`/`decodeCell`/`valueOf` per level
        of a catalogue type, which Postgres keeps finite (a composite
        cannot contain itself, 42P16); pg's literal writers and r2dbc's
        `javaOf` per level of a value the program built; r2dbc's `valueOf`
        per dimension of a driver array. Left in stage 4: okay-py and
        okay-r (4b).
      - [x] 4b — okay-py and okay-r (stack-safety-py-r, 2026-09-25). The
        VALUE walks were the holes, and every one was red first at
        200 000 levels: `PyCodec.enc`/`dec` and `RCodec.enc`/`dec` recursed
        per level of a value of a recursive type (`Link(next:
        Option[Link])`), and `Shape.json`'s Json <-> PyValue conversions
        per level of what a worker sent. The codecs take the threshold-
        then-`Cont` road now (a direct call below `Codecs.NativeThreshold`,
        `encC`/`decC` past it, a `Held[Y]` pair carrying a field's schema
        and value through `eachField` without a cast; the scalar arms are
        one `encScalar`/`decScalar` both roads end in, so neither road
        calls back into the other). Two more things that road found: the
        decode PATH was a string grown per level (`s"$at.$name"`),
        quadratic in the depth, and a 200 000-deep value ran out of HEAP
        before it ran out of stack — it is a linked `At` now, rendered
        only into a message; and a refusal printed the value it met with
        `toString`, which recurses on a deep one — `describe` says "a dict
        of N keys" instead. The Json/PyValue conversions and the workers'
        ref renamings (`in`, `out`, `local`, `refsIn`) share one bottom-up
        walk on an explicit stack, `Walk.up`, behind
        `PyValue.refs`/`rebuild`/`rebuildE` (TestPyValueWalk). The replay
        loops of `SupervisedWorker` and `RSubprocess` are loops (a durable
        run replays as many steps as it journaled), and
        `PyFacade.scalaType` peels an annotation's wrappers in a loop.
        The type printers (`goType`, `haskellType`, `rustType`) stop at a
        product, so a recursive schema cannot loop them; `TsFacade.show`
        walks a declaration a person wrote; `RSubprocess.startWith`'s
        respawn is a stored thunk. Eleven rows are paid, seventeen carry
        their bound. Stage 4 is closed.
      - [x] The stage-9 catch-up rows of the same modules, in both cores
        (stack-safety-catch-up-okay2, 2026-09-25). Each gets a depth test
        first, red on the recursion:
        - `JdbcSql.valueOf`/`arrayOf` walk a driver's nested arrays, and
          `jdbcOf` walks a user's `SqlValue`. A VALUE has no bound, so
          all three become explicit stacks.
        - `Typed.fits` compares two `SqlType` trees. It becomes a
          worklist of pairs, since `fits` is an AND over them.
        - `SparkSchema.dataType`/`struct`/`value`/`rowOf` walk a
          `ColType`, and `value` walks the value in step with its type.
          They get a BOUND like okay-arrow's: a type deeper than 64
          levels is refused at the door (depth measured on an explicit
          stack), so the walks recurse at most that deep.
        - The fs2/zio `again`s are called from the library's own lazy
          `++`/`flatMap`, so they are TRAMPOLINED. A test drives a
          million non-Writer operations through each, the path the
          million-tells tests never took.
        - RESULT. Red first in the okay core: TestJdbcDepth and
          TestTypedDepth overflowed at 200 000 levels, and TestSparkDepth
          saw a 65-level type go through. In okay2 the same tests ran
          against a mutant that put `fits`'s recursive call back, and it
          overflowed.
        - It landed ON TOP of stack-safety-okay2-catch-up (the item
          above), which had audited the same rows the same afternoon and
          written bounds for them. Two of those bounds did not hold, and
          this lane replaced them with code:
          - `jdbcOf` "per level of a parameter the program built". A
            program can build one 200 000 deep, and it overflowed.
          - SparkSchema "per level of a ColType". A ColType is public and
            nothing refused a deep one; now `MaxNesting` does.
          The rows: 7 paid in each core, 4 BOUNDED in each (the Spark
          walks under their new names). The `again`s needed no row,
          because recscan already knows those binds, and the new tests
          prove it at 200 000 operations. No UNAUDITED row is left in
          okay2.
        - FOUND on the way: `java.util.ArrayDeque` on Scala.js 1.22
          answers NULL from a non-empty deque. The first gate ran
          `fits`'s worklist on it, and the 200 000-deep test failed on
          JS only, with `MatchError: null`. A probe pushed and popped
          (Int, Int) pairs with 66 667 left over, and draining them gave
          192 nulls; JVM and Native gave none. The cross `fits` uses
          `scala.collection.mutable.Stack` now. The JVM-only walks (jdbc,
          Spark) keep the JDK deque. Filed upstream-side as
          `scalajs-arraydeque-null`.
- [x] Stage 5 — workflow: `Proc.go`/`nodes`, `Wf.go`, recursing per
      `Then` of a composed arrow (stack-safety-workflow, 2026-09-25).
      Every walk of a `Proc` term — `foldMap`, the drawer `go`, `nodes`,
      and `Wf`'s journal walker `go`/`loop` — descends once per node,
      and the term is what the program COMPOSED: its own text, or a fold
      over steps it chose. That is the bound written on the six rows
      (and okay2's two), and Decision 7 says why it is a bound and not a
      fix: the arrow's GADT (`Then[X, Y, Z]`) threads an existential
      type through every level, so an explicit stack over it is a
      redesign of the fold, not a rewrite of a loop — and no consumer
      composes thousands of steps dynamically today. The `Iter` round
      recurses through `G`'s own flatMap, a value at every program row.
      ProcMacro's rows are stage 7 (compile time).
- [x] Stage 6 — UI trees: okay-ui, okay-ui-gtk, okay-js
      (stack-safety-ui, 2026-09-25). 49 rows; the tree walks themselves
      are bounded by the program's own view (a value from outside reaches
      the tree as a flat list of lines or one level of a drill, never as
      nesting), the okay-js rows are a macro over the user's source, and
      three walks were holes because they are fed from OUTSIDE the
      program — each red first at its own depth (TestUiDepth):
      `Form.focusAt` descended once per segment of a browser's dotted
      path (`editAt`, its writing twin, had been trampolined for exactly
      that reason; the reading twin recursed inside a `flatMap`) — a
      tail loop now, 100 000 segments; `Sessions.segments` recursed once
      per connection that CLOSED in a session's journal, which grows with
      the session's life — a loop now, 200 000 closes; and
      `JsonEditor.outline` recursed per level of the document under edit
      — preorder on an explicit stack now, 100 000 levels, with the path
      held reversed so a level costs one cons rather than a copy of the
      path so far, and the indentation capped at 64 levels (a line that
      begins with a screenful of spaces says nothing more).
- [ ] Stage 7 — macros and staging: okay-direct, okay-staging,
      okay-optics `Fuse`, `ProcMacro`, per Decision 2.
- [ ] Stage 8 — the remaining single-digit modules.
- [x] Stage 9 — the guard: a check that the inventory only SHRINKS
      (the `docs/snippet-debt.txt` discipline), run where the gate
      already compiles (stack-safety-guard, 2026-09-25).
      `scripts/recscan-check.sh [--since <ref> | --all] [--write]`, which
      `gate.sh` runs after every GREEN as `--since master`. It refuses
      NEW (a recursion recscan finds that no row names, keyed by file and
      def since lines move), BARE (a row the diff added with nothing in
      its sixth column) and PAID (a row whose recursion, or whose file,
      is gone; `--write` deletes exactly those). Only the modules whose
      main sources the diff touched are scanned, about 3 s each, and a
      module with no classes, or with an UNCOMMITTED edit newer than its
      classes, is skipped and named, because stale classes lie in both
      directions. (Corrected by stack-safety-arrow: the first cut compared
      every source's mtime, and a rebase gives committed files new mtimes
      that zinc rightly ignores, so it skipped a module the same gate had
      just compiled.) `--all` is the
      whole build (about 9 s for okay2 and a minute for okay).
      recscan.py itself learned two things for it: `RECSCAN_ONLY`, and
      reading only the NEWEST `scala-*` classes of a target, since a
      module that moved to 3.9.0 still has its 3.7.4 classes, and those
      report rows for code that no longer exists.
      Tested by mutants through the real gate. A recursive def added to
      the core was RED (NEW), its row without a reason was RED (BARE), the
      row with one held, and after the def was deleted the row was RED
      (PAID, file gone) until `--write` removed it. That deletion
      compiles nothing, which is why the step runs on warm gates too.
      A first cut of `--all` read an okay2 checkout nobody had compiled
      as "56 rows paid", and `--write` would have emptied the inventory.
      The freshness check now covers `--all` as well.
      THE FIRST RUN named 30 recursions that landed after the inventory:
      16 in okay (okay-arrow, okay-py's `ArrowFrames`, `ContMacro`) and
      14 in okay2 (okay2-sql, -jdbc, -spark, the fs2/zio interop). They
      are rows marked UNAUDITED, each pointing at its backlog item
      (okay-core/stack-safety-catch-up,
      okay2/modules/stack-safety-catch-up-okay2). Six rows were paid,
      because recscan had learned to see their LazyList tail or `new
      Free.Bind` as deferred, and they are deleted.

## Decisions

7. A Proc term's depth is the program's, and the walks over it stay
   recursive (stack-safety-workflow, 2026-09-25). The shape that made
   Query's predicate a defect — `steps.reduce(_ andThen _)` over a list
   the program chose — exists here too, and it is NOT fixed, because
   `Proc.foldMap` and `Wf`'s walker are typed folds over a GADT whose
   `Then[X, Y, Z]` hides Y at every level: an explicit stack would have
   to carry that existential in a frame, which is a redesign of the
   fold, and the drawers and `nodes` would follow. What is written
   instead is the bound (the term is the program's own composition) and
   the trigger for revisiting: a consumer that composes thousands of
   steps dynamically, which none does today (backlog
   okay-core/proc-deep-composition names the shape and the measurement
   to take first).

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
   - DECIDED for `ContMacro.rewrite` (stack-safety-catch-up,
     2026-09-25): the borrowed bound is written down, because it was
     MEASURED rather than argued. `scripts/probe-contmacro-depth.sh <n>`
     writes one `shift` whose body is a tail `if … else if …` chain n
     levels deep, the shape `rewrite` follows one frame per level. On
     Scala 3.9.0 and sbt's compile thread (-Xss8m):
     - 2 000 and 2 075 compile, and the bytecode calls
       `Cont$.tailShift`, so the macro rewrote every level;
     - 2 150, 2 300, 2 450, 2 600, 3 000, 3 500 and 4 200 overflow in
       PostTyper;
     - 4 800 overflows in Typer, and 10 000 in the parser.
     PostTyper, Typer and the parser all run BEFORE the Inlining phase
     where the macro expands, and no overflow trace holds a ContMacro
     frame. So any tree the compiler lets through, `rewrite` walks. A
     future Scala that changes this re-runs the probe: a ContMacro frame
     in an overflow is the signal to convert the walk. The other stage-7
     macros stay open under this decision.
