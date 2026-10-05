# cont-js-depth — the machine apart, and no stack overflow in principle

Status: stages 1-4 done (4: re-execution, 2026-10-05), differential oracle landed. Owner lane: `cont-js-depth`.
Sprint item of the same name; follows specs/cont-stack.md and
specs/freer-kont.md.

## The operator's decisions (design conversation, 2026-10-02)

- **The whole machine goes into `Delimited`**, as it is: continuations
  are DATA (a captured segment resumed by an instruction), indexed by
  their answer types, multi-shot.
- **The strict `k` is a BRIDGE**: a body that receives its continuation
  as a host function `A => S` (`Cont.shift((A => S) => R)`) is the one
  place a run nests the host stack. It is Cont.scala's, not the
  machine's.
- **Not a compile error for opaque bodies** (refused): another solution
  is wanted for the bridge.

The reasoning that led there: in a machine whose `k` is data, resuming
is an instruction and nothing nests, on any platform — "no overflow in
principle" is a property of the construction. Only a body that calls a
strict `k` and waits for its answer forces a nested run; Scala.js has no
second stack to put it on, and compiles every method — `Option.flatMap`
included — as a plain JS function on the engine's stack (no CPS, no
generators outside `js.async`). The Scala.js LINKER sees the whole
program's IR, so "opaque" is the macro's limit, not the linker's
(cont-stack Decision 6).

## The census: which bodies stay opaque, and which of them nest

A probe in ContMacro (an `info` at each opaque leaf; not committed)
over every module: **76 opaque sites, 49 in main code**, 74 "unreadable"
and 2 "not a literal". Read one by one, most do NOT nest, because their
answer `S` is itself lazy and `k` is not called before the body returns:

- `k` into a program's `flatMap` — `perform(e).flatMap(k)` (Effects'
  `handle`, `convert`), `Writer.tell(w).flatMap(_ => k(()))`,
  `produce(w)…` (Generate, Source), `q.flatMap(a => k(a)…)` (Shift);
- `foldM` over choices (Choice, Prob): `k(x)` answers a program;
- `k` stored for later (Handler's `resume.k = k`, PWizard's callback);
- a lazy cons (`w #:: _(())`, Generate).

The ones that DO nest are state passing through a function answer:
`Zoom.scala` (okay-optics, 4 sites: `k => s1 => … k(x)(set(s1, a2))`)
and `PWizard.scala` (okay-ui, 3: `k => s => k(s)(s)`). ANSWERED: PWizard
runs on data and a zoom by a lens has `Threaded.zoom` (stage 3a below);
`PState.get`/`set`'s function answer is applied by a loop since
cont-fun-answer (2026-10-03, `PState.Bounce`), on every platform.

## Stages

1. **The machine apart** — DONE: `Frames`, `Stack`, the loop
   (`Frames.run`/`machine`/`enterAt`), `Cont0 = Shift0 | Reset0` with
   its delimiters, and `Rev` moved verbatim from Cont.scala to
   Delimited.scala; Cont.scala is the one-prompt facade and the
   strict-`k` bridge. No behaviour changed.
2. **The machine's own suite** — DONE: the DPJS law suite
   (TestDelimited, on the machine and the reference) moved to
   scala-cross, so it runs on JVM, Scala.js and Native; TestDelimitedDepth
   (scala-cross) through `Delimited` alone — a million captures whose
   bodies USE their continuation's answer (`k(1)` then `+ 1`, as data), a
   million nested delimiters, a million left-nested binds, and every one
   of 20 levels resumed twice (2^20 runs) — green on all three, and on a
   128 KB JVM thread (TestDelimitedDepthSmallStack). The answers are
   formulas checked on the reference first; the reference is CPS with no
   trampoline (its host stack grows per STEP of a run), so it checks them
   at small sizes only. MEASURED: with `k` as data the machine holds no
   host frame per level on any platform — the bound is the bridge's.
2b. **The reference as an ORACLE** (operator: "что он даёт?" — it gave
   little while it only re-ran hand-written laws): TestDelimitedDifferential
   (scala-cross) generates programs over `Delimited` — three prompts,
   `reset`/`dollar` with a `ret`, `shift`/`shift0`/`abort`, bodies that
   resume `k` once, twice, never, then continue, or resume with a
   computation — runs each on the machine and on the reference and
   compares the outcome, `NoPrompt` included: 4 500 programs a run, green
   on JVM, Scala.js and Native. MEASURED its worth: a type-correct mutant
   of the machine's fast path (capture at ANY delimiter right under the
   live segment, its prompt unchecked) passes the hand-written laws and
   the depth suite (15 of 15) and fails all three differential sets.
3a. **The census's nesting sites off the bridge** — DONE (threaded-zoom):
   `PState.Threaded` gained `zoomWith` (an `Op.Zoom`; `run` keeps the
   outer program on a TYPE-ALIGNED waiting stack, no cast, the
   four-parameter lens's type change kept) and okay-optics' lens spelling
   `PState.Threaded.zoom(lens)`; `PWizard` runs on data (`Get`/`Put`/
   `Show` operations, a loop to `Machine`, the loop resumed from the
   `Showing` callback), its names and syntax unchanged. A million nested
   zooms on JVM (and 128 KB), Scala.js and Native; a million wizard
   steps between two asks on 128 KB; full affected gate 9 479 tests.
   The shift road's `PState.zoom` stays, the bridge, marked as such.
2A. **`Effects.handle` as a translation onto the machine — REFUTED as
   written** (handle-on-machine, 2026-10-02). The probe: the handler a
   `Delim` prompt, an operation of `F` a `shift0` to it with `k` as data,
   an operation of `G` forwarded, run by `Delim.run`; it agreed with
   `Effects.handle` on every answer (a capture per handled operation,
   and a clause resuming twice). Measured on one pre-built program —
   10 000 operations, every 10th handled with a capture, the rest
   forwarded through a real effect (`Produce` is `Id`, which every row
   contains, so no machine can run it) — alternated, one lane per run,
   quiet box: **306.5 / 305.6 µs against 175.7 / 176.5 for the fold,
   1.74x; 3 398 330 B against 1 958 097, 1.74x** (history.d
   handle-on-machine). The fold road stays: its strict `k` is the
   bridge, but the census showed its answer is a lazy program, so it
   never nests. The probe code is deleted; the numbers are the record.
   Where the 1.74x would have to come from, if anyone revisits: the
   rewrite builds a second tree (a `flatMap` per operation), and every
   forwarded operation leaves the machine and re-enters it.
2c. **One door into the machine** — DONE (delimited-one-door, operator:
   "Одна дверь в машину … чтобы не использовался Frames напрямую"):
   `Delimited.runHead` is the only start of the loop; the strict `k`
   resumes as `runHead(k(x))`; the reference checks that door
   (specs/delimited.md). A probe of the bridge now has one place to act.
3b. **A program-answered body's `k`, lazy** — DONE (cont-program-answer,
   operator: "можешь решить эту проблему?" on way 1 of the bridge): when
   `S` is a program and the body calls `k` itself (outside any lambda),
   `ContMacro` emits `Cont.programLeaf`, whose `k(a)` is
   `Delay(ownedFlat(k(a)))`: forced by any interpreter (a bounded run,
   its answer the program that goes on), stepped into by a running
   machine, which continues into that answer in its own loop (`Own`'s
   `flat`). Not handle-on-machine's 1.74x: nothing is translated (`Free`
   IS the machine's tree at a narrower row) and no operation leaves and
   re-enters. A million nested such bodies on a 128 KB JVM thread, on
   Scala.js and Native, forced and stepped into; the strict leaf on the
   same program: StackOverflowError on 128 KB, "Maximum call stack size
   exceeded" on Scala.js (watched first). One site in the whole tree
   changes form (TestHandleForward's multi-shot handler, its answers and
   forwarded order unchanged); every library body passes `k` on or
   calls it inside a lambda, and keeps its leaf. Contract: host effects
   after `k(a)` run before `k`'s rest (docs/cont-stack.md).
3. The bridge on Scala.js: the nesting shapes of the census (state
   passing) and user code — candidates: the function answer applied by a
   loop (DONE for PState, cont-fun-answer: `PState.Bounce`), a link-time IR transform
   of the methods on the path of `k`, the Wasm backend with JSPI.
4. **The bridge by RE-EXECUTION** (operator, 2026-10-05: "Переисполнение",
   chosen over a link-time IR transform and over stopping here). A strict
   body that USES its `k`'s answer (`k => k(1) + 1`) cannot return before
   `k` does, so without transforming the body its call nests the host
   stack. Re-execution makes that stack DATA when it runs out:
   - at the end of a room, the strict `k` of the INNERMOST body being run
     throws `Delimited.Suspend` instead of nesting (or switching stacks);
   - every strict body the throw passes on its way out records itself:
     the body, its closed continuation, and the answers its `k` has
     already given (`Replay`);
   - the run's DRIVER, at its top, runs the deepest `k` from its argument
     on the shallow stack, then runs each recorded body AGAIN, innermost
     first: its `k` answers the recorded calls from memory and the
     interrupted one with the value just computed, and the body's answer
     goes on into its continuation, whose value is the next body's
     missing answer. Every level is re-run at most once: linear.
   - only the body being run may suspend: a `k` stored and called from a
     lambda of the run (not replayable) never does, nor does a `k` called
     from outside any run (that call starts a driver of its own).
   CONTRACT: where it suspends, the part of a strict body before its
   pending `k` call runs again — it must be free of effects (or
   idempotent), as React's render is under Suspense (the same technique:
   throw to unwind, re-render with the answer remembered). A body that
   catches `Throwable` around `k` swallows the suspension. Nothing else
   changes: no transform, no new type.
   PLATFORMS: on by default on Scala.js, which has no second stack (a
   room of fixed size); the JVM and Native keep `StackSwitch` by default
   and run the same mechanism with `okay.cont.replay=true` (the tests
   run it there too). OUT OF SCOPE: a run nested in user code of another
   run (a fresh machine started from a lambda) starts a driver of its own
   and is bounded as before.

   Behaviour:
   - [x] a million nested strict bodies using `k`'s answer, on Scala.js, and with replay on a 128 KB JVM thread and Native
   - [x] a body calling `k` twice, and one calling it after an earlier `k`'s answer: answers as without suspension
   - [x] the prefix before `k` runs again only where a suspension crossed it (counted)
   - [x] a stored `k` called from a lambda of the run, and a `k` called after its run: never suspends, still right
   - [x] a thrown exception from a body or a continuation after a suspension: the same exception
   - [x] ~~the differential oracle with replay on~~ NOT APPLICABLE: the oracle generates `Delimited` programs (Shift's captures, `k` as data); a strict `k` is Cont's leaf only, which the oracle never builds — TestContReplay compares against the run with the mechanism off instead

   Results (TestContReplay cross, TestContReplaySmallStack JVM; red first:
   "Maximum call stack size exceeded" at a million on Scala.js):
   - a million nested strict bodies: Scala.js ~2.3 s; the JVM with the
     mechanism on, 128 KB thread, no stack switched; Native at 50 000.
   - FOUND ON THE WAY, twice: (1) a call of `k` from its own continuation
     (re-entrant, inside a call of the same `k`) was recorded among the
     body's answers and replayed as its first one — only the body's own
     calls are recorded now; (2) a suspension crossing a `k` called from a
     lambda of the run unwound that lambda, which cannot be run again —
     such a call is a BARRIER now (`forced`; the body's own call is
     `forcedIn`), its nested run its own driver.
   - NATIVE: a throw unwinds at ~55 us a FRAME (4 suspensions through 500
     levels 110 ms; room 64 and 1 024 took the same time at 100 000 levels,
     so the cost is frames unwound, once each, not suspensions) — StackSwitch
     stays Native's default.
   - COST (history.d cont-replay): off, as the JVM runs by default, 1.00x
     /0.99x/1.01x (cont_seq, cont_twoShot, cont_strict_seq); on, the JVM,
     1 000 opaque levels: 4.94x against the fresh stack, ~150 ns a level.
   - OPEN (backlog cont-replay-jvm-default): the operator's bar was "no
     stack switch"; the JVM and Native still switch by default, for the
     4.94x and the contract above.


5. **A SAFE mode, chosen at compile time and at run time** (operator,
   2026-10-05: "Должен быть опциональный safe режим полного трамплининга
   без реплеев на всех платформах для функций с побочными эффектами без
   требований идемпотентности. Режим с оптимизациями … может быть по
   умолчанию. Но должна быть возможность выбора на этапе компиляции и в
   рантайме").

   WHAT IS POSSIBLE, said first: a body that needs its `k`'s VALUE now,
   and that the macro cannot read, can only wait on the host stack. With
   no replay and no second stack (Scala.js), nothing at run time can
   trampoline it. So the guarantee "full trampolining, no replay, every
   platform" is a COMPILE-TIME property: the body is CPS-transformed (the
   macro's lazy leaf: `k` is data, nothing nests), or it is refused.

   COMPILE TIME, per scope, by an import:
   - default: as stage 4 — a body the macro reads is transformed; an
     opaque one is a strict leaf that follows the run-time mode;
   - `import okay.Cont.safe.given` — SAFE: every body using `k` is
     transformed, or is a COMPILE ERROR that says why and how to write it
     (a lambda literal; `k` called, not passed to a function the macro
     cannot see into; or a program answer, whose `k` is lazy). Side
     effects run exactly once, in order: the transformed body's statements
     before `k` run when it does, the ones after it in `k`'s continuation.
     Full trampolining with no replay on JVM, Scala.js and Native;
   - `import okay.Cont.noReplay.given` — an opaque body is allowed and is
     NEVER re-executed, whatever the run-time mode: its `k` is a barrier
     (a driver of its own), so no suspension crosses it. Bounded by the
     host stack where nothing else helps (Scala.js); a fresh stack on the
     JVM and Native.

   RUN TIME, for the strict leaves the compiler left (`-Dokay.cont.mode`,
   `Cont.setMode`):
   - `auto` (the default, the optimized mode): re-execution on Scala.js,
     the fresh stack on the JVM and Native — each platform's cheapest;
   - `replay`: re-execution everywhere;
   - `safe`: never re-execute: the fresh stack on the JVM and Native; on
     Scala.js a strict body nests on the engine's stack (bounded) — there
     the guarantee is the compile-time one.
   `-Dokay.cont.replay=true` (stage 4) is `-Dokay.cont.mode=replay`.

   Behaviour:
   - [x] safe scope: a body with side effects before and after `k`, a million deep, on JVM, Scala.js and Native: every effect exactly once, in order
   - [x] safe scope: an opaque body (`k` passed to a function) is a compile error naming the fix
   - [x] safe scope: a body answering a program, `k` passed on: compiles (lazy `k`)
   - [x] noReplay scope: an opaque body with a side effect under the replay mode and a small room: run exactly once
   - [x] run time `safe`: no strict body re-executed (counted); `replay`: re-executed past the room; `auto`: the platform's default

   Results (TestContSafeMode, cross, JVM, Scala.js and Native):
   - the scope's choice reaches the macro as `shift`'s USING PARAMETER
     (`Cont.Shifts`), not by `Expr.summon`: summoned, the import read as
     unused (E198) at every user's site. A default ARGUMENT for it crashed
     the compiler (TreePickler, "method $anonfun") where `shift` is called
     inside another inline method (`Cont.Monadic.reflect`); `Shifts.default`
     is a given of the implicit scope instead, which an imported one —
     the lexical scope, searched first — overrides.
   - the backlog's cont-replay-jvm-default is answered by the modes: the
     JVM's default stays `Auto` (a fresh stack), `replay` and `safe` are
     one flag away, and a scope can fix its own bodies at compile time.

