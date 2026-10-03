# cont-js-depth — the machine apart, and no stack overflow in principle

Status: stages 1-2 and 3a done, differential oracle landed, 2026-10-02. Owner lane: `cont-js-depth`.
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
