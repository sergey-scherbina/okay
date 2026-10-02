# cont-js-depth — the machine apart, and no stack overflow in principle

Status: stage 1 done, 2026-10-02. Owner lane: `cont-js-depth`.
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
and `PWizard.scala` (okay-ui, 3: `k => s => k(s)(s)`) — shape (6) of
backlog cont-stack-layer1-c.

## Stages

1. **The machine apart** — DONE: `Frames`, `Stack`, the loop
   (`Frames.run`/`machine`/`enterAt`), `Cont0 = Shift0 | Reset0` with
   its delimiters, and `Rev` moved verbatim from Cont.scala to
   Delimited.scala; Cont.scala is the one-prompt facade and the
   strict-`k` bridge. No behaviour changed.
2. The machine's own suite: the DPJS laws as identities, and depth —
   a million nested captures and resumptions on a 128 KB JVM thread, on
   Scala.js and on Native.
3. The bridge on Scala.js: the nesting shapes of the census (state
   passing) and user code — candidates: the function answer walked as
   data on JS only (cont-stack-layer1-c (6)), a link-time IR transform
   of the methods on the path of `k`, the Wasm backend with JSPI.
