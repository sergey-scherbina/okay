# Delimited: an effect-independent machine for `Freer`, answer-type modification typed

Operator's asks, 2026-10-03, in order: remove Cont's casts and `Any`; a delimiter
with answer-type modification ("два типа и два места"); "Delimited не должен знать
о Cont/Shift/Shift0/Reset/Dollar — только где начинается и где заканчивается
сегмент стека"; "Freer/Delimited полностью независимы от того какой эффект в нем
работает — их задача предоставить все необходимые примитивы абстрактной машины";
Cont works with `A ! F` as plain values; `Shift % P` with prompts moved onto the new
machine as an effect; correctness shown by types first.

## 1. Where it comes from

`Freer[G, S, R, A]` is `(A => S) => R` as data (Atkey's parameterised monad). An
interpreter of it written directly is correct and typed by those indexes, but
grows the host stack in two places: `Bind`'s continuations (opaque lambdas) and
the interpreter's own nested runs. The machine is that host stack made DATA — the
functional correspondence (Ager, Biernacki, Danvy & Midtgaard 2003; Biernacka,
Biernacki & Danvy 2005 for shift/reset) — segmented as Dybvig, Peyton Jones &
Sabry's (JFP 2007). `Freer` does not change.

## 2. The machine (src/main/scala/Delimited.scala)

**Data.**

- `Frames[G, -A, B, S, R]` — a SEGMENT, the operator's `A => F[B, S, R]`:
  `Bind`'s continuations joined as `Bind` joins (`End`, `Frame`), and `Marked`, a
  transparent mark an effect finds again (an environment: Reader's `local`).
- `Stack[G, B, S, R, Z]` — what closes a level computing `Freer[S, R, B]` into the
  run's result `Z`: the two ends (`Done` by value, `Answered` by answer) and two
  kinds of boundary, each typed by its INSTALLATION, not by a prompt:
  - `Delim(tag, out, rest)`, a VALUE boundary: the level's value goes on into
    `out`, its answer types chain through as `Bind`'s do. A prompt; a nested run's
    barrier; (planned) a handler's frame.
  - `Bound(tag, out, rest)`, an ANSWER boundary: the level closes and its answer
    flows out as `out`'s value — Danvy & Filinski's `reset`. Answer-type
    modification is typed here with no claim.

**Primitives** (`trait Delimited[G]`, all an effect may use): `next`, `end`,
`frame`, `mark`, `delim`, `bound`; `find` (nearest mark); `cut` (walk out over
value boundaries to a marked one, `stop` at a barrier: the `Piece` above and what
lies under) and `reinstall` (put a `Piece` back); `closed` (the nearest answer
boundary and the segment it closes, `Kont`); `force` (run a closed segment now,
counted, `StackSwitch` at no room). A capture walks BOUNDARIES, never frames: to
the nearest it takes the segment as it is, O(1).

**Effects plug in** by `Delimited.Step[G, H]` (inside the object: a top-level `okay.Step` clashed with `okay.ui.PWizard.Step` under `import okay.*`): one operation, the segment and the stack →
the next state (`Next`). Three runners: `Machine` (one effect), `Under` (one
effect under others, a tagged `Sum` row: the others' operations leave in the
`Free` program it answers), `Over` (a `Free` row: `Outer` says which operations
leave, which are the row's own; a nested run of the row, `Outer.enter`, is
STEPPED INTO under a value boundary rather than forced, so nesting costs no host
frames).

**What it does not know**: any effect, any prompt's meaning, any answer type of a
mark. No `asInstanceOf` in the file.

## 3. Effects on it

- **Cont** (Cont.scala): Danvy–Filinski with ATM on answer boundaries. A leaf is
  `Op.Strict`/`Program`/`Lazily` (ContMacro's forms); `closed` gives the body `k`
  and takes its answer; a call of `k` opens a boundary of its own (`Resume`). No
  claim, no cast, no `Any`. `A ! F` are plain answers (monadic reflection).
- **Shift % P** (Shift.scala): a prompt in force is two value boundaries — its
  opening and its closing — with `ret` between; `shift0` cuts at the opening and
  skips past the closing, so `ret` goes into `k` and the body answers in the whole
  `ret $ body`'s place (λ$'s `S0 k.e`). `k(x)` is a self-contained resumption (a
  `Nested` run): stepped into by a machine running the row, run by its own
  otherwise (a `k` that outlived its run). Shift's three CLAIMS about a `Free` row
  — every node at `Unit`, an operation built in its program's row, a prompt's
  place answering the prompt's type (`eqPrompt`) — are one function in Shift, not
  in the machine.

## 4. Literature, and where this differs

- Atkey 2009; Kiselyov, "Genuine shift/reset" 2007: ATM by indexed monads, no
  machine. Ours: a machine whose host types (Scala GADTs) check it.
- Biernacka–Biernacki–Danvy 2005: the CK machine with a meta-continuation for
  shift/reset, untyped. Ours: typed, the meta-continuation is `Stack`.
- Dybvig–Peyton Jones–Sabry 2007: segmented stack, prompts between segments,
  typed prompts (fixed answer per prompt, `eqPrompt` by `unsafeCoerce`). Ours: the
  same segmentation; the answer type lives on the installation, so ATM and
  prompts share one machine; `eqPrompt` is the prompt effect's, not the machine's.
- Kobori, Kameyama, Kiselyov, "ATM without tears" (2015): shift/reset with ATM
  translated into multi-prompt control without ATM by fresh prompts — the
  closest to "the type on the installation"; theirs a translation, ours a machine.
- Materzok & Biernacki 2011: types for shift0/$ with a stack of answer pairs —
  what ATM through several levels at once would need; not taken (Freer unchanged).
- Racket (Flatt et al., ICFP 2007) and Kiselyov–Shan–Sabry (delimited dynamic
  binding, 2006): continuation marks beside prompts — our `Marked`.
- Koka (Xie, Leijen et al. 2020/2021), Effekt (Brachthäuser et al.; Muhcu et al.
  ICFP 2025): handlers on multi-prompt control; tail-resumptive operations
  without capture; state with multi-shot by backup on capture and restore on
  resumption.

## 5. Handlers and exceptions on the machine (approved 2026-10-03, built)

Today `HandleFrames` (State, Resource, Chronicle, Logic, Maybe, `Effects.handle`/
`relay`, Lexical, Once, Handler) runs on the λ$ machine (LambdaDollar.scala), and
where its frames meet Shift on the new machine five tests are red. The port:

1. **A handler's frame is a value boundary** — the same shape as a prompt: its
   opening (a `Handling` mark: `takes(op)`, `clause(op, k)`), its `ret`, its
   closing. Installing it is a Shift-family operation of the `Free` row.
2. **An operation finds its frame**: the row's `Outer` asks the machine whether a
   frame that `takes` the operation stands on the stack (`here`); if so the
   operation is the row's own, and its step cuts to the nearest such frame
   (`cut`, stopping at a barrier), gives the clause `k` (a self-contained
   resumption — deep: the frame goes back with it) and answers the clause's
   program in the frame's place. If none takes it, it leaves (`Over` forwards).
   A state frame stays parameter-passing (it answers `S => program`).
3. **The fold/frame duality stays** (`HandleFrames.Run`): forced by anything but a
   machine, a handler's run is its fold (`at(depth)`, bounded by `Limit`); a
   machine running the row steps into its FRAME (`Outer.enter` recognises a `Run`
   as it does a nested Shift run). `pending`, `shallow`, `Limit` keep their roles.
4. **Exceptions become a machine primitive, not an effect inside the loop.**
   Today the λ$ loop wraps every call of user code in `try` (`Cont0.guard`) once
   any catch frame exists in the PROCESS, and hands a throw to the nearest
   `Catching` frame. On the new machine: a run that has a catch frame on its
   stack (a per-run count, not a process-wide flag — which also answers backlog
   `handling-ever-per-machine`) calls user code under `try`; a throw goes to the
   row's `Outer` (`thrown(t, k, m)`), which finds the nearest catch mark (`find`/
   `cut`) and continues with its answer, or rethrows. The machine still knows no
   effect: "a throw is the effect's to answer" is the primitive.
5. **Then `LambdaDollar.scala` is deleted**, with `Cont0`, `LambdaFrames`,
   `LambdaStack`, `Shift.U`/`Ro`/`in`/`out`/`residual`.

**Measure** before landing — the hot path of every effect: HandlerBenchmark
(statePara, stateSmall, handlePrebuilt, relayPrebuilt, contAnswer), SplitBenchmark
(mixedList, writerShip), DelimBenchmark (delimGenerator, delimDollarResume,
stateForeign), FibBenchmark.fib100 — arms alternated against master.

## 6. Open questions

- The cost of finding a frame for every foreign operation (`here` walks the
  boundaries): a per-run count of frames skips it when there is none; whether a
  cache of the nearest frame per effect is needed is a measurement.
- Under `Under` (tagged rows) handlers are not needed yet; only `Over` gets them.
- `Once` and `Lexical.walk` re-tell programs at another row; their frames move
  with `HandleFrames`, their claims stay theirs.

## Behavior

- [x] the machine with no cast; Cont on it with no claim (TestDelimitedStack, TestCont, TestContMacro, …)
- [x] ATM through a mark (TestDelimitedMarks); effects nested as machines (TestDelimitedNested)
- [x] prompts as value boundaries in the stack, O(1) nearest capture (TestDelimitedLambda; TestShift's 100 000 captures)
- [x] nested Shift runs stepped into, not forced (TestResetDepth, TestResetSmallStack: 100 000 on 128 KB)
- [x] handlers as frames on the machine (§5.1–3): the five red tests green (okayJVM/test 835/835)
- [x] exceptions as the machine's primitive (§5.4): TestHandleFramesCatch, TestHandleFramesResource
- [x] LambdaDollar.scala deleted (§5.5); its INTERFACE kept as a test oracle over Shift (src/test/scala-cross/LambdaDollar.scala): the laws, the differential oracle and the depth programs check the new machine against the reference
- [x] the full gate; the benchmarks of §5 against master
- [x] one loop, marks as boundaries, a prompt as one boundary, a throw into a continuation (§7)

## Decisions

- `Freer` unchanged (operator).
- One machine for every effect; effects implement `Step`; the machine has no cast.
- The answer type lives on a boundary's installation; prompts are marks the
  machine cannot interpret.
- A value boundary is a stack node, not a mark in a segment: as a mark, `split`/
  `join` walked every frame above it, O(n) a capture — 100 000 captures took 142 s
  (refuted 2026-10-03; DPJS segment the stack at prompts for this reason).
- A nested run is stepped into, not forced: forced, 100 000 nested resets overflowed.
- `k(x)` is a self-contained resumption: as a bare operation it reached the
  outermost interpreter when `k` outlived its run (a state frame's `S => program`).
- Kept though not needed for Cont: `Under` (nested machines) and marks — they are
  the primitives for composing effects and for transparent tail-resumptive effects.

## Results

- 2026-10-03: the probes (src/test/scala/ProbeContAtm.scala, ProbeDelimAtm.scala)
  typed D-F ATM and a multi-prompt machine with no cast; the core machine followed.
- 2026-10-03: Cont and Shift on the machine; okayJVM/test red only where handler
  frames on the λ$ machine meet Shift (five tests, listed in Behavior).
- 2026-10-03: §5 built as designed. Two refinements the code forced:
  (1) `here` became `holds`, a walk of the BOUNDARIES only — `find` walks
  frames for marks and never saw a prompt, so a nested run's "is this prompt
  installed here" always answered no; (2) a nested run with no barrier sent
  its own `Dollar` out as "a prompt not installed here" — an installation is
  always the machine's own (19 tests, one cause). A throw is a `Delay` of
  `Delimited.Thrown`: typed at any answer with no claim (`() => Nothing`), and
  correct everywhere — anything but a machine that forces it throws it again.
- 2026-10-03: the λ$ interface's laws found one thing Shift lacked:
  RESUME WITH A COMPUTATION (DPJS `pushSubCont`). `reinstall` takes a
  program; a captured `k` is a `Shift.Resumption` with `resumeWith(m)`;
  `Shift.withSubCont` is its door.

## 7. Simplified (delimited-simplify, 2026-10-03, operator: "Сделай сразу все")

- ONE LOOP. `Machine`, `Under` and `Over` were three copies of `go`, differing
  only in what an operation of another effect does. Now `Run[G, F]` is the loop
  and `Outer[G, F]` says what leaves (`apply`, `diagonal` per operation, `enter`,
  `barrier`): a machine alone is `Run` with nothing outside (`Machine` reads its
  value back), nested machines (`under`) forward `Sum.Fwd` — diagonal by the GADT
  match, no claim — and a `Free` row (`over`) is told by its effect. The `try`
  for `guarding` is in the one loop, for every effect.
- MARKS ARE BOUNDARIES. `Frames.Marked` is gone: a mark is a value boundary with
  a tag and nothing else, so `Frames` is `Bind`'s continuations and nothing more,
  `find` is `holds` (boundaries only), and `closed` walks value boundaries to the
  nearest answer boundary — a `Kont` is a PIECE over its last segment, so Cont's
  answer-type modification works through marks (and any value boundary).
- A PROMPT IS ONE BOUNDARY. `push` (a reset) is a boundary marked by the prompt's
  `whole`; `dollar`'s is marked by the prompt, its `ret` the first frame above it:
  a capture takes `ret` into `k` and answers below it. No closing boundary, no
  `close` mark. Measured for `push` alone (shift-prompt-one-boundary, history.d):
  delimPushOnly 0.54x, delimDollarResume 0.73x, statePara 1.00x, delimGenerator
  1.07-1.22x with 18% fewer bytes — the optimization lane after this one.
- THROWING INTO A CONTINUATION. `Shift.Resumption.raise(t)` resumes `k` with a
  throw where it was captured; `Shift.raise(k)(t)` for a `k` typed as a function;
  `Paused.fail(t)` answers a dialogue's question with a failure its own `try`s
  see (TestHandleFramesDifferential).
- Not taken: `Cont.under` — a Cont with outer effects needs its own representation
  (a `Sum` row), a public type of its own; `pause`'s cast is a claim about the
  direct block's row, not about `k`.
- 2026-10-04 (delimited-cleanups): a `Piece` starts at its first segment (`One`) —
  no empty piece per capture; delimGenerator 0.99x, kept for the shape. REFUTED:
  `Resume` as its own `Pending` (no `Nested` beside it) — 40 KB fewer, yet
  delimGenerator 1.10x, delimDollarResume 1.05x: fewer objects, a worse loop, the
  pattern of delimited-simplify-costs. Not done: dropping `Shift.Cut` (a dollar's
  capture builds a new piece anyway) and a `Diag` arm without the `Inject` (it
  doubles `go`'s largest arm for an operation rare on the machine). Cont's deferred
  force (`Op.Program`) needs no depth accounting: its `k(a)` answers a program
  without running it (TestContProgramAnswerSmallStack, a million on 128 KB).
