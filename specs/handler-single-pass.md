# Handler single pass: `handle` registers, `run` walks once

Operator, 2026-10-04: "идет обход дерева и паттерн матчингом ищется для эффекта хандлер — все как и
сейчас — разница только в том что сам паттерн матчинг происходит не для одного хандлера а сразу для всех
которые указали — происходит слияние всех хандлеров в один. Каждый handle(h) не обходит сразу дерево а
только лишь регистрирует хендлер в стеке хендлеров а обход происходит уже только в самом конце в run.
Детали конечно зависят от машины но для этого мы и вводим нужные нам абстракции чтобы зависимость эта была
минимальная и строго определенная."

Backlog item: okay-core/handler-single-pass. Related: specs/handler-forms.md (the four forms),
specs/handler-fusion.md (the fused-runner measurements and the refuted no-tree roads), specs/cont-atm.md
(the machine and the handlers-as-frames probe), specs/handler-equivalence-oracle.md (the laws' instrument).

## Overview

### Today

`p.handle(h1).handle(h2).handle(h3).run` is three walks. Each `handle` returns a deferred node
(`Free.delay` of a `HandleFrames.Run`), so nothing walks at `handle` time. Once forced, though, each node
walks the program under it on its own. h1's loop answers h1's operations and REBUILDS every other one on the
way out: `forwarded(i).flatMap(x => again(k(x)))`, one `Bind` and one closure per forwarded operation. h2's
loop then walks that rebuilt program, and so on. n handlers mean n walks, and an operation of the outermost
effect is rebuilt n − 1 times. `p.handle(h1, h2, h3)` (handle-many) only spells the same nesting:
`h3.run(h2.run(h1.run(p)))`.

### The design

1. **`handle(h)` registers.** A handled program is a node holding the program under it and a STACK of
   handlers, innermost first. `handle(h)` on a program that is already such a node pushes `h` onto that
   node's stack. Anything else gets a new node with a stack of one. Nothing is walked.
2. **`run` walks once.** At the end, or wherever the node is forced, ONE loop walks the program. Each
   operation is matched against the stack in order, innermost first. The first handler whose effect it is
   answers it, and the program goes on with `k(v)`. An operation no handler takes leaves the node once, as an
   operation of the rest of the row, and its answer comes back into the same loop.
3. **One abstraction between the handlers and the machine: the STEP.** A handler that can be fused gives
   `init`, `step(s, op) => (s', v) | Stop(program)` and `ret(s, a) => program`. These are the three things the
   walker knows of a handler, and the walker is the only thing the machine knows. Since handler-one-step
   (2026-10-04) every state-threading built-in already is this step on `HandleFrames.stateRun` /
   `stateRunUntil` / `stateRunOr`. These are State, Writer's folds, Reader, Supply, Once, Refs, Chronicle,
   `Lexical.walk`, and the `Handler.answer` and `Handler.state` forms. The design exposes the step, it does
   not invent it.

### What stays exactly as it is

- The tree (`Freer`): the no-tree roads were measured and refuted (handler-fusion stage B: Eff/Func/Cont
  0.58–0.86x). Only WHO walks it changes.
- A handled program is still a program. `p.handle(h).flatMap(f)`, or a handled program passed elsewhere, is
  forced where it is met, as today. Fusion is a property of a CHAIN of `handle`s, never a change of meaning.
- The user's API: `p.handle(h)`, `p.handle(h1, h2)`, `p.run`. Only their cost changes.

## Interface

```
// the fusible handler: its step, exposed (sketch; names settled in stage 1)
trait Stepped[E[+_], S, O[_]] extends Handler[E, O]:
  def takes: TypeableK[E]                       // which operations are its
  def init: S
  def step(s: S, op: Any): (S, Any) | Stop[?]   // the HandleFrames engines' step
  def ret[A](s: S, a: A): O[A] ! ?              // the answer at the end, a program in the rest of the row

// the registered stack: a node of the program, forced by any loop that meets it
final class Handled[...](program, stack: List[Stepped | Opaque]) extends HandleFrames.Run[...]
```

`Opaque` is any handler that is not `Stepped`: `Handler.control`, `Throws.either` with its catch, `Choose.all`
and every handler that needs `k`.

## Behavior

- [ ] Registration: `p.handle(h1).handle(h2)` builds ONE node with the stack `[h1, h2]`, and nothing is walked
      before it is forced.
- [ ] One walk: a program over State + Writer + Reader handled by three `Stepped` handlers is walked by one
      loop (pinned by a counter of loops entered, the way TestProgramAnswerStackFree pins stack switches).
- [ ] Dispatch order: an operation goes to the INNERMOST handler that takes it, so two instances of one
      effect (`State % Int` twice, via `Tag`) keep today's shadowing.
- [ ] A handler's own operations go OUT, never in: an operation that a step, a `ret` or a `Stop` program
      performs is matched only against the handlers OUTSIDE the one that produced it (Xie & Leijen's rule:
      a handler runs in the context of its outer handlers). `Handler.into[F, G]` whose `G` is handled further
      out, and Writer's `ret` performing `G`, are the cases that pin it.
- [ ] `ret` and `Stop` in order: when the innermost handler finishes (its `ret`) or stops (`Stop`, Chronicle's
      halt, `stateRunUntil`'s done), its answer program goes on in the same loop with that handler popped.
- [ ] Control boundary: an `Opaque` handler SPLITS the stack. What is inside it is fused, it runs as today
      (its own loop, `Effects.handle`, a capture), and what is outside it is fused again. `p.handle(State(0),
      Throws.either, Writer.log)` is two fused walks around one `Throws` loop, and the user sees no difference.
- [ ] Laws, by the equivalence oracle: for every program in the oracle's corpus and every stack of built-ins,
      the fused stack and today's nested `handle`s give the same answer and perform the same outer
      operations in the same order. That covers multi-shot (Choose) inside and outside a fused stack, and
      recover × State (scoped-effects-laws).
- [ ] Stack: 100 000 operations through a fused stack of three, in constant host stack (the operator's
      rule, specs/cont-stack.md: the walk is a loop, never a recursion per operation).
- [ ] The machine face: the fused loop is ONE frame for a `Delimited` machine that meets it
      (`HandleFrames.statefulAll` with the union of the stack's `takes`), so a run nested in a machine keeps
      working as `HandleFrames.Run` does today.

## Dispatch: a table, not a chain of tests (operator, 2026-10-04)

"Строить какую то таблицу эффектов и по ней прямым переходом выполнять нужный хендлер." The fused loop
must not test the operation against the stack's handlers one by one. Two levels:

1. **At run time: a table keyed by the operation's EXACT class, filled lazily.** Every operation is a
   class of its own (`Get`, `Set`, `Tell`). The stack carries a small table, operation class → handler
   index. A hit is one class compare (`op.getClass eq c`, or a small identity hash when the stack sees many
   classes), then a direct call of `steps(i)`. A miss, once per class per stack, runs the `TypeableK` tests
   innermost first and records the index it finds. Correctness stays with `TypeableK`, and the table is only
   its cache. An exact-class compare is cheaper than the effect's own test: an effect is a sealed TRAIT, and
   `instanceof` against an interface scans the class's secondary supers.
2. **At compile time: the whole stack known at one call.** `p.handle(h1, h2, h3)` with statically known
   handlers lets a macro write the fused loop itself: a `match` over the operation classes whose branches
   ARE the steps, inlined. No table, and no virtual call of a step either. This is `Direct.staged`'s
   technique (2.24x over a Free direct block) applied to a handler stack.

Limits written down now:
- An instance by NAME (`Tag`, `Lexical`, two `State % Int`) is not told apart by class. Its table entry
  says "test the instance", which is the fallback test.
- Level 1's call of a step through the table is megamorphic, since every handler's step differs: ~3 ns.
  Level 2 removes it.
- For 2–3 handlers C2 already profiles a short chain of tests well, so the table may not beat it. Stage 0
  measures chain against table at 2, 4 and 8 handlers before either is chosen.

## Stages

0. **Re-measure the prize** before building anything. Measure dispatch as well: the chain of tests
   against the class table at 2, 4 and 8 handlers. The last numbers predate the step engines, `Delimited`
   and `Pure[+A]` (handler-fusion, 2026-09-27: fused against nested 1.36x / 1.31x on `foldLeft`-built
   programs, **1.05x right-nested**). FusionBenchmark's SW / TSW / SWr lanes on today's master, and beside
   them a hand-written two-state fused loop (the prototype of stage 2 for State + Writer only). This stage
   says what stages 2–4 can win. The design stands on its own (one walk, one place for dispatch), but its
   cost estimate comes from here.
1. **`Stepped`**: the step exposed by `stateOf`, `answerOf` and the built-ins' handler values, nothing fused
   yet. Zero behaviour change, pinned by the existing suites.
2. **Registration and the fused loop**: `handle` pushes onto a `Handled` node, and the loop dispatches over
   the stack. Only `Stepped` handlers so far: an `Opaque` one is a node of its own, as today. The laws (the
   oracle) land with it.
3. **The control boundary**: an `Opaque` handler splits the stack, so a mixed stack fuses around it.
4. **The machine face**: the fused loop as one `Delimited` frame.
5. **Docs**: docs/your-own-effect.md, "Which one", says which handlers fuse: the forms 1–3 and every
   state-threading built-in, while form 4 (`control`) splits the stack.

## Decisions

- **Handlers as frames of the machine are NOT this design** (handlers-as-frames, 2026-10-04, refuted
  1.5–4.3x). That probe ran every handler as a frame of `Delimited`, which reaches an operation's handler
  by searching the stack and answers through a captured `k`. Here a `Stepped` handler is answered IN PLACE
  by its step, with no capture and no search beyond a test per handler in the stack. Only `Opaque` handlers
  keep a capture.
- **The tree stays** (handler-fusion stage B, refuted): this changes who walks the tree, not what it is.
- **Fusion never changes meaning**: a fused stack must be indistinguishable from the nested `handle`s it
  replaces. That is a law over the oracle's corpus, not a hope.

## Open

- **Re-telling handlers** (`State.zoomWith`, `Maybe.prune`, `Writer.map`, okay-stream's `Source`) re-tell one
  effect as another. Are they `Stepped` (a step that performs the other effect outward), or `Opaque`?
  Settled in stage 1, case by case.
- **The types of a pushed stack**: `handle` today solves `A ! G =:= A ! E + F` per call. Pushing onto a
  `Handled` node must keep those solutions per handler, at no cost to inference. Stage 2's first question.
- **Instances by name** (`Lexical.deep`/`tail`, `Tag`): a named instance is matched by its prompt or tag, not
  by its effect's class. The dispatch test is then the instance's, which the stack must carry.
- **Scala 2 (okay2)**: the same design over okay2's handlers, after the Scala 3 core lands it.

## Results

### Stage 0 (2026-10-04): the prize re-measured, and dispatch measured

One lane per `jmh-lane.sh` run, two rounds, JDK 26, macOS arm64 (history.d handler-single-pass-stage0).

The prize, today's nested `handle`s against Fused's hand-written one-pass loop (FusionBenchmark):

| program | nested | fused | fused / nested | bytes |
|---|---|---|---|---|
| State + Writer, built by `foldLeft` (SW) | 31.1–31.4 µs | 26.3–26.4 µs | 0.84x | −15.5 KB |
| the same, right-nested: an ordinary recursion (SWr) | 11.7 µs | 11.8–12.0 µs | **1.01x, no win** | −15.5 KB |
| Throws + State + Writer (TSW) | 31.7–32.0 µs | 27.0–27.5 µs | 0.86x | −15.6 KB |

Smaller than on 2026-09-27 (1.36x, 1.05x, 1.31x then): the nested handlers got faster since, on the
step engines (handler-one-step). On the shape of a recursion one pass saves bytes and no time. So the
case for this design is the architecture (one walk, one place where dispatch lives, `handle` as
registration), not speed. A speedup is real only for programs built by folds, at 1.16–1.19x.

Dispatch, one fused loop over N stepped handlers, 1 000 operations round-robin, two operation classes an
effect (DispatchBenchmark, a prototype):

| handlers | chain of tests | class table | table / chain |
|---|---|---|---|
| 2 | 3.7–4.0 µs | 3.7–3.9 µs | 0.99x |
| 4 | 4.6–4.9 µs | 3.6–3.8 µs | 0.78x |
| 8 | 6.2 µs | 3.9–4.3 µs | 0.66x |

The table is flat in the number of handlers and the chain grows with it. The table is never worse, so it
is the dispatch of stage 2. Its cost is 160 B more per run (the cache arrays).

## Literature

- Ningning Xie and Daan Leijen, "Generalized Evidence Passing for Effect Handlers", ICFP 2021: handlers as
  an evidence vector, operations dispatched to their handler without a stack search, tail-resumptive
  operations answered in place. Koka's runtime.
- Ningning Xie, Jonathan Brachthäuser, Daniel Hillerström, Philipp Schuster and Daan Leijen, "Effect Handlers,
  Evidently", ICFP 2020: the "a handler runs in the context of its outer handlers" rule this design's
  dispatch keeps.
- kyo (getkyo/kyo): one runtime loop over a stack of handlers in Scala, the same shape in production.
