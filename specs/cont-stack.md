# Cont past the stack — stack safety for direct-style continuations

Stage 1c of specs/stack-safety.md. Operator rule (AGENTS.md, "NO
UNBOUNDED STACK RECURSION"): every recursion is tail, trampolined, or
bounded by a written limit. This spec covers `Cont`, the one place in
both cores where that rule meets a design that uses the stack by
definition.

## Overview

`Cont` is DIRECT style. A shift's body receives its continuation as an
ordinary function, `shift[A, S, R](f: (A => S) => R)`. When the body
calls `k(v)`, it waits for a real `S` value and then does what it
likes with it: `k(1) + k(10)`, `s"answer: ${k(20)}"`, `a :: k(x)`.
Calling `k` runs the REST of the program, later shifts included, inside
the body's call. So shifts in a row nest, one level per shift whose
body calls its continuation:

```
step → body₁ → k₁(v) → step → body₂ → k₂(v) → step → body₃ → …
```

Measured 2026-09-25: 20 000 such shifts overflow a 128 KB stack in
okay AND okay2. The core's other recursions became loops in stages
1a/1b (specs/stack-safety.md). This one cannot become a loop by
rewriting the runner alone. The body's pending work (`+ k(10)`) lives
in the body's own frame, and a runner that sees the body as a black
box `(A => S) => R` cannot move that frame.

### Goals

1. No `StackOverflowError` from `Cont` at any depth, on JVM and Native.
   On JS: a written bound (below).
2. No exceptions used to unwind, and nothing executed twice (no replay).
3. The public API unchanged: `shift(k => …)`, `reset`, `c / k`,
   `Cont.Pure`, `flatMap`, `map`, the `Control[Cont]` instance.
4. The hot path priced against the current runner on FibBenchmark and
   HandlerBenchmark.statePara (the lane with the sharpest response,
   see `Leaf`'s comment), and landed only within noise of it.

### Non-goals

- Rewriting bytecode or linker IR (operator, 2026-09-25: "everything we
  can without rewriting bytecode").
- Multi-shot through a captured JVM stack: `k` stays an ordinary function
  and may be called any number of times.

## Design — three layers

### Layer 1: compile time — `shift` becomes a macro that trampolines what it can see

`shift` is `inline` today. It becomes an inline macro that looks at
the body's tree and classifies each use of `k`:

- **A, tail use.** Every call of `k` is in tail position: `k => k(v)`,
  including inside `if`/`match` branches and under state-passing
  lambdas (`k => st => k(a)(st)`). The body is rewritten to RETURN a
  description, "continue with `v`" (a `Jump` node), and the runner's
  loop continues with no nested frame. Sound, because a body has
  nothing left to do after a tail call.
- **B, the answer is used.** `k(1) + k(10)`, string interpolation,
  `a :: k(x)`. The body is CPS-transformed selectively, as okay-direct
  does for `Free`: each `k(e)` becomes a step `Call(e, x => rest of
  body)`. The runner keeps an explicit, type-aligned stack of pending
  body parts (the `Wrap`/`Args` technique of stage 1a), runs the rest
  of the program for `k(e)` iteratively, and feeds the answer into
  the pending part. This is a real trampoline for any number of nested
  answer-using shifts, and `k` stays multi-shot. It costs one
  allocation per call of `k`.
- **Known higher-order functions.** `k` passed to `map`, `foreach`,
  `flatMap` or `fold` on the standard collections, `Option` and
  `Either`: the macro knows their meaning and substitutes a trampolined
  traversal.
- **Visible user functions.** A function whose definition the macro
  can read, from an `inline def` or through `Symbol.tree` (same
  compilation, or TASTy on the classpath with `-Yretain-trees`), is
  rewritten recursively along the path `k` flows. Transformed functions
  are cached.
- **`direct { !k(…) }`** inside a body: read with okay-direct's
  machinery.
- **Opaque.** Everything else, where `k` flows into code the macro
  cannot read:
  - an abstract or virtual method;
  - Java code, or a class compiled without trees;
  - a body passed to `shift` as a value rather than a literal;
  - the non-inline `Control[Cont].shift`.

  An opaque body runs direct, and Layer 2 makes it safe.

A body in A or B never grows the stack, so a program of transparent
bodies never reaches Layer 2's switch at all.

okay2 (Scala 2): `shift` becomes a blackbox def macro. A and the known
higher-order functions are in reach. Visible user functions are not: a
Scala 2 macro cannot read another method's body, so those calls count
as opaque. If B proves too brittle on Scala 2 macros, it is opaque on
okay2 until it is not, and this spec says so when it happens.

### Layer 2: run time — the room a stack has left, and a fresh stack when it runs out

This layer is implemented and tested in lane `cont-stack-switch`. It
is not landed: see Results.

- **The room is a value, never a ThreadLocal** (operator). `step` takes
  a third parameter, `room`: how many more nested levels this stack
  takes. Steps that do not go deeper (Bind rotation, `Delay`, a pure
  `Return`) pass it on unchanged.
- **The continuation carries it.** Where a body receives `k`, the runner
  hands it `Reentry(f, k, room - 1)`, an object that is the
  continuation AND the room one level down. An absorbed leaf
  (`shift(…).flatMap(g)` fused at construction) gets the runner's room
  through `leafAt`, one class test with `Shift.at`'s own standing. Read
  off its continuation, the leaf would only ever see the program's
  outermost one and the room would never count down (measured: the
  first cut overflowed that way).
- **The room is taken from the place of the CALL.** A continuation can be
  called deeper than it was made: the runner hands an answer to an
  outer continuation from inside an inner segment. So `callK` enters
  with `min(room here, room of the continuation)` (measured: without
  it the room never fell along `Return → k(a)`).
- **At zero, `StackSwitch.fresh`.** The rest runs on a fresh stack, and
  this thread waits for its answer or exception. The waiting frames,
  the bodies' own included, stay where they are. There is no exception
  and no replay.

| platform | the fresh stack | room a segment | where the waiting frames live |
|---|---|---|---|
| JVM, JDK 21+ | a virtual thread (`Thread.startVirtualThread` through a `MethodHandle`: the core compiles against 17) | 64 | a waiting virtual thread unmounts; its frames are frozen into heap `StackChunk`s |
| JVM, JDK 17–20 | a platform thread with a 1 GB stack | ~500 000 | on the waiting threads' stacks, pages committed only as touched |
| Native | a platform thread with a 1 GB stack | ~500 000 | the same |
| JS | none | unbounded count | **the bound**: the depth of nested opaque bodies is limited by the engine's stack, written here and in the docs |

The virtual thread's segment is 64 levels because HotSpot refuses to
freeze a segment whose chunk would be humongous: at 512 levels the
freeze failed with "StackOverflowError: Humongous stack chunk". The
probe ran clean at 32 and 64.

### Layer 3: how much stack there is — knowing instead of guessing

Layer 2's first room is the part of the CALLER's stack that `Cont` takes
before its first switch. Today it is a guess, `firstRoom = 256`
(`-Dokay.cont.room`), and the measurement below shows the guess costs
10x on a program that fits. The plan: when the room runs out, look,
and grant more room instead of switching when there is space.
- **Native:** exact. The address of a `stackalloc` is the current stack
  pointer, and `pthread_attr_getstack` gives the thread's bounds, so
  the room left is known in bytes. It is cheap enough to check at
  every re-entry, and needs no count at all.
- **JVM:**
  - our own threads' sizes are ours to set;
  - a caller's thread's size is known only as the VM default
    (`ThreadStackSize` through `HotSpotDiagnosticMXBean`); the main
    thread's comes from the OS, and an explicit size (`new Thread(…,
    stackSize)`) is not readable;
  - the depth in frames is `StackWalker`, O(depth): ~tens of µs at
    5 000 frames, more than a whole statePara run;
  - bytes a frame differ 3–5x between interpreted and compiled code;
  - an exact check needs the stack pointer, which Java cannot read: a
    native call (FFM, JDK 22+: thread bounds from pthread, the stack
    pointer from `getcontext`) is possible but layout-specific per OS
    and architecture.

  Decision pending (Open questions).
- **JS:** nothing to read.

## Decisions (and what was refuted, with why)

1. **No trampoline at run time without changing the type.** Tried first,
   2026-09-25. Handing the body a marker instead of a real `S` breaks
   every body that uses the answer: `k(1) + k(10)` becomes a
   ClassCastException. The runner cannot tell which body is which,
   because the body is a function value. Only the compiler can, which
   is why Layer 1 exists.
2. **No ThreadLocal counter** (operator). The room is a parameter and a
   field.
3. **No failure at a depth, and no advice to use Delim.** Proposed
   ("ContDepthExceeded at N, use Delim"), refused by the operator:
   switch automatically instead.
4. **Not catching StackOverflowError** (proposed by the operator,
   reasoned against):
   - Unwinding destroys every frame between the overflow and the
     `catch`, so continuing means running from the catch point again.
     That is replay: side effects below it run twice.
   - An overflow can strike inside the JDK, a `synchronized` block or a
     class initializer and leave that state half-changed. The JDK
     guards its own critical sections with `@ReservedStackAccess` for
     this reason.
   - The handler itself needs stack at the edge.
5. **The JVM's internal continuations are not used directly.**
   `jdk.internal.vm.Continuation` works in HotSpot's runtime, with no
   new bytecode:
   - `enterSpecial` marks the boundary;
   - the `doYield` intrinsic FREEZES the frames above it into a heap
     `StackChunk`;
   - `run()` THAWS them lazily, a few at a time, behind a return
     barrier, and the GC scans the chunks.

   A library cannot do the same, because Java code cannot read its own
   frames' layout, GC maps or JIT metadata. The API needs
   `--add-exports java.base/jdk.internal.vm=ALL-UNNAMED` from every
   user and is unsupported. Virtual threads ARE this machinery behind a
   public API, which is why Layer 2 uses them.
6. **Bytecode rewriting is out of scope for now** (operator). The
   exception-free way to make an opaque frame trampolinable is a
   Kotlin-style state machine: a `SUSPENDED` marker returned up the
   stack, and locals saved in a heap continuation object. That needs
   the opaque code rewritten, at build time or by a load-time agent. It
   was considered, and JDK classes and JNI/foreign frames stay out of
   reach even then. Scala.js and Native could do it at link time over
   the whole program's IR. It is recorded as the road not taken yet.
7. **Callbacks are not a problem.** A `k` stored and called later runs the
   rest as a plain loop on the caller's stack. Only an opaque body that
   calls `k` SYNCHRONOUSLY and waits leaves a frame behind, which is
   Layer 2's case.

## Behavior

Runtime layer (lane cont-stack-switch, TestContStack; each red on a
512 KB stack before the switch existed):
- [x] 20 000 shifts in a row, bodies `k => k(x + 1)`: the answer
- [x] the same with bodies that USE the answer, `k(x + 1) + 1`
- [x] an absorbed leaf in a row, `shift(…).flatMap(…)`
- [x] multi-shot across switches: `k(x + 1) + k(x + 1)` at every level
- [x] an exception thrown deep crosses every switch unchanged
- [ ] the fast path within noise of master on fib100/fib1000/statePara
      (NOT met: see Results)
- [ ] JDK 17 fallback (1 GB platform stack) exercised by a test run on 17
- [ ] Native: the switch runs (a test on a Native thread with a small stack)
- [ ] JS: the bound written in docs/ and a test that a shallow program
      is unchanged

Compile-time layer:
- [ ] A: tail bodies (plain, branched, state-passing) produce no nested
      frame: 1M shifts on a 128 KB stack with NO switch (a counter in the
      test's StackSwitch double proves zero switches)
- [ ] B: answer-using bodies, the same 1M, no switch, multi-shot intact
- [ ] known higher-order functions: `xs.map(k)`, `opt.fold(…)(k)`
- [ ] visible user functions: an `inline def` and a same-compilation
      `def` that call `k`
- [ ] `direct { !k(…) }` in a body
- [ ] opaque bodies still correct, through Layer 2
- [ ] every existing Cont test green, statePara/Fib within noise
- [ ] okay2: A and known higher-order functions; B if it holds up

Stack knowledge:
- [ ] Native: an exact remaining-stack check replaces the count
- [ ] JVM: whatever Open question 1 decides

## Results

- Probe `StackHop` (scratchpad, JDK 26): opaque bodies, a counter in a
  parameter, a switch to a virtual thread every N levels, caller stack
  128 KB, 1M levels.
  - Correct for tail, answer-using and multi-shot bodies, and an
    exception crosses the switches.
  - 0.2–0.4 µs a level past the switch, against 0.015 µs a level on a
    2 GB stack with no switch.
  - 60–110 B of heap a level, RSS ~210–230 MB.
  - N = 128 overflowed a cold 128 KB caller before the first switch:
    interpreted frames are ~3x compiled ones.
- Probe `BigStack` (JDK 17, no virtual threads): platform threads with
  1 GB stacks.
  - 1M levels in 2 switches, 44 ms; 3M in 6, 129 ms.
  - With 4 GB stacks, 3M in 2; with 64 MB stacks, 1M in 31.
  - Multi-shot correct.
- A/B of the runtime layer against master (src/jmh/history.d,
  2026-09-25T091202Z-cont-stack-switch; min of 3 alternating rounds,
  quiet box, JDK 26):

  | lane | ratio | bytes |
  |---|---|---|
  | fib100 | 1.12x | +1 600 B/op |
  | fib1000 | 1.00 | unchanged |
  | statePara | **9.99x** | +12% |

  statePara nests ~1000 shifts whose bodies call `k`, and it fit the
  benchmark thread's stack before. With a first room of 256 it switched
  to virtual threads ~12 times a run. The fixed first room is the
  guess Layer 3 exists to remove, and statePara's bodies are A-shaped,
  so Layer 1 removes its switches entirely.

## Stages

1. Layer 2 (implemented, lane cont-stack-switch, not landed).
2. Layer 1 A: the macro and the `Jump` node. Target: statePara back to
   master's number, with zero switches.
3. Shrink Layer 2's fast path (the `Mapped` closure, `Reentry`'s
   shape), then A/B again. Land 1–3 together, only within noise.
4. Layer 1 B, then the known higher-order functions, then visible user
   functions and `direct`.
5. Layer 3: Native exact; JVM per Open question 1.
6. okay2: stages 1–4 in Scala 2, as far as its macros reach.
7. Docs: user docs for Cont's stack behaviour per platform, with the JS
   bound, and the literature below.

## Open questions

1. JVM stack knowledge. Candidates:
   - (a) keep a count, and read the default thread stack size once to
     set a larger first room;
   - (b) an FFM stack-pointer probe on JDK 22+, per OS and architecture;
   - (c) a count only, with Layer 1 making the count matter only for
     opaque bodies.

   After stage 2, measure how often (c) switches on the benchmark and
   test corpus before paying for (b).
2. okay2 B on Scala 2 macros: feasible, or opaque there?

## Literature

- Danvy & Filinski, "Abstracting Control" (LFP 1990): shift/reset, the
  answer-type discipline `Cont[A, S, R]` keeps.
- Rompf, Maier & Odersky, "Implementing First-Class Polymorphic
  Delimited Continuations by a Type-Directed Selective CPS-Transform"
  (ICFP 2009): the compile-time transform Layer 1 follows, and the same
  wall at code it cannot transform.
- Pettyjohn, Clements, Marshall, Krishnamurthi & Felleisen,
  "Continuations from Generalized Stack Inspection" (ICFP 2005):
  capture by exceptions and re-entry, which is Decision 4's road not
  taken.
- JEP 444 (Virtual Threads) and the Loom freeze/thaw implementation:
  what Layer 2 borrows through a public API.
