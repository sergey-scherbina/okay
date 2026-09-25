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
| JVM, every JDK | a platform thread with a 1 GB stack (Decision 8) | ~500 000 by count, exact with Layer 3 | on the waiting threads' stacks, pages committed only as touched, unmapped when the segment returns |
| Native | a platform thread with a 1 GB stack | the same | the same |
| JS | none | unbounded count | **the bound**: the depth of nested opaque bodies is limited by the engine's stack, written here and in the docs |

The lane's first cut used a VIRTUAL thread on JDK 21+ (a waiting
virtual thread unmounts and its frames freeze into heap `StackChunk`s),
which forced 64-level segments: HotSpot refuses to freeze a chunk that
would be humongous ("StackOverflowError: Humongous stack chunk" at 512
levels; clean at 32 and 64 — the limit is the G1 region size, so it
moves with the heap). Measured on ONE JDK (26, Results): the virtual
road costs 0.19–0.26 µs a level past the switch, the platform road
0.010–0.016 — the price of no switch at all — because the cost is the
HOP (park, unpark, thaw), not the level, and 1M levels are 15 625 hops
at 64 a segment against 2 at 500 000. Memory is the same either way:
what a frame takes, on a chunk or on a page. The virtual road is
refuted; `StackSwitch` moves to the platform thread on every JDK.

### Layer 3: how much stack there is — knowing instead of guessing

Layer 2's first room is the part of the CALLER's stack that `Cont` takes
before its first switch. In the lane's first cut it is a guess,
`firstRoom = 256` (`-Dokay.cont.room`), and the measurement below shows
the guess costs 10x on a program that fits. This layer replaces the
guess with knowledge, and the shape of it is fixed by two facts
measured 2026-09-25 (Results, probe `StackProbe`):

- **Nothing is read on the hot path, and nothing at entry.** The count
  is the only per-level cost, one decrement in a field. Every look at
  the stack happens at EXHAUSTION — once per grant — so its price is
  divided by the grant. Not at `run` either: fib100 is 2.4 µs an op,
  and the cheapest exact read is 0.33 µs.
- **What is asked at exhaustion is "how many bytes are left", and the
  answer decides between a GRANT and a switch.** A grant is
  `(left − margin) / worst`, where `worst` is the most bytes ONE level
  has taken so far in this run, measured from the same reads (the
  stack pointer at the previous exhaustion minus this one, over the
  levels between). The first grant, before any read, uses the cold
  constant (1.2 KB, interpreted frames). The margin is 64 KB: HotSpot's
  yellow zone plus the fattest frame a body is expected to have. A
  body whose ONE frame exceeds the margin is the written bound of this
  layer on the JVM. When `left < margin`, switch.

Where the bytes come from, per platform, best knowledge first:

- **Native: exact, always.** The address of a `stackalloc` is the stack
  pointer; the runtime's own `ThreadInfo` (Platforms, below) gives the
  bounds and the guard page for every thread, main included. Cheap
  enough that the count could go, but the count stays so the runner
  is one code on every platform; Native only answers the exhaustion
  question exactly.
- **JVM with native access: exact.** FFM (JDK 22+), in okay-platform
  (Decision 12), found by the core by name. Bounds:
  `pthread_get_stackaddr_np`/`pthread_get_stacksize_np` (macOS),
  `pthread_getattr_np` + `pthread_attr_getstack` (glibc). The pointer:
  `getcontext`, reading `sp` out of the `ucontext_t` at the offset of
  the OS and architecture in hand (macOS arm64: 264 into the
  `mcontext`; the other three layouts are Open question 1). Measured
  on macOS arm64, JDK 26: present, 326 ns a read (it is a
  `sigprocmask` syscall inside), 112 B a compiled frame, the main
  thread 2060 KB, and from a virtual thread it reports the CARRIER's
  bounds — which is right, since a mounted virtual thread grows on the
  carrier's stack. USED ONLY WHEN `Module.isNativeAccessEnabled()` says
  the user allowed it: without `--enable-native-access` JDK 24+ prints
  four WARNING lines on the first restricted call and says the call
  will be refused in a future release, and a library does not print
  that on a user's console. `getcontext` is absent on musl (Alpine):
  a missing symbol, like a missing permission, falls through to the
  count.
- **JVM without native access: a calibrated count, and the written
  bound.** The VM default stack (`ThreadStackSize`, one read of
  `HotSpotDiagnosticMXBean`; 2 MB on macOS arm64, 1 MB on Linux x64)
  over the cold constant, halved for the caller's own use: ~850 levels
  here, ~400 on Linux — against 256 today. It holds for every thread of
  the default size, the launcher's main thread included (the `java`
  launcher runs `main` on a thread of that size, not on the primordial
  one). The bound: a thread created with an explicit SMALLER stack is
  not readable and must set `-Dokay.cont.room`. At exhaustion on such
  a stack there is no read to make, so it switches, as today.
- **On a stack we made** (a segment thread), the room is known by
  construction, 1 GB over 2 KB a level, and no read is needed.
- **JS:** nothing to read.

**`StackWalker` is not the instrument** (proposed 2026-09-25: walk the
stack before a risky call, keep the maximum depth seen, compare the
current depth with it). Measured: a walk costs **~7 µs before it looks
at a single frame** — `count()` and `skip(100).findFirst()` both read
6.5–7 µs on a 10-frame stack — and O(depth) after that: 56 µs at 2 000
frames, 209 µs at 10 000. A whole statePara run is 26 µs. And it counts
FRAMES: 112 B compiled, ~1.2 KB interpreted, an opaque body's frame any
size, so a maximum in frames is a lower bound on capacity only while
the frame mix does not change — deoptimisation changes it back. Where
the idea is right is the MAXIMUM, kept in the unit that is sound: the
most BYTES a level has taken, from the exact reads above. Where no read
is available the calibrated count is 20x cheaper than one walk and no
less right.

## Platforms — every runtime this library ships to, one row each

The operator's ask (2026-09-25): every platform provided for, not the
one on this box. One runner, one count, one `StackSwitch` per
platform; what differs is the fresh stack, the first room, and whether
the room can be read exactly.

| runtime | the fresh stack | first room | exact bytes at exhaustion | proved by |
|---|---|---|---|---|
| JVM 17 | platform thread, 1 GB | `ThreadStackSize` / 1.2 KB / 2 | never (no FFM): the count and its bound | `sbt verifyJdk17` (build.sbt: every forking suite on `jdk17Home`) |
| JVM 21 | the same | the same | never: FFM is still preview on 21 and this library enables no previews | a run on `jdk21Home` (build.sbt:1551 already names it) |
| JVM 22–24 | the same | the same | `okay-platform`'s FFM reader, when `Module.isNativeAccessEnabled` (22+); 24 is where the WARNING starts for callers that did not enable it | the default `Test / javaHome` when it is one of these |
| JVM 25+ | the same | the same | the same reader; a later release will REFUSE the call instead of warning (JEP 472), and the gate is the same boolean either way | JDK 26, the default `Test / javaHome` |
| JVM, caller on a VIRTUAL thread (any JDK 21+; okay's own default scheduler) | the same: the virtual thread parks on the `join` and unmounts, no carrier pinned | the same: a mounted virtual thread grows on its CARRIER's stack, which is `ThreadStackSize` | the reader sees the carrier's bounds (measured), which is the stack in use; `worst` is updated only when the bounds match the previous read's, since the thread may have moved carriers between two exhaustions | TestContStack on a virtual thread |
| Scala.js | none: no thread to switch to, no way to grow a stack synchronously | unbounded count (`Int.MaxValue`: nothing happens at zero, so nothing is counted) | nothing to read | the cross suite: a shallow program unchanged; **the bound**, in docs: nested opaque bodies are limited by the engine's stack (~10 800 frames on V8's default 984 KB; `node --stack-size` raises it) |
| Scala Native | platform thread, 1 GB (`Thread(group, r, name, stackSize)`: the javalib passes it to `NativeThread.create`, page-aligned plus its guard pages, `≤ Int.MaxValue` asserted) | ThreadInfo's own `maxStackSize` / 1.2 KB / 2 | ALWAYS, and in the core: the runtime keeps `ThreadInfo { stackTop, stackBottom, stackGuardPage, maxStackSize, isMainThread }` per thread (nativelib `nativeThreadTLS.h`, `scalanative_currentThreadInfo()`), the main thread's from the OS soft limit, and the address of a `stackalloc` is the pointer — no pthread call, no per-OS layout | a test in `src/test/scala-native` on a 128 KB thread |

The first room is `firstRoom` only until Layer 1 lands; with it, a
transparent body counts nothing and only an opaque one decrements.

### Where the code lives

- **`okay` (the core), cross:** `StackRoom`, one method —
  `left(): Long`, bytes on this stack before the guard, or −1 when
  unknown — and the runner's use of it at exhaustion. Its default
  answers −1.
- **`okay`, `src/main/scala-jvm/StackSwitch.scala`:** the platform
  thread, the `ThreadStackSize` read (one `HotSpotDiagnosticMXBean`
  call at class init, `-Dokay.cont.room` overriding), and ONE
  `Class.forName("okay.StackRoomFfm")` at class init: present, it is
  the reader; absent, the count. A name, not a `ServiceLoader`: a
  service load scans every jar's `META-INF/services` on the class
  path, needs a registration file per provider, and there is exactly
  one provider to find. The core does not depend on okay-platform and
  will not (core-modules); the soft link runs the other way, as the
  Multi-Release `Scoped` already does.
- **`okay-platform`, `src/main/scala-jvm/StackRoomFfm.scala`:** the
  FFM reader, written PLAINLY against `java.lang.foreign` — this
  module compiles with `jdkFloor(0)`, so dotc sees the ambient JDK's
  class library and emits bytecode 61, and the class loads on 17
  because call sites link lazily (the `Schedulers.hasVirtualThreads`
  pattern, proved on 17 by jdk-adaptive-scheduler). Two rules keep
  that true: no `java.lang.foreign` type in any field or signature of
  a class that loads on 17 — the reader is its own object, and the
  core only ever holds it as `StackRoom` — and nothing in it runs
  unless `Runtime.version().feature() >= 22 &&
  isNativeAccessEnabled`. The layout table (`(os, arch) → sp offset`)
  is here, and an `(os, arch)` that is not in it answers −1.
- **`okay`, `src/main/scala-native/StackSwitch.scala`:** the platform
  thread AND the exact reader, in one file: `@extern def
  scalanative_currentThreadInfo(): Ptr[ThreadInfo]` with the struct
  spelled out as SN 0.5.12 lays it out (two `size_t`, three `Ptr`, a
  `CBool`; pinned to the version in build.sbt, and a test that the
  bounds contain a `stackalloc` address guards the layout). It needs
  nothing above the core, so it does not go to okay-platform.
- **`okay`, `src/main/scala-js/StackSwitch.scala`:** as today.

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
   public API, which is why Layer 2's first cut used them — and
   Decision 8 is why it no longer does.
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
8. **The fresh stack is a platform thread with a 1 GB stack, on every
   JDK; the virtual-thread road is refuted** (2026-09-25, Results:
   same probe shape, same JDK 26 — 0.19–0.26 µs a level against
   0.010–0.016). A switch's cost is the hop, and the humongous-chunk
   limit makes the virtual road hop every 64 levels; the platform road
   hops every ~500 000 and pays a thread start each time. The
   waiting frames live on committed stack pages instead of heap chunks
   and cost the same bytes. The JDK 17 fallback of the first cut is
   therefore the one implementation, and the `MethodHandle` lookup of
   `startVirtualThread` goes.
9. **The JVM reads its stack pointer through FFM when the user allowed
   native access, and counts otherwise** (Layer 3). Not `StackWalker`:
   ~7 µs a call before the first frame, and frames are not bytes.
   Not a per-level read: 326 ns against a 15 ns level. Not a read at
   `run`: fib100 would pay 13%. Only at exhaustion, where a grant of
   hundreds of levels pays for it.
10. **No probe that overflows a stack on purpose to learn its size.**
    Considered as a way to calibrate the count without native access:
    a private thread, a pure recursion, `StackOverflowError` caught
    there and nowhere else. It measures the runner's OWN frames at the
    JIT state of that moment, not an opaque body's, so it answers the
    easy half of the question; and it is the exception Decision 4
    refuses, in a place where it is merely unnecessary rather than
    unsafe. The `ThreadStackSize` read gives the same first room
    without it.
11. **A switch costs 33 µs on the JVM whatever the stack size, and
    that is why exactness is not a nicety** (Results, `ThreadStart`):
    a 1 MB and a 1 GB thread start and join in the same 33 µs, because
    the reservation is free until touched; a virtual thread in 14 µs,
    only 2.3x less, paid 7 800x more often on the 64-level road. One
    switch a stack could have avoided costs more than a whole
    statePara run (26 µs). So the exact reader is not "an
    optimisation when native access happens to be on": for a program
    of opaque bodies it is the difference between master's number and
    2x, and the docs say so where they say how to enable native
    access.
12. **The FFM reader lives in okay-platform, the Native reader in the
    core** (the operator's suggestion, 2026-09-25, "the corresponding
    abstractions in okay-platform"). Not both in the core: the core
    compiles with `-java-output-version 17`, which REFUSES a reference
    to `java.lang.foreign` at compile time, so a reader there would be
    ~80 lines of `MethodHandle` lookups for twelve JDK 22 methods —
    written once as a probe, read by nobody. Not both in
    okay-platform: the Native reader needs only the runtime's own
    `ThreadInfo`, and a core that is exact by itself on Native should
    not lose that to symmetry. Not a Multi-Release `jdk22/` directory
    like `Scoped`'s: that variant exists only on a checkout that ran
    the script, and a guarantee that exists only sometimes is a
    bound.

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
- [ ] the platform-thread switch exercised by a test run on JDK 17 as
      well as 26 (no `MethodHandle` lookup left to differ, but the floor
      is where it is proved)
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

Stack knowledge (Layer 3):
- [ ] the fresh stack is a platform thread on JDK 26 too (the test's
      switch counter sees one switch per ~500 000 levels, not per 64)
- [ ] Native: at exhaustion the exact bytes left decide grant or switch;
      a `stackalloc` address lies inside `ThreadInfo`'s bounds (the
      struct-layout guard); a 128 KB Native thread switches and
      answers; the switch thread's own 1 GB is what `ThreadInfo` reports
- [ ] JVM, native access enabled (`--enable-native-access` in the
      test's fork options): statePara-shaped 1000 levels on a 2 MB
      thread switch ZERO times; a 128 KB thread still switches; a
      `Thread.ofPlatform().stackSize(8 MB)` thread is granted more than
      the default would allow; from a virtual thread the read answers
      the carrier's bounds and the program is correct
- [ ] JVM, native access absent: no WARNING line on stderr, ever; the
      first room is `ThreadStackSize`-derived (test: the counter on a
      default thread sees no switch below ~800 levels here)
- [ ] JVM without okay-platform on the class path: `Class.forName`
      misses, the count runs, nothing is logged (the core's own suite
      is that case)
- [ ] JVM 17 and 21: `verifyJdk17` green on the core and okay-platform
      (the reader's class loads on 17 and is never entered); the same
      on `jdk21Home`
- [ ] JS: the cross suite green; the bound in docs/
- [ ] the grant follows `worst`: a run whose later bodies have fatter
      frames than its first ones still does not overflow (a test body
      with a large local array past level 500)
- [ ] `-Dokay.cont.room` still overrides the first room

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
- **The two roads on ONE JDK** (26, same box, load 2.1, 2026-09-25
  11:28), the decision behind Decision 8:

  | road | 1M levels, `k(x+1)+1` | per level | hops | 3M levels |
  |---|---|---|---|---|
  | `StackHop`, virtual thread, 64 a segment | 194–260 ms | 0.19–0.26 µs | 15 625 | — |
  | `BigStack`, platform thread, 1 GB | 10–16 ms | 0.010–0.016 µs | 2 | 35 ms, 6 hops |

  Heap delta on the virtual road 69–98 MB (the frozen chunks); the
  platform road's equivalent is committed stack pages, unmapped as
  each segment thread returns. Multi-shot (`twice`, 2^16 calls) and a
  thrown exception correct on both.
- Probe `StackProbe` (JDK 26, macOS arm64, `-Xss2m`), the numbers
  behind Layer 3 and Decision 9:
  - `StackWalker.count()`: 7.2 µs at 10 frames, 56 µs at 2 006, 209 µs
    at 10 006. `skip(100).findFirst()` — a bounded walk — 6.5–6.8 µs at
    every depth, `skip(2000)` 55 µs: the cost is O(min(depth, limit))
    PLUS ~6.5 µs to open the walk at all.
  - FFM: `getcontext` present in libSystem; `sp` read from the
    `mcontext` at offset 264 agrees with the frame arithmetic: 1000
    frames of a trivial compiled method = 112 B each, 113 KB used of
    2060 KB. 326 ns a read, warm. `pthread_get_stacksize_np`: main
    2060 KB, a `stackSize(64 MB)` thread 65 548 KB (so an explicit size
    IS readable this way — the count-only road cannot see it, the exact
    road can), a virtual thread its carrier's 2060 KB.
  - `Module.isNativeAccessEnabled()` is false without the flag, and the
    first restricted call then prints four WARNING lines.
- Probe `ThreadStart` (JDK 26), the price of one switch — start a
  platform thread, run nothing, join; warm, 1000 rounds:

  | stack | per start+join |
  |---|---|
  | 1 MB | 33.3 µs |
  | 64 MB | 32.8 µs |
  | 1 GB | 32.8 µs |
  | 4 GB | 38.6 µs |
  | virtual thread | 14.4 µs |

  So 15 625 virtual hops at 14 µs is the 220 ms the virtual road
  measured, and a 1 GB reservation costs nothing until it is touched.
- Scala Native 0.5.12, read from the runtime's sources (javalib
  `Thread.scala`, nativelib `NativeThread.scala`,
  `nativeThreadTLS.h`, `stackOverflowGuards.c`): a `Thread` made with
  a `stackSize` reaches `pthread_attr_setstacksize` through
  `ThreadStackSize.resolve` (minimum 64 KB, plus the guard pages,
  page-aligned); every thread including main has a `ThreadInfo` with
  its bounds and guard page, kept for the runtime's own
  `StackOverflowError` (a SIGSEGV/SIGBUS handler on an alternate
  signal stack); `SCALANATIVE_THREAD_STACK_SIZE` overrides the
  default. Not yet run: the test on a 128 KB Native thread.
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
3. Layer 2 on the platform thread (Decision 8) and Layer 3 on every
   platform (the matrix above): `StackRoom` in the core, the JVM
   `StackSwitch` with the `ThreadStackSize` first room and the
   by-name lookup, `okay-platform`'s `StackRoomFfm` (macOS arm64
   first, the other layouts as Open question 1 measures them), the
   Native `StackSwitch` reading `ThreadInfo`, `worst` carried beside
   the room. Then shrink the fast path (the `Mapped` closure,
   `Reentry`'s shape) and A/B again. Land 1–3 together, only within
   noise, with `verifyJdk17` green.
4. Layer 1 B, then the known higher-order functions, then visible user
   functions and `direct`.
5. The remaining `ucontext` layouts on the JVM (Open question 1), each
   with its probe run on that OS.
6. okay2: stages 1–4 in Scala 2, as far as its macros reach.
7. Docs: user docs for Cont's stack behaviour per platform, with the JS
   bound, and the literature below.

## Open questions

1. The `sp` offset inside `ucontext_t` on the three layouts not yet
   measured: macOS x86_64, glibc x86_64 (`uc_mcontext.gregs[REG_RSP]`),
   glibc aarch64 (`uc_mcontext.sp`). Each is one constant and one probe
   run; until a layout is measured it counts (the exact road is opened
   per `(os, arch)`, never guessed). A cheaper pointer read than
   `getcontext` (326 ns: it saves the signal mask with a syscall) —
   `_setjmp` stores `sp` at a fixed slot on macOS arm64 and does no
   syscall — is a refinement to measure only if an exhaustion read
   ever shows in a profile; glibc mangles the slot, so it is macOS-only
   either way.
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
