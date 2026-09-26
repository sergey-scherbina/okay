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
  including inside `if`/`match` branches, with `v` not mentioning `k`.
  Such a body IS `pure(v)` evaluated at run time, so the rewrite needs
  no new node: `delay(() => Pure(v))`, which the runner's loop already
  walks with no nested frame. Sound, because a body has nothing left to
  do after a tail call. WHAT A IS NOT (read off `PState`, stage 3): a
  state-passing body `k => s => k(a)(s2)` — `PState.get` is
  `k => s => k(s)(s)`, `set(s2)` is `k => s => k(s)(s2)`. There the
  answer type is a function `S => R`, `k(a)` returns the REST as a
  function of the state and the body applies it to a new state: the
  answer is USED, so this is B's shape with a function answer, not A's.
  The spec's earlier line that statePara's bodies are A-shaped was
  wrong; what keeps statePara switch-free is Layer 3's exact room
  (its 1000 levels fit the 2 MB thread, and the reader says so).
- **B, the answer is used.** `k(1) + k(10)`, string interpolation,
  `a :: k(x)`. The body is CPS-transformed selectively, as okay-direct
  does for `Free`: each `k(e)` becomes a step `Call(e, x => rest of
  body)`. The runner keeps an explicit, type-aligned stack of pending
  body parts (the `Wrap`/`Args` technique of stage 1a), runs the rest
  of the program for `k(e)` iteratively, and feeds the answer into
  the pending part. This is a real trampoline for any number of nested
  answer-using shifts, and `k` stays multi-shot. It costs one
  allocation per call of `k`.
  AS LANDED (cont-stack-layer1-b, 2026-09-26): `Cont.Body` is the
  transformed body — `Done(r)` and `Call(k, a, rest)` — and `Cont.Cps`
  the leaf, an anonymous subclass per shift. NOT a function answer:
  the state-passing shape (`s => k(s)(s2)`) was built too, as a `Fun`
  answer walked in-loop with an `Ap` node for its application, and
  MEASURED at 2.8x the direct road on statePara (Results, stage E) —
  ten allocations an operation against three, on a program Layer 3's
  exact room already runs with no switch; it was taken out and the
  shape stays the opaque leaf. The macro
  hoists every k-free part evaluated ahead of a call into a val
  (A-normal form) so order is kept, follows a block's statements and
  result, an `if`/`match` in tail position, an application's function
  part and arguments in order, an ascription and an inlined expansion,
  and leaves a by-name argument where it is. The runner's `step` gains
  the pending stack as a parameter; a `Call` continues the program
  in-loop through the `Reentry`'s fields, and every exit that returned
  an answer feeds the part on top instead. What stays opaque is listed
  under cont-stack-layer1-c.
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
  answer decides between a GRANT and a switch.** A grant is ONE SLICE
  of `margin` bytes at the current estimate — `min(left − margin,
  margin) / worst` — where `worst` is the most bytes ONE level has
  taken so far in this run, measured from the same reads (the stack
  pointer at the previous exhaustion minus this one, over the levels
  between) and never lowered; the first grant, before any read, uses
  the cold constant (1.2 KB). A slice rather than the whole room
  (stage 3, measured): the estimate behind a grant is the worst level
  SEEN, and a fatter body below overshoots it — a slice of `margin`
  leaves the margin itself to absorb up to a 2x overshoot, and the
  next exhaustion measures the fatter level and raises `worst`. The
  first cut granted the whole room at the cold constant and overflowed
  on 20 000 cold levels. The price is a read per 64 KB of stack — ~200
  warm levels, ~1.6 ns a level. The margin is 64 KB. A body whose ONE
  frame exceeds twice the worst seen within one slice is the written
  bound of this layer on the JVM. When `left < margin`, switch.
- **The floor is not the end of the stack.** HotSpot throws
  `StackOverflowError` when the pointer comes within its guard zones
  PLUS the shadow it bangs ahead of every frame — `StackShadowPages +
  StackYellowPages + StackRedPages + StackReservedPages` pages, 24 ×
  16 KB = **384 KB** on macOS arm64. Measured 2026-09-25: with the
  floor at `top − size`, the runner granted down to 67 KB above it and
  overflowed, with no switch ever reached. The JDK 22+ reader adds the
  zone (the VM's own flags × libc's `getpagesize`) to `floor()`.
  Scala Native's `ThreadInfo.stackGuardPage` is already that floor.
- **Measured levels (JDK 26, TestContStack):** 1 504 B a level before
  the JIT settles, 288 B after — so `worst` rises once, early, and the
  slices are ~220 levels warm.

Where the bytes come from, per platform, best knowledge first:

- **Native: exact, always.** The address of a `stackalloc` is the stack
  pointer; the runtime's own `ThreadInfo` (Platforms, below) gives the
  bounds and the guard page for every thread, main included. Cheap
  enough that the count could go, but the count stays so the runner
  is one code on every platform; Native only answers the exhaustion
  question exactly.
- **JVM with native access: exact.** FFM (JDK 22+), the core's own
  `jdk22/` Multi-Release variant of `StackRoom` (Decision 12). Bounds:
  `pthread_get_stackaddr_np`/`pthread_get_stacksize_np` (macOS),
  `pthread_getattr_np` + `pthread_attr_getstack` (glibc). The pointer:
  `getcontext`, reading `sp` out of the `ucontext_t` at the offset of
  the OS and architecture in hand (macOS arm64: 264 into the
  `mcontext`; glibc aarch64 432 and glibc x86_64 160, inline — Decision
  13; macOS x86_64 is Open question 1). Measured
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
| JVM 22–24 | the same | the same | the core's `jdk22/` FFM reader (JEP 238 picks it), when `Module.isNativeAccessEnabled` (22+); 24 is where the WARNING starts for callers that did not enable it | the default `Test / javaHome` when it is one of these |
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
  call at class init, `-Dokay.cont.room` overriding), and the call
  `StackRoom.left()` at exhaustion. The root `StackRoom` (JDK 17
  bytecode, no `java.lang.foreign` anywhere) answers −1.
- **`okay`, `jdk22/StackRoom.scala` — the Multi-Release variant of
  that object for JDK 22+** (build.sbt `versioned("okayJdk22",
  "jdk22", 22, "okayJVM")` + `multiRelease` on `okay`'s jvmSettings,
  the mechanism mrjar-jdk25-ci-gap built for `Scoped`, 2026-09-25):
  the FFM reader written PLAINLY against `java.lang.foreign`,
  compiled with `-java-output-version 22` (the API check proves it
  uses nothing past 22), packaged under `META-INF/versions/22/` in
  EVERY core jar, and picked by the JVM itself per JEP 238 — a JVM
  below 22 never sees the class, so no lazy-linking rule, no version
  test, no `Class.forName`, no okay-platform. What it still gates on
  at run time is `Module.isNativeAccessEnabled()` (a 22+ method,
  callable because this class only exists on 22+): false, and it
  answers −1 like the root. The layout table (`(os, arch) → sp
  offset`) is here, and an `(os, arch)` that is not in it answers −1.
  Tested through the core's packaged jar (`multiRelease` puts it first
  on the forked test classpath), on 26 by default and under
  `verifyJdk17`, the way `TestScopedBackend` is.
- **`okay`, `src/main/scala-native/StackSwitch.scala`:** the platform
  thread AND the exact reader, in one file: `@extern def
  scalanative_currentThreadInfo(): Ptr[ThreadInfo]` with the struct
  spelled out as SN 0.5.12 lays it out (two `size_t`, three `Ptr`, a
  `CBool`; pinned to the version in build.sbt, and a test that the
  bounds contain a `stackalloc` address guards the layout). It needs
  nothing above the core.
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
12. **Both readers live in the core: the FFM one as a Multi-Release
    `jdk22/` variant, the Native one in `scala-native`** (2026-09-25).
    The first draft put the FFM reader in okay-platform, found by
    `Class.forName`, because the core compiles with
    `-java-output-version 17` — which REFUSES a reference to
    `java.lang.foreign` — and because the only Multi-Release variant
    in the build (`Scoped`'s) was compiled by a script run by hand and
    packaged only when its output happened to exist: a reader that a
    published jar carries "sometimes" is a bound, not a guarantee. The
    operator's answer was to fix THAT (mrjar-jdk25-ci-gap, the same
    day): the variant is an sbt project now, in every jar, tested
    through the jar. With that, the MRJar road is strictly better than
    okay-platform's: no soft link between modules, no lazy-linking
    discipline, the JVM's own version pick instead of a runtime test,
    and the reader in the core artifact itself. okay-platform is not
    involved. The Native reader needs only the runtime's own
    `ThreadInfo`, so it never left the core.
13. **A layout is MEASURED on its OS, never read off a header, and a
    glibc layout is more than one constant** (cont-stack-ucontext-layouts,
    2026-09-26). Open question 1 said "one constant and one probe run"
    per layout; the probe showed three things the sentence missed.
    (a) glibc keeps `mcontext` INLINE in `ucontext_t`, where macOS
    keeps a POINTER at 48, so the read differs in shape, not only in
    offset. (b) The bounds are a different call: glibc has no
    `pthread_get_stackaddr_np`, so it is `pthread_getattr_np` +
    `pthread_attr_getstack`, with the guard from
    `pthread_attr_getguardsize` taken off the bottom — exactly
    HotSpot's `os::Linux::current_stack_region`, so the floor the
    reader computes and the end HotSpot guards are the same address.
    (c) The buffer: glibc aarch64's `ucontext_t` is 4560 bytes and
    `getcontext` wrote up to byte 1004 of it — 20 bytes short of the
    1024 the macOS reader allocated, a margin a libc update could
    close. A glibc read hands `getcontext` 8192 bytes.
    THE PROBE, so the next layout is found the same way: fill a 16 KB
    buffer, call `getcontext` at depth 0 and again 1000 frames of a
    method compiled with `dontinline` deeper, and list every 8-byte
    word that lies inside the thread's bounds both times and FELL
    between them. On both glibc layouts exactly two words did, falling
    by the same 32 032 B (32 B a compiled frame of that probe method):
    the stack pointer and the frame pointer.

    | layout | `sp` | frame pointer | measured |
    |---|---|---|---|
    | macOS arm64 | 264 behind the pointer at 48 | — | 2026-09-25, native, JDK 26 |
    | glibc aarch64 | 432 inline | x29 at 416 | 2026-09-26, Docker linux/arm64 on Apple silicon, NATIVE, JDK 26 |
    | glibc x86_64 | 160 inline (`gregs[REG_RSP]`, 40 + 15 × 8) | RBP at 120 | 2026-09-26, Docker linux/amd64 on Apple silicon, UNDER EMULATION, JDK 26 |

    The x86_64 row was measured under emulation: the offsets are the
    libc's and the emulator runs that libc, so they carry; a native
    x86_64 run is still owed and is what the backlog keeps. macOS
    x86_64 is not measured and answers −1. musl has no `getcontext`,
    and a missing symbol falls through to the count: `readableWithout`
    builds the handles with a symbol hidden, and TestStackRoom asserts
    it cannot read without `getcontext` or `pthread_self`.

## Behavior

Runtime layer (lane cont-stack-switch, TestContStack; each red on a
512 KB stack before the switch existed):
- [x] 20 000 shifts in a row, bodies `k => k(x + 1)`: the answer
- [x] the same with bodies that USE the answer, `k(x + 1) + 1`
- [x] an absorbed leaf in a row, `shift(…).flatMap(…)`
- [x] multi-shot across switches: `k(x + 1) + k(x + 1)` at every level
- [x] an exception thrown deep crosses every switch unchanged
- [ ] the fast path within noise of master on fib100/fib1000/statePara
      — NOT PROVED at landing (operator, 2026-09-25: "мерж в мастер",
      with the A/B disqualified by the box: see Results). The number
      that is known: statePara's 1000 levels no longer switch at all
      (TestContStack, zero switches on the exact road), so its 9.99x
      cause is gone; what fib100 pays for `Reentry` + `Gauged` is the
      open measurement, backlog cont-stack-ab.
- [ ] the platform-thread switch exercised by a test run on JDK 17 as
      well as 26 (no `MethodHandle` lookup left to differ, but the floor
      is where it is proved)
- [ ] Native: the switch runs (a test on a Native thread with a small stack)
- [ ] JS: the bound written in docs/ and a test that a shallow program
      is unchanged

Compile-time layer:
- [x] A: tail bodies (plain, branched, with statements) produce no
      nested frame: 1M shifts on a 128 KB stack with NO switch
      (`StackSwitch.switches`; TestContMacro). State-passing bodies are
      B's, not A's (see Layer 1 A)
- [x] B: answer-using bodies, the same 1M, no switch, multi-shot intact
      (TestContMacro: `k(x + 1) + 1` at 1M on 128 KB, zero switches;
      `k(1) + k(10)`, `k(k(1))`, interpolation, `a :: k(x)`, a block
      with vals, tail `if`/`match`, evaluation order, an exception
      after a call; six opaque shapes still correct; a function answer
      left direct by measurement, see stage E)
- [ ] known higher-order functions: `xs.map(k)`, `opt.fold(…)(k)`
- [ ] visible user functions: an `inline def` and a same-compilation
      `def` that call `k`
- [ ] `direct { !k(…) }` in a body
- [x] opaque bodies still correct, through Layer 2 (TestContStack,
      TestContStackNative, with `shiftLeaf`)
- [x] every existing Cont test green, statePara/Fib within noise
      (core 548; statePara 1.01, fib100 1.00 after cont-stack-layer1-b;
      the new contAnswer lane 1.24x, by design — stage E)
- [ ] okay2: A and known higher-order functions; B if it holds up

Stack knowledge (Layer 3):
- [x] the fresh stack is a platform thread on JDK 26 too
      (`StackSwitch.switches` counts; 20 000 levels on 2 MB switch a
      handful of times, one per ~500 000 past the first)
- [x] Native: at exhaustion the exact bytes left decide grant or switch;
      a `stackalloc` address lies inside `ThreadInfo`'s bounds (the
      struct-layout guard — which caught the runtime's `stackTop` being
      the LOWEST address, see the Native `StackSwitch`); a 128 KB
      Native thread switches and answers; 20 000 levels on 2 MB switch
      under 100 times; multi-shot across the switch (TestContStackNative,
      4 green 2026-09-25). Native's first room is 64, not derived: a
      read there is a TLS access, and the main thread's 8 MB is the
      wrong guess for every other thread.
- [x] JVM, native access enabled (`--enable-native-access` in the
      test's fork options, first room 64 so the suite reaches
      exhaustion): 1000 levels on a 2 MB thread switch ZERO times; a
      256 KB thread still switches (128 KB cannot hold the first room's
      64 cold levels at 1.5 KB — the smaller-thread bound); an 8 MB
      thread switches fewer times than a 2 MB one on 20 000 levels
- [ ] from a virtual thread the read answers the carrier's bounds and
      the program is correct
- [ ] JVM, native access absent: no WARNING line on stderr, ever; the
      first room is `ThreadStackSize`-derived (test: the counter on a
      default thread sees no switch below ~800 levels here)
- [ ] JVM 22+ from a classes directory (a consumer's own test run,
      not a jar): the root `StackRoom` answers −1, the count runs,
      nothing is logged
- [x] JVM 17: TestContStack and TestStackRoom green forked on 17.0.19
      through the packaged jar — the count road, `sp = −1`, the
      exact-road test skipped by `assume` (2026-09-25)
- [ ] JVM 21: the same on `jdk21Home`
- [ ] JS: the cross suite green; the bound in docs/
- [ ] the grant follows `worst`: a run whose later bodies have fatter
      frames than its first ones still does not overflow (a test body
      with a large local array past level 500)
- [x] `-Dokay.cont.room` still overrides the first room (the suite runs
      at 64; `defaultStackBytes` is asserted ≥ 1 MB beside it)
- [x] `TestStackRoom`: on 22+ with native access and a known layout,
      `floor < sp < top`, a fresh thread has used under 8 MB, 200
      frames deeper reads lower; on 17/21 all three answer −1

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
  guess Layer 3 exists to remove. (An earlier version of this line
  said statePara's bodies are A-shaped and Layer 1 would remove its
  switches; they are state-passing, `k(s)(s2)` — see Layer 1 A — and it
  is the exact room of stage 3 that removes them.)

- A/B of stage 3, taken properly in the evening: plan stage A above,
  and D for the pool. The afternoon's attempt, kept for the record:
  DISQUALIFIED. The box carried siblings' whole-build gates all
  afternoon (load 19–121); the one lane that completed, fib100 on the
  lane's tree, read 5 412 ± 1 336 ns/op against the morning's 2 433 on
  the same code — ±25% within one lane and 2.2x against the morning is
  the host, not the runner. The reference arm then waited 20 minutes
  for quiet without starting. The operator chose to land on the tests'
  evidence; the measurement is filed as backlog cont-stack-ab, to be
  taken when the box is quiet (the morning protocol: min of 3
  alternating rounds, one lane per `jmh-lane.sh`). Found on the way:
  `jmh-lane.sh` runs a bare `sbt` on the PATH's JDK 17, which cannot
  compile a `versioned` variant (backlog jmh-lane-jdk-pin).
- Bytes a Cont LEVEL takes, by JIT state (cont-stack-8mb-flake,
  2026-09-26; a probe reading `StackRoom.sp` at the deepest point of 2 000
  opaque tail shifts on an 8 MB thread, macOS arm64, JDK 26): **1 858 B
  under `-Xint`, 1 223 B under `-XX:TieredStopAtLevel=1`, ~288 B warm
  C2** (1 726 B for the first, cold, run of a default JVM). A level is
  several frames, so the "~1.2 KB interpreted" above is a frame's, not a
  level's. Consequence for tests: a switch COUNT is a function of the
  JIT's state, and under load the C2 threads fall behind — the first
  8 MB test ("8 MB switches fewer times than 2 MB", 20 000 levels) read
  1 vs 1 whenever the code was still cold, and -Xint makes that red
  every time. TestContStack now asserts the bounds the reader reports
  (`top − floor`: ~7.6 MB on an 8 MB thread, ~1.66 MB on 2 MB) and
  sizes every "switches zero times" program to fit even interpreted.
  The cold constant (1 200 B) sits under the interpreted level: the
  margin's 2x covers a grant, but the count road's first room
  (`defaultStackBytes / 1 200 / 2`) spends its halving on it — backlog
  cont-stack-cold-bytes-per-level.

### cont-stack-ucontext-layouts (2026-09-26): the reader on Linux

The glibc layouts of Decision 13, checked with the REAL suites and not
only the probe: `TestStackRoom` and `TestContStack` run from the
packaged Multi-Release jar (the `META-INF/versions/22/` class in play)
through JUnit inside Docker, `--enable-native-access=ALL-UNNAMED`,
`-Dokay.cont.room=64` as the build passes them.

| container | reader | TestStackRoom + TestContStack |
|---|---|---|
| glibc aarch64, JDK 26 (native) | reads: floor < sp < top | 11 of 11 |
| glibc x86_64, JDK 26 (emulated) | reads: floor < sp < top | 11 of 11 |
| musl aarch64 (Alpine), JDK 25 | −1: no `getcontext`, counts | 11 of 11 |

The floor against the real end, the risk the macOS incident of Layer 3
named (a floor too low overflows with no switch): a thread recursing
while it reads `sp` at every level reached the reader's floor and ran
on to the StackOverflowError, and the last pointer read lay this far
BELOW the floor — so the floor is above the overflow on every one:

| platform | 256 KB thread | 2 MB thread |
|---|---|---|
| glibc aarch64 | 3 KB | 4 KB |
| glibc x86_64 (emulated) | 3 KB | 3 KB |
| macOS arm64 | 255 KB | 256 KB |

On glibc the floor is HotSpot's own guard boundary to within a page —
the bounds are the ones HotSpot computes and the zones are its own
flags at the libc's 4 KB page — while macOS keeps a quarter-megabyte
of slack (its zones at 16 KB pages). Either way the runner never grants
into the last `margin` (64 KB) above the floor.

### cont-stack-jmh-native-access (2026-09-26): both roads on one lane

Until this lane every cont-stack number in history.d was the COUNT
road: the JMH fork loaded okay from its classes directory, which the
JVM never versions, so the `jdk22/` reader was not even the class in
play, and the fork had no `--enable-native-access` either. The road a
user who passes the flag gets on JDK 22+ had never been measured, while
docs/cont-stack.md said "no switch at all with the flag".

THE CHANGE: `okayJVM`'s `Jmh / fullClasspath` puts the PACKAGED jar
first, as `multiRelease` does for the tests, so the versioned reader
is the class in play. Without the flag it answers −1 exactly like the
root and every existing lane keeps its meaning; the exact road is the
same lane with `-jvmArgsAppend --enable-native-access=ALL-UNNAMED`.
Every fork of FibBenchmark and HandlerBenchmark prints the road it
runs (`ContStackRoad`, a trial-level setup outside the measurement).

WHAT THE FIRST ATTEMPT GOT WRONG, found by that print: putting the
packaged jar first on `Jmh / fullClasspath` changed nothing — sbt runs
JMH from the Jmh PACKAGE (a `-jmh.jar` holding okay's main classes,
first on the run's classpath, no `Multi-Release` in its manifest), so
the root reader stayed the class in play, and the "exact" arm counted.
The fix is the Jmh package itself: the variant's classes under
`META-INF/versions/22/` and the manifest attribute, as `multiRelease`
does for the test jar. And the forks turned out to carry
`--enable-native-access` ALREADY — the Jmh host inherits `Test /
javaOptions` — so the moment the reader became versioned every lane
would have moved to the exact road silently; `Jmh / javaOptions`
drops that one flag, and the lanes keep the count road they were
recorded on. The same inheritance puts `-Dokay.cont.room=64` on every
lane, where build.sbt's comment said the benchmarks ran at the derived
default.

THE NUMBERS (history.d `cont-stack-jmh-native-access`, MIN of 3
alternating rounds, one lane per jmh-lane.sh run, JDK 26, macOS arm64):

| lane | room | exact road | count road | ratio |
|---|---|---|---|---|
| statePara | 64 (the lanes' inherited room) | 50.23 µs, 362 752 B | 32.81 µs, 343 265 B | **1.53x**, 1.057x bytes |
| fib100 | 64 | 2 533 ns | 2 560 ns | 0.99x, bytes equal |
| statePara | 873 (the derived default) | 32.67 µs, 346 192 B | 32.95 µs, 343 264 B | **0.99x**, 1.009x bytes |

At the room a user runs the two roads cost the same on statePara: the
exact road's reads (each an `Arena`, a `ucontext_t`, `getcontext`'s
326 ns and the bounds) price out at the count road's one hand-off to a
parked worker. At the tests' room of 64 the first look comes at level
64 and each grant is a 64 KB slice at the cold 1.2 KB a level, so the
exact road reads many more times a run, and that is the 1.53x — a cost
of the room setting, not of the road. The flag's gain is therefore not
speed but place: no second thread. The reader also re-reads the
bounds, which never change for a thread, at every exhaustion (backlog
`cont-stack-read-bounds-once`).

## Stages — what landed, and the plan after it (operator's ask, 2026-09-25 evening)

Landed 2026-09-25 as cont-stack-switch (60a59c97e): Layer 2 (the room
as a parameter and a `Reentry` field, the 1 GB platform-thread switch
on every JDK) and Layer 3 on every platform (`StackRoom` root and its
`jdk22/` FFM variant, Native's `ThreadInfo`, the slice grant, the
guard-zone floor). Landed before the macro and before the A/B, by the
operator's call.

WHERE THE FLOOR IS, so the plan chases the right thing: the direct
road costs 15 ns a level and the road past the switch the same; the
read is 1.6 ns a level and only on deep opaque programs. What is left
to pay is the BOOKKEEPING ON PROGRAMS THAT NEVER GO DEEP — fib100's
1.12x / +1 600 B/op for `Reentry` in the morning's runtime-only A/B,
plus `Gauged`'s two allocations a `run` since. Every stage below is
its own lane and claim, gated on the three lanes (fib100, fib1000,
statePara) plus HandlerBenchmark.handleCapture, min of 3 alternating
rounds through `jmh-lane.sh` on a quiet box, and lands only within
noise of the stage before it — or faster.

A. **Measure what landed** — DONE 2026-09-25 evening (history.d
   `cont-stack-ab`, ref b4934c052 = the parent of the lane's first
   commit; a first run against 60a59c97e's parent was an A/A — that
   parent is INSIDE the lane — and read ~1% noise, the floor for the
   rows below). MIN of 3 alternating rounds, `jmh-lane.sh`, JDK 26:

   | lane | ratio | bytes | reading |
   |---|---|---|---|
   | fib1000 | 1.01 | +64 B | noise |
   | fib100 | **1.17** | +1 664 B/op | the bookkeeping (`Reentry`, `Gauged`) on a program that never goes deep — stage C |
   | statePara | **5.03** | +7% | the COUNT road: the JMH fork has neither `--enable-native-access` nor the versioned jar, ~2 000 levels outrun the first room of ~870, ONE switch a run to a NEW 1 GB thread |
   | statePara, `-Dokay.cont.room=1000000 -Xss64m` | 1.08 | | diagnostic: the same tree with no switch possible — the bookkeeping is 8%, the switch the rest |

   So the expectation "statePara at master" was wrong FOR THE COUNT
   ROAD, which is what a user without the flag gets: the stack had
   room (the base ran the same program on the same 2 MB) and the
   count could not see it. That moved D3 up, next.

B. **Layer 1 A, the macro** — LANDED 2026-09-25 (cont-stack-macro). `shift` becomes
   an inline macro over the lambda literal; a body whose every use of
   `k` is a tail call `k(v)` with `v` not mentioning `k` — plain, under
   `if`/`match`, at the end of a block whose statements do not mention
   `k` — is rewritten to `delay(() => { statements; Pure(v) })`: no
   leaf, no `Reentry`, no nested frame, no count. Anything else is
   `Free.Inject(Shift.of(f))` as today. The one change that can make
   such bodies FASTER than master rather than within noise of it.
   CORRECTED at landing: `handleCapture`'s `shift(k => k(a.a))` is
   `!.shift` (Free's — the benchmark imports `!.*`), not Cont's; the
   library user is `Reader.local`'s clause `shift(k => k(r2))`, which
   becomes `tailPure(r2)`, a bare `Return` per `Ask`. Landed: 1M tail
   shifts on a 128 KB stack, zero switches (TestContMacro, red first:
   overflow / switches); statements, exceptions and timing unchanged;
   non-tail bodies unchanged. Two findings: the `given Control[Cont]`'s
   `override inline def shift` keeps a RETAINED non-inline body, so a
   macro expanded there — in the file that defines the types the macro
   reads — was a suspension cycle ("stale symbol Cont$" on every
   compile, clean or not); it calls `shiftLeaf` now. And every test
   that probed the leaf or the runtime switch with a tail body had
   silently stopped testing it — they build the leaf with `shiftLeaf`.
   Not measured (the box): a lane that exercises it is Reader.local's.

C. **The fast path's bookkeeping** — C1 LANDED 2026-09-25
   (cont-stack-fastpath): nothing allocated per run for the gauge; it is
   attached at the chain root (the outermost `Reentry`'s `k`, wrapped in
   `Gauged`) on a run's first exhaustion. Measured: −64 B/op on fib100
   (one run per op), 1.08 (history.d cont-stack-fastpath). THE FINDING
   THAT REFRAMES THE STAGE: an EXACT count (`ThreadMXBean
   .getThreadAllocatedBytes`, 200 warm runs on each tree) reads
   21 771 bytes per fib100 run on the base AND on the lane, byte for
   byte — the runtime layer allocates nothing extra. JMH's +1 600 B/op
   is C2's escape analysis: after seconds of warm-up the base
   scalar-replaces objects (the `Mapped` lambda, `Reentry`'s
   predecessor) that on the lane escape, because `callK`'s type test,
   `Reentry.enter`'s `min` and the re-entry into `step` put the hot
   path over the inliner's budget (memory: the inlining threshold has
   four faces; the escape-analysis boundary). Two JFR allocation
   profiles agree: the same classes on both, no `Reentry`/`Gauge`
   sampled in fib100's path. So C2/C3 as filed (object shapes) are the
   wrong lever; what is left of C is an inlining question —
   `-XX:+PrintInlining` / `-prof perfasm` on fib100 against the base,
   then a smaller `enter`/`callK` — filed back as cont-stack-fastpath
   with that recipe. ROUND 2 (same evening): `PrintInlining` named it —
   `Reentry.enter` at 106 bytes "callee is too large", `callK` through
   it "too much stack", where the base's 13-byte lambda inlined and
   was scalar-replaced. The exhaustion road moved out of `enter` (53
   bytes, inlines hot); one JMH fork in three then read the base's
   bytes (21 568 vs 21 552), the others the old 23 152 — the lever is
   right and the JIT's decision not yet deterministic; time 1.19 in a
   busy evening's noise. Next: `Leaf.applyAt` (131 bytes) and `callK`.
   Below: the plan as written before A. Candidates, each measured alone against the stage
   before, kept only when it pays: `Gauge` as a field of the OUTERMOST
   `Reentry` (found by the same walk) instead of a `Gauged` root per
   `run` — two allocations a `run` gone; `Mapped`'s lambda as a class
   carrying the room, so a `Mapped` chain keeps its gauge and its
   frame count; the three `Function1` specialisation bridges a level
   (`apply$mcII$sp`) — cold stack only, the JIT inlines them. Target:
   fib100 within noise of master, i.e. the morning's 1.12x gone.

D. **The switch itself** — D3 LANDED 2026-09-25 (cont-stack-parked),
   moved up by A: `StackPool`, a parked, reused worker with a 1 GB
   stack (JVM and Native, one file), at most 2 idle, 30 s idle
   timeout, the caller's context class loader set for the segment, no
   inherited thread-locals, the wait uninterruptible with the
   interrupt restored; then spin-then-park on both ends (50 µs,
   `-Dokay.cont.spinMicros`), because the ~22 µs a switch still cost
   with the pool were the two OS wake-ups. statePara on the count
   road: **134.8 → 50.3 → 31.8 µs against 27.6** (5.03x → 1.88 →
   1.15; history.d `cont-stack-parked`). Of the 107 µs a switch cost,
   85 were the thread start and its cold pages, ~18 the wake-ups, ~4
   remain; the 1.08 measured with no switch is the bookkeeping, stage
   C's. The reader's other knobs stay filed in cont-stack-fastpath —
   `_setjmp` (326 → ~10 ns a read), the slice at 128 KB — for a
   profile that shows them.

E. **Layer 1 B** — FIRST SLICE LANDED 2026-09-26 (cont-stack-layer1-b):
   answer-using bodies (`k(1) + k(10)`, `a :: k(x)`, interpolation, a
   block with `val a = k(1)`, a tail `if`/`match`) CPS-transformed onto
   an explicit stack of pending parts — `Cont.Body` (`Done`/`Call`),
   `Cont.Cps`, `step`'s `Pending` and the walked body as parameters.
   1M such shifts on a 128 KB stack, zero switches (TestContMacro, red
   first). MEASURED HONESTLY, as this entry asked, and the price is
   real: a NEW lane `HandlerBenchmark.contAnswer` (1000 levels of
   `k(x + 1) + 1`) reads 25.4 vs 20.5 µs, **1.24x** walked against
   direct (three alternating rounds, every ± under 0.35), at **0.81x
   the bytes** (198 vs 246 KB/op, `-prof gc`) — the walked road
   allocates 48 B a level LESS, so the 24% is dispatch and loop shape
   (the `Body` match, `rest.apply`, the pending push and pop through
   memory) against a direct road the JIT inlines level into level.
   statePara 1.01, fib100 1.00: nothing else moved. The plan's own
   words hold — robustness, not speed — and it lands as such: a
   readable answer-using program never touches the stack, on a 128 KB
   thread, on Scala.js where no switch exists; a JVM that would have
   run it direct and switched for ~4 µs per ~870 levels pays 1.24x.
   REFUTED WITHIN THE LANE, before landing: the state-passing
   `k(a)(s2)` of `PState` as a function answer (`Fun`) walked in-loop,
   its application an `Ap` node — 1M PState operations on 128 KB with
   no switch, and **89 vs 32 µs on statePara, 2.8x** (three rounds;
   history.d `cont-stack-layer1-b-fun-*`): ten allocations an
   operation against three, on the program Layer 3's exact room
   already runs switch-free. Taken out; the shape stays the opaque
   leaf. The rest — profiling the 24% (or a JS-only expansion if the
   JVM price is refused), non-tail conditionals, the known
   higher-order functions, visible user functions, `direct` — is
   backlog cont-stack-layer1-c.

F. **The rest, in any order:** cont-stack-ucontext-layouts (LANDED
   2026-09-26 for glibc aarch64/x86_64 and musl's missing symbol,
   Decision 13; macOS x86_64 and a native x86_64 run are backlog
   cont-stack-ucontext-x86-native), cont-stack-docs
   (the user page with the per-platform bounds and how to enable native
   access), okay2's cont-stack-okay2 (stages B and E as far as a
   Scala 2 blackbox macro reaches; Layers 2/3 port straight).
   Also, after C and D land: RE-PRICE `DelimBenchmark.stateLexDeep`
   and `stateShallow` against `stateHandle` (handlers-as-dollar read
   3.8x/4.7x time, 7.4x/7.8x bytes on 2026-09-25 morning) and update
   the "Choosing" table in docs/many-instances.md — deep's cost IS the
   capture path these stages are about, and the table quotes today's
   number (shift0-dollar-close, 2026-09-25).

What is NOT on the plan, and why: the count road (JDK 17/21 without
native access) — one switch past ~850 levels and full speed after, no
lever worth a lane; `StackWalker` in any role — 7 µs before its first
frame; catching `StackOverflowError` — Decision 4.

## Open questions

1. The `sp` offset inside `ucontext_t` on macOS x86_64, the one layout
   not yet measured, and a NATIVE x86_64 run of the glibc row measured
   under emulation (Decision 13 has both glibc rows and the probe that
   found them). Until a layout is measured it counts (the exact road is
   opened per `(os, arch)`, never guessed). A cheaper pointer read than
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
