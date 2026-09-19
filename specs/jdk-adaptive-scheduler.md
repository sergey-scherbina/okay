# The default scheduler adapts to the JVM (jdk-adaptive-scheduler, 2026-09-19)

`src/main/scala-jvm/Platform.scala`'s `given Timer` and default
`given Scheduler` called `Thread.startVirtualThread` unconditionally.
On a JDK without virtual threads (< 21), the first delay that fired or
the first fiber forked with no explicit `-Dokay.scheduler` threw
`NoSuchMethodError` — not a compile problem, a first-use-at-runtime
problem. This makes both adapt: virtual threads where the JVM has
them, a real scheduler where it doesn't, automatically, no property
required.

## The MRJar plan was wrong — refuted before writing any of it

The first plan (from the conversation that led here) was to split
`Schedulers` the way `okay.Scoped` splits across JDK versions —
Multi-Release JAR, a reduced class at the jar root, the full one under
`META-INF/versions/21/`. Checked the premise directly instead of
assuming it:

```
$ dotc (hosted on JDK 21) compiles a class with two methods:
    def safe(): String = "..."
    def risky(): Thread = Thread.startVirtualThread(() => ())
$ javap: major version 61 (JDK 17's own class file format — dotc's
  default target regardless of host JDK, unrelated to what APIs the
  source references)
$ java (JDK 17.0.19) loads the class and calls ONLY safe(): works.
```

JVM constant-pool resolution is lazy, per call site, not per class —
a class referencing an absent method anywhere in its body still
loads and runs fine on an older JVM, as long as that specific method
is never invoked. `Schedulers.class`, exactly as it already existed,
was already loadable on JDK 17. Splitting it into two class-file
variants (with the real cost that implies — `own`/`threads`/
`forkJoin`/`drive`/`adaptive`/`Running`/`Own`, several hundred lines,
would have needed a second, manually-synchronized copy to keep a
reduced variant self-contained) would have solved a problem that did
not exist. The actual problem was two call sites, not a class file.

## The fix

`Schedulers.hasVirtualThreads: Boolean = Runtime.version().feature() >= 21`
— checked once. `Runtime.version()` is JDK 9+, so this line itself
never fails on anything this library could plausibly run on.

`Schedulers.auto: Scheduler = if hasVirtualThreads then loom else own.build`
— a named, public method, callable directly from code (not only
reachable through the ambient `given` or the `-Dokay.scheduler`
property) — the operator asked for this explicitly: switching should
work in code too, not only via configuration.

`given Timer`'s `after`: a virtual thread where available, a plain
daemon `Thread` otherwise (the same shape `Schedulers.threads` already
uses for fibers).

`given Scheduler`: `own`/`adaptive`/`drive`/`threads` are always
honoured via `-Dokay.scheduler` (none of them need anything JDK
21-specific); an explicit `loom` request degrades to `Schedulers.auto`
when virtual threads are not actually there, rather than compiling
fine and then throwing the first time a fiber forks; unset or
unrecognized also resolves to `Schedulers.auto`. `"threads"` is a new
case in the switch — it existed as a scheduler but was never reachable
by property before this.

## Verification

**JDK 21 (this box's floor, unchanged-behavior check):** `okayJVM/test`
— 743 passed, identical count to before this change. A new suite,
`TestAdaptiveScheduler`, asserts `hasVirtualThreads` is true here and
`auto` picks `loom` (reference-equal — `loom` is a singleton `val`).

**JDK 17 (the actual claim, proven directly, not assumed):** packaged
`okayJVM`'s jar, compiled a tiny standalone probe against it (same
technique as script-scoped-state-mrjar / spark-jdk25-guard-fix — a
`java -cp` process, not sbt's own always-JDK21+ test loop), ran it
under `~/.sdkman/candidates/java/17.0.19-tem`:

```
Java: 17.0.19+10
hasVirtualThreads: false
auto == loom: false
timer fired: true (awaited ok=true)
PROBE PASS
```

Also checked the property-override paths land on the RIGHT
implementation class, not just "didn't throw":
`-Dokay.scheduler=loom` on JDK 17 → `okay.Schedulers$Owned` (correctly
degraded to `own`, not the anonymous `loom` class); the same flag on
JDK 21 → `okay.Schedulers$$anon$1` (correctly the real Loom
implementation, since it is both requested and available).

## What this does and doesn't change about the floor

This makes `okay` core (`okayJVM` and anything depending ONLY on it)
genuinely loadable and correct on JDK 17 — not merely compilable by a
JDK 21+ toolchain, which was already true and beside the point.

It does **not** move the project's own floor in `specs/
jdk-compatibility.md` — `okay-http`'s JDK backend
(`Executors.newVirtualThreadPerTaskExecutor`), `okay-jetty`'s
`VirtualThreadPool` (jetty-virtual-threads) and streaming write path,
`okay-netty`, `okay-cluster`, `okay-persist` and `okay-script` all
call JDK 21-only APIs DIRECTLY, outside `Schedulers`, unconditionally.
Each of those would need the same treatment — check whether their own
call sites can degrade the way `Schedulers`/`Timer` now do — before
the PROJECT's floor could move. This closes the `okay` core piece of
that; the rest is unclaimed follow-up, not assumed done.
