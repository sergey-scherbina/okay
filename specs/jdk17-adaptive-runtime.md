# Adapting the six broken modules to run on JDK 17

Follow-up to jdk-adaptive-scheduler (core) and jdk17-compat-check
(measurement). That work made `okayJVM` itself adaptive
(`Schedulers.hasVirtualThreads`, `Schedulers.auto`, see
`src/main/scala-jvm/Platform.scala:200-209`) and then measured, for real,
which OTHER modules still call a JDK21+ `Thread`/`Executors` API
unconditionally and break the first time that code path runs on JDK 17
(`specs/jdk-compatibility.md`, "Building for JDK 17 is not the question").
This spec is the fix for the six modules that measurement found broken.

**IMPLEMENTED AND MEASURED (2026-09-20).** Every call site named below was
changed as planned. Three modules are now genuinely, fully green on real
JDK 17 with no caveat: `okay-script` (208/208), `okay-cluster` (136/136),
`okay-persist` (242/242 default + 13/13 `Live`). `okay-netty` is fully
green too (16/16, `Live` included) — its own connector was never the
problem; it only failed before because `okay-jetty`'s stale classfile
(below) poisoned its test classpath. `okay-http` and `okay-jetty` are
**partially fixed**: the unconditional-API crashes this spec targeted are
gone, but each surfaced a SECOND, deeper, pre-existing problem — see
"What this surfaced that the spec didn't plan for", below. Read that
section before treating either module as done.

## The pattern already proven

`Schedulers.hasVirtualThreads: Boolean = Runtime.version().feature() >= 21`,
checked once, used to branch. No MRJar, no separate build — a class that
CONTAINS a JDK21+ call site loads fine on 17 as long as that call site isn't
reached (jdk-adaptive-scheduler's own finding, re-confirmed by every
`NoSuchMethodError` this session's measurement pass produced: they are all
first-use failures, never load-time ones).

## A shared helper, because the same three lines would otherwise repeat six times

Five of the seven call sites below are the same shape: "start a daemon
thread that runs `body`, virtual if this JVM has them, platform otherwise."
Duplicating that branch five times across five modules is exactly the case
that earns a shared primitive rather than an inline `if` at each site — one
place to get the naming/daemon-flag/interrupt behavior right, one thing to
test. Proposed, in core (`src/main/scala-jvm/Platform.scala`, next to
`Schedulers`, not inside it — this is a thread primitive, not a scheduler):

```scala
object Threads:
  /** a daemon thread running `body`: virtual when this JVM has them
   *  (Schedulers.hasVirtualThreads), an ordinary daemon Thread otherwise.
   *  Fire-and-forget — same contract `Timer`'s own fire callback already
   *  uses inline; this just gives the six call sites below one name
   *  instead of five copies of the same branch. */
  def spawn(name: String)(body: () => Unit): Unit =
    if Schedulers.hasVirtualThreads then
      Thread.ofVirtual().name(name).start(() => body())
    else
      val t = Thread(() => body(), name)
      t.setDaemon(true)
      t.start()
    ()
```

`okay-jetty`'s `VirtualThreadPool` case does NOT go through this helper —
it's a Jetty `ThreadPool` object, not a bare `Thread`, so its fix is its own
conditional constructor (below).

## Per-module plan

### okay-script — `Sessions.scala:89`

```scala
tail.foreach(ms => Thread.ofVirtual().name("okay-script-sessions-tail").start(() => tailLoop(ms)): Unit)
```
→
```scala
tail.foreach(ms => okay.Threads.spawn("okay-script-sessions-tail")(() => tailLoop(ms)))
```
One call site, one regression test: `TestSessionsShared` (already exists,
currently the thing that proves the break under `verifyJdk17` — becomes the
thing that proves the fix, on the same JDK17 fork).

### okay-http — `Server.scala:34`

```scala
s.setExecutor(java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor())
```
This one is not `Threads.spawn`-shaped — it wants an `ExecutorService`, not
a single fire-and-forget thread. Fallback candidate:
`Executors.newCachedThreadPool()` (unbounded, on-demand platform threads —
the pre-JDK21 idiom for "one thread per request", same risk profile a
`com.sun.net.httpserver.HttpServer` already assumes when unconfigured).
```scala
s.setExecutor(
  if Schedulers.hasVirtualThreads then Executors.newVirtualThreadPerTaskExecutor()
  else Executors.newCachedThreadPool())
```
Regression: `TestHttp`/`TestWs` (`Live`-tagged) under `verifyJdk17` with
`--include-tags=Live` forced — this session's measurement already showed
`TestWs` as the first to hit this path.

### okay-jetty — `Jetty.scala:122` (own pool) and `:235` (stream write)

Two independent call sites, two independent fixes:

- **`:122`**, `Server(VirtualThreadPool())` — `VirtualThreadPool`'s own
  constructor throws `IllegalStateException` on <21 (confirmed this
  session, not a `NoSuchMethodError` — jetty-virtual-threads guards itself).
  Fix: `Server(if Schedulers.hasVirtualThreads then VirtualThreadPool() else QueuedThreadPool())`
  — `QueuedThreadPool` is Jetty's own pre-loom default, so this is a
  reversion to what `Server()` did before this session's
  `jetty-virtual-threads` change, gated instead of unconditional.
- **`:235`**, `stream`'s `Thread.startVirtualThread(...)` (the
  chunk-by-chunk response body writer) → `okay.Threads.spawn("okay-jetty-stream")(...)`.

Regression: `TestResumable`, `TestJetty` (`Live`), the exact two suites the
measurement pass already named.

### okay-netty — `Netty.scala:274`

```scala
Thread.startVirtualThread { () => ... }
```
→ `okay.Threads.spawn("okay-netty-stream")(() => ...)`. Note from the
measurement: `okay-netty`'s OWN tests failed via its `okayHttp.jvm`
dependency (`Server.scala:34`) before ever reaching this call site — so
this module's fix is necessary but not sufficient by itself; `okay-http`'s
fix above has to land first (or together) for `okay-netty`'s suite to prove
anything on JDK 17.

### okay-cluster — `Served.scala:50`

```scala
val _ = Thread.ofVirtual().start(() => handle(sock, serve))
```
→ `okay.Threads.spawn("okay-cluster-served")(() => handle(sock, serve))`.
Regression: the four suites the measurement pass already named
(`TestDistributed`, `TestAcceptance`, `TestCluster`, `TestStream`).

### okay-persist — `RaftWire.scala:150-157`, `Wire.scala:75,81`

Five call sites, same shape, all fire-and-forget:
- `RaftWire.scala:150` — `acceptLoop()`
- `RaftWire.scala:151` — `tickLoop()`
- `RaftWire.scala:157` — `handleConn(sock)`
- `Wire.scala:75` — `acceptLoop()`
- `Wire.scala:81` — `serve(sock)`

All five → `okay.Threads.spawn(name)(...)`, name strings adapted from each
site's role (e.g. `"okay-persist-raft-accept"`, `"okay-persist-raft-tick"`).
Regression: `TestRaftWire`, `TestRaftStore`, `TestWireTls` (`Live`) — the
three the measurement pass already named, plus `TestWire`/`TestWireClient`
if they turn out to route through `Wire.scala`'s two sites (check when
implementing; not confirmed either way by this spec).

## What this surfaced that the spec didn't plan for

Two real, separate problems, found by actually running the fixes on real
JDK 17 rather than trusting the plan above — the second one changes this
spec's risk picture materially.

### 1. `okay-jetty`'s one Java source outran the project's own floor

`Listen.java` (the module's only `.java` file) was compiled to classfile
major version **65** (JDK 21) whenever sbt itself ran on 21+, because
`javac` — unlike `dotc` — targets the launching JVM's own bytecode level
by default. Every Scala class in this codebase has been JDK17-loadable
(major 61) all along (jdk-adaptive-scheduler's finding); this one Java
file quietly wasn't. `UnsupportedClassVersionError: ... class file version
65.0, this version ... only recognizes ... up to 61.0`, on the FIRST test
in `okay-jetty` and (via a `Test`-scoped dependency) in `okay-netty` too.
**Fixed**: `javacOptions ++= Seq("--release", "17")` on `okayJetty`'s
project settings in `build.sbt`. No other `.java` file in the six modules'
dependency graph exists (`grep -rn` across the repo confirms `compare` and
`okay-flink` are the only other homes for `.java` sources, both out of
scope here).

### 2. `Schedulers.own`'s bounded pool can genuinely HANG a real integration test, not just answer late

jdk17-adaptive-runtime's own "explicitly out of scope" section (below)
already named `okayJVM`'s `TestPar`/`TestDirectParallel` findings from
jdk17-compat-check as a real trade-off — a fiber that makes a raw
OS-blocking call (`Thread.sleep`, `CountDownLatch.await`) inside
`Schedulers.own`'s bounded platform-thread pool can starve a sibling fiber
waiting on the same pool. That was characterized as a timing/wrong-answer
risk. It is worse than that: **`okay-http`'s `Nio.scala` (raw blocking NIO,
`Async.spawn`'d) genuinely deadlocks `TestNio` on real JDK 17** — confirmed
by isolating it (`okayHttpJVM/testOnly okay.http.TestNio` alone, twice,
`timeout 30`/`40`, `exit 124` both times, not even the suite header ever
printed) after first ruling out `TestHttp`, `TestMcpHttp`, `TestMcpPushServer`
(all green, fast) as the cause. `Nio.listen`'s accept loop and every
accepted connection's read loop are each a fiber parked in a real blocking
`ServerSocketChannel`/`SocketChannel` call, scheduled through `given
Scheduler` (`Schedulers.auto` → `own` on 17) — the SAME bounded pool every
other `Async` program in the same JVM shares. Enough concurrently-blocked
fibers exhaust it with nothing left to run the client side or the next
accept, and — unlike `TestPar`'s case — there is no fallback path that
un-sticks it; it hangs until killed.

`okay.Threads.spawn` (this spec's own helper) does NOT have this problem —
it's a dedicated platform `Thread` per call, not a fiber sharing a bounded
pool, which is exactly why `okay-cluster`'s `Served.serve` and
`okay-persist`'s accept loops (both converted to it) pass cleanly under
real concurrent socket load while `Nio.scala` (which goes through
`Async.spawn` instead) does not. **The difference is the mechanism, not
the JDK version as such** — `Nio.scala` would have this exposure on ANY
JVM where `Schedulers.auto` picks `own` instead of `loom`, JDK 17 is just
the concrete case that surfaces it here.

`okay-jetty`'s `TestResumable` (the resumable SSE stream, 4 tests) also
hangs on real JDK 17 — confirmed the same way (`timeout 150`, `exit 124`,
`kill -QUIT` mid-run showed a live server, four live `HttpClient` groups,
and Jetty's own `qtp*` pool all present and not obviously deadlocked
against each other, consistent with the SAME `Schedulers.own`-starvation
shape rather than a Jetty-specific bug — a control run forcing
`QueuedThreadPool` on the AMBIENT JDK 21 passed the same file in 3s,
which rules out `QueuedThreadPool` itself as the cause and points back at
the platform-thread-fiber-pool explanation). NOT root-caused to the same
certainty as `TestNio` — `TestResumable`'s own code doesn't call `Nio`
directly, so the exact chain from "fiber blocks" to "hang" wasn't traced
line by line the way `TestNio`'s was. Recorded as the same class of
problem on the evidence available, not asserted with the same confidence.

**No fix attempted.** This is not a missing branch at an API call site —
it is `Schedulers.own`'s own bounded-pool design meeting a workload
(blocking accept/read loops, one fiber held per connection for its whole
life) it was not sized for. A real fix is one of: (a) make `own` detect
or tolerate blocking calls (grow the pool, or a dedicated blocking-pool
tier), (b) convert `Nio.scala` to `okay.Threads.spawn`-per-connection the
way `okay-cluster`/`okay-persist` already are (loses the fiber-cooperative
structure `Async.spawn` gives the rest of that file), or (c) accept that
`Nio`-based transports and Jetty's SSE resumable stream are JDK 17-unsafe
and say so plainly wherever JDK 17 is offered as an option. None of the
three is attempted here; this spec's own scope (per-call-site API swaps)
does not reach it.

**RESOLVED 2026-09-20 (own-lost-wakeup), and the diagnosis above was
wrong in one word: not exhaustion, a lost wakeup.** The thread dump of
the hung `TestNio` fork showed fourteen `own` workers with ONE blocked
in `accept()` and thirteen parked; the client fiber sat in the
submission queue because a worker inside a task counts as awake, so an
outside fork woke nobody. The fix is none of (a)–(c): `Schedulers.auto`
now picks `Schedulers.platform` — `own` with its stuck-check on — where
there is no Loom, and the stuck-check unparks a sleeper before it
grows. `TestNio` 6/6 and `TestResumable` 4/4 on real JDK 17; BUGS.md
`own-lost-wakeup` has the dump and the laws.

## Order and independence

Each module's fix is independently landable and independently gated — none
of the six blocks another except the `okay-netty` → `okay-http` dependency
noted above. Suggested order, cheapest/most-isolated first:
`okay-script` (one site) → `okay-cluster` (one site) → `okay-jetty` (two
sites, one non-trivial) → `okay-http` (one site, `ExecutorService` shape) →
`okay-netty` (one site, depends on `okay-http` landing first) →
`okay-persist` (five sites, most surface area).

The shared `okay.Threads.spawn` helper is core, so it lands once, first,
before any of the six — either as its own tiny claim or folded into
whichever module is picked up first (`okay-script` is the natural one:
smallest diff, proves the helper compiles and works before the other five
depend on it).

## Done-when (per module) — measured outcome, 2026-09-20

`verifyJdk17` (build.sbt), scoped to that module, with `Test/fork := true`
and, where the real suite is `Live`-tagged (jetty, netty, http's `TestNio`,
persist's wire suites), `--include-tags=Live` forced too — both required,
per the measurement trap `jdk17-compat-check` already documented
(`Test/javaHome` is a silent no-op without `Test/fork`).

| module | outcome |
|---|---|
| `okay-script` | **green**, 208/208 |
| `okay-cluster` | **green**, 136/136 |
| `okay-persist` | **green**, 242/242 default + 13/13 `Live` |
| `okay-netty` | **green**, 16/16 `Live` (was only ever broken by `okay-jetty`'s classfile issue, transitively) |
| `okay-http` | crash fixed; `TestNio` **hangs** (§ above) — not green |
| `okay-jetty` | crash fixed, classfile issue fixed; `TestResumable` **hangs** (§ above) — not green |

Four of six modules are done and verified green on both 17 and the
project's normal ambient default (21) — additive branches, no behavior
change on the JDK that already worked. The other two are NOT done; see
"What this surfaced" above for why and what a real fix would need.

## Explicitly out of scope here

- `okayJVM` core's own two non-API-absence JDK17 findings (`TestPar`,
  `TestDirectParallel` — the bounded-platform-thread-pool /
  blocking-call trade-off in `Schedulers.own`) are a property of the
  fallback scheduler itself, not a missing branch — no fix is proposed for
  them by this spec. (The SAME trade-off turned out to cause genuine hangs,
  not just wrong timing, in `okay-http`/`okay-jetty` — see above; still not
  fixed here, for the same reason.)
- Actually lowering the project's floor below 21 anywhere — this spec makes
  four of six modules NOT CRASH on 17; `okay-http` and `okay-jetty` remain
  genuinely unsafe there, so that decision is further off than it looked
  when this spec was first written.
