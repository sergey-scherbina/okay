# Adapting the six broken modules to run on JDK 17

Follow-up to jdk-adaptive-scheduler (core) and jdk17-compat-check
(measurement). That work made `okayJVM` itself adaptive
(`Schedulers.hasVirtualThreads`, `Schedulers.auto`, see
`src/main/scala-jvm/Platform.scala:200-209`) and then measured, for real,
which OTHER modules still call a JDK21+ `Thread`/`Executors` API
unconditionally and break the first time that code path runs on JDK 17
(`specs/jdk-compatibility.md`, "Building for JDK 17 is not the question").
This spec is the fix for the six modules that measurement found broken.
Nothing here has been implemented yet.

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

## Done-when (per module)

`verifyJdk17` (build.sbt), scoped to that module, with `Test/fork := true`
and, where the real suite is `Live`-tagged (jetty, netty, persist's wire
suites), `--include-tags=Live` forced too — both required, per the
measurement trap `jdk17-compat-check` already documented (`Test/javaHome`
is a silent no-op without `Test/fork`). Green on 17, still green on the
project's normal ambient/default JDK (21) — this is an additive branch, not
a behavior change on the JDK that already worked.

## Explicitly out of scope here

- `okayJVM` core's own two non-API-absence JDK17 findings (`TestPar`,
  `TestDirectParallel` — the bounded-platform-thread-pool /
  blocking-call trade-off in `Schedulers.own`) are a property of the
  fallback scheduler itself, not a missing branch — no fix is proposed for
  them by this spec.
- Actually lowering the project's floor below 21 anywhere — this spec makes
  six modules NOT CRASH on 17, which is a precondition for that decision,
  not the decision itself.
