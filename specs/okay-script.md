# okay-script: markdown files as Scala source

## Overview

A markdown file with fenced ` ```scala ` code blocks is a literate
Scala program: `okay-script` extracts those blocks, drives the REAL
Scala 3 compiler (`dotty.tools.dotc`) on their concatenation, and runs
the result. No new language, no interpreter — the compiler and the
JVM runtime do all the work; this module is markup metadata
extraction plus minimal preprocessing plus meta-compilation, exactly
per the operator's framing ("наша цель только лишь извлечение
метаданных из разметки и минимальный препроцессинг и метакомпиляция").

Investigated `../scalascript` first, per the operator's pointer, for
prior art. It is unrelated in kind: a full custom programming language
whose own grammar treats Markdown headings/links/front-matter as
first-class syntax (`.ssc` files, its own VM/bytecode/multiple
backends). Nothing there is a markdown-block-runs-through-real-scalac
feature to reuse — its "Markdown is syntax" is markdown replacing
Scala, not markdown carrying Scala. No code or design was carried
over; only the negative result (no reusable pattern) is recorded here.

## Design decisions (asked of the operator, 2026-09-03)

- **Scoping**: one `.md` file is one compilation unit. Every ` ```scala `
  block in the file is concatenated in document order (blank line
  between blocks) and compiled together — a later block sees a `val`
  or `def` an earlier block introduced, the way a single Scala source
  file or a REPL session would. Not one-block-one-program: that would
  forbid a document from building up a running example across
  sections, which is the whole point of literate code.
- **Compiler**: the Scala 3 compiler API in-process
  (`dotty.tools.dotc.Driver`), not a shelled-out `scala`/`scala-cli`
  subprocess. No JVM-fork-per-run cost, and compiler diagnostics come
  back as structured data instead of parsed stdout.
- **Success criterion**: a run is `ok` iff the concatenated source
  compiles with zero errors AND the compiled program runs to
  completion without throwing. This is a smoke test, not a doc-output
  checker — it does NOT compare printed output against text in the
  markdown (that is a real, heavier feature — output-comparison
  literate testing, mdoc-style — and is filed to BACKLOG as a
  follow-on, not built here).
- **Surface**: a library API only (`ScalaScript.run(markdown: String):
  Result`). No sbt-test integration, no CLI entry point, no automatic
  walk of `specs/*.md` in the default gate. A caller (a future CLI, a
  future sbt task, a test) decides when to invoke it.

## The real goal (operator, 2026-09-03) — runtime app generation

The narrower "smoke test for docs" framing above is not the end goal.
The actual target, stated directly: generate a `.md` file AT RUNTIME
and have it come up as a live web application — frontend and
backend — the way `../scalascript` already does for ITS OWN language
("Идея в том чтобы сгенерировать файл md и потом его откомпилировать
и показывать как веб приложение — это удобно для создания
магазинов... все происходит в рантайме (даже компиляция)"). `okay`
already has the application-shaped pieces scalascript had to invent
its own language to get (`okay-ui`'s declarative reactive widgets,
`okay-jetty`'s `serve`) — so `okay-script` does not need to grow a
rendering layer at all. A markdown file's ```scala block IS the app:
ordinary code that imports `okay.ui`/`okay.jetty` and calls `serve`.
`okay-script`'s only job stays what it already was — compile and run
that code — but two assumptions baked into the first cut do not
survive contact with "generate this at runtime, for an app I did not
build the JVM around":

- **The classpath cannot stay ambient.** `Classpath.ambient` (below)
  is a convenience default, not a foundation — a runtime-generated
  script needs to be handed EXACTLY what it needs (its own
  okay-ui/okay-jetty jars, whatever else the generator decided this
  storefront needs), not whatever happens to be on the host process's
  own `-cp`. This was also a live, present-tense BUG, not just a
  future concern: see `okay-script-scalac-classpath` below.
- **A script may need a library `okay` itself was never built
  against.** A generated storefront could reasonably want a charting
  library, a payment SDK — something with no reason to be a
  dependency of `okay-script` or even of the host application.
  `//> using dep` (below) is how a script asks for one.
- **Blocking/lifecycle of a live server is deliberately left to the
  CALLER**, not solved inside `ScalaScript` — `okay-jetty`'s own
  `serve` returns `Server ! Resource`, and how long that resource
  stays acquired (i.e. how long the app stays up) is a decision for
  whoever generated the script and invoked `run`, typically on its own
  thread/fiber so `run` blocking (because the script's own body blocks
  to keep the server alive) does not block the generator.

## Lifecycle — resolved 2026-09-03 (okay-script-lifecycle)

The operator asked to settle this before the storefront example: how
does a runtime-compiled app start without blocking the generator, and
stop CLEANLY (not just get abandoned) later. No new `ScalaScript` API
was needed — the answer was already sitting in `okay-demo/
ChatDemo.main`, which has run a real `okay-jetty` server this whole
session via exactly this idiom:

```scala
Resource.run[Unit, Pure](
  Jetty.serve(port)(routes)().map { s =>
    println(s"...: http://127.0.0.1:${Jetty.port(s)}")
    Thread.sleep(Long.MaxValue)   // ctrl-c ends the process and the Resource
  }
).runWith
```

A script wanting to be a long-lived app writes exactly this shape. The
two facts that make it work as a STOPPABLE app, not just a permanently
blocked one:

1. **`Resource.run` releases on any escaping `Throwable`.**
   `Resource.scala`'s `_loop` wraps the whole scope in a
   `try ... catch { case e: Throwable => releaseAll(fin); throw e }` —
   an exception from ANYWHERE inside the scope, including a plain JVM
   `Thread.sleep` call (not itself an effect), runs every acquired
   release (in reverse order) before propagating. `Jetty.serve`'s
   `Resource.acquire` release closure stops the Jetty `Server`.
2. **`Thread.interrupt()` on the exact thread running the script makes
   `Thread.sleep` throw `InterruptedException`.** `ScalaScript.run`
   invokes the compiled script's `main` via reflection SYNCHRONOUSLY,
   on whatever thread called `run` — no thread-hop inside `run` itself
   — so a caller that puts `run` on its OWN dedicated `java.lang.Thread`
   holds the exact thread the script's `Thread.sleep` blocks on, and
   `.interrupt()` on it is a real, targeted stop signal, not a
   best-effort kill.

So the full recipe for a caller running a generated app:

```scala
val t = new Thread(() => resultBox.set(ScalaScript.run(md, classpath)))
t.start()                 // does not block the generator
// ... later, to stop this one app:
t.interrupt()             // Thread.sleep throws -> Resource releases -> server.stop()
```

`ScalaScript.run`'s returned `Result` (once the interrupted thread's
`run` call actually returns) carries `ok = false` and
`thrown = Some(interruptedException)` — indistinguishable, by design,
from any other uncaught exception a script could throw; a caller that
deliberately interrupted its own thread already knows why, so `Result`
does not need a separate "stopped on purpose" case.

Proved, not just asserted, by `TestScalaScriptLifecycle` (Live-tagged:
binds a real port): a script starting a real `Jetty.serve` on a free
port, run via `ScalaScript.run` on a background thread, is confirmed
UP by a real HTTP GET before interrupting, and confirmed DOWN (the
port refuses connections again) after — with the `Result` showing the
`InterruptedException`.

## Worked example — okay-script-storefront-example (2026-09-03)

`okay-script/examples/it-consulting-storefront.md`: a real storefront
— a services page and an `/order` route — served by a REAL
`okay-jetty` server, compiled and run entirely through
`ScalaScript.run`. This is the "generate a `.md`, compile it at
runtime, get a live web app" scenario the whole `okay-script-runtime`
pivot was for, proven end to end rather than left as a claim.

The service DATA (names, descriptions, prices, currency) is taken
verbatim from `../it-consulting/site/site.md` — the REAL IT consulting
line's site content, today rendered by a different system (`busi`'s
own declarative-site engine). Only the data crosses over; none of
`busi`'s or `scalascript`'s own rendering/DSL code does — the page and
the `/order` handler are ordinary Scala, written directly in the
```scala block, using nothing beyond `okay-http`'s `Response`/`Request`
and `okay-jetty`'s `serve`. `site.md`'s own `on order: receive job;
line work; line delivery` (a `scalascript` snippet, a different
language entirely) is NOT executed or reused — the example's `/order`
route does the equivalent (log the order, confirm it) as plain Scala
instead, which is exactly the point: `okay-script` runs code, not a
second DSL.

The example follows the lifecycle recipe settled above verbatim:
`Resource.run(Jetty.serve(port)(routes)().map { s => println(...);
Thread.sleep(Long.MaxValue) }).runWith`, with the port read from a
system property (`okay.script.storefront.port`, defaulting to 8099)
so a test can inject a free one without editing the file. Proved by `TestScalaScriptStorefront` (Live: binds a real port): reads
the `.md` file from disk, runs it via `ScalaScript.run` on a
background thread (the caller-owned-thread recipe), confirms `GET /`
renders all five services with their prices, confirms `GET
/order?key=<x>` returns a confirmation page naming that service (proof
the route actually ran, not just that SOME 200 came back), then
`Thread.interrupt()`s the thread and confirms the server stops
answering. `ScalaScript.run`'s own `stdout` capture is not usable as
that proof: it only becomes readable once `run` RETURNS, and this
script's `run` call does not return until interrupted — the response
BODY is the observable side channel here, not the process's stdout.

## okay-script-scalac-classpath — found and fixed 2026-09-03

Found by a sibling agent gating an unrelated change: `okayScript/test`
failed 5/7 on `master` itself, reproducing identically on a fresh
worktree — an environment break, not a code regression. Root cause,
confirmed by printing `System.getProperty("java.class.path")` from
inside the failing test JVM: `okay-script`'s `build.sbt` block never
set `Test / fork := true` (every other project in this build does),
so its tests ran INSIDE SBT'S OWN JVM — whose `java.class.path`
system property is just `sbt-launch.jar`'s own path. sbt manages its
real classpath through its own layered classloaders, invisible to
that property. `Classpath.ambient` (then just a raw call to that
property) handed dotc a one-entry classpath with no scala-library on
it at all, and dotc crashed deep in the Typer trying to resolve
`scala.Int` (`NoSymbol` where a `ClassSymbol` was expected). Fixed by
adding `Test / fork := true` — confirmed alone sufficient to take
`okayScript/test` from 5 failures to 0 before any of the classpath
redesign below was even written. The redesign below is a second,
independent fix: `Classpath.ambient` remains only as correct as the
JVM that read it was launched — a real `-cp`, not a manifest-jar
trampoline — which is exactly why a runtime app generator should
prefer an EXPLICIT `Classpath` over depending on it.

## The model

```scala
package okay.script

final case class Block(code: String, startLine: Int)

final case class Result(
  ok: Boolean,
  stdout: String,
  errors: Vector[String],   // compiler diagnostics, empty if it compiled
  thrown: Option[Throwable], // set iff it compiled but the run threw
)

/** Explicit classpath entries a script compiles and runs against.
 * `ambient` reads the CALLING JVM's own -cp -- a hypothesis about the
 * environment (see okay-script-scalac-classpath above), not a given.
 */
final case class Classpath(entries: Vector[Path]):
  def ++(extra: Vector[Path]): Classpath
  def asString: String
object Classpath:
  val ambient: Classpath

/** `//> using dep "org:artifact:version"` (scala-cli's own directive,
 * reused rather than inventing another one), hoisted from a script's
 * blocks and resolved to jars by shelling out to the `cs`/`coursier`
 * CLI -- fetching a coordinate is inherently a network operation, so
 * this reuses the standard external resolver instead of embedding
 * one; dotc compilation itself stays fully in-process.
 */
object Deps:
  def declared(markdown: String): Vector[String]
  enum Resolved:
    case Jars(paths: Vector[Path])
    case ToolMissing
    case Failed(message: String)
  def resolve(coords: Vector[String]): Resolved

object ScalaScript:
  def blocks(markdown: String): Vector[Block]
  /** classpath defaults to Classpath.ambient; a script's own `using
   * dep` coordinates are resolved and appended before compiling,
   * regardless of which classpath was passed in. */
  def run(markdown: String, classpath: Classpath = Classpath.ambient): Result
```

- `blocks` extracts every ` ```scala ` … ` ``` ` fenced region (a line
  matching ` ```scala ` exactly opens one, the next ` ``` ` line
  closes it — fences for any OTHER language tag, e.g. ` ```yaml `, are
  skipped whole). `startLine` is the 1-based line of the first line of
  code inside the fence, in the ORIGINAL markdown — carried so a
  future caller can map a compiler error's line back to the `.md`
  file, even though the extraction step does not do that mapping
  itself (see Results below — the current implementation reports
  dotc's own line numbers against the SYNTHETIC wrapped source, not
  yet translated back).
- `run` takes `blocks(markdown)`, concatenates their `code` bodies (in
  order, separated by a blank line) and wraps the result as the body
  of a single synthetic `@main def` (see Compilation below), compiles
  it against the CURRENT process's own classpath (so anything already
  on `okay-script`'s test/run classpath — including all of `okay`
  itself — is visible to a script that imports it), and if it
  compiles, loads and invokes the generated main method in the SAME
  JVM, with `System.out`/`System.err` captured into `stdout` for the
  duration of the call.
- A markdown file with zero ` ```scala ` blocks compiles an empty
  `@main def` body and trivially succeeds (`ok = true`, empty stdout)
  — there is nothing to compile OR run, and treating "no Scala here"
  as a Result rather than an error keeps `run` total over any `.md`
  file, not just ones authored for this tool.

## Compilation strategy

Top-level Scala 3 statements cannot be compiled and RUN directly as a
standalone program without a named entry point, so the concatenated
block text is wrapped:

```scala
@main def okayScriptMain(): Unit =
  <concatenated block bodies>
```

This is the one and only piece of synthesized syntax — everything
inside the braces is exactly what the markdown author wrote, unedited.
Wrapping as a METHOD BODY (not a top-level script) means:
- `import`, `val`, `def`, `class`, `given` are all legal, at any point
  in the concatenation, because a method body can contain local
  definitions — this is what makes "each block sees the previous
  block's definitions" true without extra plumbing.
- there is exactly one compiled artifact per run: a single
  `okayScriptMain` class with a generated `main(Array[String])`
  forwarder, found and invoked via reflection after compilation.

The synthetic source is written to a fresh temp file per `run` call
(`Files.createTempDirectory("okay-script-")`), compiled with
`-d <that dir> -classpath <classpath.asString>` via
`dotty.tools.dotc.Driver`, and — success or failure — the temp
directory is deleted before `run` returns (a script run leaves no
litter, matching `okay`'s general no-temp-file-residue expectation).
The `URLClassLoader` used to load and run the compiled class is built
from that SAME classpath (plus the temp output dir) — not from
whatever the calling JVM happens to expose — so compiling and running
see identical types.

Compiler errors are collected from a custom `Reporter` (not dotc's
default, which prints to stderr) and returned as `errors`, one string
per diagnostic, instead of thrown.

`run`'s dependency step (`Deps.declared` + `Deps.resolve`) runs BEFORE
compilation: a script's `//> using dep` coordinates are resolved to
jars via the `cs`/`coursier` CLI (a `ProcessBuilder` around `cs fetch
<coords>`, one jar path per stdout line) and appended to whichever
`Classpath` was passed in. A resolution failure (bad coordinate, no
network) or a missing `cs`/`coursier` binary is reported through the
ordinary `Result.errors` channel — never thrown — so a caller sees it
exactly like a compile error. `dotc` itself never touches the network;
only this one step, and only when a script asks for it, does.

## What this is NOT (filed to BACKLOG, not built here)

- **Output-comparison literate testing** (mdoc-style: a block's
  expected stdout written inline in the markdown, checked against the
  real run). `run`'s `Result.stdout` already captures everything
  needed to build this on top — the comparison and the markdown
  convention for "expected output" are the missing piece.
- **sbt-test / CI integration** — a task that walks `specs/*.md` (or a
  configurable directory) and fails the build on the first `!ok`
  Result. Deliberately out of scope this pass per the operator's
  answer ("библиотека/API, без интеграции в sbt test пока").
- **Line-accurate error mapping** back from the synthetic wrapped
  source to the original `.md` file's line numbers — `Block.startLine`
  is captured for this purpose but not yet used to translate a dotc
  diagnostic's line number.
- **Classloader isolation** between multiple runtime-compiled scripts
  running in the same host JVM at once (two generated storefronts
  loading conflicting versions of the same library, say). The
  `URLClassLoader` built per `run` call already gives each script its
  OWN classes, but its parent is still `getClass.getClassLoader` (this
  module's own loader) rather than a minimal platform-only parent, so
  isolation from the host is partial, not guaranteed. Filed to
  BACKLOG; not needed until a caller actually runs more than one
  generated app per JVM.

## Behavior

- [x] `blocks` extracts every fenced ` ```scala ` region in document
      order, with the correct 1-based `startLine`, and skips fences
      tagged with any other language.
- [x] a markdown file with one ` ```scala ` block that prints and
      returns normally: `run` reports `ok = true`, `stdout` contains
      what was printed, `errors` is empty, `thrown` is `None`.
- [x] a markdown file with two ` ```scala ` blocks, where the second
      references a `val`/`def` the first introduced: `run` succeeds —
      proving the single-compilation-unit scoping decision.
- [x] a markdown file whose Scala block has a compile error: `run`
      reports `ok = false`, `errors` non-empty, `thrown = None` (never
      throws a compiler exception out of `run` itself).
- [x] a markdown file whose Scala block compiles but throws at
      runtime: `run` reports `ok = false`, `thrown = Some(...)`,
      `errors` empty (it DID compile).
- [x] a markdown file with zero ` ```scala ` blocks: `run` reports
      `ok = true` trivially.
- [x] no temp file/directory survives a `run` call, success or
      failure (checked by diffing `Files.list` of the system temp
      root before/after).
- [x] `Deps.declared` extracts every `//> using dep "..."` coordinate
      from a markdown file's blocks, in order, deduplicated.
- [x] `run` honors an explicit `Classpath` override: an EMPTY one
      fails to compile even the trivial `println(1)` script (no
      scala-library reachable) — proving the parameter is not silently
      ignored in favor of ambient.
- [x] (Live) `Deps.resolve` on a real Maven coordinate, with `cs`/
      `coursier` present, returns jar paths that exist on disk.
- [x] (Live) a script declaring `//> using dep` for a library NOT
      already reachable on `okay-script`'s own classpath (`fansi`, not
      a transitive dep of anything in this build) compiles and runs
      successfully, loading a class from the resolved jar — proving
      the resolved jar was actually added to the classpath, not that
      the class happened to already be visible.
- [x] (Live) a script that starts a real `okay-jetty` server through
      `Resource.run(Jetty.serve(...)().map { s => ...; Thread.sleep(
      Long.MaxValue) }).runWith`, run via `ScalaScript.run` on a
      background thread: an HTTP GET against the bound port succeeds
      WHILE the thread is alive; after `Thread.interrupt()` on that
      thread, the SAME GET starts failing (the server actually
      stopped, not just the thread abandoned), and the eventual
      `Result` has `thrown = Some(_: InterruptedException)`.
- [x] (Live) `examples/it-consulting-storefront.md`, read from disk and
      run via `ScalaScript.run`: `GET /` renders all five services
      (name + price) from `../it-consulting/site/site.md`'s real data;
      `GET /order?key=<x>` returns a confirmation page naming that
      service; `Thread.interrupt()` stops the server.

## Results

Landed 2026-09-03 (core), extended 2026-09-03 (runtime-app follow-on:
explicit `Classpath`, `//> using dep` + Coursier resolution; lifecycle:
`Thread.interrupt()` on the caller's own thread, no new API). Traps
found by the tests, all fixed before landing:

- **Lifecycle needed no fix — the hypothesis held on the first real
  run.** `TestScalaScriptLifecycle` confirmed, against a REAL
  `okay-jetty` server (not a mock): the server answers HTTP while its
  driving thread is alive, `Thread.interrupt()` makes it stop
  answering (not just abandon the thread — `Resource.run`'s
  catch-and-release actually ran the server's own stop), and the
  returned `Result` carries the `InterruptedException`. Confirms
  `okay-script` needed zero new API for a stoppable runtime app: the
  `ChatDemo.main` idiom (`Resource.run(...).map { s => ...;
  Thread.sleep(Long.MaxValue) }.runWith`) plus running `ScalaScript.run`
  on a caller-owned `Thread` is the whole answer.

- **`println` inside the compiled script did not land in `stdout`** —
  `System.setOut` alone does not redirect it, because Scala's
  `println`/`Predef` goes through `scala.Console.out`, a
  `DynamicVariable` that is NOT re-read from `System.out` on every
  call. Fix: wrap the invocation in `scala.Console.withOut(ps)`
  *in addition to* `System.setOut` (a reflective callee could still
  write directly to `System.out`, so both are captured).
- **A markdown file with zero code blocks failed to COMPILE**, not
  just failed to do anything useful: the synthesized
  `@main def okayScriptMain(): Unit =` followed by an empty body is a
  syntax error (a method body must be present). Fixed by defaulting
  the body to `()` when there are no blocks — the "zero blocks is
  trivially ok" behavior lives in the wrapping step, not as a special
  case in `run`.
- **`okayScript/test` was silently broken on master itself**
  (okay-script-scalac-classpath, see above) — `Test / fork := true`
  was missing from `okay-script`'s own `build.sbt` block, so its tests
  ran inside sbt's own JVM, and `Classpath.ambient`'s
  `System.getProperty("java.class.path")` read `sbt-launch.jar`'s path
  instead of the real test classpath. A sibling agent found this while
  gating an unrelated change; fixed here by adding the missing
  `Test / fork := true`, confirmed alone sufficient before the
  `Classpath`/`Deps` redesign was even written.

`blocks`' `startLine` is the line of the first CODE line inside the
fence (one past the ` ```scala ` line itself), confirmed against a
hand-counted markdown fixture in the test.

`Deps.resolve` shells out to `cs`/`coursier` (found on `PATH`, tried
as both names) via a plain `ProcessBuilder` rather than embedding
Coursier as a library dependency — avoids pulling a resolver into
`okay-script`'s own compile-time dependency graph for a step that only
runs when a script actually asks for an external library.
