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
/order/<x>` returns a confirmation page naming that service (proof
the route actually ran, not just that SOME 200 came back), then
`Thread.interrupt()`s the thread and confirms the server stops
answering. `ScalaScript.run`'s own `stdout` capture is not usable as
that proof: it only becomes readable once `run` RETURNS, and this
script's `run` call does not return until interrupted — the response
BODY is the observable side channel here, not the process's stdout.

## Metadata as context — `okay.script.Meta` (okay-script-meta, 2026-09-03)

The operator's ask directly: code defined inside an `.md` file should
be able to read the metadata defined in the markup AROUND it, as its
CURRENT CONTEXT — the way `../it-consulting/site/site.md` carries YAML
front-matter (`tagline`, `contact`, ...) plus a nested ```yaml
`services` list under its own "# Услуги" heading, and code living
under that heading (or a deeper one) should see both without the
markdown author having to re-declare them as Scala literals.

### What counts as metadata

- **Front-matter**: a `---` / `---` delimited block at the very START
  of the file, `key: value` lines, FILE-LEVEL (visible everywhere).
- **Nested ```yaml fences**, scoped by the HEADING (`#`…`######`) they
  physically sit under — the shape already seen in `site.md`'s own
  `services` block (a `- key: v` list of flat objects) and `model.md`
  (a flat mapping). A yaml fence attaches to the NEAREST ENCLOSING
  heading (or to the file root if it appears before any heading).
- Heading STRUCTURE itself (level + title) is part of the AST too
  (`Section.level`/`.title`), not just the yaml payload — a heading
  with no yaml block at all is still a real `Section` node.

A YAML fence's content is METADATA — CONSUMED, not shown in `render`'s
output (a deliberate, documented change from "every non-```scala fence
passes through verbatim": that rule still holds for every OTHER
language tag, e.g. ```json — only ```yaml is now special). A heading
LINE itself (`## Услуги`) is ordinary prose too and still renders
verbatim, in addition to driving the section tree.

### The typed AST — `okay.script.Meta`

```scala
package okay.script

object Meta:
  enum Value:
    case Str(s: String)
    case Arr(items: Vector[Value])
    case Obj(fields: Vector[(String, Value)])

  final case class Section(level: Int, title: String, yaml: Vector[Value], children: Vector[Section])
  final case class Doc(frontMatter: Map[String, String], root: Section)  // root: level=0, title=""

  /** `doc` is the WHOLE file's tree, always navigable; `path` is the
   * ancestor chain (root..nearest heading) for THIS position in the
   * document. */
  final case class Context(doc: Doc, path: Vector[Section]):
    /** nearest-enclosing-heading-wins, then front-matter */
    def get(key: String): Option[String]
    def apply(key: String): String   // throws if absent
    def section: Option[Section]     // nearest REAL heading (excludes the synthetic root)
```

- `Context.get`/`apply` is the UNTYPED access the operator asked for —
  no schema needed, works on any front-matter/yaml shape.
- `Context.doc` is the TYPED AST access also asked for — the full
  tree, independent of where in the document the reading code sits
  (want a SIBLING section's data, or the whole file's structure? walk
  `doc.root`).
- **A path Section's `children` reflects only what had CLOSED by the
  time that position was reached** (document order, same rule
  `run`/`render` already apply to `val`/`def` visibility) — a
  currently-OPEN ancestor's later subsections are not yet in its
  `children` field for a `path` entry, even though they ARE present on
  the SAME section reached via `doc.root` (which is only handed to
  synthesized code once the WHOLE file has been parsed, so it is
  always complete). This is a real, documented asymmetry, not a bug.

### How code reaches it — `Meta.current`, NOT an injected `given`

The first design written here injected a fresh local `given
okay.script.Meta.Context` before each segment whose heading path
differed from the one before it, reasoning that Scala local
re-declaration shadows forward the way `val` does for `run`'s own
`val`/`def` visibility. **Tested before landing, empirically, and
WRONG on both counts**, corrected here rather than silently:

1. **Re-declaring a `given` with the same name at the same flat scope
   is a COMPILE ERROR** (`"_okayScriptMeta_ is already defined as
   given instance _okayScriptMeta_"`) — unlike a plain `val`, a
   `given` does not get the ordinary local-shadowing leniency.
2. Even past that: **a plain `given x: T = expr` is evaluated ONCE**
   (memoized, like a `lazy val`), not re-evaluated per `summon` —
   confirmed with a throwaway two-line probe (`given ctx: Int =
   Holder.v`, print `summon[Int]`, mutate `Holder.v`, print again:
   `1` `1`, not `1` `2`). So even giving each declaration a UNIQUE
   name would not have worked: whichever one got summoned would keep
   answering with ITS OWN one-time snapshot, and worse, more than one
   same-TYPE given in one flat scope is ambiguous for `summon`/`using`
   resolution regardless of name. A real per-position auto-refreshing
   `given` needs actual NESTED lexical scopes — which `run`/`render`
   deliberately do NOT have, because that is exactly what keeps a
   `val`/`def` from one ```scala block visible to every LATER block
   in the file.

The actual mechanism: `Meta.current: Context` (a plain, always-fresh
method reading a mutable var) and `Meta.setCurrent(c: Context): Unit`.
The shared tokenizer (below) still tracks the heading path per
segment/block, and source synthesis still emits something before each
segment whose path differs from the one before it — but a PLAIN
STATEMENT, not a binding:

```scala
okay.script.Meta.setCurrent(okay.script.Meta.Context(_okayScriptDoc_, Vector(<path, LITERALIZED>)))
```

Plain statements have none of `given`'s restrictions — this compiles
and behaves exactly as intended, verified by the actual test suite
(not just a probe). `_okayScriptDoc_` is a `val` holding the WHOLE
`Meta.Doc`, literalized ONCE at the top of the synthesized body (a
Scala constructor-call source string, e.g. `Meta.Doc(Map(...),
Meta.Section(...))`) rather than embedding the raw markdown for the
script to re-parse at its own runtime.

**This metadata machinery is emitted ONLY when the document actually
has some** (`hasMeta`: non-empty front-matter, or the root has yaml or
a heading) — a document with none of those (every script/example that
predates this feature, and the common case going forward for a plain
app/script) gets NO `okay.script.Meta` reference in its synthesized
source at all. This matters beyond tidiness: it is what keeps
`run`/`render` SELF-SUFFICIENT (compilable against scala-library
alone) for a metadata-free script — the first version of this feature
unconditionally emitted the `Meta` reference and broke exactly that,
caught by `TestScalaScriptClassloaderIsolation`'s minimal-Classpath
test (`Not found: okay` — `okay-script`'s own classes were not on that
test's deliberately narrow `Classpath`). Using this feature AT ALL
still needs `okay-script`'s classes reachable from the script's own
`Classpath`, same as any other library a script imports.

Access from a script (an excerpt of an `.md` file, shown as markdown
itself — the outer fence below is 4 backticks specifically so its
OWN inner ```scala fence renders literally):

````markdown
```scala
import okay.script.Meta
val greeting = Meta.current("tagline")
```

Всего услуг: ${Meta.current.section.map(_.yaml.size).getOrElse(0)}
````

A script that wants `given`/context-function ERGONOMICS can still
have them — just declared LOCALLY, immediately before use, which is
correct precisely because it is a fresh read at that exact point, not
something the injected machinery carries forward across a heading
transition:

```scala
def greet()(using ctx: Meta.Context): String = "hi " + ctx("name")
given Meta.Context = Meta.current
println(greet())
```

## Interpolation — `render`, "JSP but Scala+Markdown" (okay-script-interpolation, 2026-09-03)

The operator's own framing for where `okay-script` sits: a new JSP,
except the markup is Markdown and the embedded language is real
Scala. Two things separate JSP from `run` as built: JSP mixes
expressions directly INTO the markup text (`<%= expr %>`), not only in
a separate code block; and JSP recompiles/reruns per request. This
step is the first: prose-level interpolation. Per-request/hot-reload
is a separate, later question (closer to an `okay-jetty` route than to
`okay-script` itself) and is not addressed here.

**A new function, `render`, not a change to `run`.** `run` stays
exactly what it is — compile the concatenated ```scala blocks, run
them for their SIDE EFFECTS (the storefront's server, an app's
println output) — because every existing consumer (the storefront
example, the lifecycle tests) depends on that contract and has no
prose-interpolation need. `render(markdown, classpath):
Result` is new and separate: it treats the ENTIRE document — prose
AND code — as the program, and its product is the RENDERED TEXT, not
side effects. It reuses `Result` (the rendered document is
`Result.stdout` on success) rather than inventing a second result
type, since the underlying mechanism is identical: compile one
synthesized program, run it, capture what it printed.

### The `${expr}` marker

```
Здесь пять услуг, дороже всех — ${services.maxBy(_.price).name}.
```

- `${...}` in PROSE (i.e. outside a ```scala fence) is a Scala
  expression; `render` evaluates it in the SAME document-order scope
  the enclosing ```scala blocks build (a `val`/`def` from an EARLIER
  block, or an earlier interpolation's side effect, is visible; one
  from a LATER block is not — identical scoping rule to `run`'s
  block-concatenation, just extended to interpolations too), and
  appends its `.toString` to the output in place of the marker.
- `$${` is the escape for a literal `${` in the output (mirrors
  Scala's own `s"...$$..."` convention for a literal `$`) — needed for
  documentation ABOUT `okay-script` itself, this spec included.
- The scanner is brace-depth- and quote-aware, not a naive
  first-`}`-wins regex: `${ if (n > 0) "yes" else "no" }` and
  `${services.map(s => s"${s.name}").mkString(", ")}` (a NESTED real
  Scala string interpolation inside the expr) both parse correctly —
  a `{`/`}` inside a double-quoted span within the expression does not
  count toward the brace depth that closes the marker.
- `${...}` has no special meaning INSIDE a ```scala fence — code
  there is Scala already; if it contains its OWN `s"...${x}..."`
  string interpolation, that is ordinary Scala, untouched.
- Everything outside `${...}` markers — prose text, headings, other
  fenced blocks (` ```yaml `, etc.) — passes through to the output
  BYTE-IDENTICAL, fence markers included. `render` renders the whole
  document; only `${...}` substitutes.

### Compilation strategy

The document is tokenized (line-by-line, mirroring `blocks`' own fence
detection) into `Text` / `Code` / `Interp` segments in document order,
then synthesized into ONE `run` body (see the top-level "Compilation
strategy" for why it is `object OkayScriptMain: def run(args)`, not
`@main`):

```scala
object OkayScriptMain:
  def run(args: Array[String]): Unit =
    print("""<a Text segment, raw triple-quoted>""")
    <a Code segment's statements, verbatim>
    print((<an Interp segment's expr>).toString)
    ...
```

Each `Text`/`Interp` segment is an inline `print(...)` call in document
order, NOT an append to a buffer flushed once at the end — deliberate:
`render` reuses `run`'s exact stdout-capture machinery
(`compileAndRun`, extracted from what used to be `runWith`'s tail), so
the rendered document IS what gets captured. If a code block ALSO
calls `println` for its own reasons (debugging, or a genuine part of
the output), a buffer-then-flush design would print the whole rendered
document first and any such interleaved output afterward, out of
order; direct `print` per segment keeps everything in true document
order, whatever mix of substitution and side-effecting code produced
it. `run` is unaffected — this only changes the source `render`
synthesizes, not `run`'s own block-concatenation.

— the SOURCE-SYNTHESIS step is the only thing that differs between
`run` and `render`: `run` concatenates ```scala blocks only; `render`
walks the whole document. A `Text` segment is embedded as a raw
`"""..."""` string (unescaped — Scala's plain triple-quoted strings do
not interpret `$`, so a stray `$` in prose that is NOT part of a `${`
marker needs no escaping at all) UNLESS it contains a `"""` run or ends
in a `"` (either would make the closing `"""` ambiguous), in which
case it falls back to a normal escaped string literal (`\`, `"`, and
newlines escaped) — always correct, just less readable in the
synthesized source, which nothing ever reads.

## Hot-reload — `Page` (okay-script-page, 2026-09-03)

The FIRST of two things separating a real JSP page from `render` as
built so far ("The real goal" above already named the second,
per-request/hot-reload, and deliberately left it out): JSP compiles a
page's servlet class ONCE and calls its `_jspService` method once PER
REQUEST — it does not recompile on every hit, only when the `.jsp`
file on disk actually changes. `render`, as landed, does the opposite:
every call is a full `dotc` compile from source text. Fine for a
one-shot document, wrong for a page a server would call on every
request.

**Scope, narrower than the BACKLOG entry that named this**: hot-reload
(compile once, cache by file `mtime`, re-INVOKE not re-compile on
every call) is built here. Request-object injection (JSP's implicit
`request`/`response`) is NOT — see "Not built" below.

### `Page`

```scala
final class Page(path: Path, classpath: Classpath = Classpath.ambient):
  /** compiles on the FIRST call, or whenever `path`'s mtime has
   * changed since the last compile; otherwise re-invokes the
   * already-compiled program. */
  def render(): Result
  /** releases the cached compiled program's classloader/temp dir. */
  def close(): Unit
```

`Page` wraps `ScalaScript.render`'s machinery but needs NOTHING
`render` itself doesn't already have — no new dependency, `okay-jetty`
included, matches the BACKLOG entry's own framing ("closer to an
okay-jetty route than to okay-script itself"): `Page` stays inside
`okay-script`, and an ACTUAL jetty route is glue code a caller writes,
wrapping `page.render().stdout` into a `Response` — the SAME thing the
storefront example already does by hand for `run`, just for a page
that changes.

### Compile/invoke split — `ScalaScript.compileRender`

`render`'s existing `compileAndRun` did compile, invoke, capture, AND
delete the temp output directory, all in one call — right for a
one-shot render, wrong for something meant to be invoked repeatedly
(deleting the classfiles a live classloader may still need to satisfy
a LATER lazy class load is exactly the kind of bug that would only
show up occasionally, not on the first render). Split into:

```scala
object ScalaScript:
  /** compiles once; Left carries a Result with compile/dependency
   * errors (never throws), Right an invokable, repeatable handle. */
  def compileRender(markdown: String, classpath: Classpath = Classpath.ambient): Either[Result, Compiled]

trait Compiled:
  /** runs the ALREADY-COMPILED program again -- no recompilation.
   * Each call is a fresh top-to-bottom run (a fresh `println`, a
   * fresh `Meta.current` sequence), the way a servlet's per-request
   * method is a fresh call over already-loaded bytecode. */
  def invoke(): Result
  /** releases the classloader and deletes the temp output directory
   * -- must be called when no more `invoke()`s are coming, or the
   * temp directory and loaded classes leak for the process's life. */
  def close(): Unit
```

`render(markdown, classpath)` itself is now `compileRender(...).fold(
identity, c => try c.invoke() finally c.close())` — compile, invoke
once, close immediately; unchanged observable behavior for every
existing `render` caller. `Page.render()` instead HOLDS the `Compiled`
handle across calls, closing the OLD one only when the file changed
and a fresh compile replaces it (or when `Page.close()` is called
explicitly).

### What this is NOT (filed to BACKLOG, not built here)

- **Concurrent-safety of `Page.render()` under real concurrent HTTP
  load** — `Page`'s cache check-then-compile-or-invoke is `synchronized`
  (one render at a time per `Page` instance), which is correct but
  serializes concurrent requests to the SAME page through one lock;
  fine for the hot-reload use case (a low-traffic admin page, a
  documentation site), not measured or intended for a high-throughput
  server. Filed as a possible follow-on if a real consumer needs it.

## Request context — `Web` (okay-script-web, 2026-09-03)

The remaining half of "a new JSP": a script reading the CURRENT HTTP
request (query params, headers, method) the way it already reads
`Meta.current` for file metadata. `Hot-reload`'s own "not built" list
named the obstacle: the proven pattern (a plain always-fresh method +
mutable var, NOT a `given` — see "Metadata as context" for the
empirical reason) transfers directly, but exposing `okay.http.Request`
from `okay.script`'s own code would have been `okay-script`'s first
real dependency beyond `scala3-compiler`.

**Resolved by NOT exposing `okay.http.Request` at all.** `Web` is a
plain, dependency-free value:

```scala
package okay.script

final case class Web(method: String, path: String, query: Map[String, String] = Map.empty, headers: Map[String, String] = Map.empty)

object Web:
  val empty: Web
  def current: Web
  def setCurrent(w: Web): Unit
```

A caller (an `okay-jetty` route) translates its OWN real `Request`
into `Web` — `Web(r.method.toString, path(r), queryOf(r), r.headers.
toMap)` — before calling `render`/`Page.render`. `okay-script` never
imports `okay.http` at all; the translation is the caller's own glue
code, matching how the storefront example's `/order` route is already
hand-written glue, not something `okay-script` provides.

**Unlike `Meta.current`, `Web` is NOT auto-injected by the tokenizer**
— there is exactly ONE request per render call (no per-heading
transitions the way file metadata has), so a script just imports
`okay.script.Web` and calls `Web.current` itself, wherever it needs
it, in a ```scala block or a `${expr}` marker.

**`Page.render(web: Web = Web.current)` sets it FIRST, inside the
SAME lock `Page` already takes** for its cache check — this is the
one place concurrency actually matters: two threads calling
`page.render(webA)` and `page.render(webB)` concurrently on the SAME
`Page` must never let one thread's script read the OTHER thread's
`Web`. Because `Page.render` is `synchronized` end to end (was already
true for the cache/compile logic; `setCurrent` now happens inside that
same block, before `invoke()`), this holds. `ScalaScript.render` gets
the same optional `web` parameter for API symmetry, but is NOT
synchronized (a one-shot call was never meant to serialize) — a caller
mixing concurrent one-shot `render` calls WITH real per-request `Web`
data takes on the same single-threaded caveat `Meta.current` already
carries; `Page` is the safe path for that use case, by design.

## Line-accurate errors (okay-script-line-mapping, 2026-09-03)

Since the beginning, a compile error's line number reported through
`Result.errors` was dotc's own — a line in the SYNTHESIZED wrapped
source (`object OkayScriptMain: def run(args): Unit = ...`), never
translated back to the `.md` file a real author is actually looking
at. `Block.startLine` was captured from the start for exactly this,
and sat unused until now.

**The mechanism**: as `run`/`render` build the synthesized body, they
now build a PARALLEL `Vector[Int]` — one entry per synthetic source
line, giving that line's ORIGINAL markdown line number (1-based, same
convention as `Block.startLine`), or `-1` for a line with NO original
counterpart (wrapper boilerplate — `object OkayScriptMain:`, `def
run(args...):` — and any injected `okay.script.Meta.setCurrent`/
`okay.script.Web.decodeArgs` statement). `Segment.Code` and
`Segment.Interp` both gained a `startLine: Int` (set by `tokenize`,
mirroring how `blocks` already computes `Block.startLine`); `Segment.
Text` did not, since a raw string literal cannot itself carry a
compile error worth mapping precisely.

**Per-line precision within a segment**: a multi-line `Code` block's
`k`-th physical line maps to `startLine + k` (an error on the block's
5th line correctly reports the ORIGINAL 5th line, not just the
block's first). An `Interp` segment collapses to ONE synthesized
source line (`print((<expr>).toString)`) UNLESS the expr itself
contains a literal embedded newline (a genuinely multi-line `${...}`
marker, not exercised by any test) — in that one case every physical
line of the generated call maps to the marker's single `startLine`,
an accepted imprecision for an edge case that does not currently
occur in practice.

**Diagnostic collection** (`collectingReporter`) now reads
`dia.position()` (dotc's `Optional[interfaces.SourcePosition]` — the
line-only accessor that needs no `Context` argument, unlike
`SourcePosition.line(using Context)`) — confirmed EMPIRICALLY to be
0-based via a throwaway probe (an error on a file's physical 3rd line
reported `line() == 2`) before writing anything, not assumed from the
API's own naming. When present, `line() + 1` is looked up in the
mapping vector; a hit prefixes the message with `"L<n>: "` (the
ORIGINAL `.md` line); a miss (out of range, or mapped to `-1` —
meaning the error is in SYNTHESIZED code, e.g. malformed injected
`Meta`/`Web` plumbing, which would be an `okay-script` bug, not a
markdown author's) falls back to the RAW message, unprefixed, exactly
as before. Some diagnostics (dotc's own summary line, `"1 error
found"`) carry NO position at all — also confirmed by the same probe
— and are reported unprefixed too.

Each `run` call already gets its OWN `URLClassLoader` — two scripts
running in the same JVM do not collide with each other, since each
loader is a fresh instance with its own namespace. The gap BACKLOG
named was narrower and easy to miss: that loader's PARENT was
`getClass.getClassLoader` — `okay-script`'s own defining classloader —
and `URLClassLoader` is parent-FIRST by default. So a script could
silently resolve a class that was never in its own `Classpath` at all,
as long as it happened to be reachable from `okay-script`'s OWN build
classpath (in the test JVM: `munit`, `scala3-compiler`, and in Test
scope `okay-jetty` and everything it drags in). That defeats the
entire point of `Classpath`/`Deps` (okay-script-runtime): a caller
handing a script an EXPLICIT, minimal classpath was not actually
getting isolation — the host's own classes leaked through the parent
regardless of what the caller listed.

Fixed by giving each script's `URLClassLoader` a PLATFORM-only parent
(`ClassLoader.getPlatformClassLoader()` — JDK core modules, no
application classpath at all) instead of `getClass.getClassLoader`.
A script now sees exactly: its own compiled classes (the temp `outDir`
that `run` produces), whatever `Classpath` the caller passed in (or
`Classpath.ambient` by default), and the JDK. Nothing from
`okay-script`'s own build leaks in unless the caller explicitly put it
in the `Classpath`.

`Classpath.ambient` callers see NO behavior change (the ambient
classpath already lists essentially everything the JVM was launched
with, so the platform-only parent's absence of it is filled straight
back in by the child URLs) — this only changes what happens for a
caller using an EXPLICIT, narrower `Classpath`, which is exactly the
runtime-app scenario (okay-script-runtime, storefront generation) the
isolation gap mattered for.

Proved, not just asserted: `TestScalaScriptClassloaderIsolation`
constructs a deliberately minimal `Classpath` (only the scala runtime
jars, filtered out of `Classpath.ambient` by filename) and confirms a
script given it can no longer `Class.forName("munit.Assertions")` —
`munit` IS on `okay-script`'s own test classpath, so before this fix
that lookup silently succeeded (the leak); after, it throws
`ClassNotFoundException` inside the script, surfaced through
`Result.thrown`, exactly as any other runtime failure would be.

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
  errors: Vector[String],   // compiler diagnostics, empty if it compiled -- "L<n>: <message>" where n is the ORIGINAL .md line, when known (see "Line-accurate errors")
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

/** An already-compiled program, invokable repeatedly without
 * recompiling -- see "Hot-reload" above.
 */
trait Compiled:
  def invoke(): Result
  def close(): Unit

object ScalaScript:
  def blocks(markdown: String): Vector[Block]
  /** classpath defaults to Classpath.ambient; a script's own `using
   * dep` coordinates are resolved and appended before compiling,
   * regardless of which classpath was passed in. If the document has
   * front-matter/yaml/headings, `okay.script.Meta.current` reflects
   * the right position for every ```scala block (and, for `render`,
   * every `${expr}` too) as execution reaches it -- see "Metadata as
   * context" above. */
  def run(markdown: String, classpath: Classpath = Classpath.ambient): Result
  /** the whole document, prose and code, as ONE program: ${expr}
   * markers in prose are evaluated and substituted; the rendered
   * document is Result.stdout on success. `web` seeds `Web.current`
   * for a script that reads it -- see "Request context" above; NOT
   * synchronized, see that section's own concurrency note. See
   * "Interpolation" above. */
  def render(markdown: String, classpath: Classpath = Classpath.ambient, web: Web = Web.current): Result
  /** `render`'s compile step, split from invocation -- see
   * "Hot-reload" above; the primitive `Page` is built on. */
  def compileRender(markdown: String, classpath: Classpath = Classpath.ambient): Either[Result, Compiled]

/** A `render`-mode `.md` file, compiled once and cached by mtime,
 * re-invoked (not re-compiled) while unchanged -- see "Hot-reload"
 * above.
 */
final class Page(path: Path, classpath: Classpath = Classpath.ambient):
  /** `web` is set INSIDE this call's own lock, before invoking --
   * see "Request context" above for why that ordering matters. */
  def render(web: Web = Web.current): Result
  def close(): Unit

/** The incoming HTTP request a script is answering, as plain,
 * dependency-free data -- no `okay.http` import here at all. See
 * "Request context" above.
 */
final case class Web(method: String, path: String, query: Map[String, String] = Map.empty, headers: Map[String, String] = Map.empty)
object Web:
  val empty: Web
  def current: Web
  def setCurrent(w: Web): Unit

/** Front-matter + heading-scoped ```yaml metadata, as a typed AST and
 * as a current-position Context -- see "Metadata as context" above.
 */
object Meta:
  enum Value:
    case Str(s: String)
    case Arr(items: Vector[Value])
    case Obj(fields: Vector[(String, Value)])

  final case class Section(level: Int, title: String, yaml: Vector[Value], children: Vector[Section])
  final case class Doc(frontMatter: Map[String, String], root: Section)

  final case class Context(doc: Doc, path: Vector[Section]):
    def get(key: String): Option[String]
    def apply(key: String): String
    def section: Option[Section]

  def parse(markdown: String): Doc

  /** the metadata for wherever synthesized code has most recently
   * told `setCurrent` it is -- a plain, always-fresh method (NOT a
   * `given` -- see "How code reaches it" above for why one does not
   * work here). */
  def current: Context
  def setCurrent(c: Context): Unit
```

- `blocks` extracts every ` ```scala ` … ` ``` ` fenced region (a line
  matching ` ```scala ` exactly opens one, the next ` ``` ` line
  closes it — fences for any OTHER language tag, e.g. ` ```yaml `, are
  skipped whole). `startLine` is the 1-based line of the first line of
  code inside the fence, in the ORIGINAL markdown. `blocks` itself is
  UNCHANGED and does not do any line-mapping (still a plain extractor,
  used by `Deps.declared`) — `tokenize` is the function that actually
  carries `startLine` through to compile-error mapping, one layer up;
  see "Line-accurate errors" below.
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
object OkayScriptMain:
  def run(args: Array[String]): Unit =
    <concatenated block bodies>
```

**NOT `@main`** (was, until okay-script-web): `@main`'s generated
`main(Array[String])` forwarder does not hand `args` through to the
underlying body when the `@main` method itself declares zero
parameters, which it always did here — no way to get `Web`'s encoded
request data (see "Request context") into the running program through
it. A plain top-level `object` needs no compiler-macro cooperation and
gives full control of the method signature; Scala still emits a
static forwarder class (`OkayScriptMain.run(String[])`, confirmed by
decompiling it) exactly the way `@main` did, so the load-and-invoke
step below is otherwise unchanged.

Everything inside `run`'s body is exactly what the markdown author
wrote (plus, when the document has metadata or might reference `Web`,
one or two synthesized statements at the top — see "Metadata as
context" / "Request context"), unedited. Wrapping as a METHOD BODY
(not a top-level script) means:
- `import`, `val`, `def`, `class`, `given` are all legal, at any point
  in the concatenation, because a method body can contain local
  definitions — this is what makes "each block sees the previous
  block's definitions" true without extra plumbing.
- there is exactly one compiled artifact per run: a single
  `OkayScriptMain` class with a generated `run(Array[String])` static
  forwarder, found and invoked via reflection after compilation.

Every body-line producer (`run`, `render`, `withMeta`) builds its
lines at the FINAL indentation `run`'s body needs directly, rather
than assembling text at one depth and re-indenting it afterward by
prefixing every physical line — a real bug (okay-script-web) found
exactly that way: a `Text` segment's raw triple-quoted string literal
can span several physical lines, and a blind re-indentation pass
cannot tell "a line of Scala source" from "a line INSIDE a string
literal's data", so it corrupted the SECOND kind too.

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
per diagnostic, instead of thrown — prefixed `"L<n>: "` with the
ORIGINAL `.md` line when the diagnostic's position maps to one; see
"Line-accurate errors" below.

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
      `GET /order/<x>` returns a confirmation page naming that
      service; `Thread.interrupt()` stops the server.
- [x] a script given an explicit, minimal `Classpath` cannot reach a
      class that is on `okay-script`'s own host classpath but NOT in
      that `Classpath` (`munit.Assertions`, present via `okay-script`'s
      own test dependency, absent from a `Classpath` built from just
      the scala runtime jars): `Class.forName` inside the script throws
      `ClassNotFoundException`, surfaced through `Result.thrown` —
      proving the platform-only parent actually isolates, not just
      that it compiles.
- [x] `render` on a document with a preceding ```scala block and a
      `${expr}` in prose referencing that block's `val`: the rendered
      output has the prose with the expression's value substituted in
      place, byte-identical elsewhere.
- [x] `render` on a document with NO `${...}` at all: the output is
      the document verbatim (prose passes through unchanged, including
      any non-scala, non-yaml fenced block — a ```yaml fence is now
      METADATA, consumed rather than shown; see okay-script-meta below).
- [x] `$${` in prose renders as a literal `${` — proven on text that
      ALSO contains a real `${expr}` elsewhere, so both the escape and
      the live marker are exercised in the same document.
- [x] a `${expr}` whose expr itself contains braces (an `if/else`
      block) and one containing a NESTED real Scala string
      interpolation (`s"${x}"`) both render correctly — proving the
      scanner is brace-depth- and quote-aware, not a naive
      first-`}`-wins regex.
- [x] literal text containing a `"` at the end of a chunk, and text
      containing a `"""` run, both render correctly via the escaped
      fallback — proving the raw-triple-quote optimization's own
      safety check is actually exercised, not just the common case.
- [x] a `${expr}` referencing an undefined name is a compile error via
      `Result.errors`, exactly like a bad `run` block — `render` never
      throws a compiler exception out of itself either.
- [x] `Meta.parse` on a document with front-matter and one heading
      carrying a `- key: v` list ```yaml block (the `site.md` shape):
      the resulting `Doc` has the front-matter keys AND one child
      `Section` whose `yaml` is `Vector(Value.Arr(...))` of the right
      length.
- [x] code under a DEEPER heading sees that heading's OWN yaml, its
      PARENT heading's yaml, and the front-matter, all through ONE
      `Meta.current` — nearest-wins on a key present at more than one
      level.
- [x] a code block under a PARENT heading, appearing BEFORE a deeper
      child's own yaml is declared, does not see it — document order,
      the same rule `val`/`def` visibility already follows.
- [x] code OUTSIDE any heading (before the first one, or in a document
      with none) still sees the front-matter via `Meta.current.get`/
      `apply` — `path` is just `Vector(root)`, not an error, and
      `Context.section` is `None` there.
- [x] a ```yaml fence's content does NOT appear in `render`'s output
      (consumed as metadata), while a ```json fence (or any OTHER
      non-scala language tag) still passes through verbatim — proving
      the yaml special-case does not leak into the general rule.
- [x] `${...}` in prose can read `Meta.current` directly (not just a
      ```scala block) — it compiles into the SAME flat method body.
- [x] `run` (not just `render`) gives its ```scala blocks the same
      `Meta.current`, scoped by their own heading position — proving
      this is a property of the FILE, not of which mode compiles it.
- [x] `Meta.current.doc` reaches the FULL typed tree independent of
      position — the `site.md` `services` shape, read back through
      `doc.root.children.head.yaml`.
- [x] a metadata-free document (no front-matter, no yaml, no headings)
      never references `okay.script.Meta` in its synthesized source —
      an explicit `Classpath` of scala-runtime-jars-only still compiles
      and runs it (self-sufficiency preserved for the common case).
- [x] a script that wants `given`/context-function ergonomics can
      still have them by writing `given Meta.Context = Meta.current`
      itself, locally, immediately before use.
- [x] `Page.render()` twice with no file change in between compiles
      ONCE, not twice (checked by a coarse timing gap between a first
      render that must compile and a second that must not, since
      `dotc` compilation is orders of magnitude slower than reflection
      invocation of already-loaded bytecode).
- [x] editing the file and bumping its mtime, then calling
      `Page.render()` again, picks up the NEW content — proving the
      cache is genuinely invalidated by a real change, not just
      never-invalidated.
- [x] editing the file's CONTENT but leaving its mtime UNCHANGED (set
      explicitly, not relying on wall-clock granularity) still returns
      the OLD compiled output on the next `render()` — proving the
      cache keys strictly on mtime, not on a content hash or a "did
      the file change" heuristic — a deliberate, documented limitation.
- [x] `Page.render()` reflects the SAME `okay.script.Meta`
      metadata-as-context machinery `render` itself has (front-matter/
      heading-scoped yaml) — `Page` is a caching wrapper, not a
      parallel implementation.
- [x] a compile ERROR on the file's CURRENT content is reported
      through `Page.render()`'s `Result.errors`, exactly like a direct
      `render` call — `Page` does not swallow or reshape compiler
      diagnostics.
- [x] `Page.close()` releases the cached compiled program's temp
      output directory from disk — the same "leaves no temp file
      behind" property `render`'s own one-shot path already has, now
      checked for `Page`'s hold-it-open-across-calls shape too.
- [x] a script reading `okay.script.Web.current` sees the method/path/
      query/headers the CALLER set via `page.render(web)` — no
      `okay.http` type anywhere in `okay-script`'s own compiled source.
- [x] `page.render(webA)` then `page.render(webB)` on the SAME `Page`
      (sequential, proving ordering, not concurrency) reflects the
      RIGHT `Web` each time — not a stale value left over from the
      previous call.
- [x] `Web.empty`/omitting the `web` argument does not crash a script
      that never reads `Web.current` — the parameter is additive, not
      a new required contract.
- [x] a compile error on a MULTI-LINE ```scala block's LATER line (not
      its first) is reported with THAT line's own original number, not
      the block's `startLine` — proving per-physical-line mapping
      within one segment, not just per-segment.
- [x] a compile error on the SECOND of two ```scala blocks reports the
      SECOND block's own original line, not a line number relative to
      the first block or the synthesized file as a whole.
- [x] a compile error inside a `${expr}` marker (`render`) reports the
      marker's original line.
- [x] a document with front-matter/headings (so `okay.script.Meta`
      plumbing IS injected) still reports the CORRECT original line
      for a user error, proving injected lines are excluded from the
      mapping (mapped to `-1`), not silently shifting every later
      line's mapping by however many lines the plumbing added.
- [x] a diagnostic with NO position at all (dotc's own summary line,
      `"N error(s) found"`) is still reported, unprefixed, never
      dropped or crashing the lookup.

## Results

Landed 2026-09-03 (core), extended 2026-09-03 (runtime-app follow-on:
explicit `Classpath`, `//> using dep` + Coursier resolution; lifecycle:
`Thread.interrupt()` on the caller's own thread, no new API; worked
example: examples/it-consulting-storefront.md; classloader isolation:
platform-only parent per script; interpolation: `render`, `${expr}` in
prose, examples/render-storefront.md; metadata: `okay.script.Meta`,
front-matter + heading-scoped ```yaml as `Meta.current`; hot-reload:
`Page`, compile-once-invoke-many; request context: `Web`, dependency-
free, `String[]`-encoded across the classloader boundary; line-accurate
errors: a per-line origin map through `withMeta`). Traps found by the
tests, all fixed before landing:

- **The SAME bug shape as okay-script-web's `compileOnly` fix hit
  again, one function over — a re-indentation pass blindly touching a
  `Text` segment's embedded string DATA.** Moving indentation INTO
  `withMeta` (so no caller has to re-indent already-built text
  afterward) was meant to close this off for good — and the FIRST cut
  of doing so reopened it in a NEW spot: `emitCode` indented EVERY
  physical line of whatever string it was given, uniformly — correct
  for a genuine multi-line `Code` segment (each physical line really
  is a new source line) but wrong for a `Text`/`Interp` segment's
  synthesized `print("""...""")` call, whose continuation lines (when
  the text spans several lines) are STRING DATA, not source lines —
  the lexer does not care about their indentation, and adding some
  edits what gets printed. `TestScalaScriptRender`'s own
  no-interpolation test caught it again, immediately. Fixed with an
  explicit `isStatement: Boolean` per item: `true` (`Text`/`Interp`)
  indents ONLY the first physical line; `false` (`Code`) indents every
  line, preserving a multi-line block's own internal relative
  indentation. Two landings hitting the identical bug shape
  (`compileOnly`'s wrapper-depth fix, now `withMeta` itself) is itself
  the finding worth recording: ANY code that re-indents already-
  assembled text by scanning its physical lines is suspect near a raw
  triple-quoted string, regardless of which layer it lives in.

- **`Web` hit the SAME classloader-identity trap the Console fix
  found, one level up — for a USER-DEFINED type this time, not a JDK
  one.** A host-built `Web` instance handed directly to the isolated
  script (as a `Compiled.invoke()` argument) fails reflection's
  argument-type check: the isolated classloader compiles its OWN
  separate `Web` class, and a host instance is not an instance of it.
  Fixed by encoding `Web` into a flat `Array[String]` on the host side
  (`Web.encodeArgs`) and decoding it back INSIDE the isolated
  classloader (`Web.decodeArgs`, called from the synthesized source
  itself) — only `String`/`Array[String]` ever cross the boundary.
  This also meant abandoning `@main def okayScriptMain(): Unit` for
  the wrapper entirely: `@main`'s generated `main(Array[String])`
  forwarder does not hand `args` through to the underlying body at
  all when the `@main` method itself declares zero parameters (which
  it always did, since nothing needed `args` before `Web`). Switched
  to a plain `object OkayScriptMain: def run(args: Array[String]):
  Unit`, which needed no compiler-macro cooperation and gave full
  control of the signature — confirmed by decompiling both shapes
  (`javap`) before writing the change, not assumed.
- **The wrapper change broke output for EVERY existing example,
  discovered immediately by `TestScalaScriptRender`'s own
  no-interpolation test** — not a `Web`-specific bug, a synthesis bug
  the nesting-depth change exposed. The naive fix (prefix every
  physical line of the already-built `body` with 2 more spaces, since
  the body now sits one level deeper under `object`/`def`) corrupted
  DATA: a `Text` segment's raw triple-quoted string literal can span
  several physical lines, and `body`'s own line-prefixing pass could
  not tell "a line of Scala source" from "a line INSIDE a string
  literal's data" — it added the same 2 spaces to both, so every
  rendered line of output gained a spurious leading indent. Fixed by
  having EVERY body-line producer (`run`, `render`, `withMeta`) build
  its lines at the FINAL required depth (4 spaces) directly, removing
  the second, unsafe re-indentation pass entirely — indentation is
  decided ONCE, at the point each line is actually constructed, never
  re-derived by scanning already-assembled text.
- **The unconditional `Web.decodeArgs` call in the wrapper repeated
  `hasMeta`'s own lesson from the previous landing** — caught by the
  SAME test that caught it the first time
  (`TestScalaScriptClassloaderIsolation`'s minimal-Classpath case):
  referencing `okay.script.Web` in every synthesized program, even one
  that never uses it, breaks self-sufficiency for the common
  Web-free case. Fixed the same way `hasMeta` was: a cheap check
  (`hasWeb`, a substring scan for `"Web"` in the body) gates the
  decode call, emitted only when the script might actually reference
  it.

- **`Page` surfaced a REAL, previously-invisible bug from
  okay-script-classloader-isolation: a second `invoke()` on the SAME
  compiled program silently printed NOTHING** — not a `Page` bug per
  se, a latent defect in `compileOnly`'s stdout capture that a
  one-shot `run`/`render` could never have exposed. Root cause: the
  isolated script classloader (okay-script-classloader-isolation)
  loads its OWN, separate copy of `scala.Console` — a different class
  than the host's — so host-side `scala.Console.withOut(ps)` (the fix
  from the ORIGINAL `println`-capture trap, "The model" above) never
  touches the copy the script's own `println` actually reads. It
  "worked" for a one-shot call only by coincidence: the isolated
  `Console`'s lazily-initialized default value binds to whatever
  `System.out` is AT ITS OWN FIRST TOUCH — which happened to be our
  redirected stream, on that one call — and then stays bound to THAT
  SAME stream forever after, so a genuinely SECOND `invoke()` writes
  into the FIRST call's already-drained buffer instead of the current
  one, which comes back empty. Traced from a failing `TestPage` test
  down to a minimal reproduction (a bare isolated `URLClassLoader`
  double-invoke, no `Page` involved) before writing the fix, to be
  certain of the mechanism rather than guessing at a patch. Fixed by
  driving the ISOLATED classloader's OWN `Console` object via
  reflection (`Console$.MODULE$.setOutDirect(ps)`, restored after) on
  EVERY `invoke()` call, instead of the host-side `withOut` — applies
  uniformly to `run`/`render`'s one-shot path too, though it was
  invisible there.
- **`Meta.current` was NOT the first design — a `given`-based one was,
  and it failed on BOTH counts an empirical probe checked, not just
  one.** See "How code reaches it" above for the full account: local
  `given` re-declaration at the same flat scope is a compile error
  (unlike `val`), and even past that, a plain `given` is evaluated
  ONCE (memoized), never re-evaluated per `summon` — verified with a
  two-line throwaway probe BEFORE rewriting anything, not discovered
  via a failing test after the fact. The real, working mechanism —
  `Meta.current`/`setCurrent`, a plain always-fresh method plus a
  mutable var — has none of `given`'s restrictions because it never
  asks Scala's implicit resolution to do per-position work it isn't
  built for.
- **The FIRST version of `Meta`'s wiring unconditionally referenced
  `okay.script.Meta` in every synthesized program**, breaking
  self-sufficiency for a metadata-free script (the common case, and
  every existing example/test) — caught immediately by
  `TestScalaScriptClassloaderIsolation`'s deliberately minimal
  `Classpath` test (`Not found: okay`, since that Classpath has no
  reason to carry `okay-script`'s own classes). Fixed by `hasMeta`:
  the `Meta` reference is emitted only when the document actually HAS
  front-matter, yaml, or a heading.
- **`render` needed no traps fixed — the design held on the first real
  run**, INCLUDING the case that most looked like it would break: a
  `${expr}` whose own expression contains a NESTED real Scala string
  interpolation (`s"- ${it.name} — ${priceOf(it)}"` inside an outer
  `${services.map(...).mkString(...)}` marker, in
  examples/render-storefront.md). The quote-aware brace scanner handled
  it correctly on the first try. The one real refinement made BEFORE
  any test ran (not a bug fix after the fact): switching from a
  buffer-then-flush-at-the-end design to a direct `print(...)` per
  segment, once it was clear a buffered design would silently reorder
  a code block's own `println` output after the whole rendered
  document instead of interleaving it correctly — caught while writing
  the spec, not by a failing test.
- **Classloader isolation's fix genuinely closes a leak — confirmed by
  temporarily reverting it.** Before landing, the fix was reverted
  (`ClassLoader.getPlatformClassLoader()` back to
  `getClass.getClassLoader`) and the isolation test rerun: it failed
  exactly as expected (`munit-reachable:true`, the leak), proving the
  test was not a false positive and the fix was not a no-op. Restored
  before landing.
- **The storefront example's `/order` route used a QUERY STRING
  (`/order?key=<x>`) — okay-jetty's `Request.url` never carried one at
  the time.** Fixed separately and since landed as `http-request-query`
  (`Jetty.scala`'s `requestOf` now reads `getHttpURI.getPathQuery`);
  the storefront example itself still uses the path-based `/order/
  <key>` route it landed with, since a working route needed no further
  change once the underlying bug was fixed elsewhere.
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
  call. Original fix: wrap the invocation in `scala.Console.withOut
  (ps)` *in addition to* `System.setOut`. **Superseded by
  okay-script-page** (see its own Results entry below): once scripts
  ran in an isolated classloader (okay-script-classloader-isolation),
  that `withOut` call was touching the HOST's `Console` class, not the
  isolated script's own separate copy — invisible for a one-shot call,
  wrong for `Page`'s repeated `invoke()`. The current mechanism drives
  the isolated classloader's OWN `Console` via reflection
  (`setOutDirect`) instead; `System.setOut` (JVM-global, no classloader
  identity issue) stays as the belt-and-braces catch for a reflective
  callee that writes directly to `System.out`.
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
