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

object ScalaScript:
  def blocks(markdown: String): Vector[Block]
  def run(markdown: String): Result
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
`-d <that dir> -classpath <current classpath>` via
`dotty.tools.dotc.Driver`, and — success or failure — the temp
directory is deleted before `run` returns (a script run leaves no
litter, matching `okay`'s general no-temp-file-residue expectation).

Compiler errors are collected from a custom `Reporter` (not dotc's
default, which prints to stderr) and returned as `errors`, one string
per diagnostic, instead of thrown.

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

## Results

Landed 2026-09-03. Two implementation traps found by the tests, both
fixed before landing (not left for a future session):

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

`blocks`' `startLine` is the line of the first CODE line inside the
fence (one past the ` ```scala ` line itself), confirmed against a
hand-counted markdown fixture in the test.
