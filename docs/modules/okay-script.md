# okay-script

Markdown files as Scala source (specs/okay-script.md). A `.md` file
with fenced ` ```scala ` blocks is a literate program: the blocks are
extracted, concatenated in document order, and compiled by the REAL
Scala 3 compiler in-process (`dotty.tools.dotc`), then run. No new
language and no interpreter — markup metadata extraction, minimal
preprocessing, meta-compilation.

| | |
|---|---|
| `ScalaScript.run(markdown)` | the whole surface: compile the file's blocks together and run them; a `Result` carries diagnostics as DATA |
| `Segment` | `Text` / `Code(s, startLine)` / `Interp(expr, startLine)` — the tokenized document; the start lines are what make a compile error point at the ORIGINAL `.md` line, not the synthesized source |
| `Meta` | front-matter and document metadata (`Meta.parse`, `Meta.current`), auto-injected around a run |
| `Web` / `Page` | render mode: the incoming request as plain data (`Web.current`) and `Page.render`, so an okay-jetty route can answer with a script's output. okay-script itself imports no HTTP type — the caller translates its own `Request` |
| `Classpath` / `Deps` | the ambient classpath a script compiles against, plus ` ```deps ` coordinate resolution |

One file is one compilation unit: a later block sees what an earlier
block defined, the way one Scala source or a REPL session would. A
run is `ok` iff the source compiles with zero errors AND the program
runs without throwing — a smoke test, not an output checker
(mdoc-style output comparison is filed to BACKLOG, not built).

Library API only: no CLI, no sbt-test integration, no automatic walk
of `specs/*.md` in the default gate. Its tests FORK, because the
compiler reads the test JVM's own classpath (okay-script-scalac-
classpath). okay-jetty is a TEST-only dependency, for the proof that
a compiled script can start and stop a real server.
