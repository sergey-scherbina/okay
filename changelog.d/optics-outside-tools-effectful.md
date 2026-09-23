## optics-outside-tools-effectful - a tool that is a program, at all three seams

The operator lifted the wait ("Все это нужно", 2026-09-23). Stage 2 of
specs/optics-outside.md had kept tool handlers pure — `A => String`,
the seam `Handlers.tools`, `Stepper` and `Mcp.Server` took — and named
the widening a separate decision with those three callers to carry.
They carry it now, the pure seam untouched beside the new one.

- okay-agent `Toolbox.In[F]`: `on[A](…)(run: A => String ! F)`, `raw`,
  `++`, `specs`, `table: Map[String, ToolCall => String ! F]`; a pure
  box lifts with `box.in[F]` so both kinds meet in one `++`. A bad
  argument answers `Toolbox.failed`'s data as a program.
- `Handlers.relayToolsF` (the relay over programs) and
  `Stepper.transparentF` (every pause performs the effectful tool).
- okay-mcp: `Server.serveIn[G](serving)(runTool)` — THE protocol,
  written once in `Row[G] = Take % Rpc + (Writer % Rpc + G)`; `serve`
  is `serveIn[Pure]`, `run(link, serving)` is `serveIn[Async]` with
  `Serving.callF` answered by `answering`; `overIn` for the wide row.
  The transducer skeleton became a local `go`; the old private
  `answer`/`fail` went with it. Every earlier server test passes on
  the generic stage, which is the guard that it is the old one.
- A tool's failure is its own row's business: a program cannot be
  `try`-caught from outside and the server carries no Scheduler to
  `attempt` it; the pure road keeps its `isError` answer.
- Tests: `TestToolbox` +2, `TestStepper` +1, `TestServer` +1; docs:
  declaring-an-api (the stale "pure, because" paragraph), the
  okay-agent and okay-mcp module pages; spec stage 8.

Gate `affected master` green, no warnings.
