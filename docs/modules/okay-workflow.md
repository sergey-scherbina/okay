# okay-workflow

The static workflow: `Wf`'s questions, `Proc`'s free arrow over them,
and `ProcMacro`, which builds one from an ordinary-looking block.

It left the core on 2026-09-18 for the plainest of reasons: the core
never referred to it. No file in `okay` names `Wf`, `Proc` or
`ProcMacro` in code, on any platform source directory. The one edge
that exists runs the other way — `Delim` is typed on `Replayable`, a
78-line marker, and that stayed. See
[specs/core-modules.md](../../specs/core-modules.md).

## Nothing changed in your imports

The package is still `okay`. What changed is the build: a module that
uses the workflow declares it.

```scala
lazy val myModule = (project in file("my-module"))
  .dependsOn(okay.jvm, okayWorkflow.jvm)
```

Exactly one module in this repository needed that line, okay-persist.
Everything else either does not use the workflow or reaches it
through a module that does.

## What is here

| area | the types |
|---|---|
| the questions | `Wf`, `Wf.Runtime`, `Wf.Proc.Standing` |
| the arrow | `Proc`, its `Arrow` and `Choice` instances, `Proc.direct` |
| the macro | `ProcMacro` |

The arrow story, including how `Proc` composes with optics and where
its laws live, is in [arrows.md](../arrows.md) and
[specs/arrows-plan.md](../../specs/arrows-plan.md).

## Platforms

A crossProject over JVM, JS and Native, because these files sat in the
core's shared source directory and so already compiled everywhere.
The suites run on the JVM, which is the shape they had in the core.
