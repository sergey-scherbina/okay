# okay-optics

Profunctor optics — `Iso`, `Lens`, `Prism`, `Affine`, `Traversal`,
`Kaleidoscope`, `AlgebraicLens` — with their constraint classes, their
interpretations, the `Fuse` planner that turns a named optic path into
hand-written code, `Focus`, and `Zipper` — Huet's cursor over a
`Plate[T]` (how a tree exposes its children), with `Zipper.focus` as a
lens on the cursor and `Zipper.at(path)` as the path back as an affine
— and `TypedZipper`, the same cursor with its position as a type:
frames are lenses, affines and prisms, `up` answers the parent's type
(specs/zipper.md, stages 1 and 2).

The guide is [optics.md](../optics.md), the arrow half is
[arrows.md](../arrows.md), and the theory chapter is
[theory/10-optics.md](../theory/10-optics.md). None of that changed
when the module did.

## Nothing changed in your imports, or in your call sites

The package is still `okay`, and that includes the one thing a module
split usually breaks. `State.zoom(lens)(prog)` and
`PState.zoom(lens)(m)` are spelled exactly as before.

They survive because of how the seam was cut. `State.zoom` used a lens
for precisely two things, reading the part out of the whole and
putting it back, so the core now carries the interpretation under its
own name and with no optic in it:

```scala
State.zoomWith(look: S => A, put: A => S => S)(p)
```

and this module gives the lens spelling back as an extension on
`State.type`. `PState.zoom` was already one line — the optic run at
the `Zooming` carrier — so only the `Optic.Strong` instance for that
carrier moved; the `Zooming` alias is a `Cont` and stayed.

## Using it

```scala
lazy val myModule = (project in file("my-module"))
  .dependsOn(okay.jvm, okayOptics.jvm)
```

Two modules in this repository needed that line, okay-workflow and
okay-lex. The rest reach optics through one of them.

`ArrowLaws` — the arrow laws stated once for every carrier — lives in
this module's test sources, because it is typed on `Optic.Arrow`. A
module that wants to run them against its own arrow declares
`okayOptics % "test->test"`, as okay-workflow and okay-lex do.

## Why it is a module at all

The spec had this filed as BLOCKED on two seams: `State.zoom` typed on
`Lens`, and `Proc.procArrow` typed on `Optic.Arrow`. Stage 2 moved
`Proc` out with the workflow, which removed one of them without
anybody working on it, and reading the other showed it was never about
optics. See [specs/core-modules.md](../../specs/core-modules.md).
