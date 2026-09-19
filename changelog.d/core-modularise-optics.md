## core-modularise-optics - a singleton type can be extended, so nothing had to be renamed

Stage 4 of specs/core-modules.md, and the one the spec had filed as
BLOCKED. `Optic` (720 lines), `Fuse` (469) and `Focus` (55) became
`okay-optics`, with fourteen suites and the `ArrowLaws` helper.

ONE OF THE TWO SEAMS CLOSED WITHOUT ANYBODY WORKING ON IT. The spec
named two edges from the core into optics: `State.zoom` typed on
`Lens`, and `Proc.procArrow` typed on `Optic.Arrow`. Stage 2 moved
`Proc` out with the workflow, so the second was simply gone by the
time this lane started - one lane unblocking another without either
knowing.

THE OTHER WAS NEVER ABOUT OPTICS, and reading it was the whole lane.
`State.zoom(l: Lens[S, S, A, A])` used, of that entire optic, `l.get`
and `l.set`. So it is an interpretation of one effect into another
that happens to have been SPELLED with a lens. The core now carries it
in its own terms:

    def zoomWith[S, A, X, F[+_]](look: S => A, put: A => S => S)(p: ...)

and okay-optics gives the lens spelling back:

    extension (st: State.type)
      def zoom[S, A, X, F[+_]](l: Lens[S, S, A, A])(p: ...) =
        State.zoomWith[S, A, X, F](s => l.get(s), a => s => l.set(a)(s))(p)

NO API BREAK, AND THE SPEC HAD PREDICTED ONE. Stage 1 hit a wall with
`Producer.concat`: an object cannot be reopened across compilation
units, so a member that moves must be renamed. This spec therefore
wrote that `State.zoom` "cannot stay `State.zoom` in another artifact"
and priced a rename plus three test files. That was wrong. An object
cannot be reopened, but its SINGLETON TYPE can be extended, and
`extension (st: State.type)` puts the name back exactly where it was.
`State.zoom(lens)(prog)` compiles character for character, the
documentation needed no edit, and `TestZoom` moved without a change.

`PState.zoom` was already one line - `l[Zooming[X, R]](m)`, the optic
run at the carrier - so only the `Optic.Strong` instance for that
carrier moved. `PState.Zooming` names a `Cont` and nothing else, and
stayed.

TWO `dependsOn` EDGES in the whole 73-module family: okay-workflow and
okay-lex, both `compile->compile;test->test`, because `ArrowLaws` is
typed on `Optic.Arrow` and their suites run it against their own
arrows. okay-codec (41 mentions), okay-ui, okay-sql, okay-http and
okay-persist all use optics and needed nothing.

The core is now 46 files and 11 553 lines, from 74 and 21 914 before
stage 1: 47% gone across four stages, with not one consumer's import
or call site edited.

WHAT IT LEAVES FOR STM, the last filed seam: look at what the core
actually USES of the thing it is typed against, before assuming the
type is the dependency. `Providing.Facts` is backed by `TMap` - the
question is which two or three operations of `TMap` it needs.

Spec: specs/core-modules.md.
