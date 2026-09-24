## okay2-optics - okay-optics in okay2

The second lane of porting the types okay2 lacked. A new subproject
`okay2-optics`:
- the profunctor lattice, with `andThen` taking the meet;
- the families (`Iso`, `Lens`, `Prism`, `Affine`, `Traversal`,
  `Kaleidoscope`, `AlgebraicLens`) and their constructors;
- `Lens[S](_.f)` and `Lens.field` as Scala 2 macros;
- the interpretations (`Function1` with its arrow, `Forget`, `Star`,
  `Aggregating`, `Shop`, `Market`, the zooming `Cont`) and every
  operation;
- the arrow glyphs and `kleisliArrow`;
- `State.zoom` and `PState.zoom`/`zoomCase`.

In the core, `State.zoomWith` and `PState.Zooming` (specs/okay2.md stage
24). `Optic` is contravariant in its constraint, so a composed optic
still has its declared type in Scala 2. okay's `Fuse`, its compile-time
planner, is not ported. 39 tests.

Docs: docs/okay2.md section 28.
