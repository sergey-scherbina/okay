## monadic-reflection-stacked - several monads in one block: `okay.Layered`

specs/layered-reflection.md stage 0. Filinski's layered monads (POPL
1999), built on multi-prompt control as Brachthäuser, Boruch-Gruszecki
& Odersky (2020) describe.

- `Layered.reify[M, R, F]` installs a delimiter and hands the body a
  `Reflect[M, R]` capability. `m.reflect` is a `shift0` to that
  delimiter, past any inner ones, and the order of the `reify` blocks is
  the order of the layers.
- `Layer[M]` rather than `Monad[M]`: the captured continuation is a
  program, so a layer's bind sequences programs, which makes it a
  transformer over the outer layers. Instances: Option, Either, List.
- TestLayered (7): Filinski's `reify (reflect m) = m`, one layer, two
  layers in both orders (`List(Some(11), None, Some(33))` against
  `None`), three layers, and a leaked capability failing with
  `NoPrompt`. Watched failing: a reversed `Layer.list` turned five red.
- docs/direct-style.md: a new "Layer 1½" section, and the claim that one
  block cannot mix two monads is now limited to `Monadic`.
