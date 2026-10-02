## effects-shift-reset - level 1 through the typeclass, in any encoding

Level 1 of the API, step 4 (operator, 2026-10-02; specs/shift-effect.md).

- `Effects[M]` gains `shift`, `shift0`, `reset`, `handle(m, h)` and
  `run(m)`. Their default goes through the tree, so `Eager` has them too.
  `Effects[Free]`'s are the top-level functions themselves.
- `Effects.monad[M, F]` gives `direct` a monad over `M[F, *]` in generic
  code.
- One program written over `Effects[M]` answers `(5, 32)` in Free and in
  Eager, in the monadic and in the direct style: TestEffectsLevel1 (3)
  and TestEffectsLevel1Direct (1).
- `handle` takes program and handler in one list: the level-2
  `handle(m)(ret)(clause)` is an overload of the same name.
