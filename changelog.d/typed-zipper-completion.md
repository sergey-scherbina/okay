## typed-zipper-completion - the path back, and the cursor that changes the whole's type

Stage 2 of specs/zipper.md had listed three things not built; the
operator asked for them the same night ("делать то что еще не
построено"). Two are built, one is refused again with its reason.

- `TypedZipper#asAffine: Affine[S, S, A, A]` — every frame now keeps
  its `look` beside its `put`, and the chain composes through
  `Affine.andThen` into the walk as an optic on the TREE, the typed
  twin of stage 1's `Zipper.at`. Affine, not lens: an index or a
  case frame may be missing on another tree, and the test shows it
  answering `None` there and `set(v).root` everywhere else.
- `TypedZipper.Poly[A, B, T](focus, put: B => T)` — the type-changing
  cursor: McBride's derivative applied to its hole. `down` by a
  four-parameter lens, affine or prism; `set(b): T` is the new whole.
  No frames and no `up`, because after a type change there is no
  parent of the old type — the six-parameter F-bounded chain was
  written out and refused (every `up` would need `A =:= B` evidence
  and the `dirty` shortcut is ill-typed once the parent retyped).
- NOT `left`/`right`: a field is not a sibling of a field; `at(i)`
  reaches a `Vector` element and sideways is stage 1's job.
- `TestTypedZipper` 11 (7 + 4); theory ch. 10 and guide §10 gain the
  two examples (compiled as tests before being written down).

Gate `affected master` green, no warnings.
