## zipper-mirror-derivative - the typed zipper: a cursor whose position is a type

Opened by the operator ("бери делай") an hour after stage 1 landed;
the backlog entry had recorded it "for the future" with the shape it
expected — a heterogeneous tuple of frames and a Mirror-walked
derivative. Neither was needed, and specs/zipper.md stage 2 says why.

- okay-optics `TypedZipper.scala`: `TypedZipper[S, A, Self]` with
  `Top[S]` and `Below[S, P, A, Z <: TypedZipper[S, P, Z]]`. A frame
  IS an optic — `down(lens)` total, `downPartial(affine)`,
  `downCase[B]` (a prism), `at(i)` on a `Vector` focus — and
  `field("name")` is the existing Mirror lens `Lens.field[A]` as a
  frame, so a wrong name is a compile error there and here. The
  parent's type is an F-bounded PARAMETER of the frame, carried and
  given back by `up`, never summoned (the row-membership rule);
  `down(customer).down(address).up.up` is a `Top[Order]` the
  compiler checks. `dirty` on the frame, as in stage 1: a walk
  without edits hands back the input `eq`. `TypedZipper.focus: Lens`
  on the cursor, so `State.zoom` runs a `State % Customer` program
  while the cursor is parked in an `Order` — the consumer the entry
  named.
- `TestTypedZipper` (7): down/up and the typed `up.up`, the nested
  copy with sharing, `field` by name with `compileErrors` on a
  misspelling, `at(i)`, `downCase`, the lens laws plus the zoomed
  program, and a walk sharing its prefix across two branches. The
  first run was red on `root eq input` — the flag is the fix, not the
  test.
- docs: theory ch. 10's zipper section replaces "what is not here"
  with the typed cursor and its example; guide §10; the module page.
  Decisions record what was NOT built and why: a type-changing `set`
  (retypes every frame — `PState.zoom` over the composed lens),
  a lens back from the cursor (a prism frame has no total get),
  `left`/`right` (fields are not a sequence).

Gate `affected master` green, no warnings.
