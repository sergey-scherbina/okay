- [ ] zipper-plate — a `Zipper[T]` in okay-optics over a PLATE, not a
      Mirror (operator's ask 2026-09-22; the generic road is
      `zipper-mirror-derivative` beside this). `Plate[T]` is Uniplate's
      `children`: `T => (Vector[T], Vector[T] => T)`, which `Ui`
      already has by hand as `kidsOf`/`withKids` (Ui.scala:484-505)
      and a `Traversal[T, T, T, T]` gives for free through
      `Aggregating` (Optic.scala:248). A frame is `(siblings:
      Vector[T], i: Int, rebuild: Vector[T] => T)` — the parent's own
      children vector and a position, NOT Huet's `(left.reverse,
      right)` list pair: `left`/`right` are `i ± 1` on both, but `up`
      is one `updated(i, focus)` on the vector and a `reverse ++ ::`
      walk on the pair, and `up` follows every `modify`. The plate
      answers a `Vector` already, so the vector frame converts
      nothing; the list pair would rebuild it per descent. Moves
      `down(i)/up/left/right` answer `Option` (an affine, exactly
      `Ui.path`'s refusal at a leaf); `modify`, `root`. The bridge to
      the optics is two functions, not a new family: `Zipper.focus:
      Lens[Zipper[T], Zipper[T], T, T]` (so `State.zoom(Zipper.focus)
      (inner)` runs a program against the focus, Zoom.scala) and
      `Zipper.at(path): Affine[T, T, T, T]`, which is `Ui.path` today.
      THE DIVISION OF LABOUR, and why this is not a second `Ui.path`:
      an optic is a path recomputed per operation, no state, and
      optics-fuse reaches the hand-written update byte for byte; a
      zipper is a CURSOR with the path materialised, so it wins only
      where moves outnumber edits — `ui-path-two-walks-answered`
      measured the affine at 2.9-7.9x the hand walk because it
      interprets a `List[Int]` per step, and a cursor pays that once.
      In an effect row the zipper is a value under `State % Zipper[T]`
      with the failing moves routed through `Condition` (pattern-binds);
      a move that changes the focus TYPE (into a `Json` object → `Json`,
      into a field → `Option[Json]`) is a `PState` transition, the
      four-parameter lens of `PState.zoom`. Laws: `down(i).flatMap(_.up)
      == Some(z)` on every node with an i-th child; `at(p).preview ==
      Ui.path(p).preview` on the TestUiOptic trees; `root` after any
      sequence of moves without `modify` is the input. TRIGGER: a
      consumer that MOVES — the terminal host's focus navigation
      (Tab/Shift-Tab is `right`/`left` over siblings, specs/ui-product.md),
      a caret, a tree-walking tokenizer. `Ui.patch` by path is not one;
      it stays an optic. Not before the trigger: the record above says
      the two walks stay two, and a third walk with no mover is a third.
