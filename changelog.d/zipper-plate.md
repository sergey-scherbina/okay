## zipper-plate - a cursor over a tree, and the JSON editor that needed one

The operator fired the trigger the backlog entry was waiting on
("нужен продукт с редактированием дерева", 2026-09-22), the same day
the entry's first-named trigger — the terminal's Tab — had been
checked and found to be a flat index, not a tree move.

- okay-optics `Zipper.scala`: `Plate[T]` (`children`/`withChildren`,
  arity-preserving; `Plate.of(traversal)` for anyone holding a
  self-traversal) and `Zipper[T]` — frames hold the parent node, its
  children vector and the index, plus a `dirty` flag so `up` rebuilds
  only under an edit and a walk without edits hands back the input
  tree `eq`. `down/first/up/left/right/at/modify/set/root/path/
  index/isTop`; `Zipper.focus: Lens` on the cursor (so `State.zoom`
  runs a program at the focus) and `Zipper.at(path): Affine` on the
  tree. Sideways moves need no plate — the frame knows the siblings.
  `TestZipper` (10): the laws over a rose tree, cursors compared by
  focus/path/root, `Plate.of` against the hand plate.
- okay-codec: `given Plate[Json]` in `JsonOptic` (object keys stay in
  the node and re-pair positionally; another arity keeps the node),
  `JsonOptic.removeChild`/`insertChild` on the PARENT — a plate
  cannot invent a key. `TestJsonZipper` (3): the contract, the
  helpers, and a cursor's edit equal to the `index`/`field` path
  optic on every path of sixty generated documents.
- okay-ui: `given Plate[Ui]` — the STRUCTURAL walk. Found on the way:
  `Ui` has two conventions for "the i-th child", `childAt` (the patch
  path: a `Modal`'s body at 1, a `Table` with no children) and
  `kidsOf` (every child); an editor needs the second, and
  `TestUiZipper` (3) pins `Zipper.at == Ui.path` on the nodes where
  they agree and states exactly where they part.
- okay-ui `JsonEditor(json)(done)`: a `Screen` over `Zipper[Json]`,
  buttons only — checked first: no host produces `Event.Key`, so the
  letter keys the spec's first draft listed would have been dead —
  and inline editing rather than a pushed prompt, because `Nav.To`
  replaces only the prompt's frame and would leave the old editor
  underneath. `TestJsonEditor` (6) drives it through `Nav.update`.
- docs: theory ch. 10 gains "The zipper: the residual, carried"
  (optic-vs-cursor pair, the two bridges, the editor, what the Mirror
  derivative would add); guide §10, both module pages, the chapter
  index. Huet 1997 and McBride 2001 were already in the chapter's
  references.
- Spec: specs/zipper.md, every box checked; Decisions record the two
  drafts that were rejected against the code (keys, prompt).

Gate `affected master` green, no warnings.
