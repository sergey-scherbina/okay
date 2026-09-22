# zipper — a cursor over a tree, and the editor that needs one

## Overview

An optic is a PATH: `Ui.path(is)` recomputes its way from the root on
every operation, holds no state, and optics-fuse takes its run-time
cost to the hand-written update. A zipper (Huet 1997; docs/theory/
10-optics.md, "an optic's residual is a derivative") is a CURSOR: the
path is materialised as a stack of frames, so a step to a sibling or
to the parent is O(1) and an edit at the focus is one `updated`. It
earns its place only where moves outnumber edits — and until
2026-09-22 nothing in this repository moved through a tree, which is
what `backlog.d/optics-arrows-effects/zipper-plate.md` recorded
after its first-named trigger (terminal Tab) turned out to be a flat
index. The operator then named the consumer: "нужен продукт с
редактированием дерева". This spec is the zipper, the two plates it
runs over (`Json`, `Ui`), and that product: `JsonEditor`, a `Screen`
that navigates and edits a JSON document in place.

## Interface

okay-optics, `Zipper.scala`, package `okay`:

```scala
/** how a tree exposes its children, Uniplate's `children`/`descend`
 *  pair: ARITY-PRESERVING — `withChildren(t, cs)` with `cs.length !=
 *  children(t).length` is the plate's own business (Json keeps the
 *  node, see below) and the zipper never asks it */
trait Plate[T]:
  def children(t: T): Vector[T]
  def withChildren(t: T, cs: Vector[T]): T

object Plate:
  /** a plate from a self-traversal: children by `toVector`, put back
   *  by a counted rewrite — the bridge from the optics that are here */
  def of[T](tr: Traversal[T, T, T, T]): Plate[T]

/** the cursor: a focus and the frames above it (nearest first) */
final case class Zipper[T](focus: T, frames: List[Zipper.Frame[T]]):
  def down(i: Int)(using Plate[T]): Option[Zipper[T]]
  def first(using Plate[T]): Option[Zipper[T]]         // down(0)
  def up(using Plate[T]): Option[Zipper[T]]
  def left(using Plate[T]): Option[Zipper[T]]
  def right(using Plate[T]): Option[Zipper[T]]
  def at(path: List[Int])(using Plate[T]): Option[Zipper[T]]
  def modify(f: T => T): Zipper[T]
  def set(t: T): Zipper[T]
  def root(using Plate[T]): T
  def path: List[Int]                                    // root-first indices
  def index: Option[Int]                                 // position among siblings; None at the root
  def isTop: Boolean

object Zipper:
  /** the parent as it was, its children, and which one the focus is */
  final case class Frame[T](parent: T, siblings: Vector[T], i: Int)
  def apply[T](t: T): Zipper[T]
  /** the focus as a lens on the cursor — `State.zoom(Zipper.focus)(p)` */
  def focus[T]: Lens[Zipper[T], Zipper[T], T, T]
  /** the path as an affine on the TREE — what `Ui.path` is by hand */
  def at[T: Plate](path: List[Int]): Affine[T, T, T, T]
```

okay-codec, `JsonOptic.scala`: `given Plate[Json]` — children of a
`JArr` are its values, of a `JObj` its field values (keys stay in the
node), of a scalar none; `withChildren` on a `JObj` re-pairs keys with
the new values POSITIONALLY and keeps the node unchanged when the
arity differs (a key cannot be invented). Structural edits go through
the parent: `Json.removeChild(j, i)`, `Json.insertChild(j, i, key,
v)` (key used by objects, ignored by arrays).

okay-ui, `Ui.scala` companion: `given Plate[Ui]`, the structural walk
(`kidsOf`/`withKids` with `structural = true` — every child, on screen
or not, which is `Ui.path`'s convention).

okay-ui, `JsonEditor.scala`:

```scala
object JsonEditor:
  /** a screen editing `json` in place; `done(Some(j))` on Done,
   *  `done(None)` on Cancel — the Prompt shape of every dialog here */
  def apply(json: Json)(done: Option[Json] => Nav): Screen
```

Keys, terminal and buttons alike: `j`/`k` next/previous sibling,
`l` into (first child), `h` out (parent), `e` edit the focused
scalar (a `Toolkit.prompt`; the text is `Json.parse`d, a parse error
becomes a `JStr`), `d` delete the focus (root cannot be deleted),
`a` add after the focus (`JNull`; in an object the key is prompted),
`Enter`/"done", `Esc`-less "cancel" button.

## Behavior

Zipper (`TestZipper`, okay-optics, over a rose tree the test
defines and over `Plate.of(traversal)` of the same):

- [ ] `down(i)` on a node with an i-th child then `up` is the input
      cursor, `==` AND the root `eq` the input tree — no rebuild
      when nothing was modified.
- [ ] `down(i)` past the children, `up` at the root, `left` at index
      0 and `right` at the last child answer `None`; nothing throws.
- [ ] `left` then `right` and `right` then `left` are the identity
      where both moves exist.
- [ ] `modify(f)` at a focus, then `root`, equals the hand-written
      rebuild of that one node; the siblings and every other subtree
      are `eq` to the input's.
- [ ] `at(path)` reaches what repeated `down` reaches, and `path`
      reads back what `at` was given.
- [ ] `Zipper.focus` satisfies GetPut, PutGet, PutPut on cursors at
      every depth of the test tree.
- [ ] `State.zoom(Zipper.focus)(p)` runs a `State % T` program against
      the focus and leaves the frames untouched: the same result as
      `modify` with the program's function.
- [ ] `Plate.of(tr)` agrees with the hand plate on `children` and on
      `withChildren` for every node of the test tree.

Json (`TestJsonZipper`, okay-codec):

- [ ] `Plate[Json]`: `withChildren(t, children(t)) == t` for arrays,
      objects and scalars; an object's keys are kept in order when
      values change; a different arity leaves the object unchanged.
- [ ] `removeChild`/`insertChild` do what their names say on arrays
      and objects, keep other fields' order, and are the identity on
      scalars and out-of-range indices.
- [ ] a document round-trips: `Zipper(j).at(p).map(_.set(v).root)`
      equals `JsonOptic`'s path optic setting the same value, on
      every path of a generated document.

Ui (`TestUiZipper`, okay-ui):

- [ ] `Zipper.at[Ui](p).preview(t) == Ui.path(p).preview(t)` and the
      `set`s agree, for every path of the `TestUiOptic` trees that
      passes through nodes where the structural walk and the patch
      convention name the same children (Row, Column, Box, Scroll,
      Form, Items), and past the children both refuse.
- [ ] where they part is pinned, not hidden: a `Modal`'s body is child
      1 on the patch path and child 0 for the zipper; a `Table` has
      rows for the zipper and no children for the path; a `Tabs` page
      off screen is reachable by the zipper — the editor's plate is
      the structural walk on purpose (Decisions).

JsonEditor (`TestJsonEditor`, okay-ui, driven through `Nav.update`
as `TestScreens` drives):

- [ ] the view marks the focused node and only it; the root is
      focused at start.
- [ ] `l`/`h`/`j`/`k` move the mark as the zipper moves; a move that
      does not exist leaves the view unchanged.
- [ ] `e` on a scalar pushes the prompt; answering it replaces the
      focus with the parsed value; cancelling leaves the document.
- [ ] `d` removes the focus and the focus moves to the parent; `d` at
      the root is a no-op.
- [ ] `a` after an array element inserts `JNull` after it; in an
      object the key prompt is pushed first and the field lands
      with that key.
- [ ] "done" answers `Some(edited)` through `done`; "cancel" answers
      `None` and the caller's document is the original.
- [ ] the same script through buttons (`Pressed`) and through keys
      (`Event.Key`) produces the same final document.

## Out of scope

- The Mirror-derived generic zipper (heterogeneous frames, a focus
  whose TYPE is the cursor position) — `zipper-mirror-derivative`,
  with its own trigger.
- A `PState` road for type-changing moves (`JObj` → `Option[Json]`
  at a field) — the plate zipper is homogeneous by construction.
- Renaming an object key, undo/redo, multi-line scalar editing — a
  product that needs any of them names it; the editor's state is a
  value, so undo is a list of them when asked for.
- Rewriting `Ui.path` over `Zipper.at`: `ui-path-two-walks-answered`
  measured the affine at 2.9-7.9x the hand walk, and a zipper walk
  would be a THIRD; the law here ties the two, nothing moves.

## Design

A frame keeps the PARENT NODE, its children vector and the focus
index: `up` is `withChildren(parent, siblings.updated(i, focus))`,
one path copy in the vector, and when the focus is `eq` to
`siblings(i)` — no `modify` happened below — `up` answers the parent
itself, so a walk without edits rebuilds nothing and `root eq input`
is a law rather than a hope. Huet's `(left.reverse, right)` pair was
rejected for the reason in the backlog entry: `left`/`right` cost the
same on both, `up` is a `reverse ++ ::` walk on the pair and follows
every `modify`, and the plate already answers a `Vector`.

The plate is two methods, not a `(Vector[T], Vector[T] => T)` pair:
the pair allocates a closure per descent, and `Ui.childAt` (the
hand-written frame this replaces for the editor's purpose) already
showed the pair shape costs one per step. `Plate.of(traversal)` is
the bridge for anyone holding a self-traversal: `children` is
`toVector`, `withChildren` a `modify` that reads the new children off
a counter — the one place a local `var` sits inside a pure function,
commented as such.

`Zipper.focus` is the seam to the effects: a lens on the CURSOR, so
`State.zoom(Zipper.focus)(p)` (Zoom.scala) runs a program written
against `State % T` at the focus, and the frames ride along in the
outer state. A move is an affine (`Option`), so inside a program it is
a `Condition`/pattern-bind refusal, not an exception. The editor
itself is a pure `Nav.screen` over a `Zipper[Json]` — its step is a
function of the cursor and the event, which is what makes it drivable
by `Nav.update` in a test with no host.

## Decisions

- **Plate over Mirror** — the three trees that exist (`Ui`, `Json`,
  `Schema`) are one node type with a children vector; a Mirror
  derivative would give heterogeneous frames nobody asked for and
  no uniform walk. Rejected: the generic zipper first (recorded as
  its own entry with a trigger).
- **Frames hold the parent node** — `withChildren` needs the node's
  other data (a `JObj`'s keys, a `Box`'s style); holding only the
  siblings would need a `rebuild` closure per frame. Rejected: the
  closure (an allocation per descent, and Huet's pair besides).
- **Structural edits are parent `modify`s, not zipper moves** —
  `delete`/`insert` change arity, and an object's plate cannot
  invent a key. `Json.removeChild`/`insertChild` on the parent keep
  the plate arity-preserving and the zipper homogeneous. Rejected:
  `Zipper.delete` (would need every plate to answer a shorter
  vector correctly — `JObj` cannot).
- **The editor is a `Screen`, in okay-ui, not a demo** — it is a
  reusable product screen (any host, any stack), and okay-ui already
  depends on okay-codec for `Form`. Rejected: okay-demo (JVM-only,
  not reusable).
- **Keys AND buttons** — the terminal host delivers chars, the HTML
  and Swing hosts press buttons; one `step` handles both so the test
  can assert they agree.

## Results

(after implementation)
