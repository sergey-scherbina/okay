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
  def left: Option[Zipper[T]]                             // the frame knows the siblings: no plate
  def right: Option[Zipper[T]]
  def at(path: List[Int])(using Plate[T]): Option[Zipper[T]]
  def modify(f: T => T): Zipper[T]
  def set(t: T): Zipper[T]
  def root(using Plate[T]): T
  def path: List[Int]                                    // root-first indices
  def index: Option[Int]                                 // position among siblings; None at the root
  def isTop: Boolean

object Zipper:
  /** the parent as it was, its children, which one the focus is, and
   *  whether anything below changed — `up` rebuilds only then */
  final case class Frame[T](parent: T, siblings: Vector[T], i: Int, dirty: Boolean = false)
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
the parent: `JsonOptic.removeChild(j, i)`, `JsonOptic.insertChild(j,
i, key, v)` (key used by objects, ignored by arrays).

okay-ui, `Ui.scala` companion: `given Plate[Ui]`, the structural walk
(`kidsOf`/`withKids` with `structural = true` — every child, on screen
or not). NOT `Ui.path`'s convention, as the first draft said: `childAt`
is the patch path, and the two differ on `Modal`, `Disclosure`,
`Table` and `Tabs` (Behavior, Ui).

okay-ui, `JsonEditor.scala`:

```scala
object JsonEditor:
  /** a screen editing `json` in place; `done(Some(j))` on Done,
   *  `done(None)` on Cancel — the Prompt shape of every dialog here */
  def apply(json: Json)(done: Option[Json] => Nav): Screen
```

Buttons, since every host delivers `Pressed` and none delivers
`Event.Key` (Decisions): `into` (first child), `out` (parent),
`prev`/`next` (siblings); an inline `value` input with `set` (the
text is `Json.parse`d, a parse error becomes a `JStr`); `delete` (the
focus, through the parent; the root cannot be deleted); `add` (a
`JNull` after the focus, with the `key` input's text in an object; at
a container root, inside it, last); `done` and `cancel`.

## Behavior

Zipper (`TestZipper`, okay-optics, over a rose tree the test
defines and over `Plate.of(traversal)` of the same):

- [x] `down(i)` on a node with an i-th child then `up` is the input
      cursor (same focus, path and root — cursors are compared
      observationally, the dirty flags are not the concept) AND the
      root `eq` the input tree — no rebuild when nothing was modified.
- [x] `down(i)` past the children, `up` at the root, `left` at index
      0 and `right` at the last child answer `None`; nothing throws.
- [x] `left` then `right` and `right` then `left` are the identity
      where both moves exist.
- [x] `modify(f)` at a focus, then `root`, equals the hand-written
      rebuild of that one node; the siblings and every other subtree
      are `eq` to the input's.
- [x] `at(path)` reaches what repeated `down` reaches, and `path`
      reads back what `at` was given.
- [x] `Zipper.focus` satisfies GetPut, PutGet, PutPut on cursors at
      every depth of the test tree.
- [x] `State.zoom(Zipper.focus)(p)` runs a `State % T` program against
      the focus and leaves the frames untouched: the same result as
      `modify` with the program's function.
- [x] `Plate.of(tr)` agrees with the hand plate on `children` and on
      `withChildren` for every node of the test tree.

Json (`TestJsonZipper`, okay-codec):

- [x] `Plate[Json]`: `withChildren(t, children(t)) == t` for arrays,
      objects and scalars; an object's keys are kept in order when
      values change; a different arity leaves the object unchanged.
- [x] `removeChild`/`insertChild` do what their names say on arrays
      and objects, keep other fields' order, and are the identity on
      scalars and out-of-range indices.
- [x] a document round-trips: `Zipper(j).at(p).map(_.set(v).root)`
      equals `JsonOptic`'s path optic setting the same value, on
      every path of a generated document.

Ui (`TestUiZipper`, okay-ui):

- [x] `Zipper.at[Ui](p).preview(t) == Ui.path(p).preview(t)` and the
      `set`s agree, for every path of the `TestUiOptic` trees that
      passes through nodes where the structural walk and the patch
      convention name the same children (Row, Column, Box, Scroll,
      Form, Items), and past the children both refuse.
- [x] where they part is pinned, not hidden: a `Modal`'s body is child
      1 on the patch path and child 0 for the zipper; a `Table` has
      rows for the zipper and no children for the path; a `Tabs` page
      off screen is reachable by the zipper — the editor's plate is
      the structural walk on purpose (Decisions).

JsonEditor (`TestJsonEditor`, okay-ui, driven through `Nav.update`
as `TestScreens` drives):

- [x] the view marks the focused node and only it; the root is
      focused at start.
- [x] `into`/`out`/`prev`/`next` move the mark as the zipper moves; a
      move that does not exist leaves the view unchanged.
- [x] the value input shows the focused scalar's text; `set` replaces
      the focus with the parsed value, or with the text as a string
      when it is not JSON.
- [x] `delete` removes the focus and the focus moves to the parent;
      `delete` at the root is a no-op.
- [x] `add` after an array element inserts `JNull` after it and
      focuses it; in an object the field lands with the key input's
      text; at a container root it appends inside; a scalar root
      takes nothing.
- [x] `done` answers `Some(edited)` through `done`; `cancel` answers
      `None` and the caller's document is the original.

## Out of scope

- (stage 1) The Mirror-derived generic zipper — was
  `zipper-mirror-derivative`, opened by the operator the same evening;
  it is Stage 2 below.
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
- **Buttons only, no letter keys** — checked before writing them: no
  host produces `Event.Key` (`Frame.interpretChar` drops a letter no
  `Input` is focused on), so `j`/`k` bindings would be code nothing
  can reach and a test could only drive by pretending. Rejected: the
  keys the first draft of this spec listed.
- **Inline editing, not a prompt dialog** — `Nav.To` after a `Push`ed
  `Toolkit.prompt` replaces the prompt's frame only (`Nav.updateCmd`),
  leaving the previous editor under the new one; an `Input` in the
  view keeps one frame and works on a host with no dialog. Rejected:
  the prompt the first draft of this spec listed.

## Results

2026-09-22, one lane. `TestZipper` 10, `TestJsonZipper` 3,
`TestUiZipper` 3, `TestJsonEditor` 6 — all green through
`scripts/gate.sh`, no warnings. Four things the code corrected in the
spec, each now in Decisions or Interface: sideways moves need no
plate; a frame carries a `dirty` flag rather than an `eq` test (which
would have been a cast on an unbounded `T`); no host produces
`Event.Key`, so the editor is buttons only; `Nav.To` after a `Push`ed
prompt leaves the old frame under, so the edit is an inline `Input`.
Found and pinned rather than papered over: `Ui`'s two conventions for
the i-th child (`childAt` vs `kidsOf`), which is why `Zipper.at ==
Ui.path` holds on Row/Column/Box/Scroll/Form/Items and is stated NOT
to on `Modal`/`Disclosure`/`Table`/`Tabs`. Not measured: the cursor
against the affine on a walk — `ui-path-two-walks-answered` priced the
affine at 2.9-7.9x the hand walk, and the zipper's claim is O(1) per
move by construction; a benchmark belongs to the first product that
walks enough to notice.

## Stage 2 — the typed zipper (zipper-mirror-derivative, 2026-09-22)

### Overview

Stage 1's cursor is homogeneous: one node type, a vector of children,
the plate deciding what a child is. Its frames cannot say WHAT they
are pointing into — `Zipper[Json]` at `order.customer.address` has
the type `Zipper[Json]` at the root too. McBride's derivative is per
FIELD: the one-hole context of `Order` at `customer` is "an `Order`
with a `Customer`-shaped hole", a different type from the context at
`lines`. Carried with a focus, that is a cursor whose POSITION IS A
TYPE — `TypedZipper[Order, Address, _]` says where it is, `set` takes
an `Address` and nothing else, and `up` answers the parent's type,
statically. The backlog entry named the cost before it named the
shape: every frame is a distinct type, so no loop walks "the
children" — that is stage 1's job and stays so; this is for a cursor
that knows statically where it is.

The frame IS an optic. A lens into a field is exactly the derivative
at that field with `put` as the plug; a prism into a case is the
derivative of a sum (one summand); an index into a `Vector` is the
affine the plate zipper walks by position. So the typed zipper adds
no machinery of its own to the optics: `down(lens)` is total,
`downPartial(affine)`, `downCase[B]` and `at(i)` answer an `Option`,
and `field("name")` is the Mirror lens `Lens.field[A]("name")` that
already exists — which is the whole Mirror content of the design, and
why the entry's `Frame[S, A] :: Frame[A, B] :: …` tuple path was not
needed: the parent's type is a type PARAMETER of the frame
(F-bounded, `Below[S, P, A, Z <: TypedZipper[S, P, Z]]`), carried and
never summoned, per the row-membership rule.

### Interface

okay-optics, `TypedZipper.scala`, package `okay`:

```scala
sealed trait TypedZipper[S, A, Self <: TypedZipper[S, A, Self]]:
  def focus: A
  def set(a: A): Self
  def modify(f: A => A): Self
  def root: S
  def depth: Int
  /** a lens frame: total */
  def down[B](l: Lens[A, A, B, B]): TypedZipper.Below[S, A, B, Self]
  /** an affine frame: the move may not exist */
  def downPartial[B](o: Affine[A, A, B, B]): Option[TypedZipper.Below[S, A, B, Self]]
  /** a prism frame: into one case of a sum */
  def downCase[B <: A](using ClassTag[B]): Option[TypedZipper.Below[S, A, B, Self]]
  /** the focus as a lens on THIS cursor's type, inferred from the
   *  receiver: `State.zoom(c.focusLens)(p)` needs no type arguments */
  def focusLens: Lens[Self, Self, A, A]

object TypedZipper:
  final case class Top[S](focus: S) extends TypedZipper[S, S, Top[S]]
  final case class Below[S, P, A, Z <: TypedZipper[S, P, Z]](parent: Z, put: (P, A) => P, focus: A, dirty: Boolean = false)
    extends TypedZipper[S, A, Below[S, P, A, Z]]:
    def up: Z                                    // the parent's TYPE, statically; put back only if dirty
  def apply[S](s: S): Top[S]
  /** the focus as a lens on the cursor — `State.zoom(TypedZipper.focus)(p)` */
  def focus[S, A, Z <: TypedZipper[S, A, Z]]: Lens[Z, Z, A, A]

extension [S, A <: Product, Z <: TypedZipper[S, A, Z]](z: TypedZipper[S, A, Z])
  /** a field by name, typed by the Mirror — `Lens.field[A](name)` as a frame */
  inline def field[L <: String & Singleton](inline name: L)(using Mirror.ProductOf[A]): Below[S, A, Elem, Z]
extension [S, B, Z <: TypedZipper[S, Vector[B], Z]](z: TypedZipper[S, Vector[B], Z])
  /** the i-th element of a Vector focus */
  def at(i: Int): Option[Below[S, Vector[B], B, Z]]
```

### Behavior (`TestTypedZipper`, okay-optics, over `Order(customer:
Customer(name, address: Address(city)), lines: Vector[Line])` with
`Line` a sum `Item | Discount`)

- [x] `down(l)` then `up` is the input cursor (same focus and root)
      and `root eq` the input when nothing was set; `.up.up` from two
      lenses down is a `Top[Order]` — the type is checked by the
      compiler, the test only runs it.
- [x] `set` at a two-lens focus then `root` equals the nested `copy`;
      every other field is `eq`.
- [x] `field("customer")` is `down(Lens.field[Order]("customer"))`:
      the focus has the field's declared type, and a wrong name does
      not compile (`compileErrors`).
- [x] `at(i)` on a `Vector` focus answers the element or `None` past
      the end; `set` through it updates that element only.
- [x] `downCase[Discount]` answers the case or `None`; `set` through
      it keeps the sum's other cases untouched elsewhere.
- [x] `TypedZipper.focus` satisfies GetPut, PutGet, PutPut; and
      `State.zoom(TypedZipper.focus)(p)` runs a `State % Customer`
      program at a `Customer` focus with the frames untouched — the
      program written against `Customer` while parked in an `Order`,
      which is the consumer the entry named.
- [x] a walk shares its prefix: from one `down(customer)` both
      `down(name)` and `down(address)` are taken, edited, and each
      `root` shows only its own edit.

### Decisions (stage 2)

- **Frames are optics; the parent type is an F-bounded parameter** —
  `up: Z` is what makes the position a type, and a parameter is never
  searched for. Rejected: a heterogeneous tuple of frames (the entry's
  first sketch — a `Tuple.Elem` computation per `up`, and a summon
  over it; the row-membership crash rule).
- **`field` by name reuses `Lens.field`** — the Mirror is already
  consulted there, once; a second derivation would be a second place
  for the same cast. Rejected: a `Mirror`-walking `∂` of the whole
  product (every field's frame at once, nobody asked).
- **Fixed-type frames, `Lens[A, A, B, B]`** — a type-changing `set[B]`
  retypes every frame up to the root; that is `PState.zoom` over the
  composed lens, which exists, and no cursor asked for it. Rejected:
  four-parameter frames.
- **No `at(path)` optic back from the cursor** — an affine frame has
  no total `get`, so the path is not a lens; stage 1's `Zipper.at` is
  the positional road. Rejected: keeping the getters to offer a lens
  that a prism frame would make a lie.
- **No `left`/`right`** — fields are not a sequence; a `Vector` focus
  goes through `at(i)`, and sideways among siblings is stage 1.

### Results (stage 2)

2026-09-22, the same evening, one lane. `TestTypedZipper` 7, green
through `scripts/gate.sh`, no warnings. The first run was RED on the
one law worth having — `root eq input` after a walk without edits —
because `up` always ran `put` (a `copy`); the frame gained the same
`dirty` flag stage 1's has, and the lens laws compare cursors by
focus and root. `up.up` from two lenses is typed `Top[Order]` and
`d.up` from a case frame is typed `Below[Order, Vector[Line], Line,
?]` — both are `val` ascriptions in the test, so the compiler is the
one asserting them. `compileErrors` pins that a misspelt field name
is refused. Not built, each with its reason in Decisions: a
type-changing `set`, a lens back from the cursor, `left`/`right`.
