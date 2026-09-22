# form-drill — a form shown one level at a time, and a form over a typed cursor

## Overview

A form renders its value FLAT (`Form.render`, the `Schema.Folded`
algebra in Form.scala): a nested product is a titled `Column` inside
the parent's, a list is `Items` with `$add`/`$del`, a sum is a case
`Select` and the chosen case's fields. A deep value is one long form;
`TestFormDepth` proves only that it does not overflow the stack. There
is no way to ENTER a sub-record, edit it on its own screen, and come
back — which every product with a record of records wants (the
operator: "будет", 2026-09-23).

The zipper arc (specs/zipper.md) asked where a cursor meets a form,
and the code answered before anything was built: `Form.edit` already
routes every event by a DOTTED PATH (`address.city`, `lines[1].qty`,
`shape.$case` — `Path.parse`, `editAt`), `Form.errors` reports by the
same paths, and a rendered widget's key IS its path. So a drill-down
form's cursor is that path — a `List[Seg]` the screen pushes on
`into` and pops on `out` — and NOT a `Zipper[Json]` in lockstep with
a schema cursor as the first sketch said: a form makes one move per
many edits, the router recomputes from the root per edit already
(the optic road, right for that ratio), and a second walk beside it
would be the third walk `ui-path-two-walks-answered` warned about.
What is new is small: the schema and the value AT a path, and a
second MODE of the same render algebra that stops one level down.

The typed zipper meets forms elsewhere, and exactly where the entry
predicted: a program written against a PART. `Form.askAt(cursor)`
asks the form of the focus's type, seeded from the focus, and
answers the cursor with the focus replaced — `TypedZipper(order)
.down(customer)` edited by `Form.ask[Customer]`'s own loop, `up` a
`Top[Order]`. That road needs `Form.askFrom`, an `ask` that starts
from a VALUE rather than from `blank` — the seam `TestFormOptic`
encodes by hand today (`Json.parse(Json.write(a))`).

## Interface

okay-ui, `Form.scala` (scala-form):

```scala
object Form:
  /** the form of the value at `path`, ONE LEVEL: the focus's scalar
   *  fields as widgets, its composite fields and items as `into`
   *  buttons (`<key>$into`), keys prefixed by the path so every
   *  event still folds through `edit`/`submitted` unchanged */
  def renderAt[A](value: Json, path: String, errors: Vector[(String, String)] = Vector.empty,
                  labels: Map[String, String] = Map.empty)(using Schema[A]): Ui

  /** the drill-down screen: the whole partial value and a path; `into`
   *  pushes, `out` pops, every other event folds through `submitted`;
   *  `done` answers the value when `errors` is empty (else shows them),
   *  `cancel` answers None */
  def drill[A](value: Json)(done: Option[Json] => Nav)(using Schema[A]): Screen
  /** the same over an existing A: seeded by the codec, decoded on done */
  def drillValue[A](a: A)(done: Option[A] => Nav)(using Schema[A]): Screen

  /** `ask`, started from a value instead of `blank` */
  def askFrom[A](message: String, initial: A, checks: Check[A]*)(using Schema[A]): Option[A] ! Dialog
  /** the form of a typed cursor's focus, seeded from it; the answer is
   *  the cursor with the focus replaced, or None on cancel */
  def askAt[S, A, Z <: TypedZipper[S, A, Z]](z: TypedZipper[S, A, Z], message: String, checks: Check[A]*)
                                             (using Schema[A]): Option[Z] ! Dialog
```

## Behavior

`TestFormDrill` (okay-ui), over `Order(id, customer: Customer(name,
address: Address(city, zip)), lines: Vector[Line], shape: Shape)`
with `Line(sku, qty)` and `Shape = Circle(r) | Square(side)`:

- [ ] `renderAt` at the root shows `id` as an `Input` and `customer`,
      `lines`, `shape` as `into` buttons keyed `customer$into`,
      `lines$into`, `shape$into`; no nested widget is rendered.
- [ ] `renderAt` at `customer` shows `name` keyed `customer.name` and
      `address` as `customer.address$into`; at `customer.address` the
      two scalars keyed `customer.address.city`/`.zip`.
- [ ] `renderAt` at `lines` shows each item as `lines[i]$into` with its
      `$del`, and the list's `$add`; at `lines[1]` the item's scalars
      keyed `lines[1].sku`/`.qty`.
- [ ] `renderAt` at `shape` shows the case `Select` keyed `shape.$case`
      and the chosen case's scalars; choosing the other case through
      `edit` re-renders with its scalars.
- [ ] `renderAt` at a path that is not on the schema, or past a list's
      end, renders the root (total, like `edit`).
- [ ] THE LAW: a script of edits typed through the drill screen — with
      `into`/`out` moves between them — leaves the same `Json` as the
      same `Edited`/`Chosen`/`Pressed` events folded through
      `Form.submitted` on the flat form, for every script over the
      generated schema shapes; the moves are invisible to the value.
- [ ] `drill`: `into` pushes the path and the view shows the sub-form,
      `out` pops, `out` at the root is a no-op; `done` with a field
      error shows the error under its field at the focus and stays;
      `done` with none answers `Some(value)`; `cancel` answers `None`.
- [ ] `drillValue`: an `Order` in, an edited `Order` out through the
      codec, or `None`.
- [ ] `askFrom` shows the initial value's fields filled; `ok` without
      edits answers `Some(initial)`.
- [ ] `askAt`: `TypedZipper(order).down(customer)` asked, name edited,
      `ok` — the answer is `Some(cursor)` whose `.up.root` is the order
      with the new name and nothing else changed; `cancel` is `None`.

## Out of scope

- A cursor over the SCHEMA as a value (`Plate[Schema[?]]`) — the path
  walk answers the sub-schema in one function and nobody needs it as
  a zipper.
- Rendering more than one level (a depth knob) — one level is the
  drill-down; the flat form is depth ∞ and exists.
- Breadcrumb navigation by clicking an ancestor — `out` repeatedly is
  the same walk; a product that wants the crumb clickable adds the
  button.
- Reordering list items, undo — as in `JsonEditor`.

## Design

`renderAt` walks the schema and the value together by `Seg`s the way
`editAtNative` does (a private `focusAt(s, value, path): Option[
(Schema[?], Option[Json])]`, sums routed into the chosen case, isos
and options transparent), then runs the render algebra in DRILL MODE
with `prefix = path` and an empty name — so the focus is `root` to
the algebra, and its children are exactly one level down. The mode is
a flag on the algebra (`RenderAlgebra(drill: Boolean)`, one class,
two `Folded` instances): with it set, a product, a list and a sum
that are NOT root render as `Button(name ›, key = s"$key$$into")` and
produce no kids; a root list renders its items (a list CAN be the
focus in drill mode, where the flat form calls it "unsupported form"
because a form's root is never a list). Scalars, options, the case
`Select`, `$add`/`$del` and error placement are the flat algebra's,
untouched.

The screen's state is `(value: Json, path: List[Seg], errors)`. The
path's string form is the prefix `renderAt` takes, and `Path.parse`
of an `into` key minus its suffix is the push — the same parser the
router uses, so a key the renderer wrote is a path the router reads.

`askFrom` is `ask`'s loop started from `Json.parse(Json.write(
initial))` instead of `blank`; `askAt` is `askFrom(z.focus)` mapped
through `z.set`. Nothing in `ask` changes.

## Decisions

- **The cursor is the path, not a zipper** — for the ratio reason in
  the Overview and because the router, the errors and the keys are
  already paths: three things agree by construction. Rejected: the
  `Zipper[Json]` + schema-cursor pair (a third walk, and a positional
  zipper cannot follow a partial object whose fields are absent).
- **`into` is a key suffix like `$add`/`$del`** — the event vocabulary
  of forms is keys with suffixes, and a host that delivers `Pressed`
  delivers this. Rejected: a new `Event` case (every host would need
  to learn it).
- **The typed zipper enters through `askAt`, not through `drill`** — a
  drill-down UI chooses where to go at RUN time, and a typed cursor's
  position is a compile-time type, so a screen cannot hold "one of
  these cursors" without an existential; the typed road is for
  navigation the CODE chooses. Rejected: `Form.drill` over
  `TypedZipper` (was the first sketch, refuted by writing its state).
- **`drillValue`/`askFrom` go through the codec** — `Json.parse(
  Json.write(a))` is what `TestFormOptic` already does and what the
  drift law of the second order is about; a second encoder would be
  a second place to drift.

## Results

(after implementation)
