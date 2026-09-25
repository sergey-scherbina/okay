# Cursors and declarations

Two ideas, built out over two days (2026-09-22/23), that turned out to
be one: **a walk through a structure is a value you can hold**, and
**a declaration is a value several interpreters can read**. This page
is the map of what shipped, what each thing is for, the law each one
pins, and where the longer story lives. Every snippet here is lifted
from a test that runs in the gate; the file is named beside it.

Reading order if you are new: §1 is the stream side (a consumer that
is a program), §2–§3 are the cursors (a position in a tree, untyped
then typed), §4 is where a cursor meets a form, §5 is the declarations
(policy, query, tools), §6 is a one-line bridge, §7 a table of when to
reach for which.

## 1. A consumer is a program: iteratees, `Pull`, `Take.each`

Oleg Kiselyov's *iteratee* is a consumer that asks for its next
element and is suspended until one arrives; the producer is whatever
answers. okay had it before it had the name: `Take.Await` is the ask,
a `Writer` program is the producer, `pipe(producer)(consumer)` is the
pairing by delimited control, `Stage` the transducer between them
([theory ch. 7](theory/07-logic-streams.md#iteratees-the-consumer-as-a-program),
[Kiselyov 2012](https://doi.org/10.1007/978-3-642-29822-6_15)). Two
things were still hand-written and are not any more.

**A loop over a source whose step is a program.** A `Stream` carrier's
`uncons`, a writer program's next told value, the `Take` side of a
stage — none has an iterator, so `for x <- src do …` in a direct block
could not read them. `Pull[A, G]` is that source as a value:

```scala
// okay-direct TestDirectSource
val (log, _) = run(direct { for x <- Pull.of(List(1, 2, 3)) do say(s"x$x").? })
assertEquals(log, Seq("x1", "x2", "x3"))

// okay-llm TestTakeLoopInBlock — an iteratee written as a loop: a Stage
val doubling: Stage[Int, Int, Unit] = direct[[A] =>> A ! Take % Int + Writer % Int] {
  for i <- Take.each[Int] do Writer.tell(i * 2).?
}
```

The loop is emitted as a program — one `step` bound per element,
through the same row lift a mark takes — and it fires on the
receiver's TYPE, marks in the body or not. Outside a block the loop is
`src.loop(f)`, a program by name; the `for` over a source does not
typecheck there, on purpose (a program in statement position is the
discarded-program error build.sbt escalates). Laws: laziness by a step
counter; the producer's own effects interleave with the body's, one
step per element. `Pull.of(stream)`, `Pull.told(p)`/`toldIn(p)`,
`Take.each[I]`. Design: [direct-style.md](direct-style.md), "A loop
over a source"; specs/direct-loops.md v3.

## 2. A cursor over a tree: `Zipper`, and the editor that needed it

An optic is a *path*: recomputed from the root on every operation,
holding nothing between two of them — the right shape for an edit,
and `Fuse` makes it cost the hand-written update. A **zipper** (Huet
1997) is a *cursor*: the path materialised as frames, so a step to a
sibling or to the parent is O(1). It earns its place only where moves
outnumber edits, and it took a product that edits a tree to open it.

```scala
// okay-optics TestZipper — over any tree with a Plate (Uniplate's children/descend)
val z = Zipper(tree).at(List(2, 1)).get
z.modify(r => r.copy(label = r.label.toUpperCase)).root     // one node rebuilt, every other subtree shared
Zipper(tree).at(List(2, 0)).get.set(x).right.get.set(y).root // an edit survives a sideways move
```

`Plate[T]` is how a tree exposes its children (`Json` and `Ui` have
one; `Plate.of(traversal)` builds one from a self-traversal). A walk
without edits hands back the input tree `eq` — a frame carries a
`dirty` flag and `up` rebuilds only under an edit. The two bridges to
the optics: `Zipper.focus: Lens[Zipper[T], Zipper[T], T, T]` (so
`State.zoom(Zipper.focus)(p)` runs a program at the focus) and
`Zipper.at(path): Affine[T, T, T, T]`, pinned equal to `Ui.path` on
every node where the two conventions for "the i-th child" agree — and
stated where they part (`TestUiZipper`).

The product: `JsonEditor(json)(done)` in okay-ui, a `Screen` over
`Zipper[Json]` — into/out/prev/next, an inline edit at the focus,
add/delete as a `modify` of the *parent* (a plate is arity-preserving
and an object's key cannot be invented by one), driven in
`TestJsonEditor` through `Nav.update` with no host.

Theory: [ch. 10, "The zipper: the residual, carried"](theory/10-optics.md#the-zipper-the-residual-carried)
(McBride's derivative is the one-hole context; Huet's zipper carries
it with a focus). Spec: specs/zipper.md, stage 1.

## 3. A cursor whose position is a type: `TypedZipper`

McBride's derivative is per FIELD: the context of `Order` at
`customer` is a different type from its context at `lines`. A lens
into that field *is* that derivative with `put` as the plug, a prism
into a case the derivative of a sum, an index the affine of §2. So
every frame of the typed cursor is an optic the library already has:

```scala
// okay-optics TestTypedZipper
val c = TypedZipper(order).down(customer)                    // TypedZipper[Order, Customer, _]
val top: TypedZipper.Top[Order] = c.down(address).up.up      // the compiler checks the type of `up`
c.field("address").field("zip").focus: Int                   // by NAME, checked against the Mirror
TypedZipper(order).down(lines).at(1).flatMap(_.downCase[Line.Discount])   // an index, then a case
State.run(c)(State.zoom(c.focusLens)(renameCustomer))        // a State % Customer program, parked in an Order
```

What else it answers:

- `asAffine` — the walk back as an optic on the tree (affine: an
  index or a case frame may be missing on another tree).
- `pathKey` — the walk as the dotted key a form routes by
  (`customer.address.city`, `lines[1].qty`); `Some` only when every
  frame was taken by name, `None` below a lens frame.
- `at(i)` on a `Vector` focus is an element frame with `left`/`right`
  among its siblings — the one sideways move the types allow; on
  FIELDS there is none (the field beside `customer` is a `Vector[Line]`,
  not another `Customer`), and code that wants another field names
  it: `c.up.field("lines")`.
- `TypedZipper.Poly[A, B, T]` — the type-changing cursor: a focus and
  the plug `put: B => T`, `down` by a four-parameter lens, `set` is
  the new whole. No frames and no `up`, because after a type change
  there is no parent of the old type.

Spec: specs/zipper.md, stages 2–5. Theory: ch. 10, the same section.

## 4. Where a cursor meets a form: `Form.drill`, `askAt`

A form renders flat; a deep record was one long form. `Form.drill`
shows it one level at a time — and its cursor is **not** a zipper:

```scala
// okay-ui TestFormDrill
Form.renderAt[Order](doc, "customer")   // keys: customer.name, customer.address$into
Form.drill[Order](value)(done)          // into pushes the path, out pops, done answers when errors is empty
Form.drillAt(TypedZipper(order).field("customer").field("address"))(done)   // opened where the TYPE said
Form.askAt(TypedZipper(order).down(customer), "customer")     // the form of the focus, answering the cursor
```

The code answered the design before anything was built: `Form.edit`
routes every event by a dotted path, `Form.errors` reports by the
same paths, a widget's key *is* its path — and a form makes one move
per many edits. So the drill's cursor is that path (a `List[Seg]` the
screen pushes and pops), the render algebra gained a DRILL MODE
(composites one level down become `<key>$into` buttons), and the law
`TestFormDrill` pins is that a script of edits with `into`/`out`
moves between them leaves the value the flat fold leaves. The typed
cursor enters a form where the CODE chooses the part — `askAt`,
`drillAt` — never where the user does. Forms hold the draft as `Json`
and meet `A` at `askFrom`/`drillValue` (through the codec) and at
`decode`: a half-filled form is not an `A`.

Spec: specs/form-drill.md. Guide: [frontend-guide.md §3](frontend-guide.md).

## 5. One declaration, several readers: `Policy`, `Query`, `Toolbox.In`

The criterion specs/optics-outside.md set for an optic in a public
API: the same declaration must be given to more than one interpreter,
and at least one of them must DESCRIBE it instead of running it. A
function `A => B` cannot be asked what it looks at. Three declarations
now meet it, each with the law that couples its readers.

**A projection policy** (okay-codec `Policy`): which fields may not be
seen, embedded, logged.

```scala
// okay-codec TestPolicy
val policy = Policy.hide[Order]("customer.email", "lines.price", "shape.secret", "note").toOption.get
policy.touches                 // DESCRIBE, no document: Set("customer.email", "lines.price", ...)
policy.project(doc)            // RUN: the fields removed — every line's price included
policy.redact(doc)             // RUN: kept, values replaced
policy.text(order)             // what an embedding or a log line may see
```

A key naming nothing the schema writes is refused by name at
construction. **Law:** the keys `project` removes are exactly
`touches`, restricted to the keys the document has — through lists
element-wise, through a sum into the case that has it, an absent
`Option` written as `null` and therefore present.

**A query** (okay-sql `Query`): a predicate over rows of `A` by field
name, checked against the schema — by name and by column type.

```scala
// okay-sql TestQueryPure, okay-jdbc TestQuerySqlite
val age = Query.field[Customer, Int]("age").toOption.get
val w = (name like "a%") and (age >= 18) and !(active === false)
w.sql        // DESCRIBE: ("((user_name LIKE ?) AND (age >= ?)) AND (NOT (active = ?))", params)
w.test(ann)  // RUN: the same predicate in memory, NULL three-valued
Query.select[Customer]("customer").toOption.get.where(w)   // what Typed.rows decodes, columns off the schema
Query.update[Customer]("customer").set(balance, 99.0).where(w)   // the UPDATE, and apply(a) in memory
```

**Law, against a real engine:** for fourteen predicates, the rows
SQLite returns are the rows `test` keeps; an UPDATE leaves the table
as the in-memory edit leaves the rows. The one divergence is a test
that says so: SQLite's `LIKE` is case-insensitive for ASCII, SQL's is
not. No joins, no expressions, no dialect object — the swamp the spec
named, stayed out of.

**A tool that is a program** (okay-agent `Toolbox.In[F]`, okay-mcp
`Server.serveIn`): the pure seam `A => String` stays; a tool that
must do I/O is `A => String ! F`, same declaration, same decode.

```scala
// okay-agent TestToolbox, okay-mcp TestServer
val effectful: Toolbox.In[W] = Toolbox.In.empty[W]
  .on[Add]("log", "Log a task.")(a => Writer.tell(s"logged ${a.text}").map(_ => "ok"))
  ++ box.in[W]                                  // a pure box lifted beside it
Handlers.relayToolsF(effectful.table)(prog)     // the agent, the row one effect shorter
Server.serveIn[Async](serving)(Server.answering(serving))   // the MCP protocol, written once, in a row carrying G
```

`serve` is `serveIn[Pure]` — the eleven pure server tests are the
guard that the generic stage is the old one. A tool's failure is its
own row's business: a program cannot be `try`-caught from outside.

**A subscription is a lens** (okay-live `Watched`): a document many
viewers watch through paths — `subscribe("customer.address")` is
told the address, and only the address, each time it changes;
`set(key, value)` is the client's write through the same lens; the
dotted key is the lens's wire form, compiled against the schema and
refused by name, and a `TypedZipper`'s `pathKey` is one. **Law:**
over any history of edits a subscriber receives exactly the distinct
consecutive values of its focus (`TestWatched`).

Spec: specs/optics-outside.md, stages 7–10. Guide: [optics.md](optics.md)
§6–7, [declaring-an-api.md](declaring-an-api.md).

## 6. The one-line bridge: `Reader.lift`, `Reader.unlift`

A context function `E ?=> A` IS a pure Reader program. `Reader.lift(cf)`
asks once and applies; `Reader.unlift(p)` runs a Reader program under
the ambient `E`, forwarding the rest of the row. Functions, not
`Conversion`s — measured on 2026-09-01: a context function
auto-applies at the ascription site before a conversion could see it
(`TestReaderBridge`, specs/context-functions.md).

## 7. When to reach for which

| you have | you want | reach for |
|---|---|---|
| a tree, one edit at a known path | the update, at hand-written cost | an optic: `Lens`/`Affine`, `Ui.path`, `JsonOptic.path` |
| a tree, many moves per edit, chosen at run time | a cursor | `Zipper[T]` over a `Plate` (`JsonEditor` is the shape) |
| a record, a position the CODE chooses, typed | a cursor with a type | `TypedZipper` (`field`, `at`, `downCase`; `asAffine`, `pathKey`) |
| a record whose whole must change type with the part | the plug | `TypedZipper.Poly`, or `PState.zoom` over the composed lens |
| a form over a deep record | one level at a time | `Form.drill` — the cursor is the dotted path; `drillAt`/`askAt` from a typed cursor |
| a source whose next element is a program | a loop in a direct block | `Pull` (`for x <- Pull.of(s) do …`, `Take.each`), `loop(f)` outside a block |
| a rule about fields you must also be able to AUDIT | a declaration two readers share | `Policy` (fields), `Query` (rows), `Toolbox.In` (tools) |
| a document many viewers watch, each a part of it | a subscription that is a lens | `Watched[A]` — `subscribe(key)`, `set(key, v)`; the key from a `TypedZipper.pathKey` |
| `E ?=> A` where a Reader program is wanted, or back | the bridge | `Reader.lift` / `Reader.unlift` |

Two things the walk refused, with the reason recorded where it was
refused: a covariant `Free` (it typechecks, and it made `Source.merge`
5–7% slower — specs/writer-covariance.md) and `left`/`right` across
FIELDS of a case class (the neighbour of a field has another type).

## Literature

- Gérard Huet, *The zipper*, JFP 7(5), 1997; Conor McBride, *The
  derivative of a regular type is its type of one-hole contexts*,
  2001 — both in [theory ch. 10](theory/10-optics.md#references).
- Oleg Kiselyov, *Iteratees*, FLOPS 2012 (doi:10.1007/978-3-642-29822-6_15);
  Kiselyov, Peyton Jones, Sabry, *Lazy v. Yield*, APLAS 2012
  (doi:10.1007/978-3-642-35182-2_14) — [theory ch. 7](theory/07-logic-streams.md#references).
- Profunctor optics and Tambara modules — [theory ch. 10](theory/10-optics.md).
