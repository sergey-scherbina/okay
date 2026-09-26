# One facade over every foreign language

> A chapter of [Foreign languages](foreign-languages.md): the model every
> language here shares — what crosses, who carries what, the wire.

okay talks to Python, R, TypeScript, Haskell, Go, Rust — each in its
own process, over one wire — and to Clojure, Frege and Java inside the
JVM. This page is about writing the Scala side ONCE: a job names a
module and a function, and a typeclass by the module's type says who
runs it, how the data crosses, and what that language cannot do at all.
The design, its stages and its measurements are
[specs/foreign-facade.md](../specs/foreign-facade.md); the languages
themselves have their own pages ([Python and R](python-and-r.md),
[Rust](rust.md), [Go](go.md), [TypeScript](typescript.md),
[the JVM's languages](jvm-languages.md)).

## A shop, priced by whoever runs it

A module is a value: a `PyModule` is Python source, an `RModule` is R
source, a `JvmModule` is Scala functions by name. The job below does not
say which:

```scala
final case class Order(sku: String, qty: Long) derives Schema
final case class Priced(sku: String, total: Double) derives Schema
```

```scala
val one = Road.value[JvmModule, Order, Priced](shop, "price")(Order("tea", 2L))
```

`Road.value` asks for `Calls[module.type]` — the typeclass for ONE typed
call, a value in and a value out — and `shop` here is a `JvmModule`,
so the call is a Scala function and nothing crosses:

```scala
  val shop = JvmModule("shop")
    .fn[Order, Priced]("price")(o => Priced(o.sku, o.qty * 2.5))
    .frame("priceAll")(identity)
```

Hand the same line a `PyModule` whose `price` is a Python function, and
the `Order` reaches Python as a dict, the answer comes back as a
`Priced`, and a Python exception comes back as a `Left` naming its kind
— the same text, the other side of a pipe. That is what the facade is:
`Calls`, `Frames`, `Streams`, `Programs`, `Holds`, `Methods`, `Speaks`,
each a typeclass with an instance per language that can honestly give
it, and a job that compiles against exactly the ones it uses.

## The data picks the road

There is no switch for "fast" — the SHAPE of what you hand over picks
the tier, and every language crosses each tier on the best road it has:

| you hand over | tier | it crosses as |
|---|---|---|
| a value | 1, a call | one line of the wire (JSON) |
| rows, or a `Table` | 2, a frame | Arrow IPC where the worker speaks it, columnar JSON where it does not; on the JVM the same `Table` object, by reference |
| a cluster `Flow` | 3, a stream | one frame per chunk, the next sent when the last answered |

```scala
    val orders = Vector(Order("tea", 2L), Order("milk", 1L))
    val priced = Road.rows[JvmModule, Order, Order](shop, "priceAll")(orders)
```

```scala
    val flow = Road.flow[JvmModule, Order, Order](shop, "priceAll", 4096)(Flow.slices(orders, 1))
```

A function written for a frame takes a frame (a dict of columns in
Python, a data.frame in R) and one written for a record takes a record:
they are different functions, so rows are always one frame and a value
is always a call — no count turns the one into the other. Tier 3 is
tier 2 repeated: a `Flow` of a million rows crosses in frames of 4 096,
and neither side ever holds more than a frame of it. What each tier
costs, per language, is measured in the spec's table; on this box a
frame of 100 000 rows to Python and back is ~190 ms, and streaming the
same rows in 25 frames costs nothing over the one.

`Speaks` says what THIS worker does, as against what its language can:

```scala
    val report = summon[Speaks[JvmModule]].speaks(shop)
```

`report.frames` is `arrow`, `columnar-json` or `by-reference`;
`report.programs` says whether a far-side program's continuation can be
resumed twice (`multi-shot`: Python, R — their continuations are
values).

## What a language cannot do does not compile

R holds objects but has no methods to call on them; the JVM needs no
programs-as-data because a Scala function is already here. Each is a
missing instance, and a job that asks for it stops at the compiler:

```scala
    assert(compileErrors("summon[Methods[okay.r.RModule]]").nonEmpty)
```

A module type of your own is welcome the same way: give it the
instances it can honestly give, and the conformance suite
(`FacadeConformance`, one body per capability) is what it has to pass
— the same body Python and R pass.

## Programs as data, and objects held

A far-side program that performs named operations and is continued by
okay's handlers — a callback answered under a `Reader`, a `Choice` that
resumes the same Python continuation twice — goes through `Programs`
with ONE callback type, `Cb`, over `Schema` values (Python's and R's own
`Callback`s are converted at the seam). A whole dialogue runs on one
worker, because that worker holds the continuations. An object the far
side keeps — a fitted model, an open file — is a handle from `Holds`,
passed to functions as an argument like any other and released when
done; its own methods and attributes are `Methods`, which only Python
gives. The bodies for both are in `FacadeConformance`, run over python3
in `TestPyFacade`.

The JVM's own languages take `Programs` too (foreign-jvm-programs): a
Clojure namespace (`CljModule`) or a set of Frege programs (`FregeModule`)
is walked in this process by `okay.Foreign`, and what it performs is the
same `Cb` callbacks — so the one conformance body runs over them:

```scala
FacadeConformance.programs(CljModule("okay.cluster.facade"), "priced", "pairs")
```

The Clojure function takes the argument as JVM data (a record is a
`java.util.Map`) and answers the program; a Frege program is registered
with the small glue its lazy arguments need. A Scala function module has
no instance: there, a program is just a function returning `Out ! F`.

## Adding a language

A module type, the instances it can honestly give, and the conformance
suite green for each. No `mapXx`, no new door. The shims grow toward the
tiers one at a time. Since foreign-more-languages every wire language has
its module type: `TsModule` (TypeScript source, as `PyModule` is Python's)
and `WorkerModule` (a compiled Go, Rust or Haskell worker, by the command
that starts it), each one `Language` value under the same bodies. A
compiled worker has `Holds` and `Models` too since foreign-held-values —
its libraries keep held values — and no `Methods` or `Stateful`: a value
has no methods by name, and a held value is not changed in place. The
compiler says so.

## Literature

- Matthias Grimmer, Chris Seaton, Roland Schatz, Thomas Würthinger, Hanspeter Mössenböck. *[High-performance cross-language interoperability in a multi-language runtime.](https://doi.org/10.1145/2816707.2816714)* DLS 2015. Truffle's interop protocol: one set of messages every language answers, and a call site that specialises on who answers — the closest prior art to a facade per capability, in one process rather than over a wire.
- Philip Wadler, Stephen Blott. *[How to make ad-hoc polymorphism less ad hoc.](https://doi.org/10.1145/75277.75283)* POPL 1989. A typeclass per capability, an instance per type that has it, and a missing instance a compile error — the facade's whole shape.
- Mark Raasveldt, Hannes Mühleisen. *[Don't hold my data hostage: a case for client protocol redesign.](https://doi.org/10.14778/3115404.3115408)* PVLDB 2017. Why a frame crosses as columns and not as cells; the tier-2 road and its measurement.
- Apache Arrow. *[Arrow Flight RPC.](https://arrow.apache.org/docs/format/Flight.html)* One memory format on both sides of a wire, streams of record batches with back-pressure: tier 3's shape, with okay's own wire in place of gRPC.
- Project Jupyter. *[Messaging in Jupyter.](https://jupyter-client.readthedocs.io/en/stable/messaging.html)* A kernel's `kernel_info` reply says what the kernel speaks before anything runs — `Speaks`, before it was ours.
- Joe Armstrong. *[Making reliable distributed systems in the presence of software errors.](https://erlang.org/download/armstrong_thesis_2003.pdf)* PhD thesis, 2003. Ports: a foreign program as a process that speaks a protocol, and the one place where the language does not matter.
- Substrait. *[Substrait: cross-language serialization for relational algebra.](https://substrait.io/)* What a plan looks like when it must mean the same thing to every engine — the tier above frames that this design does not take yet.
- Alistair Cockburn. *[Hexagonal architecture.](https://alistair.cockburn.us/hexagonal-architecture/)* 2005. Ports and adapters: the facade is the port, each language's instances an adapter, and the conformance suite is what makes an adapter one.

The whole design, stage by stage, with what was measured and what was
withdrawn: [specs/foreign-facade.md](../specs/foreign-facade.md).
