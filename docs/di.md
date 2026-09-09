# Dependency injection — the module vocabulary

[Capabilities](capabilities.md) tells how a dependency becomes a
TYPE: a consumer declares what it needs (`using`, `?=>`, `wire[A]`),
`provide`/`providing` install values for a scope, and a missing
dependency is a compile error rather than a container exception at
start-up. This page is the other half — what to do when a dependency
must be OPENED and CLOSED, when one dependency is built from another,
when the application runs inside somebody else's container, and when
a deployment wants to know what the application needs from the place
it runs in.

Everything here exists in the tree, and the samples below are
COMPILED: they run as `TestDiDocs` in okay-deploy, so a rename that
makes this page wrong fails a test rather than misleading a reader.
The application example is okay-demo's `ChatDemo`; the design record,
with what was refuted along the way, is
[specs/di.md](../specs/di.md).

## A module is an installer that has not been built yet

`providing[Db](db)` holds a value that already exists. A `Module[F]`
builds one:

```scala
val db   = module[Db](Db.open(url))(_.close())
val conf = Module.value[Conf](Conf("/app/data/board.log"))
```

`module` acquires inside the `Resource` region and the region
releases, in reverse order, whatever else the program does — at its
value, at a throw, at a cancellation. `Module.value` is the module
with nothing to build: a config, a test double, a value the caller
already has.

Where the thing you open is not the thing you install, say both:

```scala
moduleAs[Store, FileStore](FileStore.open(path))(_.close())
```

A `FileStore` is opened and closed; `Store` is what the program
should see, and `Store` has no `close` and should not grow one for
this.

## The graph is the composition

`and` puts its right operand INSIDE the left's context, so a module
that needs an earlier one just reads it:

```scala
val db = module[Db](Db.open(url))(_.close())
val pool: Db ?=> Module[[X] =>> Pool ?=> X] =
  module[Pool](Pool.over(wire[Db]))(_.close())

Resource.run[Int, Pure]((db and pool) { wire[Pool].borrow() })
```

There is no separate wiring language: the dependency IS the
composition, and the compiler checks it — an acquisition naming a
capability that no module before it installs does not compile. Left
acquires first, so the right releases first.

The right operand is the inner layer, which is what makes a test
double one more `and`:

```scala
val base = db and pool
base and Module.value[Db](fake)     // `fake` wins under nearest-wins
```

Cycles cannot be expressed. That is the feature, not the limit.

## Running the body

`m { body }` installs everything and runs a body that answers a
value; `m.use { body }` runs a body that is ITSELF a program in the
scope — a server, a stream, anything that acquires further:

```scala
Resource.run[Unit, Pure](modules.use { Jetty.serve(port)(routes)().map(report) })
```

The two type arguments are the body's answer and the row that is
FORWARDED — `Pure` when the region is the whole story.

Use `use` for an application. `apply` would answer a program inside a
program, and the discarded-value lint would catch it at every call
site.

Where the end of the scope belongs to somebody else — a Spring
context, a `main` with a shutdown hook — open it and keep the closer:

```scala
val (values, close) = Resource.open(modules.exports)
```

`close` releases in reverse order and is idempotent.

## Two of one type: qualifiers are types

A string qualifier would be a runtime concept in a compile-time
story. Use an opaque type per ROLE:

```scala
opaque type Primary = Db
opaque type Replica = Db
```

`wire[Replica]` where only `Primary` is installed is a compile error
naming the role. Two roles over one class stay two capabilities
everywhere, including through the bridges: two beans by name, one
ambiguous type.

## Reading a module without building it

Three questions can be answered before anything opens.

**What will it install, in what order?** `m.plan` is a
`Vector[String]` read off the module's TYPE by a macro — the curried
chain `A ?=> B ?=> … ?=> X` already says it, so nothing is built to
print it, and an opaque qualifier shows as itself.

```scala
(db and pool and log).plan     // Vector("Db", "Pool", "Log")
```

**What did it install, and under what class?** `m.exports` collects
each installed value with its plan name and erased class — that is
what a container registration needs, and no reflection touches the
values.

**What does it need from the place it runs in?** A component declares
that where it opens the thing:

```scala
import okay.deploy.Needs.needs      // the extension lives with its reader

moduleAs[Store, FileStore](FileStore.open(file))(_.close())
  .needs(Need.Volume(dir))
```

`and` merges the declarations; `Needs.declared(app)` reads them off
the composed value. The kinds of fact are open: `Fact[V]` is a typed
key with its own merge, so a reader outside the core defines its own
kind, and the core knows no deployment word.

**When these can be read.** A dependent module hides behind its
function until its input exists. So a module knows whether it is
READY — `Module.value` and `Module.ready` are, an acquired one is
not — and when the left of `and` is ready, the right is applied at
once and its facts come with it. In practice: everything after the
CONFIG and before the first ACQUISITION is readable, which is when a
manifest is written. Facts declared below an acquisition wait for the
scope to run.

## An application, end to end

okay-demo's chat is wired this way. Its config is a value, so
everything after it can be read without opening anything:

```scala
final case class ChatConf(db: String)

def wiring(using Timer): ChatConf ?=> Module[[X] =>>
    Store ?=> Board ?=> Transport ?=> Secrets ?=> X] =
  val path = wire[ChatConf].db
  val store =
    if path == ":memory:" then Module.value[Store](MemoryStore())
    else moduleAs[Store, FileStore](FileStore.open(Path.of(path)))(_.close())
           .needs(Need.Volume(dirOf(path)))
  val board: Store ?=> Module[[X] =>> Board ?=> X] =
    module[Board](Board(Board.topicOf(wire[Store])))(_ => ())
  store and board and
    Module.value[Transport](guarded(Transports.http())) and
    Module.value[Secrets](Secrets.env)

type Root = Timer ?=> Module[[X] =>>
    ChatConf ?=> Store ?=> Board ?=> Transport ?=> Secrets ?=> X]
```

`main` composes the config with the wiring and runs the server with
`use`; the deployment reads the same value with the settings it ships
as the config, and gets the volume the store declared. What is left
UNRESOLVED in `Root` is what something outside must give — here a
`Timer`, which the process brings and no place provides.

Two things this conversion taught, both worth expecting in your own:
a global `lazy val` is exactly what a module replaces, and every
reader of it becomes a door (the demo's routes had to take `Store` as
a capability, or a module's store beside the global would have opened
one log twice); and the file the application had always opened was
never closed until the region owned it.

## Inside somebody else's container

The rule is one sentence: **the container stays the container, the
module stays a value.** Nothing scans a classpath, reads an
annotation off your class, or proxies anything; your classes take
their dependencies as constructor parameters and `using` clauses,
which is the condition of portability.

| | module → container | container → module |
|---|---|---|
| [Spring](modules/okay-spring.md) | `OkaySpring.register(ctx, m.exports)` — a singleton per capability, named by the plan, a `DisposableBean` closing the scope with the context | `OkaySpring.bean[A](ctx)` |
| [Guice](modules/okay-guice.md) | `OkayGuice.bindings(m.exports)` — by name, and by type where the class is unique; the closer bound as an instance | `OkayGuice.instance[A](injector)` |
| [CDI](modules/okay-cdi.md) | `OkayCdi.extension(m.exports)` — a synthetic `@Singleton` `@Named` bean per capability, released at `BeforeShutdown` | `OkayCdi.instance[A](container)` |
| [ZIO](modules/okay-zio.md) | `ZioLayers.toLayer(m)` | `ZioLayers.fromLayer(layer)`, `fromEnvironment(env)` |

A WebFlux controller may return `A ! Async` directly: okay-spring's
Boot auto-configuration registers both an adapter and a result
handler. The second is not optional — WebFlux reads a reactive type's
element from generic index 0, and ours is at index 1.

## What a deployment reads

Two roads, and an application usually has both.

**By type** — what the root still WAITS for. Each such capability
declares once what it is to a deployment:

```scala
given Needs[Pg]    = Needs(Need.Database(Engine.Postgres, "16", "shop"))
given Needs[Timer] = Needs.runtime
Needs.of[Root]     // the place's needs; the runtime's are dropped
```

An input that declares NEITHER is a compile error naming the type: a
need the code has and the deployment does not know about stops the
build. `Timer` and `Scheduler` are declared runtime in `Needs`'s own
companion, once, for every application.

**By value** — what the components DECLARED as they were composed
(`Needs.declared`, above). Put together, a `Service` says what the
code needs and what only the place can say:

```scala
needs = Needs.declared(conf and wiring) :+ Need.Port(8090)
```

## Gotchas

- **`Module` is a class, not an alias over its program.** An
  extension `apply` on `Providing[F] ! Resource` types
  `m { wire[Db].q }` without its expected type and applies the body
  eagerly — the E10 trap of specs/context-functions.md. If you write
  your own combinator over modules, make it a method.
- **`Module.ready`, not `Module.apply`,** lifts a `Providing` — a
  companion `apply` overload would shadow the constructor.
- **Facts below an acquisition are not readable early**, by
  construction. If a deployment must see them, put the value they
  depend on (the config) above the first acquisition, which is where
  it belongs anyway.
- **A qualifier's export carries the ERASED class.** Two roles over
  one class are two names and one ambiguous type in every container;
  ask by name there.
- **`register`/`bindings`/`extension` are EAGER.** They build the
  module when called. A value the module needs FROM the container is
  reached with `bean`/`instance`, not the other way round.

## Where to go next

- [Capabilities](capabilities.md) — the mechanism underneath: context
  functions, doors, `provide`/`providing`/`wire`, and the boundaries.
- [specs/di.md](../specs/di.md) — the design record: what each stage
  decided, what was refuted, and what using it in an application
  changed.
- [okay-deploy](modules/okay-deploy.md) — the deployment value the
  needs above feed, and every target it renders.
