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

## What you are holding: a recipe, not a running thing

Everything on this page builds ONE kind of value. A `Module` is a
description of what to construct — nothing in it has run. Composing
two modules makes a bigger description. Reading `plan` or the declared
needs reads the description. Nothing opens a file, binds a port or
allocates a connection until a region runs it:

```scala
val app = (db and pool).installing(Routes)   // nothing has happened yet
Resource.scoped(app { serve(wire[Routes]) }) // NOW it opens, serves, and closes
```

So `app` is to the running service roughly what a Dockerfile is to a
container. That distinction is what makes the rest possible: a
deployment can read what an application needs without starting it, a
test can print the plan without a database, and the same description
can be handed to Spring, to Guice or to nothing at all.

The region is also what ends it. Whatever the modules acquired is
released when the region closes — in reverse order, at a value, at a
throw, at a cancellation.

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

## Three lifetimes, and who chooses them

A `module` installs ONE value: everything downstream shares it, and
the region releases it at the end. That is the singleton, and it is
the default — there is no annotation to write and none to forget.

An instance per CONSUMER is a different capability: not the thing,
but the ability to make it.

```scala
val conns = prototype[Conn](Conn.open(url))(_.close())   // or without a release
...
direct { val c = !fresh[Conn]; c.query(sql) }
```

`fresh[A]` always answers a PROGRAM, even where nothing has to be
closed. That is deliberate: the day a provider starts closing what it
makes, one line changes and no consumer moves. A pure shape answering
a bare `A` would have made that a rewrite.

**The region that RUNS the `fresh` releases it**, so the caller picks
the lifetime by picking the region:

```scala
def handle(r: Request): New[Conn] ?=> Response =
  Resource.scoped(fresh[Conn].map(c => answer(r, c)))     // one per request
```

`Resource.scoped` is the region as an expression — open, run,
release, answer — for a scope that is the whole story. Inside one
long-lived region instead, every `fresh` piles up until that region
ends; that is the trade to know, and it is why a per-request region
is the shape a handler wants.

For something expensive and reusable — a database connection — a
prototype is usually the wrong answer and a pool is the right one:
one capability for the application, `borrow` inside the request.

In a plan a prototype keeps what it makes: `Vector("Log", "New[Conn]")`.

`fresh[A]` is `wire` at another type — `wire[New[A]]()` — so there is
one primitive underneath and the difference lives in what you ask
for, not in the verb. Ask for a `fresh[Db]` where a `module[Db]`
installed the singleton and the compiler answers with both roads:
read the region's one with `wire[Db]`, or have the provider offer a
`prototype[Db]`. It is a compile error, never a silent fallback to
the shared instance — which is the whole difference between a
dependency that is a type and one that is a lookup.

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

## Against the containers next door

What Spring Boot, Guice, Dagger, ZLayer and distage offer, how it is
spelled here, and where we have nothing. Rows marked **no** are not
oversights unless they say so.

| What it is for | There | Here |
|---|---|---|
| Declare a component | `@Component`, `@Bean`, `@Provides`, `ZLayer.apply` | `module` / `moduleAs` / `Module.value` / `prototype` — a value, not an annotation |
| Find components | classpath scanning, `@ComponentScan` | **no, by design**: the graph is the `and` expression, so nothing is found by accident and nothing is missing at start-up |
| Wire by type | autowiring | the type IS the wiring; a missing one is a COMPILE error |
| Two of one type | `@Qualifier`, `@Named`, `@Primary` | an opaque type per role; the winner is the nearest `and`, not an annotation |
| Constructor injection | the recommended style | the only style — plain parameters and `using` clauses |
| Field / setter injection | supported | **no, by design**: it needs mutation and defeats the compile-time check |
| Singleton scope | default, `@Singleton` | the default: one `module`, one value for the region |
| Prototype scope | `@Scope("prototype")`, `Provider<T>` | `prototype` + `fresh[A]` — and `fresh` answers a program, so the provider can add a release without touching a consumer |
| Request / session scope | `@RequestScope` and a proxy | a region per request (`Resource.scoped`), or a nested `provide` — no proxy, and it works on any thread |
| Custom scopes | `Scope` SPI | there is no SPI because there is nothing to extend: a scope is the extent of an expression |
| Lifecycle callbacks | `@PostConstruct`, `DisposableBean`, `SmartLifecycle` | acquisition and release ARE the module; order is composition order, reverse on the way out |
| Lazy beans | `@Lazy` | a module is a recipe: nothing is built until the region runs. Within a region, acquisition is eager and ordered |
| Circular dependencies | resolved for setter/field injection | **impossible to express** — that is the feature |
| Conditional beans | `@ConditionalOnProperty`, `@Profile` | ordinary Scala: `if config.x then Module.value(...) else module(...)`. The demo picks a memory store this way |
| Configuration binding | `@Value`, `@ConfigurationProperties` | okay-conf: a case class with a derived `Schema`, `Conf.layered` for defaults → file → environment; the config is a module like any other |
| Auto-configuration | starters | okay-spring ships one FOR Spring. Ours needs none: there is nothing to discover |
| A collection of all implementations | `List<T>` injection, `Multibinder`, `MapBinder` | each module declares its piece as a FACT, `installing(k)` merges them by that kind's own rule and installs the result as a capability — see "Several contributors, one collection" |
| Assisted injection | `@AssistedInject`, factories | a capability that is a function: install `Make[A]` of your own shape. `New[A]` is the no-argument case |
| Provider indirection | `Provider<T>`, `ObjectProvider<T>` | `New[A]`, and `wire[New[A]]` if you want it by hand |
| Memoisation of a shared dependency | ZLayer builds a layer once however many depend on it | **not needed, and not built**: a shared dependency here is an INPUT (`Db ?=> Module[…]`), never embedded, so the diamond memoisation exists for does not arise — the application installs `Db` once and both readers see it. What can still bite is installing one capability TWICE, and `m.shadowed` names those (a test double is a deliberate one) |
| The plan as data | distage's plan, Spring's conditions report | `m.plan` (from the type, nothing built) and `m.exports` (what was built, with classes) |
| Verify the graph in a test | distage's plan check | the compiler; plus `plan` and `Needs.of[Root]` as assertions |
| Method interception | AOP, `@Transactional`, `@Cacheable`, `@Async` | **no proxies, by design**: a handler wraps an effect row (`Resilient.http`, `Tracer.traced`, `Typed.transact`), which is visible in the type instead of woven behind it |
| Events | `ApplicationEventPublisher`, `@EventListener` | **no**: use a channel or a hub — okay-demo publishes board changes through `Hub` |
| Test overrides | `@MockBean`, `Modules.override` | one more `and`, or `provide` in the test — no framework |
| Child injectors / private modules | `createChildInjector`, `PrivateModule` | a nested region or a nested `provide`; what a module installs is scoped to where it is applied |
| Runtime reflection | central to Spring and Guice | none anywhere — which is why this runs on Scala Native and Scala.js, and why there is no AOT story to write |
| Startup diagnostics | failure analyzers, "consider defining a bean" | the compiler names the missing type; `New`'s message names both roads when a prototype is confused with a singleton |
| Living inside one of them | — | `okay-spring`, `okay-guice`, `okay-cdi`, `okay-zio`: the module renders into their vocabulary, and their container is a source of values for ours |

**What is left missing**, restated so it is not buried in the table:
no method interception and no event bus. Both are refusals with
reasons — a handler around an effect row instead of a proxy, a channel
or a hub instead of a bus — and the reasons are in the rows. The two
gaps this table named as small when it was first written are closed:
set-binding below, and memoisation answered by the shape of the
vocabulary rather than by machinery.

## Several contributors, one collection

**Why this is not just another capability.** Installing SHADOWS: two
modules installing routes leave the second's and lose the first's,
which is right for a capability — a test double must be able to
replace one — and wrong for a contribution. A FACT accumulates
instead, by whatever rule its kind states.

The case that motivates it: several features each own part of a
service's surface, and the server must serve all of it.

```scala
type Routes = Request |=> Response ! Async     // `|=>` is PartialFunction, infix

// how two contributions merge: `orElse`, the one every server here uses
given Monoid[Routes] = Monoid.of(PartialFunction.empty[Request, Response ! Async])(_ orElse _)
object Surface extends Fact[Routes]

// A FEATURE IS ONE MODULE: its capability and the routes that use it
val boardFeature: Module[[X] =>> Board ?=> X] =
  Module.value[Board](Board(...)).declaring(Surface) {
    case r if r.url == "/board" => text(wire[Board].items.mkString(","))
  }

// it may read what came before it, like any module
def adminFeature: Board ?=> Module[[X] =>> Admin ?=> X] =
  Module.value[Admin](Admin(...)).declaring(Surface) {
    case r if r.url == "/admin"       => text(wire[Admin].token)
    case r if r.url == "/admin/count" => text(wire[Board].items.size.toString)
  }

// and a feature that owns NO capability installs nothing
def health: Board ?=> Module[[X] =>> X] =
  Module.contributing(Surface) {
    case r if r.url == "/healthz" => text(if wire[Board].items.nonEmpty then "ok" else "empty")
  }

val app = (boardFeature and adminFeature and health).installing(Surface)
app { serve(wire[Routes]) }
```

**What this buys, in one line:** `app`'s definition names no URL and
no `orElse`. The service's surface is a by-product of wiring its
features, not a second list somebody maintains by hand — and the
failure it removes is a feature that is wired, compiles, and silently
answers nothing because its routes were never added to that list.

Three things in that shape are worth naming.

**Three ways to declare, and which to reach for.** `declaring(k) { … }`
computes the fact INSIDE the module's own installer, so it can read
what that module installs — that is the feature that owns its
capability, and the usual one. `declare(k)(v)` computes it outside, so
it sees only what came BEFORE (which is right for a fact derived from
the config, like a deployment's volume). `Module.contributing(k) { … }`
installs nothing at all, for a feature written over somebody else's
capabilities — it is `Module.nothing`, the module with no capability
and no acquisition, carrying one fact. All three are curried, so the
block takes its type from `Fact[V]` and needs no ascription.

**The merge rule is the kind's, not the collection's.** `Routes`
merge with `orElse`; a deployment's needs merge with dedup (two
modules on one volume declare one volume); a plain list appends. That
rule is a `Monoid`, so a kind is one line when the givens already
have it — `object Surface extends Fact[Routes]` above needed only
that `given Monoid[Routes]` beside it. Any monoid works, so a fact
over `String` concatenates and one over `Int` sums; that is a
consequence, not the point.

**The pieces arrive in acquisition order**, and one declared BELOW an
acquisition arrives too — facts travel with the build, not only with
the value read early. The early read (`Needs.declared`) still stops
at the first acquisition, because a deployment reads it before
anything opens; the two are not the same thing.

The three uses this repository has, in order of how much they earn
their keep: a deployment's needs (shipped — the module that opens the
board's log declares the volume it lives on, and the manifest mounts
it), a service's routes (above, compiled as `TestRouteFacts` — on both platforms, so it uses
`runAsync` rather than the blocking runner, which Scala.js does not
have at compile time), and
anything a reader collects without knowing its contributors — health
checks, metrics reporters, migrations.

The fact's VALUE type becomes the capability type, so name it — an
opaque type or a small wrapper — where a bare `Vector[X]` would
collide with another collection of the same element. And declaring a
kind needs the given in scope: `import okay.given`, not `import
okay.*`, which does not bring givens (see the note in "Three
lifetimes" above — measured from outside the package).

One capability installed twice is not a merge: the second wins and
the first is acquired for nothing. `m.shadowed` reads those off the
plan, building nothing. A test double is a deliberate one, which is
why it is a report rather than an error.

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
