# okay-spring

> A Module's values as Spring singletons closed with the context, a
> Spring bean as a module, and a controller returning `A ! Async`
> served through WebFlux — wired by a Boot auto-configuration.

Depends on: `okay` (JVM), spring-context, reactor-core,
spring-boot-autoconfigure.

## Guide

**Rendering, not emulation** (specs/di.md). Spring stays the
container; the module stays a value. Nothing here scans a classpath,
reads an annotation off your class, or proxies anything.

**Module → Spring.** `OkaySpring.register(ctx, (db and pool).exports)`
builds the module now (`Resource.open`) and registers one singleton per
installed capability — named by the plan (`"Db"`, `"Pool"`), typed by
the erased class — plus one `DisposableBean` whose destroy is the
scope's closer, so `ctx.close()` releases in reverse order exactly as
`Resource.run` would. Call it before `refresh`: from an
`ApplicationContextInitializer`, or `SpringApplication.addInitializers`.
`exports` is a macro over the module's type, like `plan`; an opaque
qualifier exports under its underlying class and keeps its role in the
bean name.

```scala
val db   = module[Db](Db.open(url))(_.close)
val pool: Db ?=> Module[[X] =>> Pool ?=> X] = module[Pool](Pool.over(wire[Db]))(_.close)
app.addInitializers(ctx => OkaySpring.register(ctx, (db and pool).exports))
```

**Spring → Module.** `OkaySpring.bean[Clock](ctx)` is a module whose
acquisition looks the bean up by type when the scope builds; compose
it with `and` like any other. A container is a source of values for a
`Providing`, nothing more.

**Controllers.** With okay-spring on the classpath the auto-configuration
registers an adapter on the shared `ReactiveAdapterRegistry`, so a
WebFlux handler may return `A ! Async` and Spring subscribes to it as
it would to a `Mono`: the program runs on Reactor's bounded-elastic
scheduler on subscription, once per subscriber, and may park.
`OkayReactive.mono` and `fromPublisher` are the same two conversions
by hand.

## Gotchas

- `register` is EAGER: acquisitions run at registration, not at
  refresh. A bean the module needs from Spring is reached through
  `bean[A]` inside the module, not the other way round.
- The bean's class is the erased one. Two opaque roles over one class
  are two beans by NAME (`"Primary"`, `"Replica"`), one by type — ask
  by name where Spring would otherwise see an ambiguity.
- `Class.cast` in `register` and one `@unchecked` pattern in the
  adapter are the two places this module meets Spring's `Object`-typed
  APIs; both restate a check the JVM or the registry already made.
