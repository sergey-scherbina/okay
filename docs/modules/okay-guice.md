# okay-guice

> A Module's values as Guice bindings — by name from the plan, by type
> when unique — with the scope's closer bound beside them; an
> injector's instance as a module.

Depends on: `okay` (JVM), guice 7 (jakarta.inject).

## Guide

**Rendering, not emulation** (specs/di.md). Guice stays the injector;
the module stays a value. No annotations on your classes, no
`@Inject` constructors — ours take their dependencies as parameters
and `using` clauses, and that is what makes them portable.

**Module → Guice.** `Guice.createInjector(OkayGuice.bindings((db and pool).exports))`
builds the module now (`Resource.open`) and binds each installed value
by NAME — `@Named("Db")`, the plan's name — and by type where its
erased class occurs once among the exports. Two opaque roles over one
class (`Primary`, `Replica` over `Db`) are two names and no type
binding, rather than the duplicate Guice would refuse.

**The closer.** Guice has no lifecycle, so the scope's closer is a
bound instance: `injector.getInstance(classOf[OkayGuice.ModuleScope]).close()`
releases in reverse acquisition order, when the application says so.
Idempotent.

**Guice → Module.** `OkayGuice.instance[Clock](injector)` is a module
whose acquisition asks the injector by type when the scope builds;
compose it with `and` like any other.

## Gotchas

- `bindings` is EAGER: acquisitions run when it is called, before the
  injector exists. An instance the module needs from Guice is reached
  through `instance[A]` inside the module, not the other way round.
- `Class.cast` in `bindings` is the one place a value meets Guice's
  `Class[T]`-typed API; `Installed` carries the erased class beside
  the value, so the cast restates a check the export already made.
